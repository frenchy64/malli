(ns malli.dev
  (:require [malli.clj-kondo :as clj-kondo]
            [malli.core :as m]
            [malli.dev.pretty :as pretty]
            [malli.instrument :as mi]))

(defn -log!
  ([text] (-log! text (pretty/-printer)))
  ([text printer] (pretty/-log! text printer)))

(defn -capture-fail!
  ([] (-capture-fail! nil))
  ([{:keys [report] :or {report (pretty/reporter)}}]
   (alter-var-root
    #'m/-fail!
    (fn [f] (-> (fn -fail!
                  ([type] (-fail! type nil))
                  ([type data] (let [e (m/-exception type data)]
                                 (report type data)
                                 (throw e))))
                (with-meta {::original f}))))))

(defn -unwrap-original! [v]
  (alter-var-root v (fn [f] (-> f meta ::original (or f)))))

(defn -uncapture-fail! [] (-unwrap-original! #'m/-fail!))

(defonce reloading-fs (atom []))

(defn -reloading-op! [v f]
  (alter-var-root
    v
    (fn [original]
      (if (::original (meta original))
        original
        (with-meta (f original) {::original original})))))

(defn -unreloading-op! [v f]
  (alter-var-root
    v
    (fn [original]
      (if (::original (meta original))
        original
        (with-meta (f original) {::original original})))))

(defn -reloading-validator! []
  (-reloading-op!
    #'m/validator
    (fn [validator]
      (fn reloading-validator
        ([?schema] (reloading-validator ?schema nil))
        ([?schema options] (if (m/schema? ?schema)
                             ;; only reloadable if ?schema not already a Schema, so don't even bother.
                             (validator ?schema options)
                             (let [v (validator ?schema options)
                                   vol (volatile! v)]
                               (swap! reloading-fs
                                      conj (java.lang.ref.WeakReference.
                                             #(vreset! vol (validator ?schema options))))
                               #(@vol %))))))))
(defn -unreloading-validator! [] (-unwrap-original! #'m/validator))

(defn -reloading-explainer! []
  (-reloading-op!
    #'m/explainer
    (fn [explainer]
      (fn reloading-explainer
        ([?schema] (reloading-explainer ?schema nil))
        ([?schema options] (if (m/schema? ?schema)
                             ;; only reloadable if ?schema not already a Schema, so don't even bother.
                             (explainer ?schema options)
                             (let [v (explainer ?schema options)
                                   vol (volatile! v)]
                               (swap! reloading-fs
                                      conj (java.lang.ref.WeakReference.
                                             #(vreset! vol (explainer ?schema options))))
                               #(@vol %))))))))
(defn -unreloading-explainer! [] (-unwrap-original! #'m/explainer))

(defn -reloading-parser! []
  (-reloading-op!
    #'m/parser
    (fn [parser]
      (fn reloading-parser
        ([?schema value] (reloading-parser ?schema value nil))
        ([?schema value options] (if (m/schema? ?schema)
                                   ;; only reloadable if ?schema not already a Schema, so don't even bother.
                                   (parser ?schema value options)
                                   (let [v (parser ?schema value options)
                                         vol (volatile! v)]
                                     (swap! reloading-fs
                                            conj (java.lang.ref.WeakReference.
                                                   #(vreset! vol (parser ?schema value options))))
                                     #(@vol %))))))))
(defn -unreloading-parser! [] (-unwrap-original! #'m/parser))

(defn -reloading-unparser! []
  (-reloading-op!
    #'m/unparser
    (fn [unparser]
      (fn reloading-unparser
        ([?schema] (reloading-unparser ?schema nil))
        ([?schema options] (if (m/schema? ?schema)
                             ;; only reloadable if ?schema not already a Schema, so don't even bother.
                             (unparser ?schema options)
                             (let [v (unparser ?schema options)
                                   vol (volatile! v)]
                               (swap! reloading-fs
                                      conj (java.lang.ref.WeakReference.
                                             #(vreset! vol (unparser ?schema options))))
                               #(@vol %))))))))
(defn -unreloading-unparser! [] (-unwrap-original! #'m/unparser))

(m/-register-global-cache-invalidation-watcher!
  ::dev-reloading
  #(swap! reloading-fs
          (fn [vs]
            (into [] (keep (fn [^java.lang.ref.WeakReference r]
                             (when-some [f (.get r)]
                               (f)
                               r)))
                  vs))))

(defn -reloading-ops! []
  (-reloading-validator!)
  (-reloading-explainer!)
  (-reloading-parser!)
  (-reloading-unparser!))

(defn -unreloading-ops! []
  (-unreloading-validator!)
  (-unreloading-explainer!)
  (-unreloading-parser!)
  (-unreloading-unparser!))

;;
;; Public API
;;

(defn stop!
  "Stops instrumentation for all functions vars and removes clj-kondo type annotations."
  []
  (remove-watch @#'m/-function-schemas* ::watch)
  (->> (mi/unstrument!) (count) (format "unstrumented %d function vars") (-log!))
  (clj-kondo/save! {})
  (-uncapture-fail!)
  (-unreloading-ops!)
  (-log! "dev-mode stopped"))

(defn start!
  "Collects defn schemas from all loaded namespaces and starts instrumentation for
   a filtered set of function Vars (e.g. `defn`s). See [[malli.core/-instrument]]
   for possible options. Re-instruments if the function schemas change. Also emits
   clj-kondo type annotations."
  ([] (start! {:report (pretty/reporter)}))
  ([options]
   (with-out-str (stop!))
   (-capture-fail! options)
   (mi/collect! {:ns (all-ns)})
   (let [watch (bound-fn [_ _ old new]
                 (->> (for [[n d] (:clj new)
                            :let [no (get-in old [:clj n])]
                            [s d] d
                            :when (not= d (get no s))]
                        [[n s] d])
                      (into {})
                      (reduce-kv assoc-in {})
                      (assoc options :data)
                      (mi/instrument!))
                 (clj-kondo/emit! options))]
     (add-watch @#'m/-function-schemas* ::watch watch))
   (let [count (->> (mi/instrument! options) (count))]
     (when (pos? count) (-log! (format "instrumented %d function vars" count))))
   (clj-kondo/emit! options)
   (-reloading-ops!)
   (-log! "dev-mode started")))
