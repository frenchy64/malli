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

(defn -uncapture-fail! []
  (alter-var-root #'m/-fail! (fn [f] (-> f meta ::original (or f)))))

(defonce reloading-validators (atom []))

(defn -reloading-validator! []
  (alter-var-root
    #'m/validator
    (fn [validator]
      (if (::original (meta validator))
        validator
        (-> (fn reloading-validator
              ([?schema] (reloading-validator ?schema nil))
              ([?schema options] (if (m/schema? ?schema)
                                   ;; only reloadable if ?schema not already a Schema, so don't even bother.
                                   (validator ?schema options)
                                   (let [v (validator ?schema options)
                                         vol (volatile! v)]
                                     (swap! reloading-validators
                                            conj (java.lang.ref.WeakReference.
                                                   #(vreset! vol (validator ?schema options))))
                                     #(@vol %)))))
            (with-meta {::original validator}))))))

(defn -unreloading-validator! []
  (alter-var-root #'m/validator (fn [f] (-> f meta ::original (or f)))))

(m/-register-global-cache-invalidation-watcher!
  ::-reloading-validator
  (fn [] (swap! reloading-validators
                (fn [vs]
                  (into [] (keep (fn [^java.lang.ref.WeakReference r]
                                   (when-some [f (.get r)]
                                     (f)
                                     r)))
                        vs)))))

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
  (-unreloading-validator!)
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
   (-reloading-validator!)
   (-log! "dev-mode started")))
