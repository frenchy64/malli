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

(defn -with-reloading-global-schemas []
  (alter-var-root
    #'m/-save-ctor
    (fn [f]
      (-> (fn -save-ctor [t] `#(do ~t))
          (with-meta {::original f}))))
  (alter-var-root
    #'m/-global-schema
    (fn [f]
      (-> (fn -reg* [k v]
            (swap-vals! @#'m/global-schemas
                        (fn [m]
                          (let [prev (m k)]
                            (-> m
                                (assoc k v))))))
          (with-meta {::original f}))))
  (alter-var-root
    #'m/-reg*
    (fn [f]
      (-> (fn -reg* [k v]
            (swap-vals! @#'m/global-schemas
                        (fn [m]
                          (let [prev (m k)]
                            (-> m
                                (assoc k v))))))
          (with-meta {::original f})))))

(defn -without-reloading-global-schemas []
  (alter-var-root #'m/-reg* (fn [f] (-> f meta ::original (or f))))
  (alter-var-root #'m/-global-schema (fn [f] (-> f meta ::original (or f))))
  (alter-var-root #'m/-save-ctor (fn [f] (-> f meta ::original (or f)))))

;;
;; Public API
;;

(defn stop!
  "Stops instrumentation for all functions vars and removes clj-kondo type annotations."
  []
  (remove-watch @#'m/-function-schemas* ::watch)
  (->> (mi/unstrument!) (count) (format "unstrumented %d function vars") (-log!))
  (clj-kondo/save! {})
  (-without-reloading-global-schemas)
  (-uncapture-fail!)
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
   (-without-reloading-global-schemas)
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
   (-log! "dev-mode started")))
