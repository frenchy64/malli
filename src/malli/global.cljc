(ns malli.global
  (:require [malli.core :as m]
            [malli.registry :as mr]))

(declare -global-options)
(def ^:private global-schemas (atom {}))
(defn- -global-schema [type]
  (let [r @global-schemas]
    (or (r type)
        (loop [r r
               fprev nil
               res-prev nil]
          (when-some [f (-> r meta ::cache (get type))]
            (let [m-or-res (if (identical? fprev f)
                             res-prev
                             (if (fn? f) (f) f))
                  m (if (map? m-or-res) m-or-res {type m-or-res})
                  res (get m type)
                  r' (reduce-kv (fn [r k v]
                                  (if (get r k)
                                    (reduced (with-meta (update m #(cond-> % (not (m/into-schema? %)) (m/schema -global-options)))
                                                        (meta r)))
                                    (assoc r k (cond-> v (not (m/into-schema? v)) (m/schema -global-options)))))
                                r m)]
              (if (compare-and-set! global-schemas r r')
                res
                (recur @global-schemas f res))))))))

(defn -global-registry []
  (reify
    mr/Registry
    (-schema [_ type] (-global-schema type))
    (-schemas [_] @global-schemas)))

;#_
(def ^:private __add-global-registry__
  (let [strict #?(:cljs (identical? mr/mode "strict")
                  :default (= mr/mode "strict"))
        custom #?(:cljs (identical? mr/type "custom")
                  :default (= mr/type "custom"))]
    (when (or strict custom)
      (m/-fail! ::global-registry-disabled))
    (swap! @#'mr/registry* mr/composite-registry (-global-registry))
    nil))

(def ^:private -global-options {:registry (-global-registry)})

(defn -reg! [k f]
  (let [v' (if (fn? f) (f) f)
        v' (cond-> v'
             (not (m/into-schema? v'))
             (m/schema -global-options))]
    (swap! global-schemas
           (fn [r]
             (-> r
                 (cond-> (get r k) empty)
                 (vary-meta assoc-in [::cache k] f)
                 (assoc k v')))))
  k)

(defn -reg-schemas! [f]
  (let [r (f)
        c (update-vals r (constantly f))]
    (swap! global-schemas
           (fn [m]
             (let [m (reduce-kv (fn [m k v]
                                  (if (get m k)
                                    (reduced (with-meta r (meta m)))
                                    (assoc m k v)))
                                m r)]
               (vary-meta m update ::cache (fnil into {}) c)))))
  nil)

(-reg-schemas! m/default-schemas)

#?(:clj
   (defn- -reg-macro [qkw args mode extra at-form at-env]
     (let [file *file*
           nsym (ns-name *ns*)
           line (.deref clojure.lang.Compiler/LINE)
           column (.deref clojure.lang.Compiler/COLUMN)
           ;_ (when-not (qualified-keyword? qkw)
           ;    (m/-fail! ::reg-qualified-keywords-only))
           [m args] (if (and (string? (first args))
                             (next args))
                      [{:doc (first args)} (next args)]
                      [{} args])
           [m args] (if (and (map? (first args))
                             (next args))
                      [(conj m (first args)) (next args)]
                      [m args])
           s (first args)
           m (-> m
                 (assoc :file *file*)
                 (assoc :ns nsym)
                 (assoc :line line)
                 (assoc :column column)
                 (assoc :form at-form)
                 (assoc :schema-form s)
                 (into extra))
           _ (when-not (seq args)
               (m/-fail! ::reg-missing-schema))
           _ (when (next args)
               (m/-fail! ::reg-too-many-args))
           platform (if (:ns at-env) :cljs :clj)
           _ ((requiring-resolve 'malli.doc/-register-schema-meta!) qkw m platform)]
       ;;TODO allow omitting fn in production
       `(-reg! ~qkw #(do ~s)))))

#?(:clj
   (defmacro reg
     "Register a global schema under a qualified keyword.
     
     Supports defn-like metadata which can be looked up with `doc`."
     [qkw & args]
     (-reg-macro qkw args :reg {} &form &env)))

#?(:clj
   (defmacro reg-type
     "Register a global schema constructor under a qualified keyword.

     Type should be a function of zero arguments that returns an into-schema.
     
     Supports defn-like metadata which can be looked up with `doc`."
     [qkw & args]
     (-reg-macro qkw args :reg-type {:into-schema true} &form &env)))
