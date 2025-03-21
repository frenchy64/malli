(ns malli.global
  (:require [malli.core :as m]
            [malli.registry :as mr]))

(def ^:private global-schemas (atom {}))
(defn- -global-schema [type] (@global-schemas type))

;; TODO reloading-registry
(defn -global-registry []
  (reify
    mr/Registry
    (-schema [_ type] (-global-schema type))
    (-schemas [_] @global-schemas)))

(def ^:private __add-global-registry__
  (let [strict #?(:cljs (identical? mr/mode "strict")
                  :default (= mr/mode "strict"))
        custom #?(:cljs (identical? mr/type "custom")
                  :default (= mr/type "custom"))]
    (when (or strict custom)
      (m/-fail! ::global-registry-disabled))
    (swap! @#'mr/registry* mr/composite-registry (-global-registry))
    nil))

(defn- -reg*
  [k v]
  (swap! global-schemas assoc k v)
  k)

(defn -reg!
  "Register a schema form under a qualified keyword in the global registry.
  Returns the keyword."
  [qkw sform]
  (when-not (qualified-keyword? qkw)
    (m/-fail! ::reg-key-must-be-qualified-keyword))
  (-reg* qkw (m/schema sform)))

(defn -reg-type!
  "Register an IntoSchema under a qualified keyword in the global registry.
  Returns the keyword."
  [qkw t]
  (when-not (qualified-keyword? qkw)
    (m/-fail! ::reg-type-key-must-be-qualified-keyword))
  (when-not (m/into-schema? t)
    (m/-fail! ::reg-type-must-be-into-schema))
  (-reg* qkw t))

#?(:clj
   (defn- -save-ctor [t] t))

#?(:clj
   (defn- -reg-macro [qkw args mode extra at-form at-env]
     (let [file *file*
           nsym (ns-name *ns*)
           line (.deref clojure.lang.Compiler/LINE)
           column (.deref clojure.lang.Compiler/COLUMN)
           _ (when-not (qualified-keyword? qkw)
               (m/-fail! ::reg-qualified-keywords-only))
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
           platform (if (:ns at-env) :cljs :clj)]
       ((requiring-resolve 'malli.doc/-register-schema-meta!) qkw m platform)
       (case mode
         :reg `(-reg! ~qkw ~(-save-ctor s))
         :reg-type `(-reg-type! ~qkw ~(-save-ctor s))))))

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
