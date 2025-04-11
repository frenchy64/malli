(ns malli.direct
  (:require [malli.core :as m]
            [malli.registry :as mr]))

(declare direct)

(defn -direct-default [s rvar options-local opts]
  (let [p (m/properties s)
        pno-reg (dissoc p :registry)
        reg (not-empty (:registry p))
        ;;wrap in delayed
        pexpr (if reg
                )
        goptions-local (gensym 'options-local)
        gr (gensym 'r)]
    ;;TODO pointer schemas
    `(let [~gr ~(when reg
                  (reduce-kv (fn [acc k v]
                               (assoc acc (list 'quote k) `(reify m/IntoSchema
                                                             (~'-into-schema [_# _# _# ~goptions-local]
                                                               ~(direct v goptions-local opts)))))
                             {} m))
           ~goptions-local ~(if reg
                              `(m/-add-options-registry ~options-local (m/-delayed-registry ~reg #(%1 %2)))
                              options-local)
           p# ~(if reg
                 `(assoc ~(list 'quote pno-reg) :registry )
                 ~(list 'quote pno-reg))]
       (m/-into-schema (mr/schema ~(symbol rvar) '~(m/type s))
                       ~pexpr
                       ~(mapv #(-direct-all % rvar goptions-local opts) (m/children s))
                       ~goptions-local))))

(defonce default (Object.))

(defmulti -direct (fn [s _ _ _] (m/type s))
  :default default)

(defmethod -direct default [s rvar options-local opts] (-direct-all s rvar opts))

(defn direct
  ([s rvar] (direct s rvar nil))
  ([s rvar opts]
   (if (m/-ast? x)
     (do ;(assert (m/-direct? x)) ;;TODO
         (-direct-all (m/schema s opts) rvar nil opts)) ;;TODO
     (-direct-default (m/schema s opts) rvar nil opts))))

(comment
  (direct :int #'m/default-registry)
  (direct [:vector :int] #'m/default-registry)
  (direct [:schema :int] #'m/default-registry)

  (direct [:schema {:registry {::foo :int}} :int] #'m/default-registry)
  (-> (m/ast [:schema {:registry {::foo :int}} :int])
      :registry
      first
      second
      :type)

  (m/ast [:schema {:registry {::foo [:ref ::bar]
                              ::bar [:ref ::foo]}} :int])
  (m/ast [:schema {:registry {::foo ::bar
                              ::bar :int}} :int])

  (malli.core/-into-schema
    (malli.registry/schema malli.core/default-registry :vector)
    'nil
    [(malli.core/-into-schema
       (malli.registry/schema malli.core/default-registry :int)
       'nil
       []
       nil)]
    nil)

  (malli.core/-into-schema
    (malli.registry/schema malli.core/default-registry :int)
    'nil
    []
    nil)

  (malli.core/-into-schema
    (malli.registry/schema malli.core/default-registry :schema)
    '{:registry #:malli.direct{:foo :int}}
    [(malli.core/-into-schema
       (malli.registry/schema malli.core/default-registry :int)
       'nil
       []
       nil)]
    nil)
  )
