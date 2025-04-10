(ns malli.direct
  (:require [malli.core :as m]
            [malli.registry :as mr]))

(defn -direct-all [s rvar opts]
  (let [p (m/properties s)
        pno-reg (dissoc p :registry)
        reg (:registry p)
        pexpr (if reg
                )]
    `(m/-into-schema (mr/schema ~(symbol rvar) '~(m/type s))
                     ~pexpr
                     ~(mapv #(-direct-all % rvar opts) (m/children s))
                     nil)))

(defonce default (Object.))

(defmulti -direct (fn [s _ _] (m/type s))
  :default default)

(defmethod -direct default [s rvar opts] (-direct-all s rvar opts))

(defn direct
  ([s rvar] (direct s rvar nil))
  ([s rvar opts] (-direct-all (m/schema s opts) rvar opts)))

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
