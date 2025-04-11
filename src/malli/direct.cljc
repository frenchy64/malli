(ns malli.direct
  (:require [malli.core :as m]
            [malli.registry :as mr]))

(declare direct)

(defn -direct-default [s rvar options-local opts]
  (let [p (m/properties s)
        pno-reg (dissoc p :registry)
        reg (not-empty (:registry p))
        goptions-local (gensym 'options-local)
        gr (gensym 'r)]
    ;;TODO pointer schemas
    `(let [~goptions-local ~options-local
           ~gr ~(some->> reg
                         (reduce-kv (fn [acc k v]
                                      (assoc acc (list 'quote k) (direct v goptions-local opts)))
                                    {}))
           ~goptions-local ~(if reg
                              `(m/-add-options-registry ~options-local (m/-delayed-registry ~reg #(%1 %2)))
                              options-local)
           p# ~(if reg
                 `(assoc ~(list 'quote pno-reg) :registry ~gr)
                 (list 'quote pno-reg))]
       (m/-into-schema (mr/schema ~(symbol rvar) '~(m/type s))
                       p#
                       ~(mapv #(direct % rvar goptions-local opts) (m/children s))
                       ~goptions-local))))

(defonce default (Object.))

(defmulti -direct (fn [s _ _ _] (m/type s))
  :default default)

(defmethod -direct default [s rvar options-local opts] (-direct-default s rvar options-local opts))

(defn direct
  ([s rvar] (direct s rvar nil nil))
  ([s rvar options-local opts]
   (let [s (m/schema s opts)]
     (if (m/-ast? s)
       (do ;(assert (m/-direct? s)) ;;TODO
           (-direct s rvar options-local opts)) ;;TODO
       (-direct s rvar options-local opts)))))

(comment
  (direct :int #'m/default-registry)
  (direct [:vector :int] #'m/default-registry)
  (direct [:schema :int] #'m/default-registry)
  (direct [:map [:foo :int]] #'m/default-registry)

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



  )
