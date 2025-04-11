(ns malli.direct
  (:require [malli.core :as m]
            [malli.registry :as mr]))

(declare direct)

(def ^:dynamic ^:private *gensym* gensym)

(defn -direct-default [s options-local opts]
  (let [p (m/properties s)
        pno-reg (dissoc p :registry)
        reg (not-empty (:registry p))
        gr (*gensym* 'r)
        gx (*gensym* 'x)
        gp (*gensym* 'p)]
    ;;TODO pointer schemas
    (if reg
      `(let [~gr ~(some->> reg
                           (reduce-kv (fn [acc k v]
                                        (assoc acc (list 'quote k) (direct v options-local opts)))
                                      {}))
             ~options-local ~(if reg
                               `(m/-update ~options-local :registry (fn [~gx] (mr/composite-registry ~gr (or ~gx (m/-registry ~options-local)))))
                               options-local)
             ~gp ~(if reg
                    `(assoc ~(list 'quote pno-reg) :registry ~gr)
                    (list 'quote pno-reg))]
         (m/-into-schema (mr/schema (:registry ~options-local) '~(m/type s))
                         ~gp
                         ~(mapv #(direct % options-local opts) (m/children s))
                         ~options-local))
      `(m/-into-schema (mr/schema (:registry ~options-local) '~(m/type s))
                       '~pno-reg
                       ~(mapv #(direct % options-local opts) (m/children s))
                       ~options-local))))

(defonce default (Object.))

(defmulti -direct (fn [s _ _] (m/type s))
  :default default)

(defmethod -direct default [s options-local opts] (-direct-default s options-local opts))

(defn direct
  ([s] (let [go (*gensym* 'r)]
         `(let [~go {:registry (m/-registry)}]
            ~(direct s go nil))))
  ([s options-local opts]
   (let [s (m/schema s opts)]
     (if (m/-ast? s)
       (do ;(assert (m/-direct? s)) ;;TODO
           (-direct s options-local opts)) ;;TODO
       (-direct s options-local opts)))))

(comment
  (direct :int)
  (direct [:vector :int])
  (direct [:schema :int])
  (direct [:map [:foo :int]])

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
