(ns malli.direct
  (:require [clojure.set :as set]
            [malli.core :as m]
            [malli.registry :as mr]))

#?(:cljs (goog-define mode "dev")
   :clj  (def mode (or (System/getProperty "malli.registry/mode") "dev")))

(declare direct*)

(def ^:dynamic ^:private *gensym* gensym)

(defn -direct-default [s options-local process-children opts]
  (let [p (m/properties s)
        pno-reg (not-empty (dissoc p :registry))
        reg (not-empty (:registry p))]
    (if reg
      (let [gr (*gensym* 'r)
            gx (*gensym* 'x)
            gp (*gensym* 'p)
            gproperties' (*gensym* 'properties')
            gproperties (*gensym* 'properties)
            goptions (*gensym* 'options)]
        ;;inlines m/into-schema
        `(let [~gr '~(update-vals (:registry p) m/form)
               ~options-local (m/-update ~options-local :registry (fn [~gx] (mr/composite-registry ~gr (or ~gx (m/-registry ~options-local)))))
               ~gp ~(let [r (reduce-kv (fn [acc k v]
                                         (assoc acc (list 'quote k) (direct* v options-local opts)))
                                       {} reg)]
                      (if pno-reg
                        `(assoc ~(list 'quote pno-reg) :registry ~r)
                        {:registry r}))]
           (m/-into-schema (mr/schema (:registry ~options-local) '~(m/type s))
                           ~gp
                           ~(process-children (m/children s) options-local opts)
                           ~options-local)))
      `(m/-into-schema (mr/schema (:registry ~options-local) '~(m/type s))
                       '~pno-reg
                       ~(process-children (m/children s) options-local opts)
                       ~options-local))))

(defn -direct-nested [s options-local opts]
  (-direct-default s options-local (fn [children options-local opts] (mapv #(direct* % options-local opts) children)) opts))

(defonce default (Object.))

(defmulti -direct (fn [s _ _] (m/type s))
  :default default)

(defn -direct-value [s options-local opts]
  (-direct-default s options-local (fn [children options-local opts] (list 'quote (vec children))) opts))

(defn -direct-child [s options-local opts]
  (-direct-default s options-local
                   (fn [[c :as children] options-local opts]
                     {:pre [(= 1 (count children))]}
                     [(direct* c options-local opts)])
                   opts))

(defn -direct-childless [s options-local opts]
  (-direct-default s options-local
                   (fn [children _ _]
                     {:pre [(empty? children)]}
                     [])
                   opts))

(defmethod -direct :ref [s options-local opts] (-direct-value s options-local opts))
(defmethod -direct ::m/schema [s options-local opts]
  (if-some [ref (m/-ref s)]
    `(m/schema '~ref ~options-local) ;; m/-pointer case, inline further?
    (-direct-nested s options-local opts)))

(defn -infer-direct-ast [s options-local opts]
  (if (m/-ast? s)
    (let [ast (m/ast s)
          ks (-> ast (dissoc :properties :registry) keys set)]
      (case ks
        #{:type :value} (-direct-value s options-local opts)
        #{:type :child} (-direct-child s options-local opts)
        #{:type} (-direct-childless s options-local opts)
        nil))
    (-direct-nested s options-local opts)))

(defmethod -direct default [s options-local opts]
  (or (-infer-direct-ast s options-local opts)
      (m/-fail! ::cannot-compile-schema {:schema s})))

(defn direct*
  ([s] (let [go (*gensym* 'o)]
         `(let [~go {:registry (m/-registry)}]
            ~(direct* s go nil))))
  ([s options-local opts] (-direct (m/schema s opts) options-local opts)))

(defn direct-cljs* [s]
  (let [s (m/schema (eval s))
        c (direct* s)]
    `(let [s# ~c]
       (when-not (identical? mode "dev")
         (assert (= (m/form s#) '~(m/form s))
                 (str "Cannot compile: " &form)))
       s#)))

(defn direct-clj* [s] (direct* (m/schema (eval s))))

(defmacro direct [s] ((if (:ns &env) direct-cljs* direct-clj*) s))

(comment
  (direct* :int)
  (direct* [:vector :int])
  (direct* [:schema :int])
  (direct* [:map [:foo :int]])

  )
