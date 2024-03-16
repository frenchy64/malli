(ns malli.negate
  (:require [malli.core :as m]
            [malli.impl.util :refer [-last]]))

(declare negate)

(defmulti -negate-schema (fn [schema options] (m/type schema options)) :default ::default)
(defmethod -negate-schema :and [schema options] (m/schema (into [:or] (map #(negate % options))
                                                                (m/children schema))
                                                          options))
(defmethod -negate-schema :or [schema options] (m/schema (into [:and] (map #(negate % options))
                                                               (m/children schema))
                                                         options))
(defmethod -negate-schema := [schema options] (m/schema [:not= (first (m/children schema))] options))
(defmethod -negate-schema :not [schema options] (first (m/children schema)))
(defmethod -negate-schema :not= [schema options] (m/schema [:= (first (m/children schema))] options))
(defmethod -negate-schema :map [schema options]
  (let [entries (m/entries schema)]
    (assert (not (:closed (m/properties schema)))
            "TODO negate :closed :map")
    (assert (not-any? #(= ::m/default (first %))
                      entries)
            "TODO negate ::m/default :map")
    (-> [:or [:not #'clojure.core/map?]]
        (into (keep (fn [[k :as e]]
                      (when (-> e -last m/properties :optional not)
                        [:map [k {:optional true} :never]])))
              (m/entries schema))
        (into (mapcat (fn [[k s :as e]]
                        [[:map [k (negate (-> s m/children first) options)]]]))
              (m/entries schema)))))

(defmethod -negate-schema :map-of [schema options]
  (let [{:keys [min max]} (m/properties schema options)
        [ks vs] (m/children schema options)]
    (assert (and (not max) (not min))
            "TODO :min/:max + :map-of")
    [:or
     [:not #'clojure.core/map?]
     [:map-of {:min 1} (negate ks options) :any]
     [:map-of {:min 1} :any (negate vs options)]]))

(defmethod -negate-schema :nil [_ _] :some)
(defmethod -negate-schema :some [_ _] :nil)

(defmethod -negate-schema :any [_ _] :never)
(defmethod -negate-schema :never [_ _] :any)

(defn negate
  ([?schema] (negate ?schema nil))
  ([?schema options]
   (m/schema (-negate-schema (m/schema ?schema options) options))))
