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
  (-> [:or [:not #'clojure.core/map?]]
      (into (keep (fn [[k :as e]]
                    (when (-> e -last m/properties :optional not)
                      [:map [k {:optional true} :never]])))
            (m/entries schema))
      (into (mapcat (fn [[k s :as e]]
                      [[:map [k (negate (-> s m/children first) options)]]]))
            (m/entries schema))))

(defn negate
  ([?schema] (negate ?schema nil))
  ([?schema options]
   (-negate-schema (m/schema ?schema options) options)))
