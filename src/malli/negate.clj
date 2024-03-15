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
    (if (every? #(-> % -last m/properties :optional)
                entries)
      [:fn {:error/message {:en "should not be a map"}}
       `(fn [~'x] (not (map? ~'x)))]
      ;;TODO closed?
      [:multi {:dispatch #'map?}
       [true (-> [:multi {:dispatch `(fn [~'x]
                                       (cond
                                         ~@(mapcat (fn [[k s :as e]]
                                                     (when-not (-> e -last m/properties :optional)
                                                       `[(not (contains? ~'x '~k)) '~k]))
                                                   entries)
                                         :else ::default))}]
                 (into (map (fn [[k s :as e]]
                              [k (into [:map]
                                       (keep (fn [[k' s :as e]]
                                               (assert (not (-> e -last m/properties :optional))
                                                       "TODO")
                                               [k' {:optional true} (if (= k k') :never :any)]))
                                       entries)]))
                       entries)
                 (conj [::default (into [:map]
                                        (map (fn [[k s :as e]]
                                               (assert (not (-> e -last m/properties :optional))
                                                       "TODO")
                                               [k (negate (first (m/children s)) options)]))
                                        entries)]))]
       [false [:not #'map?]]])))

(defn negate
  ([?schema] (negate ?schema nil))
  ([?schema options]
   (-negate-schema (m/schema ?schema options) options)))
