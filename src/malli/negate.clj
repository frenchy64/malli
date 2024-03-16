(ns malli.negate
  (:require [malli.core :as m]
            [malli.impl.ref :refer [-identify-ref-schema]]
            [malli.impl.util :refer [-last]]))

(declare negate*)

(defmulti -negate-schema (fn [schema options] (m/type schema options)) :default ::default)
(defmethod -negate-schema :and [schema options] (into [:or] (map #(negate* % options))
                                                      (m/children schema)))
(defmethod -negate-schema :or [schema options] (into [:and] (map #(negate* % options))
                                                     (m/children schema)))
(defmethod -negate-schema := [schema options] [:not= (first (m/children schema))])
(defmethod -negate-schema :not [schema options] (m/form (first (m/children schema))))
(defmethod -negate-schema :not= [schema options] [:= (first (m/children schema))])
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
                        [[:map [k (negate* (-> s m/children first) options)]]]))
              (m/entries schema)))))

(defmethod -negate-schema :map-of [schema options]
  (let [{:keys [min max] gen-min :gen/min gen-max :gen/max} (m/properties schema options)
        [ks vs] (m/children schema options)]
    (assert (and (not max) (not min))
            "TODO :min/:max + :map-of")
    (assert (and (not gen-max) (not gen-min))
            "TODO :gen-min/:gen-max + :map-of")
    [:or
     [:not #'clojure.core/map?]
     [:into-map
      [:cat
       [:* [:tuple :any :any]]
       [:+ [:tuple (negate* ks options) :any]]
       [:* [:tuple :any :any]]]]
     [:into-map
      [:cat
       [:* [:tuple :any :any]]
       [:+ [:tuple :any (negate* vs options)]]
       [:* [:tuple :any :any]]]]]))

(defmethod -negate-schema :vector [schema options]
  (let [{:keys [min max] gen-min :gen/min gen-max :gen/max} (m/properties schema options)
        [es] (m/children schema options)]
    (assert (and (not max) (not min))
            "TODO :min/:max + :vector")
    (assert (and (not gen-max) (not gen-min))
            "TODO :gen-min/:gen-max + :vector")
    [:or
     [:not #'clojure.core/vector?]
     [:and [:cat {:gen/fmap #'vec}
            [:* :any]
            [:+ (negate es options)]
            [:* :any]]
      #'vector?]]))

(defmethod -negate-schema :nil [_ _] :some)
(defmethod -negate-schema :some [_ _] :nil)

(defmethod -negate-schema :any [_ _] :never)
(defmethod -negate-schema :never [_ _] :any)

(defn -min-max [schema options]
  (let [{:keys [min max] gen-min :gen/min gen-max :gen/max} (m/properties schema options)
        t (m/type schema)]
    (assert (and (not gen-min) (not gen-max))
            "TODO :gen-min/:gen-max")
    (cond-> [:or [:not t]]
      (and min max (pos? min)) (conj [t {:max (dec min)}]
                                     [t {:min (inc max)}])
      (and (not min) max) (conj [t {:min (inc max)}])
      (and min (not max) (pos? min)) (conj [t {:max (dec min)}]))))

(defmethod -negate-schema :string [schema options] (-min-max schema options))
(defmethod -negate-schema :int [schema options] (-min-max schema options))

(defmethod -negate-schema :boolean [_ _] [:not :boolean])
(defmethod -negate-schema :keyword [_ _] [:not :keyword])
(defmethod -negate-schema :symbol [_ _] [:not :symbol])
(defmethod -negate-schema :qualified-keyword [_ _] [:not :qualified-keyword])
(defmethod -negate-schema :qualified-symbol [_ _] [:not :qualified-symbol])
(defmethod -negate-schema :uuid [_ _] [:not :uuid])

(defmethod -negate-schema :ref [schema options]
  (let [ref-id (-identify-ref-schema schema)]
    (or (force (get-in options [::rec-negate ref-id]))
        (let [ref-name (first (m/children schema))
              recursive-ref (delay [:ref ref-name])
              nschema (negate* (m/deref schema)
                               (assoc-in options [::rec-negate ref-id] recursive-ref))]
          (-> (if (not (realized? recursive-ref))
                nschema
                [:schema {:registry {ref-name nschema}}
                 [:ref ref-name]]))))))

;;FIXME wrap in `:schema`?
(defmethod -negate-schema :schema [schema options] (negate* (m/deref schema) options))
(defmethod -negate-schema ::m/schema [schema options] (negate* (m/deref schema) options))

(defmethod -negate-schema :maybe [schema options]
  [:and (negate* (first (m/children schema)) options) :some])

(defmethod -negate-schema :tuple [schema options]
  (let [children (m/children schema)
        nchildren (count children)]
    (-> [:or [:not #'vector?]]
        (into (map (fn [i]
                     (into [:tuple]
                           (map-indexed
                             (fn [i' s]
                               (if (= i i')
                                 (negate* s options)
                                 :any)))
                           children)))
              (range nchildren))
        (cond->
          (pos? nchildren)
          (conj [:vector {:max (dec nchildren)} :any]))
        (conj [:vector {:min (inc nchildren)} :any]))))

(defn- negate* [?schema options]
  (-negate-schema (m/schema ?schema options) options))

(defn negate
  ([?schema] (negate ?schema nil))
  ([?schema options]
   (-> ?schema
       (negate* options)
       (m/schema options))))
