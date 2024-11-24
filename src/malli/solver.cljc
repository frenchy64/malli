(ns malli.solver
  (:require [clojure.core :as c]
            [clojure.math :as math]
            [clojure.math.combinatorics :as comb]
            [malli.core :as m]
            [malli.impl.util :refer [-merge]]))

(declare solve)

(defn- -intersect-number-constraints [all-sols mink maxk]
  (let [maxv (some->> (seq (keep maxk all-sols)) (apply c/min))
        minv (some->> (seq (keep mink all-sols)) (apply c/max))]
    (if (and minv maxv)
      (if (<= minv maxv)
        [{mink minv
          maxk maxv}]
        [])
      (if minv
        [{mink minv}]
        (when maxv
          [{maxk maxv}])))))

(defn- -intersect-min-max [all-sols]
  (if-some [sols (when (seq all-sols)
                   (not-empty (into [] (keep (fn [[mink maxk]]
                                               (-intersect-number-constraints all-sols mink maxk)))
                                    [[:min-count :max-count]
                                     [:min-range :max-range]])))]
    (lazy-seq
      (->> (apply comb/cartesian-product sols)
           (map #(apply merge %))))
    [{}]))

(def ^:private type-super
  {:int #{:number}})

(defn- -type-supers [t] (into #{} (mapcat #(cons % (-type-supers %))) (type-super t)))

(defn- -type-constraints [all-sols]
  (when-some [types (not-empty (into #{} (map :type) all-sols))]
    (let [remove-redundant (reduce (fn [acc t] (apply disj acc (-type-supers t))) types types)]
      (mapv #(hash-map :type %) remove-redundant))))

(defn- -intersect-type [all-sols] (or (-type-constraints all-sols) [{}]))

(defn -intersect [sols]
  (letfn [(rec [cart-sols]
            (lazy-seq
              (when-some [[[sol1 & sols :as all-sols]] (seq cart-sols)]
                (when-some [unsupported-keys (not-empty
                                               (disj (into #{} (mapcat keys) all-sols)
                                                     :type
                                                     :max-count :min-count
                                                     :max-range :min-range))]
                  (m/-fail! ::unsupported-solution {:unsupported-keys unsupported-keys}))
                (let [type-constraints (-intersect-type all-sols)
                      number-solutions (-intersect-min-max all-sols)
                      combined-sols (comb/cartesian-product type-constraints number-solutions)]
                  (if (empty? combined-sols)
                    []
                    (concat (map #(apply merge %) combined-sols)
                            (rec (rest cart-sols))))))))]
    (distinct (rec (apply comb/cartesian-product (distinct sols))))))

(defn- -min-max-range [stype schema {::keys [mode]}]
  (let [{gen-min :gen/min gen-max :gen/max :keys [min max]} (m/properties schema)]
    [(cond-> {:type stype}
       min (assoc :min-range min)
       max (assoc :max-range max)
       (= :gen mode) (cond->
                       gen-min (assoc :min-range gen-min)
                       gen-max (assoc :max-range gen-max)))]))

(defmulti -solve (fn [schema options] (m/type schema)))

(defn- -solve-from-return [props]
  (when (contains? props :gen/return)
    [{:= (:gen/return props)}]))

(defn- -solve-from-elements [props]
  (some->> (:gen/elements props) (mapv #(hash-map := %))))

(defn- -solve-from-schema [props options]
  (some-> (:gen/schema props) (solve options)))

(defn solve [schema {::keys [mode] :as options}]
  {:post [(every? map? %)]}
  (lazy-seq
    (or (when (= :gen mode) ;;TODO :gen/fmap
          (let [props (-merge (m/type-properties schema)
                              (m/properties schema))]
            (or (-solve-from-return props)
                (-solve-from-elements props)
                (-solve-from-schema props options))))
        (-solve schema options))))

(defmethod -solve :any [schema options] [{}])
(defmethod -solve :and [schema options] (-intersect (map #(solve % options) (m/children schema))))
(defmethod -solve :or [schema options] (into [] (mapcat #(solve % options)) (m/children schema)))
(defmethod -solve :<= [schema options] [{:type :number :max-range (first (m/children schema))}])
(defmethod -solve :>= [schema options] [{:type :number :min-range (first (m/children schema))}])
(defmethod -solve :< [schema options] [{:type :number :max-range (math/next-down (first (m/children schema)))}])
(defmethod -solve :> [schema options] [{:type :number :min-range (math/next-up (first (m/children schema)))}])
(defmethod -solve 'pos? [schema options] [{:type :number :min-range (math/next-up 0)}])
(defmethod -solve 'neg? [schema options] [{:type :number :max-range (math/next-down 0)}])
(defmethod -solve :int [schema options] (-min-max-range :int schema options))
(defmethod -solve :double [schema options] (-min-max-range :double schema options))
(defmethod -solve :float [schema options] (-min-max-range :float schema options))
(defmethod -solve :default [schema options] [{}])
