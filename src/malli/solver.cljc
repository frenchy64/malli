(ns malli.solver
  (:require [clojure.core :as c]
            [clojure.math :as math]
            [clojure.math.combinatorics :as comb]
            [malli.core :as m]
            [malli.impl.util :refer [-merge]]))

;; known constraints
;; :type              a keyword describing the value type
;; :{min,max}-number  numbers describing value {minimum,maximum} bounds

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
                                     [:min-number :max-number]])))]
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
                                                     :max-number :min-number))]
                  (m/-fail! ::unsupported-solution {:unsupported-keys unsupported-keys}))
                (let [type-constraints (-intersect-type all-sols)
                      number-solutions (-intersect-min-max all-sols)
                      combined-sols (comb/cartesian-product type-constraints number-solutions)]
                  (if (empty? combined-sols)
                    []
                    (concat (map #(apply merge %) combined-sols)
                            (rec (rest cart-sols))))))))]
    (distinct (rec (apply comb/cartesian-product (distinct sols))))))

(defn- -min-max-number [stype schema {::keys [mode]}]
  (let [{gen-min :gen/min gen-max :gen/max :keys [min max]} (m/properties schema)]
    [(cond-> {:type stype}
       min (assoc :min-number min)
       max (assoc :max-number max)
       (= :gen mode) (cond->
                       gen-min (assoc :min-number gen-min)
                       gen-max (assoc :max-number gen-max)))]))

(defmulti -solve (fn [schema options] (m/type schema)))

(defn- -solve-from-schema [props options] (some-> (:gen/schema props) (solve options)))

(defn solve
  "Returns a sequence of maps each describing values that satisfy schema.
  
  Options:
  - ::mode if :gen, consider generative fields like :gen/schema and :gen/min
           as necessary to satify the schema."
  ([schema] (solve schema nil))
  ([schema {::keys [mode] :as options}]
   (lazy-seq
     (or (when (= :gen mode) ;;TODO :gen/fmap, :gen/return, :gen/elements
           (let [props (-merge (m/type-properties schema)
                               (m/properties schema))]
             (-solve-from-schema props options)))
         (-solve schema options)))))

(defmethod -solve :any [schema options] [{}])
(defmethod -solve :and [schema options] (-intersect (map #(solve % options) (m/children schema))))
(defmethod -solve :or [schema options] (mapcat #(solve % options) (m/children schema)))
(defmethod -solve :<= [schema options] [{:type :number :max-number (first (m/children schema))}])
(defmethod -solve :>= [schema options] [{:type :number :min-number (first (m/children schema))}])
(defmethod -solve :< [schema options] [{:type :number :max-number (math/next-down (first (m/children schema)))}])
(defmethod -solve :> [schema options] [{:type :number :min-number (math/next-up (first (m/children schema)))}])
(defmethod -solve 'pos? [schema options] [{:type :number :min-number (math/next-up 0)}])
(defmethod -solve 'neg? [schema options] [{:type :number :max-number (math/next-down 0)}])
(defmethod -solve :int [schema options] (-min-max-number :int schema options))
(defmethod -solve :double [schema options] (-min-max-number :double schema options))
(defmethod -solve :float [schema options] (-min-max-number :double schema options))
(defmethod -solve 'zero? [schema options] [{:type :number :min-number 0 :max-number 0}])

(defmethod -solve 'coll? [schema options] [{:type :coll}])
(defmethod -solve 'empty? [schema options] (mapv #(do {:type % :min-count 0 :max-count 0}) [:counted :seqable]))
(defmethod -solve 'indexed? [schema options] [{:type :indexed}])
(defmethod -solve 'list? [schema options] [{:type :list}])
(defmethod -solve 'map? [schema options] [{:type :map}])
(defmethod -solve 'seq? [schema options] [{:type :seq}])
(defmethod -solve 'seqable? [schema options] [{:type :seqable}])
(defmethod -solve 'sequential? [schema options] [{:type :sequential}])
(defmethod -solve 'set? [schema options] [{:type :set}])
(defmethod -solve 'vector? [schema options] [{:type :vector}])

(defmethod -solve :default [schema options] [{}])
