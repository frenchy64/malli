(ns malli.constraint.solver
  (:require [clojure.math.combinatorics :as comb]
            [clojure.set :as set]
            [malli.core :as m]
            [malli.constraint :as mc]
            [malli.constraint.protocols :as mcp]
            [malli.impl.util :as miu]))

(defn -number-solutions [min-int max-int mink maxk]
  (if (and min-int max-int)
    (if (<= min-int max-int)
      [{mink min-int
        maxk max-int}]
      [])
    (if min-int
      [{mink min-int}]
      (when max-int
        [{maxk max-int}]))))

(defn -number-constraints [all-sols mink maxk]
  (let [the-max (some->> (seq (keep maxk all-sols)) (apply min))
        the-min (some->> (seq (keep mink all-sols)) (apply max))]
    (-number-solutions the-min the-max mink maxk)))

(defn -conj-number-constraints [all-sols]
  (if-some [sols (when (seq all-sols)
                   (not-empty (into [] (keep (fn [[mink maxk]]
                                               (-number-constraints all-sols mink maxk)))
                                    [[:min-count :max-count]
                                     [:min-range :max-range]])))]
    (lazy-seq
      (->> (apply comb/cartesian-product sols)
           (map #(apply merge %))))
    [{}]))

(defn -conj-solutions [& sols]
  (letfn [(rec [cart-sols]
            (lazy-seq
              (when-some [[[sol1 & sols :as all-sols]] (seq cart-sols)]
                (when-some [unsupported-keys (not-empty
                                               (disj (into #{} (mapcat keys) all-sols)
                                                     :max-count :min-count))]
                  (miu/-fail! ::unsupported-conj-solution {:unsupported-keys unsupported-keys}))
                (let [number-solutions (-conj-number-constraints all-sols)
                      combined-sols (comb/cartesian-product
                                      number-solutions)]
                  (if (empty? combined-sols)
                    []
                    (concat (map #(apply merge %) combined-sols)
                            (rec (rest cart-sols))))))))]
    (distinct (rec (apply comb/cartesian-product (distinct sols))))))

;; math.combinatorics
(defn- unchunk
  "Given a sequence that may have chunks, return a sequence that is 1-at-a-time
lazy with no chunks. Chunks are good for efficiency when the data items are
small, but when being processed via map, for example, a reference is kept to
every function result in the chunk until the entire chunk has been processed,
which increases the amount of memory in use that cannot be garbage
collected."
  [s]
  (lazy-seq
    (when (seq s)
      (cons (first s) (unchunk (rest s))))))

;; math.combinatorics
(defn- join
  "Lazily concatenates a collection of collections into a flat sequence,
  because Clojure's `apply concat` is insufficiently lazy."
  [colls]
  (lazy-seq
   (when-let [s (seq colls)]
     (concat (first s) (join (rest s))))))

;; math.combinatorics
(defn- mapjoin
  "Uses join to achieve lazier version of mapcat (on one collection)"
  [f coll]
  (join (map f coll)))

(defn- -min-max [min max gen-min gen-max mink maxk]
  (cond
    ;; not sure about these cases, why is a smaller :gen/min contradictory?
    (and min gen-min (< gen-min min)) []
    (and max gen-max (> gen-max max)) []
    :else (let [min (or gen-min min)
                max (or gen-max max)]
            (if (and min max (> min max))
              []
              [(cond-> {}
                 min (assoc mink min)
                 max (assoc maxk max))]))))

(defmulti -constraint-solutions* (fn [constraint constraint-opts options] (m/type constraint)))

(defn -constraint-solutions [constraint constraint-opts options]
  {:post [(every? map? %)]}
  (assert (mcp/-constraint? constraint) (pr-str (#?(:cljs type :default class) constraint)))
  (lazy-seq
    (-constraint-solutions* constraint constraint-opts options)))

(defmethod -constraint-solutions* ::mc/true-constraint [constraint constraint-opts options] [{}])

(defmethod -constraint-solutions* ::mc/and
  [constraint constraint-opts options]
  (apply -conj-solutions (map #(-constraint-solutions % constraint-opts options) (m/children constraint))))

(defmethod -constraint-solutions* ::mc/count-constraint
  [constraint constraint-opts {::keys [mode] :as options}]
  (let [[min max] (m/children constraint)
        {gen-min :gen/min gen-max :gen/max} (when (= :gen mode) (m/properties constraint))]
    (assert (<= 0 min)) ;;should this be enforced?
    (-min-max min max gen-min gen-max :min-count :max-count)))

(defmethod -constraint-solutions* ::mc/range-constraint
  [constraint constraint-opts {::keys [mode] :as options}]
  (let [[min max] (m/children constraint)
        {gen-min :gen/min gen-max :gen/max} (when (= :gen mode) (m/properties constraint))]
    (-min-max min max gen-min gen-max :min-range :max-range)))

(defmethod -constraint-solutions* :default [constraint constraint-opts options]
  (miu/-fail! ::unknown-constraint {:constraint constraint}))
