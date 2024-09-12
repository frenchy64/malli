(ns malli.constraint.solver
  (:require [clojure.math.combinatorics :as comb]
            [clojure.set :as set]
            [malli.core :as m]
            [malli.constraint :as mc]
            [malli.constraint.protocols :as mcp]
            [malli.impl.util :as miu]))

(defn -conj-number-constraints [[sol1 & sols :as all-sols]]
  (if (empty? all-sols)
    [{}]
    (let [max-count (some->> (seq (keep :max-count all-sols)) (apply min))
          min-count (some->> (seq (keep :min-count all-sols)) (apply max))
          count-solutions (lazy-seq
                            (if (and min-count max-count)
                              (if (<= min-count max-count)
                                ;; TODO exact count
                                [{:min-count min-count
                                  :max-count max-count}]
                                [])
                              (if min-count
                                [{:min-count min-count}]
                                (if max-count
                                  [{:max-count max-count}]
                                  [{}]))))
          all-solutions [count-solutions]]
      (lazy-seq
        (->> (apply comb/cartesian-product all-solutions)
             (map #(apply merge %)))))))

(comment
 (map #(apply merge %) (comb/cartesian-product [{:< 1} {:> 2}] [{:max-count 1} {:min-count 3}]))
 (assert (= (-conj-number-constraints [{:max-count 5 :> 3} {:min-count 4 :< 4}])
            [{:> 3, :< 4, :min-count 4, :max-count 5}]))
 (assert (= (-conj-number-constraints [{:max-count 5 :> 3} {:min-count 4 :> 4}])
            [{:> 4, :min-count 4, :max-count 5}]))

)

(defn -conj-solutions [& sols]
  (letfn [(rec [cart-sols]
            (lazy-seq
              (when-some [[[sol1 & sols :as all-sols]] (seq cart-sols)]
                (when-some [unsupported-keys (not-empty
                                               (disj (into #{} (mapcat keys) all-sols)
                                                     :max-count :min-count))]
                  (miu/-fail! ::unsupported-conj-solution {:unsupported-keys unsupported-keys}))
                (let [number-solutions
                      (if (not-any? (some-fn :max-count :min-count) all-sols)
                        [{}]
                        (-conj-number-constraints all-sols))
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

(defn- -min-max-count [min max gen-min gen-max]
  (cond
    (and min gen-min (< gen-min min)) []
    (and max gen-max (> gen-max max)) []
    :else (let [min (or gen-min min)
                max (or gen-max max)]
            [(cond-> {}
               (some-> min pos?) (assoc :min-count min)
               max (assoc :max-count max))])))

(defmulti -constraint-solutions* (fn [constraint constraint-opts options] (m/type constraint)))

(defn -constraint-solutions [constraint constraint-opts options]
  {:post [(every? map? %)]}
  (assert (mcp/-constraint? constraint) (pr-str (class constraint)))
  (lazy-seq
    (-constraint-solutions* constraint constraint-opts options)))

(defmethod -constraint-solutions* ::mc/true-constraint [constraint constraint-opts options] [{}])
(defmethod -constraint-solutions* ::mc/and
  [constraint constraint-opts options]
  (apply -conj-solutions (map #(-constraint-solutions % constraint-opts options) (unchunk (next constraint)))))
(defmethod -constraint-solutions* ::mc/count-constraint
  [constraint constraint-opts {::keys [mode] :as options}]
  (let [[min max] (m/children constraint)
        {gen-min :gen/min gen-max :gen/max} (when (= :gen mode) (m/properties constraint))]
    (-min-max-count min max gen-min gen-max)))
(defmethod -constraint-solutions* :default [constraint constraint-opts options] (miu/-fail! ::unknown-constraint {:constraint constraint}))
