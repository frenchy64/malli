(ns malli.solver
  (:require [clojure.math :as math]
            [clojure.math.combinatorics :as comb]
            [malli.core :as m]
            [malli.impl.util :refer [-merge]]))

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

(def type-super
  {:int #{:number}})

(defn type-supers [t]
  (into #{} (mapcat #(cons % (type-supers %)))
        (type-super t)))

(comment
  (type-supers :int)
  )

(defn -type-constraints [all-sols]
  (when-some [types (not-empty (into #{} (map :type) all-sols))]
    (let [remove-redundant (reduce (fn [acc t] (apply disj acc (type-supers t))) types types)]
      (mapv #(hash-map :type %) remove-redundant))))

(defn -conj-type-constraints [all-sols]
  (or (-type-constraints all-sols)
      [{}]))

(defn -conj-solutions [& sols]
  (letfn [(rec [cart-sols]
            (lazy-seq
              (when-some [[[sol1 & sols :as all-sols]] (seq cart-sols)]
                (when-some [unsupported-keys (not-empty
                                               (disj (into #{} (mapcat keys) all-sols)
                                                     :type
                                                     :max-count :min-count
                                                     :max-range :min-range))]
                  (m/-fail! ::unsupported-conj-solution {:unsupported-keys unsupported-keys}))
                (let [type-constraints (-conj-type-constraints all-sols)
                      number-solutions (-conj-number-constraints all-sols)
                      combined-sols (comb/cartesian-product
                                      type-constraints
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

(defmulti -constraint-solutions* (fn [constraint options] (m/type constraint)))

(defn- -solutions-from-return [props]
  (when (contains? props :gen/return)
    [{:= (:gen/return props)}]))

(defn- -solutions-from-elements [props]
  (some->> (:gen/elements props) (mapv #(hash-map := %))))

(declare -constraint-solutions)

(defn- -solutions-from-schema [props options]
  (some-> (:gen/schema props) (-constraint-solutions options)))

(defn -constraint-solutions [constraint options]
  {:post [(every? map? %)]}
  (lazy-seq
    (let [props (-merge (m/type-properties constraint)
                        (m/properties constraint))
          sols (or (-solutions-from-return props)
                   (-solutions-from-elements props)
                   (-solutions-from-schema props options)
                   ;; TODO -solutions-from-gen: can we turn a generator into a solution?
                   (-constraint-solutions* constraint options))]
      ;; TODO should we use :gen/fmap from the generating schema?
      ;; [:and [:int] [:fn {:gen/fmap #(* 2 %)} pos?]]
      ;; => [:int {:min 1 :gen/fmap #(* 2 %)}]
      sols)))

(defmethod -constraint-solutions* :any [constraint options] [{}])
;;TODO (defmethod -constraint-solutions* :never [constraint options] [])

(defn -solve-constraints [cs options]
  (apply -conj-solutions (map #(-constraint-solutions % options) cs)))

(defmethod -constraint-solutions* :and [constraint options] (-solve-constraints (m/children constraint) options))

(defmethod -constraint-solutions* :or
  [constraint options]
  (into [] (mapcat #(-constraint-solutions % options)) (m/children constraint)))

(defmethod -constraint-solutions* :count
  [constraint {::keys [mode] :as options}]
  (let [{:keys [min max]} (m/properties constraint)
        {gen-min :gen/min gen-max :gen/max} (when (= :gen mode) (m/properties constraint))]
    (-min-max min max gen-min gen-max :min-count :max-count)))

(defmethod -constraint-solutions* :<= [constraint options] [{:type :number :max-range (first (m/children constraint))}])
(defmethod -constraint-solutions* :>= [constraint options] [{:type :number :min-range (first (m/children constraint))}])
(defmethod -constraint-solutions* :< [constraint options] [{:type :number :max-range (math/next-down (first (m/children constraint)))}])
(defmethod -constraint-solutions* :> [constraint options] [{:type :number :min-range (math/next-up (first (m/children constraint)))}])
(defmethod -constraint-solutions* 'pos? [constraint options] [{:type :number :min-range (math/next-up 0)}])
(defmethod -constraint-solutions* 'neg? [constraint options] [{:type :number :max-range (math/next-down 0)}])
(defmethod -constraint-solutions* :int [constraint {::keys [mode] :as options}]
  (let [{gen-min :gen/min gen-max :gen/max :keys [min max]} (m/properties constraint)]
    [(cond-> {:type :int}
       min (assoc :min-range min)
       max (assoc :max-range max)
       (= :gen mode) (cond->
                       gen-min (assoc :min-range gen-min)
                       gen-max (assoc :max-range gen-max)))]))

(defmethod -constraint-solutions* ::m/range-constraint
  [constraint {::keys [mode] :as options}]
  (let [{:keys [min max]} (m/properties constraint)
        {gen-min :gen/min gen-max :gen/max} (when (= :gen mode) (m/properties constraint))]
    (-min-max min max gen-min gen-max :min-range :max-range)))

(defmethod -constraint-solutions* :default [constraint options]
  (println (str "WARNING: -constraint-solutions* " (pr-str (m/form constraint))))
  [{}])

(comment
  (-constraint-solutions :any nil)
  (-constraint-solutions [:and [:>= -1] [:<= -1]] nil)
  (-constraint-solutions [:and [:>= -1] [:<= -1]] {:gen true})
  (-constraint-solutions [:and :int [:>= -1] [:<= -1]] nil)
  (-constraint-solutions [:and [:int {:gen/min -1}] [:>= -1] [:<= -1]] nil)
  (-constraint-solutions [:and [:int {:gen/min -2}] [:>= -5] [:<= 5]] {:gen false})
  (-constraint-solutions [:and [:int {:gen/min -2}] [:>= -5] [:<= 5]] {:gen true})
  (-constraint-solutions [:or [:>= -5] [:<= 5]] {:gen true})
  (-constraint-solutions [:and :int [:or [:>= -5] [:<= 5]]] {:gen true})
  (-constraint-solutions [:and :int [:or
                                     [:and [:<= -5] [:>= -5]]
                                     [:and [:<= 5] [:>= 5]]]] {:gen true})
  )
