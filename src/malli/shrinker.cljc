(ns malli.shrinker
  (:refer-clojure :exclude [compare sort sort-by comparator])
  (:require [clojure.core :as c]
            [malli.core :as m]))

(declare sort sort-by compare comparator)

(defmulti -deconstructor
  (fn [schema opts] (m/type schema))
  :default ::default)
(defmulti -divider
  (fn [schema opts] (m/type schema))
  :default ::default)
(defmulti -comparator
  (fn [schema opts] (m/type schema))
  :default ::default)

(defmethod -deconstructor ::default [_ _] (fn [_ _]))
(defmethod -divider ::default [_ _] (fn [_ _]))
(defmethod -comparator ::default [_ _ _ _] :unknown)

(defmethod -deconstructor :tuple [schema opts]
  (let [cs (m/children schema)]
    (fn [v path]
      (map-indexed (fn [i v]
                     {:schema (nth cs i)
                      :path (conj path i)
                      :vals [v]})
                   v))))

(defmethod -comparator :tuple [schema opts]
  (let [comparators (mapv #(-comparator % opts) (m/children schema))]
    (fn [left right]
      (reduce (fn [_ i]
                (let [r ((nth comparators i) (nth left i) (nth right i))]
                  (case r
                    :unknown (reduced :unknown)
                    :equal :equal
                    (:left-smaller :right-smaller) r)))
              :equal (range (count comparators))))))

(defn -core-comparator
  ([] (-core-comparator identity))
  ([f]
   (fn [left right]
     (let [r (c/compare (f left) (f right))]
       (cond
         (zero? r) :equal
         (neg? r) :left-smaller
         :else :right-smaller)))))

(defmethod -comparator :int [schema opts] (-core-comparator (juxt abs neg?)))
(defmethod -comparator :boolean [schema opts] (-core-comparator))
(defmethod -comparator :symbol [schema opts] (-core-comparator))
(defmethod -comparator :keyword [schema opts] (-core-comparator))

(defmethod -comparator :enum [schema opts]
  (-core-comparator (into {} (map-indexed (fn [i v] [v i])) (m/children schema))))

(defmethod -comparator :maybe [schema opts]
  (let [cmp (-comparator (first (m/children schema)) opts)]
    (fn [left right]
      (cond
        (and (nil? left) (nil? right)) :equal
        (nil? left) :left-smaller
        (nil? right) :right-smaller
        :else (cmp left right)))))

(defmethod -comparator :orn [schema opts]
  (let [cmp (-core-comparator (into {} (map-indexed (fn [i [k]] [k i])) (m/children schema)))]
    (fn [left right]
      (let [parse (m/parser schema)
            lp (:key (parse left))
            rp (:key (parse right))
            co (cmp lp rp)]
        (case co
          (:left-smaller :right-smaller :unknown) co
          ;;TODO precompute
          :equal (compare (m/-get schema lp nil) left right opts))))))

(defmethod -comparator :schema [schema opts] (-comparator (m/deref schema) opts))
(defmethod -comparator ::m/schema [schema opts] (-comparator (m/deref schema) opts))
(defmethod -comparator :ref [schema opts] (-comparator (m/deref schema) opts))
(defmethod -comparator :merge [schema opts] (-comparator (m/deref schema) opts))
(defmethod -comparator :union [schema opts] (-comparator (m/deref schema) opts))
(defmethod -comparator :select-keys [schema opts] (-comparator (m/deref schema) opts))

(defn -seq-parts [schema opts]
  (let [[c] (m/children schema)]
    (fn [v path]
      [{:schema c
        :path (conj path 0)
        :vals v}])))

(defmethod -deconstructor :set [schema opts] (-seq-parts schema opts))
(defmethod -deconstructor :sequential [schema opts] (-seq-parts schema opts))
(defmethod -deconstructor :seqable [schema opts] (-seq-parts schema opts))
(defmethod -deconstructor :every [schema opts] (-seq-parts schema opts))

(defmethod -deconstructor :any [schema opts]
  (fn [v path]
    (when (coll? v)
      [{:schema schema
        :path path
        ;; allow maps to be deconstructed into their map entries
        :vals v}])))

(defmethod -deconstructor :map-of [schema opts]
  (let [[ks vs] (m/children schema)
        {:keys [min max]} (m/properties schema)]
    (fn [v path]
      (when-some [m (seq v)]
        (let [c (count m)
              kvs (vec (keys v))]
          [{:schema ks
            :path (conj path 0)
            :vals (keys m)}
           {:schema vs
            :path (conj path 1)
            :vals (vals m)}])))))

(defmethod -comparator :map-of [schema opts]
  ;;TODO precompute
  (fn [left right]
    (let [cl (count left)
          cr (count right)]
      (cond
        (< cl cr) :left-smaller
        (> cl cr) :right-smaller
        (zero? cl) :equal
        :else (let [[ks vs] (m/children schema)
                    l (vec (sort-by ks first (seq left)))
                    r (vec (sort-by ks first (seq right)))]
                (reduce (fn [_ i]
                          (let [[lk lv] (nth l i)
                                [rk rv] (nth r i)
                                rk (compare ks lk rk opts)]
                            (case rk
                              (:left-smaller :right-smaller) rk
                              :equal (let [rv (compare vs lv rv opts)]
                                       (case rv
                                         (:left-smaller :right-smaller :equal) rv
                                         :unknown (reduced :unknown)))
                              :unknown (reduced :unknown))))
                        :equal (range cl)))))))

(defmethod -divider :map-of [schema opts]
  (fn [v path]
    ;;TODO
    #_
    (when-some [m (seq v)]
      (let [c (count m)
            kvs (vec (keys v))]
        [{:schema schema
          :path path
          :vals (cond-> [])}
         ]))))

(defmethod -deconstructor :orn [schema opts]
  (let [parse (m/parser schema opts)
        child-deconstructors (into {}
                                   (map (fn [[k _ s]]
                                          [k (fn [v]
                                               ;;TODO cache by eagerly tying knot
                                               ((-deconstructor s opts) v))]))
                                   (m/children schema))]
    (fn [v path]
      (let [p (parse v)
            _ (assert (not= ::m/invalid p))
            {:keys [key value]} p]
        ((child-deconstructors key) value path)))))

(defn -vector-divider [{:keys [min max]} coerce _opts]
  (let [xduce (comp (if min (filter #(<= min (count %))) identity)
                    (if max (filter #(>= max (count %))) identity)
                    (if coerce (map coerce) identity))]
    (fn [v]
      (let [v (vec v)
            c (count v)]
        (when (pos? c)
          (let [mid (quot c 2)]
            (sequence (comp (distinct) xduce)
                      (cond-> [(subvec v 0 mid)
                               (subvec v mid)]
                        (< 3 c) (conj (subvec v 1))
                        (< 2 c) (conj (subvec v 0 (dec c)))))))))))

(defmethod -divider :string [schema opts]
  (-vector-divider (m/properties schema) #(apply str %) opts))

#_
(defn -identify-schema [schema]
  {:scope (-> schema m/-options m/-registry mr/-schemas)
   :form (m/-form schema)})

#_
(defn -recursive-paths [?schema opts]
  (let [schema (m/schema ?schema opts)
        rec-id (#'m/-identify-ref-schema schema)
        r (m/deref-all schema opts)]
    (m/-walk
      schema
      (reify m/Walker
        (-accept [_ s path options] (not (or @result (reset! result (f s path options)))))
        (-inner [this s path options] (when-not @result (m/-walk s this path options)))
        (-outer [_ _ _ _ _]))
      [] opts)))

;; public API

(defn deconstructor
  ([?schema] (deconstructor ?schema nil))
  ([?schema opts] (-deconstructor (m/schema ?schema opts) opts)))

(defn deconstruct
  "Decompose a value conforming to ?schema into a sequence
  of maps representing the children of the schema/value."
  ([?schema value] (deconstruct ?schema value nil))
  ([?schema value opts] ((deconstructor ?schema opts) value [])))

(defn shrinker
  "Takes a schema and
  returns a seq of deconstructor parts of value that
  still conform to the overall schema."
  [?schema opts]
  (let [schema (m/deref-all (m/schema ?schema opts))
        parse (m/parser schema opts)
        unparse (m/unparser schema opts)]
    (fn [value]
      (let [p (parse value)
            _ (assert (not= p ::m/invalid))]
        ))))

(defn shrink
  "Takes a schema and a value conforming to it,
  returns a seq of deconstructor parts of value that
  still conform to the overall schema."
  ([?schema value] (shrink ?schema value nil))
  ([?schema value opts]
   ((shrinker ?schema opts) value)))

(defn comparator
  ([?schema] (comparator ?schema nil))
  ([?schema opts] (-comparator (m/schema ?schema opts) opts)))

(defn compare
  ([?schema left right] (compare ?schema left right nil))
  ([?schema left right opts] ((comparator ?schema opts) left right)))

(defn smaller-pred
  ([?schema] (smaller-pred ?schema nil))
  ([?schema opts]
   (let [cmp (comparator ?schema opts)]
     (fn [left right]
       (= :left-smaller (cmp left right))))))

(defn smaller?
  "True if left is strictly smaller than right. False otherwise."
  ([?schema left right] (smaller? ?schema left right nil))
  ([?schema left right opts] ((smaller-pred ?schema opts) left right)))

(defn larger-pred
  ([?schema] (larger-pred ?schema nil))
  ([?schema opts]
   (let [cmp (comparator ?schema opts)]
     (fn [left right]
       (= :right-smaller (cmp left right))))))

(defn larger?
  "True if left is strictly larger than right. False otherwise."
  ([?schema left right] (larger? ?schema left right nil))
  ([?schema left right opts] ((larger-pred ?schema opts) left right)))

(defn sorter
  ([?schema] (sorter ?schema nil))
  ([?schema opts]
   (let [cmp (comparator ?schema opts)]
     (fn [vs]
       (let [sortable? (volatile! true)
             sorted (c/sort #(case (cmp % %2)
                               :left-smaller -1
                               :equal 0
                               :right-smaller 1
                               :unknown (do (vreset! sortable? false)
                                            0))
                            vs)]
         (when @sortable?
           sorted))))))

(defn sort
  "Sort vs, a collection of values assumed to pass ?schema.
  If unsortable, returns nil."
  ([?schema vs] (sort ?schema vs nil))
  ([?schema vs opts] ((sorter ?schema opts) vs)))

(defn sorter-by
  ([?schema f] (sorter-by ?schema f nil))
  ([?schema f opts]
   (let [cmp (comparator ?schema opts)]
     (fn [vs]
       (let [sortable? (volatile! true)
             sorted (c/sort-by f
                               #(case (cmp % %2)
                                  :left-smaller -1
                                  :equal 0
                                  :right-smaller 1
                                  :unknown (do (vreset! sortable? false)
                                               0))
                               vs)]
         (when @sortable?
           sorted))))))

(defn sort-by
  "Sort vs, a collection of values where (f v) is assumed to pass ?schema.
  If unsortable, returns nil."
  ([?schema f vs] (sort-by ?schema f vs nil))
  ([?schema f vs opts] ((sorter-by ?schema f opts) vs)))
