(ns malli.shrinker
  (:refer-clojure :exclude [compare sort sort-by])
  (:require [clojure.core :as c]
            [malli.core :as m]))

(declare sort sort-by compare)

(defmulti -deconstructor
  (fn [schema opts] (m/type schema))
  :default ::default)
(defmulti -divider
  (fn [schema opts] (m/type schema))
  :default ::default)
(defmulti -compare
  (fn [schema left right opts] (m/type schema))
  :default ::default)

(defmethod -deconstructor ::default [_ _] (fn [_ _]))
(defmethod -divider ::default [_ _] (fn [_ _]))
(defmethod -compare ::default [_ _ _ _] :unknown)

(defmethod -deconstructor :tuple [schema opts]
  (let [cs (m/children schema)]
    (fn [v path]
      (map-indexed (fn [i v]
                     {:schema (nth cs i)
                      :path (conj path i)
                      :vals [v]})
                   v))))

(defmethod -compare :tuple [schema left right opts]
  (let [children (m/children schema)]
    (reduce (fn [_ i]
              (let [r (-compare (nth children i) (nth left i) (nth right i) opts)]
                (case r
                  :unknown (reduced :unknown)
                  :equal :equal
                  (:left-smaller :right-smaller) r)))
            :equal (range (count children)))))

(defn -core-compare
  ([left right] (-core-compare identity left right))
  ([f left right]
   (let [r (c/compare (f left) (f right))]
     (cond
       (zero? r) :equal
       (neg? r) :left-smaller
       :else :right-smaller))))

(defmethod -compare :int [schema left right opts] (-core-compare (juxt abs neg?) left right))
(defmethod -compare :boolean [schema left right opts] (-core-compare left right))
(defmethod -compare :symbol [schema left right opts] (-core-compare left right))
(defmethod -compare :keyword [schema left right opts] (-core-compare left right))

(defmethod -compare :enum [schema left right opts]
  (-core-compare (into {} (map-indexed (fn [i v] [v i])) (m/children schema)) left right))

(defmethod -compare :maybe [schema left right opts]
  (cond
    (and (nil? left) (nil? right)) :equal
    (nil? left) :left-smaller
    (nil? right) :right-smaller
    :else (compare (first (m/children schema)) left right opts)))

(defmethod -compare :orn [schema left right opts]
  (let [parse (m/parser schema)
        lp (:key (parse left))
        rp (:key (parse right))
        co (-core-compare (into {} (map-indexed (fn [i [k]] [k i])) (m/children schema)) lp rp)]
    (case co
      (:left-smaller :right-smaller :unknown) co
      :equal (compare (m/-get schema lp nil) left right opts))))

(defmethod -compare :schema [schema left right opts] (compare (m/deref schema) left right opts))
(defmethod -compare ::m/schema [schema left right opts] (compare (m/deref schema) left right opts))
(defmethod -compare :ref [schema left right opts] (compare (m/deref schema) left right opts))
(defmethod -compare :merge [schema left right opts] (compare (m/deref schema) left right opts))
(defmethod -compare :union [schema left right opts] (compare (m/deref schema) left right opts))
(defmethod -compare :select-keys [schema left right opts] (compare (m/deref schema) left right opts))

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

(defmethod -compare :map-of [schema left right opts]
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
                      :equal (range cl))))))

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

(defn compare
  ([?schema left right] (compare ?schema left right nil))
  ([?schema left right opts] (-compare (m/schema ?schema opts) left right opts)))

(defn smaller?
  ([?schema left right] (smaller? ?schema left right nil))
  ([?schema left right opts] (= :left-smaller (compare ?schema left right opts))))

(defn larger?
  ([?schema left right] (larger? ?schema left right nil))
  ([?schema left right opts] (= :right-smaller (compare ?schema left right opts))))

(defn sort
  ([?schema vs] (sort ?schema vs nil))
  ([?schema vs opts]
   (let [s (m/schema ?schema opts)
         sortable? (volatile! true)
         sorted (c/sort #(case (compare s % %2 opts)
                           :left-smaller -1
                           :equal 0
                           :right-smaller 1
                           :unknown (do (vreset! sortable? false)
                                        0))
                        vs)]
     (when @sortable?
       sorted))))

(defn sort-by
  ([?schema f vs] (sort ?schema vs nil))
  ([?schema f vs opts]
   (let [s (m/schema ?schema opts)
         sortable? (volatile! true)
         sorted (c/sort-by f
                           #(case (compare s % %2 opts)
                              :left-smaller -1
                              :equal 0
                              :right-smaller 1
                              :unknown (do (vreset! sortable? false)
                                           0))
                           vs)]
     (when @sortable?
       sorted))))
