(ns malli.shrinker
  (:refer-clojure :exclude [compare sort sort-by comparator])
  (:require [clojure.core :as c]
            [malli.core :as m]))

(declare sort compare comparator sorter-by)

(defmulti -deconstructor
  (fn [schema opts] (m/type schema))
  :default ::default)
(defmulti -divider
  (fn [schema opts] (m/type schema))
  :default ::default)
(defmulti -comparator
  (fn [schema opts] (m/type schema))
  :default ::default)
(defmulti -differ
  (fn [schema path in opts] (m/type schema))
  :default ::default)

(defmethod -deconstructor ::default [_ _] (fn [_ _]))
(defmethod -divider ::default [_ _] (fn [_ _]))
(defmethod -comparator ::default [s _]
  (prn "unsupported: " (m/type s))
  (fn [_ _] :unknown))
(defmethod -differ ::default [s path in opts]
  (prn "unsupported: " (m/type s))
  (fn [left right]
    [{:result :unknown
      :path path
      :in in
      :left left
      :right right}]))

(defmethod -deconstructor :tuple [schema opts]
  (let [cs (m/children schema)]
    (fn [v path]
      (map-indexed (fn [i v]
                     {:schema (nth cs i)
                      :path (conj path i)
                      :vals [v]})
                   v))))

(defmethod -comparator :tuple [schema opts]
  (let [;; TODO sort by schema complexity
        comparators (mapv #(-comparator % opts) (m/children schema))
        nchildren (count comparators)]
    (fn [left right]
      (reduce (fn [_ i]
                (let [r ((nth comparators i) (nth left i) (nth right i))]
                  (case r
                    (:smaller :larger :unknown) (reduced r)
                    :equal r)))
              :equal (range nchildren)))))

(defmethod -differ :tuple [schema path in opts]
  (let [differs (into [] (map-indexed (fn [i c] (-differ c (conj path i) (conj in i) opts))) (m/children schema))]
    (fn [left right]
      (not-empty
        (into [] (comp (map-indexed
                         (fn [i f]
                           (f (nth left i) (nth right i))))
                       cat)
              differs)))))

(defn -core-comparator
  ([] (-core-comparator identity))
  ([f]
   (fn [left right]
     (let [r (c/compare (f left) (f right))]
       (cond
         (zero? r) :equal
         (neg? r) :smaller
         :else :larger)))))

(defmethod -comparator :int [schema opts] (-core-comparator (juxt abs neg?)))
(defmethod -comparator :boolean [schema opts] (-core-comparator))
(defmethod -comparator :symbol [schema opts] (-core-comparator))
(defmethod -comparator :keyword [schema opts] (-core-comparator))

(defn -leaf-differ [schema comparator path in opts]
  (fn [left right]
    (let [r (comparator left right)]
       (when-not (= :equal r)
         [{:result r
           :schema schema
           :path path
           :in in
           :left left
           :right right}]))))

(defmethod -differ :int [schema path in opts] (-leaf-differ schema (-core-comparator (juxt abs neg?)) path in opts))
(defmethod -differ :boolean [schema path in opts] (-leaf-differ schema (-core-comparator) path in opts))
(defmethod -differ :symbol [schema path in opts] (-leaf-differ schema (-core-comparator) path in opts))
(defmethod -differ :keyword [schema path in opts] (-leaf-differ schema (-core-comparator) path in opts))

(defmethod -comparator :enum [schema opts]
  (-core-comparator (into {} (map-indexed (fn [i v] [v i])) (m/children schema))))

(defmethod -comparator :maybe [schema opts]
  (let [cmp (-comparator (first (m/children schema)) opts)]
    (fn [left right]
      (cond
        (and (nil? left) (nil? right)) :equal
        (nil? left) :smaller
        (nil? right) :larger
        :else (cmp left right)))))

(defmethod -comparator :orn [schema opts]
  (let [cmp (-core-comparator)
        validators (mapv (comp m/validator peek) (m/children schema))
        nchildren (count validators)
        clause-key (fn [v]
                     (or (some (fn [i]
                                 (when ((nth validators i) v)
                                   i))
                               (range nchildren))
                         (throw (ex-info "unmatched" {:schema schema
                                                      :value v}))))
        ;; TODO tie-the-knot for eager computation
        k->cmp (into {} (map-indexed (fn [i [_ _ s]] [i #((comparator s opts) % %2)])) (m/children schema))]
    (fn [left right]
      (let [lp (clause-key left)
            rp (clause-key right)
            co (cmp lp rp)]
        (case co
          (:smaller :larger :unknown) co
          :equal ((k->cmp lp) left right))))))

(defmethod -comparator :or [schema opts]
  (let [cmp (-core-comparator)
        validators (mapv m/validator (m/children schema))
        nchildren (count validators)
        clause-key (fn [v]
                     (or (some (fn [i]
                                 (when ((nth validators i) v)
                                   i))
                               (range nchildren))
                         (throw (ex-info "unmatched" {:schema schema
                                                      :value v}))))
        ;; TODO tie-the-knot for eager computation
        k->cmp (mapv (fn [s] #((comparator s opts) % %2)) (m/children schema))]
    (fn [left right]
      (let [lp (clause-key left)
            rp (clause-key right)
            co (cmp lp rp)]
        (case co
          (:smaller :larger :unknown) co
          :equal ((k->cmp lp) left right))))))

(defmethod -comparator :schema [schema opts] (-comparator (m/deref schema) opts))
(defmethod -comparator ::m/schema [schema opts] (-comparator (m/deref schema) opts))
(defmethod -comparator :ref [schema opts] (-comparator (m/deref schema) opts))
(defmethod -comparator :merge [schema opts] (-comparator (m/deref schema) opts))
(defmethod -comparator :union [schema opts] (-comparator (m/deref schema) opts))
(defmethod -comparator :select-keys [schema opts] (-comparator (m/deref schema) opts))

(defmethod -differ :schema [schema path in opts] (-differ (m/deref schema) (conj path 0) in opts))
(defmethod -differ ::m/schema [schema path in opts] (-differ (m/deref schema) (conj path 0) in opts))
(defmethod -differ :ref [schema path in opts] (-differ (m/deref schema) (conj path 0) in opts))
(defmethod -differ :merge [schema path in opts] (-differ (m/deref schema) (conj path ::m/in) in opts))
(defmethod -differ :union [schema path in opts] (-differ (m/deref schema) (conj path ::m/in) in opts))
(defmethod -differ :select-keys [schema path in opts] (-differ (m/deref schema) (conj path ::m/in) in opts))

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

(defmethod -divider :any [schema opts]
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
  (let [[ks vs] (m/children schema)
        sort-by-keys (comp vec (sorter-by ks first opts))
        compare-keys (comparator ks opts)
        compare-vals (comparator vs opts)]
    (fn [left right]
      (let [cl (count left)
            cr (count right)]
        (cond
          (< cl cr) :smaller
          (> cl cr) :larger
          (zero? cl) :equal
          :else (let [l (sort-by-keys (seq left))
                      r (sort-by-keys (seq right))]
                  (reduce (fn [_ i]
                            (let [[lk lv] (nth l i)
                                  [rk rv] (nth r i)
                                  rk (compare-keys lk rk)]
                              (case rk
                                (:smaller :larger) rk
                                :equal (let [rv (compare-vals lv rv)]
                                         (case rv
                                           (:smaller :larger :equal) rv
                                           :unknown (reduced :unknown)))
                                :unknown (reduced :unknown))))
                          :equal (range cl))))))))

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

(defn divider
  ([?schema] (divider ?schema nil))
  ([?schema opts] (-divider (m/schema ?schema opts) opts)))

(defn divide
  "Split a value conforming to ?schema into a sequence
  of maps representing the children of the schema/value."
  ([?schema value] (divide ?schema value nil))
  ([?schema value opts] ((divider ?schema opts) value [])))

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

(defn differ
  ([?schema] (differ ?schema nil))
  ([?schema opts] (-differ (m/schema ?schema opts) [] [] opts)))

(defn diff
  ([?schema left right] (diff ?schema left right nil))
  ([?schema left right opts] ((differ ?schema opts) left right)))

(defn smaller-pred
  ([?schema] (smaller-pred ?schema nil))
  ([?schema opts]
   (let [cmp (comparator ?schema opts)]
     (fn [left right]
       (= :smaller (cmp left right))))))

(defn smaller?
  "True if left is strictly smaller than right. False otherwise."
  ([?schema left right] (smaller? ?schema left right nil))
  ([?schema left right opts] ((smaller-pred ?schema opts) left right)))

(defn larger-pred
  ([?schema] (larger-pred ?schema nil))
  ([?schema opts]
   (let [cmp (comparator ?schema opts)]
     (fn [left right]
       (= :larger (cmp left right))))))

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
                               :smaller -1
                               :equal 0
                               :larger 1
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
                                  :smaller -1
                                  :equal 0
                                  :larger 1
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
