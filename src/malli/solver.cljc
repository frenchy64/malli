(ns malli.solver
  (:require [clojure.core :as c]
            [clojure.math :as math]
            [clojure.math.combinatorics :as comb]
            [clojure.set :as set]
            [malli.core :as m]
            [malli.impl.util :refer [-merge]]
            [malli.util :as mu]))

(declare solve -intersect)

(defn- -child [schema] (first (m/children schema)))

(defn- -intersect-number-constraints [cmp all-sols mink maxk]
  (let [maxv (some->> (seq (keep maxk all-sols)) (apply c/min))
        minv (some->> (seq (keep mink all-sols)) (apply c/max))]
    (if (and minv maxv)
      (if (cmp minv maxv)
        [{mink minv
          maxk maxv}]
        [])
      (if minv
        [{mink minv}]
        (when maxv
          [{maxk maxv}])))))

(defn- -merge-number-constraints [{:keys [min-number max-number >-number <-number] :as sol}]
  (prn "-merge-number-constraints" sol)
  (if (or min-number >-number max-number <-number)
    (let [[min-type min] (if (and min-number >-number)
                           (if (<= min-number >-number) [:>-number >-number] [:min-number min-number])
                           (or (some->> >-number (vector :>-number)) (some->> min-number (vector :min-number))))
          [max-type max] (if (and max-number <-number)
                           (if (<= <-number max-number) [:<-number <-number] [:max-number max-number])
                           (or (some->> <-number (vector :<-number)) (some->> max-number (vector :max-number))))]
      (when (or (not min-type)
                (not max-type)
                (and (= :min-number min-type)
                     (= :max-number max-type))
                (< min max))
        (cond-> (dissoc sol :min-number :max-number :>-number :<-number)
          min (assoc min-type min)
          max (assoc max-type max))))
    sol))

(defn- -intersect-min-max [all-sols]
  (if-some [sols (when (seq all-sols)
                   (not-empty
                     (into [] (keep (fn [[cmp mink maxk]]
                                      (-intersect-number-constraints cmp all-sols mink maxk)))
                           [[<= :min-count :max-count]
                            [<= :min-number :max-number]
                            [< :>-number :<-number]])))]
    (lazy-seq
      (->> (apply comb/cartesian-product sols)
           (keep (comp -merge-number-constraints #(apply merge %)))))
    [{}]))

(defn -intersect-elements [all-sols]
  (if-some [elements (some-> (seq (keep :elements all-sols)) -intersect)]
    (if (empty? all-sols)
      []
      [{:elements elements}])
    [{}]))

(def ^:private type-super
  {:integer #{:number}
   :int #{:number}
   :list #{:counted :sequential :coll}
   :vector #{:counted :indexed :sequential :coll}
   :map #{:counted :indexed :coll}
   :set #{:counted :indexed :coll}
   :sequential #{:coll}
   :coll #{:seqable}})

(defn- -type-supers [t] (into #{} (mapcat #(cons % (-type-supers %))) (type-super t)))

(defn- -type-constraints [all-sols]
  (when-some [types (not-empty (into #{} (keep :type) all-sols))]
    (let [remove-redundant (reduce (fn [acc t] (apply disj acc (-type-supers t))) types types)]
      (mapv #(hash-map :type %) remove-redundant))))

(defn- -intersect-type [all-sols] (or (-type-constraints all-sols) [{}]))

(defn- -intersect-map [all-sols]
  (let [keysets (keep :keyset all-sols)
        gets (keep :get all-sols)
        open-maps (keep :open-map all-sols)
        keyss (keep :keys all-sols)
        valss (keep :vals all-sols)
        default-keyss (keep :default-keys all-sols)
        default-valss (keep :default-vals all-sols)
        intersect-get (apply merge-with #(-intersect %&) gets)
        intersect-keyset (apply merge-with
                                (fn [l r]
                                  (or (when (= l r) l)
                                      (when (and (every? #{:present :absent} [l r])
                                                 (not= l r))
                                        :contradiction)
                                      (when (some #{:absent} [l r]) :absent)
                                      (when (some #{:present} [l r]) :present)
                                      (m/-fail! ::unrecognized-keyset-solution {:l l :r r})))
                                keysets)
        unsatisfiable-keyset (some #{:contradiction} (vals intersect-keyset))
        intersect-open-map (some->> (seq open-maps) (not-any? false?))
        absent-keys (mapcat (fn [[k v]] (when (= :absent v) [k])) intersect-keyset)
        intersect-get (apply dissoc intersect-get absent-keys)
        intersect-keys (-intersect keyss)
        intersect-vals (-intersect valss)
        intersect-default-keys (-intersect default-keyss)
        intersect-default-vals (-intersect default-valss)
        contradiction (or (and unsatisfiable-keyset [:unsatisfiable-keyset intersect-keyset])
                          (some #(when (empty? (val %)) %)
                                (into (into {} (map (fn [[k v]] [[:intersect-get k] v])) intersect-get)
                                      (cond-> {:intersect-keys intersect-keys 
                                               :intersect-vals intersect-vals
                                               :intersect-default-keys intersect-default-keys 
                                               :intersect-default-vals intersect-default-vals}
                                        ;; how does this interact with :open-map?
                                        #_#_
                                        (seq intersect-keys) (assoc :intersect-keys+get
                                                                    (-intersect (concat intersect-keys
                                                                                        (map #(do [{:= #{%}}]) (keys intersect-get)))))
                                        #_#_
                                        (seq intersect-vals) (assoc :intersect-vals+get
                                                                    (-intersect (concat intersect-vals
                                                                                        (vals intersect-get))))
                                        ))))]
    (cond-> []
      (not contradiction)
      (conj (cond-> {}
              (seq intersect-keyset) (assoc :keyset intersect-keyset)
              (seq intersect-get) (assoc :get intersect-get)
              (not= [{}] intersect-keys) (assoc :keys intersect-keys)
              (not= [{}] intersect-vals) (assoc :vals intersect-vals)
              (some? intersect-open-map) (assoc :open-map intersect-open-map))))))

(defn -intersect [sols]
  (letfn [(rec [cart-sols]
            (lazy-seq
              (when-some [[[sol1 & sols :as all-sols]] (seq cart-sols)]
                (when-some [unsupported-keys (not-empty
                                               (disj (into #{} (mapcat keys) all-sols)
                                                     :type
                                                     :keys :vals :default-keys :default-vals
                                                     :keyset :get :open-map
                                                     :max-count :min-count
                                                     :max-number :min-number
                                                     :<-number :>-number
                                                     :elements))]
                  (m/-fail! ::unsupported-solution {:unsupported-keys unsupported-keys}))
                (let [type-constraints (-intersect-type all-sols)
                      number-solutions (-intersect-min-max all-sols)
                      ;;TODO contains number constraints
                      map-constraints (-intersect-map all-sols)
                      elements-constraints (-intersect-elements all-sols)
                      combined-sols (comb/cartesian-product type-constraints number-solutions map-constraints
                                                            elements-constraints)]
                  (if (empty? combined-sols)
                    []
                    ;;TODO check solutions are compatible and/or merge them
                    ;; e.g., [:and [:map [:a :int]] empty?] gets count from two different places
                    (concat (map #(apply merge %) combined-sols)
                            (rec (rest cart-sols))))))))]
    (if (empty? sols)
      [{}]
      (distinct (rec (apply comb/cartesian-product (distinct sols)))))))

(defn- -min-max-* [stype schema mink maxk {::keys [mode]}]
  (let [{gen-min :gen/min gen-max :gen/max :keys [min max]} (m/properties schema)]
    [(cond-> {:type stype}
       min (assoc mink min)
       max (assoc maxk max)
       (= :gen mode) (cond->
                       gen-min (assoc mink gen-min)
                       gen-max (assoc maxk gen-max)))]))

(defn- -min-max-number [stype schema options] (-min-max-* stype schema :min-number :max-number options))

(defmulti -solve (fn [schema options] (m/type schema)))

(defn- -solve-from-schema [props options] (some-> (:gen/schema props) (solve options)))

(defn solve
  "Returns a sequence of maps each describing values that satisfy schema.
  
  Options:
  - ::mode if :gen, consider generative fields like :gen/schema and :gen/min
           as necessary to satify the schema."
  ([?schema] (solve ?schema nil))
  ([?schema {::keys [mode] :as options}]
   (let [schema (m/schema ?schema options)]
     (lazy-seq
       (or (when (= :gen mode) ;;TODO :gen/fmap, :gen/return, :gen/elements
             (let [props (-merge (m/type-properties schema)
                                 (m/properties schema))]
               (-solve-from-schema props options)))
           (-solve schema options))))))

(defn -union [sols] (apply concat sols))

(defmethod -solve 'float? [schema options] [{:type :double}])
(defmethod -solve 'int? [schema options] [{:type :int}])
(defmethod -solve 'integer? [schema options] [{:type :integer}])
(defmethod -solve 'nat-int? [schema options] [{:type :int :min-number 0}])
(defmethod -solve 'neg-int? [schema options] [{:type :int :max-number -1}])
(defmethod -solve 'neg? [schema options] [{:type :number :<-number 0}])
(defmethod -solve 'number? [schema options] [{:type :number}])
(defmethod -solve 'pos-int? [schema options] [{:type :int :min-number 1}])
(defmethod -solve 'pos? [schema options] [{:type :number :>-number 0}])
(defmethod -solve 'zero? [schema options] [{:type :number :min-number 0 :max-number 0}])
(defmethod -solve :< [schema options] [{:type :number :<-number (-child schema)}])
(defmethod -solve :<= [schema options] [{:type :number :max-number (-child schema)}])
(defmethod -solve :> [schema options] [{:type :number :>-number (-child schema)}])
(defmethod -solve :>= [schema options] [{:type :number :min-number (-child schema)}])
(defmethod -solve :and [schema options] (-intersect (map #(solve % options) (m/children schema))))
(defmethod -solve :any [schema options] [{}])
(defmethod -solve :double [schema options] (-min-max-number :double schema options))
(defmethod -solve :float [schema options] (-min-max-number :double schema options))
(defmethod -solve :int [schema options] (-min-max-number :int schema options))
(defmethod -solve :or [schema options] (-union (map #(solve % options) (m/children schema))))

;;TODO test
(defmethod -solve := [schema options] [{:= #{(-child schema)}}])
(defmethod -solve :enum [schema options] [{:= (set (m/children schema))}])
(defmethod -solve :not= [schema options] [{:not= #{(-child schema)}}])

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

(defn- -min-max-count [stype schema options] (-min-max-* stype schema :min-count :max-count options))

(defmethod -solve :tuple [schema options]
  (let [children (m/children schema)
        len (count children)
        sols (into {} (map-indexed (fn [i c] [i (solve c options)])) children)]
    [{:type :vector
      :min-count len :max-count len
      :elements (-union (vals sols))
      :nth sols}]))

(defn- -solve-collection-schema [stype schema options]
  (mapv #(assoc % :elements (solve (-child schema) options))
        (-min-max-count stype schema options)))

(defmethod -solve :vector [schema options] (-solve-collection-schema :vector schema options))
(defmethod -solve :seqable [schema options] (-solve-collection-schema :seqable schema options))
(defmethod -solve :every [schema options] (-solve-collection-schema :seqable schema options))
(defmethod -solve :set [schema options] (-solve-collection-schema :set schema options))
(defmethod -solve :sequential [schema options] (-solve-collection-schema :sequential schema options))

(defmethod -solve :merge [schema options] (solve (m/deref schema) options))
(defmethod -solve :union [schema options] (solve (m/deref schema) options))
(defmethod -solve :select-keys [schema options] (solve (m/deref schema) options))

(defmethod -solve :map-of [schema options]
  (let [[ks vs] (mapv #(solve % options) (m/children schema))
        extra (cond-> {:open-map false}
                (not= ks [{}]) (assoc :keys ks)
                (not= vs [{}]) (assoc :vals vs))]
    (mapv #(into % extra) (-min-max-count :map schema options))))

(defmethod -solve :map [schema options]
  (let [default (m/default-schema schema)
        entries (into {} (comp (if default
                                 (remove m/-default-entry)
                                 identity)
                               (map (fn [[k v]]
                                      [k {:props (m/properties v)
                                          :schema (mu/get v 0)}])))
                      (m/entries schema))
        base-keyset (into {} (map (fn [[k {{:keys [optional]} :props}]]
                                    [k (if optional :optional :present)]))
                          entries)
        base-get (into {} (map (fn [[k {:keys [schema]}]]
                                 [k (solve schema options)]))
                       entries)
        {base-closed :closed} (m/properties schema)
        default-solutions (when default
                            (mapv (fn [solution]
                                    (cond-> solution
                                      (:get solution) (update :get #(apply dissoc % (keys base-keyset)))
                                      (:keyset solution) (update :keyset #(apply dissoc % (keys base-keyset)))))
                                  (solve default options)))
        closed (when-not default (-> schema m/properties :closed boolean))
        min-count (count (remove (comp :optional :props val) entries))
        max-count (when closed (count entries))
        base-solution (cond-> {:type :map}
                        (seq base-get) (assoc :get base-get)
                        (seq base-keyset) (assoc :keyset base-keyset)
                        (some? closed) (assoc :open-map (not closed))
                        (pos? min-count) (assoc :min-count min-count)
                        max-count (assoc :max-count max-count))]
    (into [] (mapcat #(-intersect
                        [[(dissoc % :keys :vals)]
                         [(cond-> base-solution
                            (:keys %) (assoc :keys (:keys %))
                            (:vals %) (assoc :vals (:vals %)))]]))
          (or default-solutions [{}]))))

(defmethod -solve :default [schema options] [{}])
