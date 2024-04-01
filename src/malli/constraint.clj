(ns malli.constraint
  (:require [clojure.set :as set]
            [malli.impl.util :refer [-fail!]]))

(def composite-constraint-types
  #{:and :or :implies :xor :iff :not})

(def schema-constraints
  {:map {:property-keys (-> composite-constraint-types (disj :not) (conj :disjoint))
         :constraint-types (into composite-constraint-types #{:disjoint :max :min :contains})
         :constraint-remap {:max :max-count
                            :min :min-count}}
   :set {:property-keys (-> composite-constraint-types (disj :not) (conj :disjoint))
         :constraint-types (into composite-constraint-types #{:disjoint :max :min :contains})
         :constraint-remap {:max :max-count
                            :min :min-count}}
   :map-of {:property-keys (-> composite-constraint-types (disj :not) (conj :disjoint))
            :constraint-types (into composite-constraint-types #{:disjoint :max :min :contains})
            :constraint-remap {:max :max-count
                               :min :min-count}}
   :int {:properties (-> composite-constraint-types (disj :not))
         :constraint-types (into composite-constraint-types #{:max :min :< :> :<= :>=})
         :constraint-remap {:max :<=
                            :min :>=}}})

(defn -contains-constraint-key [constraint]
  (if (or (symbol? constraint)
          (keyword? constraint)
          (string? constraint))
    [constraint]
    (when (and (vector? constraint)
               (= :contains (first constraint))
               (or (= 2 (count constraint))
                   (-fail! ::contains-constraint-takes-one-child {:constraint constraint})))
      (subvec constraint 1))))

(defn ->constraint-opts [type-or-map]
  (if (map? type-or-map)
    type-or-map
    (get schema-constraints type-or-map)))

(defn -constraint-validator [constraint constraint-opts options]
  (let [{:keys [constraint-types]} (->constraint-opts constraint-opts)]
    (letfn [(-constraint-validator [constraint]
              (if-some [[k] (when (:contains constraint-types)
                              (-contains-constraint-key constraint))]
                #(contains? % k)
                (let [op (when (vector? constraint)
                           (first constraint))
                      op (or (constraint-types op)
                             (-fail! ::disallowed-constraint {:constraint constraint}))]
                  (case op
                    (:<= :< :>= :>) (let [[n :as all] (subvec constraint 1)
                                          _ (when-not (= 1 (count all))
                                              (-fail! ::numeric-constraint-takes-one-child {:constraint constraint}))
                                          _ (when-not (number? n)
                                              (-fail! ::numeric-constraint-takes-integer {:constraint constraint}))]
                                      (case op
                                        :<  #(<  n %)
                                        :<= #(<= n %)
                                        :>  #(>  n %)
                                        :>= #(>= n %)))
                    (:max-count :min-count) (let [[n :as all] (subvec constraint 1)
                                                  _ (when-not (= 1 (count all))
                                                      (-fail! ::min-max-constraint-takes-one-child {:constraint constraint}))
                                                  _ (when-not (nat-int? n)
                                                      (-fail! ::min-max-constraint-takes-integer {:constraint constraint}))]
                                              (case op
                                                :max-count #(<= (count %) n)
                                                :min-count #(<= n (count %))))
                    :disjoint (let [ksets (next constraint)
                                    ps (mapv (fn [ks]
                                               (when (empty? ks)
                                                 (-fail! ::disjoint-keyset-must-be-non-empty {:constraint constraint}))
                                               (when-not (apply distinct? ks)
                                                 (-fail! ::disjoint-keyset-must-be-distinct {:constraint constraint}))
                                               (when-not (vector? ks)
                                                 (-fail! ::disjoint-constraint-takes-vectors-of-keys {:constraint constraint}))
                                               #(boolean
                                                  (some (fn [k]
                                                          (contains? % k))
                                                        ks)))
                                             ksets)
                                    _ (when (next ksets)
                                        (let [in-multiple (apply set/intersection (map set ksets))]
                                          (when (seq in-multiple)
                                            (-fail! ::disjoint-keyset-must-be-distinct {:in-multiple-keys in-multiple}))))]
                                #(let [rs (keep-indexed (fn [i p]
                                                          (when (p %)
                                                            i))
                                                        ps)]
                                   (or (empty? rs)
                                       (not (next rs)))))
                    :not (let [[p :as all] (next constraint)
                               _ (when-not (= 1 (count all))
                                   (-fail! ::not-constraint-takes-one-child {:constraint constraint}))
                               p (-constraint-validator p)]
                           #(not (p %)))
                    :and (let [ps (mapv -constraint-validator (next constraint))]
                           #(every? (fn [p] (p %)) ps))
                    :or (let [ps (mapv -constraint-validator (next constraint))]
                          #(boolean 
                             (some (fn [p] (p %)) ps)))
                    :xor (let [ps (mapv -constraint-validator (next constraint))]
                           #(let [rs (filter (fn [p] (p %)) ps)]
                              (boolean
                                (and (seq rs) (not (next rs))))))
                    :iff (let [[p & ps] (mapv -constraint-validator (next constraint))]
                           (when-not p
                             (-fail! ::empty-iff))
                           #(let [expect (p %)]
                              (every? (fn [p] (identical? expect (p %))) ps)))
                    :implies (let [[p & ps] (mapv -constraint-validator (next constraint))]
                               (when-not p
                                 (-fail! ::missing-implies-condition {:constraint constraint}))
                               #(or (not (p %))
                                    (every? (fn [p] (p %)) ps)))
                    (-fail! ::unknown-keyset-constraint {:constraint constraint})))))]
      (-constraint-validator constraint))))

(defn -constraint-from-properties [properties constraint-opts options]
  (let [{:keys [property-keys]} (->constraint-opts constraint-opts)]
    (when-some [cs (-> []
                       (cond->
                         (:and property-keys)
                         (into (get properties :and)))
                       (into (keep #(some->> (get properties %)
                                             (into [%]))
                                   (disj property-keys :and)))
                       not-empty)]
      (if (= 1 (count cs))
        (first cs)
        (into [:and] cs)))))
