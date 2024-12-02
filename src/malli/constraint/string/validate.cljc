(ns malli.constraint.string.validate
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [malli.core :as-alias m]
            [malli.constraint.char :as char]
            [malli.constraint.string.util :refer [code-point-seq]]
            [malli.impl.util :as miu]))

(defn- -flip? [{:keys [constraint]} _]
  (false? (second constraint)))

(defn- -wrap [f] (fn [constraint-opts options]
                   (cond-> f
                     ;;TODO :alpha false == [:not :alpha]
                     ;(-flip? constraint-opts options)
                     ;complement
                     )))
(defn- -idempotent [f] (-wrap (fn [s] (= s (f s)))))

(defn- -reverse [s]
  #?(:clj (str (.reverse (StringBuilder. ^String s)))
     :cljs (miu/-fail! ::reverse-string-cljs-nyi)))

(defn- -distinct-code-points? [s]
  (or (zero? (count s))
      (apply distinct? (code-point-seq s))))

(defn- -string-code-points-in [min max]
  #?(:cljs (miu/-fail! ::string-code-points-in-cljs-nyi)
     :clj (if (and (some-> min zero?)
                   (not max))
            (fn [_] true)
            (if (and (= 1 min)
                     (some-> max pos?))
              (fn [s] (pos? (count s)))
              (fn [s]
                (or (let [max-code-points (count s)
                          min (or min 0)
                          max (or max ##Inf)]
                      (and (<= min max-code-points max)
                           (let [min-code-points (+ (quot max-code-points 2)
                                                    (rem max-code-points 2))]
                             (<= min min-code-points max))))
                    (loop [cs (code-point-seq s)
                           len 0]
                      (if min
                        (if max
                          (if (empty? cs)
                            (<= min len max)
                            (if (< len max)
                              false
                              (recur (next cs) (inc len))))
                          (if (<= min len)
                            (if (empty? cs)
                              false
                              (recur (next cs) (inc len)))))
                        (if max
                          (if (empty? cs)
                            (<= len max)
                            (if (< max len)
                              false
                              (recur (next cs) (inc len)))))))))))))

(defn validators []
  {:max-code-points-string (fn [{[_ max :as constraint] :constraint} _]
                             (when-not (= 2 (count constraint))
                               (miu/-fail! ::max-code-points-takes-one-child {:constraint constraint}))
                             (-string-code-points-in nil max))
   :min-code-points-string (fn [{[_ min :as constraint] :constraint} _]
                             (when-not (= 2 (count constraint))
                               (miu/-fail! ::min-code-points-takes-one-child {:constraint constraint}))
                             (-string-code-points-in min nil))
   :max-chars-string (fn [{[_ max :as constraint] :constraint} _]
                       (when-not (= 2 (count constraint))
                         (miu/-fail! ::max-chars-constraint-takes-one-child {:constraint constraint}))
                       (fn [s] (<= (count s) max)))
   :min-chars-string (fn [{[_ min :as constraint] :constraint} _]
                       (when-not (= 2 (count constraint))
                         (miu/-fail! ::min-chars-constraint-takes-one-child {:constraint constraint}))
                       (fn [s] (<= (count s) max)))
   :alpha-string (-wrap (fn [s] (every? char/alpha? s)))
   :non-alpha-string (-wrap (fn [s] (not-any? char/alpha? s)))
   :numeric-string (-wrap (fn [s] (every? char/numeric? s)))
   :non-numeric-string (-wrap (fn [s] (not-any? char/numeric? s)))
   :alphanumeric-string (-wrap (fn [s] (every? char/alphanumeric? s)))
   :non-alphanumeric-string (-wrap (fn [s] (not-any? char/alphanumeric? s)))
   :distinct-code-points-string (-wrap -distinct-code-points?)
   :palindrome-string (-idempotent -reverse)
   :trim-string (-idempotent str/trim)
   :triml-string (-idempotent str/triml)
   :trimr-string (-idempotent str/trimr)
   :trim-newline-string (-idempotent str/trim-newline)
   :blank-string (-wrap str/blank?)
   :non-blank-string (-wrap (complement str/blank?))
   :escapes-string (fn [{:keys [constraint]} _]
                     (when-not (= 2 (count constraint))
                       (miu/-fail! ::escapes-constraint-takes-one-child {:constraint constraint}))
                     (let [m (not-empty (nth constraint 1))
                           _ (when-not (map? m)
                               (miu/-fail! ::escapes-constraint-takes-non-empty-map-child {:constraint constraint}))
                           _ (run! (fn [s]
                                     (or (if (string? s)
                                           (not-any? #(contains? m %) s)
                                           (not (contains? m s)))
                                         (miu/-fail! ::escape-constraint-map-cannot-overlap-keys-vals)))
                                   (vals m))
                           not-allowed (into #{} (map (fn [c]
                                                        (when-not (char? c)
                                                          (miu/-fail! ::escapes-constraint-map-takes-characters
                                                                      {:constraint constraint}))
                                                        c))
                                             (keys m))]
                       (fn [s]
                         (not-any? not-allowed s))))
   :includes-string (fn [{:keys [constraint value]} _]
                      (when-not (= 2 (count constraint))
                        (miu/-fail! ::includes-constraint-takes-one-child
                                    {:constraint constraint}))
                      (let [s (second constraint)
                            _ (when-not (string? s)
                                (miu/-fail! ::includes-constraint-takes-string-child
                                            {:constraint constraint}))]
                        (fn [v]
                          (str/includes? v s))))
   :edn-string (fn [{:keys [constraint value]} {::m/keys [schema validator -regex-op?]}]
                 (assert (and schema validator -regex-op?))
                 ;;TODO schema arg
                 (when-not (<= 1 (count constraint) 2)
                   (miu/-fail! ::edn-constraint-takes-at-most-one-child
                               {:constraint constraint}))
                 (let [?schema (second constraint)
                       ?schema (if (false? ?schema)
                                 (miu/-fail! ::edn-child-is-true-or-schema {:constraint constraint})
                                 (when-not (true? ?schema)
                                   ?schema))
                       eof (Object.)
                       opts {:eof eof}
                       schema (when (some? ?schema)
                                (schema ?schema))
                       ;;TODO if regex allow multiple (or no) forms
                       _ (when (some-> schema -regex-op?)
                           (miu/-fail! ::edn-string-regex-schema-not-yet-implemented))
                       p (miu/-every-pred
                           (cond-> [#(not (identical? eof %))]
                             schema (conj (validator ?schema))))]
                   (fn [v]
                     (try (p (edn/read-string opts v))
                          (catch Exception _
                            false)))))})
