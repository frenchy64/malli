;; AI GENERATED CONTENT
;; Prompt:
;; Start writing unit tests for interesting cases and their expected results.
;; Start with the smallest schema. Then build up larger ones. Concentrate
;; on :or, like [:or int? number?], [:or number? int?] and subtle
;; variations of overlapping schema children.

(ns malli.roundtrip-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.roundtrip :as rt]))

(deftest roundtrippable-simple-types
  (testing "Simple base types are always roundtrippable"
    (is (nil? (rt/explain-roundtrip [:int])))
    (is (nil? (rt/explain-roundtrip [:string])))
    (is (nil? (rt/explain-roundtrip [:boolean])))
    (is (nil? (rt/explain-roundtrip [:keyword])))
    (is (nil? (rt/explain-roundtrip [:nil])))
    (is (nil? (rt/explain-roundtrip [:any])))
    (is (nil? (rt/explain-roundtrip number?)))
    (is (nil? (rt/explain-roundtrip int?)))
    (is (nil? (rt/explain-roundtrip [:enum "a" "b" "c"])))))

(deftest roundtrippable-or-nonoverlapping
  (testing ":or with non-overlapping branches is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:or [:int] [:string]])))
    (is (nil? (rt/explain-roundtrip [:or [:keyword] [:string]])))))

(deftest roundtrippable-or-overlapping
  (testing ":or with overlapping branches is not roundtrippable"
    (let [result (rt/explain-roundtrip [:or [:int] number?])]
      (is (vector? result))
      (is (seq result))
      (is (some #(re-find #"overlap" (:problem %)) result)))
    (let [result (rt/explain-roundtrip [:or number? [:int]])]
      (is (vector? result))
      (is (seq result))
      (is (some #(re-find #"overlap" (:problem %)) result)))))

(deftest roundtrippable-or-fn-orn
  (testing ":or with :fn and :orn allows roundtripping"
    (is (nil? (rt/explain-roundtrip
                [:or [:fn record?] [:orn [:i :int]]])))))

(deftest roundtrippable-or-fn-int
  (testing ":or with :fn and :int does not roundtrip if overlapping"
    (let [result (rt/explain-roundtrip
                   [:or [:fn int?] [:int]])]
      ;; both are int? so they overlap
      (is (vector? result))
      (is (seq result))
      (is (some #(re-find #"overlap" (:problem %)) result)))))

(deftest roundtrippable-nested-or
  (testing "nested :or with overlapping children"
    (let [result (rt/explain-roundtrip
                   [:or [:or [:int] number?] [:string]])]
      (is (vector? result))
      (is (seq result)))))

(deftest roundtrippable-orn
  (testing ":orn is always roundtrippable"
    (is (nil? (rt/explain-roundtrip
                [:orn [:i [:int]] [:s [:string]]])))
    (is (nil? (rt/explain-roundtrip
                [:orn [:a [:any]] [:b [:nil]]])))))
