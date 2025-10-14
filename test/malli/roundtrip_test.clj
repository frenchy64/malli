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
  (testing ":or with :fn record? and :orn does NOT allow roundtripping"
    ;; :orn produces Tag records, which match record? predicate
    ;; So [:fn record?] overlaps with [:orn ...] output
    (let [result (rt/explain-roundtrip
                   [:or [:fn record?] [:orn [:i [:int]]]])]
      (is (vector? result))
      (is (seq result)))))

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
                [:orn [:a [:any]] [:b [:nil]]]))))
  (testing ":orn with overlapping types is still roundtrippable"
    ;; Even though int and number overlap, :orn makes it roundtrippable
    (is (nil? (rt/explain-roundtrip
                [:orn [:i [:int]] [:n number?]])))))

(deftest roundtrippable-map
  (testing ":map with roundtrippable children is roundtrippable"
    (is (nil? (rt/explain-roundtrip
                [:map [:x [:int]] [:y [:string]]]))))
  (testing ":map with non-roundtrippable children is not roundtrippable"
    (let [result (rt/explain-roundtrip
                   [:map [:x [:or [:int] number?]]])]
      (is (vector? result))
      (is (seq result)))))

(deftest roundtrippable-vector-set-sequential
  (testing ":vector with roundtrippable child is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:vector [:int]]))))
  (testing ":vector with non-roundtrippable child is not roundtrippable"
    (let [result (rt/explain-roundtrip [:vector [:or [:int] number?]])]
      (is (vector? result))
      (is (seq result))))
  (testing ":set with roundtrippable child is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:set [:int]]))))
  (testing ":sequential is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:sequential [:int]])))))

(deftest roundtrippable-tuple-cat
  (testing ":tuple with roundtrippable children is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:tuple [:int] [:string] [:boolean]]))))
  (testing ":tuple with non-roundtrippable child is not roundtrippable"
    (let [result (rt/explain-roundtrip [:tuple [:int] [:or [:int] number?]])]
      (is (vector? result))
      (is (seq result))))
  (testing ":cat with roundtrippable children is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:cat [:int] [:string]])))))

(deftest roundtrippable-and
  (testing ":and with roundtrippable children is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:and [:int] pos-int?]))))
  (testing ":and with non-roundtrippable child is not roundtrippable"
    (let [result (rt/explain-roundtrip [:and [:or [:int] number?] pos-int?])]
      (is (vector? result))
      (is (seq result)))))

(deftest roundtrippable-maybe
  (testing ":maybe with roundtrippable child is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:maybe [:int]]))))
  (testing ":maybe with non-roundtrippable child is not roundtrippable"
    (let [result (rt/explain-roundtrip [:maybe [:or [:int] number?]])]
      (is (vector? result))
      (is (seq result)))))

(deftest roundtrippable-complex-nested
  (testing "deeply nested roundtrippable schema"
    (is (nil? (rt/explain-roundtrip
                [:map
                 [:user [:map
                         [:name [:string]]
                         [:age [:int]]]]
                 [:items [:vector [:orn
                                   [:product [:map [:id [:int]] [:name [:string]]]]
                                   [:service [:map [:id [:int]] [:description [:string]]]]]]]]))
    ;; This is a complex schema with maps, vectors, and :orn - all roundtrippable
    ))
  (testing "deeply nested non-roundtrippable schema"
    (let [result (rt/explain-roundtrip
                   [:map
                    [:data [:vector [:or [:int] number?]]]])]
      (is (vector? result))
      (is (seq result))
      ;; The problem should be in the nested :or
      (is (some #(re-find #"overlap" (:problem %)) result)))))

(deftest roundtrippable-original-example
  (testing "original example from problem statement"
    ;; (let [schema [:or [:fn record?] [:orn [:i :int]]]]
    ;;   (->> 123
    ;;        (m/parse schema)
    ;;        (m/unparse schema)))
    ;; This should work because :fn record? doesn't overlap with :orn output (Tag)
    ;; Actually, the Tag IS a record, so they DO overlap!
    (let [result (rt/explain-roundtrip
                   [:or [:fn record?] [:orn [:i [:int]]]])]
      ;; For soundness, :fn with record? could accept Tag records
      ;; So we conservatively flag this as non-roundtrippable
      (is (vector? result))
      (is (seq result)))))

(deftest roundtrippable-fn-variations
  (testing ":fn schemas with different predicates"
    ;; :fn with int? overlaps with :int
    (let [result (rt/explain-roundtrip [:or [:fn int?] [:int]])]
      (is (vector? result))
      (is (seq result)))
    ;; :fn with string? doesn't overlap with :int
    (is (nil? (rt/explain-roundtrip [:or [:fn string?] [:int]]))))
  (testing "multiple :fn schemas"
    ;; Two :fn schemas might overlap
    (let [result (rt/explain-roundtrip [:or [:fn int?] [:fn number?]])]
      (is (vector? result))
      (is (seq result)))))

(deftest roundtrippable-enum
  (testing ":enum is always roundtrippable"
    (is (nil? (rt/explain-roundtrip [:enum 1 2 3])))
    (is (nil? (rt/explain-roundtrip [:enum "a" "b" "c"])))))

(deftest roundtrippable-predicate-variations
  (testing "various predicate combinations"
    ;; string? doesn't overlap with int?
    (is (nil? (rt/explain-roundtrip [:or string? int?])))
    ;; number? overlaps with int?
    (let [result (rt/explain-roundtrip [:or number? int?])]
      (is (vector? result))
      (is (seq result)))
    ;; Same predicate definitely overlaps
    (let [result (rt/explain-roundtrip [:or int? int?])]
      (is (vector? result))
      (is (seq result)))))

(deftest roundtrippable-or-orn-interaction
  (testing ":or containing :orn doesn't flag overlap with :orn when no actual overlap"
    ;; :orn with non-record predicates don't overlap
    (is (nil? (rt/explain-roundtrip
                [:or [:orn [:i [:int]] [:s [:string]]] [:boolean]]))))
  (testing "but non-:orn branches can overlap with other branches"
    (let [result (rt/explain-roundtrip
                   [:or [:orn [:i [:int]] [:s [:string]]] [:int]])]
      ;; The :int branch overlaps with :orn's possible outputs
      ;; Actually, :orn outputs Tags, not plain ints, so no overlap
      ;; So this should be roundtrippable
      (is (nil? result)))))

(deftest roundtrippable-repeat
  (testing ":repeat with roundtrippable child is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:repeat [:int]]))))
  (testing ":repeat with non-roundtrippable child is not roundtrippable"
    (let [result (rt/explain-roundtrip [:repeat [:or [:int] number?]])]
      (is (vector? result))
      (is (seq result)))))

(deftest roundtrippable-multi-arity-or
  (testing ":or with three branches, two overlapping"
    (let [result (rt/explain-roundtrip [:or [:int] [:string] number?])]
      (is (vector? result))
      (is (seq result))
      ;; Should report overlap between [:int] and number?
      (is (some #(and (re-find #"overlap" (:problem %))
                      (or (= [0 2] (:path %))
                          (= [2 0] (:path %)))) result))))
  (testing ":or with three branches, all non-overlapping"
    (is (nil? (rt/explain-roundtrip [:or [:int] [:string] [:boolean]])))))

(deftest roundtrippable-edge-cases
  (testing ":enum with values is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:enum 1]))))
  (testing ":any is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:any]))))
  (testing ":nil is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:nil]))))
  (testing "nested :and with overlapping :or"
    (let [result (rt/explain-roundtrip
                   [:and [:or [:int] number?] pos-int?])]
      (is (vector? result))
      (is (seq result)))))

(deftest roundtrippable-fn-edge-cases
  (testing ":fn with any? predicate overlaps with everything"
    (let [result (rt/explain-roundtrip [:or [:fn any?] [:int]])]
      ;; any? accepts everything, so it overlaps with :int
      (is (vector? result))
      (is (seq result))))
  (testing ":fn with disjoint predicates don't overlap"
    (is (nil? (rt/explain-roundtrip [:or [:fn string?] [:fn int?]]))))
  (testing ":fn with related predicates overlap"
    (let [result (rt/explain-roundtrip [:or [:fn int?] [:fn number?]])]
      (is (vector? result))
      (is (seq result)))))

(deftest roundtrippable-print-explanation
  (testing "print-roundtrip-explanation doesn't crash"
    ;; Just verify it runs without error
    (is (string? (with-out-str
                   (rt/print-roundtrip-explanation [:or [:int] number?]))))
    (is (string? (with-out-str
                   (rt/print-roundtrip-explanation [:int]))))))
