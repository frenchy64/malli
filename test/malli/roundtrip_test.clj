;; AI GENERATED CONTENT
;; Prompt:
;; Start writing unit tests for interesting cases and their expected results.
;; Start with the smallest schema. Then build up larger ones. Concentrate
;; on :or, like [:or int? number?], [:or number? int?] and subtle
;; variations of overlapping schema children.

(ns malli.roundtrip-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
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
  (testing ":or with overlapping simple-parser branches IS roundtrippable"
    ;; When both branches have simple (non-transforming) parsers,
    ;; there's no ambiguity even if types overlap
    (is (nil? (rt/explain-roundtrip [:or [:int] number?])))
    (is (nil? (rt/explain-roundtrip [:or number? [:int]]))))
  (testing ":or with overlapping non-simple branches is NOT roundtrippable"
    ;; When branches have transforming parsers and overlap, it's not roundtrippable
    (let [schema [:or [:orn [:i :int]] [:orn [:j :int]]]
          result (rt/explain-roundtrip schema)]
      ;; Verify the exact error structure
      (is (= [{:schema schema
               :branch-a [:orn [:i :int]]
               :branch-b [:orn [:j :int]]
               :path [0 1]
               :problem ":or branches at positions 0 and 1 overlap in their parsed domain, so this schema is not roundtrippable. If you need roundtripping, use :orn instead of :or."}]
             result))
      ;; Note: This schema actually roundtrips if the first branch is chosen,
      ;; but we flag it as potentially non-roundtrippable for soundness
      )))

(deftest roundtrippable-or-fn-orn
  (testing ":or with :fn record? and :orn does NOT allow roundtripping (order matters)"
    ;; :orn produces Tag records, which match record? predicate
    ;; When :fn record? comes first, it captures the Tag during unparse
    (let [schema [:or [:fn record?] [:orn [:i :int]]]
          v1 123
          result (rt/explain-roundtrip schema)]
      ;; Verify the error (note: fn shows as object in form)
      (is (= 1 (count result)))
      (is (= [0 1] (:path (first result))))
      (is (= [:orn [:i :int]] (:branch-b (first result))))
      ;; Demonstrate the actual failure
      (let [parsed (m/parse schema v1)
            unparsed (m/unparse schema parsed)]
        (is (= parsed unparsed)) ;; Tag stays as Tag
        (is (not= v1 unparsed))))) ;; Doesn't roundtrip to original value
  
  (testing ":or with :orn and :fn record? (swapped order) - still flagged"
    ;; Even when order is swapped, we still flag it as non-roundtrippable
    ;; because overlap exists (conservative for soundness)
    (let [schema [:or [:orn [:i :int]] [:fn record?]]
          v1 123
          result (rt/explain-roundtrip schema)]
      ;; Still flagged as non-roundtrippable
      (is (= 1 (count result)))
      (is (= [0 1] (:path (first result))))
      (is (= [:orn [:i :int]] (:branch-a (first result))))
      ;; In this order, it actually does roundtrip (first branch chosen)
      (let [parsed (m/parse schema v1)
            unparsed (m/unparse schema parsed)]
        (is (= v1 unparsed))))))

(deftest roundtrippable-or-fn-int
  (testing ":or with :fn and :int with simple parsers is roundtrippable"
    ;; Both [:fn int?] and [:int] have simple parsers
    (is (nil? (rt/explain-roundtrip [:or [:fn int?] [:int]])))))

(deftest roundtrippable-nested-or
  (testing "nested :or with simple parsers is roundtrippable"
    ;; Even though inner :or has overlapping types, all are simple parsers
    (is (nil? (rt/explain-roundtrip [:or [:or [:int] number?] [:string]]))))
  (testing "nested :or with non-simple parsers is not roundtrippable"
    (let [result (rt/explain-roundtrip
                   [:or [:or [:orn [:i :int]] [:orn [:j :int]]] [:string]])]
      (is (vector? result))
      (is (seq result)))))

(deftest roundtrippable-orn
  (testing ":orn is always roundtrippable"
    (is (nil? (rt/explain-roundtrip
                [:orn [:i :int] [:s [:string]]])))
    (is (nil? (rt/explain-roundtrip
                [:orn [:a [:any]] [:b [:nil]]]))))
  (testing ":orn with overlapping types is still roundtrippable"
    ;; Even though int and number overlap, :orn makes it roundtrippable
    (is (nil? (rt/explain-roundtrip
                [:orn [:i :int] [:n number?]])))))

(deftest roundtrippable-map
  (testing ":map with roundtrippable children is roundtrippable"
    (is (nil? (rt/explain-roundtrip
                [:map [:x [:int]] [:y [:string]]]))))
  (testing ":map with non-roundtrippable children is not roundtrippable"
    (let [schema [:map [:x [:or [:orn [:i :int]] [:orn [:j :int]]]]]
          result (rt/explain-roundtrip schema)]
      (is (= [{:schema [:or [:orn [:i :int]] [:orn [:j :int]]]
               :branch-a [:orn [:i :int]]
               :branch-b [:orn [:j :int]]
               :path [:x 0 1]
               :problem ":or branches at positions 0 and 1 overlap in their parsed domain, so this schema is not roundtrippable. If you need roundtripping, use :orn instead of :or."}]
             result)))))

(deftest roundtrippable-vector-set-sequential
  (testing ":vector with roundtrippable child is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:vector [:int]]))))
  (testing ":vector with non-roundtrippable child is not roundtrippable"
    (let [schema [:vector [:or [:orn [:i :int]] [:orn [:j :int]]]]
          result (rt/explain-roundtrip schema)]
      (is (= [{:schema [:or [:orn [:i :int]] [:orn [:j :int]]]
               :branch-a [:orn [:i :int]]
               :branch-b [:orn [:j :int]]
               :path [:vector 0 1]
               :problem ":or branches at positions 0 and 1 overlap in their parsed domain, so this schema is not roundtrippable. If you need roundtripping, use :orn instead of :or."}]
             result))))
  (testing ":set with roundtrippable child is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:set [:int]]))))
  (testing ":sequential is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:sequential [:int]])))))

(deftest roundtrippable-tuple-cat
  (testing ":tuple with roundtrippable children is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:tuple [:int] [:string] [:boolean]]))))
  (testing ":tuple with non-roundtrippable child is not roundtrippable"
    (let [schema [:tuple [:int] [:or [:orn [:i :int]] [:orn [:j :int]]]]
          result (rt/explain-roundtrip schema)]
      (is (= [{:schema [:or [:orn [:i :int]] [:orn [:j :int]]]
               :branch-a [:orn [:i :int]]
               :branch-b [:orn [:j :int]]
               :path [1 0 1]
               :problem ":or branches at positions 0 and 1 overlap in their parsed domain, so this schema is not roundtrippable. If you need roundtripping, use :orn instead of :or."}]
             result))))
  (testing ":cat with roundtrippable children is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:cat [:int] [:string]])))))

(deftest roundtrippable-and
  (testing ":and with roundtrippable children is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:and [:int] pos-int?]))))
  (testing ":and with non-roundtrippable child is not roundtrippable"
    (let [schema [:and [:or [:orn [:i :int]] [:orn [:j :int]]] pos-int?]
          result (rt/explain-roundtrip schema)]
      (is (= [{:schema [:or [:orn [:i :int]] [:orn [:j :int]]]
               :branch-a [:orn [:i :int]]
               :branch-b [:orn [:j :int]]
               :path [0 0 1]
               :problem ":or branches at positions 0 and 1 overlap in their parsed domain, so this schema is not roundtrippable. If you need roundtripping, use :orn instead of :or."}]
             result)))))

(deftest roundtrippable-maybe
  (testing ":maybe with roundtrippable child is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:maybe [:int]]))))
  (testing ":maybe with non-roundtrippable child is not roundtrippable"
    (let [schema [:maybe [:or [:orn [:i :int]] [:orn [:j :int]]]]
          result (rt/explain-roundtrip schema)]
      (is (= [{:schema [:or [:orn [:i :int]] [:orn [:j :int]]]
               :branch-a [:orn [:i :int]]
               :branch-b [:orn [:j :int]]
               :path [:maybe 0 1]
               :problem ":or branches at positions 0 and 1 overlap in their parsed domain, so this schema is not roundtrippable. If you need roundtripping, use :orn instead of :or."}]
             result)))))

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
    (let [schema [:map [:data [:vector [:or [:orn [:i :int]] [:orn [:j :int]]]]]]
          result (rt/explain-roundtrip schema)]
      (is (= [{:schema [:or [:orn [:i :int]] [:orn [:j :int]]]
               :branch-a [:orn [:i :int]]
               :branch-b [:orn [:j :int]]
               :path [:data :vector 0 1]
               :problem ":or branches at positions 0 and 1 overlap in their parsed domain, so this schema is not roundtrippable. If you need roundtripping, use :orn instead of :or."}]
             result)))))

(deftest roundtrippable-original-example
  (testing "original example from problem statement"
    ;; (let [schema [:or [:fn record?] [:orn [:i :int]]]]
    ;;   (->> 123
    ;;        (m/parse schema)
    ;;        (m/unparse schema)))
    ;; This should work because :fn record? doesn't overlap with :orn output (Tag)
    ;; Actually, the Tag IS a record, so they DO overlap!
    (let [result (rt/explain-roundtrip
                   [:or [:fn record?] [:orn [:i :int]]])]
      ;; For soundness, :fn with record? could accept Tag records
      ;; So we conservatively flag this as non-roundtrippable
      (is (vector? result))
      (is (seq result)))))

(deftest roundtrippable-fn-variations
  (testing ":fn schemas all have simple parsers"
    ;; :fn has simple parsers, so no overlap issues even if predicates match
    (is (nil? (rt/explain-roundtrip [:or [:fn int?] [:int]])))
    ;; :fn with string? doesn't overlap with :int
    (is (nil? (rt/explain-roundtrip [:or [:fn string?] [:int]]))))
  (testing "multiple :fn schemas have simple parsers"
    ;; Two :fn schemas both have simple parsers
    (is (nil? (rt/explain-roundtrip [:or [:fn int?] [:fn number?]])))))

(deftest roundtrippable-enum
  (testing ":enum is always roundtrippable"
    (is (nil? (rt/explain-roundtrip [:enum 1 2 3])))
    (is (nil? (rt/explain-roundtrip [:enum "a" "b" "c"])))))

(deftest roundtrippable-predicate-variations
  (testing "various predicate combinations"
    ;; string? doesn't overlap with int?
    (is (nil? (rt/explain-roundtrip [:or string? int?])))
    ;; number? overlaps with int? but both have simple parsers
    (is (nil? (rt/explain-roundtrip [:or number? int?])))
    ;; Same predicate definitely overlaps but still simple
    (is (nil? (rt/explain-roundtrip [:or int? int?])))))

(deftest roundtrippable-or-orn-interaction
  (testing ":or containing :orn doesn't flag overlap with :orn when no actual overlap"
    ;; :orn with non-record predicates don't overlap
    (is (nil? (rt/explain-roundtrip
                [:or [:orn [:i :int] [:s [:string]]] [:boolean]]))))
  (testing "but non-:orn branches can overlap with other branches"
    (let [result (rt/explain-roundtrip
                   [:or [:orn [:i :int] [:s [:string]]] [:int]])]
      ;; The :int branch overlaps with :orn's possible outputs
      ;; Actually, :orn outputs Tags, not plain ints, so no overlap
      ;; So this should be roundtrippable
      (is (nil? result)))))

(deftest roundtrippable-repeat
  (testing ":repeat with roundtrippable child is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:repeat [:int]]))))
  (testing ":repeat with non-roundtrippable child is not roundtrippable"
    (let [schema [:repeat [:or [:orn [:i :int]] [:orn [:j :int]]]]
          result (rt/explain-roundtrip schema)]
      (is (= [{:schema [:or [:orn [:i :int]] [:orn [:j :int]]]
               :branch-a [:orn [:i :int]]
               :branch-b [:orn [:j :int]]
               :path [:repeat 0 1]
               :problem ":or branches at positions 0 and 1 overlap in their parsed domain, so this schema is not roundtrippable. If you need roundtripping, use :orn instead of :or."}]
             result)))))

(deftest roundtrippable-multi-arity-or
  (testing ":or with three simple branches is roundtrippable"
    ;; All simple parsers, so no issue even with overlap
    (is (nil? (rt/explain-roundtrip [:or [:int] [:string] number?]))))
  (testing ":or with three branches, non-simple overlapping"
    (let [schema [:or [:orn [:i :int]] [:string] [:orn [:j :int]]]
          result (rt/explain-roundtrip schema)]
      (is (= [{:schema [:or [:orn [:i :int]] :string [:orn [:j :int]]]
               :branch-a [:orn [:i :int]]
               :branch-b [:orn [:j :int]]
               :path [0 2]
               :problem ":or branches at positions 0 and 2 overlap in their parsed domain, so this schema is not roundtrippable. If you need roundtripping, use :orn instead of :or."}]
             result))))
  (testing ":or with three branches, all non-overlapping"
    (is (nil? (rt/explain-roundtrip [:or [:int] [:string] [:boolean]])))))

(deftest roundtrippable-edge-cases
  (testing ":enum with values is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:enum 1]))))
  (testing ":any is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:any]))))
  (testing ":nil is roundtrippable"
    (is (nil? (rt/explain-roundtrip [:nil]))))
  (testing "nested :and with simple :or is roundtrippable"
    ;; Simple parsers throughout
    (is (nil? (rt/explain-roundtrip [:and [:or [:int] number?] pos-int?]))))
  (testing "nested :and with non-simple :or is not roundtrippable"
    (let [schema [:and [:or [:orn [:i :int]] [:orn [:j :int]]] pos-int?]
          result (rt/explain-roundtrip schema)]
      (is (= [{:schema [:or [:orn [:i :int]] [:orn [:j :int]]]
               :branch-a [:orn [:i :int]]
               :branch-b [:orn [:j :int]]
               :path [0 0 1]
               :problem ":or branches at positions 0 and 1 overlap in their parsed domain, so this schema is not roundtrippable. If you need roundtripping, use :orn instead of :or."}]
             result)))))

(deftest roundtrippable-fn-edge-cases
  (testing ":fn schemas all have simple parsers"
    ;; :fn has simple parsers, so no overlap issues
    (is (nil? (rt/explain-roundtrip [:or [:fn any?] [:int]]))))
  (testing ":fn with disjoint predicates don't overlap"
    (is (nil? (rt/explain-roundtrip [:or [:fn string?] [:fn int?]]))))
  (testing ":fn with related predicates have simple parsers"
    ;; Even though they overlap, both are simple parsers
    (is (nil? (rt/explain-roundtrip [:or [:fn int?] [:fn number?]])))))

(deftest roundtrippable-print-explanation
  (testing "print-roundtrip-explanation doesn't crash"
    ;; Just verify it runs without error
    (is (string? (with-out-str
                   (rt/print-roundtrip-explanation [:or [:int] number?]))))
    (is (string? (with-out-str
                   (rt/print-roundtrip-explanation [:int]))))))

(deftest roundtrippable-simple-parser-precision
  (testing "Simple parser analysis is precise"
    ;; All simple parsers - roundtrippable
    (is (nil? (rt/explain-roundtrip [:or [:int] [:int]])))
    (is (nil? (rt/explain-roundtrip [:or int? number? pos-int?])))
    (is (nil? (rt/explain-roundtrip [:or [:fn int?] [:fn number?] [:int]])))
    
    ;; Mixed simple and non-simple - roundtrippable if no parsed overlap
    ;; :int parses to int, :orn parses to Tag - different parsed outputs!
    (is (nil? (rt/explain-roundtrip [:or [:int] [:orn [:i :int]]])))
    
    ;; Multiple non-simple branches overlapping - not roundtrippable
    (let [schema [:or [:orn [:i :int]] [:orn [:j :int]] [:orn [:k :int]]]
          result (rt/explain-roundtrip schema)]
      ;; Should detect overlap between branches 0-1, 0-2, and 1-2
      (is (= 3 (count result)))
      (is (= [{:schema schema
               :branch-a [:orn [:i :int]]
               :branch-b [:orn [:j :int]]
               :path [0 1]
               :problem ":or branches at positions 0 and 1 overlap in their parsed domain, so this schema is not roundtrippable. If you need roundtripping, use :orn instead of :or."}
              {:schema schema
               :branch-a [:orn [:i :int]]
               :branch-b [:orn [:k :int]]
               :path [0 2]
               :problem ":or branches at positions 0 and 2 overlap in their parsed domain, so this schema is not roundtrippable. If you need roundtripping, use :orn instead of :or."}
              {:schema schema
               :branch-a [:orn [:j :int]]
               :branch-b [:orn [:k :int]]
               :path [1 2]
               :problem ":or branches at positions 1 and 2 overlap in their parsed domain, so this schema is not roundtrippable. If you need roundtripping, use :orn instead of :or."}]
             result)))
    
    ;; Non-simple branches that don't overlap - roundtrippable
    (is (nil? (rt/explain-roundtrip [:or [:orn [:i :int]] [:orn [:s :string]]])))))

(deftest roundtrippable-nested-simple-parser-analysis
  (testing "Nested schemas respect simple parser rules"
    ;; Deeply nested all-simple - roundtrippable
    (is (nil? (rt/explain-roundtrip
                [:map
                 [:level1 [:or int? number?]]
                 [:level2 [:vector [:or [:int] [:double]]]]])))
    
    ;; Deeply nested with non-simple at leaf - not roundtrippable
    (let [schema [:map [:data [:vector [:or [:orn [:i :int]] [:orn [:j :int]]]]]]
          result (rt/explain-roundtrip schema)]
      (is (= [{:schema [:or [:orn [:i :int]] [:orn [:j :int]]]
               :branch-a [:orn [:i :int]]
               :branch-b [:orn [:j :int]]
               :path [:data :vector 0 1]
               :problem ":or branches at positions 0 and 1 overlap in their parsed domain, so this schema is not roundtrippable. If you need roundtripping, use :orn instead of :or."}]
             result)))
    
    ;; Non-simple in container but no overlap - roundtrippable
    (is (nil? (rt/explain-roundtrip
                [:tuple [:orn [:i :int]] [:orn [:s :string]]])))))
