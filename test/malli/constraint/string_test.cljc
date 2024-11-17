(ns malli.constraint.string-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [malli.constraint :as mc]
            [malli.core :as m]
            [malli.error :as me]
            [malli.constraint.protocols :as mcp]))

(defn constraint-options []
  (-> {:registry (m/default-schemas)}
      mc/with-base-constraints))

(defn validate [?schema value] (m/validate ?schema value (constraint-options)))
(defn explain [?schema value] (m/explain ?schema value (constraint-options)))

(deftest string-constraint-test
  (testing "default constraint"
    (is (= :malli.constraint/true-constraint
           (-> :string
               (m/schema (constraint-options))
               mcp/-get-constraint
               m/type))))
  (testing "constraint validators don't have preconditions"
    (is (false? (-> [:string {:max 1}]
                    (m/schema (constraint-options))
                    (m/validate 1))))
    #?(:clj (is (thrown-with-msg? Exception
                                  #"Don't know how to create ISeq from: java\.lang\.Long"
                                  (-> [:string {:max 1}]
                                      (m/schema (constraint-options))
                                      mcp/-get-constraint
                                      (m/validate 1))))))
  (testing ":min/:max"
    (is (validate [:string {:min 1 :max 5}] "ab"))
    (is (validate [:string {:min 4 :max 4}] "🌉🜉"))
    (is (not (validate [:string {:min 1 :max 5}] "")))
    (is (= ["should have at least 1 character"]
           (me/humanize (explain [:string {:min 1}] ""))
           (me/humanize (explain [:string {:min 1 :max 10}] ""))
           (me/humanize (explain [:string {:and [[:min 1]]}] ""))))
    (is (= ["should have at least 2 characters"]
           (me/humanize (explain [:string {:min 2}] ""))
           (me/humanize (explain [:string {:min 2 :max 10}] ""))
           (me/humanize (explain [:string {:and [[:min 2]]}] ""))))
    (is (= ["should have at most 1 character"]
           (me/humanize (explain [:string {:max 1}] "🌉"))
           (me/humanize (explain [:string {:max 1}] "12"))
           (me/humanize (explain [:string {:min 0 :max 1}] "12"))
           (me/humanize (explain [:string {:and [[:max 1]]}] "12"))))
    (is (= ["should have at most 2 characters"]
           (me/humanize (explain [:string {:max 2}] "123"))
           (me/humanize (explain [:string {:min 1 :max 2}] "123"))
           (me/humanize (explain [:string {:and [[:max 2]]}] "123"))))
    (is (= ["should have 1 character"]
           (me/humanize (explain [:string {:min 1 :max 1}] "123"))
           (me/humanize (explain [:string {:and [[:min 1] [:max 1]]}] "123"))))))
