(ns malli.constraint.string-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [malli.constraint :as mc]
            [malli.core :as m]
            [malli.error :as me]))

(deftest string-constraint-test
  (testing "default constraint"
    (is (= ::m/true-constraint
           (-> :string
               m/schema
               mc/-get-constraint
               m/type))))
  (testing "constraint validators don't have preconditions"
    (is (false? (-> [:string {:max 1}]
                    m/schema
                    (m/validate 1))))
    #?(:clj (is (thrown-with-msg? Exception
                                  #"Don't know how to create ISeq from: java\.lang\.Long"
                                  (-> [:string {:max 1}]
                                      m/schema
                                      mc/-get-constraint
                                      (m/validate 1))))))
  (testing ":min/:max"
    (is (m/validate [:string {:min 1 :max 5}] "ab"))
    (is (m/validate [:string {:min 4 :max 4}] "🌉🜉"))
    (is (not (m/validate [:string {:min 1 :max 5}] "")))
    (is (= ["should be at least 1 character"]
           (me/humanize (m/explain [:string {:min 1}] ""))
           (me/humanize (m/explain [:string {:min 1 :max 10}] ""))
           (me/humanize (m/explain [:string {:and [[:min 1]]}] ""))))
    (is (= ["should be at least 2 characters"]
           (me/humanize (m/explain [:string {:min 2}] ""))
           (me/humanize (m/explain [:string {:min 2 :max 10}] ""))
           (me/humanize (m/explain [:string {:and [[:min 2]]}] ""))))
    (is (= ["should be at most 1 character"]
           (me/humanize (m/explain [:string {:max 1}] "🌉"))
           (me/humanize (m/explain [:string {:max 1}] "12"))
           (me/humanize (m/explain [:string {:min 0 :max 1}] "12"))
           (me/humanize (m/explain [:string {:and [[:max 1]]}] "12"))))
    (is (= ["should be at most 2 characters"]
           (me/humanize (m/explain [:string {:max 2}] "123"))
           (me/humanize (m/explain [:string {:min 1 :max 2}] "123"))
           (me/humanize (m/explain [:string {:and [[:max 2]]}] "123"))))
    (is (= ["should be 1 character"]
           (me/humanize (m/explain [:string {:min 1 :max 1}] "123"))
           (me/humanize (m/explain [:string {:and [[:min 1] [:max 1]]}] "123"))))))
