(ns malli.constraint.string-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [malli.core :as m]
            [malli.error :as me]))

(defn constraint-options []
  {::m/constraint-options (mc/base-constraint-extensions)
   :registry (merge (mc/base-constraints)
                    (m/default-schemas))})

(deftest string-constraint-test
  (testing ":min/:max"
    (is (m/validate [:string {:min 1 :max 5}] "ab"))
    (is (m/validate [:string {:min 4 :max 4}] "🌉🜉"))
    (is (not (m/validate [:string {:min 1 :max 5}] "")))
    (is (= ["should be at least 1 character, given 0"]
           (me/humanize (m/explain [:string {:min 1}] ""))
           (me/humanize (m/explain [:string {:min 1 :max 10}] ""))
           (me/humanize (m/explain [:string {:and [[:min 1]]}] ""))))
    (is (= ["should be at least 2 characters, given 0"]
           (me/humanize (m/explain [:string {:min 2}] ""))
           (me/humanize (m/explain [:string {:min 2 :max 10}] ""))
           (me/humanize (m/explain [:string {:and [[:min 2]]}] ""))))
    (is (= ["should be at most 1 character, given 2"]
           (me/humanize (m/explain [:string {:max 1}] "🌉"))
           (me/humanize (m/explain [:string {:max 1}] "12"))
           (me/humanize (m/explain [:string {:min 0 :max 1}] "12"))
           (me/humanize (m/explain [:string {:and [[:max 1]]}] "12"))))
    (is (= ["should be at most 2 characters, given 3"]
           (me/humanize (m/explain [:string {:max 2}] "123"))
           (me/humanize (m/explain [:string {:min 1 :max 2}] "123"))
           (me/humanize (m/explain [:string {:and [[:max 2]]}] "123"))))
    ;; FIXME
    #_
    (is (= [::TODO]
           (me/humanize (m/explain [:string {:min 1 :max 1}] "123"))
           (me/humanize (m/explain [:string {:and [[:min 1] [:max 1]]}] "123"))))))
