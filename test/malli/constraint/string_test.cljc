(ns malli.constraint.string-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [malli.constraint :as mc]
            [malli.core :as m]
            [malli.error :as me]))

(defn constraint-options []
  {::m/constraint-options (mc/base-constraint-extensions)
   :registry (merge (mc/base-constraints)
                    (m/default-schemas))})

(defn validate [?schema value] (m/validate ?schema value (constraint-options)))
(defn explain [?schema value] (m/explain ?schema value (constraint-options)))

(deftest string-constraint-test
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
