(ns malli.constraint.int-test
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

(deftest int-constraint-test
  (testing ":min/:max"
    (is (validate [:int {:min 1 :max 5}] 2))
    (is (validate [:int {:min 4 :max 4}] 4))
    (is (not (validate [:int {:min 1 :max 5}] "")))
    (is (= ["should be at least 1"]
           (me/humanize (explain [:int {:min 1}] 0))
           (me/humanize (explain [:int {:min 1 :max 10}] 0))
           (me/humanize (explain [:int {:and [[:min 1]]}] 0))))
    (is (= ["should be at least 2"]
           (me/humanize (explain [:int {:min 2}] 0))
           (me/humanize (explain [:int {:min 2 :max 10}] 0))
           (me/humanize (explain [:int {:and [[:min 2]]}] 0))))
    (is (= ["should be at most 1"]
           (me/humanize (explain [:int {:max 1}] 2))
           (me/humanize (explain [:int {:max 1}] 2))
           (me/humanize (explain [:int {:min 0 :max 1}] 2))
           (me/humanize (explain [:int {:and [[:max 1]]}] 2))))
    (is (= ["should be at most 2"]
           (me/humanize (explain [:int {:max 2}] 3))
           (me/humanize (explain [:int {:min 1 :max 2}] 3))
           (me/humanize (explain [:int {:and [[:max 2]]}] 3))))
    (is (= ["should be 1"]
           (me/humanize (explain [:int {:min 1 :max 1}] 3))
           (me/humanize (explain [:int {:and [[:min 1] [:max 1]]}] 3))))))
