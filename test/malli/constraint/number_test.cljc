(ns malli.constraint.number-test
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
           (me/humanize (explain [:int {:and [[:min 1]]}] 0))
           (me/humanize (explain [:int {:and [[:min 1] [:min -5]]}] 0))))
    (is (= ["should be at least 2"]
           (me/humanize (explain [:int {:min 2}] 0))
           (me/humanize (explain [:int {:min 2 :max 10}] 0))
           (me/humanize (explain [:int {:and [[:min 2]]}] 0))
           (me/humanize (explain [:int {:and [[:min -2] [:min 2]]}] 0))))
    (is (= ["should be at most 1"]
           (me/humanize (explain [:int {:max 1}] 2))
           (me/humanize (explain [:int {:max 1}] 2))
           (me/humanize (explain [:int {:min 0 :max 1}] 2))
           (me/humanize (explain [:int {:and [[:max 1] [:max 23]]}] 2))))
    (is (= ["should be at most 2"]
           (me/humanize (explain [:int {:max 2}] 3))
           (me/humanize (explain [:int {:min 1 :max 2}] 3))
           (me/humanize (explain [:int {:and [[:max 23] [:max 2]]}] 3))))
    (is (= ["should be 1"]
           (me/humanize (explain [:int {:min 1 :max 1}] 3))
           (me/humanize (explain [:int {:and [[:min 1] [:max 1]]}] 3))
           (me/humanize (explain [:int {:and [[:min 1] [:max 1]
                                              [:min 0] [:max 2]]}] 3))))))

(deftest double+float-constraint-test
  (doseq [type [:double :float]]
    (testing (str type " :min/:max")
      (is (validate [type {:min 1.0 :max 5.0}] 2.0))
      (is (validate [type {:min 4.0 :max 4.0}] 4.0))
      (is (not (validate [type {:min 1.0 :max 5.0}] "")))
      (is (= ["should be at least 1.5"]
             (me/humanize (explain [type {:min 1.5}] 0.5))
             (me/humanize (explain [type {:min 1.5 :max 10.5}] 0.5))
             (me/humanize (explain [type {:and [[:min 1.5]]}] 0.5))
             (me/humanize (explain [type {:and [[:min 1.5] [:min -1.5]]}] 0.5))))
      (is (= ["should be at least 2.5"]
             (me/humanize (explain [type {:min 2.5}] 0.5))
             (me/humanize (explain [type {:min 2.5 :max 10.5}] 0.5))
             (me/humanize (explain [type {:and [[:min 2.5]]}] 0.5))
             (me/humanize (explain [type {:and [[:min 0.5] [:min 2.5]]}] 0.5))))
      (is (= ["should be at most 1.5"]
             (me/humanize (explain [type {:max 1.5}] 2.5))
             (me/humanize (explain [type {:max 1.5}] 2.5))
             (me/humanize (explain [type {:min 0.5 :max 1.5}] 2.5))
             (me/humanize (explain [type {:and [[:max 1.5] [:max 23.5]]}] 2.5))))
      (is (= ["should be at most 2.5"]
             (me/humanize (explain [type {:max 2.5}] 3.5))
             (me/humanize (explain [type {:min 1.5 :max 2.5}] 3.5))
             (me/humanize (explain [type {:and [[:max 23.5] [:max 2.5]]}] 3.5))))
      (is (= ["should be 1.5"]
             (me/humanize (explain [type {:min 1.5 :max 1.5}] 3.5))
             (me/humanize (explain [type {:and [[:min 1.5] [:max 1.5]
                                                [:min 0.5] [:max 2.5]]}] 3.5)))))))
