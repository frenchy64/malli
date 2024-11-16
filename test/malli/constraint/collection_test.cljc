(ns malli.constraint.collection-test
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

(deftest min-max-collection-constraint-test
  (testing ":min/:max"
    (is (validate [:vector {:min 1 :max 5} :int] [0 1]))
    (is (validate [:vector {:min 4 :max 4} :int] [0 1 2 4]))
    (is (not (validate [:vector {:min 1 :max 5} :int] [])))
    (is (= ["should have at least 1 element"]
           (me/humanize (explain [:vector {:min 1} :int] []))
           (me/humanize (explain [:vector {:min 1 :max 10} :int] []))
           (me/humanize (explain [:vector {:and [[:min 1]]} :int] []))))
    (is (= ["should have at least 2 elements"]
           (me/humanize (explain [:vector {:min 2} :int] []))
           (me/humanize (explain [:vector {:min 2 :max 10} :int] []))
           (me/humanize (explain [:vector {:and [[:min 2]]} :int] []))))
    (is (= ["should have at most 1 element"]
           (me/humanize (explain [:vector {:max 1} :int] [0 1]))
           (me/humanize (explain [:vector {:max 1} :int] [0 1]))
           (me/humanize (explain [:vector {:min 0 :max 1} :int] [0 1]))
           (me/humanize (explain [:vector {:and [[:max 1]]} :int] [0 1]))))
    (is (= ["should have at most 2 elements"]
           (me/humanize (explain [:vector {:max 2} :int] [0 1 2]))
           (me/humanize (explain [:vector {:min 1 :max 2} :int] [0 1 2]))
           (me/humanize (explain [:vector {:and [[:max 2]]} :int] [0 1 2]))))
    (is (= ["should have 1 element"]
           (me/humanize (explain [:vector {:min 1 :max 1} :int] [0 1 2]))
           (me/humanize (explain [:vector {:and [[:min 1] [:max 1]]} :int] [0 1 2]))))))
