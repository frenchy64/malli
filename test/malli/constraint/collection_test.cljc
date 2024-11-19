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
  (doseq [[type coerce] [[:vector #'vec]
                         [:sequential #'sequence]
                         ;;TODO bounded count
                         ;; [:every #'eduction]
                         [:seqable #'eduction]
                         [:set #'set]]]
    (testing (str type " " coerce " :min/:max")
      (is (m/validate [type {:min 1 :max 5} :int] (coerce [0 1])))
      (is (validate [type {:min 1 :max 5} :int] (coerce [0 1])))
      (is (m/validate [type {:min 4 :max 4} :int] (coerce [0 1 2 4])))
      (is (validate [type {:min 4 :max 4} :int] (coerce [0 1 2 4])))
      (is (not (m/validate [type {:min 1 :max 5} :int] (coerce []))))
      (is (not (validate [type {:min 1 :max 5} :int] (coerce []))))
      (is (seq (explain [type :int] [1 "2"])))
      (is (not (validate [type :int] [1 "2"])))
      (is (= ["should have at least 1 element"]
             (me/humanize (m/explain [type {:min 1} :int] (coerce [])))
             (me/humanize (explain [type {:min 1} :int] (coerce [])))
             (me/humanize (m/explain [type {:min 1 :max 10} :int] (coerce [])))
             (me/humanize (explain [type {:min 1 :max 10} :int] (coerce [])))
             (me/humanize (explain [type {:and [[:min 1]]} :int] (coerce [])))))
      (is (= ["should have at least 2 elements"]
             (me/humanize (m/explain [type {:min 2} :int] (coerce [])))
             (me/humanize (explain [type {:min 2} :int] (coerce [])))
             (me/humanize (m/explain [type {:min 2 :max 10} :int] (coerce [])))
             (me/humanize (explain [type {:min 2 :max 10} :int] (coerce [])))
             (me/humanize (explain [type {:and [[:min 2]]} :int] (coerce [])))))
      (is (= ["should have at most 1 element"]
             (me/humanize (m/explain [type {:max 1} :int] (coerce [0 1])))
             (me/humanize (explain [type {:max 1} :int] (coerce [0 1])))
             (me/humanize (explain [type {:max 1} :int] (coerce [0 1])))
             (me/humanize (m/explain [type {:max 1} :int] (coerce [0 1])))
             (me/humanize (explain [type {:min 0 :max 1} :int] (coerce [0 1])))
             (me/humanize (m/explain [type {:min 0 :max 1} :int] (coerce [0 1])))
             (me/humanize (explain [type {:and [[:max 1]]} :int] (coerce [0 1])))))
      (is (= ["should have at most 2 elements"]
             (me/humanize (m/explain [type {:max 2} :int] (coerce [0 1 2])))
             (me/humanize (explain [type {:max 2} :int] (coerce [0 1 2])))
             (me/humanize (explain [type {:min 1 :max 2} :int] (coerce [0 1 2])))
             (me/humanize (m/explain [type {:min 1 :max 2} :int] (coerce [0 1 2])))
             (me/humanize (explain [type {:and [[:max 2]]} :int] (coerce [0 1 2])))))
      (is (= ["should have 1 element"]
             (me/humanize (m/explain [type {:min 1 :max 1} :int] (coerce [0 1 2])))
             (me/humanize (explain [type {:min 1 :max 1} :int] (coerce [0 1 2])))
             (me/humanize (explain [type {:and [[:min 1] [:max 1]]} :int] (coerce [0 1 2]))))))))
