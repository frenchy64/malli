(ns malli.constraint.collection-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [malli.core :as m]
            [malli.error :as me]))

(deftest ^:constraints min-max-collection-constraint-test
  (doseq [[type coerce] [[:vector #'vec]
                         [:sequential #'sequence]
                         ;;TODO bounded count
                         ;; [:every #'eduction]
                         [:seqable #'eduction]
                         [:set #'set]]]
    (testing (str type " " coerce " :min/:max")
      (is (m/validate [type {:min 1 :max 5} :int] (coerce [0 1])))
      (is (m/validate [type {:min 1 :max 5} :int] (coerce [0 1])))
      (is (m/validate [type {:min 4 :max 4} :int] (coerce [0 1 2 4])))
      (is (m/validate [type {:min 4 :max 4} :int] (coerce [0 1 2 4])))
      (is (not (m/validate [type {:min 1 :max 5} :int] (coerce []))))
      (is (not (m/validate [type {:min 1 :max 5} :int] (coerce []))))
      (is (= ["should have at least 1 element"]
             (me/humanize (m/explain [type {:min 1} :int] (coerce [])))
             (me/humanize (m/explain [type {:min 1 :max 10} :int] (coerce [])))
             (me/humanize (m/explain [type {:min 1 :max 10} :int] (coerce [])))
             (me/humanize (m/explain [type {:and [[:min 1]]} :int] (coerce [])))))
      (is (= ["should have at least 2 elements"]
             (me/humanize (m/explain [type {:min 2} :int] (coerce [])))
             (me/humanize (m/explain [type {:min 2 :max 10} :int] (coerce [])))
             (me/humanize (m/explain [type {:and [[:min 2]]} :int] (coerce [])))))
      (is (= ["should have at most 1 element"]
             (me/humanize (m/explain [type {:max 1} :int] (coerce [0 1])))
             (me/humanize (m/explain [type {:min 0 :max 1} :int] (coerce [0 1])))
             (me/humanize (m/explain [type {:and [[:max 1]]} :int] (coerce [0 1])))))
      (is (= ["should have at most 2 elements"]
             (me/humanize (m/explain [type {:max 2} :int] (coerce [0 1 2])))
             (me/humanize (m/explain [type {:min 1 :max 2} :int] (coerce [0 1 2])))
             (me/humanize (m/explain [type {:min 1 :max 2} :int] (coerce [0 1 2])))
             (me/humanize (m/explain [type {:and [[:max 2]]} :int] (coerce [0 1 2])))))
      (is (= ["should have 1 element"]
             (me/humanize (m/explain [type {:min 1 :max 1} :int] (coerce [0 1 2])))
             (me/humanize (m/explain [type {:and [[:min 1] [:max 1]]} :int] (coerce [0 1 2]))))))))
