(ns malli.constraint-test
  (:require [clojure.string :as str]
            [clojure.test :refer [are deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.walk :as walk]
            [malli.constraint :as mc]
            [malli.core :as m]
            [malli.edn :as edn]
            [malli.generator :as mg]
            [malli.error :as me]
            [malli.impl.util :as miu]
            [malli.registry :as mr]
            [malli.transform :as mt]
            [malli.util :as mu]
            #?(:clj [malli.test-macros :refer [when-env]]))
  #?(:clj  (:import (clojure.lang IFn PersistentArrayMap PersistentHashMap))
     :cljs (:require-macros [malli.test-macros :refer [when-env]])))

(def count-constraint-options
  {::m/constraint-context {:parse-constraint {:min (fn [{:keys [properties children]} opts]
                                                     [:count {:min (first children)}])
                                              :max (fn [{:keys [properties children]} opts]
                                                     [:count {:min 0 :max (first children)}])}}
   :registry {:count (m/-count-constraint)}})

(deftest constraint-test
  (testing "Constraints are returned as-is"
    (is (= :any
           (m/type (m/constraint (m/schema (m/-any-schema))))
           (m/type (m/constraint (m/schema (m/-any-schema)) nil)))))
  (testing "IntoSchema's are not allowed in raw form"
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.core/missing-parse-constraint-options"
          (m/constraint [::m/true-constraint]
                        {:registry {::m/true-constraint (m/-any-schema)}}))))
  (testing "m/form requires a constraint context"
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.core/no-constraint-form"
          (mc/-constraint-form (m/constraint (m/schema (m/-any-schema)))))))
  (testing ":parse-constraint desugars constraints"
    (is (= :count
           (m/type (m/constraint [:min 1] count-constraint-options))))
    (is (= (m/properties (m/constraint [:min 1] count-constraint-options))
           {:min 1}))
    (is (= (m/properties (m/constraint [:max 1] count-constraint-options))
           {:min 0 :max 1}))
    (testing "properties not forwarded in :max"
      (is (= {:min 0 :max 1} (m/properties (m/constraint [:max {:property true} 1] count-constraint-options)))))))

(defn string-context []
  {::m/constraint-context (:string (m/base-constraint-extensions))})

(defn errors [{:keys [errors]}]
  (mapv #(update % :schema m/form) errors))

(deftest string-constraint-test
  (is (= :count (m/type (m/constraint [:min 1] (string-context)))))
  (is (= [:min 1] (mc/-constraint-form (m/constraint [:min 1] (string-context)))))
  (is (= [:count {:min 1}] (m/form (m/constraint [:min 1] (string-context)))))
  (is (= :count (m/type (m/constraint [:max 1] (string-context)))))
  (is (= [:max 1] (mc/-constraint-form (m/constraint [:max 1] (string-context)))))
  (is (= [:count {:max 1}] (m/form (m/constraint [:max 1] (string-context)))))
  (is (= :any (m/form (m/constraint [:any] (string-context)))))
  (is (= [:any] (mc/-constraint-form (m/constraint [:any] (string-context)))))
  ;;FIXME
  #_
  (is (= ::FIXME
         (m/ast (m/constraint [:max 1] (string-context)))))
  ;;FIXME
  #_
  (is (= ::FIXME
         (m/ast (m/constraint [:and [:min 1] [:max 2]] (string-context)))))
  ;;TODO have a separate -constraint-form for pretty printing
  ;; use -form for independent printing
  ;; don't store constraint context in constraint
  (testing "constraints are simplified" ;;TODO actually simplify?
    (is (= [:and [:min 0] [:min 1] [:max 1] [:max 2]]
           (mc/-constraint-form (m/constraint [:and [:min 0] [:min 1] [:max 1] [:max 2]] (string-context))))))
  (testing "but properties are preserved"
    (is (= [:string {:and [[:and [:min 1] [:max 1]]]}]
           (m/form (m/schema [:string {:and [[:and [:min 1] [:max 1]]]}] (string-context))))))
  (is (m/validate (m/constraint [:and [:min 1] [:max 1]] (string-context)) "a"))
  (is (m/validate (m/schema [:string {:min 1 :max 1}]) "a"))
  (is (m/validate (m/schema [:string {:and [[:min 1] [:max 1]]}]) "a"))
  (is (not (m/validate (m/constraint [:and [:min 1] [:max 1]] (string-context)) "")))
  (is (= '({:path [], :in [], :schema [:count {:min 1}], :value "" :type ::m/count-limits})
         (errors (m/explain (m/constraint [:min 1] (string-context)) ""))))
  (is (= '({:path [], :in [], :schema [:count {:max 1}], :value "12" :type ::m/count-limits})
         (errors (m/explain (m/constraint [:max 1] (string-context)) "12"))))
  (is (= '({:path [0], :in [], :schema [:count {:min 1}], :value "" :type ::m/count-limits})
         (errors (m/explain (m/constraint [:and [:min 1] [:max 1]] (string-context)) ""))))
  (is (= [{:path [:malli.constraint/constraint], :in [], :schema [:count {:min 5}], :value "", :type ::m/count-limits}]
         (errors (m/explain (m/schema [:string {:min 5}]) ""))))
  (is (= [{:path [:malli.constraint/constraint], :in [], :schema [:count {:max 1}], :value "20", :type ::m/count-limits}]
         (errors (m/explain (m/schema [:string {:max 1}]) "20"))))
  (is (= [{:path [:malli.constraint/constraint 0], :in [], :schema [:count {:min 5}], :value "", :type ::m/count-limits}]
         (errors (m/explain (m/schema [:string {:and [[:min 5] [:max 10]]}]) ""))))
  (is (= [{:path [:malli.constraint/constraint 1], :in [], :schema [:count {:min 5}], :value "", :type ::m/count-limits}]
         (errors (m/explain (m/schema [:string {:min 5 :max 10}]) ""))))
  (is (= [{:path [:malli.constraint/constraint 1], :in [], :schema [:count {:min 1}], :value "", :type ::m/count-limits}]
         (errors (m/explain (m/schema [:string {:min 1 :max 1}]) ""))))
  (is (= [{:path [:malli.constraint/constraint 0], :in [], :schema [:count {:min 1}], :value "", :type ::m/count-limits}]
         (errors (m/explain (m/schema [:string {:and [[:min 1] [:max 1]]}]) ""))))
  (is (= [{:path [:malli.constraint/constraint 0], :in [], :schema [:count {:min 1}], :value "", :type ::m/count-limits}]
         (errors (m/explain (m/schema [:string {:and [[:min 1] [:max 1]]}]) ""))))
  ;;TODO "should be 1 character"
  (is (= ["should be at least 1 character"]
         (me/humanize (m/explain (m/schema [:string {:min 1 :max 1}]) ""))
         (me/humanize (m/explain (m/schema [:string {:and [[:min 1] [:max 1]]}]) ""))))
  (is (= ["should be at most 1 character"]
         (me/humanize (m/explain (m/schema [:string {:and [[:max 1]]}]) "12"))
         (me/humanize (m/explain (m/schema [:string {:max 1}]) "12")))))
