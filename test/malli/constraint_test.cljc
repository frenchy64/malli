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
                                                     [::m/count-constraint (first children) nil])
                                              :max (fn [{:keys [properties children]} opts]
                                                     [::m/count-constraint 0 (first children)])}}
   :registry {::m/count-constraint (m/-count-constraint)}})

(deftest constraint-test
  (testing "Constraints are returned as-is"
    (is (= ::m/true-constraint
           (m/type (m/constraint (m/schema (m/-true-constraint))))
           (m/type (m/constraint (m/schema (m/-true-constraint)) nil)))))
  (testing "IntoSchema's are not allowed in raw form"
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.core/missing-parse-constraint-options"
          (m/constraint [::m/true-constraint]
                        {:registry {::m/true-constraint (m/-true-constraint)}}))))
  (testing "m/form requires a constraint context"
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.core/no-constraint-form"
          (m/form (m/constraint (m/schema (m/-true-constraint)))))))
  (testing ":parse-constraint desugars constraints"
    (is (= ::m/count-constraint
           (m/type (m/constraint [:min 1] count-constraint-options))))
    (is (= (m/children (m/constraint [:min 1] count-constraint-options))
           [1 nil]))
    (is (= (m/children (m/constraint [:max 1] count-constraint-options))
           [0 1]))
    (testing "properties unused in :max"
      (is (nil? (m/properties (m/constraint [:max {:property true} 1] count-constraint-options)))))))

(defn string-context []
  {::m/constraint-context (:string (m/base-constraint-extensions))})

(defn errors [{:keys [errors]}]
  (mapv #(update % :schema m/form) errors))

(deftest string-constraint-test
  (is (= ::m/count-constraint (m/type (m/constraint [:min 1] (string-context)))))
  (is (= [:min 1] (m/form (m/constraint [:min 1] (string-context)))))
  (is (= ::m/count-constraint (m/type (m/constraint [:max 1] (string-context)))))
  (is (= [:max 1] (m/form (m/constraint [:max 1] (string-context)))))
  (is (= [:true] (m/form (m/constraint [:true] (string-context)))))
  ;;FIXME
  #_
  (is (= ::FIXME
         (m/ast (m/constraint [:max 1] (string-context)))))
  ;;FIXME
  #_
  (is (= ::FIXME
         (m/ast (m/constraint [:min 1] (string-context)))))
  ;;FIXME
  #_
  (is (= ::FIXME
         (m/ast (m/constraint [:and [:min 1] [:max 2]] (string-context)))))
  (testing "constraints are simplified"
    (is (= [:and [:min 1] [:max 1]]
           (m/form (m/constraint [:and [:min 0] [:min 1] [:max 1] [:max 2]] (string-context))))))
  (testing "but properties are preserved"
    (is (= [:string {:and [[:and [:min 1] [:max 1]]]}]
           (m/form (m/schema [:string {:and [[:and [:min 1] [:max 1]]]}] (string-context))))))
  (is (m/validate (m/constraint [:and [:min 1] [:max 1]] (string-context)) "a"))
  (is (m/validate (m/schema [:string {:min 1 :max 1}]) "a"))
  (is (m/validate (m/schema [:string {:and [[:min 1] [:max 1]]}]) "a"))
  (is (not (m/validate (m/constraint [:and [:min 1] [:max 1]] (string-context)) "")))
  (is (= '({:path [], :in [], :schema [:min 1], :value "" :type ::m/count-limits})
         (errors (m/explain (m/constraint [:min 1] (string-context)) ""))))
  (is (= '({:path [], :in [], :schema [:max 1], :value "12" :type ::m/count-limits})
         (errors (m/explain (m/constraint [:max 1] (string-context)) "12"))))
  (is (= '({:path [], :in [], :schema [:and [:min 1] [:max 1]], :value "" :type ::m/count-limits})
         (errors (m/explain (m/constraint [:and [:min 1] [:max 1]] (string-context)) ""))))
  (is (= [{:path [:malli.constraint/constraint], :in [], :schema [:min 5], :value "", :type ::m/count-limits}]
         (errors (m/explain (m/schema [:string {:min 5}]) ""))))
  (is (= [{:path [:malli.constraint/constraint], :in [], :schema [:max 1], :value "20", :type ::m/count-limits}]
         (errors (m/explain (m/schema [:string {:max 1}]) "20"))))
  ;; TODO should be :path [:malli.constraint/constraint 0]
  (is (= [{:path [:malli.constraint/constraint], :in [], :schema [:and [:min 5] [:max 10]], :value "", :type ::m/count-limits}]
         (errors (m/explain (m/schema [:string {:min 5 :max 10}]) ""))
         (errors (m/explain (m/schema [:string {:and [[:min 5] [:max 10]]}]) ""))))
  ;; TODO should be [:schema [:min 1]], :in [0]
  (is (= [{:path [:malli.constraint/constraint], :in [], :schema [:and [:min 1] [:max 1]], :value "", :type ::m/count-limits}]
         (errors (m/explain (m/schema [:string {:min 1 :max 1}]) ""))
         (errors (m/explain (m/schema [:string {:and [[:min 1] [:max 1]]}]) ""))))
  (is (= [{:path [:malli.constraint/constraint], :in [], :schema [:and [:min 1] [:max 1]], :value "", :type ::m/count-limits}]
         (errors (m/explain (m/schema [:string {:and [[:min 1] [:max 1]]}]) ""))))
  (is (= ["should be 1 character"]
         (me/humanize (m/explain (m/schema [:string {:min 1 :max 1}]) ""))
         (me/humanize (m/explain (m/schema [:string {:and [[:min 1] [:max 1]]}]) ""))))
  (is (= ["should be at most 1 character"]
         (me/humanize (m/explain (m/schema [:string {:and [[:max 1]]}]) "12"))
         (me/humanize (m/explain (m/schema [:string {:max 1}]) "12")))))
