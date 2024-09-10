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
  {::m/constraint-options {:parse-constraint {:min (fn [{:keys [properties children]} opts]
                                                     [::mc/count-constraint (first children) nil])
                                              :max (fn [{:keys [properties children]} opts]
                                                     [::mc/count-constraint 0 (first children)])}}
   :registry {::mc/count-constraint (mc/-count-constraint)}})

(def true-constraint-options
  {::m/constraint-options {::mc/true-constraint (fn [c opts] :true)}
   :registry {::mc/true-constraint (mc/-true-constraint)}})

(deftest constraint-test
  (testing "Constraints are returned as-is"
    (is (= (m/type (mc/constraint (m/schema (mc/-true-constraint))))
           (m/type (mc/constraint (m/schema (mc/-true-constraint)) nil))
           ::mc/true-constraint)))
  (testing "IntoSchema's are not allowed in raw form"
    (is (thrown-with-msg?
          Exception #":malli\.constraint/missing-parse-constraint-options"
          (mc/constraint [::mc/true-constraint]
                         {:registry {::mc/true-constraint (mc/-true-constraint)}}))))
  (testing "m/form requires a constraint context"
    (is (thrown-with-msg?
          Exception #":malli\.constraint/no-constraint-form"
          (m/form (mc/constraint (m/schema (mc/-true-constraint)))))))
  (testing ":parse-constraint desugars constraints"
    (is (= (m/type (mc/constraint [:min 1] count-constraint-options))
           ::mc/count-constraint))
    (is (= (m/children (mc/constraint [:min 1] count-constraint-options))
           [1 nil]))
    (is (= (m/children (mc/constraint [:max 1] count-constraint-options))
           [0 1]))
    (testing "properties unused in :max"
      (is (nil? (m/properties (mc/constraint [:max {:property true} 1] count-constraint-options)))))))

(defn string-options []
  {::m/constraint-options (:string (mc/base-constraint-extensions))
   :registry (merge (mc/base-constraints)
                    (m/base-schemas))})

(defn errors [{:keys [errors]}]
  (mapv #(update % :schema m/form) errors))

(deftest string-constraint-test
  (is (= ::mc/count-constraint (m/type (mc/constraint [:min 1] (string-options)))))
  (is (= [:min 1] (m/form (mc/constraint [:min 1] (string-options)))))
  (is (= ::mc/count-constraint (m/type (mc/constraint [:max 1] (string-options)))))
  (is (= [:max 1] (m/form (mc/constraint [:max 1] (string-options)))))
  ;;TODO
  #_(is (= ::FIXME (m/ast (mc/constraint [:max 1] (string-options)))))
  (is (= [:and [:min 1] [:max 1]] (m/form (mc/constraint [:and [:min 1] [:max 1]] (string-options)))))
  (is (m/validate (mc/constraint [:and [:min 1] [:max 1]] (string-options)) "a"))
  (is (not (m/validate (mc/constraint [:and [:min 1] [:max 1]] (string-options)) "")))
  (is (= '({:path [], :in [], :schema [:min 1], :value "" :type ::mc/count-limits})
         (errors (m/explain (mc/constraint [:min 1] (string-options)) ""))))
  (is (= '({:path [0], :in [], :schema [:min 1], :value "" :type ::mc/count-limits})
         (errors (m/explain (mc/constraint [:and [:min 1] [:max 1]] (string-options)) ""))))
  #_(is (m/explain (m/schema [:string {:min 1 :max 1}]) ""))
  )
