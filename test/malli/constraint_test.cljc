(ns malli.constraint-test
  (:require [clojure.string :as str]
            [clojure.test :refer [are deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.walk :as walk]
            [malli.constraint :as mc]
            [malli.constraint.protocols :as mcp]
            [malli.constraint.util :as mcu]
            [malli.constraint.true :refer [-true-constraint]]
            [malli.constraint.count :refer [-count-constraint]]
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
                                                     [::mc/count-constraint (first children) nil])
                                              :max (fn [{:keys [properties children]} opts]
                                                     [::mc/count-constraint 0 (first children)])}}
   :registry {::mc/count-constraint (-count-constraint)}})

(def true-constraint-options
  {::m/constraint-context {::mc/true-constraint (fn [c opts] :true)}
   :registry {::mc/true-constraint (-true-constraint)}})

(deftest constraint-test
  (testing "Constraints are returned as-is"
    (is (= ::mc/true-constraint
           (m/type (mcu/constraint (m/schema (-true-constraint))))
           (m/type (mcu/constraint (m/schema (-true-constraint)) nil)))))
  (testing "IntoSchema's are not allowed in raw form"
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.constraint/missing-parse-constraint-options"
          (mcu/constraint [::mc/true-constraint]
                         {:registry {::mc/true-constraint (-true-constraint)}}))))
  (testing "m/form requires a constraint context"
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.constraint/no-constraint-form"
          (m/form (mcu/constraint (m/schema (-true-constraint)))))))
  (testing ":parse-constraint desugars constraints"
    (is (= ::mc/count-constraint
           (m/type (mcu/constraint [:min 1] count-constraint-options))))
    (is (= (m/children (mcu/constraint [:min 1] count-constraint-options))
           [1 nil]))
    (is (= (m/children (mcu/constraint [:max 1] count-constraint-options))
           [0 1]))
    (testing "properties unused in :max"
      (is (nil? (m/properties (mcu/constraint [:max {:property true} 1] count-constraint-options)))))))

(defn constraint-options []
  {::m/constraint-options (mc/base-constraint-extensions)
   :registry (merge (mc/base-constraints)
                    (m/default-schemas))})

(defn string-context []
  (assoc (constraint-options) ::m/constraint-context (:string (mc/base-constraint-extensions))))

(defn errors [{:keys [errors]}]
  (mapv #(update % :schema m/form) errors))

(deftest string-constraint-test
  (is (= ::mc/count-constraint (m/type (mcu/constraint [:min 1] (string-context)))))
  (is (= [:min 1] (m/form (mcu/constraint [:min 1] (string-context)))))
  (is (= ::mc/count-constraint (m/type (mcu/constraint [:max 1] (string-context)))))
  (is (= [:max 1] (m/form (mcu/constraint [:max 1] (string-context)))))
  (is (= [:true] (m/form (mcu/constraint [:true] (string-context)))))
  ;;FIXME
  #_
  (is (= ::FIXME
         (m/ast (mcu/constraint [:max 1] (string-context)))))
  ;;FIXME
  #_
  (is (= ::FIXME
         (m/ast (mcu/constraint [:min 1] (string-context)))))
  ;;FIXME
  #_
  (is (= ::FIXME
         (m/ast (mcu/constraint [:and [:min 1] [:max 2]] (string-context)))))
  (testing "constraints are simplified"
    (is (= [:and [:min 1] [:max 1]]
           (m/form (mcu/constraint [:and [:min 0] [:min 1] [:max 1] [:max 2]] (string-context))))))
  (testing "but properties are preserved"
    (is (= [:string {:and [[:and [:min 1] [:max 1]]]}]
           (m/form (m/schema [:string {:and [[:and [:min 1] [:max 1]]]}] (string-context))))))
  (is (m/validate (mcu/constraint [:and [:min 1] [:max 1]] (string-context)) "a"))
  (is (m/validate (m/schema [:string {:min 1 :max 1}] (constraint-options)) "a"))
  (is (m/validate (m/schema [:string {:and [[:min 1] [:max 1]]}] (constraint-options)) "a"))
  (is (not (m/validate (mcu/constraint [:and [:min 1] [:max 1]] (string-context)) "")))
  (is (= '({:path [], :in [], :schema [:min 1], :value "" :type ::mc/count-limits})
         (errors (m/explain (mcu/constraint [:min 1] (string-context)) ""))))
  (is (= '({:path [], :in [], :schema [:max 1], :value "12" :type ::mc/count-limits})
         (errors (m/explain (mcu/constraint [:max 1] (string-context)) "12"))))
  (is (= '({:path [], :in [], :schema [:and [:min 1] [:max 1]], :value "" :type ::mc/count-limits})
         (errors (m/explain (mcu/constraint [:and [:min 1] [:max 1]] (string-context)) ""))))
  (is (= [{:path [:malli.constraint/constraint], :in [], :schema [:min 5], :value "", :type :malli.constraint/count-limits}]
         (errors
           (m/explain (m/schema [:string {:min 5}]
                                (constraint-options))
                      ""))))
  (is (= [{:path [:malli.constraint/constraint], :in [], :schema [:max 1], :value "20", :type :malli.constraint/count-limits}]
         (errors
           (m/explain (m/schema [:string {:max 1}]
                                (constraint-options))
                      "20"))))
  ;; TODO should be :path [:malli.constraint/constraint 0]
  (is (= [{:path [:malli.constraint/constraint], :in [], :schema [:and [:min 5] [:max 10]], :value "", :type :malli.constraint/count-limits}]
         (errors
           (m/explain (m/schema [:string {:min 5 :max 10}]
                                (constraint-options))
                      ""))
         (errors
           (m/explain (m/schema [:string {:and [[:min 5] [:max 10]]}]
                                (constraint-options))
                      ""))))
  ;; TODO should be [:schema [:min 1]], :in [0]
  (is (= [{:path [:malli.constraint/constraint], :in [], :schema [:and [:min 1] [:max 1]], :value "", :type :malli.constraint/count-limits}]
         (errors
           (m/explain (m/schema [:string {:min 1 :max 1}]
                                (constraint-options))
                      ""))
         (errors
           (m/explain (m/schema [:string {:and [[:min 1] [:max 1]]}]
                                (constraint-options))
                      ""))))
  (is (= [{:path [:malli.constraint/constraint], :in [], :schema [:and [:min 1] [:max 1]], :value "", :type :malli.constraint/count-limits}]
         (errors
           (m/explain (m/schema [:string {:and [[:min 1] [:max 1]]}]
                                (constraint-options))
                      ""))))
  (is (= ["should have 1 character"]
         (me/humanize
           (m/explain (m/schema [:string {:min 1 :max 1}]
                                (constraint-options))
                      ""))
         (me/humanize
           (m/explain (m/schema [:string {:and [[:min 1] [:max 1]]}]
                                (constraint-options))
                      ""))))
  (is (= ["should have at most 1 character"]
         (me/humanize
           (m/explain (m/schema [:string {:and [[:max 1]]}]
                                (constraint-options))
                      "12"))
         (me/humanize
           (m/explain (m/schema [:string {:max 1}]
                                (constraint-options))
                      "12")))))
