(ns malli.constraint-test
  (:require [clojure.string :as str]
            [clojure.test :refer [are deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.walk :as walk]
            [malli.constraint :as mc]
            [malli.constraint.protocols :as mcp]
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
   :registry {::mc/count-constraint (mc/-count-constraint)}})

(def true-constraint-options
  {::m/constraint-context {::mc/true-constraint (fn [c opts] :true)}
   :registry {::mc/true-constraint (mc/-true-constraint)}})

(deftest constraint-test
  (testing "Constraints are returned as-is"
    (is (= (m/type (mc/constraint (m/schema (mc/-true-constraint))))
           (m/type (mc/constraint (m/schema (mc/-true-constraint)) nil))
           ::mc/true-constraint)))
  (testing "IntoSchema's are not allowed in raw form"
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.constraint/missing-parse-constraint-options"
          (mc/constraint [::mc/true-constraint]
                         {:registry {::mc/true-constraint (mc/-true-constraint)}}))))
  (testing "m/form requires a constraint context"
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.constraint/no-constraint-form"
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

(defn constraint-options []
  {::m/constraint-options (mc/base-constraint-extensions)
   :registry (merge (mc/base-constraints)
                    (m/default-schemas))})

(defn string-context []
  (assoc (constraint-options) ::m/constraint-context (:string (mc/base-constraint-extensions))))

(defn errors [{:keys [errors]}]
  (mapv #(update % :schema m/form) errors))

(deftest string-constraint-test
  (is (= ::mc/count-constraint (m/type (mc/constraint [:min 1] (string-context)))))
  (is (= [:min 1] (m/form (mc/constraint [:min 1] (string-context)))))
  (is (= ::mc/count-constraint (m/type (mc/constraint [:max 1] (string-context)))))
  (is (= [:max 1] (m/form (mc/constraint [:max 1] (string-context)))))
  (is (= [:true] (m/form (mc/constraint [:true] (string-context)))))
  ;;TODO
  #_(is (= ::FIXME (m/ast (mc/constraint [:max 1] (string-context)))))
  (testing "constraints are simplified"
    (is (= [:count 1 1] (m/form (mc/constraint [:and [:min 1] [:max 1]] (string-context))))))
  (testing "but properties are preserved"
    (is (= [:string {:and [[:and [:min 1] [:max 1]]]}]
           (m/form (m/schema [:string {:and [[:and [:min 1] [:max 1]]]}] (string-context))))))
  (is (m/validate (mc/constraint [:and [:min 1] [:max 1]] (string-context)) "a"))
  (is (m/validate (m/schema [:string {:min 1 :max 1}] (constraint-options)) "a"))
  (is (m/validate (m/schema [:string {:and [[:min 1] [:max 1]]}] (constraint-options)) "a"))
  (is (not (m/validate (mc/constraint [:and [:min 1] [:max 1]] (string-context)) "")))
  (is (= '({:path [], :in [], :schema [:min 1], :value "" :type ::mc/count-limits})
         (errors (m/explain (mc/constraint [:min 1] (string-context)) ""))))
  (is (= '({:path [], :in [], :schema [:max 1], :value "12" :type ::mc/count-limits})
         (errors (m/explain (mc/constraint [:max 1] (string-context)) "12"))))
  (is (= '({:path [], :in [], :schema [:count 1 1], :value "" :type ::mc/count-limits})
         (errors (m/explain (mc/constraint [:and [:min 1] [:max 1]] (string-context)) ""))))
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
  (is (= [{:path [:malli.constraint/constraint], :in [], :schema [:count 5 10], :value "", :type :malli.constraint/count-limits}]
         (errors
           (m/explain (m/schema [:string {:min 5 :max 10}]
                                (constraint-options))
                      ""))
         (errors
           (m/explain (m/schema [:string {:and [[:max 10] [:min 5]]}]
                                (constraint-options))
                      ""))))
  (is (= [{:path [:malli.constraint/constraint], :in [], :schema [:count 1 1], :value "", :type :malli.constraint/count-limits}]
         (errors
           (m/explain (m/schema [:string {:min 1 :max 1}]
                                (constraint-options))
                      ""))
         (errors
           (m/explain (m/schema [:string {:and [[:max 1] [:min 1]]}]
                                (constraint-options))
                      ""))))
  (is (= [{:path [:malli.constraint/constraint], :in [], :schema [:count 1 1], :value "", :type :malli.constraint/count-limits}]
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
                      ""))
         (me/humanize
           (m/explain (m/schema [:string {:and [[:count 1 1]]}]
                                (constraint-options))
                      ""))
         (me/humanize
           (m/explain (m/schema [:string {:count [1 1]}]
                                (constraint-options))
                      ""))
         (me/humanize
           (m/explain (m/schema [:string {:count 1}]
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
                      "12"))
         (me/humanize
           (m/explain (m/schema [:string {:count [0 1]}]
                                (constraint-options))
                      "12"))
         (me/humanize
           (m/explain (m/schema [:string {:and [[:count 0 1]]}]
                                (constraint-options))
                      "12")))))
