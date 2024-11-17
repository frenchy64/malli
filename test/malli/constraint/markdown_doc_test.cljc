(ns malli.constraint.markdown-doc-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [malli.constraint :as mc]
            [malli.core :as m]
            [malli.error :as me]
            [malli.constraint.protocols :as mcp]))

(defn constraint-options []
  {::m/constraint-options (mc/base-constraint-extensions)
   :registry (merge (mc/base-constraints)
                    (m/default-schemas))})

#?(:clj
   (deftest constraint-md-test
     (testing "constraint validators don't have preconditions"
       (is (false? (-> [:string {:max 1}]
                       (m/schema (constraint-options))
                       (m/validate 1))))
       (is (thrown-with-msg? Exception
                             #"Don't know how to create ISeq from: java\.lang\.Long"
                             (-> [:string {:max 1}]
                                 (m/schema (constraint-options))
                                 mcp/-get-constraint
                                 (m/validate 1)))))))
