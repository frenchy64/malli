(ns malli.constraint.markdown-doc-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [malli.constraint :as mc]
            [malli.core :as m]
            [malli.error :as me]))

#?(:clj
   (deftest ^:constraints constraint-md-test
     (testing "constraint validators don't have preconditions"
       (is (false? (-> [:string {:max 1}]
                       m/schema
                       (m/validate 1))))
       (is (thrown-with-msg? Exception
                             #"Don't know how to create ISeq from: java\.lang\.Long"
                             (-> [:string {:max 1}]
                                 m/schema
                                 mc/-get-constraint
                                 (m/validate 1)))))))
