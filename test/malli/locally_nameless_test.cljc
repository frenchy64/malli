(ns malli.locally-nameless-test
  (:require [clojure.test :refer [are deftest is testing]]
            [malli.core :as m]
            [malli.locally-nameless :as mln]
            [malli.registry :as mr]))

(def options {:registry (mr/composite-registry m/default-registry (mln/schemas))})

(deftest -abstract-test
  (is (= (mln/-scoped [::mln/b 0])
         (m/form (mln/-abstract [::mln/f :a] :a options))))
  (is (= (mln/-scoped [:schema {:registry {::a [::mln/b 0]}} ::a])
         (m/form (mln/-abstract [:schema {:registry {::a [::mln/f :a]}} ::a] :a options))))
  (is (= (mln/-scoped [::mln/b 1] 2)
         (m/form (mln/-abstract (mln/-scoped [::mln/f :a]) :a options))))
  (is (= (mln/-scoped [::mln/b 2] 3)
         (m/form (mln/-abstract (mln/-scoped [::mln/f :a] 2) :a options))))
  (is (= (mln/-scoped [:-> [::mln/b 1] [::mln/b 2]] 3)
         (m/form (mln/-abstract (mln/-scoped [:-> [::mln/b 1] [::mln/f :a]] 2) :a options)))))

(deftest -instantiate-test
  (is (= :any
         (m/form
           (mln/-instantiate (mln/-scoped [::mln/b 0]) :any options))))
  (is (= [:schema {:registry {::a :a}} ::a]
         (m/form
           (mln/-instantiate (mln/-scoped [:schema {:registry {::a [::mln/b 0]}} ::a])
                             [::mln/f :a]
                             options))))
  (is (= (mln/-scoped :int)
         (m/form
           (mln/-instantiate
             (mln/-scoped [::mln/b 1] 2)
             :int
             options))))
  (is (= (mln/-scoped :int 2)
         (m/form
           (mln/-instantiate
             (mln/-scoped [::mln/b 2] 3)
             :int
             options))))
  (is (= (mln/-scoped [:-> [::mln/b 1] :int] 2)
         (m/form
           (mln/-instantiate
             (mln/-scoped [:-> [::mln/b 1] [::mln/b 2]] 3)
             :int
             options)))))

(deftest fv-test
  (is (= #{:a123} (mln/-fv [::mln/f :a123] options)))
  (is (= #{:a :b} (mln/-fv [:schema {:registry {::a [::mln/f :b]}}
                            [::mln/f :a]]
                           options))))
