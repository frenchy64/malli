(ns malli.constraint.countable.humanize
  (:require [malli.impl.util :as miu]))

(defn- -first-child [f]
  (fn [{:keys [constraint value]} _]
    (let [[n :as all] (subvec constraint 1)
          _ (when-not (= 1 (count all))
              (miu/-fail! ::min-max-constraint-takes-one-child {:constraint constraint}))
          _ (when-not (nat-int? n)
              (miu/-fail! ::min-max-constraint-takes-integer {:constraint constraint}))]
      (f value n))))

(defn humanizers []
  {:max-count (-first-child (fn [value max]
                              (let [cnt (miu/-safe-count value)]
                                (when-not (<= cnt max)
                                  (str "should be at most " max
                                       (if (string? value)
                                         " character"
                                         " element")
                                       (when-not (= 1 max) "s")
                                       ", given " cnt)))))
   :min-count (-first-child (fn [value min]
                              (let [cnt (miu/-safe-count value)]
                                (when-not (<= min cnt)
                                  (str "should be at least " min
                                       (if (string? value)
                                         " character"
                                         " element")
                                       (when-not (= 1 min) "s")
                                       ", given " cnt)))))})
