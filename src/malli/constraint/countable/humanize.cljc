(ns malli.constraint.countable.humanize
  (:require [malli.core :as m]
            [malli.impl.util :as miu]))

(defn humanizers []
  {::m/count-constraint (fn [{:keys [constraint value]} _]
                          (let [[min max] (m/children constraint)
                                ;; TODO bounded count
                                cnt (miu/-safe-count value)]
                            (cond
                              (and min max)
                              (if (= min max)
                                (when-not (= cnt max)
                                  (str "should be " max
                                       (if (string? value)
                                         " character"
                                         " element")
                                       (when-not (= 1 max) "s")
                                       ", given " cnt))
                                (when-not (<= cnt max)
                                  (str "should be at most " max
                                       (if (string? value)
                                         " character"
                                         " element")
                                       (when-not (= 1 max) "s")
                                       ", given " cnt)))
                              min (when-not (<= min cnt)
                                    (str "should be at least " min
                                         (if (string? value)
                                           " character"
                                           " element")
                                         (when-not (= 1 min) "s")
                                         ", given " cnt))
                              max (when-not (<= cnt max)
                                    (str "should be at most " max
                                         (if (string? value)
                                           " character"
                                           " element")
                                         (when-not (= 1 max) "s")
                                         ", given " cnt)))))})
