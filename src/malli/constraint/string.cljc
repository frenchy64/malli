(ns malli.constraint.string
  (:require [malli.core :as-alias m]))

(defn schema-constraints []
  {:string {:parse-properties {:max (fn [v opts]
                                      [::m/count-constraint 0 v])
                               :min (fn [v opts]
                                      [::m/count-constraint v nil])
                               :gen/max (fn [v opts]
                                          [::m/count-constraint {::m/gen true} 0 v])
                               :gen/min (fn [v opts]
                                          [::m/count-constraint {::m/gen true} v nil])
                               :and (fn [vs opts]
                                      (into [::m/and nil] vs))}
            :unparse-properties {::m/count-constraint
                                 (fn [c c-properties [cmin cmax :as c-children] into-properties opts]
                                   (cond-> into-properties
                                     cmax (update (if (::m/gen c-properties) :gen/max :max) #(if % (min % cmax) cmax))
                                     (pos? cmin) (update (if (::m/gen c-properties) :gen/min :min) #(if % (max % cmin) cmin))))
                                 ::m/and
                                 (fn [c c-properties [cmin cmax :as c-children] into-properties opts]
                                   (reduce (fn [into-properties]
                                             (or (::m/unparse-properties c into-properties opts)
                                                 ))
                                           into-properties))
                                 }
            }})
