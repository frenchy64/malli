(ns malli.constraint.ext.string
  (:require [clojure.core :as cc]
            [clojure.set :as set]
            [malli.constraint.protocols :as mcp]
            [malli.core :as m]
            [malli.constraint :as-alias mc]
            [malli.constraint.util :as mcu]
            [malli.impl.util :as miu :refer [-fail!]]
            [malli.registry :as mr])
  #?(:clj (:import (clojure.lang IPersistentVector))))

(defn base-constraint-extensions []
  {:string {:-walk mcu/-walk-leaf+constraints
            :constraint-from-properties mcu/-constraint-from-properties
            :parse-constraint (into (mcu/default-parse-constraints)
                                    {:count (fn [{:keys [properties children]} opts]
                                              (m/-check-children! :count properties children 2 2)
                                              (into [::mc/count-constraint properties] children))
                                     :max (fn [{:keys [properties children]} opts]
                                            (m/-check-children! :max properties children 1 1)
                                            [::mc/count-constraint 0 (first children)])
                                     :min (fn [{:keys [properties children]} opts]
                                            (m/-check-children! :min properties children 1 1)
                                            [::mc/count-constraint (first children) nil])
                                     :gen/max (fn [{:keys [properties children]} opts]
                                                (m/-check-children! :gen/max properties children 1 1)
                                                [::mc/count-constraint {:gen/max (first children)} 0 nil])
                                     :gen/min (fn [{:keys [properties children]} opts]
                                                (m/-check-children! :gen/min properties children 1 1)
                                                [::mc/count-constraint {:gen/min (first children)} 0 nil])})
            :constraint-form (into (mcu/default-constraint-form)
                                   {::mc/count-constraint (fn [c options]
                                                            (let [[min-count max-count] (m/children c)
                                                                  {gen-min :gen/min gen-max :gen/max} (m/properties c)
                                                                  frms (cond-> []
                                                                         (pos? min-count) (conj [:min min-count])
                                                                         max-count (conj [:max max-count])
                                                                         (some-> gen-min pos?) (conj [:gen/min gen-min])
                                                                         gen-max (conj [:gen/max gen-max]))]
                                                              (case (count frms)
                                                                0 [:true]
                                                                1 (first frms)
                                                                (let [ps (cond-> nil
                                                                           (some-> gen-min pos?) (assoc :gen/min gen-min)
                                                                           gen-max (assoc :gen/max gen-max))]
                                                                  (-> [:count]
                                                                      (cond-> ps (conj ps))
                                                                      (conj min-count max-count))))))})
            :parse-properties (into (mcu/default-parse-properties)
                                    {:count (fn [v opts]
                                              (if (nat-int? v)
                                                [:count v v]
                                                (into [:count] v)))
                                     :max (fn [v opts] [:max v])
                                     :min (fn [v opts] [:min v])
                                     :gen/max (fn [v opts] [:gen/max v])
                                     :gen/min (fn [v opts] [:gen/min v])})
            :unparse-properties (into (mcu/default-unparse-properties)
                                      {::mc/count-constraint
                                       (fn [c into-properties _]
                                         (let [[cmin cmax] (m/children c)
                                               c-properties (m/properties c)]
                                           (cond-> into-properties
                                             cmax (update (if (::mc/gen c-properties) :gen/max :max) #(if % (min % cmax) cmax))
                                             (pos? cmin) (update (if (::mc/gen c-properties) :gen/min :min) #(if % (max % cmin) cmin)))))})}})
