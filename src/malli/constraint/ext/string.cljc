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
                                    {;; [:count 5 5] => [::mc/count-constraint 5 5]
                                     :count (fn [{:keys [properties children]} opts]
                                              (m/-check-children! :count properties children 2 2)
                                              (into [::mc/count-constraint properties] children))
                                     ;; [:max 5] => [::mc/count-constraint 0 5]
                                     :max (fn [{:keys [properties children]} opts]
                                            (m/-check-children! :max properties children 1 1)
                                            [::mc/count-constraint 0 (first children)])
                                     ;; [:min 5] => [::mc/count-constraint 5 nil]
                                     :min (fn [{:keys [properties children]} opts]
                                            (m/-check-children! :min properties children 1 1)
                                            [::mc/count-constraint (first children) nil])
                                     ;; [:gen/max 5] => [::mc/count-constraint {:gen/max 5} 0 nil]
                                     :gen/max (fn [{:keys [properties children]} opts]
                                                (m/-check-children! :gen/max properties children 1 1)
                                                [::mc/count-constraint {:gen/max (first children)} 0 nil])
                                     ;; [:gen/min 5] => [::mc/count-constraint {:gen/min 5} 0 nil]
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
                                                                ;; [:string {:and [:true]} <= [::mc/count-constraint 0 nil]
                                                                0 [:true]
                                                                ;; [:string {:and [:min 5]} <= [::mc/count-constraint 5 nil]
                                                                ;; [:string {:and [:max 4]} <= [::mc/count-constraint 0 4]
                                                                ;; [:string {:and [:gen/min 5]}] <= [::mc/count-constraint {:gen/min 5} 0 nil]
                                                                ;; [:string {:and [:gen/max 4]}] <= [::mc/count-constraint {:gen/max 4} 0 nil]
                                                                1 (first frms)
                                                                ;; [:string {:and [:count {:gen/min 1 :gen/max 2} 3 4]] <= [::mc/count-constraint {:gen/min 1 :gen/max 2} 3 4]
                                                                (let [ps (cond-> nil
                                                                           (some-> gen-min pos?) (assoc :gen/min gen-min)
                                                                           gen-max (assoc :gen/max gen-max))]
                                                                  (-> [:count]
                                                                      (cond-> ps (conj ps))
                                                                      (conj min-count max-count))))))})
            :parse-properties (into (mcu/default-parse-properties)
                                    {;; (m/-get-constraint [:string {:count 5}]) => [:count 5 5]
                                     :count (fn [v opts]
                                              ;;FIXME ??
                                              (if (nat-int? v)
                                                [:count v v]
                                                (into [:count] v)))
                                     ;; (m/-get-constraint [:string {:max 5}]) => [:max 5]
                                     :max (fn [v opts] [:max v])
                                     ;; (m/-get-constraint [:string {:min 5}]) => [:min 5]
                                     :min (fn [v opts] [:min v])
                                     ;; (m/-get-constraint [:string {:gen/max 5}]) => [:gen/max 5]
                                     :gen/max (fn [v opts] [:gen/max v])
                                     ;; (m/-get-constraint [:string {:gen/min 5}]) => [:gen/min 5]
                                     :gen/min (fn [v opts] [:gen/min v])})
            :unparse-properties (into (mcu/default-unparse-properties)
                                      {::mc/count-constraint
                                       (fn [c into-properties _]
                                         (let [[cmin cmax] (m/children c)
                                               c-properties (m/properties c)]
                                           (cond-> into-properties
                                             ;; [:string {:max 4}] <= [::mc/count-constraint 0 4]
                                             ;; :string <= [::mc/count-constraint 0 nil]
                                             cmax (update (if (::mc/gen c-properties) :gen/max :max) #(if % (min % cmax) cmax))
                                             ;; [:string {:min 5}] <= [::mc/count-constraint 5 nil]
                                             ;; :string <= [::mc/count-constraint 0 nil]
                                             (pos? cmin) (update (if (::mc/gen c-properties) :gen/min :min) #(if % (max % cmin) cmin)))))})}})
