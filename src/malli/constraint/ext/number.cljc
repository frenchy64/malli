(ns malli.constraint.ext.number
  (:require [malli.core :as m]
            [malli.constraint :as-alias mc]
            [malli.constraint.util :as mcu]))

(defn -base-number-extension []
  {:-walk mcu/-walk-leaf+constraints
   :constraint-from-properties mcu/-constraint-from-properties
   :parse-constraint (into (mcu/default-parse-constraints)
                           {;; [:max 5] => [::mc/range-constraint nil 5]
                            :max (fn [{:keys [properties children]} opts]
                                   (m/-check-children! :max properties children 1 1)
                                   [::mc/range-constraint nil nil (first children)])
                            ;; [:min 5] => [::mc/range-constraint 5 nil]
                            :min (fn [{:keys [properties children]} opts]
                                   (m/-check-children! :min properties children 1 1)
                                   [::mc/range-constraint nil (first children) nil])
                            ;; [:gen/max 5] => [::mc/range-constraint {:gen/max 5} 0 nil]
                            :gen/max (fn [{:keys [properties children]} opts]
                                       (m/-check-children! :gen/max properties children 1 1)
                                       [::mc/range-constraint {:gen/max (first children)} nil nil])
                            ;; [:gen/min 5] => [::mc/range-constraint {:gen/min 5} 0 nil]
                            :gen/min (fn [{:keys [properties children]} opts]
                                       (m/-check-children! :gen/min properties children 1 1)
                                       [::mc/range-constraint {:gen/min (first children)} nil nil])})
   :constraint-form (into (mcu/default-constraint-form)
                          {::mc/range-constraint (fn [c options]
                                                   (let [[min-range max-range] (m/children c)
                                                         {gen-min :gen/min gen-max :gen/max} (m/properties c)
                                                         frms (cond-> []
                                                                min-range (conj [:min min-range])
                                                                max-range (conj [:max max-range])
                                                                gen-min (conj [:gen/min gen-min])
                                                                gen-max (conj [:gen/max gen-max]))]
                                                     (case (count frms)
                                                       ;; [:int {:and [:true]} <= [::mc/range-constraint 0 nil]
                                                       0 [:true]
                                                       ;; [:int {:and [:min 5]} <= [::mc/range-constraint 5 nil]
                                                       ;; [:int {:and [:max 4]} <= [::mc/range-constraint nil 4]
                                                       ;; [:int {:and [:gen/min 5]}] <= [::mc/range-constraint {:gen/min 5} nil nil]
                                                       ;; [:int {:and [:gen/max 4]}] <= [::mc/range-constraint {:gen/max 4} nil nil]
                                                       1 (first frms)
                                                       ;; [:int {:and [:range {:gen/min 1 :gen/max 2} 3 4]}] <= [::mc/range-constraint {:gen/min 1 :gen/max 2} 3 4]
                                                       (into [:and] frms))))})
   :parse-properties (into (mcu/default-parse-properties)
                           {;; (m/-get-constraint [:int {:max 5}]) => [:max 5]
                            :max (fn [v opts] [:max v])
                            ;; (m/-get-constraint [:int {:min 5}]) => [:min 5]
                            :min (fn [v opts] [:min v])
                            ;; (m/-get-constraint [:int {:gen/max 5}]) => [:gen/max 5]
                            :gen/max (fn [v opts] [:gen/max v])
                            ;; (m/-get-constraint [:int {:gen/min 5}]) => [:gen/min 5]
                            :gen/min (fn [v opts] [:gen/min v])})
   :unparse-properties (into (mcu/default-unparse-properties)
                             {::mc/range-constraint
                              (fn [c into-properties _]
                                (let [[cmin cmax] (m/children c)
                                      c-properties (m/properties c)]
                                  (cond-> into-properties
                                    ;; [:int {:max 4}] <= [::mc/range-constraint 0 4]
                                    ;; :int <= [::mc/range-constraint nil nil]
                                    ;;FIXME what does ::mc/gen mean?
                                    cmax (update (if (::mc/gen c-properties) :gen/max :max) #(if % (min % cmax) cmax))
                                    ;; [:int {:min 5}] <= [::mc/range-constraint 5 nil]
                                    ;; :int <= [::mc/range-constraint nil nil]
                                    ;;FIXME
                                    (pos? cmin) (update (if (::mc/gen c-properties) :gen/min :min) #(if % (max % cmin) cmin)))))})})

(defn base-constraint-extensions []
  (let [ext (-base-number-extension)]
    {:int ext
     :double ext
     :float ext}))
