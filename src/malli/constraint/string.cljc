(ns malli.constraint.string)

(defn schema-constraints []
  {:string {:flat-property-keys #{:max :gen/max :min :gen/min}
            :conjunction-constraint-key :and
            :nested-property-keys #{:and}
            :generator-constraint-types {:max :max-count
                                         :min :min-count
                                         :gen/max :max-count
                                         :gen/min :min-count
                                         :and :and}
            :validator-constraint-types {:max :max-count
                                         :min :min-count
                                         :gen/max :any
                                         :gen/min :any
                                         :and :and}}})
