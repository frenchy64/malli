(ns malli.constraint.range
  (:require [clojure.core :as cc]
            [malli.constraint.protocols :as mcp]
            [malli.core :as m]
            [malli.constraint.util :as mcu]
            [malli.constraint :as-alias mc]
            [malli.constraint.ext.string :as string-ext]
            [malli.impl.util :as miu :refer [-fail!]]
            [malli.registry :as mr]))

;; assumes :and and :true
(defn default-constraint-extensions []
  {:parse-constraint {;; [:max 5] => [::mc/range-constraint nil 5]
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
                                 [::mc/range-constraint {:gen/min (first children)} nil nil])}
   :constraint-form {::mc/range-constraint (fn [c options]
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
                                                 (into [:and] frms))))}
   :parse-properties {;; (m/-get-constraint [:int {:max 5}]) => [:max 5]
                      :max (fn [v opts] [:max v])
                      ;; (m/-get-constraint [:int {:min 5}]) => [:min 5]
                      :min (fn [v opts] [:min v])
                      ;; (m/-get-constraint [:int {:gen/max 5}]) => [:gen/max 5]
                      :gen/max (fn [v opts] [:gen/max v])
                      ;; (m/-get-constraint [:int {:gen/min 5}]) => [:gen/min 5]
                      :gen/min (fn [v opts] [:gen/min v])}
   :unparse-properties {::mc/range-constraint
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
                              (pos? cmin) (update (if (::mc/gen c-properties) :gen/min :min) #(if % (max % cmin) cmin)))))}})

(defn -range-constraint []
  (let [type ::mc/range-constraint]
    ^{:type ::m/into-schema}
    (reify
      m/AST
      (-from-ast [parent ast options] (throw (ex-info "TODO" {})))
      m/IntoSchema
      (-type [_] type)
      (-type-properties [_])
      (-properties-schema [_ _])
      (-children-schema [_ _])
      (-into-schema [parent properties children options]
        (m/-check-children! type properties children 2 2)
        (let [[min-range max-range] children
              ;; unclear if we want to enforce (<= min-range max-range)
              ;; it's a perfectly well formed constraint that happens to satisfy no values
              _ (when-not (or (nil? min-range)
                              (number? min-range))
                  (-fail! ::mc/range-constraint-min {:min min-range}))
              _ (when-not (or (nil? max-range)
                              (number? max-range))
                  (-fail! ::mc/range-constraint-max {:max max-range}))
              this (volatile! nil)
              form (delay (mcu/-constraint-form @this options))
              cache (m/-create-cache options)]
          (vreset!
            this
            ^{:type ::m/schema}
            (reify
              mcp/Constraint
              (-constraint? [_] true)
              (-intersect [_ that options']
                (when (= type (m/type that))
                  (let [{gen-min :gen/min gen-max :gen/max} properties
                        [min-range' max-range'] (m/children that)
                        {gen-min' :gen/min gen-max' :gen/max} (m/properties that)
                        gen-min (or (when (and gen-min gen-min') (cc/max gen-min gen-min')) gen-min gen-min')
                        gen-max (or (when (and gen-max gen-max') (cc/min gen-max gen-max')) gen-max gen-max')]
                    (m/-into-schema parent
                                    (cond-> {}
                                      gen-min (assoc :gen/min gen-min)
                                      gen-max (assoc :gen/max gen-max))
                                    [(or (when (and min-range min-range') (cc/max min-range min-range')) min-range min-range')
                                     (or (when (and max-range max-range') (cc/min max-range max-range')) max-range max-range')]
                                    options))))
              m/AST
              (-to-ast [this _] (m/-to-value-ast this))
              m/Schema
              (-validator [_]
                (cond
                  (and min-range max-range) (if (= min-range max-range)
                                              #(= min-range %)
                                              (if (<= min-range max-range)
                                                #(and (<= min-range %)
                                                      (<= % max-range))
                                                (fn [_] false)))
                  min-range #(<= min-range %)
                  max-range #(<= % max-range)
                  :else any?))
              (-explainer [this path]
                (let [pred (m/-validator this)]
                  (fn [x in acc]
                    (cond-> acc
                      (not (pred x))
                      (conj (miu/-error path in this x ::mc/range-limits))))))
              (-parser [this]
                (let [validator (m/-validator this)]
                  (fn [x] (if (validator x) x ::m/invalid))))
              (-unparser [this] (m/-parser this))
              (-transformer [this transformer method options] (-fail! ::mc/constraints-cannot-be-transformed this))
              (-walk [this walker path options] (m/-walk-leaf this walker path options))
              (-properties [_] properties)
              (-options [_] options)
              (-children [_] children)
              (-parent [_] parent)
              (-form [_] @form)
              m/Cached
              (-cache [_] cache)
              m/LensSchema
              (-keep [_] (throw (ex-info "TODO" {})))
              (-get [_ _ default] (throw (ex-info "TODO" {})))
              (-set [this key _] (throw (ex-info "TODO" {}))))))))))
