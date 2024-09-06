(ns malli.constraint
  (:require [clojure.set :as set]
            [malli.constraint.atomic.validate :as mcv-atomic]
            [malli.constraint.compound.validate :as mcv-comp]
            [malli.constraint.countable.validate :as mcv-cnt]
            [malli.constraint.string :as mc-str]
            [malli.impl.util :as miu :refer [-fail!]]
            [malli.registry :as mr]
            [malli.core :as m]))

;; TODO :qualified-keyword + :namespace
;; TODO add to options
(defn default-schema-constraints []
  (mc-str/schema-constraints))

;; TODO add to options
(defn default-validators []
  (merge (mcv-atomic/validators)
         (mcv-cnt/validators)
         (mcv-comp/validators)))

(defn -resolve-op [constraint constraint-types options]
  (let [op (when (vector? constraint)
             (first constraint))
        op (or (get constraint-types op)
               (-fail! ::disallowed-constraint {:type op :constraint constraint
                                                :allowed (keys constraint-types)}))]
    (loop [op op
           seen #{}]
      (when (seen op)
        (-fail! ::infinite-constraint
                {:constraint constraint :constraint-types constraint-types
                 :seen seen
                 :options options}))
      (let [op' (get constraint-types op op)]
        (cond-> op'
          (not= op op') (recur (conj seen op)))))))

(defn ->constraint-opts [type-or-map]
  (if (map? type-or-map)
    type-or-map
    (get (default-schema-constraints) type-or-map)))

(defn -constraint-validator [constraint constraint-opts options]
  (let [{:keys [validator-constraint-types] :as constraint-opts} (->constraint-opts constraint-opts)
        validators (default-validators)]
    (letfn [(-constraint-validator [constraint]
              (let [op (-resolve-op constraint validator-constraint-types options)]
                (if-some [custom-validator (validators op)]
                  (custom-validator {:constraint (if (= :any op) [:any] constraint)
                                     :constraint-opts constraint-opts
                                     ;;TODO other arities
                                     :constraint-validator (fn ([constraint] (-constraint-validator constraint)))}
                                    options)
                  (-fail! ::unknown-constraint {:constraint constraint}))))]
      (-constraint-validator constraint))))

(defn register-constraint-extension!
  ([extensions] (register-constraint-extension! extensions default-registry))
  ([extensions registry]
   (reduce-kv (fn [_ k v] (-register-constraints (mr/-schema registry k) v)) nil extensions)))

(comment
  (-constraint-from-properties
    {:max 1 :min 0}
    {::m/constraint-options (:string constraint-extensions)})
  )

(defn constraint-from-ast
  []
  (throw (ex-info "TODO" {})))

(declare constraint-extensions)

(def constraint-extensions
  {:string {:parse-constraint {:max (fn [{:keys [properties children]} opts]
                                      (-check-children! :max properties children 1 1)
                                      [::count-constraint 0 (first children)])
                               :min (fn [{:keys [properties children]} opts]
                                      (-check-children! :min properties children 1 1)
                                      [::count-constraint (first children) nil])
                               :gen/max (fn [{:keys [properties children]} opts]
                                          (-check-children! :gen/max properties children 1 1)
                                          [::count-constraint {::gen true} 0 (first children)])
                               :gen/min (fn [{:keys [properties children]} opts]
                                          (-check-children! :gen/min properties children 1 1)
                                          [::count-constraint {::gen true} (first children) nil])
                               :and (fn [{:keys [properties children]} opts]
                                      (into [::and nil] children))}
            :constraint-form {::count-constraint (fn [c opts]
                                                   (let [[min max] (-children c)]
                                                     (cond
                                                       (and min max) [:and [:min min] [:max max]]
                                                       min [:min min]
                                                       :else [:max max])))
                              ::m/and-constraint (fn [c opts]
                                                   ;; TODO flatten :and?
                                                   (into [:and] (-children c)))}
            :parse-properties {:max (fn [v opts]
                                      [::count-constraint 0 v])
                               :min (fn [v opts]
                                      [::count-constraint v nil])
                               :gen/max (fn [v opts]
                                          [::count-constraint {::gen true} 0 v])
                               :gen/min (fn [v opts]
                                          [::count-constraint {::gen true} v nil])
                               :and (fn [vs opts]
                                      (into [::and nil] vs))}
            :unparse-properties {::count-constraint
                                 (fn [c c-properties [cmin cmax :as c-children] into-properties opts]
                                   (cond-> into-properties
                                     cmax (update (if (::gen c-properties) :gen/max :max) #(if % (min % cmax) cmax))
                                     (pos? cmin) (update (if (::gen c-properties) :gen/min :min) #(if % (max % cmin) cmin))))
                                 ::m/and-constraint
                                 (fn [c c-properties [cmin cmax :as c-children] into-properties opts]
                                   (reduce (fn [into-properties]
                                             (throw (ex-info "TODO" {}))
                                             (or (::unparse-properties c into-properties opts)
                                                 ))
                                           into-properties))
                                 }}})

(defn -count-constraint []
  (let [type ::count-constraint]
    ^{:type ::into-schema}
    (reify
      AST
      (-from-ast [parent ast options] (throw (ex-info "TODO" {})))
      IntoSchema
      (-type [_] ::count-constraint)
      (-type-properties [_])
      (-properties-schema [_ _])
      (-children-schema [_ _])
      (-into-schema [parent properties children options]
        (-check-children! type properties children 2 2)
        (let [[min-count max-count] children
              ;; unclear if we want to enforce (<= min-count max-count)
              ;; it's a perfectly well formed constraint that happens to satisfy no values
              _ (when-not (or (nil? min-count)
                              (nat-int? min-count))
                  (-fail! ::count-constraint-min {:min min-count}))
              _ (when-not (or (nil? max-count)
                              (nat-int? max-count))
                  (-fail! ::count-constraint-max {:max max-count}))
              form (delay (-simple-form parent properties children identity options))
              cache (-create-cache options)]
          ^{:type ::schema}
          (reify
            Constraint
            (-constraint? [_] true)
            AST
            (-to-ast [this _] (throw (ex-info "TODO" {})))
            Schema
            (-validator [_]
              ;;TODO bounded counts
              (cond
                (::gen properties) any?
                (and min-count max-count) (if (= min-count max-count)
                                            #(= min-count (miu/-safe-count %))
                                            #(<= min-count (miu/-safe-count %) max-count))
                min-count #(<= min-count (miu/-safe-count %))
                max-count #(<= (miu/-safe-count %) max-count)
                :else any?))
            (-explainer [this path] (-fail! ::constraints-cannot-have-explainers this))
            (-parser [this] (-fail! ::constraints-cannot-be-parsed this))
            (-unparser [this] (-fail! ::constraints-cannot-be-unparsed this))
            (-transformer [this transformer method options] (-fail! ::constraints-cannot-be-transformed this))
            (-walk [this walker path options]
              (throw (ex-info "TODO" {}))
              #_
              (if-some [co @constraint-opts]
                (-walk-leaf+constraints this walker path co options)
                (-walk-leaf this walker path options)))
            (-properties [_] properties)
            (-options [_] options)
            (-children [_] children)
            (-parent [_] parent)
            (-form [_] @form)
            Cached
            (-cache [_] cache)
            LensSchema
            (-keep [_] (throw (ex-info "TODO" {})))
            (-get [_ _ default] (throw (ex-info "TODO" {})))
            (-set [this key _] (throw (ex-info "TODO" {})))))))))


(defn base-constraints []
  {::count-constraint (-count-constraint)
   ::m/and-constraint (m/-and-constraint)})

(defn activate-base-constraints!
  ([] (mr/swap-default-registry! activate-base-constraints!))
  ([?registry] (->> ?registry (mr/composite-registry (base-constraints)) (register-constraint-extension! constraint-extensions))))
