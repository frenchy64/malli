(ns malli.constraint
  (:require [clojure.set :as set]
            [malli.constraint.atomic.validate :as mcv-atomic]
            [malli.constraint.compound.validate :as mcv-comp]
            [malli.constraint.countable.validate :as mcv-cnt]
            [malli.constraint.string :as mc-str]
            [malli.impl.util :as miu :refer [-fail!]]
            [malli.core :as-alias m]))

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

(defn -constraint-from-properties [properties options]
  (let [{:keys [parse-properties]} (::m/constraint-options options)
        ks (-> parse-properties keys sort)]
    (when-some [cs (-> []
                       (into (keep #(when-some [[_ v] (find properties %)]
                                      (parse-properties v options)))
                             ks)
                       not-empty)]
      (if (= 1 (count cs))
        (first cs)
        (into [::m/and-constraint] cs)))))

(defn constraint-from-ast
  []
  (throw (ex-info "TODO" {})))

