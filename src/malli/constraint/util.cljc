(ns malli.constraint.util
  (:require [clojure.core :as cc]
            [clojure.set :as set]
            [malli.constraint.protocols :as mcp]
            [malli.core :as m]
            [malli.constraint :as-alias mc]
            [malli.impl.util :as miu :refer [-fail!]]
            [malli.registry :as mr])
  #?(:clj (:import (clojure.lang IPersistentVector))))

(defn constraint
  ([?constraint] (constraint ?constraint nil))
  ([?constraint options]
   (cond
     (mcp/-constraint? ?constraint) ?constraint
     ;; reserving for now for "contains" constraints for :map. will be an extension per-schema.
     (keyword? ?constraint) (-fail! ::mc/constraints-must-be-vectors {:outer-schema (-> options ::m/constraint-context :type)
                                                                   :constraint ?constraint})
     (vector? ?constraint) (let [v #?(:clj ^IPersistentVector ?constraint, :cljs ?constraint)
                                 n #?(:bb (count v) :clj (.count v), :cljs (count v))
                                 op #?(:clj (.nth v 0), :cljs (nth v 0))
                                 ?p (when (> n 1) #?(:clj (.nth v 1), :cljs (nth v 1)))
                                 prs (or (-> options ::m/constraint-context :parse-constraint)
                                         (-fail! ::mc/missing-parse-constraint-options {:constraint ?constraint}))
                                 f (or (prs op)
                                       (-fail! ::mc/missing-constraint-parser {:op op
                                                                            :constraint ?constraint}))]
                             (m/schema (if (or (nil? ?p) (map? ?p))
                                         (f {:properties ?p :children (when (< 2 n) (subvec ?constraint 2 n))} options)
                                         (f {:children (when (< 1 n) (subvec ?constraint 1 n))} options))
                                       options))
     :else (-fail! ::mc/invalid-constraint {:outer-schema (-> options ::m/constraint-context :type)
                                         :constraint ?constraint}))))

(defn -constraint-from-properties [properties options]
  (let [{:keys [parse-properties]} (::m/constraint-context options)
        ;; important for deterministic m/explain ordering
        ks (-> parse-properties keys sort)
        cs (-> []
               (into (keep #(when-some [[_ v] (find properties %)]
                              (constraint ((get parse-properties %) v options) options)))
                     ks)
               not-empty)]
    (case (count cs)
      0 (constraint [:true] options)
      1 (first cs)
      (constraint (into [:and] cs) options))))

(defn -walk-leaf+constraints [schema walker path constraint {::m/keys [constraint-opts] :as options}]
  (when (m/-accept walker schema path options)
    (let [constraint' (when constraint
                        (let [constraint-walker (or (::mc/constraint-walker options)
                                                    (reify m/Walker
                                                      (-accept [_ constraint _ _] constraint)
                                                      (-inner [this constraint path options] (m/-walk constraint this path options))
                                                      (-outer [_ constraint _ children _] (m/-set-children constraint children))))]
                          (m/-walk constraint constraint-walker (conj path ::mc/constraint)
                                   (assoc options
                                          ::mc/constraint-walker constraint-walker
                                          ;; enables constraints that contain schemas, e.g., [:string {:edn :int}]
                                          ::mc/schema-walker walker))))
          schema (cond-> schema
                   ;; don't try and guess the 'unparsed' properties we don't need to.
                   ;; 
                   (and (some? constraint')
                        (not (identical? constraint constraint')))
                   (m/-update-properties (fn [properties]
                                           (let [{:keys [unparse-properties]} constraint-opts
                                                 f (or (get unparse-properties (m/type constraint'))
                                                       (-fail! ::mc/cannot-unparse-constraint-into-properties
                                                               {:constraint constraint'}))]
                                             (f constraint' properties options)))))]
      (m/-outer walker schema path (m/-children schema) options))))

(defn default-parse-constraints []
  {:and (fn [{:keys [properties children]} opts]
          (into [::mc/and nil] children))
   :true (fn [{:keys [properties children]} opts]
           (m/-check-children! :true properties children 0 0)
           [::mc/true-constraint])})

(defn default-parse-properties []
  {:and (fn [v _] (into [:and] v))})

(defn default-unparse-properties []
  {::mc/and
   (fn [c into-properties {{:keys [unparse-properties]} ::m/constraint-context :as opts}]
     (reduce (fn [into-properties c]
               (unparse-properties c into-properties opts))
             into-properties (m/children c)))
   ::mc/true (fn [_ into-properties _] into-properties)})

(defn default-constraint-form []
  {::mc/and (fn [c options] (into [:and] (map m/form) (m/children c)))
   ::mc/true-constraint (fn [c options] [:true])})

(defn -constraint-form [constraint {{:keys [constraint-form]} ::m/constraint-context :as options}]
  (let [t (m/type constraint)
        f (or (get constraint-form t)
              (-fail! ::mc/no-constraint-form {:type t}))]
    (f constraint options)))

(defn register-constraint-extensions! [extensions]
  (swap! m/constraint-extensions #(merge-with into % extensions)))
