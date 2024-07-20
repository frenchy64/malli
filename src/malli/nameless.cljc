(ns malli.nameless
  (:refer-clojure :exclude [eval type -deref deref -lookup -key assert])
  #?(:cljs (:require-macros malli.core))
  (:require [clojure.walk :as walk]
            [clojure.core :as c]
            [malli.core :as m]
            [malli.impl.regex :as re]
            [malli.impl.util :as miu]
            [malli.registry :as mr]
            [malli.sci :as ms]
            [malli.poly-protocols :refer [AllSchema -bounds -inst]]))

(declare inst)

;; -abstract / -instantiate for locally nameless representation
;; See "I am not a number: I am a free variable" - Conor McBride and James McKinna

(defn -abstract [?schema nme options]
  (let [inner (fn [this s path options]
                (let [properties (m/properties s)
                      options (cond-> options
                                (::scope properties) (update ::abstract-index inc))
                      s (cond-> s
                          (:registry properties)
                          (-> (m/ast options)
                              (update :registry #(not-empty
                                                   (into {} (map (fn [[k ast]]
                                                                   [k (-> ast
                                                                          (m/from-ast options)
                                                                          (m/-walk this (conj path :registry k) options)
                                                                          (m/ast options))]))
                                                         %)))
                              (m/from-ast options)))]
                  (m/-walk s this path options)))
        outer (fn [s path children {::keys [abstract-index] :as options}]
                (let [s (m/-set-children s children)]
                  (case (m/type s)
                    ::f (let [[id] children]
                          (if (= nme id)
                            (m/schema [::b abstract-index] options)
                            s))
                    ::m/val (first children)
                    s)))]
    (m/schema
      [:schema {::scope true}
       (inner
         (reify m/Walker
           (-accept [_ s path options] true)
           (-inner [this s path options] (inner this s path options))
           (-outer [_ schema path children options]
             (outer schema path children options)))
         (m/schema ?schema options)
         []
         (assoc options
                ::m/walk-refs false
                ::m/walk-schema-refs false
                ::m/walk-entry-vals true
                ::abstract-index 0))]
      options)))

(defn -abstract-many [s names options]
  (reduce (fn [s nme]
            (-abstract s nme options))
          s names))

(defn -instantiate [?scope to options]
  (let [to (m/schema to options)
        inner (fn [this s path options]
                (let [properties (m/properties s)
                      options (cond-> options
                                (::scope (m/-properties s)) (update ::instantiate-index inc))
                      s (cond-> s
                          (:registry properties)
                          (-> (m/ast options)
                              (update :registry #(not-empty
                                                   (into {} (map (fn [[k ast]]
                                                                   [k (-> ast
                                                                          (m/from-ast options)
                                                                          (m/-walk this (conj path :registry k) options)
                                                                          (m/ast options))]))
                                                         %)))
                              (m/from-ast options)))]
                  (m/-walk s this path options)))
        outer (fn [s path children {::keys [instantiate-index] :as options}]
                (let [s (m/-set-children s children)]
                  (case (m/type s)
                    ::b (let [[id] children]
                          (if (= instantiate-index id)
                            to
                            s))
                    ::m/val (first children)
                    s)))
        s (m/schema ?scope options)
        _ (when-not (-> s m/-properties ::scope)
            (m/-fail! ::instantiate-non-scope {:schema s}))]
    (inner
      (reify m/Walker
        (-accept [_ s path options] true)
        (-inner [this s path options] (inner this s path options))
        (-outer [_ schema path children options]
          (outer schema path children options)))
      (first (m/-children s))
      []
      (assoc options
             ::m/walk-refs false
             ::m/walk-schema-refs false
             ::m/walk-entry-vals true
             ::instantiate-index 0))))

;; TODO single pass
(defn -instantiate-many [s images options]
  (reduce (fn [s image]
            (-instantiate s image options))
          s images))
