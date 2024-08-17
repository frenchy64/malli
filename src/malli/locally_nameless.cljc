(ns malli.locally-nameless
  (:refer-clojure :exclude [type])
  #?(:cljs (:require-macros malli.core))
  (:require [malli.core :as m]))

;; -abstract / -instantiate for locally nameless representation
;; See "I am not a number: I am a free variable" - Conor McBride and James McKinna

(defn -scoped
  ([s] [::scope {::scopes 1} s])
  ([s n]
   ;;TODO increment ::scopes property
   (reduce (fn [acc _] (-scoped acc))
           s (range n))))

;; disallow free variables under registries. because of the way pointers/refs/derefs
;; are implemented, it's quite difficult to update a local registry such that pointers
;; are also updated. this is because -deref doesn't take a dynamic environment.
(defn -fail-if-property-registry! [s properties]
  (when (and (m/-ref-schema? s)
             (m/-ref s))
    (m/-fail! ::local-registries-not-allowed {:schema s}))
  (when (seq (:registry properties))
    (m/-fail! ::local-registries-not-allowed {:schema s})))

(defn -abstract [?schema nme options]
  (let [inner (fn [this s path options]
                (let [properties (m/properties s)
                      _ (-fail-if-property-registry! s properties)
                      options (cond-> options
                                (= ::scope (m/type s)) (update ::abstract-index + (::scopes properties)))]
                  (m/-walk s this path options)))
        outer (fn [s path children {::keys [abstract-index] :as options}]
                (let [s (m/-set-children s children)]
                  (case (m/type s)
                    ::f (let [[id] children]
                          (if (= nme id)
                            (m/schema [::b abstract-index] options)
                            s))
                    s)))]
    (m/schema
      (-scoped
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
                 ::m/walk-entry-vals false
                 ::abstract-index 0))
        1)
      options)))

(defn -abstract-many [s names options]
  (reduce (fn [s nme]
            (-abstract s nme options))
          s names))

(defn -instantiate [?scope to options]
  (let [to (m/schema to options)
        inner (fn [this s path options]
                (let [properties (m/properties s)
                      _ (-fail-if-property-registry! s properties)
                      options (cond-> options
                                (= ::scope (m/type s)) (update ::instantiate-index + (::scopes properties)))]
                  (m/-walk s this path options)))
        outer (fn [s path children {::keys [instantiate-index] :as options}]
                (let [s (m/-set-children s children)]
                  (case (m/type s)
                    ::b (let [[id] children]
                          (if (= instantiate-index id)
                            to
                            s))
                    s)))
        s (m/schema ?scope options)
        _ (when-not (= ::scope (m/type s))
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
             ::m/walk-entry-vals false
             ::instantiate-index 0))))

(defn -fv [?schema options]
  (let [fvs (atom #{})
        inner (fn [this s path options]
                (when (seq (:registry (m/properties s)))
                  (m/-fail! ::local-registries-not-allowed {:schema s}))
                (m/-walk s this path options))
        outer (fn [s path children {::keys [instantiate-index] :as options}]
                (let [s (m/-set-children s children)]
                  (case (m/type s)
                    ::f (let [[id] children]
                          (swap! fvs conj id)
                          s)
                    s)))]
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
             ::m/walk-entry-vals false))
    @fvs))

;; TODO single pass
(defn -instantiate-many [s images options]
  (reduce (fn [s image]
            (-instantiate s image options))
          s images))

(defn- -free-or-bound-schema [{:keys [type]}]
  (assert (#{::f ::b} type))
  ^{:type ::m/into-schema}
  (reify
    m/AST
    (-from-ast [parent ast options] (m/-from-value-ast parent ast options))
    m/IntoSchema
    (-type [_] type)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children options]
      (m/-check-children! type properties children 1 1)
      (when-not (case type
                  ::f (-> children first simple-keyword?)
                  ::b (-> children first nat-int?))
        (m/-fail! ::free-should-have-simple-keyword {:children children}))
      (let [form (delay (case type
                          ::f (-> children first)
                          (m/-simple-form parent properties children identity options)))
            cache (m/-create-cache options)]
        ^{:type ::m/schema}
        (reify
          m/AST
          (-to-ast [this _] (m/-to-value-ast this))
          m/Schema
          (-validator [_] (m/-fail! ::cannot-validate-free))
          (-explainer [this path] (m/-fail! ::cannot-explain-free))
          (-parser [this] (m/-fail! ::cannot-parse-free))
          (-unparser [this] (m/-fail! ::cannot-unparse-free))
          (-transformer [this transformer method options]
            (m/-intercepting (m/-value-transformer transformer this method options)))
          (-walk [this walker path options] (m/-walk-leaf this walker path options))
          (-properties [_] properties)
          (-options [_] options)
          (-children [_] children)
          (-parent [_] parent)
          (-form [_] @form)
          m/Cached
          (-cache [_] cache)
          m/LensSchema
          (-keep [_])
          (-get [_ _ default] default)
          (-set [this key _] (m/-fail! ::non-associative-schema {:schema this, :key key})))))))

(defn -scope-schema []
  (m/-proxy-schema {:type ::scope :min 1 :max 1
                    :fn (fn [properties children options]
                          (when-not (= [::scopes] (keys properties))
                            (m/-fail! ::invalid-scope-properties {:properties properties}))
                          (let [children (mapv #(m/schema % options) children)]
                            [children (mapv m/form children) (delay (m/-fail! ::cannot-use-scope-schema {:schema children}))]))}))

(defn schemas []
  {::f (-free-or-bound-schema {:type ::f})
   ::b (-free-or-bound-schema {:type ::b})
   ::scope (-scope-schema)})
