(ns malli.constraint.true
  (:require [clojure.core :as cc]
            [malli.constraint.protocols :as mcp]
            [malli.core :as m]
            [malli.constraint.util :as mcu]
            [malli.constraint :as-alias mc]
            [malli.constraint.ext.string :as string-ext]
            [malli.impl.util :as miu :refer [-fail!]]
            [malli.registry :as mr]))

(defn -true-constraint []
  (let [type ::mc/true-constraint]
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
        (m/-check-children! type properties children 0 0)
        (let [this (volatile! nil)
              form (delay (mcu/-constraint-form @this options))
              cache (m/-create-cache options)]
          (vreset!
            this
            ^{:type ::m/schema}
            (reify
              mcp/Constraint
              (-constraint? [_] true)
              (-intersect [this that options] (when (= type that) this))
              m/AST
              (-to-ast [this _] (throw (ex-info "TODO" {})))
              m/Schema
              (-validator [_] any?)
              ;;TODO make explainer and hook it up to humanizer
              (-explainer [this path] (fn [x in acc] acc))
              (-parser [this] identity)
              (-unparser [this] identity)
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
              (-set [this key _] (m/-fail! ::mc/non-associative-constraint {:schema this, :key key})))))))))
