(ns malli.rec
  (:refer-clojure :exclude [eval type -deref deref -lookup -key assert])
  #?(:cljs (:require-macros malli.core))
  (:require [clojure.walk :as walk]
            [clojure.core :as c]
            [malli.core :as m]
            [malli.impl.regex :as re]
            [malli.impl.util :as miu]
            [malli.registry :as mr]
            [malli.locally-nameless :as mln :refer [-instantiate-many]]
            [malli.poly-protocols :refer [AllSchema -bounds -inst]]))

(defn- -rec-binder-name [[kw :as binder]]
  (when-not (and (= 1 (count binder))
                 (simple-keyword? kw))
    (m/-fail! ::bad-rec-binder {:binder binder}))
  kw)

(defn -rec-schema [{:keys [type]}]
  {:pre [type]}
  ^{:type ::m/into-schema}
  (reify m/IntoSchema
    (-type [_] type)
    (-type-properties [_])
    (-properties-schema [_ _])
    (-children-schema [_ _])
    (-into-schema [parent properties children {::m/keys [function-checker] :as options}]
      (m/-check-children! type properties children 2 2)
      (let [[binder body-syntax] children
            bname (-rec-binder-name binder)
            body' (let [parsed-body (m/schema body-syntax
                                              (update options :registry
                                                      #(mr/composite-registry
                                                         {bname (m/schema [::mln/f bname] options)}
                                                         (or % {}))))]
                    (mln/-abstract parsed-body bname options))
            form (delay
                   (m/-create-form type properties
                                   (assoc children 1
                                          (-> body'
                                              (mln/-instantiate 
                                                (m/schema [::mln/f bname] options)
                                                options)
                                              m/form))
                                   options))
            cache (m/-create-cache options)
            ->checker (if function-checker #(function-checker % options) (constantly nil))
            this (volatile! nil)
            unfold (delay (mln/-instantiate body' @this options))]
        (->> ^{:type ::m/schema}
             (reify
               m/Schema
               (-validator [this] (m/-validator @unfold))
               (-explainer [this path] (m/-explainer @unfold path))
               (-parser [this] (m/-parser @unfold))
               (-unparser [this] (m/-unparser @unfold))
               (-transformer [_ transformer method options] (m/-transformer @unfold transformer method options))
               ;;TODO
               (-walk [this walker path options]
                 (when (m/-accept walker this path options)
                   (m/-outer walker this path
                             (let [b (m/-inner walker body' (conj path 1) options)
                                   fvs (mln/-fv b options)
                                   bname (if (fvs bname)
                                           (-> bname name gensym keyword)
                                           bname)]
                               [binder (mln/-instantiate b [::mln/f bname] options)])
                             options)))
               (-properties [_] properties)
               (-options [_] options)
               (-children [_] children)
               (-parent [_] parent)
               (-form [_] @form)
               m/FunctionSchema
               (-function-schema? [this] (m/-function-schema? @unfold))
               (-function-schema-arities [this] (m/-function-schema-arities @unfold))
               (-function-info [this] (m/-function-info @unfold))
               (-instrument-f [schema props f options] (m/-instrument-f @unfold props f options))
               m/Cached
               (-cache [_] cache)
               m/LensSchema
               (-keep [_])
               (-get [_ key default] (get children key default))
               (-set [this key value] (m/-set-assoc-children this key value))
               m/RefSchema
               (-ref [_])
               (-deref [_] @unfold))
             (vreset! this))))))

(defn schemas []
  (into (mln/schemas) {:rec (-rec-schema {:type :rec})}))

(comment
  (m/ast [:schema {:registry {::a :int}} ::a])
  (m/ast [:enum :foo :bar])
  )
