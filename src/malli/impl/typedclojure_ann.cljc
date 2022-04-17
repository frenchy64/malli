(ns malli.impl.typedclojure-ann
  (:require [typed.clojure :as t]
            [malli.core #?(:clj :as-alias :cljs :as) m]
            [malli.impl.regex #?(:clj :as-alias :cljs :as) re]
            [malli.impl.util #?(:clj :as-alias :cljs :as) miu]
            [malli.registry #?(:clj :as-alias :cljs :as) mr]))
;;TODO support namespace aliases in ann-protocol first arg

(t/defalias ?Schema t/Any)
(t/defalias ?Registry (t/U (t/I Registry (t/Not (t/Map t/Any t/Any))) (t/Map t/Any Schema)))
(t/defalias Options (t/HMap :optional {:registry (t/Nilable ?Registry)}))
(t/defalias Parser [t/Any :-> t/Any])
(t/defalias Unparser [t/Any :-> t/Any])
(t/defalias MinMax (t/HMap :optional {:min (t/Nilable t/Int)
                                      :max (t/Nilable t/Int)}))
(t/defalias Properties (t/Nilable (t/Map t/Any t/Any)))
(t/defalias Children (t/Nilable (t/SequentialColl Schema)))

(t/defalias Schema
  (t/I m/Schema
       m/IntoSchema))
(t/defalias RegexSchema
  (t/I m/RegexSchema Schema))

(t/defalias Path (t/Vec t/Any))
(t/defalias Type (t/U t/Sym t/Kw))

(t/ann-protocol malli.core/IntoSchema
                -type [m/IntoSchema :-> Type]
                -type-properties [m/IntoSchema :-> (t/Nilable (t/Map t/Any t/Any))]
                -properties-schema [m/IntoSchema t/Any :-> t/Any]
                -children-schema [m/IntoSchema t/Any :-> (t/Nilable (t/SequentialColl t/Any))]
                -into-schema [m/IntoSchema Properties Children Options :-> Schema])

(t/defalias Validator [t/Any :-> t/Bool])
(t/defalias Arity (t/U t/Int ':varargs))
(t/defalias FunctionInfo (t/HMap :mandatory {:min t/Int
                                             :arity Arity
                                             :input Schema
                                             :output Schema}
                                 :optional {:max t/Int}))

(t/ann-protocol malli.registry/Registry
                -schema [Registry t/Any :-> (t/Nilable Schema)]
                -schemas [Registry :-> (t/Map t/Any Schema)])
(t/defalias Registry mr/Registry)

(t/ann-protocol malli.core/Schema
                -validator [Schema :-> Validator]
                -explainer [Schema (t/Vec t/Any) :-> [t/Any t/Any t/Any :-> t/Any]]
                -parser [Schema :-> Parser]
                -unparser [Schema :-> [t/Any :-> t/Any]]
                -transformer [Schema t/Any t/Any t/Any :-> [t/Any :-> t/Any]]
                -walk [Schema t/Any t/Any t/Any :-> t/Any]
                -properties [Schema :-> Properties]
                -options [Schema :-> t/Any]
                -children [Schema :-> (t/Nilable (t/SequentialColl t/Any))]
                -parent [Schema :-> m/IntoSchema]
                -form [Schema :-> t/Any])

(t/ann-protocol malli.core/AST
                -to-ast [t/AST (t/Map t/Any t/Any) :-> (t/Map t/Kw t/Any)]
                -from-ast [t/AST (t/Map t/Any t/Any) :-> t/Schema])

(t/ann-protocol malli.core/EntryParser
                -entry-keyset [m/EntryParser :-> (t/Set t/Any)]
                -entry-children [m/EntryParser :-> (t/Nilable (t/SequentialColl t/Any))]
                -entry-entries [m/EntryParser :-> (t/Nilable (t/SequentialColl t/Any))]
                -entry-forms [m/EntryParser :-> (t/Nilable (t/SequentialColl t/Any))])

(t/ann-protocol malli.core/EntrySchema
                -entries [m/EntrySchema :-> (t/SequentialColl t/Any)]
                -entry-parser [m/EntrySchema :-> m/EntryParser])

(t/ann-protocol malli.core/Cached
                -cache [m/Cached :-> (t/Atom1 (t/Map t/Any t/Any))])

(t/ann-protocol malli.core/LensSchema
                -keep [m/LensSchema :-> t/Any]
                -get (t/All [x] [m/LensSchema t/Any x :-> (t/U Schema x)])
                -set [m/LensSchema t/Any Schema :-> m/LensSchema])

(t/ann-protocol malli.core/RefSchema
                -ref [m/RefSchema :-> (t/Nilable (t/U t/Sym t/Kw))]
                -deref [RegexSchema :-> RegexSchema])

(t/ann-protocol malli.core/Walker
                -accept [m/Walker Schema (t/Vec t/Any) (t/Map t/Any t/Any) :-> t/Any]
                -inner [m/Walker Schema (t/Vec t/Any) (t/Map t/Any t/Any) :-> t/Any]
                -outer [m/Walker Schema (t/Vec t/Any) (t/Seqable Schema) (t/Map t/Any t/Any) :-> t/Any])

(t/ann-protocol malli.core/Transformer
                -transformer-chain [m/Transformer :-> (t/Vec '{:name t/Any :encoders t/Any :decoders t/Any :options t/Any})]
                -value-transformer [m/Transformer Schema t/Any (t/Map t/Any t/Any) :-> t/Any])

(t/ann-protocol malli.core/RegexSchema
                -regex-op? [RegexSchema :-> t/Bool]
                -regex-validator [RegexSchema :-> t/Any]
                -regex-explainer [RegexSchema (t/Vec t/Any) :-> t/Any]
                -regex-unparser [RegexSchema :-> t/Any]
                -regex-parser [RegexSchema :-> t/Any]
                -regex-transformer [RegexSchema t/Any t/Any t/Any :-> t/Any]
                -regex-min-max [RegexSchema :-> (t/HMap :mandatory {:min t/Int}
                                                        :optional {:max (t/Nilable t/Int)})])

;; TODO add asymmetric instance?/implements? filters to checker
(t/ann ^:no-check m/-ref-schema? [t/Any :-> t/Bool :filters {:then (is m/RefSchema 0)}])
(t/ann ^:no-check m/-entry-parser? [t/Any :-> t/Bool :filters {:then (is m/EntryParser 0)}])
(t/ann ^:no-check m/-entry-schema? [t/Any :-> t/Bool :filters {:then (is m/EntrySchema 0)}])
(t/ann ^:no-check m/-cached? [t/Any :-> t/Bool :filters {:then (is m/Cached 0)}])
(t/ann ^:no-check m/-ast? [t/Any :-> t/Bool :filters {:then (is m/AST 0)}])
(t/ann ^:no-check m/-transformer? [t/Any :-> t/Bool :filters {:then (is m/Transformer 0)}])

(t/ann m/parser [?Schema Options :? :-> Parser])
(t/ann m/unparser [?Schema Options :? :-> Unparser])
(t/ann m/-fail! [t/Any t/Any :? :-> t/Nothing])
(t/ann m/-safe-pred [[t/Any :-> t/Any] :-> [t/Any :-> t/Bool]])
(t/ann m/-guard [[t/Any :-> t/Any] [t/Any :-> t/Any] :-> (t/Nilable [t/Any :-> t/Any])])
(t/ann m/-deprecated! [t/Str :-> nil])
(t/ann m/-keyword->string [(t/U t/Kw t/Str) :-> t/Str])
(t/ann m/-unlift-keys (t/All [x] [(t/Map t/Ident x) (t/U t/Str t/Ident) :-> (t/Map t/Ident x)]))
(t/ann m/-check-children? [:-> t/Bool])
(t/ann m/-check-children! (t/IFn [t/Any t/Any (t/Seqable t/Any) MinMax :-> (t/Nilable t/Any)]
                                 [t/Any t/Any (t/Seqable t/Any) (t/Nilable t/Int) (t/Nilable t/Int) :-> (t/Nilable t/Any)]))
(t/ann m/-schema-schema [(t/HMap :optional {:id t/Any :raw t/Any}) :-> Schema])
(t/ann m/-pointer [t/Any Schema Options :-> Schema])
(t/ann m/-ref-schema [(t/Nilable (t/HMap :optional {:lazy t/Bool
                                                    :type-properties (t/Nilable (t/Map t/Any t/Any))}))
                      :? :-> Schema])
(t/ann m/-reference? [?Schema :-> t/Bool :filters {:then (is (t/U t/Kw t/Str) 0)
                                                   :else (! t/Str 0)}])
(t/ann m/-lazy [Schema Options :-> Schema])
(t/ann m/-boolean-fn [(t/U t/Bool [t/Any :-> t/Bool] (t/Not clojure.lang.IFn)) :-> [t/Any :-> t/Bool]])
(t/ann ^:no-check
       m/-comp (t/IFn [:-> [t/Any :* :-> t/Any]]
                      [[t/Any :* :-> t/Any] :-> [t/Any :* :-> t/Any]]
                      [[t/Any :-> t/Any] [t/Any :* :-> t/Any] :-> [t/Any :* :-> t/Any]]
                      [[t/Any :-> t/Any] [t/Any :-> t/Any] [t/Any :* :-> t/Any] :-> [t/Any :* :-> t/Any]]))
;;TODO
(t/ann ^:no-check m/-update [(t/Nilable (t/Map t/Any t/Any)) t/Any t/Any :-> (t/Map t/Any t/Any)])
;;TODO occurrence typing hook. make = an path element, probably need to make literal values paths elements too.
(t/ann m/-equals [t/Any t/Any :-> t/Any])
(t/ann m/-vmap (t/All [x y]
                      (t/IFn [(t/Seqable x) :-> (t/Vec x)]
                             [[x :-> y] (t/Seqable y) :-> (t/Vec y)])))
;;TODO AtomicReference
(t/ann ^:no-check m/-memoize (t/All [x]
                                    [[x :-> x] :-> [x :-> x]]))
(t/ann m/type [?Schema :-> Type])
;;TODO (when (= (type schema) :=>) ...)
(t/ann ^:no-check
       m/-function-info [Schema :-> (t/Nilable FunctionInfo)])
(t/ann ^:no-check m/-group-by-arity! [(t/Seqable FunctionInfo) :-> (t/Map Arity FunctionInfo)])
(t/ann m/-re-min-max [[(t/Nilable t/Int) (t/Nilable t/Int) :-> t/Int] MinMax RegexSchema :-> MinMax])
(t/ann m/-re-alt-min-max [MinMax RegexSchema :-> (t/HMap :mandatory {:min t/Int}
                                                         :optional {:max t/Int})])
(t/ann m/-register-var [(t/Map t/Any Schema) (t/U (t/Var1 t/Any) '[(t/Var1 t/Any) t/Any]) :-> (t/Map t/Any Schema)])
(t/ann m/-simple-schema [(t/Rec [x] (t/U nil (t/Map t/Any t/Any) (t/I clojure.lang.Fn [Properties Children :-> x])))
                         :-> Schema])
(t/ann m/default-registry Registry)
(t/ann m/-registry [Options :? :-> Registry])

;; malli.impl.regex
(t/defalias Explainer t/Any)
(t/defalias SchemaExplainer [t/Any In (t/Coll t/Any) :-> (t/Coll Error)])
(t/defalias Pos t/Int)
(t/defalias Regs t/Any)
(t/defalias ValidatorK [t/Int (t/Coll t/Any) :-> t/Any])
(t/defalias ValidatorTramp [re/IValidationDriver Regs Pos (t/Coll t/Any) ValidatorK :-> t/Any])
(t/defalias ExplainerK ValidatorK)
(t/defalias ExplainerTramp [re/IExplanationDriver t/Any t/Int (t/Coll t/Any) ExplainerK :-> t/Any])
(t/defalias ParserK [t/Any Pos (t/Coll t/Any) :-> t/Any])
(t/defalias ParserTramp [re/IParseDriver Regs Pos (t/Coll t/Any) ParserK :-> t/Any])
(t/defalias EncoderK [(t/Coll t/Any) Pos (t/Coll t/Any) :-> t/Any])
(t/defalias EncoderTramp [re/IParseDriver Regs (t/Coll t/Any) Pos (t/Coll t/Any) EncoderK :-> t/Any])
(t/defalias TransformerK EncoderK)
(t/defalias TransformerTramp EncoderTramp)
(t/defalias In (t/Vec Pos))
(t/defalias Transformer t/Any)
(t/defalias Encoder [t/Any :-> t/Any])
(t/defalias Decoder Encoder)
(t/defalias KR (t/TFn [[tramp :variance :covariant]]
                      '[t/Any tramp]))
(t/defalias ?KR (t/TFn [[tramp :variance :covariant]]
                       (t/U (KR tramp) (t/I tramp (t/Not (t/Vec t/Any))))))
(t/ann-protocol malli.impl.regex/Driver
                succeed! [re/Driver :-> t/Any]
                succeeded? [re/Driver :-> t/Bool]
                pop-thunk! [re/Driver :-> (t/Nilable [:-> t/Any])])
(t/ann-protocol malli.impl.regex/IValidationDriver
                noncaching-park-validator! [re/IValidationDriver Validator Regs Pos (t/Seqable t/Any) ValidatorK :-> t/Any]
                park-validator! [re/IValidationDriver ValidatorTramp Regs Pos (t/Coll t/Any) ValidatorK :-> t/Any])
(t/ann-protocol malli.impl.regex/IExplanationDriver
                noncaching-park-explainer! [re/IExplanationDriver Explainer Regs Pos (t/Seqable t/Any) ExplainerK :-> t/Any]
                park-explainer! [re/IExplanationDriver Explainer Regs Pos (t/Seqable t/Any) ExplainerK :-> t/Any]
                value-path [re/IExplanationDriver Pos :-> In]
                fail! [re/IExplanationDriver Pos (t/Seqable Error) :-> t/Any])
(t/ann-protocol malli.impl.regex/IParseDriver
                noncaching-park-transformer! [re/IParseDriver Transformer Regs (t/Seqable t/Any) Pos (t/Seqable t/Any) ParserK :-> t/Any]
                park-transformer! [re/IParseDriver Transformer Regs Pos (t/Seqable t/Any) ParserK :-> t/Any]
                succeed-with! [re/IParseDriver t/Any :-> t/Any]
                success-result [re/IParseDriver :-> t/Any]) ;;returns coll sometimes? polymorphic?
(t/ann re/item-validator [Validator :-> ValidatorTramp])
(t/ann re/item-explainer [Path Schema SchemaExplainer :-> ExplainerTramp])
(t/ann re/item-parser [Parser :-> ParserTramp])
(t/ann ^:no-check re/item-unparser [Unparser :-> [t/Any :-> t/Any]])
(t/ann re/item-transformer [(t/U ':encode ':decode) Validator (t/U Encoder Decoder) :-> EncoderTramp])
(t/ann re/item-encoder [Validator Encoder :-> EncoderTramp])
(t/ann re/item-decoder [Decoder Validator :-> EncoderTramp])
(t/ann ^:no-check re/item-unparser [Unparser :-> [t/Any :-> t/Any]])
(t/ann re/end-validator [:-> ValidatorTramp])
(t/ann re/end-explainer [Schema Path :-> ExplainerTramp])
(t/ann re/end-parser [:-> ParserTramp])
(t/ann re/end-transformer [:-> TransformerTramp])
(t/ann re/pure-parser [t/Any :-> ParserTramp])
(t/ann re/pure-unparser [t/Any :-> '[]])
(t/ann re/fmap-parser [[t/Any :-> t/Any] ParserTramp :-> ParserTramp])
;; FIXME (I x (Not (Vec Any))) intersects to x because:
;; - x <: (Not (Vec Any)) since
;; - Any <: (Not (Vec Any))
;; the call to nth in the body of re/entry->regex fails because the target
;; is inferred as (I x (Vec Any)), since x is restricted by the vector? predicate.
(t/ann ^:no-check re/entry->regex (t/All [x] [(?KR x) :-> x]))
(t/ann re/cat-validator [(?KR ValidatorTramp) :* :-> ValidatorTramp])
(t/ann re/cat-explainer [(?KR ExplainerTramp) :* :-> ExplainerTramp])
(t/ann re/cat-parser [ParserTramp :* :-> ParserTramp])
(t/ann re/catn-parser [(KR ParserTramp) :* :-> ParserTramp])
(t/ann re/cat-unparser [Unparser :* :-> Unparser])
(t/ann re/catn-unparser ['[t/Any Unparser] :* :-> Unparser])
(t/ann re/cat-transformer [(?KR TransformerTramp) :* :-> TransformerTramp])
(t/ann re/alt-validator [(?KR ValidatorTramp) :+ :-> ValidatorTramp])
(t/ann re/alt-explainer [(?KR ExplainerTramp) :+ :-> ExplainerTramp])
(t/ann re/alt-parser [ValidatorTramp :+ :-> ValidatorTramp])

;; malli.impl.util
(t/defalias Error (t/HMap :mandatory {:path Path :in In :schema Schema :value t/Any}
                          :optional {:type t/Any}))
(t/defalias Invalid ':malli.core/invalid)
(t/defalias Tagged (t/TFn [[k :variance :covariant]
                           [v :variance :covariant]]
                          (t/AMapEntry k v)))
(t/ann miu/-vmap (t/All [x y]
                        (t/IFn [(t/Seqable x) :-> (t/Vec x)]
                               [[x :-> y] (t/Seqable y) :-> (t/Vec y)])))
(t/ann miu/+max-size+ t/Int)
(t/ann miu/-error [Path In Schema t/Any t/Any :? :-> Error])
(t/ann miu/-invalid? (t/Pred Invalid))
(t/ann miu/-map-valid (t/All [x y] [[x :-> y] (t/U x Invalid) :-> (t/U y Invalid)]))
(t/ann miu/-reduce-kv-valid (t/All [a k v]
                                   [[a k v :-> (t/U (t/Reduced (t/U a Invalid)) a Invalid)] a (t/Option (t/Associative k v)) :-> (t/U a Invalid)]))
(t/ann miu/-tagged (t/All [k v] [k v :-> (Tagged k v)]))
(t/ann miu/-tagged? (t/Pred (Tagged t/Any t/Any)))

;; malli.registry
(t/ann mr/mode t/Str)
(t/ann mr/type t/Str)
(t/ann ^:no-check mr/registry? [t/Any :-> t/Bool :filters {:then (is mr/Registry 0)}])
(t/ann ^:no-check mr/fast-registry [java.util.Map :-> Registry])
(t/ann mr/simple-registry [(t/Map t/Any Schema) :-> Registry])
(t/ann mr/registry (t/IFn [?Registry :-> Registry]
                          [(t/Nilable ?Registry) :-> (t/Nilable Registry)]))
(t/ann mr/registry* (t/Atom1 Registry))
(t/ann mr/set-default-registry! [(t/U (t/I Registry (t/Not (t/Map t/Any t/Any))) (t/Map t/Any Schema))
                                 :-> Registry])
(t/ann mr/custom-default-registry [:-> Registry])
(t/ann mr/mutable-registry [(t/Deref (t/U (t/I Registry (t/Not (t/Map t/Any t/Any))) (t/Map t/Any Schema))) :-> Registry])
(t/ann ^:no-check mr/composite-registry [Registry :* :-> Registry])
(t/ann mr/*registry* (t/Map t/Any Schema))
(t/ann mr/lazy-registry [Registry [t/Any Registry :-> Schema] :-> Registry])
(t/ann mr/dynamic-registry [:-> Registry])
(t/ann mr/schema [Registry t/Any :-> (t/Nilable Schema)])
(t/ann mr/schemas [Registry :-> (t/Map t/Any Schema)])
