(ns malli.impl.typedclojure-ann
  (:require [typed.clojure :as t]
            [malli.core #?(:clj :as-alias :cljs :as) m]
            [malli.impl.regex #?(:clj :as-alias :cljs :as) re]))
;;TODO support namespace aliases in ann-protocol first arg

(t/defalias ?Schema t/Any)
(t/defalias Options (t/Nilable (t/Map t/Any t/Any)))
(t/defalias Parser [t/Any :-> t/Any])
(t/defalias Unparser [t/Any :-> t/Any])
(t/defalias MinMax '{:min (t/Nilable t/Int)
                     :max (t/Nilable t/Int)})

(t/defalias Schema
  (t/I m/Schema
       m/IntoSchema))
(t/defalias RegexSchema
  (t/I m/RegexSchema Schema))

(t/defalias Path (t/Vec t/Any))

(t/ann-protocol malli.core/IntoSchema
                -type [m/IntoSchema :-> (t/U t/Sym t/Kw)]
                -type-properties [m/IntoSchema :-> (t/Nilable (t/Map t/Any t/Any))]
                -properties-schema [m/IntoSchema t/Any :-> t/Any]
                -children-schema [m/IntoSchema t/Any :-> (t/Nilable (t/SequentialColl t/Any))]
                -into-schema [m/IntoSchema t/Any t/Any t/Any :-> Schema])

(t/defalias Validator [t/Any :-> t/Bool])

(t/ann-protocol malli.core/Schema
                -validator [Schema :-> Validator]
                -explainer [Schema (t/Vec t/Any) :-> [t/Any t/Any t/Any :-> t/Any]]
                -parser [Schema :-> Parser]
                -unparser [Schema :-> [t/Any :-> t/Any]]
                -transformer [Schema t/Any t/Any t/Any :-> [t/Any :-> t/Any]]
                -walk [Schema t/Any t/Any t/Any :-> t/Any]
                -properties [Schema :-> t/Any]
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
                -entries [m/EntrySchema :-> (t/SeqentialColl t/Any)]
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
                -regex-min-max [RegexSchema :-> MinMax])

;; TODO add asymmetric instance?/implements? filters to checker
(t/ann ^:no-check m/-ref-schema? [t/Any :-> t/Bool :filters {:then (is m/RefSchema 0)}])
(t/ann ^:no-check m/-entry-parser? [t/Any :-> t/Bool :filters {:then (is m/EntryParser 0)}])
(t/ann ^:no-check m/-entry-schema? [t/Any :-> t/Bool :filters {:then (is m/EntrySchema 0)}])
(t/ann ^:no-check m/-cached? [t/Any :-> t/Bool :filters {:then (is m/Cached 0)}])
(t/ann ^:no-check m/-ast? [t/Any :-> t/Bool :filters {:then (is m/AST 0)}])
(t/ann ^:no-check m/-transformer? [t/Any :-> t/Bool :filters {:then (is m/Transformer 0)}])

(t/ann m/parser (t/IFn [?Schema :-> Parser]
                       [?Schema Options :-> Parser]))
(t/ann m/unparser (t/IFn [?Schema :-> Unparser]
                         [?Schema Options :-> Unparser]))
(t/ann m/-fail! (t/IFn [t/Any :-> t/Nothing]
                       [t/Any t/Any :-> t/Nothing]))
(t/ann m/-safe-pred [[t/Any :-> t/Any] :-> [t/Any :-> t/Bool]])
(t/ann m/-guard [[t/Any :-> t/Any] [t/Any :-> t/Any] :-> (t/Nilable [t/Any :-> t/Any])])
(t/ann m/-deprecated! [t/Str :-> nil])
(t/ann m/-keyword->string [(t/U t/Kw t/Str) :-> t/Str])
(t/ann m/-unlift-keys (t/All [x] [(t/Map t/Ident x) (t/U t/Str t/Ident) :-> (t/Map t/Ident x)]))
(t/ann m/-check-children? [:-> t/Bool])
(t/ann m/-check-children! (t/IFn [t/Any t/Any (t/Seqable t/Any) MinMax :-> (t/Nilable t/Any)]
                                 [t/Any t/Any (t/Seqable t/Any) (t/Nilable t/Int) (t/Nilable t/Int) :-> (t/Nilable t/Any)]))
(t/ann m/-schema-schema [(t/HMap :optional {:id t/Any :raw t/Any}) :-> Schema])
(t/ann m/-pointer [t/Any Schema t/Any :-> Schema])
(t/ann m/-ref-schema [t/Any Schema t/Any :-> Schema])
(t/ann m/-reference? [?Schema :-> t/Bool :filters {:then (is (t/U t/Kw t/Str) 0)
                                                   :else (! t/Str 0)}])

;; malli.impl.regex
(t/ann re/item-validator [Validator :-> t/Any])
(t/ann re/item-explainer [Path t/Any [t/Any t/Any t/Any :-> t/Any] :-> [t/Any t/Any t/Any t/Any t/Any :-> t/Any]])
(t/ann re/item-parser [Parser :-> [t/Any t/Any t/Any t/Any t/Any :-> t/Any]])
(t/ann re/item-unparser [Unparser :-> [t/Any t/Any t/Any t/Any t/Any :-> t/Any]])
(t/ann re/item-transformer [t/Any [t/Any :-> t/Any] [t/Any :-> t/Any] :-> [t/Any t/Any t/Any t/Any t/Any :-> t/Any]])
