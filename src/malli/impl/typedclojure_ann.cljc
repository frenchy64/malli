(ns malli.impl.typedclojure-ann
  (:require [typed.clojure :as t]
            [malli.core :as m]))
;;TODO support namespace aliases in ann-protocol first arg

(t/ann-protocol malli.core/IntoSchema
                -type [m/IntoSchema :-> (t/U t/Sym t/Kw)]
                -type-properties [m/IntoSchema :-> (t/Nilable (t/Map t/Any t/Any))]
                -properties-schema [m/IntoSchema t/Any :-> t/Any]
                -children-schema [m/IntoSchema t/Any :-> (t/Nilable (t/SequentialColl t/Any))]
                -into-schema [m/IntoSchema t/Any t/Any t/Any :-> m/Schema])

(t/ann-protocol malli.core/Schema
                -validator [m/Schema :-> [t/Any :-> t/Bool]]
                -explainer [m/Schema (t/Vec t/Any) :-> [t/Any t/Any t/Any :-> t/Any]]
                -parser [m/Schema :-> [t/Any :-> t/Any]]
                -unparser [m/Schema :-> [t/Any :-> t/Any]]
                -transformer [m/Schema t/Any t/Any t/Any :-> [t/Any :-> t/Any]]
                -walk [m/Schema t/Any t/Any t/Any :-> t/Any]
                -properties [m/Schema :-> t/Any]
                -options [m/Schema :-> t/Any]
                -children [m/Schema :-> (t/Nilable (t/SequentialColl t/Any))]
                -parent [m/Schema :-> m/IntoSchema]
                -form [m/Schema :-> t/Any])

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
                -get (t/All [x] [m/LensSchema t/Any x :-> (t/U m/Schema x)])
                -set [m/LensSchema t/Any m/Schema :-> m/LensSchema])

(t/ann-protocol malli.core/RefSchema
                -ref [m/RefSchema :-> (t/Nilable (t/U t/Sym t/Kw))]
                -deref [m/RefSchema :-> m/Schema])

(t/ann-protocol malli.core/Walker
                -accept [m/Walker m/Schema (t/Vec t/Any) (t/Map t/Any t/Any) :-> t/Any]
                -inner [m/Walker m/Schema (t/Vec t/Any) (t/Map t/Any t/Any) :-> t/Any]
                -outer [m/Walker m/Schema (t/Vec t/Any) (t/Seqable m/Schema) (t/Map t/Any t/Any) :-> t/Any])

(t/ann-protocol malli.core/Transformer
                -transformer-chain [m/Transformer :-> (t/Vec '{:name t/Any :encoders t/Any :decoders t/Any :options t/Any})]
                -value-transformer [m/Transformer m/Schema t/Any (t/Map t/Any t/Any) :-> t/Any])

(t/ann-protocol malli.core/RegexSchema
                -regex-op? [m/RegexSchema :-> t/Bool]
                -regex-validator [m/RegexSchema :-> t/Any]
                -regex-explainer [m/RegexSchema (t/Vec t/Any) :-> t/Any]
                -regex-unparser [m/RegexSchema :-> t/Any]
                -regex-parser [m/RegexSchema :-> t/Any]
                -regex-transformer [m/RegexSchema t/Any t/Any t/Any :-> t/Any]
                -regex-min-max [m/RegexSchema :-> '[(t/Nilable t/Int) (t/Nilable t/Int)]])

(t/ann ^:no-check m/-ref-schema? [t/Any :-> t/Bool :filters {:then (is m/RefSchema 0)}])
(t/ann ^:no-check m/-entry-parser? [t/Any :-> t/Bool :filters {:then (is m/EntryParser 0)}])
(t/ann ^:no-check m/-entry-schema? [t/Any :-> t/Bool :filters {:then (is m/EntrySchema 0)}])
(t/ann ^:no-check m/-cached? [t/Any :-> t/Bool :filters {:then (is m/Cached 0)}])
(t/ann ^:no-check m/-ast? [t/Any :-> t/Bool :filters {:then (is m/AST 0)}])
(t/ann ^:no-check m/-transformer? [t/Any :-> t/Bool :filters {:then (is m/Transformer 0)}])
