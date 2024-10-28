(ns malli.impl.util
  (:require [clojure.core :as c])
  #?(:clj (:import #?(:bb  (clojure.lang MapEntry)
                      :clj (clojure.lang MapEntry LazilyPersistentVector))
                   (java.util.concurrent TimeoutException TimeUnit FutureTask))))

(def ^:const +max-size+ #?(:clj Long/MAX_VALUE, :cljs (.-MAX_VALUE js/Number)))

(defn -tagged [k v] #?(:clj (MapEntry. k v), :cljs (MapEntry. k v nil)))
(defn -tagged? [v] (instance? MapEntry v))

(defn -invalid? [x] #?(:clj (identical? x :malli.core/invalid), :cljs (keyword-identical? x :malli.core/invalid)))
(defn -map-valid [f v] (if (-invalid? v) v (f v)))
(defn -map-invalid [f v] (if (-invalid? v) (f v) v))
(defn -reduce-kv-valid [f init coll] (reduce-kv (comp #(-map-invalid reduced %) f) init coll))

(defn -last [x] (if (vector? x) (peek x) (last x)))
(defn -some [pred coll] (reduce (fn [ret x] (if (pred x) (reduced true) ret)) nil coll))
(defn -merge [m1 m2] (if m1 (persistent! (reduce-kv assoc! (transient m1) m2)) m2))

(defn -error
  ([path in schema value] {:path path, :in in, :schema schema, :value value})
  ([path in schema value type] {:path path, :in in, :schema schema, :value value, :type type}))

(defn -vmap
  ([os] (-vmap identity os))
  ([f os] #?(:clj  (let [c (count os)]
                     (if-not (zero? c)
                       (let [oa (object-array c), iter (.iterator ^Iterable os)]
                         (loop [n 0] (when (.hasNext iter) (aset oa n (f (.next iter))) (recur (unchecked-inc n))))
                         #?(:bb  (vec oa)
                            :clj (LazilyPersistentVector/createOwning oa))) []))
             :cljs (into [] (map f) os))))

#?(:clj
   (defn ^:no-doc -run [^Runnable f ms]
     (let [task (FutureTask. f), t (Thread. task)]
       (try
         (.start t) (.get task ms TimeUnit/MILLISECONDS)
         (catch TimeoutException _ (.cancel task true) ::timeout)
         (catch Exception e (.cancel task true) (throw e))))))

#?(:clj
   (defmacro -combine-n
     [c n xs]
     (let [syms (repeatedly n gensym)
           g (gensym "preds__")
           bs (interleave syms (map (fn [n] `(nth ~g ~n)) (range n)))
           arg (gensym "arg__")
           body `(~c ~@(map (fn [sym] `(~sym ~arg)) syms))]
       `(let [~g (-vmap ~xs) ~@bs]
          (fn [~arg] ~body)))))

#?(:clj
   (defmacro -pred-composer
     [c n]
     (let [preds (gensym "preds__")
           f (gensym "f__")
           cases (mapcat (fn [i] [i `(-combine-n ~c ~i ~preds)]) (range 2 (inc n)))
           else `(let [p# (~f (take ~n ~preds)) q# (~f (drop ~n ~preds))]
                   (fn [x#] (~c (p# x#) (q# x#))))]
       `(fn ~f [~preds]
          (case (count ~preds)
            0 (constantly (boolean (~c)))
            1 (first ~preds)
            ~@cases
            ~else)))))

(def ^{:arglists '([[& preds]])} -every-pred
  #?(:clj  (-pred-composer and 16)
     :cljs (fn [preds] (fn [m] (boolean (reduce #(or (%2 m) (reduced false)) true preds))))))

(def ^{:arglists '([[& preds]])} -some-pred
  #?(:clj  (-pred-composer or 16)
     :cljs (fn [preds] (fn [x] (boolean (some #(% x) preds))))))

(defn -exception [type data] (ex-info (str type) {:type type, :message type, :data data}))

(defn -fail!
  ([type] (-fail! type nil))
  ([type data] (throw (-exception type data))))

(defn -distinct-by
  "Returns a sequence of distinct (f x) values)"
  [f coll]
  (let [seen (atom #{})]
    (filter (fn [x] (let [v (f x)] (when-not (@seen v) (swap! seen conj v)))) coll)))

;; also doubles as a predicate for the :every schema to bound the number
;; of elements to check, so don't add potentially-infinite countable things like seq's.
(defn -safely-countable? [x]
  (or (nil? x)
      (counted? x)
      (indexed? x)
      ;; note: js/Object not ISeqable
      #?(:clj (instance? java.util.Map x))
      ;; many Seq's are List's, so just pick some popular classes
      #?@(:bb  []
          :clj [(instance? java.util.AbstractList x)
                (instance? java.util.Vector x)])
      #?(:clj  (instance? CharSequence x)
         :cljs (string? x))
      #?(:clj  (.isArray (class x))
         :cljs (identical? js/Array (c/type x)))))


(defn -safe-count [x]
  (if (-safely-countable? x)
    (count x)
    (reduce (fn [cnt _] (inc cnt)) 0 x)))
