(ns krulak.util
  (:require [ring.util.codec :as codec]
            [cemerick.url :as url]
            [clojure.string :as str]
            [clj-time.core :as tm]
            [clj-time.format :as tf]
            [cheshire.core :refer :all]
            [clojure.core.cache :as cache])
  
  (:import javax.crypto.Mac
           javax.crypto.spec.SecretKeySpec))

(def month-names ["January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"])

(defn pretty-date
  "Return date formatted like \"February 2, 2014\"."
  ([] (pretty-date (tm/now)))
  ([dt]
     (let [month-name (-> dt tm/month dec month-names)]
       (str month-name " " (tm/day dt) ", " (tm/year dt)))))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(def date-format (tf/formatter "yyyy/MM/dd HH:mm:ss"))

(def parse-date (partial tf/parse date-format))

(defn lru-ttl-cache [base threshold ttl]
  (-> base (cache/lru-cache-factory :threshold threshold) (cache/ttl-cache-factory :ttl ttl)))

(defn format-date 
  ([] (format-date (tm/now)))
  ([dt] (tf/unparse date-format dt)))

(defmacro defalias [new-name old-name]
  `(let [doc-str# (:doc (meta #'~old-name))]
     (def ^{:doc doc-str#} ~new-name @#'~old-name)))

(defmacro defaliases [& syms]
  `(do
    ~@(map
       (fn [[new-name old-name]]
         `(defalias ~new-name ~old-name))
       (partition 2 syms))))

(defn args-with-options
  "Split an argument collection into a map of options and a seq of the non-option args. If
   the first arg is a map, it is the options. If the first arg is a keyword, all the keywords
   at the start of args are gathered into a map with values of true (like ^:key-a ^:key-b in metadata).
   Otherwise, option is nil. You can't use both map and keyword options."
  [[head & tail :as args]]
  (cond
   (map? head) [head tail]
   (keyword? head)
   [(into {}
          (for [kw (take-while keyword? args)]
            [kw true]))
    (seq (drop-while keyword? args))]
   true [nil args]))

(defn wrap-with-options
  "Use args-with-options to provide an options map as the first arg to the wrapped fn."
  [f]
  #(apply apply f (args-with-options %&)))

(defmacro fn-with-options
  "Create a fn wrapped by wrap-with-options."
  [& body]
  `(wrap-with-options (fn ~@body)))

(defmacro defn-wrap
  "Like defn, but applies wrap-fn."
  [name-sym wrap-fn & body]
  `(do
     (defn ~name-sym ~@body)
     (alter-var-root #'~name-sym ~wrap-fn)))

#_(defmacro defn-wrap
  "Like defn, but applies wrap-fn."
  [name-sym wrap-fn & body]
  `(def ~name-sym
     (~wrap-fn
      (fn ~name-sym ~@body))))

(defmacro defn-with-options
  "Define a fn wrapped by wrap-with-options."
  [name-sym & body]
  `(defn-wrap ~name-sym wrap-with-options ~@body))

(def url-slug-replacements {(first " ") \-, \( \_, \) \_})

(def url-slug-allowed-chars
  (set "0123456789abcdefghijklmnopqrstuvwxyz-_"))

(defn to-url-slug [& xs]
  (->> (apply str xs)
       str/lower-case
       (map #(url-slug-replacements % %))
       (filter url-slug-allowed-chars)
       (apply str)))

                                        ;copied from v1.1 clojure.contrib.lazy-xml
(def xml-escape-map
  (zipmap "'<>\"&" 
          (map #(str \& % \;) '[apos lt gt quot amp])))

(defn xml-escape [s]
  (apply str (map #(xml-escape-map % %) (str s))))

(def xml-unescape-map
  (into {} (for [[v k] xml-escape-map] [v k])))

(defn replace-multi [^String s replacements]
  (if (empty? replacements)
    s
    (let [[[k v] & more] (seq replacements)]
      (recur (.replace s k v) more))))

(defn xml-unescape [s]
  (replace-multi s xml-unescape-map))

(defn alter-doc! [the-var & args]
  (apply alter-meta! the-var update-in [:doc] args))

(defn html-format [s]
  (.replace (xml-escape s) "\n" "<br>"))

(defn coerce-to-sequential [x]
  (if (sequential? x) x [x]))

(defn hmac-fn [algorithm]
  (fn [key data]
    (let [key (.getBytes key "UTF-8")
          data (.getBytes data "UTF-8")
          mac (Mac/getInstance algorithm)]
      (.init mac (SecretKeySpec. key algorithm))
      (.doFinal mac data))))

(def hmac-sha1 (hmac-fn "HmacSHA1"))
(def hmac-sha256 (hmac-fn "HmacSHA256"))
(def base64-hmac-sha1 (comp codec/base64-encode hmac-sha1))
(def base64-hmac-sha256 (comp codec/base64-encode hmac-sha256))

(defn padded-hex [num]
  (let [s (Integer/toHexString num)]
    (if (even? (count s))
      s
      (str "0" s))))

(defn base16-encode [x]
  {:pre [(not (number? x))]}
  (->> (byte-array x)
       (map padded-hex)
       (apply str)))

(def url-base64-map {\+ \-, \/ \_, \= \.})

(defn url-base64-encode
  "A url-safe base64 variant."
  [x]
  {:post [(= % (url/url-encode %))]
   :pre [(not (number? x))]}
  (->> (byte-array x)
       codec/base64-encode
       (map #(url-base64-map % %))
       (apply str)))

(defn md5 [x]
  {:pre [(not (number? x))]}
  (-> (java.security.MessageDigest/getInstance "MD5")
      (.digest (.getBytes x))))

(def base16-md5 (comp base16-encode md5))
(def url-base64-md5 (comp url-base64-encode md5))

(defn rand-bytes [num]
  {:pre [(number? num)]
   :post [(= num (count %))]}
  (let [arr (byte-array num)]
    (.nextBytes
     (java.security.SecureRandom/getInstance "SHA1PRNG")
     arr)
    arr))

(defn url-base64-uid
  "Return a random url-safe base64 id. The default length is 21 chars, which represent 126 bits, 2 less than a normal uuid."
  ([] (url-base64-uid 21))
  ([length]
     (->> (rand-bytes length)
          url-base64-encode
          (take length)
          (apply str))))

(defn keyword-syntax? [s]
  (re-matches #"[A-Za-z*+!_?-][A-Za-z0-9*+!_?-]*" s))

(defn keywordify-keys [target]
  (cond
   (map? target)
   (into {}
         (for [[k v] target]
           [(if (and (string? k) (keyword-syntax? k))
              (keyword k)
              k)
            (keywordify-keys v)]))
   (vector? target)
   (vec (map keywordify-keys target))
   :else
   target))

(defmacro require-auth [auth & body]
  (let [cond-form (if (vector? auth) `if-let `if)]
    `(~cond-form ~auth
                 (do ~@body)
                 {:status 401
                  :body "Unauthorized"})))

(defn errors-response [errors]
  {:status 403
   :body (generate-string {:errors errors})})

(defmacro errors-or [errors & body]
  `(if-let [errors# ~errors]
     (errors-response errors#)
     (do ~@body)))

(defn hash-id [& segments]
  (->> (map str segments)
       (map url/url-encode)
       (str/join \/)
       url-base64-md5))

(defn deep-merge [& args]
  (if (every? #(or (map? %) (nil? %)) args)
    (apply merge-with deep-merge args)
    (last args)))

(defn filter-keys [pred m]
  (into (empty m)
        (for [[k v] m :when (pred k)]
          [k v])))
		 
(defn filter-vals [pred m]
  (into (empty m)
        (for [[k v] m :when (pred v)]
		 [k v])))

(defn remove-keys [pred m]
  (filter-keys (complement pred) m))

(defn remove-vals [pred m]
  (filter-vals (complement pred) m))

(defn rmerge [& ms]
  "Like clojure.core/merge, but in reverse order."
  (apply merge (reverse ms)))

(defmacro def-do-multi [name-sym op & [args-xform]]
  `(let [form->goal# (partial list* '~op)
         args-xform# (or ~args-xform identity)]
     (defmacro ~name-sym [& forms#]
       (->> (args-xform# forms#)
            (map form->goal#)
            (list* `do)))))

(def-do-multi def-do-multis def-do-multi)

#_(def-do-multi defaliases defalias (partial partition 2))

(defmacro def-keyed-fn [name-sym fn-key]
  `(let [fn-key# ~fn-key]
     (defn ~name-sym [x# & args#]
       (apply (get x# fn-key#) x# args#))))

(defmacro def-kw-fn [name-sym]
  `(def-keyed-fn ~name-sym ~(keyword (name name-sym))))

(def-do-multis
  (def-keyed-fns def-keyed-fn (partial partition 2))
  (def-kw-fns def-kw-fn (partial partition 1)))

(defmacro defdef [name-sym op]
  `(defmacro ~name-sym [name# & args#]
     (list `def name#
            (list ~op args#))))

(defn apply-fns [x fns]
  (if (seq fns)
    (recur ((first fns) x) (rest fns))
    x))
	
(defn rpartial [f & args]
  #(apply f (concat %& args)))

(defn to-date [f x]
  (if (number? x)
    (-> x f tm/from-now)
    x))

(def to-date-seconds (partial to-date tm/seconds))

(defn map-xform
  ([f m]
     (into (empty m)
           (for [kv m] (f kv))))
  ([key-fn val-fn m]
     (map-xform (juxt (comp key-fn key) (comp val-fn val)) m)))

(defn map-keys [f m]
  (map-xform f identity m))

(defn map-vals [f m]
  (map-xform identity f m))

(defn date->str [x] (if (string? x) x (format-date x)))

(defn resolve-str [s]
  (when (seq s)
    (let [sym (symbol s)
          ns (namespace sym)]
      (when ns (-> ns symbol require))
      (resolve sym))))

(defn secret-key [& [num-bytes]]
  (-> (or num-bytes 16) rand-bytes codec/base64-encode))

(def numeric-chars (set "0123456789"))

(def domain-slug-chars (set "0123456789abcdefghijklmnopqrstuvwxyz_"))

(defn domain-slug [& xs]
  (->> (apply str xs)
       str/lower-case
       (filter domain-slug-chars)
       (drop-while numeric-chars)
       (apply str)))

