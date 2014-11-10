(ns krulak.test-util
  (:use clojure.test
        krulak.util))

(deftest test-deep-merge
  (is (= {:a 1 :b {:x 1 :y 3} :c 2}
         (deep-merge {:a 1 :b {:x 0 :y 3}} {:b {:x 1} :c 2})))
  (is (= {:a {:a 1} :b {:b 2}}
         (deep-merge {:a nil :b {:b 2}} {:a {:a 1} :b nil}))))

(deftest test-url-base64-uid
  (is (= 21 (count (url-base64-uid))))
  (is (= 32 (count (url-base64-uid 32)))))

(deftest test-rmerge
  (is (= {:a 0} (rmerge {:a 0} {:a 1})))
  (is (= {:a 0 :b 1 :c 2} (rmerge {:a 0 :b 1} {:a 1 :c 2}))))

(deftest test-xml-escape
  (is (= "&lt;a href=&quot;&quot;&gt;&apos;Click&lt;/a&gt;"
         (xml-escape "<a href=\"\">'Click</a>"))))

(deftest test-xml-unescape
  (is (= "<a href=\"\">'Click</a>"
         (xml-unescape "&lt;a href=&quot;&gt;&apos;Click&lt;/a&gt;")))
  (let [span "<span class='\"'></span>"]
    (is (= span (xml-unescape (xml-escape span))))))

(deftest test-resolve-str
  (ns krulak.test-util-data)
  (def lue 42)
  (in-ns 'krulak.test-util)
  (are [a b] (= a@(resolve-str b))
       resolve-str "resolve-str"
       42 "krulak.test-util-data/lue")
  (is (nil? (resolve-str nil)))
  (is (nil? (resolve-str ""))))

(deftest test-secret-key
  (is (= 24 (count (secret-key))))
  (is (= 44 (count (secret-key 32)))))

(deftest test-domain-slug
  (are [a b] (= a (domain-slug b))
       "alpha" "ALPHA"
       "abc" "123abc"
       "vinividivici" "vini, vidi-vici"
       "abc" ".123abc"))
