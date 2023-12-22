(ns jpeg.jpeg-test
  (:require [clojure.test :as t :refer [deftest is testing are]]
            [jpeg.jpeg :refer [raw-match]]))


(defn- es [x y]
  (= (first x) y))

(defn- se [y x]
  (es x y))

(deftest default-tests
  (is (es (raw-match '"foo" "foobar") 3))
  (is (es (raw-match '"foobar" "foobar") 6))
  (is (es (raw-match '"bar" "foobar") nil)))

(deftest !-tests
  (is (es (raw-match `(! "foo") "foobar") nil))
  (is (es (raw-match `(! "bar") "foobar") 0)))

(deftest not-tests
  (is (es (raw-match `(not "foo") "foobar") nil))
  (is (es (raw-match `(not "bar") "foobar") 0)))

(deftest +-tests
  (are [x y] (es (raw-match `(+ "foo" "bar") x) y)
    "qux" nil
    "foobar" 3
    "foofoo" 3
    "bartop" 3)
  (are [x y] (es (raw-match `(+ "foo" "bar" "qux") x) y)
    "qux" 3
    "foobar" 3
    "foofoo" 3
    "bartop" 3
    "quxxy" 3
    "qux" 3
    "spaz" nil
    "floofbar" nil))

(deftest choice-tests
  (are [x y] (es (raw-match `(choice "foo" "bar") x) y)
    "qux" nil
    "foobar" 3
    "foofoo" 3
    "bartop" 3)
  (are [x y] (es (raw-match `(choice "foo" "bar" "qux") x) y)
    "qux" 3
    "foobar" 3
    "foofoo" 3
    "bartop" 3
    "quxxy" 3
    "qux" 3
    "spaz" nil
    "floofbar" nil))

(deftest *-tests
  (are [x y] (es (raw-match `(* "foo" "bar") x) y)
    "qux" nil
    "foo" nil
    "bar" nil
    "foobar" 6)
  (are [x y] (es (raw-match `(* "f" "oo" "bar") x) y)
    "qux" nil
    "foo" nil
    "bar" nil
    "foobar" 6))

(deftest sequence-tests
  (are [x y] (es (raw-match `(sequence "foo" "bar") x) y)
    "qux" nil
    "foo" nil
    "bar" nil
    "foobar" 6)
  (are [x y] (es (raw-match `(sequence "f" "oo" "bar") x) y)
    "qux" nil
    "foo" nil
    "bar" nil
    "foobar" 6))

(deftest set-tests
  (are [x y] (es (raw-match `(set "abc") x) y)
    "aaa" 1
    "a" 1
    "bar" 1
    "foobar" nil
    "" nil))

(deftest keyword-tests
  (is (es (raw-match `:foo "foo" {:foo "foo"}) 3))
  (is (es (raw-match `:foo "foo" {:foo "bar"}) nil))
  (is (es (raw-match `(* :foo :bar) "foobar" {:foo "foo" :bar "bar"}) 6))
  (is (es (raw-match `(* :foo :bar) "fooqux" {:foo "foo" :bar "bar"}) nil)))

(deftest number-tests
  (is (es (raw-match `3 "foo") 3))
  (is (es (raw-match `3 "foobar") 3))
  (is (es (raw-match `3 "fo") nil))
  (is (es (raw-match `-3 "foo") nil))
  (is (es (raw-match `-3 "fo") 0)))

(deftest range-tests
  (is (se 1 (raw-match `(range "az") "apple")))
  (is (se nil (raw-match `(range "az") "0apple")))
  (is (se 1 (raw-match `(range "af") "fapple")))
  (is (se nil (raw-match `(range "af") "")))
  (is (se 1 (raw-match `(range "09") "123"))))

(deftest any-tests
  (is (se 4 (raw-match `(any :d) "1234abc")))
  (is (se 2 (raw-match `(any :d) "12")))
  (is (se 0 (raw-match `(any :d) "")))
  (is (se 0 (raw-match `(any :d) "a1234abc"))))

(deftest some-tests
  (is (se 4 (raw-match `(some :d) "1234abc")))
  (is (se nil (raw-match `(some :d) "a1234abc"))))

(deftest repeat-tests
    (are [n x text y] (es (raw-match `(repeat ~n ~x) text) y)
      3 :d "123" 3
      3 :d "12a" nil
      3 :d "12" nil
      3 :d "122342" 3
      2 :a "123" nil
      5 :a "alfalfa" 5
      5 :a "al1alfa" nil))

(deftest at-least-tests
    (are [n x text y] (es (raw-match `(at-least ~n ~x) text) y)
      3 :d "123" 3
      3 :d "12345678" 8
      3 :d "12asdf" nil
      3 :d "12" nil
      2 :a "123" nil
      5 :a "alfalfa" 7
      5 :a "al1alfa" nil))

(deftest at-most-tests
    (are [n x text y] (es (raw-match `(at-most ~n ~x) text) y)
      3 :d "123" 3
      3 :d "12345678" 3
      3 :d "12asdf" 2
      3 :d "12" 2
      2 :a "123" 0
      5 :a "alfalfa" 5
      5 :a "al1alfa" 2))

(deftest between-tests
    (are [min max x text y] (es (raw-match `(between ~min ~max ~x) text) y)
      3 5 :d "12" nil
      0 1 :d "12" 1
      3 5 :d "123" 3
      3 5 :d "1234" 4
      3 5 :d "12345" 5
      3 5 :d "123456" 5
      3 5 :d "12a3456" nil
      3 5 :d "123a456" 3
      3 5 :d "1234a56" 4
      3 5 :d "12345a6" 5
      3 5 :d "123456a" 5
      2 5 :a "123" nil
      2 10 :a "alfalfa" 7
      2 5 :a "alfalfa" 5
      2 10 :a "al1alfa" 2))

(deftest opt-tests
    (are [x text y] (es (raw-match `(opt ~x) text) y)
      :d "123" 1
      :d "12a" 1
      :d "12" 1
      :d "a122342" 0
      :a "123" 0
      :a "alfalfa" 1
      :a "al1alfa" 1))

(deftest if-tests
    (are [cond x text y] (es (raw-match `(if ~cond ~x) text) y)
      :d :d+ "1234" 4
      :a :d+ "1234" nil
      "12" :d+ "1234" 4
      "12" :d+ "01234" nil))

(deftest if-not-tests
    (are [cond x text y] (es (raw-match `(if-not ~cond ~x) text) y)
      :d :d+ "1234" nil
      :a :d+ "1234" 4
      "12" :d+ "1234" nil
      "12" :d+ "01234" 5))

(deftest look-tests
    (are [n x text y] (es (raw-match `(look ~n ~x) text) y)
      4 :d "123456" 0
      4 :d "1234a6" nil
      4 :d "abcd1e" 0))

(deftest >-tests
    (are [n x text y] (es (raw-match `(> ~n ~x) text) y)
      4 :d "123456" 0
      4 :d "1234a6" nil
      4 :d "abcd1e" 0))

(deftest to-tests
    (are [x text y] (es (raw-match `(to ~x) text) y)
      :d "abcde" nil
      :d "abcde1" 5
      :d "abcde1234" 5
      :d "" nil
      :d "1" 0))

(deftest thru-tests
    (are [x text y] (es (raw-match `(thru ~x) text) y)
      :d "abcde" nil
      :d "abcde1" 6
      :d "abcde1234" 6
      :d "" nil
      :d "1" 1))

(deftest iso-date
  (let [digit `(+ "0" (+ "1" (+ "2" (+ "3" (+ "4" (+ "5" (+ "6" (+ "7" (+ "8" "9")))))))))
        year `(* ~digit (* ~digit (* ~digit ~digit)))
        month `(* ~digit ~digit)
        day month
        iso-date `(* ~year (* "-" (* ~month (* "-" ~day))))]
    (are [x y] (es (raw-match iso-date x) y)
      "2019-06-10" 10
      "0012-00-00" 10
      "201-06-10" nil
      "201--6-10" nil))
  (let [digit `(set "0123456789")
        year `(* ~digit ~digit  ~digit ~digit)
        month `(* ~digit ~digit)
        day month
        iso-date `(* ~year "-" ~month "-" ~day)]
    (are [x y] (es (raw-match iso-date x) y)
      "2019-06-10" 10
      "0012-00-00" 10
      "201-06-10" nil
      "201--6-10" nil))
  (let [iso-peg `{:digit :d
                  :year (* :digit :digit :digit :digit)
                  :month (* :digit :digit)
                  :day (* :digit :digit)
                  :main (* :year "-" :month "-" :day)}]
    (are [x y] (es (raw-match iso-peg x) y)
      "2019-06-10" 10
      "0012-00-00" 10
      "201-06-10" nil
      "201--6-10" nil)))

;; # Capture Tests

(deftest capture-tests
  ;; Basic capture tests
  (are [patt string len stack]
      (let [[n caps] (raw-match `(capture ~patt) string)]
         (is (= n len))
         (is (or (nil? caps) (= (caps :_stack) stack)) "stack doesn't match!"))
    :d "12" 1 ["1"]
    :d+ "12" 2 ["12"]
    :d "abc" nil nil
    `(* :d :d :d) "123abc" 3 ["123"]
    `(* :d :d :d) "12abc" nil nil)

  ;; only capture some inside of a pattern
  (are [patt1 patt patt2 string len stack]
      (let [[n caps] (raw-match `(* ~patt1 (capture ~patt) ~patt2) string)]
         (is (= n len))
         (is (or (nil? caps) (= (caps :_stack) stack)) "stack doesn't match!"))
    :a+ :d :a+ "abc1def" 7 ["1"]
    :a+ :d :a+ "abc12def" nil nil
    :d :d :d "1235" 3 ["2"]
    :d :a+ :d "1abc5" 5 ["abc"]))


(deftest <--tests
  ;; Basic capture tests
  (are [patt string len stack]
      (let [[n caps] (raw-match `(<- ~patt) string)]
         (is (= n len))
         (is (or (nil? caps) (= (caps :_stack) stack)) "stack doesn't match!"))
    :d "12" 1 ["1"]
    :d+ "12" 2 ["12"]
    :d "abc" nil nil
    `(* :d :d :d) "123abc" 3 ["123"]
    `(* :d :d :d) "12abc" nil nil)

  ;; only capture some inside of a pattern
  (are [patt1 patt patt2 string len stack]
      (let [[n caps] (raw-match `(* ~patt1 (<- ~patt) ~patt2) string)]
         (is (= n len))
         (is (or (nil? caps) (= (caps :_stack) stack)) "stack doesn't match!"))
    :a+ :d :a+ "abc1def" 7 ["1"]
    :a+ :d :a+ "abc12def" nil nil
    :d :d :d "1235" 3 ["2"]
    :d :a+ :d "1abc5" 5 ["abc"]))


(deftest replace-tests
  (are [patt subst string len stack]
      (let [[n caps] (raw-match `(replace ~patt ~subst) string)]
         (is (= n len))
         (is (or (nil? caps) (= (caps :_stack) stack)) "stack doesn't match!"))
    :d parse-long "12" 1 [1]
    :d :foo "12" 1 [:foo]
    :d+ parse-long "12" 2 [12]
    :d :foo "abc" nil nil
    `(* :d :d :d) parse-long "123abc" 3 [123]
    `(* :d :d :d) parse-long "12abc" nil nil)

  ;; only capture some inside of a pattern
  (are [patt1 patt subst patt2 string len stack]
      (let [[n caps] (raw-match `(* ~patt1 (replace ~patt ~subst) ~patt2) string)]
         (is (= n len))
         (is (or (nil? caps) (= (caps :_stack) stack)) "stack doesn't match!"))
    :a+ :d parse-long :a+ "abc1def" 7 [1]
    :a+ :d parse-long :a+ "abc12def" nil nil
    :d :d parse-long :d "1235" 3 [2]
    :d :a+ :foo :d "1abc5" 5 [:foo]))


(deftest shortreplace-tests
  (are [patt subst string len stack]
      (let [[n caps] (raw-match `(/ ~patt ~subst) string)]
         (is (= n len))
         (is (or (nil? caps) (= (caps :_stack) stack)) "stack doesn't match!"))
    :d parse-long "12" 1 [1]
    :d :foo "12" 1 [:foo]
    :d+ parse-long "12" 2 [12]
    :d :foo "abc" nil nil
    `(* :d :d :d) parse-long "123abc" 3 [123]
    `(* :d :d :d) parse-long "12abc" nil nil)

  ;; only capture some inside of a pattern
  (are [patt1 patt subst patt2 string len stack]
      (let [[n caps] (raw-match `(* ~patt1 (/ ~patt ~subst) ~patt2) string)]
         (is (= n len))
         (is (or (nil? caps) (= (caps :_stack) stack)) "stack doesn't match!"))
    :a+ :d parse-long :a+ "abc1def" 7 [1]
    :a+ :d parse-long :a+ "abc12def" nil nil
    :d :d parse-long :d "1235" 3 [2]
    :d :a+ :foo :d "1abc5" 5 [:foo]))

(deftest constant-tests
  ;; Basic capture tests
  (are [k string len stack]
      (let [[n caps] (raw-match `(constant ~k) string)]
         (is (= n len))
         (is (or (nil? caps) (= (caps :_stack) stack)) "stack doesn't match!"))
    1 "12" 0 [1]
    :foo "12" 0 [:foo]))

(deftest position-tests
  (are [patt string len stack]
      (let [[n caps] (raw-match `(* ~patt (position)) string)]
         (is (= n len))
         (is (or (nil? caps) (= (caps :_stack) stack)) "stack doesn't match!"))
    :d "12" 1 [1]
    :d+ "12" 2 [2]
    `(some (* :d :a :d)) "1a23a45a5foo" 9 [9]))

(deftest $-tests
  (are [patt string len stack]
      (let [[n caps] (raw-match `(* ~patt ($)) string)]
         (is (= n len))
         (is (or (nil? caps) (= (caps :_stack) stack)) "stack doesn't match!"))
    :d "12" 1 [1]
    :d+ "12" 2 [2]
    `(some (* :d :a :d)) "1a23a45a5foo" 9 [9]))

(deftest line-tests
  (are [patt string len stack]
      (let [[n caps] (raw-match `(* ~patt (line)) string)]
         (is (= n len))
         (is (or (nil? caps) (= (caps :_stack) stack)) "stack doesn't match!"))
    :d "12" 1 [0]
    `(some (+ :d :s)) "1\n2\n345\n" 8 [3]
    `(some (+ :d :s)) "1\n2\n3a45\n" 5 [2]
    :d+ "12" 2 [0]
    `(some (* :d :a :d)) "1a23a45a5foo" 9 [0]))


(deftest column-tests
  (are [patt string len stack]
      (let [[n caps] (raw-match `(* ~patt (column)) string)]
         (is (= n len))
         (is (or (nil? caps) (= (caps :_stack) stack)) "stack doesn't match!"))
    :d "12" 1 [1]
    :d+ "12" 2 [2]
    `(some (+ :d :s)) "1\n2\n345\n" 8 [0]
    `(some (+ :d :s)) "1\n2\n3a45\n" 5 [1]
    `(some (* :d :a :d)) "1a23a45a5foo" 9 [9]))

