(ns jpeg.jpeg-test
  (:require [clojure.test :as t :refer [deftest is testing are]]
            [jpeg.jpeg :refer [raw-match]]))

(defn at [y]
  (when y [y {}]))

(deftest default-tests
  (is (= (raw-match '"foo" "foobar") (at 3)))
  (is (= (raw-match '"foobar" "foobar") (at 6)))
  (is (= (raw-match '"bar" "foobar") nil)))

(deftest !-tests
  (is (= (raw-match `(! "foo") "foobar") nil))
  (is (= (raw-match `(! "bar") "foobar") (at 0))))

(deftest not-tests
  (is (= (raw-match `(not "foo") "foobar") nil))
  (is (= (raw-match `(not "bar") "foobar") (at 0))))

(deftest +-tests
  (are [x y] (= (raw-match `(+ "foo" "bar") x) (at y))
    "qux" nil
    "foobar" 3
    "foofoo" 3
    "bartop" 3)
  (are [x y] (= (raw-match `(+ "foo" "bar" "qux") x) (at y))
    "qux" 3
    "foobar" 3
    "foofoo" 3
    "bartop" 3
    "quxxy" 3
    "qux" 3
    "spaz" nil
    "floofbar" nil))

(deftest choice-tests
  (are [x y] (= (raw-match `(choice "foo" "bar") x) (at y))
    "qux" nil
    "foobar" 3
    "foofoo" 3
    "bartop" 3)
  (are [x y] (= (raw-match `(choice "foo" "bar" "qux") x) (at y))
    "qux" 3
    "foobar" 3
    "foofoo" 3
    "bartop" 3
    "quxxy" 3
    "qux" 3
    "spaz" nil
    "floofbar" nil))

(deftest *-tests
  (are [x y] (= (raw-match `(* "foo" "bar") x) (at y))
    "qux" nil
    "foo" nil
    "bar" nil
    "foobar" 6)
  (are [x y] (= (raw-match `(* "f" "oo" "bar") x) (at y))
    "qux" nil
    "foo" nil
    "bar" nil
    "foobar" 6))

(deftest sequence-tests
  (are [x y] (= (raw-match `(sequence "foo" "bar") x) (at y))
    "qux" nil
    "foo" nil
    "bar" nil
    "foobar" 6)
  (are [x y] (= (raw-match `(sequence "f" "oo" "bar") x) (at y))
    "qux" nil
    "foo" nil
    "bar" nil
    "foobar" 6))

(deftest set-tests
  (are [x y] (= (raw-match `(set "abc") x) (at y))
    "aaa" 1
    "a" 1
    "bar" 1
    "foobar" nil
    "" nil))

(deftest keyword-tests
  (is (= (at 3) (raw-match `:foo "foo" {:foo "foo"})))
  (is (= nil (raw-match `:foo "foo" {:foo "bar"})))
  (is (= (at 6) (raw-match `(* :foo :bar) "foobar" {:foo "foo" :bar "bar"})))
  (is (= nil (raw-match `(* :foo :bar) "fooqux" {:foo "foo" :bar "bar"}))))

(deftest number-tests
  (is (= (at 3) (raw-match `3 "foo")))
  (is (= (at 3) (raw-match `3 "foobar")))
  (is (= nil (raw-match `3 "fo")))
  (is (= nil (raw-match `-3 "foo")))
  (is (= (at 0) (raw-match `-3 "fo"))))

(deftest range-tests
  (is (= (at 1) (raw-match `(range "az") "apple")))
  (is (= nil (raw-match `(range "az") "0apple")))
  (is (= (at 1) (raw-match `(range "af") "fapple")))
  (is (= nil (raw-match `(range "af") "")))
  (is (= (at 1) (raw-match `(range "09") "123"))))

(deftest iso-date
  (let [digit `(+ "0" (+ "1" (+ "2" (+ "3" (+ "4" (+ "5" (+ "6" (+ "7" (+ "8" "9")))))))))
        year `(* ~digit (* ~digit (* ~digit ~digit)))
        month `(* ~digit ~digit)
        day month
        iso-date `(* ~year (* "-" (* ~month (* "-" ~day))))]
    (are [x y] (= (raw-match iso-date x) (at y))
      "2019-06-10" 10
      "0012-00-00" 10
      "201-06-10" nil
      "201--6-10" nil))
  (let [digit `(set "0123456789")
        year `(* ~digit ~digit  ~digit ~digit)
        month `(* ~digit ~digit)
        day month
        iso-date `(* ~year "-" ~month "-" ~day)]
    (are [x y] (= (raw-match iso-date x) (at y))
      "2019-06-10" 10
      "0012-00-00" 10
      "201-06-10" nil
      "201--6-10" nil))
  (let [iso-peg `{:digit :d
                  :year (* :digit :digit :digit :digit)
                  :month (* :digit :digit)
                  :day (* :digit :digit)
                  :main (* :year "-" :month "-" :day)}]
    (are [x y] (= (raw-match iso-peg x) (at y))
      "2019-06-10" 10
      "0012-00-00" 10
      "201-06-10" nil
      "201--6-10" nil)))
