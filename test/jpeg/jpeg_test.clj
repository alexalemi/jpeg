(ns jpeg.jpeg-test
  (:require [clojure.test :as t :refer [deftest is testing are]]
            [jpeg.jpeg :refer [match]]))

(deftest default-tests
  (is (= (match '"foo" "foobar") 3))
  (is (= (match '"foobar" "foobar") 6))
  (is (= (match '"bar" "foobar") nil)))

(deftest not-tests
  (is (= (match `(! "foo") "foobar") nil))
  (is (= (match `(! "bar") "foobar") 0)))

(deftest +-tests
  (are [x y] (= (match `(+ "foo" "bar") x) y)
    "qux" nil
    "foobar" 3
    "foofoo" 3
    "bartop" 3)
  (are [x y] (= (match `(+ "foo" "bar" "qux") x) y)
    "qux" 3
    "foobar" 3
    "foofoo" 3
    "bartop" 3
    "quxxy" 3
    "qux" 3
    "spaz" nil
    "floofbar" nil))

(deftest *-tests
  (are [x y] (= (match `(* "foo" "bar") x) y)
    "qux" nil
    "foo" nil
    "bar" nil
    "foobar" 6)
  (are [x y] (= (match `(* "f" "oo" "bar") x) y)
    "qux" nil
    "foo" nil
    "bar" nil
    "foobar" 6))

(deftest set-tests
  (are [x y] (= (match `(set "abc") x) y)
    "aaa" 1
    "a" 1
    "bar" 1
    "foobar" nil
    "" nil))

(deftest keyword-tests
  (is (= 3 (match `:foo "foo" {:foo "foo"})))
  (is (= nil (match `:foo "foo" {:foo "bar"})))
  (is (= 6 (match `(* :foo :bar) "foobar" {:foo "foo" :bar "bar"})))
  (is (= nil (match `(* :foo :bar) "fooqux" {:foo "foo" :bar "bar"}))))

(deftest iso-date
  (let [digit `(+ "0" (+ "1" (+ "2" (+ "3" (+ "4" (+ "5" (+ "6" (+ "7" (+ "8" "9")))))))))
        year `(* ~digit (* ~digit (* ~digit ~digit)))
        month `(* ~digit ~digit)
        day month
        iso-date `(* ~year (* "-" (* ~month (* "-" ~day))))]
    (are [x y] (= (match iso-date x) y)
      "2019-06-10" 10
      "0012-00-00" 10
      "201-06-10" nil
      "201--6-10" nil))
  (let [digit `(set "0123456789")
        year `(* ~digit ~digit  ~digit ~digit)
        month `(* ~digit ~digit)
        day month
        iso-date `(* ~year "-" ~month "-" ~day)]
    (are [x y] (= (match iso-date x) y)
      "2019-06-10" 10
      "0012-00-00" 10
      "201-06-10" nil
      "201--6-10" nil))
  (let [iso-peg `{:digit (set "0123456789")
                  :year (* :digit :digit :digit :digit)
                  :month (* :digit :digit)
                  :day (* :digit :digit)
                  :main (* :year "-" :month "-" :day)}]
    (are [x y] (= (match iso-peg x) y)
      "2019-06-10" 10
      "0012-00-00" 10
      "201-06-10" nil
      "201--6-10" nil)))
