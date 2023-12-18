;; # PEG
;; Try to implement the [janet language peg parser](https://janet-lang.org/docs/peg.html)
;; in clojure.
;;
;; There is a nice [blog post](https://bakpakin.com/writing/how-janets-peg-works.html) detailing
;; the basic construction of the peg module.
(ns jpeg.jpeg
  "A simple clojure version of the Janet PEG library."
  (:require [clojure.string :as str]))

(defn peg-type [peg _] (type peg))

(defmulti match peg-type)

(defmethod match String
  [peg text]
  (when (str/starts-with? text peg) (count peg)))

(defmethod match :default
  [peg text]
  (println "DEFUALT"))

(comment
  (match `(+ "foo" "bar") "foobar"))

(defn leading-symbol [peg _]
  (when (symbol? (first peg))
    (name (first peg))))

(defmulti match-seq leading-symbol)

(defmethod match clojure.lang.Cons
  [peg text]
  (match-seq peg text))

(defmethod match-seq "!"
  [[_ x] text]
  (when-not (match x text) 0))


(defmethod match-seq "+"
  [[_ & xs] text]
  (some (fn [x] (match x text)) xs))


(defmethod match-seq "*"
  [[_ & xs] text]
  (first
   (reduce
    (fn [[acc text] x]
      (if-let [n (match x text)]
        [(+ acc n) (subs text n)]
        (reduced nil)))
    [0 text]
    xs)))


(defmethod match-seq
  "set"
  [[_ chrs] text]
  (when ((set chrs) (first text))
    1))


(comment
  (match `(set "abcd") "afoo")

  (first `{:a :b})
  (type `(:a :b))

  (type `:green)

  (leading-symbol `{:a "abcde"} "foo"))

(comment
  (test/run-all-tests)

  (remove-all-methods match)
  (remove-all-methods match-seq)

  (methods match))
