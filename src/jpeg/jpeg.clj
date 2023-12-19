;; # PEG
;; Try to implement the [janet language peg parser](https://janet-lang.org/docs/peg.html)
;; in clojure.
;;
;; There is a nice [blog post](https://bakpakin.com/writing/how-janets-peg-works.html) detailing
;; the basic construction of the peg module.
(ns jpeg.jpeg
  "A simple clojure version of the Janet PEG library."
  (:require [clojure.string :as str]))

(defn peg-type [peg _text _grammar] (type peg))

(defmulti match-impl peg-type)

(defmethod match-impl String
  [peg text _grammar]
  (when (str/starts-with? text peg) (count peg)))

(defn leading-symbol [peg _text _grammar]
  (when (symbol? (first peg))
    (name (first peg))))

(defmulti match-seq leading-symbol)

(defmethod match-impl clojure.lang.Cons
  [peg text grammar]
  (match-seq peg text grammar))

(defmethod match-seq "!"
  [[_ x] text grammar]
  (when-not (match-impl x text grammar) 0))


(defmethod match-seq "+"
  [[_ & xs] text grammar]
  (some (fn [x] (match-impl x text grammar)) xs))


(defmethod match-seq "*"
  [[_ & xs] text grammar]
  (first
   (reduce
    (fn [[acc text] x]
      (if-let [n (match-impl x text grammar)]
        [(+ acc n) (subs text n)]
        (reduced nil)))
    [0 text]
    xs)))

(defmethod match-seq
  "set"
  [[_ chrs] text _grammar]
  (when ((set chrs) (first text)) 1))

(defmethod match-impl clojure.lang.Keyword
  [x text grammar]
  (match-impl (grammar x) text grammar))

(defmethod match-impl clojure.lang.PersistentHashMap
  [map text grammar]
  (let [new-grammar (into grammar map)]
    (match-impl (new-grammar :main) text new-grammar)))

(defn match
  "Main entrypoint for the API."
  ([grammar text] (if (map? grammar)
                    (match-impl (grammar :main) text grammar)
                    (match-impl grammar text {})))
  ([peg text grammar] (match-impl peg text grammar)))


(comment
  (remove-all-methods match-impl)
  (remove-all-methods match-seq)

  (methods match-impl))
