;; # PEG
;; Try to implement the [janet language peg parser](https://janet-lang.org/docs/peg.html)
;; in clojure.
;;
;; There is a nice [blog post](https://bakpakin.com/writing/how-janets-peg-works.html) detailing
;; the basic construction of the peg module.
;;
;; The inner most thing returns either nil, if there isn't a match or [bytes captures] if there is.
(ns jpeg.jpeg
  "A simple clojure version of the Janet PEG library."
  (:require [clojure.string :as str]))

(defn peg-type [peg _text _grammar _captures] (type peg))

(defmulti match-impl peg-type)

(defmethod match-impl String
  [peg text _grammar captures]
  (when (str/starts-with? text peg) [(count peg) captures]))

(defn leading-symbol [peg _text _grammar _captures]
  (when (symbol? (first peg))
    (name (first peg))))

(defmulti match-seq leading-symbol)

(defmethod match-impl clojure.lang.Cons
  [peg text grammar captures]
  (match-seq peg text grammar captures))

(defn- match-not [[_ x] text grammar captures]
  (when-not (match-impl x text grammar captures) [0 captures]))

(defmethod match-seq "!"
  [x text grammar captures]
  (match-not x text grammar captures))

(defmethod match-seq "not"
  [x text grammar captures]
  (match-not x text grammar captures))

(defmethod match-seq "set"
  [[_ chrs] text _grammar captures]
  (when ((set chrs) (first text)) [1 captures]))

(defn- char-range [[start end]]
  (map char (range (int start) (inc (int end)))))

(defmethod match-seq "range"
  [[_ & rngs] text _grammar captures]
  (let [chrs (into #{} (mapcat char-range) rngs)]
    (when (chrs (first text)) [1 captures])))

(defmethod match-impl clojure.lang.Keyword
  [x text grammar captures]
  (match-impl (grammar x) text grammar captures))

(defmethod match-impl clojure.lang.PersistentHashMap
  [map text grammar captures]
  (let [new-grammar (into grammar map)]
    (match-impl (new-grammar :main) text new-grammar captures)))

(defmethod match-impl Number
  [n text _grammar captures]
  (if (pos? n)
    (when (>= (count text) n) [n captures])
    (when (< (count text) (abs n)) [0 captures])))

;; ## Sequential captures

(defn- match-choice [[_ & xs] text grammar captures]
  (some (fn [x] (match-impl x text grammar captures)) xs))

(defmethod match-seq "+"
  [x text grammar captures]
  (match-choice x text grammar captures))

(defmethod match-seq "choice"
  [x text grammar captures]
  (match-choice x text grammar captures))

(defn- match-sequence [[_ & xs] text grammar captures]
  (butlast
   (reduce
    (fn [[acc caps text] x]
      (if-let [[n cap] (match-impl x text grammar captures)]
        [(+ acc n) (into caps cap) (subs text n)]
        (reduced nil)))
    [0 captures text]
    xs)))

(defmethod match-seq "*"
  [x text grammar captures]
  (match-sequence x text grammar captures))

(defmethod match-seq "sequence"
  [x text grammar captures]
  (match-sequence x text grammar captures))

(defmethod match-seq "any"
  [[_ x] text grammar captures]
  (butlast
   (reduce
    (fn [[acc caps text] x]
      (if-let [[n cap] (match-impl x text grammar captures)]
        [(+ acc n) (into caps cap) (subs text n)]
        (reduced [acc caps text])))
    [0 captures text]
    (repeat x))))

(defmethod match-seq "some"
  [[_ x] text grammar captures]
  (match-sequence `(* ~x (any ~x)) text grammar captures))

(defmethod match-seq "repeat"
  [[_ n x] text grammar captures]
  (match-sequence `(* ~@(repeat n x)) text grammar captures))

(defmethod match-seq "at-least"
  [[_ n x] text grammar captures]
  (match-sequence `(* (repeat ~n ~x) (any ~x)) text grammar captures))

(defmethod match-seq "at-most"
  [[_ n x] text grammar captures]
  (butlast
   (reduce
    (fn [[acc caps text left] x]
      (if (zero? left)
        (reduced [acc caps text left])
        (if-let [[n cap] (match-impl x text grammar captures)]
          [(+ acc n) (into caps cap) (subs text n) (dec left)]
          (reduced [acc caps text left]))))
    [0 captures text n]
    (repeat x))))

(defmethod match-seq "between"
  [[_ min max x] text grammar captures]
  (match-sequence `(* (repeat ~min ~x) (at-most ~(- max min) ~x)) text grammar captures))

(defmethod match-seq "opt"
  [[_ x] text grammar captures]
  (match-seq `(between 0 1 ~x) text grammar captures))

(defmethod match-seq "?"
  [[_ x] text grammar captures]
  (match-seq `(opt ~x) text grammar captures))

(defmethod match-seq "if"
  [[_ cond x] text grammar captures]
  (when (match-impl cond text grammar captures)
        (match-impl x text grammar captures)))

(defmethod match-seq "if-not"
  [[_ cond x] text grammar captures]
  (when (match-impl `(not ~cond) text grammar captures)
        (match-impl x text grammar captures)))

(defn- match-look
  [[_ offset patt] text grammar captures]
  (when (match-impl patt (subs text offset) grammar captures)
        [0 captures]))

(defmethod match-seq "look"
  [x text grammar captures]
  (match-look x text grammar captures))

(defmethod match-seq ">"
  [x text grammar captures]
  (match-look x text grammar captures))

(defmethod match-seq "to"
  [[_ x] text grammar captures]
  (match-seq `(* (any (if-not ~x 1)) (> 0 ~x)) text grammar captures))

(defmethod match-seq "thru"
  [[_ x] text grammar captures]
  (match-seq `(* (any (if-not ~x 1)) ~x) text grammar captures))

;; TODO backmatch

;; ## Default Grammar

(def default-grammar
  `{:d (range "09")
    :a (range "az" "AZ")
    :w (range "az" "AZ" "09")
    :s (set " \t\r\n\0\f")
    :h (range "09" "af" "AF")
    :D (if-not :d 1)
    :A (if-not :a 1)
    :W (if-not :w 1)
    :S (if-not :s 1)
    :H (if-not :h 1)
    :d+ (some :d)
    :a+ (some :a)
    :w+ (some :w)
    :s+ (some :s)
    :h+ (some :h)
    :d* (any :d)
    :a* (any :a)
    :w* (any :w)
    :s* (any :s)
    :h* (any :h)})

(defn- default-captures [text]
  {:_otext text
   :_stack []})

(defn raw-match
  "Main entrypoint for the API."
  ([grammar text] (if (map? grammar)
                    (match-impl (grammar :main) text (into default-grammar grammar) (default-captures text))
                    (match-impl grammar text default-grammar (default-captures text))))
  ([peg text grammar] (match-impl peg text grammar (default-captures text))))


(comment
  (remove-all-methods match-impl))
