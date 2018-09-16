(ns indra-clj.complex
  (:refer-clojure :rename {+ clj-add
                           - clj-sub
                           * clj-mul
                           / clj-div} )
  (:import [org.apache.commons.math3.complex Complex]
           [org.apache.commons.math3.util FastMath]))

;; simple shim around the apache commons complex number implementation

(comment
  (set! *warn-on-reflection* true)
  (set! *unchecked-math* :warn-on-boxed))

(def zero (Complex. 0))

(def one (Complex. 1))

(def i (Complex. 0 1))

(defn rect
  [real imag]
  (Complex. real imag))

(defn polar
  [^double mag ang]
  (Complex. (clj-mul mag (FastMath/cos ang))
            (clj-mul mag (FastMath/sin ang))))

(defn real
  [^Complex z]
  (.getReal z))

(defn imag
  [^Complex z]
  (.getImaginary z))

(defn abs
  [^Complex z]
  (.abs z))

(defn argument
  [^Complex z]
  (.getArgument z))

(defn +
  ([] zero)
  ([x] x)
  ([^Complex x ^Complex y] (.add x y))
  ([x y & more] (reduce + (+ x y) more)))

(defn -
  ([] zero)
  ([^Complex x] (.negate x))
  ([^Complex x ^Complex y] (.subtract x y))
  ([x y & more] (reduce - (- x y) more)))

(defn *
  ([] one)
  ([x] x)
  ([^Complex x ^Complex y] (.multiply x y))
  ([x y & more] (reduce * (* x y) more)))

(defn /
  ([] one)
  ([^Complex x] (.reciprocal x))
  ([^Complex x ^Complex y] (.divide x y))
  ([x y & more] (reduce / (/ x y) more)))
