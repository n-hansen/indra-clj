(ns indra.complex
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

(def zero Complex/ZERO)
(def one Complex/ONE)
(def i Complex/I)
(def inf Complex/INF)

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

(defn abs ^double
  [^Complex z]
  (.abs z))

(defn argument
  [^Complex z]
  (.getArgument z))

(defn zero?
  [^Complex z]
  (= z zero))

(defn inf?
  [^Complex z]
  (.isInfinite z))

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

(defn reciprocal
  [^Complex z] (.reciprocal z))

(defn div ; `/` looks dumb when fully qualified
  ([] one)
  ([^Complex x] (.reciprocal x))
  ([^Complex x ^Complex y] (.multiply x (.reciprocal y))) ; avoiding .divide to handle inf correctly
  ([x y & more] (reduce div (div x y) more)))

(defn pow
  [^Complex x ^Complex y]
  (.pow x y))

(defn sqrt
  [^Complex z]
  (if (inf? z) z (.sqrt z)))

(defn =*
  ([x y] (=* x y 1e-9))
  ([x y ^double e]
   (<= (-> (- x y) abs) e)))

; arithmetic operations by a scalar

(defn +real
  [^Complex z ^double r]
  (.add z r))

(defn -real
  [^Complex z ^double r]
  (.subtract z r))

(defn *real
  [^Complex z ^double r]
  (.multiply z r))

(defn div-real
  [^Complex z ^double r]
  (.divide z r))

(defn pow-real
  [^Complex z ^double r]
  (.pow z r))

(defmethod print-method Complex
  [o w]
  (print-simple
   (format "%.2g%+.2gi" (real o) (imag o))
   w))

(comment (rect 1 2))
