(ns dcnn.pr3
  (:require [clojure.math.numeric-tower :refer [gcd]]))


(.modInverse (biginteger 101) (biginteger 199))

(def p (biginteger 19))

(def x1 (biginteger 15))
(def x2 (biginteger 3))
(def y1 (biginteger 2))
(def y2 (biginteger 10))

(map (fn [x] (mod (* x x) 7)) (range 7))

(mod (inc (* 6 6 6)) 7)
(mod (* 6 3) 7)

(mapv (fn [x] (mod (* x x) 29)) (range 29))

(.modInverse (biginteger 1) (biginteger 7))