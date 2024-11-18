(ns dcnn.affine-cipher
  (:require [clojure.math.numeric-tower :refer [gcd]]))

(defn affine-cipher [m k1 k2 p]
  (-> m
      (* k1)
      (+ k2)
      (mod p)))


(defn affine-decipher [c k1 k2 p]
  (-> (biginteger c)
      (.subtract k2)
      (.multiply (.modInverse k1 p))
      (.mod p)))

(def p (biginteger 7919))

(def k1 (biginteger 151))
(def k2 (biginteger 117))

(def m1 111)
(def m2 5123)
(def m3 999)
(def m4 1234)

(def c1 (affine-cipher m1 k1 k2 p))
(def c2 (affine-cipher m2 k1 k2 p))
(def c3 (affine-cipher m3 k1 k2 p))
(def c4 (affine-cipher m4 k1 k2 p))

(affine-decipher c1 k1 k2 p) ;=> 111
(affine-decipher c2 k1 k2 p) ;=> 5123
(affine-decipher c3 k1 k2 p) ;=> 999
(affine-decipher c4 k1 k2 p) ;=> 1234

(defn =modp [a b p]
  (= (mod a p) (mod b p)))

(comment

  ; різницею с1 і с2 скорочуємо ключ к2
  (=modp (- c1 c2) (* k1 (- m1 m2)) p) ;=> true

  ;тоді
  (=modp k1 (* (- c1 c2) (.modInverse (biginteger (- m1 m2)) p)) p) ;=> true

  ; підставивши к1 маємо такі добутки конгруентні за модулем р
  (=modp (* (- c1 c2) (- m1 m3)) (* (- c1 c3) (- m1 m2)) p) ;=> true

  ; тоді їх різниця теж кратна р і р можна шукати серед його розкладу на прості множники
  ; отже можна знайти за мінімум трьома парами відкритий текст/шифртекст
  (=modp (- (* (- c1 c2) (- m1 m3)) (* (- c1 c3) (- m1 m2))) 0 p) ;=> true

  (=modp (- (* c1 (- m2 m3)) (* m1 (- c2 c3))) 0 p) ;=> true


  ; додавши четверту пару відкритий текст/шифртекст можна р можна шукати як НСД таких різниць
  (let [f (fn [[c1 c2 c3 m1 m2 m3]] (- (* (- c1 c2) (- m1 m3)) (* (- c1 c3) (- m1 m2))))]
    (->> [[c1 c2 c3 m1 m2 m3] [c4 c2 c3 m4 m2 m3] [c1 c4 c3 m1 m4 m3] [c1 c2 c4 m1 m2 m4]]
         (map f)
         (reduce gcd)
         )) ;=> 7919

  )