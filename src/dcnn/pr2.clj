(ns dcnn.pr2
  (:require [clojure.math.numeric-tower :refer [gcd]]))

(def n 23843)

(defn fermat [n t]
  (loop [n n t t]
    (let [b2 (- (Math/pow t 2) n)
          b (Math/sqrt b2)]
     (prn n t b b2)
     (if (= (int b) b)
       t
       (recur n (inc t))))))



(defn pollard [n]
  (let [g (fn [x] (.add (.modPow (biginteger x) (biginteger 2) (biginteger n)) (biginteger 1)))]
    (loop [x 2
           y 2]
      (let [x (g x)
            y (g (g y))
            d (gcd (.abs (.subtract x y)) n)]
        (prn x y (.abs (.subtract x y)) d)
        (if (= 1 d)
          (if (= n d) "fail" (recur x y))
          d)))))

(def n2 61063)
(def a1 1882)
(def a2 1898)


(def N (biginteger 3751785649))
(def p 60631)
(def q 61879)
(def n (biginteger (* p q)))
(def d (biginteger 1653279371))
(def e (.modInverse d (biginteger (* (dec q) (dec p)))))
(def m 2970048811)

(def m2 (biginteger 17761831))

(comment
  (.modPow (biginteger 67736579) e N)

  (.modPow (biginteger 1113231101) e (biginteger (* p q)))
  (.modPow (biginteger 917998221) e (biginteger (* p q)))


  (Math/sqrt 3682849691)
  (pollard 3682849691)
  (pollard 1739)
  (gcd (- 10309 667) n)
  (Math/pow 677 2)

  (.modPow (biginteger 1150732830) (biginteger 26017705) (biginteger 3682849691))
  (.modInverse (biginteger 17) (biginteger 1656))


  (mod (Math/pow a1 2) n2)
  (mod (Math/pow a2 2) n2)
  (gcd n (- ))

  (/ n2 (gcd (int (- (mod (* 2 (Math/pow 3 4) (* 5 5)) n2)

                (mod (* a1 a2) n2))) n2))

  (mod (.modPow (biginteger m2) e N) N)

  (.modPow (biginteger 2970048813) e N)
  )

(def p (biginteger 58477))

(def q (biginteger 59651))

(def d (biginteger 1052472841))

(.modInverse (biginteger 13) (biginteger 264))
(.modPow (biginteger 2) (biginteger 88) (biginteger 509))

(.modPow (biginteger 314) (biginteger 17) (biginteger 1739))


(def c1 (biginteger 2883179429))

(def c2 (biginteger 1390467723))

(.modPow c1 d (.multiply p q))
(.modPow c2 d (.multiply p q))

(gcd 1021763679 519424709)