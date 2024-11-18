(ns dcnn.ec.math)

(defn ->bs [i]
  (Integer/toBinaryString i))

(defn <-bs [s]
  (Integer/parseInt s 2))

(def poly-deg (comp count ->bs))

(defn p-mult [a b]
  (reduce bit-xor
          (map-indexed
            (fn [i bit]
              (if (= bit \1)
                (bit-shift-left a i)
                0))
            (reverse (->bs b)))))

(defn p-reduce [res m]
  (let [d (- (poly-deg res) (poly-deg m))]
    (if (< d 0)
      res
      (let [mm (bit-shift-left m d)]
        (p-reduce (bit-xor res mm) m)))))




(defn poly-mult [a b m]
  ;(prn "mult " a b m)
  (-> (p-mult a b)
      (p-reduce m)))

(defn p-pow [a b m]
  (reduce (fn [res _] (poly-mult res a m)) 1 (range b)))

(defn p-inv [a m]
  (p-pow a (- (Math/pow 2 (dec (poly-deg m))) 2) m))

(defn pt-nss-ec->lr [x y a b p]
  [(bit-xor (poly-mult y y p) (poly-mult x y p))
   (bit-xor (p-pow x 3 p) (poly-mult a (poly-mult x x p) p) b)])

(defn pt-ss-ec->lr [x y a b c p]
  [(bit-xor (poly-mult y y p) (poly-mult c y p))
   (bit-xor (p-pow x 3 p) (poly-mult a x p) b)])

(def on-ss-ec? (comp (partial apply =) pt-ss-ec->lr))
(def on-ncss-ec? (comp (partial apply =) pt-nss-ec->lr))

(defn add-pt [p a b [x1 y1 :as s] [x2 y2 :as r]]
  {:pre [(not= y1 y2)
         ]}
  ;(prn "add" (map ->bs s) (map ->bs r))
  (let [t1 (bit-xor x1 x2)
        t2 (p-inv t1 p)
        t3 (bit-xor y1 y2)
        lam (poly-mult t2 t3 p)
        lam2 (poly-mult lam lam p)
        x3 (bit-xor lam2 lam x1 x2 a)
        t6 (bit-xor x1 x3)
        t7 (poly-mult lam t6 p)
        y3 (bit-xor t7 x3 y1)]
    [x3 y3]))

(defn dbl-pt [p a b [x1 y1]]

  ;(prn "dbl" (map ->bs [x1 y1]))
  (if (zero? (bit-xor x1 y1))
    [-1 -1]
    (let [t1 (p-inv x1 p)
          t2 (poly-mult y1 t1 p)
          mu (bit-xor x1 t2)
          mu2 (poly-mult mu mu p)
          x4 (bit-xor mu2 mu a)
          t3 (poly-mult x1 x1 p)
          t4 (poly-mult mu x4 p)
          y4 (bit-xor t3 t4 x4)]
      [x4 y4])))

(defn pt-sum [p a b [rx ry :as r] [sx sy :as s]]

  (cond (= -1 rx)
        s

        (= -1 sx)
        r

        (not= sx rx)
        (add-pt p a b r s)

        :else
        (cond (= sy (bit-xor rx ry))
              [-1 -1]

              (= sy ry)
              (dbl-pt p a b s)

              :otherwise
              (throw (ex-info "pt-sum err" {:r r :s s})))))

(defn pt-prod [p a b r n]
  (nth (iterate #(pt-sum p a b % r) r) (dec n)))

(defn dbl-pt-ss [p a b c [x1 y1]]
  ;(prn "dblss" (map ->bs [x1 y1]))
  (if (zero? x1)
    [-1 -1]
    (let [t1 (p-inv c p)
          t2 (bit-xor (poly-mult x1 x1 p) a)
          l (poly-mult t2 t1 p)
          x4 (poly-mult l l p)
          t3 (bit-xor x1 x4)
          t4 (poly-mult t3 l p)
          y4 (bit-xor t4 y1 c)]
      [x4 y4])))

(defn add-pt-ss [p a b c [x1 y1 :as s] [x2 y2 :as r]]
  {:pre [(not= y1 y2)]}
  ;(prn "addss" (map ->bs s) (map ->bs r))
  (let [t1 (bit-xor y1 y2)
        t2 (bit-xor x1 x2)
        t3 (p-inv t2 p)
        l (poly-mult t1 t3 p)
        l2 (poly-mult l l p)
        x3 (bit-xor l2 x1 x2)
        t4 (bit-xor x1 x3)
        t5 (poly-mult l t4 p)
        y3 (bit-xor t5 y1 c)]
    [x3 y3]))

(defn pt-sum-ss [p a b c [rx ry :as r] [sx sy :as s]]
  (cond (= -1 rx)
        s

        (= -1 sx)
        r

        (and (not= sx rx) (not= sy ry))
        (add-pt-ss p a b c r s)

        :else
        (cond (= sy (bit-xor c ry))
              [-1 -1]

              (= sy ry)
              (dbl-pt-ss p a b c s)

              :DO_NOT_COMPUTE
              (throw (ex-info "pt-sum err" {:r r :s s})))))

(defn pt->ss-ecs [x y p]
  "y^2 + cy = x^3 + ax + b
  x y => a b c"
  {:pre [(< (poly-deg x) (poly-deg p))
         (< (poly-deg y) (poly-deg p))]}
  (for [a (range (Math/pow 2 (dec (poly-deg p))))
        b (range (Math/pow 2 (dec (poly-deg p))))
        c (range (Math/pow 2 (dec (poly-deg p))))
        :let [[l r] (pt-ss-ec->lr x y a b c p)]
        :when (= l r)]
    [(map ->bs [a b c])]))


(defn pt->nss-ecs [x y p]
  "y^2 + xy = x^3 + ax + b
  x y => a b"
  {:pre [(< (poly-deg x) (poly-deg p))
         (< (poly-deg y) (poly-deg p))]}
  (for [a (range (Math/pow 2 (dec (poly-deg p))))
        b (range (Math/pow 2 (dec (poly-deg p))))
        :let [[l r] (pt-nss-ec->lr x y a b p)]
        :when (= l r)]
    (map ->bs [a b])))

(defn nss-ec-pts [a b p]
  (for [x (range (Math/pow 2 (dec (poly-deg p))))
        y (range (Math/pow 2 (dec (poly-deg p))))
        :let [[l r] (pt-nss-ec->lr x y a b p)]
        :when (= l r)]
    (map ->bs [x y])))

(defn ss-ec-pts [a b c p]
  (for [x (range (Math/pow 2 (dec (poly-deg p))))
        y (range (Math/pow 2 (dec (poly-deg p))))
        :let [[l r] (pt-ss-ec->lr x y a b c p)]
        :when (= l r)]
    [(map ->bs [x y])]))

;; char F > 3
(def bi biginteger)



(defn add-pt-char>3 [n a b [x1 y1 :as s] [x2 y2 :as r]]
  (let [n (bi n)
        t1 (mod (- y2 y1) n)
        t2 (mod (- x2 x1) n)
        t3 (.modInverse (bi t2) n)
        t4 (mod (* t1 t3) n)
        t5 (.modPow t4 (bi 2) n)
        x3 (mod (- t5 x1 x2) n)
        t6 (mod (- x1 x3) n)
        t7 (mod (* t4 t6) n)
        y3 (mod (- t7 y1) n)]
    [x3 y3]))

(defn dbl-pt-char>3 [n a b [x y :as s]]
  (let [n (bi n)
        t1 (mod (* x x) n)
        t2 (mod (* t1 3) n)
        t3 (mod (+ t2 a) n)
        t4 (mod (* y 2) n)
        t5 (.modInverse (bi t4) n)
        t6 (mod (* t3 t5) n)
        t7 (mod (* t6 t6) n)
        x4 (mod (- t7 (* 2 x)) n)
        y4 (mod (- (* t6 (- x x4)) y) n)]
    [x4 y4]))

(defn pt-sum-char>3 [n a b [x1 y1 :as s] [x2 y2 :as r]]
  (cond (= -1 x1)
        r

        (= -1 x2)
        s

        (not= x1 x2)
        (add-pt-char>3 n a b s r)

        :else
        (cond (= y1 (mod (- y2) n))
              [-1 -1]

              (= y1 y2)
              (dbl-pt-char>3 n a b s)

              :DO_NOT_COMPUTE
              (throw (ex-info "pt-sum err" {:r r :s s})))))

(defn pt-prod-char>3 [n a b r q]
  (nth (iterate #(pt-sum-char>3 n a b % r) r) (dec q)))

(comment
  (pt->ss-ecs 2r11 2r1 2r11001)

  (pt->nss-ecs 2r110 2r1100 2r11001)

  (pt-sum-char>3 11 5 7 [9 0] [9 0])

  (pt-prod-char>3 11 5 7 [9 0] 9)


  (->>
    (for [x (range 29)]
     [x (mod (* x x) 29)])
    (map second) distinct)

  (bit-shift-left (bit-shift-left 3 1) 1)
  (reduce bit-shift-left 3 (range 2))

  (p-mult 2 4)

  (->bs (poly-mult 2r1101 2r1101 2r11001))


  (->bs (poly-mult 2r111 2r111 2r11001))
  (->bs (p-pow 2r101 3 2r11001))


  (->bs (p-inv 2r1101 2r11001))
  (->bs (p-pow 2r1101 14 2r11001))


  (->bs (bit-shift-left (bit-shift-left 2r1101 1) 1))
  (->bs (poly-deg 2r11001))

  (loop [r [2r0001 2r0010] s [2r0001 2r0010]]
    (println (map ->bs r))
    (if (= r [-1 -1])
      :done
      (recur (pt-sum 2r11001 2r10 2r0101 r s) s))
    )

  (map ->bs (pt-sum 2r11001 2r10 2r0101 [2r0010 2r1011] [2r0001 2r0010]))

  (map ->bs (pt-prod 2r11001 2r10 2r101 [2r1 2r10] 9))


  (map ->bs (pt-sum 2r11001 2r10 2r101 [2r11 2r11] [2r1 2r10]))

  (map ->bs (pt-nss-ec->lr 2r110 2r1100 2r11 2r1110 2r11001))

  (nss-ec-pts 2r101 2r110 2r1101)

  (map ->bs (pt-nss-ec->lr 2r1110 2r1001 2r11 2r1110 2r11001))

  ;
  (let [pt [2r1010 2r1110]]
    (loop [r pt s pt i 1]
      (println (map ->bs r))
      (if (or (> i 100) (= r [-1 -1]))
        :done
        (recur (pt-sum-ss 2r11001 2r1110 2r1 2r100 r s) s (inc i)))
      ))

  (ss-ec-pts 2r1110 2r1 2r100 2r11001)

  (map ->bs (pt-ss-ec->lr 2r1111 2r1 2r1110 2r1 2r100 2r11001)))

