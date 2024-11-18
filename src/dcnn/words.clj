(ns dcnn.words
  (:require [clojure.set :as set]))

(def letters ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j"])

(defn inverse? [x]
  (= "-1" (apply str (rest x))))

(defn inv [x] (if (inverse? x) (str (first x)) (str x "-1")))

(defn inv-s [xs]
  (mapv inv xs))

(defn r [xs]
  (vec (conj (butlast xs) (last xs))))

(defn rn [xs n]
  (assert (int? n))
  (reduce (fn [xs _] (r xs)) xs (range (mod n (count xs)))))

(defn s [xs]
  (-> xs reverse r inv-s))

(defn sn [xs n]
  (assert (int? n))
  (rn (s xs) n))




(comment
  (sn ["a" "b" "c" "d" "e" "f"] 2)
  (rn ["a" "b" "c" "d" "e" "f"] 2)


  (sn ["a" "b" "c" "d" "e" "f"] 0)
  (rn (s ["a" "b" "c" "d"]) 1)



  (rn ["a" "b" "c" "d" "e" "f" "g" "h"] 4)
  (rn ["a" "b" "c" "d" "e" "f" "g" "h"] 1)

  (sn ["a" "b" "c" "d"] 1)

  (sn ["a" "b" "c" "d" "e" "f"] 1)

  (sn ["a" "b" "c" "d" "e" "f" "g" "h"] 0))

; <r,s: r^6 = s^2 = e, srs = r^-1>
(assert (= ["a" "b" "c" "d" "e" "f"] (rn ["a" "b" "c" "d" "e" "f"] 6)))
(assert (= ["a" "b" "c" "d" "e" "f"] (-> ["a" "b" "c" "d" "e" "f"] s s)))
(assert (= (-> ["a" "b" "c" "d" "e" "f"] s r s) (rn ["a" "b" "c" "d" "e" "f"] -1)))



(defn glued-edge [word i]
  (let [edge (get word i)
        orientable? (seq (filter (partial = (inv edge)) word))
        non-orientable? (= 2 (count (filter (partial = edge) word)))
        glued? (or orientable? non-orientable?)]
    (if glued?
      (let [j (if orientable?
                (.indexOf word (inv edge))
                (.indexOf (assoc word i nil) edge))]
        [orientable? j])
      [nil nil])))

(defn relabel [word]
  (let [new-word (into [] (repeat (count word) nil))]
   (first
     (reduce (fn [[new-word i new-index] _]
               (if (get new-word i)
                 [new-word (inc i) new-index]
                 (let [[orientable? j] (glued-edge word i)
                       new-letter (get letters new-index)]
                   (if j
                     [(-> new-word
                          (assoc i new-letter)
                          (assoc j (if orientable? (inv new-letter) new-letter)))
                      (inc i) (inc new-index)]
                     [(assoc new-word i new-letter)
                      (inc i) (inc new-index)]))))
             [new-word 0 0] word))))

(defn glue [word e1 e2 orientable?]
  (assert (not= e1 e2))
  (assert (not (contains? word (inv e1))))
  (assert (not (contains? word (inv e2))))
  (assert (= 1 (count (filter (partial = e1) word))))
  (assert (= 1 (count (filter (partial = e2) word))))
  (let [i1 (.indexOf word e1)
        i2 (.indexOf word e2)
        lower-idx (if (> i1 i2) i2 i1)
        higher-idx (if (< i1 i2) i2 i1)]
    (relabel (assoc word higher-idx (if orientable? (inv (get word lower-idx)) (get word lower-idx))))))

(defn glue [word e1 e2 orientable?]
  (assert (not= e1 e2))
  (assert (not (contains? word (inv e1))))
  (assert (not (contains? word (inv e2))))
  (assert (= 1 (count (filter (partial = e1) word))))
  (assert (= 1 (count (filter (partial = e2) word))))
  (let [i1 (.indexOf word e1)
        i2 (.indexOf word e2)
        lower-idx (if (> i1 i2) i2 i1)
        higher-idx (if (< i1 i2) i2 i1)]
    (relabel (assoc word higher-idx (if orientable? (inv (get word lower-idx)) (get word lower-idx))))))




(defn =up-to-dn [w1 w2]
  (assert (= (count w1) (count w2)))
  (assert (even? (count w1)))
  (let [n (count w1)]
    (reduce (fn [_ i]
              (if (or (= w1 (relabel (rn w2 i)))
                      (= w1 (relabel (rn (sn w2 0) i))))
                (reduced true)
                false)) nil (range n))))

(defn =up-to-cn [w1 w2]
  (assert (= (count w1) (count w2)))
  (assert (even? (count w1)))
  (let [n (count w1)]
    (reduce (fn [_ i]
              (if (= w1 (relabel (rn w2 i)))
                (reduced true)
                false)) nil (range n))))

(defn symmetries [w]
  (let [n (count w)]
    (concat
      (for [i (range n)
            :when (= w (relabel (rn w i)))]
        (rn w i))
      (for [i (range n)
            :when (= w (relabel (sn w i)))]
        (sn w i)))))


(defn glue-next [word glued-idx all equivalence]
  (let [free-idx (-> (range (count word)) set (set/difference glued-idx))]
    (mapv (fn [i] (let [free-idx (set/difference free-idx #{i})]
                    (mapv (fn [j]
                            (let [e1 (get word i)
                                  e2 (get word j)
                                  o (glue word e1 e2 true)
                                  no (glue word e1 e2 false)]
                              (when-not (some (partial equivalence o) @all)
                                (swap! all conj o)
                                (glue-next o (set/union glued-idx #{i j}) all equivalence))
                              (when-not (some (partial equivalence no) @all)
                                (swap! all conj no)
                                (glue-next no (set/union glued-idx #{i j}) all equivalence))))
                          free-idx))) free-idx)))

(defn all-polygons
  ([n] (all-polygons n =up-to-dn))
  ([n equivalence]
   (let [initial (into [] (take n letters))
         all (atom [])]
     (mapv (fn [i]
             (let [e1 (get initial 0)
                   e2 (get initial i)
                   o (glue initial e1 e2 true)
                   no (glue initial e1 e2 false)]
               (when-not (some (partial equivalence o) @all)
                 (swap! all conj o)
                 (glue-next o #{0 i} all equivalence))
               (when-not (some (partial equivalence no) @all)
                 (swap! all conj no)
                 (glue-next no #{0 i} all equivalence))))
           (range 1 n))
     @all)))

(def all-polygons (memoize all-polygons))

(defn word->latex [labels]
  (apply str (map (fn [l] (if (inverse? l)
                            (format "%s^{-1} " (inv l))
                            (format "%s " l))) labels) ))




(comment



  (symmetries ["a" "a-1" "b" "b-1" "c" "c-1" "d" "d-1"])

  (count (all-polygons 10 =up-to-dn))
  (count (all-polygons 8 =up-to-dn))
  (all-polygons 4 =up-to-cn)
  (all-polygons 4 =)

  (some (partial =up-to-dn ["a" "b"]) [])

  (rn ["a" "b" "a-1" "c" "d" "c-1" "d-1" "b-1"] -1)

  (inv-s ["a" "b" "a-1" "d" "e" "f-1" "g" "f-1"])
  (word->latex ["a-1" "b-1" "a" "d-1" "e-1" "f" "g-1" "f"])
  (word->latex (relabel ["a-1" "b-1" "a" "d-1" "e-1" "f" "g-1" "f"]))

  (=up-to-dn ["a" "b" "a" "c" "d" "e"] ["a" "b" "c" "b" "d" "e"])

  (=up-to-dn ["a" "b" "a" "c" "d" "e"] ["a" "b" "c" "d" "b" "e"])

  (relabel (r ["a" "b" "c" "b" "d" "e"]))

  (relabel ["a" "b" "c" "b" "d" "e"])
  (relabel (rn ["a" "b" "c" "b" "d" "e"] 5))
  )