(ns dcnn.dp)


(defn argmax [f args]
  (second (reduce (fn [[m a] i] (let [fi (f i)]
                           (if (or (not m) (> fi m))
                             [fi i] [m a]))) [nil nil] args)))

(defn print-state [state]
  (println "-------")
  (mapv println state)
  (println "-------"))

(defn next-turn [t] (if (= t "x") "o" "x"))

(def empty-state [[" " " " " "]
                 [" " " " " "]
                 [" " " " " "]])

(def states (atom #{}))

(defn assign-state-reward [state]
  (let [idxs [[[0 0] [1 1] [2 2]]
              [[0 2] [1 1] [2 0]]
              [[0 0] [0 1] [0 2]]
              [[1 0] [1 1] [1 2]]
              [[2 0] [2 1] [2 2]]
              [[0 0 [1 0] [2 0]]]
              [[0 1 [1 1] [2 1]]]
              [[0 2 [1 2] [2 2]]]]]
    (cond (some true? (map (fn [idxs] (every? #(= "x" (get-in state %)) idxs)) idxs))
          1

          (some true? (map (fn [idxs] (every? #(= "o" (get-in state %)) idxs)) idxs))
          -1

          :else 0)))

(defn possible-actions [state]
  (for [i (range 3) j (range 3)
        :when (and (= 0 (assign-state-value state)) (= " " (get-in state [i j])))]
    [i j]))

(defn init-states [state turn]
  (swap! states conj state)
  (doseq [[i j] (possible-actions state)]
    (init-states (assoc-in state [i j] turn) (next-turn turn)))
  nil)

(defn take-action [state action turn]
  (assoc-in state action turn))

(def values (atom {}))

(defn init-values [states]
  (mapv (fn [s] (swap! values assoc s 0)) states))

(defn value [state action turn]
  (if (empty? (possible-actions state))
    (assign-state-reward state)
    (let [action (argmax #(value state % turn) (possible-actions state))
          new-state (take-action state action turn)
          next-possible-states (map (fn [action] (take-action new-state action (next-turn turn))
                                      (possible-actions new-state)))]

      (+ (assign-state-reward state)
         (* (/ 1 (count next-possible-states))
            (reduce + (map #(value %))))))))

(comment

  (argmax #(+ (- (* % %)) (* 10 %) 5) (range -10 10 0.1))
  (map println init-state)

  (init-states empty-state "x")

  (assign-state-reward [["o" " " "o"] [" " "o" "x"] ["x" " " "x"]])

  (map (fn [s] (print-state s) (println (assign-state-reward s))) (count @states))
  )