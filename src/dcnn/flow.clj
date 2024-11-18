(ns dcnn.flow
  (:require [quil.core :as q]))


(def step 1)
(def num-trajectories 50) ; Number of trajectories to draw
(def trajectory-length 1000) ; Length of each trajectory
(def epsilon 0.001)
(def scale 10)

(defn vector-field [x y]
  [(Math/sin x) (Math/sin y)])

(prn (vector-field 100 100))


(defn draw-forward [x y grad]
  (prn grad)
  (let [[dx dy] grad]
    (if (or (> (Math/abs ^float dx) epsilon)
            (> (Math/abs ^float dy) epsilon))
      (let [new-x (* step dx)
            new-y (* step dy)]
        (print x y new-x new-y)
        (q/line x y new-x new-y)
        (draw-forward new-x new-y (vector-field new-x new-y))))))

(defn draw-backward [x y grad]
  (let [[dx dy] grad]
    (if (or (> (Math/abs ^float dx) epsilon)
            (> (Math/abs ^float dy) epsilon))
      (let [new-x (* step dx -1)
            new-y (* step dy -1)]
        (print x y new-x new-y)
        (q/line (* x scale) (* y scale) (* new-x scale) (* new-y scale))
        (draw-backward new-x new-y (vector-field new-x new-y))))))

(defn draw-trajectory [x y]
  (prn x y)
  (draw-forward x y (vector-field x y))
  (draw-backward x y (vector-field x y)))

(defn setup []
  (q/background 255)
  (q/stroke-weight 2)
  (q/smooth)
  (q/stroke 0 0 255)
  (draw-trajectory 100 100)
  (q/stroke 0 0 255)
  )



(q/defsketch vector-field
             :title "Vector Field Trajectories"
             :setup setup
             :size [800 600])
