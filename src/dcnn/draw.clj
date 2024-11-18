(ns dcnn.draw
  (:require [dcnn.words :as w]
            [clojure.set :as set])
  (:import (java.util Locale)))

(def vertices ["A" "B" "C" "D" "E" "F" "G" "H" nil])

(def colors ["black" "red" "blue" "green" "cyan" "magenta" "yellow" "violet" "olive" "orange" "purple" "brown"])

(Locale/setDefault Locale/US)

(defn adjacent-edges [n word]
  [n (mod (dec n) (inc (count word)))])

(defn next-v [vs]
  (get vertices (->> vs (keep identity) set count)))

(defn glue-vertices [i1 i2 vs]
  (let [v1 (get vs i1)
        v2 (get vs i2)]
    (cond (and v1 v2)
          (let [[new_v replace_v] (if (> (.indexOf vertices v1) (.indexOf vertices v2)) [v2 v1] [v1 v2])]
            (mapv (fn [v] (if (= v replace_v) new_v v)) vs))
          v1 (assoc vs i2 v1)
          v2 (assoc vs i1 v2)
          :else (assoc vs i1 (next-v vs) i2 (next-v vs)))))

(declare update-adjacent)

(defn update-v [j first? i word vs]
  (let [v1_idx (if first? j (mod (inc j) (count word)))
        v1 (get vs v1_idx)]
    (if v1
      (glue-vertices i v1_idx vs)
      (update-adjacent
        (glue-vertices i v1_idx vs)
        v1_idx word))
    ))

(defn update-adjacent [vs i word]
  (let [[i1 i2] (adjacent-edges i word)
        [o1? j1] (w/glued-edge word i1)
        [o2? j2] (w/glued-edge word i2)]
    (if-not (or j1 j2)
      (assoc vs i (next-v vs))
     (cond->> vs
              j1 (update-v j1 (not o1?) i word)
              j2 (update-v j2 o2? i word)))))

(defn cv* [vs i word]
  (if (every? identity vs)
    vs
    (if (get vs i)
      (cv* vs (inc i) word)
      (cv* (update-adjacent vs i word)
           (inc i) word)
      )))

(defn cv [word]
  (let [n (count word)
        vs (into [] (repeat n nil))]
    (cv* vs 0 word)))

(defn euler-characteristic [word]
  (let [v (-> (cv word) distinct count)
        e (-> (map (fn [e] (if (w/inverse? e) (w/inv e) e)) word) distinct count)]
    (- v e -1)))


(defn free-edges [word]
  (let [vs (cv word)]
    (second (reduce (fn [[i g] e]
                      (if e
                        [(inc i) (conj g (set [(get vs i) (get vs (mod (inc i) (count vs)))]))]
                        [(inc i) g])) [0 []] (map-indexed (fn [i e] (if (second (w/glued-edge word i)) nil e)) word)))))


(defn get-boundary [g b]
  (if (and (seq g) (some (fn [e] (seq (set/intersection b e))) g))
    (apply get-boundary (reduce (fn [[g b] e]
                             (if (empty? (set/intersection b e))
                               [(conj g e) b]
                               [g (set/union b e)])) [[] b] g))
    b))

(defn bc* [components g]
  (let [b (get-boundary g (first g))
        b-rest (filter (fn [e] (empty? (set/intersection b e))) g)]
    (if (seq b-rest)
      (bc* (conj components b) b-rest)
      (conj components b))))

(defn bc [word]
  (let [g (free-edges word)]
    (if (seq g)
     (bc* [] g)
     [])))

(defn orientable? [word]
  (= (count word) (count (distinct word))))


(defn hex-from-quads [w]
  (let [vs (cv w)
        n (count w)]
    (first
     (reduce (fn [[es sym] i]
               (let [v1 i
                     v2 (mod (+ i 3) (count w))]
                 (if (some (fn [s]
                             (or (= s (w/relabel (w/rn w i)))
                                 (= s (w/relabel (w/rn w (+ i 3))))
                                 (= s (w/relabel (w/sn w (dec i))))
                                 (= s (w/relabel (w/sn w (+ i 2))))))
                           sym)
                   [es sym]
                   [(conj es {(set [(get vs v1) (get vs v2)])
                              [v1 v2]})
                    (conj sym (w/rn w i))]
                   ))) [{} []] (range (/ (count w) 2))))))

(defn oct-from-quads [w]
  (let [vs (cv w)]
    (first
     (reduce (fn [[es [s1 s2]] i]
               (let [v1 i
                     v2 (mod (+ i 3) (count w))
                     v3 (mod (+ i 5) (count w))
                     v4 (mod (+ i 4) (count w))
                     v5 (mod (+ i 7) (count w))

                     s1? (some (fn [s]
                                 (or (= s (w/relabel (w/rn w i)))
                                     (= s (w/relabel (w/rn w (+ i 4))))
                                     (= s (w/relabel (w/sn w (dec i))))
                                     (= s (w/relabel (w/sn w (+ i 3))))))
                               s1)
                     s2? (some (fn [s]
                                 (or (= s (w/relabel (w/rn w i)))
                                     (= s (w/relabel (w/sn w (dec i))))))
                               s2)]
                 [(cond-> es
                          (not s1?) (conj {(set [(set [(get vs v1) (get vs v2)]) (set [(get vs v1) (get vs v3)])])
                                           [[v1 v2] [v1 v3]]})
                          (not s2?) (conj {(set [(set [(get vs v1) (get vs v2)]) (set [(get vs v4) (get vs v5)])])
                                           [[v1 v2] [v4 v5]]}))
                  [(if-not s1? (conj s1 (w/rn w i)) s1)
                   (if-not s2? (conj s2 (w/rn w i)) s2)]]))
             [{} []] (range (/ (count w) 2))))))

(comment

  (summary-glued (read-string (slurp "d8.edn")))

  (w/relabel (w/sn ["a" "a-1" "b" "b-1" "c" "c-1" "d" "d-1"] 5))



  (cv ["a" "a-1" "b" "b-1" "c" "c-1"])
  (cv ["a" "b" "a" "b"])
  (cv ["a" "b" "a-1" "b-1"])
  (euler-characteristic ["a" "b" "a-1" "b-1"])
  (euler-characteristic ["a" "a-1" "b" "b-1" "c" "c-1"])
  (euler-characteristic ["a" "b" "a-1" "c"])
  (boundary-components ["a" "b" "a-1" "c"])

  (boundary-components ["a" "b" "a" "c" "d" "e" "d" "f"])
  (print (octagon ["a" "b" "a" "c" "d" "e" "d" "f"]))


  (bc ["a" "b" "a" "c" "d" "e" "f"])
  (bc ["a" "b" "a-1" "c" "d" "e" "d-1" "f"])
  (bc ["a" "b" "a-1" "c"])
  (bc ["a" "b" "a" "c"])

  (hex-from-quads ["a" "b" "a" "c" "d" "d"])

  (cv ["a" "b" "a-1" "c"])

  )


(defn octagon
  ([labels] (octagon labels 1))
  ([labels scale]
   (let [locations (mapv #(mapv (partial * scale) %) [[0 6] [2 6] [4 4] [4 2] [2 0] [0 0] [-2 2] [-2 4]])
         label-pos ["above" "above right" "right" "below right" "below" "below left" "left" "above left"]
         vls (cv labels)
         es (oct-from-quads labels)]
     (str "\\begin{figure}\n"
          (format
            "\\begin{tikzpicture}[>={Latex[width=%.3fmm,length=%.3fmm]},
                                node distance = %.3fcm and %.3fcm,
                                el/.style = {inner sep=2pt, align=center, sloped},
                                every label/.append style = {font=\\tiny}]\n"
            (* scale 2) (* scale 3) (* scale 3) (* scale 4))
          (apply str
                 (for [i (range 8)
                       :let [v (get vertices i)
                             [x y] (get locations i)
                             vl (get vls i)]]

                   (format "\\node[shape=circle,draw=black,scale=%.3f] (%s) at (%.3f,%.3f) {%s};\n" (double scale) v x y vl)))
          "\n\n"
          (apply str
                 (for [i (range 8)
                       :let [v1 (get vertices i)
                             v2 (get vertices (mod (inc i) 8))
                             l (get labels i)
                             inv? (w/inverse? l)
                             pos (get label-pos i)]]
                   (format "\\path [%s](%s) edge node[%s] {%s} (%s);\n"
                           (if inv?
                             "<-" "->") v1 pos (if inv? (w/inv l) l) v2)))

          (apply str
                 (map-indexed (fn [i [_ [e1 e2]]]
                                (let [[v1 v2] (if (= 1 (count e1))
                                                [(first e1) (first e1)]
                                                (vec e1))
                                      [v3 v4] (if (= 1 (count e2))
                                                [(first e2) (first e2)]
                                                (vec e2))]
                                  (str (format "\\path [-](%s) edge[color=%s] node {} (%s);\n"
                                               (get vertices v1) (get colors i) (get vertices v2))
                                       (format "\\path [-](%s) edge[color=%s] node {} (%s);\n"
                                               (get vertices v3) (get colors i) (get vertices v4)))))
                              es))

          (comment
            (let [eul (euler-characteristic labels)
                 boundary (count (bc labels))
                 o? (orientable? labels)
                 genus (if o?
                         (/ (- 2 eul boundary) 2)
                         (- 2 eul boundary))]
             (str (format "\\node[text width=6cm, anchor=west, right] at (5,4) {$\\rchi = %s $};\n" eul)
                  (format "\\node[text width=6cm, anchor=west, right] at (5,3) {Number of boundary components: %s};\n" boundary)
                  (format "\\node[text width=6cm, anchor=west, right] at (5,2) {Orientable: %s};\n" o?)
                  (format "\\node[text width=6cm, anchor=west, right] at (5,1) {Genus: %s};\n" genus))))


          "\\end{tikzpicture}\n"
          (str "\\caption{$" (apply str (map (fn [l] (if (w/inverse? l)
                                                       (format "%s^{-1} " (w/inv l))
                                                       (format "%s " l))) labels))

               "$}\n")
          "\\end{figure}\n"
          "\\clearpage \n")))

  )

(defn hexagon
  ([labels] (hexagon labels 1))
  ([labels scale]
   (let [locations (mapv #(mapv (partial * scale) %) [[0 4] [2 4] [3.5 2] [2 0] [0 0] [-1.5 2]])
         label-pos ["above" "above right" "below right" "below" "below left" "above left"]
         vls (cv labels)
         es (hex-from-quads labels)]
     (str "\\begin{figure}\n"
          "\\begin{tikzpicture}[>={Latex[width=2mm,length=3mm]},
                                node distance = 3cm and 4cm,
                                el/.style = {inner sep=2pt, align=center, sloped},
                                every label/.append style = {font=\\tiny}]\n"
          (apply str
                 (for [i (range 6)
                       :let [v (get vertices i)
                             [x y] (get locations i)
                             vl (get vls i)]]

                   (format "\\node[shape=circle,draw=black] (%s) at (%s,%s) {%s};\n" v (str x) (str y) vl)))
          "\n\n"
          (apply str
                 (for [i (range 6)
                       :let [v1 (get vertices i)
                             v2 (get vertices (mod (inc i) 6))
                             l (get labels i)
                             inv? (w/inverse? l)
                             pos (get label-pos i)
                             slice (set (take i labels))
                             glued? (or (slice l) (slice (w/inv l)))]]
                   (format "\\path [%s](%s) edge node[%s] {%s} (%s);\n"
                           (if inv?
                             "<-" "->") v1 pos (if inv? (w/inv l) l) v2)))

          (apply str
                 (map-indexed (fn [i [_ v]]
                                (let [[v1 v2] (if (= 1 (count v))
                                                [(first v) (first v)]
                                                (vec v))]
                                  (format "\\path [-](%s) edge[color=%s] node {} (%s);\n"
                                          (get vertices v1) (get colors i) (get vertices v2))))
                              es))
          (comment (let [eul (euler-characteristic labels)
                         boundary (count (bc labels))
                         o? (orientable? labels)
                         genus (if o?
                                 (/ (- 2 eul boundary) 2)
                                 (- 2 eul boundary))]
                     (str (format "\\node[text width=6cm, anchor=west, right] at (5,4) {$\\rchi = %s $};\n" eul)
                          (format "\\node[text width=6cm, anchor=west, right] at (5,3) {Number of boundary components: %s};\n" boundary)
                          (format "\\node[text width=6cm, anchor=west, right] at (5,2) {Orientable: %s};\n" o?)
                          (format "\\node[text width=6cm, anchor=west, right] at (5,1) {Genus: %s};\n" genus))))


          "\\end{tikzpicture}\n"
          (str "\\caption{$" (apply str (map (fn [l] (if (w/inverse? l)
                                                       (format "%s^{-1} " (w/inv l))
                                                       (format "%s " l))) labels))

               "$}\n")
          "\\end{figure}\n")))

  )

(defn quadrilateral [labels]
  (let [locations [[0 1.8] [1.8 1.8] [1.8 0] [0 0]]
        label-pos ["above" "right" "below" "left"]
        vls (cv labels)]
    (str "\\begin{subfigure}{.18\\linewidth}\n"
         "\\centering\n"
         "\\begin{tikzpicture}[>={Latex[width=1mm,length=1.5mm]},
                               node distance = 1.5cm and 2cm,
                               el/.style = {inner sep=2pt, align=center, sloped},
                               every label/.append style = {font=\\tiny}]\n"
         (apply str
                (for [i (range 4)
                      :let [v (get vertices i)
                            [x y] (get locations i)
                            vl (get vls i)]]

                  (format "\\node[shape=circle,draw=black,scale=0.5] (%s) at (%s,%s) {%s};\n" v (str x) (str y) vl)))
         "\n\n"
         (apply str
                (for [i (range 4)
                      :let [v1 (get vertices i)
                            v2 (get vertices (mod (inc i) 4))
                            l (get labels i)
                            inv? (w/inverse? l)
                            pos (get label-pos i)
                            slice (set (take i labels))
                            glued? (or (slice l) (slice (w/inv l)))]]
                  (format "\\path [%s](%s) edge node[%s] {%s} (%s);\n"
                          (if inv?
                            "<-" "->") v1 pos (if inv? (w/inv l) l) v2)))

         (comment
           (let [eul (euler-characteristic labels)
                boundary (count (bc labels))
                o? (orientable? labels)
                genus (if o?
                        (/ (- 2 eul boundary) 2)
                        (- 2 eul boundary))]
            (str (format "\\node[text width=6cm, anchor=west, right] at (5,4) {$\\rchi = %s $};\n" eul)
                 (format "\\node[text width=6cm, anchor=west, right] at (5,3) {Number of boundary components: %s};\n" boundary)
                 (format "\\node[text width=6cm, anchor=west, right] at (5,2) {Orientable: %s};\n" o?)
                 (format "\\node[text width=6cm, anchor=west, right] at (5,1) {Genus: %s};\n" genus))))


         "\\end{tikzpicture}\n"
         (str "\\caption{$"(apply str (map (fn [l] (if (w/inverse? l)
                                                     (format "%s^{-1} " (w/inv l))
                                                     (format "%s " l))) labels))

              "$}\n")
         "\\end{subfigure}\n \\hfill \n"))

  )

(defn summary [words]
  (reduce (fn [cl w]
            (let [eul (euler-characteristic w)
                  boundary (count (bc w))
                  o? (orientable? w)
                  vertices (-> (cv w) distinct count)
                  edges (-> (map (fn [e] (if (w/inverse? e) (w/inv e) e)) w) distinct count)
                  genus (if o?
                          (/ (- 2 eul boundary) 2)
                          (- 2 eul boundary))
                  key {:euler-ch              eul
                       :orientable          o?
                       :genus genus
                       :boundary-components boundary}]
              (if (cl key)
                (update cl key inc)
                (assoc cl key 1))))
          {} words))

(defn summary-glued [words]
  (reduce (fn [cl w]
            (let [n (count w)
                  quad-glueings (if (= n 8)
                                  (count (oct-from-quads w))
                                  (count (hex-from-quads w)))
                  eul (euler-characteristic w)
                  boundary (count (bc w))
                  vertices (-> (cv w) distinct count)
                  edges (-> (map (fn [e] (if (w/inverse? e) (w/inv e) e)) w) distinct count)
                  o? (orientable? w)
                  genus (if o?
                          (/ (- 2 eul boundary) 2)
                          (- 2 eul boundary))
                  key {:euler-ch            eul
                       :orientable          (if o? "t" "f")
                       :boundary-components boundary}]
              (if (cl key)
                (update cl key + quad-glueings)
                (assoc cl key quad-glueings))))
          {} words))



(defn summary->latex [data]
  (let [header {:euler-ch "$\\chi$"
                :orientable "orientable"
                :boundary-components "boundary components"
                :n "n"}]
    (str "\\begin{table}[h!]\n"
         (format "\\begin{tabular}{%s}\n" (apply str "|c|" (repeat (count data) "c|")))
         "\\hline\n"
         (apply str
                (for [[k v] header]
                  (str v " & " (apply str (interpose " & " (if (= :n k) (vals data) (map k (keys data))))) "\\\\ \n \\hline \n")))
         "\\end{tabular}\n"
         "\\end{table}\n")))


(defn hexagon2 [labels shift]
  (let [locations (mapv (fn [[a b]] [(+ a shift) b]) [[0 2] [1 2] [1.75 1] [1 0] [0 0] [-0.75 1]])
        label-pos ["above" "above right" "below right" "below" "below left" "above left"]
        vls (cv labels)
        es (hex-from-quads labels)]
    (str
         "\\begin{tikzpicture}[>={Latex[width=1mm,length=1.5mm]},
                               node distance = 1.5cm and 2cm,
                               el/.style = {inner sep=1pt, align=center, sloped},
                               every label/.append style = {font=\\tiny}]\n"
         (apply str
                (for [i (range 6)
                      :let [v (get vertices i)
                            [x y] (get locations i)
                            vl (get vls i)]]

                  (format "\\node[shape=circle,draw=black, scale=0.5] (%s) at (%s,%s) {%s};\n" v (str x) (str y) vl)))
         "\n\n"
         (apply str
                (for [i (range 6)
                      :let [v1 (get vertices i)
                            v2 (get vertices (mod (inc i) 6))
                            l (get labels i)
                            inv? (w/inverse? l)
                            pos (get label-pos i)
                            slice (set (take i labels))
                            glued? (or (slice l) (slice (w/inv l)))]]
                  (format "\\path [%s](%s) edge node[%s] {%s} (%s);\n"
                          (if inv?
                            "<-" "->") v1 pos (if inv? (w/inv l) l) v2)))

         "\\end{tikzpicture}\n"
         ))

  )

(comment

  (print (hexagon ["a" "b" "a-1" "b-1" "c" "c"] 0.5))

  (print (summary->latex (summary-glued (read-string (slurp "d8.edn")))))



  (let [d4 (read-string (slurp "d4.edn"))
        c4 (read-string (slurp "c4.edn"))
        =4 (read-string (slurp "=4.edn"))
        d6 (read-string (slurp "d6.edn"))
        c6 (read-string (slurp "c6.edn"))
        =6 (read-string (slurp "=6.edn"))
        d8 (read-string (slurp "d8.edn"))
        c8 (read-string (slurp "c8.edn"))
        =8 (read-string (slurp "=8.edn"))]
    (spit "summary2.edn"
          (pr-str
           {:c4 {:count   (count c4)
                 :summary (summary c4)}
            :d4 {:count   (count d4)
                 :summary (summary d4)}
            :=4 {:count   (count =4)
                 :summary (summary =4)}
            :d6 {:count         (count d6)
                 :summary       (summary d6)
                 :summary-glued (summary-glued d6)}
            :c6 {:count         (count c6)
                 :summary       (summary c6)
                 :summary-glued (summary-glued c6)}
            :=6 {:count         (count =6)
                 :summary       (summary =6)
                 :summary-glued (summary-glued =6)}
            :d8 {:count         (count d8)
                 :summary       (summary d8)
                 :summary-glued (summary-glued d8)}
            :c8 {:count         (count c8)
                 :summary       (summary c8)
                 :summary-glued (summary-glued c8)}
            :=8 {:count         (count =8)
                 :summary       (summary =8)
                 :summary-glued (summary-glued =8)}})))


  (.indexOf (read-string (slurp "d8.edn")) ["a" "a-1" "b" "b-1" "c" "c-1" "d" "d-1"])

  (spit "./diags" (apply str (mapv #(octagon % 0.5) (filter (fn [w] (and (orientable? w)
                                                                         (= -2 (euler-characteristic w))
                                                                         (= 0 (count (bc w)))))
                                                            (read-string (slurp "d8.edn"))))))

  (spit "./diags" (apply str (mapv quadrilateral (count (read-string (slurp "d4.edn"))))))

  (spit "./diags" (apply str (mapv hexagon2 (let [w ["a" "b" "c" "d" "e" "f"]]
                                             [w (w/rn w 3) (w/sn w -1) (w/sn w 2)]))))

  (spit "./diags" (octagon ["a" "b" "c" "d" "e" "f" "g" "h"] 0.5))


  (read-string (slurp "summary.edn")))