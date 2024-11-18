(ns dcnn.cipher
  (:require [clojure.string :as str]))

(def c "UXENRBWXCUXENFQRLQJUCNABFQNWRCJUCNAJCRXWORWMB")

(def c2 "KZRNK GJKIP ZBOOB XLCRG BXFAU GJBNG RIXRU XAFGJ BXRME
  MNKNG BURIX KJRXR SBUER ISATB UIBNN RTBUM NBIGK EBIGR
  OCUBR GLUBN JBGRL SJGLN GJBOR ISLRS BAFFO AZBUN RFAUS
  AGGBI NGLXM IAZRX RMNVL GEANG CJRUE KISRM BOOAZ GLOKW
  FAUKI NGRIC BEBRI NJAWB OBNNO ATBZJ KOBRC JKIRR NGBUE
  BRINK XKBAF QBROA LNMRG MALUF BBG")

(def alpha "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn caesar [c k]
  (let [alpha (vec alpha)]
    (transduce (map (fn [a]
                      (let [i (.indexOf alpha a)]
                        (get alpha (mod (+ i k) 26)))))
               str c)))

(defn substitution [c m]
  (transduce (map (fn [a] (if (m a) (m a) a))) str c))


(comment

  (let [ct (str/replace c2 #"\s+" "")
        bi1 (partition 2 ct)
        bi2 (partition 2 (rest ct))
        bigrams (concat bi1 bi2)
        tri1 (partition 3 ct)
        tri2 (partition 3 (rest ct))
        tri3 (partition 3 (rest (rest ct)))
        trigrams (concat tri1 tri2 tri3)
        f1 (partition 4 ct)
        f2 (partition 4 (rest ct))
        f3 (partition 4 (-> ct rest rest))
        f4 (partition 4 (-> ct rest rest rest))
        f (concat f1 f2 f3 f4)]
    ;(->> (frequencies trigrams) seq (sort-by second))
    (->> (frequencies f) seq (sort-by second))
    ;ct
    ;(->> (frequencies ct) seq (sort-by second))
    ;(->> (frequencies bigrams) seq (sort-by second))
    )





  (frequencies c2)
  (map #(vector % (caesar c %)) (range 26))
  (caesar c 17); => "LOVEISNOTLOVEWHICHALTERSWHENITALTERATIONFINDS"


  (substitution c2 {\N \t \G \h \R \e \B \a \A \n \I \r \C \r \L \i \U \d \X \p \O \l})
  (substitution c2 {\G \t \J \h \B \e \R \a \N \s \I \n \F \f \A \o \U \r})
  (substitution c2 {\G \t \J \h \B \e \R \a \N \s \I \n \F \f \A \o \U \r \O \l \Z \w \K \i \E \m \P \k \X \d \L \u \M \y \S \g \T \v \W \p \V \b \Q \j \C \c})
  ;=>
  "iwasi think welle ducat edfor thest andar dofth edaym
   ysist erand ihada germa ngove rness avery senti menta
   lcrea tures hetau ghtus thela nguag eoffl owers aforg
   otten study nowad aysbu tmost charm ingay ellow tulip
   forin stanc emean shope lessl ovewh ileac hinaa sterm
   eansi dieof jealo usyat yourf eet"


  ; letters frequencies
  ([\P 1] [\Q 1] [\V 1] [\W 2] [\T 3] [\C 5] [\Z 6] [\E 7] [\S 7] [\F 8] [\M 8] [\L 10]
   [\X 10] [\J 11] [\O 12] [\K 13] [\U 14] [\A 16] [\I 16] [\N 20] [\G 22] [\R 28] [\B 32])

  ; bigrams
  ([(\I \X) 2] [(\N \N) 2] [(\B \A) 2] [(\Z \B) 2] [(\R \O) 2] [(\N \R) 2] [(\A \T) 2] [(\J \R) 2]
   [(\S \B) 2] [(\L \U) 2] [(\X \K) 2] [(\I \G) 2] [(\U \B) 2] [(\R \S) 2] [(\A \L) 2] [(\B \G) 2]
   [(\N \J) 2] [(\R \N) 2] [(\O \O) 2] [(\Z \R) 2] [(\R \X) 2] [(\S \A) 2] [(\C \J) 2] [(\L \N) 2]
   [(\R \U) 2] [(\M \N) 3] [(\U \E) 3] [(\E \B) 3] [(\A \Z) 3] [(\J \K) 3] [(\T \B) 3] [(\B \I) 3]
   [(\I \S) 3] [(\N \K) 3] [(\R \M) 3] [(\O \B) 3] [(\A \F) 3] [(\F \A) 3] [(\R \G) 3] [(\B \X) 3]
   [(\A \U) 3] [(\B \O) 4] [(\K \I) 4] [(\X \R) 4] [(\G \L) 4] [(\I \N) 4] [(\J \B) 4] [(\G \B) 4]
   [(\G \J) 4] [(\O \A) 4] [(\B \N) 4] [(\G \R) 4] [(\B \R) 5] [(\B \U) 6] [(\N \G) 7] [(\R \I) 7])

  ; trigrams
  ([(\J \K \I) 2] [(\O \A \Z) 2] [(\T \B \U) 2] [(\A \T \B) 2] [(\N \G \R) 2] [(\R \I \X) 2]
   [(\B \I \G) 2] [(\X \R \M) 2] [(\B \U \E) 2] [(\G \B \U) 2] [(\B \R \I) 2] [(\G \R \I) 2]
   [(\R \X \R) 2] [(\R \I \N) 2] [(\I \N \G) 2] [(\R \S \B) 2] [(\B \N \N) 2] [(\B \O \O) 2]
   [(\E \B \R) 2] [(\R \I \S) 2] [(\B \A \F) 2] [(\N \G \B) 2] [(\F \A \U) 3] [(\G \J \B) 3])

  ; quadrigrams
  ( [(\E \B \R \I) 2] [(\N \G \R \I) 2] [(\B \R \I \N) 2] [(\N \G \B \U) 2])

  ; english text frequencies reference
  ; https://www3.nd.edu/~busiforc/handouts/cryptography/Letter%20Frequencies.html

  "i was i think well educated for the standard of the day my sister and i
 had a german governess a very sentimental creature she taught us the language of flowers
 a forgotten study nowadays but most charming a yellow tulip for instance means hopeless
 love while a china aster means i die of jealousy at your feet"
  )