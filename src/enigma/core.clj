(ns enigma.core
  (:require [clojure.string :as str]))

(defn plus [n1 n2]
  (mod (+ n1 n2) 26))

(defn minus [n1 n2]
  (mod (- n1 n2) 26))

(defn alph->int [char]
  (- (int char) 65))

(defn int->alph [num]
  (char (+ 65 num)))

(defn kwc->int [kwc]
  (alph->int (first (str/upper-case (name kwc)))))

(def rotor-turnovers [16 4 21 9 25])

(defn gen-rotor [outstr]
  (let [outseq (map alph->int (seq (str/upper-case outstr)))
        correspondence (map vector (range 26) outseq)]
    [(vec outseq) (mapv first (sort-by second correspondence))]))

(def rotors (map gen-rotor ["EKMFLGDQVZNTOWYHXUSPAIBRCJ"
                            "AJDKSIRUXBLHWTMCQGZNPYFVOE"
                            "BDFHJLCPRTXVZNYEIWGAKMUSQO"
                            "ESOVPZJAYQUIRHXLNFTGKDCMWB"
                            "VZBRGITYUPSDNHLXAWMJQOFECK"]))

(def reflectors (map gen-rotor ["EJMZALYXVBWFCRQUONTSPIKHGD"
                                "YRUHQSLDPXNGOKMIEBFZCWVJAT"
                                "FVPJIAOYEDRZXWGCTKUQSBNMHL"]))

(defn rotor-crypt [direction char rotor]
  (minus (plus (nth (nth (nth rotors (:number rotor)) direction)
                    (plus (:position rotor) (minus char (:ringstellung rotor))))
               (:ringstellung rotor))
         (:position rotor)))

(defn rotors-crypt [char rotors reflector]
  (let [forward (reduce (partial rotor-crypt 0) char (reverse rotors))
        reflected (nth (nth (nth reflectors reflector) 0) forward)]
    (reduce (partial rotor-crypt 1) reflected rotors)))

(defn crypt-char [char plugboard rotors reflector]
  (let [rotor-crypted (rotors-crypt (get plugboard char char) rotors reflector)]
    (get plugboard rotor-crypted rotor-crypted)))

(defn step-positions [rotors]
  (reverse (loop [rotors (reverse rotors)
                  adjusted-rotors []
                  increment? true]
             (if (empty? rotors)
               adjusted-rotors
               (let [rotor (first rotors)]
                 (recur (rest rotors)
                        (conj adjusted-rotors
                              (if increment?
                                (assoc rotor :position (plus (:position rotor) 1))
                                rotor))
                        (= (:position rotor) (nth rotor-turnovers (:number rotor)))))))))

;; Example of usage
;;
;; (crypt "GCDSE AHUGW TQGRK VLFGX UCALX VYMIG MMNMF DXTGN VHVRM MEVOU YFZSL RHDRR XFJWC FHUHM UNZEF RDISI KBGPM YVXUZ"
;;        [{:number 2 :ringstellung :X :position :A}
;;         {:number 1 :ringstellung :M :position :B}
;;         {:number 3 :ringstellung :V :position :L}]
;;        :A
;;        {:A :M
;;         :F :I
;;         :N :V
;;         :P :S
;;         :T :U
;;         :W :Z})
;;

(defn crypt [message rotors reflector plugboard]
  (let [plugboard (reduce (fn [acc [c1 c2]]
                            (-> acc
                                (assoc (kwc->int c1) (kwc->int c2))
                                (assoc (kwc->int c2) (kwc->int c1))))
                          {}
                          plugboard)
        _ (println plugboard)
        rotors (map #(assoc % :position (kwc->int (:position %))
                              :ringstellung (kwc->int (get % :ringstellung :A))
                              :number (dec (:number %))) rotors)]
    (loop [chars (map alph->int (seq (str/upper-case (str/replace message #"\s+" ""))))
           ciph []
           rotors rotors]
      (let [rotors (step-positions rotors)]
        (if (empty? chars)
          (apply str (map int->alph ciph))
          (recur (rest chars)
                 (conj ciph (crypt-char (first chars) plugboard rotors (alph->int (first (str/upper-case (name reflector))))))
                 rotors))))))
