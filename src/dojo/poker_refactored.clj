(ns dojo.poker-refactored
  (:require [clojure.string :as str])
  (:gen-class :main true))

(def card-table
  {"T" "10" , "J" "11" , "Q" "12" , "K" "13", "A" "14" , "2" "2"
   "3" "3" , "4" "4" ,"5" "5" , "6" "6" , "7" "7" , "8" "8" , "9" "9"})

(defn parse-card [[n t]]
  {:value (-> n str card-table Integer/parseInt) :suit (str t)})

(defn parse-hand [s]
  (->> (str/split s #" ")
       (map parse-card)
       (sort-by :value)))

(defn group-cards [hand]
  (->> (group-by :value hand)
       (map (comp count second))
       sort reverse))

(defn sorted-group-cards [hand]
  (let [result (group-by :value hand)
        comp-fn (fn [x y] (compare [(count (get result y)) y]
                                  [(count (get result x)) x]))]
    (into (sorted-map-by comp-fn) result)))

(defn same-suit? [hand]
  (= (-> (map :suit hand) distinct count) 1))

(defn match [in not-in]
  {:match true :in-rank in :not-in-rank not-in})

(def no-match (assoc (match [] []) :match false))

(defn flush-hand [hand]
  (if (same-suit? hand)
    (match hand []) no-match))

(defn straight? [hand]
  (let [[f _ _ _ l :as vs] (map :value hand)
        dist (count (distinct vs))]
    (and (= dist 5) (= (- l f) 4))))

(defn straight [hand]
  (if (straight? hand)
    (match hand []) no-match))

(defn straight-flush [hand]
  (if (and (straight? hand) (same-suit? hand))
    (match hand []) no-match))

(defn four-of-a-kind [hand]
  (let [sorted (sorted-group-cards hand)]
       (if (= (count (val (first sorted))) 4)
         (match (val (first sorted)) (val (last sorted))) no-match)))

(defn house [hand]
  (let [[f s] (group-cards hand)]
    (if (and (= f 3) (= s 2))
      (match hand []) no-match)))

(defn two-pair [hand]
  (let [sorted (sorted-group-cards hand)]
    (if (and (= (-> sorted first val count) 2) (= (-> sorted second val count) 2))
      (match (-> (take 2 sorted) vals flatten) (val (last sorted))) no-match)))

(defn one-pair [hand]
  (let [sorted (sorted-group-cards hand)]
    (if (= 2 (count (first sorted)))
      (match (val (first sorted)) (-> sorted rest vals flatten)) no-match)))

(defn three-of-a-kind [hand]
  (let [sorted (sorted-group-cards hand)]
    (if (= (-> sorted first val count) 3)
      (match (val (first sorted)) (-> sorted rest vals flatten)) no-match)))

(def score-fns
  [straight-flush
   four-of-a-kind
   house
   flush-hand
   straight
   three-of-a-kind
   two-pair
   one-pair])

(defn high-card [hand1 hand2]
  (loop [[c1 & m1] (->> hand1 (sort-by :value) reverse (map :value))
         [c2 & m2] (->> hand2 (sort-by :value) reverse (map :value))]
    (cond (or (nil? c1) (nil? c2)) false
          (> c1 c2) :player-1
          (< c1 c2) :player-2
          :else (recur m1 m2))))

(defn resolve-hand [score1 score2]
  (let [in-rank (high-card (:in-rank score1) (:in-rank score2))
        not-in-rank (high-card (:not-in-rank score1) (:not-in-rank score2))]
    (cond in-rank in-rank
          not-in-rank not-in-rank
          :else :tie)))

(defn play [hand1 hand2]
  (let [[fs1 :as score1] (->> (map #(% hand1) score-fns) (drop-while #(false? (:match %))))
        [fs2 :as score2] (->> (map #(% hand2) score-fns) (drop-while #(false? (:match %))))
        c1 (count score1)
        c2 (count score2)]
    (cond (> c1 c2) :player-1
          (< c1 c2) :player-2
          :else (resolve-hand fs1 fs2))))

(defn -main []
  (let [hand1 (do (println "Enter player 1's hand:") (read-line))
        hand2 (do (println "Enter player 2's hand:") (read-line))
        result (play (parse-hand hand1) (parse-hand hand2))]
    (do (cond (= result :player-1) (println "Player 1 won!")
              (= result :player-2) (println "Player 2 won!")
              :else (println "It's a tie!"))
        (println)
        (recur))))
