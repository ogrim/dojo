(ns dojo.gol)

(defn qprint [board]
  (map #(println %) board))

(def blinker
  [[0 0 0 0 0]
   [0 0 1 0 0]
   [0 0 1 0 0]
   [0 0 1 0 0]
   [0 0 0 0 0]])

(def beacon
  [[0 0 0 0 0 0]
   [0 1 1 0 0 0]
   [0 1 1 0 0 0]
   [0 0 0 1 1 0]
   [0 0 0 1 1 0]
   [0 0 0 0 0 0]])

(defn get-n [[x y] world]
  (nth (nth world y) x))

(defn neighbours [x y world]
  [[(dec y) (dec x)]
   [(dec y) x]
   [(dec y) (inc x)]
   [y (dec x)]
   [y (inc x)]
   [(inc y) (dec x)]
   [(inc y) x]
   [(inc y) (inc x)]])

(defn score [[x y] world]
  (let [size (count world)
        cell (get-n [x y] world)]
    [cell (->> (neighbours x y world)
               (filter #(and (-> % first neg? not)
                             (-> % second neg? not)
                             (< (first %) size)
                             (< (second %) size)))
               (map #(get-n % world))
               (apply +))]))

(defn live? [state]
  (case state
    [0 3] 1
    [1 2] 1
    [1 3] 1
    0))

(defn score-board [board]
  (let [size (count board)]
    (->> (for [x (range size) y (range size)]
           (live? (score [x y] board)))
         (partition size)
         (mapv #(into [] %)))))

(def state (atom beacon))

(defn tick []
  (qprint (swap! state score-board)))
