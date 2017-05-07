(ns game-of-life.core
  (:use clojure.pprint))



(defn empty-board
  "Creates a rectangular empty board of the given width and height."
  [w h]
  (vec (repeat w (vec (repeat h nil)))))

(defn populate
  "Turns :on each of the cells specified as [y, x] coordinates"
  [board living-cells]
  (reduce (fn [board coordinates]
            (assoc-in board coordinates :on))
          board
          living-cells))

(defn neighbors
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))

(defn count-neighbors
  [board loc]
  (count (filter #(get-in board %) (neighbors loc))))

(defn indexed-step
  "Yields the next state of the board, using indices to determine neighbors"
  [board]
  (let [w (count board)
        h (count (first board))]
    (loop [new-board board
           x 0
           y 0]
      (cond
        (>= x h) new-board
        (>= y h) (recur new-board (inc x) 0)
        :else
          (let [new-liveness
                (case (count-neighbors board [x y])
                  2 (get-in board [x y])
                  3 :on
                  nil)]
            (recur (assoc-in new-board [x y] new-liveness) x (inc y)))))))


;; replace loops with a nested reduce call
(defn indexed-step-with-reduce
  "Yields the next state of the board, using indices to determine neighbors"
  [board]
  (let [w (count board)
        h (count (first board))]
    (reduce
      (fn [new-board x]
        (reduce
          (fn [new-board y]
            (let [new-liveness
                  (case (count-neighbors board [x y])
                    2 (get-in board [x y])
                    3 :on
                    nil)]
              (assoc-in new-board [x y] new-liveness)))
          new-board (range h)))
      board (range w))))

(defn indexed-step-with-reduce-collapsed
  [board]
  (let [w (count board)
        h (count (first board))]
    (reduce
      (fn [new-board [x y]]
        (let [new-liveness
              (case (count-neighbors board [x y])
                2 (get-in board [x y])
                3 :on
                nil)]
          (assoc-in new-board [x y] new-liveness)))
      board (for [x (range w) y (range h)] [x y]))))



(def glider (populate (empty-board 6 6) #{[2 0] [2 1] [2 2] [1 2] [0 1]}))
(-> (iterate indexed-step glider) (nth 8) pprint)

