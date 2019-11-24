(ns intro-to-ec.game-2048)

"Setup start state"
(def full-board [2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17])
(def blank-board [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])
(def blank-game {:score 0
                 :board blank-board})

"Utility Functions"
(defn print-board
  [board]
  (let [board (vec board)]
    (println (take 4 board))
    (println (take 4 (subvec board 4)))
    (println (take 4 (subvec board 8)))
    (println (take 4 (subvec board 12)))))

(defn stop-game
  [board]
  (println "There are no more moves possible")
  (print-board  board))

"Movement Functions"
(defn process-seq
  ([xs] (process-seq (first xs) (rest xs)))
  ([last xs]
   (cond
     (empty? xs)
     (if (> last 0)
       (seq [last])
       '())
     (= 0 (first xs))
     (process-seq last (rest xs))
     (= last (first xs))
     (cons (* 2 last) (process-seq -1 (rest xs)))
     (> last 0)
     (cons last (process-seq (first xs) (rest xs)))
     :else
     (process-seq (first xs) (rest xs)))))

(defn pad-length-4
  [vec]
  (if (< (count vec) 4)
    (pad-length-4 (conj vec 0))
    vec))

(defn get-col
  [n row1 row2 row3 row4]
  [(nth row1 n) (nth row2 n) (nth row3 n) (nth row4 n)])

(defn move-left
  [board]
  (concat (pad-length-4 (vec (process-seq (vec (first (partition 4 board))))))
          (pad-length-4 (vec (process-seq (vec (second (partition 4 board))))))
          (pad-length-4 (vec (process-seq (vec (nth (partition 4 board) 2)))))
          (pad-length-4 (vec (process-seq (vec (nth (partition 4 board) 3)))))))

(defn move-right
  [board]
  (concat (pad-length-4 (reverse (process-seq (reverse (vec (first (partition 4 board)))))))
          (pad-length-4 (reverse (process-seq (reverse (vec (second (partition 4 board)))))))
          (pad-length-4 (reverse (process-seq (reverse (vec (nth (partition 4 board) 2))))))
          (pad-length-4 (reverse (process-seq (reverse (vec (nth (partition 4 board) 3))))))))

(defn move-up
  [board]
  (let [row1 (nth (partition 4 board) 0)
        row2 (nth (partition 4 board) 1)
        row3 (nth (partition 4 board) 2)
        row4 (nth (partition 4 board) 3)]
    (let [r1 (vec (pad-length-4 (vec (process-seq (vec (get-col 0 row1 row2 row3 row4))))))
          r2 (vec (pad-length-4 (vec (process-seq (vec (get-col 1 row1 row2 row3 row4))))))
          r3 (vec (pad-length-4 (vec (process-seq (vec (get-col 2 row1 row2 row3 row4))))))
          r4 (vec (pad-length-4 (vec (process-seq (vec (get-col 3 row1 row2 row3 row4))))))]
    (concat (get-col 0 r1 r2 r3 r4) 
            (get-col 1 r1 r2 r3 r4) 
            (get-col 2 r1 r2 r3 r4)
            (get-col 3 r1 r2 r3 r4)))))

(defn move-down
  [board]
  (let [row1 (nth (partition 4 board) 0)
        row2 (nth (partition 4 board) 1)
        row3 (nth (partition 4 board) 2)
        row4 (nth (partition 4 board) 3)]
    (let [r1 (vec (pad-length-4 (reverse (vec (process-seq (reverse (vec (get-col 0 row1 row2 row3 row4))))))))
          r2 (vec (pad-length-4 (reverse (vec (process-seq (reverse (vec (get-col 1 row1 row2 row3 row4))))))))
          r3 (vec (pad-length-4 (reverse (vec (process-seq (reverse (vec (get-col 2 row1 row2 row3 row4))))))))
          r4 (vec (pad-length-4 (reverse (vec (process-seq (reverse (vec (get-col 3 row1 row2 row3 row4))))))))]
      (concat (get-col 0 r1 r2 r3 r4)
              (get-col 1 r1 r2 r3 r4)
              (get-col 2 r1 r2 r3 r4)
              (get-col 3 r1 r2 r3 r4)))))

"Computer Turn Code"
(defn generate
  [board]
  (let [zeroLoc (filter #(zero? (nth blank-board %)) (range 16))]
    (assoc board (rand-nth zeroLoc) 2)))

(defn computer-turn
  [game]
  {:score (game :score)
   :board (generate (game :board))})


"Deterministic Computer Turn Code"
(defn determanistic-generate
  [board]
  (if (== (.indexOf board 0) -1) board
  (assoc board (.indexOf board 0) 2)))

(defn determanistic-computer-turn
  [game]
  {:score (game :score)
   :board (determanistic-generate (game :board))})

"Game Loop"

(def game (atom {
           :score 0
           :board (determanistic-generate blank-board)}))

 (defn new-game 
   []
   (reset! game {
             :score 0
             :board (determanistic-generate blank-board)}))

(defn end-game
  []
  (reset! game {
            :score 0
            :board full-board}))

(defn up
  [game]
  (if (= (vec (move-up (@game :board))) (@game :board)) ()
  (reset! game (determanistic-computer-turn {:score 0
                                             :board (vec (move-up (@game :board)))})))
  (print-board (@game :board)))

(defn down
  [game]
  (if (= (vec (move-down (@game :board))) (@game :board)) ()
  (reset! game (determanistic-computer-turn {:score 0
                                             :board (vec (move-down (@game :board)))})))
  (print-board (@game :board)))

(defn left
  [game]
  (if (= (vec (move-left (@game :board))) (@game :board)) ()
  (reset! game (determanistic-computer-turn {:score 0
                                             :board (vec (move-left (@game :board)))})))
  (print-board (@game :board)))
  
(defn right
  [game]
  (if (= (vec (move-right (@game :board))) (@game :board)) ()
  (reset! game (determanistic-computer-turn {:score 0
                                             :board (vec (move-right (@game :board)))})))
  (print-board (@game :board)))