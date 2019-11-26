(ns intro-to-ec.2048.search
  (:require [intro-to-ec.2048.game :refer [board-left board-right board-up board-down start-board print-board]])
  (:require [clojure.set :as cset])
  (:require [clojure.data.priority-map :as pm]))

"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~goal~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
(defn has-2048?
  [board]
  (not= (.indexOf board 2048) -1))

(defn has-1024?
  [board]
  (not= (.indexOf board 1024) -1))

(defn has-512?
  [board]
  (not= (.indexOf board 512) -1))

(defn has-256?
  [board]
  (not= (.indexOf board 256) -1))

(defn has-64?
  [board]
  (not= (.indexOf board 64) -1))

(defn has-16?
  [board]
  (not= (.indexOf board 16) -1))

(defn has-8?
  [board]
  (not= (.indexOf board 8) -1))

"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~heuristic~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
(defn score
  [board]
  (apply + board))

(defn count-zeros
  [board]
  (count (filter zero? board)))

(defn score-times-zeros
  [board]
  (*(score board) (count-zeros board)))
has-2048?
; One thing we tried. Had the same amount of steps, just a different score.
(defn score-board
  [board]
  (score board))

; One thing we tried. Had the same amount of steps, just a different score.
(defn score-times-zeros-retain-some-multiplier
  [board]
  (*(score board) (+ 0.1 (count-zeros board))))

(defn weighted-sum-v1
  [board]
  (+ (* 0.9 (apply max board)) (* 0.1 (count-zeros board))))

(defn weighted-sum-v2
  [board]
  ; (+ (+ (* 0.9 (apply max board)) (* 0.1 (count-zeros board))) (* 0.1 (score-times-zeros board))))
  (apply + [ (* 0.8 (apply max board)) (* 0.8 (apply max board)) (* 0.1 (count-zeros board)) (* 0.1 (score-times-zeros board))  ] ))
  ; (+ (+ (apply max board) (count-zeros board)) (score-times-zeros board)) )

(defn three-max
  [board]
  (let [max-values (take 3 (sort > board))
        first (first max-values)
        second (second max-values)
        third (nth max-values 2)]
    (apply + [first 
              second 
              third])))

(defn five-max
  [board]
  (let [max-values (take 5 (sort > board))
        first (first max-values)
        second (second max-values)
        third (nth max-values 2)
        fourth (nth max-values 3)
        fifth (nth max-values 4)]
    (apply + [first
              second
              third
              fourth
              fifth])))
  
 

"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Stolen~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
(defn remove-visited
  [new-states frontier visited]
  (remove (cset/union (set frontier) (set visited)) new-states))

(defn generate-path
  [came-from node]
  (if (= :start-node (get came-from node))
    [node]
    (conj (generate-path came-from (get came-from node)) node)))

"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Make Children~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
(defn make-children
  [board]
  (let [children (vector (board-left board) (board-right board) (board-up board) (board-down board))]
    (filter #(not (= % board)) children)))

"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Add Children~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
(defn add-children
  [kids frontier]
  (sort-by first (into frontier (map (fn [kid] [(* -1 (five-max kid)) kid]) kids))))

(def the-magic-question
  {:get-next (fn [pmap] (second (first pmap)))
   :add-children add-children})
(def the-magic-solution
  {:goal? has-2048?
   :make-children make-children
   })

;8 in 5 steps
;16 in 10 steps
;64 in 60 steps
;256 in 191 steps
;512 in 321 steps
;1024 in 570 steps
;2048 in 1655 steps

; EXAMPLE ON HOW TO RUN, IN SUCH A WAY THAT WE CAN SEE A WRONG SOLUTION
;(search the-magic-question the-magic-solution intro-to-ec.2048.game/start-board 62)

; working example
(into (sorted-map)
      (map (fn [[k v]] [k (vec v)])
           (clojure.set/map-invert {[4 32] -5, [2 64] -6})))

(defn search
  [{:keys [get-next add-children]}
   {:keys [goal? make-children]}
   start-state max-calls]
  (loop [frontier (into (sorted-map) {10 start-state})
         came-from {start-state :start-node}
         num-calls 0]   
    (let [current (get-next frontier)]
      ; (println "-----------")
      ; (println "Num- Calls: "num-calls)
      ; (println (first frontier))
      ; (println "Rest: ")
      ; (println (first (rest frontier)))
      ; (println (second (rest frontier)))
      ; (println "-----------")
      (cond
        (goal? current) num-calls
        (= num-calls max-calls) :max-calls-reached
        :else
        (let [kids (remove-visited (make-children current) frontier (keys came-from))]
          (recur
           (add-children kids (rest frontier))
           (reduce (fn [cf child] (assoc cf child current)) came-from kids)
           (inc num-calls)
           ))))))


 ; ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~(Use this value for the frontier)~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ; ;uses this solution, inverted order w/map-invert, but yet to be applied to our solution
    ; ;https://stackoverflow.com/questions/40560300/sorting-vectors-in-clojure-map-of-vectors/40560347#40560347
    ; (def properlySortedFrontier 
    ;   (into (sorted-map)
    ;         (map (fn [[k v]] [k (vec v)])
    ;              (clojure.set/map-invert (rest frontier)))))
    ; (println frontier)
    ; (println num-calls ": " (first properlySortedFrontier) "\n")
    ; ; (println "\n" num-calls "\n")
    ; ; (println (pm/priority-map frontier 10))
    ; (println "\nRest: " "\n\n" (rest properlySortedFrontier) "\n")
    ; (println "~~~~~~~~~~~~~~~~~")
    ; ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~(END)~~~~~~~~~~~~~~~~~~~~~~~~~~~~