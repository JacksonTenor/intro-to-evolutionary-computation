(ns intro-to-ec.2048.search
  (:require [intro-to-ec.2048.game :refer [board-left board-right board-up board-down start-board print-board]])
  (:require [clojure.set :as cset])
  (:require [clojure.data.priority-map :as pm]))

"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~goal~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
(defn has-2048?
  [board]
  (not= (.indexOf board 2048) -1))

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

; One thing we tried. Had the same amount of steps, just a different score.
(defn score-board
  [board]
  (score board))

; One thing we tried. Had the same amount of steps, just a different score.
(defn score-times-zeros-retain-some-multiplier
  [board]
  (*(score board) (+ 0.1 (count-zeros board))))

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
  (into frontier (map (fn [kid] [kid (* -1 (score-times-zeros kid))]) kids)))

(defn add-children-modified
  [kids frontier]
  (assoc frontier  (fn [kid] [kid (* -1 (score-times-zeros kid))]) kids) )

(def the-magic-question
  {:get-next (fn [pmap] (first (first pmap)))
   :add-children add-children})
(def the-magic-solution
  {:goal? has-64?
   :make-children make-children
   })

(defn search
  [{:keys [get-next add-children]}
   {:keys [goal? make-children]}
   start-state max-calls]
  (loop [frontier (pm/priority-map start-state 10)
         came-from {start-state :start}
         num-calls 0]
    (println num-calls ": " (first frontier) "\n")
    ; (println "\n" num-calls "\n")
    ; (println (pm/priority-map frontier 10))
    (println "\nRest: " "\n\n" (rest frontier) "\n")
    (println "~~~~~~~~~~~~~~~~~")
    (let [current (get-next frontier)]
      (cond
        (goal? current) (generate-path came-from current)
        (= num-calls max-calls) :max-calls-reached
        :else
        (let [kids (remove-visited (make-children current) frontier (keys came-from))]
          (recur
           (add-children kids (rest frontier))
           (reduce (fn [cf child] (assoc cf child current)) came-from kids)
           (inc num-calls)
           ))))))