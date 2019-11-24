(ns intro-to-ec.2048.search
  (:require [intro-to-ec.2048.game :refer [board-left board-right board-up board-down start-board]])
  (:require [clojure.set :as cset])
  (:require [clojure.data.priority-map :as pm]))

"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~goal~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
(defn has-2048?
  [board]
  (not= (.indexOf board 2048) -1))

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

(def the-magic-question
  {:get-next (fn [pmap] (first (first pmap)))
   :add-children add-children})
(def the-magic-solution
  {:goal? has-2048?
   :make-children make-children
   })

(defn search
  [{:keys [get-next add-children]}
   {:keys [goal? make-children]}
   start-state max-calls]
  (loop [frontier (pm/priority-map start-state 10)
         came-from {start-state :start}
         num-calls 0]
    (println num-calls ": " (first frontier))
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