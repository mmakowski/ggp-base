(ns gamer_namespace
  (:import [org.ggp.base.player.gamer.statemachine StateMachineGamer]
           [org.ggp.base.util.statemachine.implementation.prover ProverStateMachine]))

(defn random-move [gamer]
  (let [state-machine (.getStateMachine gamer)
        current-state (.getCurrentState gamer)
        role          (.getRole gamer)
        random-move   (.getRandomMove state-machine current-state role)]
    random-move))

(defn higher-score [curr-max candidate]
  (if (< (:score curr-max) (:score candidate))
    candidate
    curr-max))

(defn compulsive-deliberation [gamer]
  (do
    (assert (= (count (.getRoles (.getStateMachine gamer))) 1))
    (let [state-machine (.getStateMachine gamer)
          current-state (.getCurrentState gamer)
          role          (.getRole gamer)
          make-move     (fn [state move] (.getNextState state-machine state [move]))]
      (letfn [(max-score [state]
                (if (.isTerminal state-machine state)
                  (.getGoal state-machine state role)
                  (let [moves  (.getLegalMoves state-machine state role)
                        scores (map (comp max-score (partial make-move state)) moves)]
                    (reduce max 0 scores))))]
        (let [with-score (fn [move]
                           (let [new-state (make-move current-state move)]
                             {:move move :score (max-score new-state)}))
              moves         (.getLegalMoves state-machine current-state role)
              scored-moves  (map with-score moves)
              best-move-and-score (reduce higher-score (first scored-moves) scored-moves)]
          (:move best-move-and-score))))))

(defn minimax [gamer]
  (let [state-machine (.getStateMachine gamer)
        current-state (.getCurrentState gamer)
        role          (.getRole gamer)
        make-move     (fn [state joint-moves] (.getNextState state-machine state joint-moves))]
    (letfn [(min-score [state move]
              (let [joint-moves (.getLegalJointMoves state-machine state role move)
                    next-states (map (partial make-move state) joint-moves)
                    max-scores (map (comp :score max-score) next-states)
                    min-max-score (reduce min (first max-scores) max-scores)]
                {:move move :score min-max-score}))
            (max-score [state]
              (if (.isTerminal state-machine state)
                {:move :f :score (.getGoal state-machine state role)}
                (let [moves  (.getLegalMoves state-machine state role)
                      scored-moves (do (assert (> (count moves) 0)) (map (partial min-score state) moves))]
                  (reduce higher-score (first scored-moves) scored-moves))))]
      (let [moves (.getLegalMoves state-machine current-state role)]
        (if (= (count moves) 1)
          (first moves)
          (:move (max-score current-state)))))))

(defn MeerkatGamer []
  (proxy [StateMachineGamer] []

    (getInitialStateMachine []
      (ProverStateMachine.))

    (stateMachineSelectMove [timeout]
      (if (= (count (.getRoles (.getStateMachine this))) 1)
        (compulsive-deliberation this)
        (minimax this)))

    (stateMachineMetaGame [timeout]
      (println "MeerkatGamer metagame called"))

    (stateMachineAbort []
      (println "MeerkatGamer abort called"))

    (stateMachineStop []
      (println "MeerkatGamer stop called"))))
