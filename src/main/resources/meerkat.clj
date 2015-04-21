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

(defn alpha-beta [gamer]
  (let [state-machine (.getStateMachine gamer)
        current-state (.getCurrentState gamer)
        role          (.getRole gamer)]
    (letfn [(min-score [state move alpha beta]
              (let [joint-moves   (.getLegalJointMoves state-machine state role move)
                    make-move     (fn [joint-move] (.getNextState state-machine state joint-move))
                    score         (fn [beta joint-move] (max-score (make-move joint-move) alpha beta))
                    beta-or-score (fn [beta joint-move] (min beta (score beta joint-move)))
                    cutoff-score  (fn [beta joint-move]
                                    (if (<= beta alpha)
                                      alpha
                                      (beta-or-score beta joint-move)))
                    min-max-score (reduce cutoff-score beta joint-moves)]
                min-max-score))
            (max-score [state alpha beta]
              (if (.isTerminal state-machine state)
                (.getGoal state-machine state role)
                (let [moves          (.getLegalMoves state-machine state role)
                      alpha-or-score (fn [alpha move]  (max alpha (min-score state move alpha beta)))
                      cutoff-score   (fn [alpha move]
                                       (if (>= alpha beta)
                                         beta
                                         (alpha-or-score alpha move)))]
                  (reduce cutoff-score alpha moves))))]
      (let [moves (.getLegalMoves state-machine current-state role)
            initial {:move (first moves) :score 0}
            best-move (fn [curr-best move]
                        (if (= (:score curr-best) 100)
                          curr-best
                          (let [score (min-score current-state move (:score curr-best) 100)]
                            (if (> score (:score curr-best))
                              {:move move :score score}
                              curr-best))))]
        (:move (reduce best-move initial moves))))))

(defn MeerkatGamer []
  (proxy [StateMachineGamer] []

    (getInitialStateMachine []
      (ProverStateMachine.))

    (stateMachineSelectMove [timeout]
      (if (= (count (.getRoles (.getStateMachine this))) 1)
        (compulsive-deliberation this)
        (alpha-beta this)))

    (stateMachineMetaGame [timeout]
      (println "MeerkatGamer metagame called"))

    (stateMachineAbort []
      (println "MeerkatGamer abort called"))

    (stateMachineStop []
      (println "MeerkatGamer stop called"))))
