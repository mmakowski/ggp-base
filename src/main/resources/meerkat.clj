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

(defn bounded-depth [gamer max-depth]
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

(defn one-nonterm-or-actual [state-machine state roles]
  (if (.isTerminal state-machine state)
    (.getGoal state-machine state (first roles))
    1))

(defn my-goal [state-machine state roles]
  (.getGoal state-machine state (first roles)))

(defn advantage [state-machine state roles]
  (let [goals (map (fn [role] (.getGoal state-machine state role)) roles)
        my-goal (first goals)
        others-max (reduce max 0 (rest goals))]
    (- my-goal others-max)))

(defn monte-carlo [probe-count]
  (fn [state-machine state roles]
    (letfn [(goal-or [expansion-fn state]
              (if (.isTerminal state-machine state)
                (.getGoal state-machine state (first roles))
                (expansion-fn state)))
            (random-move [state]
              (let [moves (.getRandomJointMove state-machine state)]
                (.getNextState state-machine state moves)))
            (one-more-level [state]
              (goal-or probe (random-move state)))
            (probe [state]
              (goal-or one-more-level state))
            (probe-total [remaining-probes state acc]
              (if (= 0 remaining-probes)
                acc
                (probe-total (- remaining-probes 1) state (+ acc (probe state)))))
            (run-probes [state]
              (/ (probe-total probe-count state 0) probe-count))]
      (goal-or run-probes state))))

(defn ordered-roles [roles role]
  (if (= (first roles) role)
    roles
    (ordered-roles (concat (rest roles) [(first roles)]) role)))

(defn alpha-beta [gamer max-depth heuristic]
  (let [state-machine (.getStateMachine gamer)
        current-state (.getCurrentState gamer)
        role          (.getRole gamer)
        roles         (ordered-roles (.getRoles state-machine) role)]
    (letfn [(min-score [state move alpha beta depth]
              (let [joint-moves   (.getLegalJointMoves state-machine state role move)
                    make-move     (fn [joint-move] (.getNextState state-machine state joint-move))
                    score         (fn [beta joint-move]
                                    (let [state (make-move joint-move)]
                                      (if (< depth max-depth)
                                        (max-score state alpha beta depth)
                                        (max alpha (heuristic state-machine state roles)))))
                    beta-or-score (fn [beta joint-move] (min beta (score beta joint-move)))
                    cutoff-score  (fn [beta joint-move]
                                    (if (<= beta alpha)
                                      alpha
                                      (beta-or-score beta joint-move)))
                    min-max-score (reduce cutoff-score beta joint-moves)]
                min-max-score))
            (max-score [state alpha beta depth]
              (if (.isTerminal state-machine state)
                (.getGoal state-machine state role)
                (let [moves          (.getLegalMoves state-machine state role)
                      alpha-or-score (fn [alpha move]  (max alpha (min-score state move alpha beta (+ depth 1))))
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
                          (let [score (min-score current-state move (:score curr-best) 100 1)]
                            (if (> score (:score curr-best))
                              {:move move :score score}
                              curr-best))))]
        (:move (reduce best-move initial moves))))))

(defn MeerkatGamer []
  (proxy [StateMachineGamer] []

    (getInitialStateMachine []
      (ProverStateMachine.))

    (stateMachineSelectMove [timeout]
      (alpha-beta this 1 (monte-carlo 6)))

    (stateMachineMetaGame [timeout]
      (println "MeerkatGamer metagame called"))

    (stateMachineAbort []
      (println "MeerkatGamer abort called"))

    (stateMachineStop []
      (println "MeerkatGamer stop called"))))
