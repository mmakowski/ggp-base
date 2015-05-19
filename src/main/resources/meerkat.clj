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

(require '[clojure.zip :as zip])

; TODO: newly added children are not visited in current iteration -- this wastes memory
(defn monte-carlo-tree-search [gamer timeout]
  (let [state-machine        (.getStateMachine gamer)
        current-state        (.getCurrentState gamer)
        role                 (.getRole gamer)
        roles                (.getRoles state-machine)
        role-count           (count roles)
        my-idx               (.indexOf roles role)
        random-move          (.getRandomMove state-machine current-state role)

        submission-time-ms   2000
        search-end-time      (- timeout submission-time-ms)

        initial-utilities    (vec (replicate role-count 0))]

    (letfn [(run [root-loc]
              (if (>= (System/currentTimeMillis) search-end-time)
                (minimax root-loc)
                (let [selected-loc     (select root-loc)
                      expanded-loc     (expand selected-loc)
                      scores           (simulate expanded-loc)
                      updated-root-loc (backpropagate expanded-loc scores)]
                  (run updated-root-loc))))

            (minimax [root-loc]
              (let [my-move (fn [node] (nth (:move node) my-idx))
                    child-nodes (:children (zip/node root-loc))
                    grouped-nodes (group-by my-move child-nodes)
                    min-score (fn [kv]
                                (let [nodes (second kv)
                                      scores (map my-utility nodes)
                                      min-score (reduce min scores)]
                                  min-score))]
                (first (max-by min-score grouped-nodes))))

            (select [start-loc]
              (if (not-visited start-loc)
                start-loc
                (letfn [(first-not-visited-sibling [loc]
                          (if (nil? loc)
                            nil
                            (if (not-visited loc)
                              loc
                              (first-not-visited-sibling (zip/right loc)))))]
                  (let [first-child             (zip/down start-loc)
                        first-not-visited-child (first-not-visited-sibling first-child)]
                    (if (not (nil? first-not-visited-child))
                      first-not-visited-child
                      (if (nil? (zip/node first-child))
                        start-loc
                        (let [active-role-idx (active-role-idx (:state (zip/node start-loc)))]
                          (select (max-by (muct active-role-idx) (sibling-locs [first-child]))))))))))

            (expand [loc]
              (let [state (:state (zip/node loc))]
                (if (.isTerminal state-machine state)
                  loc
                  (let [moves (.getLegalJointMoves state-machine state)
                        child-nodes (map (partial mk-node state) moves)]
                    (reduce zip/append-child loc child-nodes)))))

            (simulate [loc]
              (let [state     (:state (zip/node loc))
                    avg-score (make-array Double/TYPE role-count)
                    avg-depth (make-array Double/TYPE 1)
                    depth-discount-factor 1
                    repetitions 10]
                (do
                  (.getAverageDiscountedScoresFromRepeatedDepthCharges
                    state-machine
                    state
                    avg-score
                    avg-depth
                    depth-discount-factor
                    repetitions)
                  (vec avg-score))))

            (backpropagate [loc scores]
              (let [update-utility  (fn [visits]
                                      (fn [utility-and-score]
                                        (let [utility (first utility-and-score)
                                              score   (second utility-and-score)]
                                          (/ (+ (* visits utility) score) (+ visits 1)))))
                    update-utilities (fn [visits utilities]
                                       (map (update-utility visits) (map vector utilities scores)))
                    update      (fn [node]
                                  (assoc node
                                         :visits  (+ (:visits node) 1)
                                         :utilities (update-utilities (:visits node) (:utilities node))))
                    updated-loc (zip/edit loc update)
                    parent-loc  (zip/up updated-loc)]
                (if (nil? parent-loc)
                  updated-loc
                  (backpropagate parent-loc scores))))

            ; modified upper confidence bounds on trees -- "modified" to reject terminal states
            ; NOTE: will not work for simultaneous moves games, as assumes that in each state there is one active
            ;       player who chooses the next move that is most promising for them
            (muct [active-role-idx]
              (fn [loc]
                (let [node        (zip/node loc)
                      parent-node (zip/node (zip/up loc))
                      utility     (nth (:utilities node) active-role-idx)]
                  (if (.isTerminal state-machine (:state node))
                    0
                    (+ utility (Math/sqrt (* 2 (/ (Math/log (:visits parent-node)) (:visits node)))))))))

            (not-visited [loc]
              (= (:visits (zip/node loc)) 0))

            (my-utility [node]
              (nth (:utilities node) my-idx))

            (active-role-idx [state]
              (let [active-role (max-by (fn [r] (count (.getLegalMoves state-machine state r))) roles)]
                (.indexOf roles active-role)))

            (sibling-locs [locs]
              (let [next-sibling (zip/right (last locs))]
                (if (nil? next-sibling)
                  locs
                  (sibling-locs (concat locs [next-sibling])))))

            (max-by [f seq]
              (do
                (assert (not (empty? seq)))
                (let [higher-snd (fn [p1 p2] (if (> (second p2) (second p1)) p2 p1))
                      pairs      (map (fn [x] [x (f x)]) seq)]
                  (first (reduce higher-snd (first pairs) pairs)))))

            (mk-node [state joint-move]
              {:move      joint-move
               :state     (.getNextState state-machine state joint-move)
               :utilities initial-utilities
               :visits    0
               :children  []})

            (mk-root-loc [tree]
              (zip/zipper map? :children (fn [n c] (assoc n :children c)) tree))]
      (run (mk-root-loc {:move nil :state current-state :utilities initial-utilities :visits 0 :children []})))))

(defn MeerkatGamer []
  (proxy [StateMachineGamer] []

    (getInitialStateMachine []
      (ProverStateMachine.))

    (stateMachineSelectMove [timeout]
      (monte-carlo-tree-search this timeout))

    (stateMachineMetaGame [timeout]
      (println "MeerkatGamer metagame called"))

    (stateMachineAbort []
      (println "MeerkatGamer abort called"))

    (stateMachineStop []
      (println "MeerkatGamer stop called"))))
