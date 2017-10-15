(ns test.carly.world
  "Record and functions for simulating world states. Each world is represented
  by an immutable map containing the model state, current history, and map of
  pending thread operations."
  (:require
    [test.carly.op :as op]))


;; A world represents a point in time along a possible history. The `model`
;; holds the current representation of the system, the `history` is the
;; sequence of operations which have already happened, and `pending` is a map
;; from thread ids to lists of pending operations for each thread.
(defrecord World
  [model history pending futures])


(defn end-of-line?
  "Determine whether the worldline has ended."
  [world]
  (empty? (:pending world)))


(defn- peek-pending
  "Get the next pending operation for the identified thread, if any."
  [pending thread-id]
  (first (get pending thread-id)))


(defn- pop-pending
  "Remove the next pending operation for the identified thread. Returns an
  updated map with the remaining ops, or without the thread if no ops were left."
  [pending thread-id]
  (let [[_ & more] (get pending thread-id)]
    (if (seq more)
      (assoc pending thread-id more)
      (dissoc pending thread-id))))


(defn- future-count
  "Calculate the number of possible futures a world has based on a map of thread
  ids to pending operations."
  [pending]
  (if (<= (count pending) 1)
    1
    (let [fact (fn fact [n] (reduce * 1N (map inc (range n))))
          op-counts (map count (vals pending))]
      (apply / (fact (reduce + op-counts)) (map fact op-counts)))))


(defn initialize
  "Initialize a new world state given the initial model and a map of pending
  operations."
  [model pending]
  (map->World
    {:model model
     :history []
     :pending pending
     :futures (future-count pending)}))


(defn step
  "Compute a step by applying the next operation from the identified thread to
  the world. Returns an updated world state, or nil if the operation result was
  invalid."
  ([world]
   (step world (first (keys (:pending world)))))
  ([world thread-id]
   (let [op (peek-pending (:pending world) thread-id)]
     (when-not op
       (throw (IllegalStateException.
                (format "Cannot step thread %d - no ops pending" thread-id))))
     (when-not (contains? op ::op/result)
       (throw (IllegalStateException.
                (format "Cannot step op %s with no result" (pr-str op)))))
     (when (op/check op (:model world) (::op/result op))
       (-> world
           (update :model (partial op/update-model op))
           (update :history conj (assoc op ::thread thread-id))
           (update :pending pop-pending thread-id)
           (as-> w (assoc w :futures (future-count (:pending w)))))))))


(defn next-steps
  "Compute all possible valid next steps for the world, returning a sequence of
  new world states."
  [world]
  (keep (partial step world) (keys (:pending world))))


(defn visit-key
  "Return the key used to compare worlds which are equivalent nodes in the
  graph of possible futures."
  [world]
  [(:model world) (:pending world)])
