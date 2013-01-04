(ns ^{ :doc "Combit Component Combination"
       :author "Yannick Scherer" }
  combit.combination)

;; ## Concatenation

(defn sequential*
  "Create component that represents the sequential concatenation of the given ones.
   Outputs from one component will be passed as inputs to the next one."
  [components]
  (if-not (seq components)
    vector
    (fn [& inputs]
      (loop [inputs inputs
             comps components]
        (if-not (seq comps)
          (vec inputs)
          (let [[c & rest-comps] comps
                next-inputs (apply c inputs)]
            (if (fn? next-inputs) 
              (sequential* (cons next-inputs rest-comps)) ;; sequential currying
              (recur next-inputs rest-comps))))))))

(defn sequential
  "See `sequential*`."
  [& components]
  (sequential* components))

;; ## Parallelization

(defn parallel*
  "Create component that represents the parallelization of the given components.
   If component A takes n inputs and produces m outputs, and B takes n' and produces
   m', the component (parallel [n n'] [A B]) will take (n + n') inputs and produce 
  (m + m') outputs.
  
   The first seq is necessary to determine where inputs should be split. Both parameters
   can be infinite seqs, which is very useful in creating components with unlimited inputs."
  [input-counts components]
  (fn [& inputs]
    (loop [inputs inputs
           counts input-counts
           comps components
           results []]
      (if-not (and (seq inputs) (seq counts) (seq comps))
        (vec results)
        (let [ic (first counts)
              rest-counts (rest counts)
              c (first comps)
              rest-comps  (rest comps)
              next-inputs (take ic inputs)
              nic (count next-inputs)]
          (if (< nic ic)
            (parallel* ;; parallel currying
              (cons (- ic nic) rest-counts)
              (cons #(concat results (apply c (concat next-inputs %&))) rest-comps))
            (let [rest-inputs (drop ic inputs)
                  next-result (apply c next-inputs)]
              (recur rest-inputs rest-counts rest-comps
                     (concat results next-result)))))))))

(defn parallel
  "See `parallel*`."
  [input-counts & components]
  (parallel* input-counts components))

(defn parallel-n*
  "See `parallel*`. Assumes each component has the same number of inputs."
  [n components]
  (apply parallel
    (repeatedly (constantly n))
    components))

(defn parallel-n
  "See `parallel*`."
  [n & components]
  (parallel-n* n components))

(defn parallel-1*
  "See `parallel*`. Assumes each component has exactly one input."
  [components]
  (parallel-n* 1 components))

(defn parallel-1
  "See `parallel*`. Assumes each component has exactly one input."
  [& components]
  (parallel-n* 1 components))

(defn parallel-2*
  "See `parallel*`. Assumes each component has exactly two inputs."
  [components]
  (parallel-n* 2 components))

(defn parallel-2
  "See `parallel*`. Assumes each component has exactly two inputs."
  [& components]
  (parallel-n* 2 components))

(defn parallel-3*
  "See `parallel*`. Assumes each component has exactly three inputs."
  [components]
  (parallel-n* 3 components))

(defn parallel-3
  "See `parallel*`. Assumes each component has exactly three inputs."
  [& components]
  (parallel-n* 3 components))

;; ## Keep Inputs

(defn prepend-inputs
  "Create component that will append all inputs to the result of the given
   component."
  [c]
  (fn [& inputs]
    (concat 
      inputs
      (apply c inputs))))

(defn append-inputs
  "Create component that will prepend all inputs to the result of the given
   component."
  [c]
  (fn [& inputs]
    (concat 
      inputs
      (apply c inputs))))

;; ## Selection/Reordering

(defn select-outputs
  "Create component that selects a previous component's outputs (given by indices).
   Indices may repeat and can also be used for reordering."
  ([c outputs] (sequential c (select-outputs outputs)))
  ([outputs] (fn [& previous-outputs]
               (let [outs (vec previous-outputs)]
                 (vec (map #(get outs %) outputs))))))

(defn drop-outputs
  "Create component that drops a previous component's outputs (given by indices)."
  ([c outputs] (sequential c (drop-outputs outputs)))
  ([outputs] (let [keep-output? (complement (set outputs))]
               (fn [& previous-outputs]
                 (vec (keep-indexed #(when (keep-output? %1) %2) 
                                    previous-outputs))))))
