(ns ^{ :doc "Create stream-based Components from block-based ones."
       :author "Yannick Scherer" }
  combit.stream
  (:use [combit.data :as data]
        combit.core))

(defn- concat-output-blocks
  "Concatenate a series of outputs to a single output."
  [output-blocks]
  (reduce
    (fn [out1 out2]
      (map
        (fn [block1 block2]
          (concat-data block1 block2))
        out1 out2))
    output-blocks))

(defn- split-inputs
  "Create lazy seq of inputs consisting of data with the given element count.
   Example:

      (split-inputs [[1 2 3 4] [1 2]] [2 1])
      ;=> ( ([1 2] [1]) ([3 4] [2]) )
   
   No padding is performed."
  [inputs block-sizes]
  (letfn [(next-inputs [inputs]
            (map take-elements block-sizes inputs))
          (rest-inputs [inputs]
            (map drop-elements block-sizes inputs))
          (lazy-split [inputs]
            (lazy-seq
              (when (some (complement empty-data?) inputs)
                (cons (next-inputs inputs)
                      (lazy-split (rest-inputs inputs))))))]
    (lazy-split inputs)))

(defn wrap-stream
  "Wrap a component function, passing subsequent blocks of the given sizes to it."
  [f block-sizes]
  (fn [inputs & _]
    (let [input-blocks (split-inputs inputs block-sizes)
          output-blocks (map f input-blocks)]
      (concat-output-blocks output-blocks))))

(defmacro stream-component
  "Create new stream component function, operating on blocks of the given sizes."
  [inputs outputs & transformations]
  (let [input-pairs (normalize-specs inputs)
        input-sizes (vec (map (comp :width second) input-pairs))]
    `(let [c# (component ~inputs ~outputs ~@transformations)]
       (wrap-stream c# ~input-sizes)))) 
