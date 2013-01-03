(ns ^{ :doc "Create stream-based Components from block-based ones."
       :author "Yannick Scherer" }
  combit.stream
  (:use [combit.data :as data]
        [combit.component :as c :only [normalize-specs]]
        [combit.core :only [component gate primitive]]))

;; ## Simple Stream Components
;;
;; Components generated by `wrap-stream-component` split the given inputs into suitable parts
;; and `map` a block-based component over the resulting sequence. After that, the
;; different output blocks are merged into a single output seq.

;; ### Split Inputs/Concatenate Outputs

(defn- concat-output-blocks
  "Concatenate a series of outputs to a single output."
  [output-blocks]
  (let [output-blocks (map #(if (fn? %) (%) %) output-blocks)]
    (reduce
      (fn [out1 out2]
        (map
          (fn [block1 block2]
            (data/concat-data block1 block2))
          out1 out2))
      output-blocks)))

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
              (when (some (complement data/empty-data?) inputs)
                (cons (next-inputs inputs)
                      (lazy-split (rest-inputs inputs))))))]
    (lazy-split inputs)))

;; ### Wrappers

(defn wrap-stream-component
  "Wrap a component function, passing subsequent blocks of the given sizes to it."
  [f block-sizes]
  (->
    (fn [& inputs]
      (let [input-blocks (split-inputs inputs block-sizes)
            output-blocks (map #(apply f %) input-blocks)
            outputs# (concat-output-blocks output-blocks)]
        (constantly outputs#)))
    (c/wrap-component (count block-sizes))))

(defn wrap-stream-gate
  "Wrap a component function, passing subsequent one-element data containers to it."
  [f input-count]
  (wrap-stream-component f (take input-count (repeatedly (constantly 1)))))

;; ### Macros

(defmacro stream-component
  "Create new stream component function, operating on blocks of the given sizes."
  [inputs outputs & transformations]
  (let [input-pairs (c/normalize-specs inputs)
        input-sizes (vec (map (comp :width second) (:specs input-pairs)))]
    `(let [c# (component ~inputs ~outputs ~@transformations)]
       (wrap-stream-component c# ~input-sizes)))) 

(defmacro def-stream
  "Define new stream component."
  [id inputs outputs & transformations]
  `(def ~id (stream-component ~inputs ~outputs ~@transformations)))

(defmacro stream-gate
  "Create new stream gate function, operating on blocks of size 1."
  [inputs outputs & transformations]
  (let [inputs (mapcat (fn [sym] `[~sym 1]) inputs)
        outputs (mapcat (fn [sym] `[~sym 1]) outputs)]
    `(stream-component ~(vec inputs) ~(vec outputs) ~@transformations)))

(defmacro def-stream-gate
  "Define new stream gate function."
  [id inputs outputs & transformations]
  `(def ~id (stream-gate ~inputs ~outputs ~@transformations)))

(defmacro stream-primitive
  "Create new stream primitive (inputs of size 1, exactly one output of size 1)."
  [inputs & body]
  `(->
     (primitive ~inputs ~@body)
     (wrap-stream-gate ~(count inputs))))

(defmacro def-stream-primitive
  "Define new stream primitive."
  [id inputs & body]
  `(def ~id (stream-primitive ~inputs ~@body)))

;; ## Stream Components with Feedback
;;
;; A component's output for a block A can be used as the same component's input for
;; a block B (where B follows A) by using `wrap-feedback-component` or `wrap-feedback-gate`:
;;
;;     (def-gate xor1 [current-bit previous-result] [result] ...)
;;     (def xor
;;       (wrap-feedback-gate xor1 [[_ p] :> [r]]
;;         (>> (r) (p))))
;;
;;         
;;         
