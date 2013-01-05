(ns ^{ :doc "NAND based logical gates."
       :author "Yannick Scherer" }
  combit.examples.nand-logic
  (:use combit.core
        [combit.combination :as combine]))

;; ## NAND Universal Gate

(def-primitive nand-prim [a b]
  (if (= (+ a b) 2) 0 1))

;; ## Derived Gates
;; see: http://en.wikipedia.org/wiki/NAND_logic

(def ^:dynamic *nand* nand-prim)

(defn nand-gate 
  [& inputs]
  (apply *nand* inputs))

(def-gate not-gate [a] [out]
  (>> [(a) (a)] 
      (nand-gate) 
      (out)))

(def-gate and-gate [a b] [out]
  (>> [(a) (b)] 
      (nand-gate) 
      (not-gate) 
      (out)))

(def-gate or-gate [a b] [out]
  (>> [(a) (b)]
      (combine/parallel-1 (not-gate) (not-gate))
      (nand-gate)
      (out)))
       
(def-gate nor-gate [a b] [out]
  (>> [(a) (b)]
      (or-gate)
      (not-gate)
      (out)))

(def-gate xor-gate [a b] [out]
  (>> [(a) (b)]
      (combine/parallel-1
        (combine/prepend-inputs (nand-gate (b)))
        (combine/prepend-inputs (nand-gate (a))))
      (combine/parallel-2 (nand-gate) (nand-gate))
      (nand-gate)
      (out)))

(def-gate xnor-gate [a b] [out]
  (>> [(a) (b)]
      (xor-gate)
      (not-gate)
      (out)))

;; ## NAND Visualization and Optimization

(def-primitive nand-builder [a b]
  (let [{ na :n nas :nands ia :inputs } a
        { nb :n nbs :nands ib :inputs } b
        n (keyword (gensym "n"))]
    (-> {}
      (assoc :n n)
      (assoc :inputs (set (concat ia ib)))
      (assoc :nands
             (-> (merge nas nbs)
               (assoc n [na nb]))))))

(defn- optimize-nands
  "Optimize a NAND based tree using Fixpoint Iteration."
  [{:keys[nands inputs n] :as g}]
  (let [rename-map (->> nands
                     (group-by (comp set second))
                     (mapcat (fn [[k v]]
                               (if (> (count v) 1)
                                 (let [k' (keyword (gensym "n"))]
                                   (map vector (map first v) (repeat k')))
                                 [])))
                     (into {}))
        rename-key (fn [k]
                     (if-let [rn (rename-map k)]
                       rn
                       k))
        g' (-> {}
             (assoc :n (rename-key n))
             (assoc :inputs (set (map rename-key inputs)))
             (assoc :nands (->> nands
                             (map (fn [[k vs]]
                                    (vector (rename-key k)
                                            (vec (map rename-key vs)))))
                             (into {}))))]
    (if (= g g') g (recur g'))))

(defn build-nand-tree
  "Build an optimized NAND tree for a given component with the given number of inputs."
  [component input-count]
  (let [input-names (map (comp keyword #(str "i" %)) (range input-count))
        inputs (map (comp vector #(hash-map :n % :inputs #{%} :nands nil)) input-names)]
    (binding [*nand* nand-builder]
      (let [[[tree]] (apply component inputs)]
        (optimize-nands tree)))))
