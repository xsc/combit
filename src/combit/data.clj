(ns ^{ :doc "Data Representation for Combit."
       :author "Yannick Scherer" }
  combit.data)

;; ## Block of Bits

(declare set-width)

(defn create-block
  "Create new block of the given width."
  ([width]
   (-> {}
     (assoc :width width)
     (assoc :data (vec (map (constantly 0) (range width))))))
  ([data width]
   (-> {}
     (assoc :width (count data))
     (assoc :data (vec data))
     (set-width width))))

(defn block-width
  "Get width of block."
  [{:keys[width]}]
  width)

(defn block-data
  "Get data of block."
  [{:keys[data]}]
  data)

(defn get-bits
  "Get a new block consisting of the given bits, 0 being the LSB."
  [{:keys[width data]} vector-of-bits]
  (let [data (map
               (fn [n]
                 (if (< n width)
                   (nth data (- width n 1))
                   0))
               vector-of-bits)]
    (create-block data (count vector-of-bits))))

(defn get-bit
  "Get nth bit of the block, 0 being the LSB."
  [block n]
  (get-bits block [n]))

(defn set-bit
  "Set nth bit of the block, 0 being the LSB. Everything that does not
   evaluate to 0 or to false/nil will be treated as 1."
  [{:keys[width data] :as block} n x]
  (if (< n width)
    (assoc-in block [:data (- width n 1)]
              (if (or (= x 0) (not x))
                0
                1))
    block))

(defn set-width
  [{:keys[width data] :as block} w]
  (if-not w
    block
    (let [width (or width (count data))]
      (-> block
        (assoc :width w)
        (assoc :data
               (cond (< width w) (vec (concat 
                                        (take (- w width) (repeatedly (constantly 0)))
                                        data))
                     (> width w) (vec (drop (- width w) data))
                     :else data))))))

;; ## Block from Data

(defprotocol Block
  "Protocol for Types that can be converte to Blocks."
  (data->block* [data] "Convert data to block."))

(extend-protocol Block

  clojure.lang.IPersistentVector
  (data->block* [v]
    (let [data (vec
                 (map (fn [x]
                        (if (or (= x 0) (not x))
                          0
                          1))
                      v))]
      (create-block data (count data))))

  clojure.lang.IPersistentMap
  (data->block* [{:keys[width data]}]
    (create-block (or data []) (or width 0)))

  java.lang.Integer
  (data->block* [n]
    (let [data (vec
                 (loop [n n
                        block []]
                   (case n
                     0 (cons 0 block)
                     1 (cons 1 block)
                     (recur (quot n 2) (cons (rem n 2) block)))))]
      (create-block data (count data))))

  java.lang.Long
  (data->block* [n]
    (let [data (vec
                 (loop [n n
                        block []]
                   (case n
                     0 (cons 0 block)
                     1 (cons 1 block)
                     (recur (quot n 2) (cons (rem n 2) block)))))]
      (create-block data (count data)))))

(defn data->block
  ([data] (data->block data nil))
  ([data width]
   (-> 
     (data->block* data)
     (set-width width))))
