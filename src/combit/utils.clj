(ns ^{ :doc "Combit Utilitites"
       :author "Yannick Scherer" }
  combit.utils)

(defn new-vector
  "Create vector of given length with the given initial element (or nil)."
  ([len] (new-vector len nil))
  ([len initial]
   (vec
     (->>
       (repeatedly (constantly initial))
       (take len)))))
