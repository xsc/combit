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

(defn throw-error
  "Throw Error Exception."
  [sender & parts]
  (throw (Exception. (str "[" sender "] " (apply str parts)))))

(defn curry
  "Create function that enables currying of a given function f with n parameters,
   always passing the given arguments first. If all parameters are given, the function
   will be evaluated."
  ([f n] (curry f n []))
  ([f n args] (fn x 
                ([] x)
                ([& new-args]
                 (let [c (count new-args)]
                   (if (< c n)
                     (curry f (- n c) (concat args new-args))
                     (->>
                       (concat args new-args)
                       (apply f))))))))
