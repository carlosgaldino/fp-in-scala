;; Exercise 2
(defn tail [coll]
  (rest coll))

;; Exercise 3
(defn set-head [x coll]
  (cons x (tail coll)))

;; Exercise 4
(defn dropp [n coll]
  (if (and (pos? n) coll)
    (recur (dec n) (tail coll))
    coll))

;; Exercise 5
(defn dropp-while [f coll]
  (if (f (first coll))
    (recur f (tail coll))
    coll))

;; Exercise 6
(defn init [coll]
  (loop [ret [] coll coll]
    (if (next coll)
      (recur (conj ret (first coll)) (tail coll))
      ret)))

(defn fold-r [coll ret func]
  (if (empty? coll)
    ret
    (func (first coll) (fold-r (tail coll) ret func))))

(defn summ [coll]
  (fold-r coll 0 #(+ %1 %2)))

(defn product [coll]
  (fold-r coll 1.0 #(* %1 %2)))

;; Exercise 9
(defn length [coll]
  (fold-r coll 0 #(+ 1 %2)))

;; Exercise 10
(defn fold-l [coll ret func]
  (if (empty? coll)
    ret
    (fold-l (tail coll) (func ret (first coll)) func)))

;; Exercise 11
(defn suml [coll]
  (fold-l coll 0 #(+ %1 %2)))

(defn productl [coll]
  (fold-l coll 1.0 #(* %1 %2)))

(defn llength [coll]
  (fold-l coll 0 #(+ 1 %2)))

;; Exercise 12
(defn rreverse [coll]
  (fold-l coll [] #(cons %2 %1)))

;; Exercise 14
(defn append [xs zs]
  (fold-r xs zs #(cons %1 %2)))
