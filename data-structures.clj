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

;; Exercise 16
(defn plus-one [xs]
  (fold-r xs [] #(cons (+ 1 %1) %2)))

;; Exercise 17
(defn double-to-string [xs]
  (fold-r xs [(str 0.0)] #(cons (str %1) %2)))

;; Exercise 18
(defn mapp [xs func]
  (fold-r xs [] #(cons (func %1) %2)))

;; Exercise 19
(defn filterr [xs pred]
  (fold-r xs [] #(if (pred %1) (cons %1 %2) %2)))

;; Exercise 20
(defn flat-map [xs func]
  (apply concat (mapp xs func)))

;; Exercise 21
(defn filter-via-flat-map [xs pred]
  (flat-map xs #(if (pred %1) (cons %1 []) nil)))

;; Exercise 22
(defn add-lists [xs zs]
  (fold-r xs [] #(cons (+ %1 (nth zs (.indexOf xs %1))) %2)))

;; Exercise 23
(defn zip-with [xs zs f]
  (fold-r xs [] #(cons (f %1 (nth zs (.indexOf xs %1))) %2)))

;; Or
(comment
  (defn zip-with [xs zs f]
    (map f xs zs)))
