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

;; Exercise 24
(defn starts-with [xs zs]
  (if (empty? zs)
    true
    (if (empty? xs)
      false
      (if (= (first xs) (first zs))
        (recur (tail xs) (tail zs))))))

(defn has-subsequence [xs sub]
  (if (empty? xs)
    false
    (if (starts-with xs sub)
      true
      (recur (tail xs) sub))))

;; Exercise 25
(defn leaf [value]
  { :value value })

(defn branch [left right]
  { :left left :right right })

(defn leaf? [node]
  (contains? node :value))

(defn left [branch]
  (:left branch))

(defn right [branch]
  (:right branch))

(defn size [tree]
  (if (leaf? tree)
    1
    (+ 1 (size (left tree)) (size (right tree)))))

(comment
  (def tree (branch (leaf "blah") (branch (leaf "bleh") (leaf "meh"))))

  (size tree))

;; Exercise 26
(defn value [node]
  (:value node))

(defn maximum [tree]
  (if (leaf? tree)
    (value tree)
    (max (maximum (left tree)) (maximum (right tree)))))

(comment
  (def tree (branch (leaf 90) (branch (leaf 10) (leaf 99))))

  (maximum tree))

;; Exercise 27
(defn depth [tree]
  (if (leaf? tree)
    1
    (+ 1 (max (depth (left tree)) (depth (right tree))))))

(comment
  (def tree (branch (branch (branch (leaf 90) (leaf 10)) (leaf 10)) (branch (leaf 10) (leaf 99))))

  (depth tree))

;; Exercise 28
(defn mapt [f tree]
  (if (leaf? tree)
    (leaf (f (value tree)))
    (branch (mapt f (left tree)) (mapt f (right tree)))))

;; Exercise 29
(defn foldt [tree f g]
  (if (leaf? tree)
    (f (value tree))
    (g (foldt (left tree) f g) (foldt (right tree) f g))))

(defn sizef [tree]
  (foldt tree #(+ 1 %1 %2) (constantly 1)))

(defn maximumf [tree]
  (foldt tree #(max %1 %2) #(identity %1)))

(defn depthf [tree]
  (foldt tree #(+ 1 (max %1 %2)) (constantly 1)))
