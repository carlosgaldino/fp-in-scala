;; Exercise 1
(defn fib [n]
  (loop [cnt (dec n) prev 0 cur 1]
    (if (= cnt 0) prev
      (recur (dec cnt) cur (+ prev cur)))))

(comment
  (defn fib [n]
    (nth (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1])) (dec n))))

;; Exercise 2
(defn is-sorted [coll func]
  (every? true? (map func (partition 2 1 coll))))

;; Exercise 3
;; Almost curry. You still need to wrap the result to actually evaluate the
;; function.
;; Based on the code from Programming Clojure book.
(defn curry [& args]
  (apply partial partial args))

(comment
  (defn plus [a b]
    (+ a b))

  (def plus1
    (curry plus 1))

  (prn ((plus1 5))))

;; Exercise 5
(defn compose [f g]
  (fn [a]
    (f (g a))))

(comment
  (defn half [a] (/ a 2))

  (def negative-half (compose half -))

  (negative-half 8))
