;; Exercise 1
(defn fib [n]
  (loop [cnt (dec n) prev 0 cur 1]
    (if (= cnt 0) prev
      (recur (dec cnt) cur (+ prev cur)))))

(comment
  (defn fib [n]
    (nth (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1])) (dec n))))
