(ns nsu-labs.core
    (:gen-class))

(use
 'clojure.test)

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn parallel-filter
  ([predicate coll]
   (let [parts       (partition-all 2048 (take 16384 coll))
         black-magic (fn [coll2]
                       (doall
                        (map deref
                             (doall (map (fn [coll1] (future (doall (filter predicate coll1)))) coll2)))))]
     (lazy-seq
      (if (empty? coll)
        '()
        (concat (reduce concat (black-magic parts))
                (parallel-filter predicate (drop 16384 coll))))))))

(defn prime? [n]
  (if (even? n)
    false
    (let [root (num (int (Math/sqrt n)))]
      (loop [i 3]
        (if (> i root)
          true
          (if (zero? (mod n i))
            false
            (recur (+ i 2))))))))

(defn my-wierd-predicate [x]
  (and (prime? x) (= (mod x 1337) 22)))
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Usual filter:")
  (println (time (doall (filter my-wierd-predicate (range 1000000)))))
  (println (time (doall (filter my-wierd-predicate (range 1000000)))))
  ;(println (time (doall (filter my-wierd-predicate (range 1000000)))))
  (println "My parallel filter:")
  (println (time (doall (parallel-filter my-wierd-predicate (range 1000000)))))
  (println (time (doall (parallel-filter my-wierd-predicate (range 1000000)))))
  ;(println (time (doall (parallel-filter my-wierd-predicate (range 1000000)))))
  (println "lazyness test")
  (println (take 1 (parallel-filter my-wierd-predicate (iterate inc 1))))
  (shutdown-agents))

