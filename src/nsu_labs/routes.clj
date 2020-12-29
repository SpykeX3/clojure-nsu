(ns nsu-labs.routes
    (:gen-class))
;;;an empty route map
;;;it is enough to use either forward or backward part (they correspond to each other including shared reference to number of tickets)
;;;:forward is a map with route start point names as keys and nested map as values
;;;each nested map has route end point names as keys and route descriptor as values
;;;each route descriptor is a map (structure in fact) of the fixed structure where 
;;;:price contains ticket price
;;;and :tickets contains reference to tickets number
;;;:backward has the same structure but start and end points are reverted 
(def empty-map
  {:forward  {},
   :backward {}})

(defn route
  "Add a new route (route) to the given route map
   route-map - route map to modify
   from - name (string) of the start point of the route
   to - name (string) of the end poiunt of the route
   price - ticket price
   tickets-num - number of tickets available"
  [route-map from to price tickets-num]
  (let [tickets                (ref tickets-num :validator (fn [state] (>= state 0))),     ;reference for the number of tickets
        orig-source-desc       (or (get-in route-map [:forward from]) {}),
        orig-reverse-dest-desc (or (get-in route-map [:backward to]) {}),
        route-desc             {:price   price,
                                ;route descriptor
                                :tickets tickets},
        source-desc            (assoc orig-source-desc to route-desc),
        reverse-dest-desc      (assoc orig-reverse-dest-desc from route-desc)]
    (-> route-map
        (assoc-in [:forward from] source-desc)
        (assoc-in [:backward to] reverse-dest-desc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def inf 999999)

; default dijksta algorithm
(defn dijkstra [g src dst]
  (loop [dists (assoc (zipmap (keys g) (repeat inf)) src 0)
         curr src ;
         ns (keys (get g curr)) ; available moves
         unvisited (apply hash-set (keys g))
         p (zipmap (keys g) (repeat nil))] ; paths
    (if (empty? unvisited) ; if all cities are visited, shortest path is found
      [(get dists dst),
       (loop [path []
              t dst]
         (if (= t src)
           (cons src path)
           (recur (cons t path) (get p t))))]
      (if (empty? ns)
        (let [u (disj unvisited curr)
              next-n (first (sort-by #(get dists %) u))]
          (recur dists next-n (filter #(some (fn [s] (= s %)) u) (keys (get g next-n))) u p))
        (let [cdst (get dists curr) ; calculate new distance and update dists
              idist (get dists (first ns))
              sum (+ cdst (get (get g curr) (first ns)))
              result (if (< sum idist)
                       (assoc dists (first ns) sum)
                       dists)
              path (if (< sum idist) ; update path if shorter one is found
                     (assoc p (first ns) curr)
                     p)]
          (recur result curr (rest ns) unvisited path))))))

  "Reconstruct map to common representation:
  {A {B 10, C 20}, C {A 20}, B {A 10}}
  (map<city,map<city,price>>)"
(defn simplify-graph [graph]
  (let [g (get graph :forward)]
    (zipmap
     (keys g)
     (map #(zipmap
            (keys %)
            (map
             (fn [param1] (if
                            (> (deref (param1 :tickets)) 0)
                            (get param1 :price)
                            9999)) (vals %))) (vals g))))) ; if no tickets, price is set too high

;;implementation must be pure functional besides the transaction itself, tickets reference modification and
;;restarts monitoring (atom could be used for this)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn book-tickets
  "Tries to book tickets and decrement appropriate references in route-map atomically
   returns map with either :price (for the whole route) and :path (a list of destination names) keys
          or with :error key that indicates that booking is impossible due to lack of tickets"
  [route-map from to]
  (if (= from to)
    {:path '(), :price 0} ; if already there, there is no need to travel
    (let [graph (simplify-graph route-map)
          path (dijkstra graph from to)]
      (try
        (dosync ; start transaction
         (loop [p (second path)] ; loop through the path
           (if (< (count p) 3)
             nil ; exit loop
             (let [r (:tickets (get (get (:forward route-map) (first p)) (second p)))]
               (do
                 (alter r dec) ; decrease tickets count, if there were no tickets, exception is thrown
                 (recur (next p))))))) ; move to the next flight
        {:path (second path), :price (first path)} ;
        (catch Exception e {:error e}))))) ; caught exception -> abort transaction and return error

;;;cities
(def spec1
  (-> empty-map
      (route "City1" "Capital" 200 5)
      (route "a" "Capital" 200 5)
      (route "Capital" "City1" 250 5)
      (route "City2" "Capital" 200 5)
      (route "Capital" "City2" 250 5)
      (route "City3" "Capital" 300 3)
      (route "Capital" "City3" 400 3)
      (route "City1" "Town1_X" 50 2)
      (route "Town1_X" "City1" 150 2)
      (route "Town1_X" "TownX_2" 50 2)
      (route "TownX_2" "Town1_X" 150 2)
      (route "Town1_X" "TownX_2" 50 2)
      (route "TownX_2" "City2" 50 3)
      (route "City2" "TownX_2" 150 3)
      (route "City2" "Town2_3" 50 2)
      (route "Town2_3" "City2" 150 2)
      (route "Town2_3" "City3" 50 3)
      (route "City3" "Town2_3" 150 2)))

(defn booking-future [route-map from to init-delay loop-delay]
  (future
   (Thread/sleep init-delay)
   (loop [bookings []]
     (Thread/sleep loop-delay)
     (let [booking (book-tickets route-map from to)]
       (if (booking :error)
         bookings
         (recur (conj bookings booking)))))))

(defn print-bookings [name ft]
  (println (str name ":") (count ft) "bookings")
  (doseq [booking ft]
    (println "price:" (booking :price) "path:" (booking :path))))

(defn run []
  ;;try to tune timeouts in order to all the customers gain at least one booking 
  (let [f1 (booking-future spec1 "City1" "City3" 0 1),
        f2 (booking-future spec1 "City1" "City2" 100 1),
        f3 (booking-future spec1 "City2" "City3" 100 1)]
    (print-bookings "City1->City3:" @f1)
    (print-bookings "City1->City2:" @f2)
    (print-bookings "City2->City3:" @f3)
    ))

(defn -main
  [& args]
  (run)
  (Thread/sleep 2000)
  (shutdown-agents))
