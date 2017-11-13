(ns binomial.core)

(def defaultX 100)
(def default-strike-price 110)
(def defaultu 1.4) ;;up factor
(def defaultd 0.6) ;;down factor
(def defaultr 0.05) ;; risk free rate of return
(def default-total-time-periods 2) ;;levels in binomial tree, not including root
(def total-time-periods default-total-time-periods)

(defn discount-factor [r t]  
  (Math/pow Math/E (* -1 r t)))

(def calculate-q (memoize 
                   (fn [r t u d]
                     (/ (- (discount-factor r t) d) (- u d)))))

(defn build-empty-tree []
  {:left nil :right nil :payoff nil :id nil})

(defn build-tree [tree time-periods]
  (let [new-tree (assoc tree :left (build-empty-tree) :right (build-empty-tree))]
    (if (> time-periods 1)
      (let [new-tree-left (build-tree (:left new-tree) (- time-periods 1))
            new-tree-right (build-tree (:right new-tree) (- time-periods 1))]
        (assoc new-tree :left new-tree-left :right new-tree-right))
      new-tree)))

(defn calculate-leaf-payoff [X strike-price u d number-of-ups number-of-downs]
  (let [future-X-price (* (Math/pow d number-of-downs) (* (Math/pow u number-of-ups) X))]    
    #_(if (> strike-price future-X-price)
        (- strike-price future-X-price)
        0)
    (if (> strike-price future-X-price)
        (- strike-price future-X-price )
        0)
    
    ))

(defn leaf? [tree]
  (nil? (:left tree)))

(defn calculate-leaves [tree X strike-price u d number-of-ups number-of-downs]
  (if (leaf? tree)
    (assoc tree :payoff (calculate-leaf-payoff X strike-price u d number-of-ups number-of-downs))
    (do
      (let [left (calculate-leaves (:left tree) X strike-price u d (+ number-of-ups 1) number-of-downs)
            right (calculate-leaves (:right tree) X strike-price u d number-of-ups (+ number-of-downs 1))]
        (assoc tree :left left :right right)))))

(defn calculate-payoff [r t q payoff-up payoff-down]
  (* (discount-factor r t) (+ (* q payoff-up) (* (- 1 q) payoff-down))))

(defn calculate-root-c [tree r t q]
  (if (and 
        (nil? (:payoff tree))
        (and (not (nil? (:payoff (:left tree))))
             (not (nil? (:payoff (:right tree))))))
    (let [new-tree (assoc tree :payoff (calculate-payoff r t q (:payoff (:left tree)) (:payoff (:right tree))))]
      ;(prn (:payoff (:left tree)))
      ;(prn (:payoff (:right tree)))
      ;(clojure.pprint/pprint new-tree)
      ;;BUG! must take new-tree and put it back into original tree      
      (calculate-root-c new-tree r t q))
    (do 
      (if (nil? (:payoff (:left tree)))
        (calculate-root-c (:left tree) r t q) 
        (if (nil? (:payoff (:right tree)))
          (calculate-root-c (:right tree) r t q)
          tree)))))

(defn calculate-c 
  ([X strike-price r t u d]
    (let [q (calculate-q r t u d)
          binomial-tree (build-tree (build-empty-tree) default-total-time-periods)]
      (calculate-c binomial-tree X strike-price r t q u d)))
  ([binomial-tree X strike-price r t q u d]
    (let [binomial-tree-with-leaves-populated (calculate-leaves binomial-tree X strike-price u d 0 0)]
      (clojure.pprint/pprint binomial-tree-with-leaves-populated)      
      (:payoff (calculate-root-c binomial-tree-with-leaves-populated r t q)))))