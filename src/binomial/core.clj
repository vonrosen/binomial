(ns binomial.core
  (:gen-class)
  (:require [clojure.pprint :as pp]))

(def defaultX 100)
(def default-strike-price 110)
(def defaultu 1.4) ;;up factor
(def defaultd 0.6) ;;down factor
(def defaultr 0.05) ;; risk free rate of return
(def default-total-time-periods 3) ;;levels in binomial tree, not including root
(def total-time-periods default-total-time-periods)

(defn discount-factor [r t]  
  (Math/pow Math/E (* -1 r t)))

(def calculate-q
  (fn [X r t u d]        
    (/ (- (discount-factor r t) d) (- u d))))

(defn build-empty-tree []
  {:left nil :right nil :payoff nil :X nil})

(defn build-tree [tree time-periods]
  (let [new-tree (assoc tree :left (build-empty-tree) :right (build-empty-tree))]
    (if (> time-periods 1)
      (let [new-tree-left (build-tree (:left new-tree) (- time-periods 1))
            new-tree-right (build-tree (:right new-tree) (- time-periods 1))]
        (assoc new-tree :left new-tree-left :right new-tree-right))
      new-tree)))

(defn calculate-leaf-payoff [is-call X strike-price u d number-of-ups number-of-downs]
  (let [future-X-price (* (Math/pow d number-of-downs) (* (Math/pow u number-of-ups) X))]
    (if is-call
      (max (- future-X-price strike-price) 0)
      (max (- strike-price future-X-price) 0))))

(defn leaf? [tree]
  (nil? (:left tree)))

(defn calculate-future-X [X u d number-of-ups number-of-downs]
  (* (Math/pow d number-of-downs) (* (Math/pow u number-of-ups) X)))

(defn calculate-leaves [is-call tree X strike-price u d number-of-ups number-of-downs]
  (if (leaf? tree)
    (assoc tree :payoff (calculate-leaf-payoff is-call X strike-price u d number-of-ups number-of-downs) 
           :X (calculate-future-X X u d number-of-ups number-of-downs))
    (do
      (let [left (calculate-leaves is-call (:left tree) X strike-price u d (+ number-of-ups 1) number-of-downs)
            right (calculate-leaves is-call (:right tree) X strike-price u d number-of-ups (+ number-of-downs 1))]
        (assoc tree :left left :right right)))))

(defn calculate-payoff [r t q payoff-up payoff-down]
  (* (discount-factor r t) (+ (* q payoff-up) (* (- 1 q) payoff-down))))

(defn calculate-root-c [tree r t q]
  (if (and 
        (nil? (:payoff tree))
        (and (not (nil? (:payoff (:left tree))))
             (not (nil? (:payoff (:right tree))))))
    (assoc tree :payoff (calculate-payoff r t q (:payoff (:left tree)) (:payoff (:right tree))))
    (do 
      (if (nil? (:payoff (:left tree)))
        (let [new-tree (assoc tree :left (calculate-root-c (:left tree) r t q))]
          (if (nil? (:payoff (:right new-tree)))
            (let [new-tree (assoc new-tree :right (calculate-root-c (:right new-tree) r t q))]
              (calculate-root-c new-tree r t q))                                  
            tree))))))

(defn calculate-c  
  ([is-call X strike-price r t u d n]
    (if is-call (prn "pricing call option") (prn "pricing put option"))
    (let [q (calculate-q X r t u d)          
          binomial-tree (build-tree (build-empty-tree) n)]
      (prn (str "q= " q))
      (calculate-c is-call binomial-tree X strike-price r t q u d)))
  ([is-call binomial-tree X strike-price r t q u d]
    (let [binomial-tree-with-leaves-populated (calculate-leaves is-call binomial-tree X strike-price u d 0 0)]
      (pp/pprint (calculate-root-c binomial-tree-with-leaves-populated r t q))      
      (prn (str "option value = " (:payoff (calculate-root-c binomial-tree-with-leaves-populated r t q)))))))

(defn -main [& args]  
  (if (not (empty? args))
    (let [is-call (Boolean/valueOf (nth args 0))
         X (Long/valueOf (nth args 1))
         strike-price (Long/valueOf (nth args 2))
         r (Double/valueOf (nth args 3))
         t (Double/valueOf (nth args 4))
         u (Double/valueOf (nth args 5))
         d (Double/valueOf (nth args 6))
         n (Integer/valueOf (nth args 7))]
      (calculate-c is-call X strike-price r t u d n))))