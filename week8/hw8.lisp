(defun state-update (state)
  "Updates the traffic state based on transition probabilities."
  (cond
    ((eql state 'traffic) (if (< (random 100) 70) 'traffic 'no-traffic))
    (t (if (< (random 100) 30) 'traffic 'no-traffic))))

(defun observation (state)
  "Generates an observation (late or not-late) based on traffic state."
  (cond
    ((eql state 'traffic) (if (< (random 100) 90) 'late 'not-late))
    (t (if (< (random 100) 20) 'late 'not-late))))

(defun DBN (prior observation)
  "Dynamic Bayesian Network filtering step."
  (let* ((p-e-given-state (if (eql observation 'late) '(0.9 0.2) '(0.1 0.8)))
         (p-traffic-given-traffic '(0.7 0.3))
         (p-traffic-given-no-traffic '(0.3 0.7))
         (prior-traffic (car prior))
         (prior-no-traffic (cadr prior))
         (posterior (list 
                     (* (car p-e-given-state) 
                        (+ (* (car p-traffic-given-traffic) prior-traffic) 
                           (* (car p-traffic-given-no-traffic) prior-no-traffic)))
                     (* (cadr p-e-given-state) 
                        (+ (* (cadr p-traffic-given-traffic) prior-traffic) 
                           (* (cadr p-traffic-given-no-traffic) prior-no-traffic))))))
    ;; Normalize probabilities
    (list (/ (car posterior) (+ (car posterior) (cadr posterior)))
          (/ (cadr posterior) (+ (car posterior) (cadr posterior))))))

(defun run-db-n-times (state prior N)
  "Runs the Dynamic Bayesian Network for N time steps."
  (when (> N 0)
    (let* ((new-state (state-update state))
           (new-observation (observation new-state))
           (new-posterior (DBN prior new-observation)))
      (format t "Time step ~a: State ~a | Observation ~a | Belief ~a~%" 
              (- 21 N) new-state new-observation new-posterior)
      (run-db-n-times new-state new-posterior (1- N)))))

;; Run the DBN for 20 time steps
(run-db-n-times 'traffic '(0.8 0.2) 20)
