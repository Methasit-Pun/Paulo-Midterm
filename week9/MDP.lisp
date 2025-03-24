
;states
(defun flu ())
(defun allergy ())
(defun healthy ())

;actions
(defun flu-medicine ())
(defun allergy-medicine ())

;Hidden Markov Model for health
(defun state-update (state action)
	(let
		(
			(chance (random 100))
		)
		(cond
			((eql state `flu) 
				(cond
					((eql action `flu-medicine) (if (< chance 10) 
													`flu
													(if (< chance 40)
														`allergy
														`healthy
													) 
												)
					)
					;allergy medicine
					(t 							(if (< chance 90) 
													`flu
													`healthy 
												)
					) 
				)
			)
			;allergy
			(t (cond
					((eql action `flu-medicine) (if (< chance 99) 
													`allergy
													`healthy 
												)
					)
					;allergy medicine
					(t 							(if (< chance 15) 
													`flu
													(if (< chance 5)
														`allergy
														`healthy
													) 
												)
					) 
				)
			) 
		) 
	)
)

;observations
(defun runny-nose ())
(defun headache ())
(defun fever ())

;Observation Model for flu
(defun observation (state)
	(let
		(
			(chance (random 100))
		)
		(cond
			;flu
			((eql state `flu) 	(if (< chance 20) 
									`runny-nose
									(if (< chance 90)
										`fever
										`headache
									) 
								)
			)
			;allergy
			(t 					(if (< chance 80) 
									`runny-nose
									(if (< chance 90)
										`fever
										`headache
									) 
								)
			) 
		) 
	)
)



;P(X1 โฃ e1) = ฮฑ P(e1 โฃ X1)(P(X1 โฃ x0 = flu) p(x0) + P(X1 โฃ x0 = allergy) p(!x0))
; p(x0) = car prior
; p(!x0) =  car (cdr prior)
(defun HMM (prior observation last-action)
	(let
		(
			;P(e1 โฃ X1)
			(p-e-given-state 	(if (eql observation `runny-nose)
									(list 0.9 0.2)
									(if (eql observation `fever)
										(list 0.7 0.1)
										(list 0.1 0.1)
									)
								))
			;(P(X1 โฃ x0 = true)
			(p-health-given-flu 	(if (eql last-action `flu-medicine)
										(list 0.1 0.3)
										(list 0.9 0.0)
									))
			;(P(X1 โฃ x0 = false)
			(p-health-given-allergy (if (eql last-action `flu-medicine)
										(list 0.0 0.99)
										(list 0.15 0.05)
									))
			;p(x0 = flu)
			(prior-flu (car prior))
			;p(x0 = allergy)
			(prior-allergy (car (cdr prior)))
		) 
		(let
			(
				(posterior 	
					(list 
						(* (car p-e-given-state) (+ (* (car p-health-given-flu) prior-flu) (* (car p-health-given-allergy) prior-allergy)))
						(* (car (cdr p-e-given-state)) (+ (* (car (cdr p-health-given-flu)) prior-flu) (* (car (cdr p-health-given-allergy)) prior-allergy)))
				))
			)
			;normalize
			(list 	(/  (car posterior) (+ (car posterior) (car (cdr posterior))))
					(/  (car (cdr posterior)) (+ (car posterior) (car (cdr posterior))))
			) 
		)
	)
)

(defun MDP (posterior)
	(let
		(
			(p-flu (car posterior))
			(p-allergy (car (cdr posterior)))
		)
		(if (> p-flu p-allergy)
			(progn
				(format t ", Action:FM)->")
				`flu-medicine
			)
			(progn
				(format t ", Action:AM)->")
				`allergy-medicine
			)
		) 
	)
)


(defun run-until-healthy (state prior action)
	;(format t "(State:~a" state)
	(let
		(
			(new-state (state-update state action))
		) 
		(if (eql new-state `healthy) 
			(progn
				(format t "(State:HEALTHY)~%") 
				(return-from run-until-healthy)
			)
		)
		(let
			(
				(new-observation (observation new-state))
				(new-guess nil)
			)
			(let
				(
					(posterior (HMM prior new-observation action))
				)
				(format t "~%(State:~a, Obs:~a, Guess:~a" new-state new-observation posterior)
				;(format t "   Guess: ~a~%" posterior)
				(run-until-healthy new-state posterior (MDP posterior)) 
			)
		)
	)
)	



(defun run-N-times (N)
	(unless (eql N 0)
		(format t "(State:~a, Action:~a)->" `flu `flu-medicine)
		(run-until-healthy `flu (list 0.6 0.4) `flu-medicine)
		(format t "~%")
		(run-N-times (- N 1))
	)
)



(run-N-times 10)