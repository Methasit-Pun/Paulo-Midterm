;symbols for "stock"
(defun up ()) (defun down ())
;symbols for "sales"
(defun s1()) (defun s2()) (defun s3())
;symbols for "bonus"
(defun big ()) (defun small ())
;symbols for "gift"
(defun expensive ()) (defun cheap ())
;symbols for "interest"
(defun high ()) (defun low ())

(defclass histogram () 
	(
		;sales
		s1
		s2
		s3
		;stock
		up
		down
		;bonus
		big
		small
		;interest
		high
		low
		;gift
		expensive
		cheap
	)
)

(defun BN-result (hist)
	(let 
		(
			(s1 (slot-value hist `s1))
			(s2 (slot-value hist `s2))
			(s3 (slot-value hist `s3))
			(up (slot-value hist `up))
			(down (slot-value hist `down))
			(big (slot-value hist `big))
			(small (slot-value hist `small))
			(high (slot-value hist `high))
			(low (slot-value hist `low))
			(expensive (slot-value hist `expensive))
			(cheap (slot-value hist `cheap))
		)
		(format t "Sales~%")
		(format t "   s1: ~a s2: ~a s3: ~a~%" (/ s1 (+ s1 s2 s3)) (/ s2 (+ s1 s2 s3)) (/ s3 (+ s1 s2 s3)))
		(format t "Stocks~%")
		(format t "   up: ~a down: ~a~%" (/ up (+ up down)) (/ down (+ up down)))
		(format t "CEO Bonus~%")
		(format t "   big: ~a small: ~a~%" (/ big (+ big small)) (/ small (+ big small)))
		(format t "Trader interest~%")
		(format t "   high: ~a low: ~a~%" (/ high (+ high low)) (/ low (+ high low)))
		(format t "Gift to husband~%")
		(format t "   expensive: ~a cheap: ~a~%~%" (/ expensive (+ expensive cheap)) (/ cheap (+ expensive cheap)))
	)
)


(defun update-histogram (result state)
	(cond
		((equal (nth 0 state) `up) 		(setf (slot-value result `up) (+ (slot-value result `up) 1)))
		((equal (nth 0 state) `down) 	(setf (slot-value result `down) (+ (slot-value result `down) 1)))
	)
	(cond
		((equal (nth 1 state) `s1) (setf (slot-value result `s1) (+ (slot-value result `s1) 1)))
		((equal (nth 1 state) `s2) (setf (slot-value result `s2) (+ (slot-value result `s2) 1)))
		((equal (nth 1 state) `s3) (setf (slot-value result `s3) (+ (slot-value result `s3) 1)))
	)
	(cond
		((equal (nth 2 state) `big) 	(setf (slot-value result `big) (+ (slot-value result `big) 1)))
		((equal (nth 2 state) `small) 	(setf (slot-value result `small) (+ (slot-value result `small) 1)))
	)
	(cond
		((equal (nth 3 state) `expensive) 	(setf (slot-value result `expensive) (+ (slot-value result `expensive) 1)))
		((equal (nth 3 state) `cheap) 		(setf (slot-value result `cheap) (+ (slot-value result `cheap) 1)))
	)
	(cond
		((equal (nth 4 state) `high) 	(setf (slot-value result `high) (+ (slot-value result `high) 1)))
		((equal (nth 4 state) `low) 	(setf (slot-value result `low) (+ (slot-value result `low) 1)))
	)
)





(defun MC-generate-recursive (rnd acc dist val)
	(when (eql dist nil)
		(format t "Error: was passed non-normalized distribution~%")
		(return-from MC-generate-recursive nil)
	)
	(if (< rnd (+ acc (car dist)))
		(car val)
		(MC-generate-recursive rnd (+ acc (car dist)) (cdr dist) (cdr val))
	)
)

;given a probability distribution, generates a random sample
;"val" is a list of possible values
;"dist" is the corresponding distribution for those values
(defun MC-generate (val dist)
	(MC-generate-recursive (/ (random 100) 100.0) 0 dist val)
)


;given a non-normalized distribution, normalizes it
(defun normalize (dist)
	(let 
		(
			(acc 0)
		)
		(dolist (x dist)
			(setf acc (+ acc x))
		)
		(mapcar (lambda (lst) (/ lst acc) ) dist)
	)
)

;(format t "~a~%" (normalize (list 1 2 3)))
(defun return-random-element (lst)
	(nth (random (list-length lst)) lst)
)

(defvar *values* (list 	(list `up `down)
						(list `s1 `s2 `s3)
						(list `big `small)
						(list `expensive `cheap)
						(list `high `low)
					)
)


(defun Stocks (val)
	(cond
		((eql val `up) 	0.6)
		(t 				0.4) 
	)
)

(defun Sales (val)
	(cond
		((eql val `s1) 	0.2)
		((eql val `s1) 	0.5)
		(t 				0.3) 
	)
)

(defun Interest (val stocks)
	(cond
		((and (eql val `high) (eql stocks `up)) 	0.8)
		((and (eql val `high) (eql stocks `down)) 	0.3)
		((and (eql val `low)  (eql stocks `up)) 	0.2)
		(t 				0.7) 
	)
)

(defun Gift (val bonus)
	(cond
		((and (eql val `expensive) (eql bonus `big)) 	0.9)
		((and (eql val `cheap) (eql bonus `big)) 	0.1)
		((and (eql val `expensive) (eql bonus `small)) 	0.5)
		(t 				0.5) 
	)
)


(defun Bonus (val stocks sales)
	(cond
		((and (eql val `big) (eql stocks `up) (eql sales `s1)) 	0.6)
		((and (eql val `big) (eql stocks `up) (eql sales `s2)) 	0.7)
		((and (eql val `big) (eql stocks `up) (eql sales `s3)) 	0.9)
		((and (eql val `small) (eql stocks `up) (eql sales `s1)) 	0.4)
		((and (eql val `small) (eql stocks `up) (eql sales `s2)) 	0.3)
		((and (eql val `small) (eql stocks `up) (eql sales `s3)) 	0.1)
		((and (eql val `big) (eql stocks `down) (eql sales `s1)) 	0.2)
		((and (eql val `big) (eql stocks `down) (eql sales `s2)) 	0.3)
		((and (eql val `big) (eql stocks `down) (eql sales `s3)) 	0.4)
		((and (eql val `small) (eql stocks `down) (eql sales `s1)) 	0.8)
		((and (eql val `small) (eql stocks `down) (eql sales `s2)) 	0.7)
		(t 				0.6) 
	)
)

;given a node, return the conditional probability distribution given its Markov blanket
(defun MBlanket (node state)
	(let
		(
			(stocks 	(nth 0 state))
			(sales 		(nth 1 state))
			(bonus 		(nth 2 state))
			(gift 		(nth 3 state))
			(interest 	(nth 4 state))
		) 
		(normalize 
			(cond
			((equal node `Stocks) 	(list 	(* (Stocks `up) (Bonus bonus `up sales) (Interest interest `up)) 
											(* (Stocks `down) (Bonus bonus `down sales) (Interest interest `down))))  
			((equal node `Sales) 	(list 	(* (Sales `s1) (Bonus bonus stocks `s1)) 
											(* (Sales `s2) (Bonus bonus stocks `s2)) 
											(* (Sales `s3) (Bonus bonus stocks `s3))))
			((equal node `Bonus) 	(list 	(* (Bonus `big stocks sales) (Gift gift `big))
											(* (Bonus `small stocks sales) (Gift gift `small))))
			((equal node `Gift) 	(list 	(Gift `expensive bonus)
											(Gift `cheap bonus)))
			(t 						(list 	(Interest `high stocks)
											(Interest `low stocks)));Interest 
			)
		)
	)
)






(defconstant *N* 1000);number of iterations for MCMC


;format is (nil nil `Bonus `Gift nil) (non-evidence values are nil) for evidence-vars
;same thing for initial value of evidence (vals)
(defun MCMC (evidence-vars evidence-vals)
	(let
		(
			(BN (list `Stocks `Sales `Bonus `Gift `Interest))
			(state (list nil nil nil nil nil))
			(result (make-instance `histogram)) ;histogram to store results

		)
		;clear initial histogram
		(setf (slot-value result `s1) 0)
		(setf (slot-value result `s2) 0)
		(setf (slot-value result `s3) 0)
		(setf (slot-value result `up) 0)
		(setf (slot-value result `down) 0)
		(setf (slot-value result `big) 0)
		(setf (slot-value result `small) 0)
		(setf (slot-value result `high) 0)
		(setf (slot-value result `low) 0)
		(setf (slot-value result `expensive) 0)
		(setf (slot-value result `cheap) 0)
		;initialize MC state
		(dotimes (x 5)
			(if (not (nth x evidence-vars))
				(setf (nth x state) (return-random-element (nth x *values*)))
				(setf (nth x state) (nth x evidence-vals))
			)
		)
		;do MCMC N times
		(dotimes (i *N*)
			(dotimes (x 5)
				;(format t "~a~%" state)
				(if (not (nth x evidence-vars))
					(progn
						;(format t "updating...~%")
						;update given other variables' values
						(setf (nth x state) (MC-generate (nth x *values*) (MBlanket (nth x BN) state))); calculate the probability of the node given the Markov blanket
						(update-histogram result state)
					)
				)
			)
		)
		(BN-result result)
	)
)
; represent conditional probability distribution of each node
; Updating the value of makov chain each point update the histogram
; 

;(MCMC (list nil nil `Bonus `Gift nil) (list nil nil `small `cheap nil))
;(MCMC (list nil nil `Bonus `Gift nil) (list nil nil `small `expensive nil))
(MCMC (list nil nil `Bonus `Gift nil) (list nil nil `big `expensive nil))
;(MCMC (list nil nil nil `Gift nil) (list nil nil nil `cheap nil))
; sbcl --script class7.lisp
;multi calrov chain monte carlo
; markov chain only need the previous state to based on so it effiecient and small even though there are 1000 loops 1000 lines