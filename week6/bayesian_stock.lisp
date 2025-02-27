(defun make-initial-stocks ()
	(list  100  120  110  60  100  120  110  90) 
)
; We'll treat first 4 as tech, last 4 as non-tech

(defun make-initial-belief ()
	(let 
		(
			(x (list 50.0 50.0))
		)
		(list x x x x x x x x x) 
	)
)

(defun tech-update ()
	(let 
		(
			(chance (random 100))
		)
		(if (< chance 64) 1 -1)
	)
)

(defun non-tech-update ()
	(let 
		(
			(chance (random 100))
		)
		(if (< chance 16) 1 -1)
	)
)

(defun stock-update-array ()
	(list (tech-update) (tech-update) (tech-update) (tech-update) (non-tech-update) (non-tech-update) (non-tech-update) (non-tech-update))
)





(defun get-first-half (stocks)
	(list (nth 0 stocks) (nth 1 stocks) (nth 2 stocks) (nth 3 stocks))
)

(defun get-second-half (stocks)
	(list (nth 4 stocks) (nth 5 stocks) (nth 6 stocks) (nth 7 stocks))
)

(defun update-stocks (stocks update)
	(mapcar '+ stocks update)
)

;given a prior and evidence, returns a normalized posterior
(defun bayesian (prior e)
	(let
		(
			(p-e-given-h		0.64) ;likelihood evidence, given hypothesis true
			(p-note-given-h		0.36) ;likelihood not evidence, given hypothesis true
			(p-e-given-noth		0.16) ;likelihood evidence, given hypothesis false
			(p-note-given-noth	0.84) ;likelihood not evidence, given hypothesis false
			(p-e 				0.4)  ;marginal evidence 
			(p-note 			0.6)  ;marginal not evidence 
			(p-h	(car prior))  ;prior hypothesis true
			(p-noth	(car (cdr prior)))  ;prior hypothesis false
		) 
		(let
			(
				(p-h-given-e 		(/ (* p-e-given-h 		p-h) 	p-e))
				(p-h-given-note 	(/ (* p-note-given-h 	p-h) 	p-note))
				(p-noth-given-e 	(/ (* p-e-given-noth 	p-noth) p-e))
				(p-noth-given-note 	(/ (* p-note-given-noth p-noth) p-note))
			)
			(if (eql e 1) 
				;true
				(list (/ p-h-given-e (+ p-h-given-e p-noth-given-e)) (/ p-noth-given-e (+ p-h-given-e p-noth-given-e)))
				;false
				(list (/ p-h-given-note (+ p-h-given-note p-noth-given-note)) (/ p-noth-given-note (+ p-h-given-note p-noth-given-note)))
			)
		)
	)
)

(defun update-belief (belief update)
	(mapcar 'bayesian belief update)
)


(defun print-belief (belief)
	(format t "  ~a~%" (nth 0 belief))
	(format t "  ~a~%" (nth 1 belief))
	(format t "  ~a~%" (nth 2 belief))
	(format t "  ~a~%" (nth 3 belief))
	(format t "  ~a~%" (nth 4 belief))
	(format t "  ~a~%" (nth 5 belief))
	(format t "  ~a~%" (nth 6 belief))
	(format t "  ~a~%" (nth 7 belief))
)


(defconstant +ITERATIONS+ 10)

(defun update-N-times (stocks belief N)
	(let
		(
			(update (stock-update-array))
		) 
		(let
			(
				(new-stocks (update-stocks stocks update))
				(new-belief (update-belief belief update))
			)
			;(format t "   ~a~%" update)
			(format t "~a~%" new-stocks)
			(print-belief new-belief)
			(if (not (eql N 0)) (update-N-times new-stocks new-belief (- N 1))) 
		)
	)
)



(defun main ()
	(let 
		(
			(stocks (make-initial-stocks))
			(belief (make-initial-belief))
		) 
		(format t "~a~%" stocks)
		(print-belief belief)
		(update-N-times stocks belief +ITERATIONS+)
	)
)

(main)