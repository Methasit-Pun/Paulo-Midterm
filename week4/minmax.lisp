
;all possible moves legality
(defun is-one-A-legal (piles)
	(unless (eql (nth 0 piles) 0)
		(return-from is-one-A-legal t)
	)
	nil
)
(defun is-all-A-legal (piles)
	(unless (eql (nth 0 piles) 0)
		(return-from is-all-A-legal t)
	)
	nil
)
(defun is-all-but-one-A-legal (piles)
	(when (> (nth 0 piles) 1)
		(return-from is-all-but-one-A-legal t)
	)
	nil
)
(defun is-one-B-legal (piles)
	(unless (eql (nth 1 piles) 0)
		(return-from is-one-B-legal t)
	)
	nil
)
(defun is-all-B-legal (piles)
	(unless (eql (nth 1 piles) 0)
		(return-from is-all-B-legal t)
	)
	nil
)
(defun is-all-but-one-B-legal (piles)
	(when (> (nth 1 piles) 1)
		(return-from is-all-but-one-B-legal t)
	)
	nil
)
(defun is-one-C-legal (piles)
	(unless (eql (nth 2 piles) 0)
		(return-from is-one-C-legal t)
	)
	nil
)
(defun is-all-C-legal (piles)
	(unless (eql (nth 2 piles) 0)
		(return-from is-all-C-legal t)
	)
	nil
)
(defun is-all-but-one-C-legal (piles)
	(when (> (nth 2 piles) 1)
		(return-from is-all-but-one-C-legal t)
	)
	nil
)

;all possible moves
(defun one-A (piles)
	(list (- (nth 0 piles) 1) (nth 1 piles) (nth 2 piles))
)
(defun all-A (piles)
	(list 0 (nth 1 piles) (nth 2 piles))
)
(defun all-but-one-A (piles)
	(list 1 (nth 1 piles) (nth 2 piles))
)
(defun one-B (piles)
	(list (nth 0 piles) (- (nth 1 piles) 1) (nth 2 piles))
)
(defun all-B (piles)
	(list (nth 0 piles) 0 (nth 2 piles))
)
(defun all-but-one-B (piles)
	(list (nth 0 piles) 1 (nth 2 piles))
)
(defun one-C (piles)
	(list (nth 0 piles) (nth 1 piles) (- (nth 2 piles) 1) )
)
(defun all-C (piles)
	(list (nth 0 piles) (nth 1 piles) 0)
)
(defun all-but-one-C (piles)
	(list (nth 0 piles) (nth 1 piles) 1)
)


;check for endgame
(defun end (piles)
	(dolist (x piles)
		(if (not (eql x 0))
			(return-from end nil)
		)	
	)
	t
)

(defconstant *actions* (list 	(list 'is-one-A-legal 'one-A)
								(list 'is-all-A-legal 'all-A)
								(list 'is-all-but-one-A-legal 'all-but-one-A)
								(list 'is-one-B-legal 'one-B)
								(list 'is-all-B-legal 'all-B)
								(list 'is-all-but-one-B-legal 'all-but-one-B)
								(list 'is-one-C-legal 'one-C)
								(list 'is-all-C-legal 'all-C)
								(list 'is-all-but-one-C-legal 'all-but-one-C)
						))

(defun minimax (piles)
	;(format t "Minimax ~a~%" piles)
	(let 
		(
			(finished 	(end piles))
		)
		(unless finished
			(dolist (action *actions*)
				(if (funcall (car action) piles)
					(if (eql (maximin (transition piles (car (cdr action)))) 0)
						(return-from minimax 0)
					)
				)
			)
		)
	)
	1
)

(defun maximin (piles)
	;(format t "Maximin ~a~%" piles)
	(let 
		(
			(finished 	(end piles))
		)
		(unless finished
			(dolist (action *actions*)
				(if (funcall (car action) piles)
					(if (eql (minimax (transition piles (car (cdr action)))) 1)
						(return-from maximin 1)
					)
				)
			)
		)
	)
	0
)


;will perform minimax
(defun agent (piles)
	(dolist (action *actions*)
		(when (funcall (car action) piles)
			;(format t "~a legal~%" (car (cdr action)))
			(if (eql (minimax (transition piles (car (cdr action)))) 1)
				(return-from agent (transition piles (car (cdr action))))
			)
		)
	)
	(format t "Gave up~%")
	piles
)

(defun transition (piles action)
	(funcall action piles)
)

(defun nim-sim (pile)
	(format t "~a~%" (agent pile))
)


(defun readarray (&rest args)
            (values (read-from-string
                     (concatenate 'string "("
                                  (apply #'read-line args)
                                  ")"))))

(nim-sim (readarray))