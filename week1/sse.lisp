(defun make-environment ()
	(list :x 0 :y 0 :grid (list (list 1 0 0 0) (list 1 0 1 0) (list 1 1 1 1) (list 1 0 0 1)))
)

(defun print-environment (env)
	(let
		(
			(grid (getf env :grid))
		) 
		(format t "~a~%" (nth 3 grid))
		(format t "~a~%" (nth 2 grid))
		(format t "~a~%" (nth 1 grid))
		(format t "~a" (nth 0 grid))
	)
	(format t " Agent at ~a" (getf env :x))
	(format t ",~a"  (getf env :y))
	(format t " ~%~%")
)

(defun move-up (env)
	(let ((y (getf env :y)))
		(list :x (getf env :x) :y (if (< y 3) (+ y 1) y) :grid (getf env :grid))
	)
)

(defun move-down (env)
	(let ((y (getf env :y)))
		(list :x (getf env :x) :y (if (> y 0) (- y 1) y) :grid (getf env :grid))
	)
)

(defun move-right (env)
	(let ((x (getf env :x)))
		(list :x (if (< x 3) (+ x 1) x) :y (getf env :y) :grid (getf env :grid))
	)
)

(defun move-left (env)
	(let ((x (getf env :x)))
		(list :x (if (> x 0) (- x 1) x) :y (getf env :y) :grid (getf env :grid))
	)
)

(defun move-cut (env)
	(let 
		(
			(grid (getf env :grid))
			(row-mask (list 0 0 0 0))
			(column-mask (list 0 0 0 0))
		)
		(setf (nth (getf env :x) row-mask) 1)
		(setf (nth (getf env :y) column-mask) 1)
		(let
			(
				(after-cut (mapcar 	(lambda (a b)
										(if (eql b 1)
											(mapcar 	(lambda (c d)
														(if (eql d 1)
															0
															c
													)) a row-mask)
											a
									)) grid column-mask))
			)
			(list :x (getf env :x) :y (getf env :y) :grid after-cut)
		)
	)
)


(defun build-tree (env depth)
	(if (eql depth 0) 
		;then
		nil 
		;else
		(let 
			(
				(x_pos (getf env :x))
				(y_pos (getf env :y))
			)
			(list 	env
					(if (< y_pos 3) (build-tree (move-up env) (- depth 1)) nil) 
					(if (> y_pos 0) (build-tree (move-down env) (- depth 1)) nil)
					(if (< x_pos 3) (build-tree (move-right env) (- depth 1)) nil)
					(if (> x_pos 0) (build-tree (move-left env) (- depth 1)) nil)
					(build-tree (move-cut env) (- depth 1))
			)
		)
	)
)

(defun sum-row (row)
	(+ (nth 0 row) (nth 1 row)(nth 2 row) (nth 3 row))
)

(defun sum-grid (grid)
	(+ (sum-row (nth 0 grid)) (sum-row (nth 1 grid)) (sum-row (nth 2 grid)) (sum-row (nth 3 grid)))
)

(defun utility (env)
	(when (eql env nil)
		(return-from utility 0))
	(let
		(
			(grid (getf env :grid))
		)
		(- 16 (sum-grid grid))
	)
)

(defun map-utility (tree)
	(when (eql tree nil)
		(return-from map-utility))
	(list (utility (car tree)) 
		(map-utility (nth 1 tree)) 
		(map-utility (nth 2 tree)) 
		(map-utility (nth 3 tree)) 
		(map-utility (nth 4 tree)) 
		(map-utility (nth 5 tree)))
)


(defun nth-of-biggest (terms)
	(let 
		(
			(n 0)
		)
		(if (>=  (nth 1 terms) (nth n terms))
			(setf n 1))
		(if (>=  (nth 2 terms) (nth n terms))
			(setf n 2))
		(if (>=  (nth 3 terms) (nth n terms))
			(setf n 3))
		(if (>=  (nth 4 terms) (nth n terms))
			(setf n 4))
		(if (>=  (nth 5 terms) (nth n terms))
			(setf n 5))
		n
	)
)

(defun highest-of (terms)
	(if (eql terms nil) (return-from highest-of 0))
	(if (> (car terms) (highest-of (cdr terms)))
		(car terms)
		(highest-of (cdr terms))
	)
)

(defun propagate-utility-recursive (tree)
	(if (eql tree nil) (return-from propagate-utility-recursive 0))
	(let
		(
			(terms 	(list 	(car tree)
						(propagate-utility-recursive (nth 1 tree)) 
						(propagate-utility-recursive (nth 2 tree)) 
						(propagate-utility-recursive (nth 3 tree)) 
						(propagate-utility-recursive (nth 4 tree)) 
						(propagate-utility-recursive (nth 5 tree))
					)
			)
		)
		;find highest of 6 terms
		(highest-of terms) 
	)
)

(defun propagate-utility (tree)
	(list 
		(car tree)
		(propagate-utility-recursive (nth 1 tree))
		(propagate-utility-recursive (nth 2 tree))
		(propagate-utility-recursive (nth 3 tree))
		(propagate-utility-recursive (nth 4 tree))
		(propagate-utility-recursive (nth 5 tree))
		)
)

(defun get-action-highest-utility (tree)
	(let 
		(
			(propagated-tree (propagate-utility tree))
		)
		(nth-of-biggest propagated-tree)
	)
)


(defconstant +DEPTH+ 3)


(defun agent (env) 
	;build state evolution tree
	(let 
		(
			(tree (build-tree env +DEPTH+))
		)
		(let
			(
				(utility-tree (map-utility tree))
			)
			;find action that leads to highest node
			(let
				(
					(action-number (get-action-highest-utility utility-tree))
				)
				(let
					(
						(action (nth action-number (list () #\U #\D #\L #\R #\C)))
					)
					(format t "Agent decided ~a~%" action)
					action 
				)
			)
		)
	)
)


(defun transition (env) 
	;applies return action of agent to environment
	(let 
		(
			;get agent action
			(action (agent env))
		)
		(let 
			(
				(new-env 	(case action
								(#\U (move-up env))
								(#\D (move-down env))
								(#\L (move-left env))
								(#\R (move-right env))
								(#\C (move-cut env))
							))
			)
			(format t "Current utility ~a~%~%" (utility new-env))
			new-env
		)
	)
)

(defun run-N-times (env N) 
	(print-environment env)
	(if (eql N 1) 
		;then
		(transition env)
		;else
		(run-N-times (transition env) (- N 1))
	)
)


(defun sim-agent () 
	(format t "starting... ~%")	
	(let 	
		(
			;initialize environment
			(env (make-environment))
		)
		;(format t "~a~%" env)
		(print-environment env)
		(format t "Initial utility is ~a~%" (utility env))
		;run agent 10 times
		(run-N-times env 10)
	)
)


(sim-agent)