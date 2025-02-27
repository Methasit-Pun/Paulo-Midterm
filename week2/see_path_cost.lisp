(defun make-environment ()
	(list :x 0 :y 0 :grid (list 
				(list (random 2) (random 2) (random 2) (random 2)) 
				(list (random 2) (random 2) (random 2) (random 2)) 
				(list (random 2) (random 2) (random 2) (random 2)) 
				(list (random 2) (random 2) (random 2) (random 2))))
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


(defun build-tree (env cost depth)
	(if (eql depth 0) 
		;then
		nil 
		;else
		(list 	:env env :cost cost
				:u (build-tree (move-up env) (+ cost (* 0.2 2))  (- depth 1)) 
				:d (build-tree (move-down env) (+ cost (* 0.2 2)) (- depth 1)) 
				:r (build-tree (move-right env) (+ cost (* 0.2 2)) (- depth 1))
				:l (build-tree (move-left env) (+ cost (* 0.2 2)) (- depth 1))
				:c (build-tree (move-cut env) (+ cost (* 0.2 1)) (- depth 1))
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
		(return-from utility (- 16 (sum-grid grid)))
	)
)

(defun map-utility (tree)
	(when (eql tree nil)
		(return-from map-utility))
	(list (- (utility (getf tree :env)) (getf tree :cost)) 
		(map-utility (getf tree :u)) 
		(map-utility (getf tree :d)) 
		(map-utility (getf tree :l)) 
		(map-utility (getf tree :r)) 
		(map-utility (getf tree :c)))
)


(defun nth-of-biggest (list)
	(let 
		(
			(*n* 0)
		)
		(if (> (car (nth 1 list)) (car (nth *n* list)))
			(setf *n* 1))
		(if (> (car (nth 2 list)) (car (nth *n* list)))
			(setf *n* 2))
		(if (> (car (nth 3 list)) (car (nth *n* list)))
			(setf *n* 3))
		(if (> (car (nth 4 list)) (car (nth *n* list)))
			(setf *n* 4))
		(+ *n* 1)
	)
)

(defun get-action-highest-utility (tree)
	(when (eql (car (cdr tree)) nil)
		(return-from get-action-highest-utility (list (car tree) #\C))
	)
	(let
		(
			(fixed-tree (list 	(car tree) 
								(get-action-highest-utility (nth 1 tree)) 
								(get-action-highest-utility (nth 2 tree)) 
								(get-action-highest-utility (nth 3 tree)) 
								(get-action-highest-utility (nth 4 tree)) 
								(get-action-highest-utility (nth 5 tree))
			)) 
		) 
		(list (car (nth (nth-of-biggest (cdr fixed-tree)) fixed-tree)) (nth (nth-of-biggest (cdr fixed-tree)) (list () #\U #\D #\L #\R #\C)))
		;fixed-tree
	)
)


(defconstant +DEPTH+ 5)

(defun agent (env) 
	;build state evolution tree
	(let 
		(
			(tree (build-tree env 0 +DEPTH+))
		)
		;find action that leads to highest node
		(let
			(
				(action (car (cdr (get-action-highest-utility (map-utility tree)))))
			)
			;(format t "Tree ~a~%~%" (map-utility tree))
			(format t "Agent decided ~a" action)
			(format t " ,expected utility ~a~%" (car (get-action-highest-utility (map-utility tree))))
			action
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
		(when (eql action #\U)
			(return-from transition (move-up env))
		)
		(when (eql action #\D)
			(return-from transition (move-down env))
		)
		(when (eql action #\L)
			(return-from transition (move-left env))
		)
		(when (eql action #\R)
			(return-from transition (move-right env))
		)
		(when (eql action #\C)
			(return-from transition (move-cut env))
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
		(format t "Final utility is ~a~%" (utility (run-N-times env 10)))
	)
)


(sim-agent)