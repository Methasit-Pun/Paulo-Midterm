;
;
;	Environment initialization and printing
;
;

(defun random-element ()
	
		(when (< (random 10) 2)
			(return-from random-element #\R)
		)
		(when (< (random 10) 4)
			(return-from random-element #\Y)
		)
		nil
)

(defun make-environment ()
	(list 	(list (random-element) (random-element) (random-element))
			(list (random-element) (random-element) (random-element))
			(list (random-element) (random-element) (random-element))
	)
)

(defun print-row (row)
	(if (nth 0 row)
		(format t "~a" (nth 0 row))
		(format t " ")
	)
	(if (nth 1 row)
		(format t "~a" (nth 1 row))
		(format t " ")
	)
	(if (nth 2 row)
		(format t "~a" (nth 2 row))
		(format t " ")
	)
	(format t "~%")
)
(defun print-environment (env)
	(print-row (nth 2 env))
	(print-row (nth 1 env))
	(print-row (nth 0 env))
	(format t "Score ~a~%~%" (utility env))
)



;per row, add up number of Red pieces
(defun utility (env)
	(let
		(
			(score 0)
		) 
		(dolist (x (nth 0 env))
			(if (eql x #\R)
				(setf score (+ score 1))
			)
		)
		(dolist (x (nth 1 env))
			(if (eql x #\R)
				(setf score (+ score 1))
			)
		)
		(dolist (x (nth 2 env))
			(if (eql x #\R)
				(setf score (+ score 1))
			)
		)
		score
	)
)


;
;
;	Class definition for Red node modes and Yellow node moves
;	Red is deterministic, Yellow is probabilistic
;
;
(defclass base-node () ())

(defclass R-chance-node (base-node)
	(
		env
		tree
		score
		fn ;function that generated this node
	) 
)
(defclass Y-chance-node (base-node)
	(
		env-Y ;environment after placing Y, 70%chance
		env-R ;environment after turning adjacent R, 30%chance
		tree-Y
		tree-R
		score
		fn ;function that generated this node
	)
)

(defgeneric node-utility (node)
)

(defmethod node-utility ((node R-chance-node))
	(utility (slot-value node `env))
)
(defmethod node-utility ((node Y-chance-node))
	(+ 
		(* (utility (slot-value node `env-Y)) 0.7)
		(* (utility (slot-value node `env-R)) 0.3)
	)
)


;
;
;	Helper functions to modify environment
;
;
(defmacro pure-setf-x ((index lst) form)
	`(let 
		(
			(xcnt 0)
		)
		(mapcar
			(lambda (item)
				(let
					(
						(result (if 	(eql xcnt ,index)
										,form
										item
								)
						)
					) 
					(setf xcnt (+ xcnt 1))
					result
				)
			)
			,lst
		)
	)
)
(defmacro pure-setf-x-y ((x y lst) form)
	`(let 
		(
			(ycnt 0)
		)
		(mapcar
			(lambda (item)
				(let
					(
						(result (if 	(eql ycnt ,y)
										(pure-setf-x (,x item) ,form)
										item
								)
						)
					) 
					(setf ycnt (+ ycnt 1))
					result
				)
			)
			,lst
		)
	)
)


(defun pure-flip-row (x env y)
	(let 
		(
			(xcnt 0)
		)
		(mapcar
			(lambda (item)
				(let
					(
						(result (cond
									((eql x xcnt) 		(if (not y) (if (eql item #\Y) #\R item) item))
									((eql (- x 1) xcnt) (if (eql item #\Y) #\R item))
									((eql (+ x 1) xcnt) (if (eql item #\Y) #\R item))
									(t item)
								)
						)
					) 
					(setf xcnt (+ xcnt 1))
					result
				)
			)
			env
		)
	)
)
(defun pure-flip (x y env)
	(let 
		(
			(ycnt 0)
		)
		(mapcar
			(lambda (item)
				(let
					(
						(result (cond 
									((eql y ycnt) 			(pure-flip-row x item t))
									((eql (- y 1) ycnt) 	(pure-flip-row x item nil))
									((eql (+ y 1) ycnt) 	(pure-flip-row x item nil))
									(t item)
								)
						)
					) 
					(setf ycnt (+ ycnt 1))
					result
				)
			)
			env
		)
	)
)

(defun Y-noflip-x-y (x y env)
	(pure-setf-x-y (x y env)
		#\Y)
)
(defun Y-flip-x-y (x y env)
	(let
		(
			(flipped (pure-setf-x-y (x y env)
										#\Y)
			)
		) 
		(pure-flip x y flipped)
	)
)



;
;
;	State update functions for state space evaluation
;
;

(defun Y-x-y (x y env)
	(let
		(
			(node (make-instance `Y-chance-node))
		) 
		(setf (slot-value node `env-Y) 	(Y-noflip-x-y x y env))
		(setf (slot-value node `env-R) 	(Y-flip-x-y x y env))
		(setf (slot-value node `fn) (list `Y-x-y x y))
		node
	)
)

(defun R-x-y (x y env)
	(let
		(
			(node (make-instance `R-chance-node))
		) 
		(setf (slot-value node `env) 	(pure-setf-x-y (x y env)
															#\R)
		)
		(setf (slot-value node `fn) (list `R-x-y x y))
		node
	)
)

;
;
;	All possible actions agent might take
;
;
(defconstant *actions* 	(list 	(list 'Y-x-y 0 0)
								(list 'Y-x-y 0 1)
								(list 'Y-x-y 0 2)
								(list 'Y-x-y 1 0)
								(list 'Y-x-y 1 1)
								(list 'Y-x-y 1 2)
								(list 'Y-x-y 2 0)
								(list 'Y-x-y 2 1)
								(list 'Y-x-y 2 2)
								(list 'R-x-y 0 0)
								(list 'R-x-y 0 1)
								(list 'R-x-y 0 2)
								(list 'R-x-y 1 0)
								(list 'R-x-y 1 1)
								(list 'R-x-y 1 2)
								(list 'R-x-y 2 0)
								(list 'R-x-y 2 1)
								(list 'R-x-y 2 2) 	
						)
)

(defun is-legal (env action)
	(let 
		(
			(x (nth 1 action))
			(y (nth 2 action))
		)
		(eql (nth x (nth y env)) nil)
	)
)

;
;
;	Generates list of all possible new states, given current state
;
;

(defun recurse-actions (tree env actions)
	(if (eql actions nil)
		(return-from recurse-actions tree)
	)
	(if (is-legal env (car actions))
		(let
			(
				(new-tree 	(if (eql tree nil)
								(list (funcall (nth 0 (car actions)) (nth 1 (car actions)) (nth 2 (car actions)) env))
								(append tree  (list (funcall (nth 0 (car actions)) (nth 1 (car actions)) (nth 2 (car actions)) env)))
							)
				)
			) 
			(recurse-actions new-tree env (cdr actions))
		)
		(recurse-actions tree env (cdr actions))
	)
)

(defgeneric build-tree (root depth))

(defmethod build-tree ((root R-chance-node) depth)
	(if (eql depth 0)
		(return-from build-tree nil)
	)
	(let
		(
			(current-level (recurse-actions nil (slot-value root `env) *actions*))
		)
		(setf (slot-value root `tree) 	(mapcar (lambda (lst)
											(build-tree lst (- depth 1))
										) current-level)
		)
		root		
	)
)
(defmethod build-tree ((root Y-chance-node) depth)
	(if (eql depth 0)
		(return-from build-tree nil)
	)
	(let
		(
			(current-level-Y (recurse-actions nil (slot-value root `env-Y) *actions*))
			(current-level-R (recurse-actions nil (slot-value root `env-R) *actions*))
		)
		(setf (slot-value root `tree-Y) 	(mapcar (lambda (lst)
												(build-tree lst (- depth 1))
											) current-level-Y)
		)
		(setf (slot-value root `tree-R) 	(mapcar (lambda (lst)
												(build-tree lst (- depth 1))
											) current-level-R)
		)
		root		
	)
)




(defgeneric get-utility (root))

(defmethod get-utility (root)
	0
)
(defmethod get-utility ((root R-chance-node))
	(slot-value root `score)
)
(defmethod get-utility ((root Y-chance-node))
	(slot-value root `score)
)


;
;
;	Generates utility score for each node in the tree
;
;
(defgeneric map-utility (root))

(defmethod map-utility (root))

(defmethod map-utility ((root R-chance-node))
	(setf (slot-value root `score) (node-utility root))
	(dolist (node (slot-value root `tree))
		(map-utility node)
		(if (> (get-utility node) (slot-value root `score))
			(setf (slot-value root `score) (get-utility node))
		)
	)
)

(defmethod map-utility ((root Y-chance-node))
	(setf (slot-value root `score) (node-utility root))
	(dolist (node (slot-value root `tree-Y))
		(map-utility node)
		(if (> (get-utility node) (slot-value root `score))
			(setf (slot-value root `score) (get-utility node))
		)
	)
	(dolist (node (slot-value root `tree-R))
		(map-utility node)
		(if (> (get-utility node) (slot-value root `score))
			(setf (slot-value root `score) (get-utility node))
		)
	)
)


(defun return-highest (lst)
	(let 
		(
			(best (car lst))
		)
		(dolist (x (cdr lst))
			(if (> (slot-value x `score) (slot-value best `score))
				(setf best x)
			)
		)
		(slot-value best `fn)
	)
)

;
;
;	Real functions called to update environment when action is decides
;	For Yellow placement, result is probabilistic
;
;
(defun real-Y-x-y (x y env)
	(let
		(
			(chance (random 10))
		) 
		(if (< chance 7)
			(Y-noflip-x-y x y env)
			(Y-flip-x-y x y env)
		)
	)
)

(defun real-R-x-y (x y env)
	(pure-setf-x-y (x y env) #\R)
)


(defconstant +DEPTH+ 3)

;given an environment state, returns an updated state
(defun agent (env)
	(let
		(
			(root (make-instance `R-chance-node))
		)
		(setf (slot-value root `env) env)
		(map-utility (build-tree root +DEPTH+))
		;find action that leads to highest utility
		(let 
			(
				(action (return-highest (slot-value root `tree)))
			)
			;apply real function with corresponding probabilities
			(if (eql (car action) `Y-x-y)
				(real-Y-x-y (nth 1 action) (nth 2 action) env)
				(real-R-x-y (nth 1 action) (nth 2 action) env)
			)
		)
	)
)

(defun env-full (env)
	(dolist (row env)
		(dolist (x row)
			(if (eql x nil)
				(return-from env-full nil)
			)
		)
	)
	t
)

(defun run-N-times (env)
	(if (env-full env)
		(return-from run-N-times)
	)
	(let 
		(
			(iter (agent env))
		)
		(print-environment iter)
		(run-N-times iter)
	)
)


(defvar init (make-environment))
(print-environment init)
(run-N-times init)
