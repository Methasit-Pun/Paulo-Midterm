;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; 
; 	Knowledge Base 
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass Knowledge-Base ()
	(
		propositions ;known propositions (invariants + fluents)
		update-with-percepts-rules  ;rules that update fluents
	)
)

;symbolic definition of relevant propositions
(defun !PD ()) 	;no Pit of Death
(defun PD ())	;Pit of Death
(defun !M ())	;no Monster
(defun M ())	;Monster
(defun !BS ())	;no Bad Smell
(defun BS ())	;Bad Smell
(defun !Bz ())	;no Breeze
(defun Bz ())	;Breeze
(defun V()) 	;Visited position



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
; 	Helper functions to update and query the Knowledge Base 
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun KB-position-valid (KB x y)
; 	;returns true, iff we find propositions 
; 	; !PD_{x,y} AND !M_{x,y}
; 	;for position x,y
	(dolist (prop (slot-value KB `propositions))
		(if (equal prop (list `!PD x y))
			(dolist (prop (slot-value KB `propositions))
				(if (equal prop (list `!M x y))
					(return-from KB-position-valid t)
				)
			)
		)
	)
 	nil
)

(defun not-in-propositions (KB prop-t prop-f)
	(dolist (prop (slot-value KB `propositions))
		(if (equal prop prop-t)
			(return-from not-in-propositions nil)
		)
		(if (equal prop prop-f)
			(return-from not-in-propositions nil)
		)
	)
	t
)

(defun in-propositions (KB test)
	(dolist (prop (slot-value KB `propositions))
		(if (equal prop test)
			(return-from in-propositions t)
		)
	)
	nil
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
; 	Fluent update rules 
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Monster-rule (KB propositions)
	;returns either M_x,y or !M_x,y (if either is a new proposition)
	;or nil, if it cannot infer new propositions
	(let 
		(
			(new-props nil)
		)
		(dotimes (y 4)
			(dotimes (x 4)
				;first, check if not already in propositions list
				(if (not-in-propositions KB (list `M x y) (list `!M x y))
					;if not, see if we can build a rule
					(let
						(
							(bs-up 		(in-propositions KB (list `BS x (+ y 1))))
							(bs-down 	(in-propositions KB (list `BS x (- y 1))))
							(bs-right 	(in-propositions KB (list `BS (+ x 1) y)))
							(bs-left 	(in-propositions KB (list `BS (- x 1) y)))
							(!bs-up 	(in-propositions KB (list `!BS x (+ y 1))))
							(!bs-down 	(in-propositions KB (list `!BS x (- y 1))))
							(!bs-right 	(in-propositions KB (list `!BS (+ x 1) y)))
							(!bs-left 	(in-propositions KB (list `!BS (- x 1) y)))
						)
						(if (or !bs-up !bs-down !bs-right !bs-left)
							;(return-from Monster-rule (list `!M x y))
							(setf new-props (append new-props (list (list `!M x y))))	
						)
						(if (and bs-up bs-down bs-right bs-left)
							;(return-from Monster-rule (list `M x y))
							(setf new-props (append new-props (list (list `M x y))))
						)
					)
				)
			)
		)
		new-props
	)
)

(defun Pit-of-Death-rule (KB propositions)
	;returns either PD_x,y or !PD_x,y (if either is a new proposition)
	;or nil, if it cannot infer new propositions
	(let 
		(
			(new-props nil)
		)
		(dotimes (y 4)
			(dotimes (x 4)
				;first, check if not already in propositions list
				(if (not-in-propositions KB (list `PD x y) (list `!PD x y))
					;if not, see if we can build a rule
					(let
						(
							(bz-up 		(in-propositions KB (list `Bz x (+ y 1))))
							(bz-down 	(in-propositions KB (list `Bz x (- y 1))))
							(bz-right 	(in-propositions KB (list `Bz (+ x 1) y)))
							(bz-left 	(in-propositions KB (list `Bz (- x 1) y)))
							(!bz-up 	(in-propositions KB (list `!Bz x (+ y 1))))
							(!bz-down 	(in-propositions KB (list `!Bz x (- y 1))))
							(!bz-right 	(in-propositions KB (list `!Bz (+ x 1) y)))
							(!bz-left 	(in-propositions KB (list `!Bz (- x 1) y)))
						)
						(if (or !bz-up !bz-down !bz-right !bz-left)
							(setf new-props (append new-props (list (list `!PD x y))))	
						)
						(if (and bz-up bz-down bz-right bz-left)
							(setf new-props (append new-props (list (list `PD x y))))
						)
					)
				)
			)
		)
		new-props
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; 
; 	Main functions to drive agent behavior, updating and querying the Knowledge Base 
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;print KB in readable way
(defun print-KB (KB)
	(format t "Knowledge Base:~%")
	(format t "Propositions: ")
	(dolist (prop (slot-value KB `propositions))
		(format t "~a " prop)
	)
	(format t "~%Rules: ")
	(dolist (rule (slot-value KB `update-with-percepts-rules))
		(format t "~a " rule)
	)
	(format t "~%")
)

;KB initialization
(defun initialize-KB ()
	(let 
		(
			(KB (make-instance `Knowledge-Base))
		)
		(setf (slot-value KB `propositions)
			(list
				;there is no Pit of Death at 0,0
				(list `!PD 0 0)
				;there is no Monster at 0,0
				(list `!M 0 0)
				;visited position 0,0
				(list `V 0 0)
			)
		)
		(setf (slot-value KB `update-with-percepts-rules)
			(list `Monster-rule `Pit-of-Death-rule)
		)
		KB
	)
)

;updates and returns the KB given new percept
(defun update-KB-with-percepts (KB percepts)
	;given percepts, add relevant propositions to KB, and ...
	(dolist (prop percepts)
		(setf (slot-value KB `propositions) (append (slot-value KB `propositions) (list prop)))
	)
	;... find KB rule for fluents update that matches, creating a new known proposition
	(dolist (rule (slot-value KB `update-with-percepts-rules))
		(let 
			(
				(prop (funcall rule KB (slot-value KB `propositions)))
			)
			(unless (eql prop nil)
				;if return value is a proposition
				(setf (slot-value KB `propositions) (append (slot-value KB `propositions) prop))
			)
		)
	)
	KB
)


;updates and returns the KB given performed action (visited position)
(defun update-KB-with-action (KB action)
	(setf (slot-value KB `propositions) (append (slot-value KB `propositions) (list action)))
	KB
)

;returns list of viable actions given KB
;Encoded as valid positions to be in
;position is valid if unvisited, and no monster or pit of death
(defun get-action-from-KB (KB)
	(let
		(
			(lst nil)
		)
		(dotimes (y 4)
			(dotimes (x 4)
				;iff position is valid
				(if (KB-position-valid KB x y)
					(if (not-in-propositions KB (list `V x y) (list `V x y)) 
						(setf lst (append lst (list (list x y))))
					)
				)
			)
		)
		lst 
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; 
; 	Environment simulation 
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;environment has monster at (0,3)
;Pits of death at (2,0) and (3,1)
;Goal at (3,2)

(defun adjacent (x y a b)
	(cond
		((and (eql x a) (eql y (+ b 1)) t))
		((and (eql x a) (eql y (- b 1)) t))
		((and (eql x (+ a 1)) (eql y b) t))
		((and (eql x (- a 1)) (eql y b) t))
		(t nil) 
	)
)

(defun get-percepts (x y)
	(let 
		(
			(percepts nil)
		)
		;monster at 0,3
		(if (adjacent x y 0 3)
			(setf percepts (append percepts (list (list `BS x y))))
			(setf percepts (append percepts (list (list `!BS x y))))
		)
		;pit of death at 2,0 and 3,1
		(if (or (adjacent x y 2 0) (adjacent x y 3 1))
			(setf percepts (append percepts (list (list `Bz x y))))
			(setf percepts (append percepts (list (list `!Bz x y))))
		)
		percepts
	)
)


(defun goal (x y)
	(if (and (eql x 3) (eql y 2))
		t
		nil
	)
)

(defun run-N-times (KB x y)
	;(print-KB KB)
	(format t "Agent at ~a,~a~%~%" x y)
	(when (goal x y)
		(return-from run-N-times t)
	)
	;get percepts
	(let 
		(
			(percepts (get-percepts x y))
		)
		;(format t "  Perceived ~a~%" percepts)
		;update KB with percepts
		(update-KB-with-percepts KB percepts)
		(let
			(
				(actions (get-action-from-KB KB))
			)
			(format t "  Actions ~a~%" actions)
			(if (not actions)
				(return-from run-N-times nil)
			) 
			(let
				(
					;let's just pick first available action
					(new-x (nth 0 (car actions)))
					(new-y (nth 1 (car actions)))
				)
				;update KB with action
				(update-KB-with-action KB (list `V new-x new-y))
				;loop
				(run-N-times KB new-x new-y)
			) 
		)
	)
)

(defun agent ()
	(let 
		(
			(KB (initialize-KB))
			(x 0)
			(y 0)
		)
		(if (run-N-times KB x y)
			(format t "Goal Reached!!!!!!~%")
			(format t "Gave up.... :(~%")
		)
	)
)

(agent)

