;embedded DSL specification

(defvar *grid* nil)

(defun X ())
(defun O ())

(defun Circle (x y)
	(setf (nth x (nth y *grid*)) `O)
)

(defun Cross (x y)
	(setf (nth x (nth y *grid*)) `X)
)

(defun program-eval (program)
	;evaluates a tentative program 
	(dolist (x program)
		(eval x)
	)
)


;helper printing functions

(defun print-row (row)

	(format t "|")
	(if (eql (nth 0 row) nil)
		(format t " ")
		(format t "~a" (nth 0 row))
	)
	(if (eql (nth 1 row) nil)
		(format t " ")
		(format t "~a" (nth 1 row))
	)
	(if (eql (nth 2 row) nil)
		(format t " ")
		(format t "~a" (nth 2 row))
	)
	(format t "|~%")
)

(defun print-grid ()
	(let
		(
			(row-0 (nth 0 *grid*))
			(row-1 (nth 1 *grid*))
			(row-2 (nth 2 *grid*))
		) 
		(print-row row-0)
		(print-row row-1)
		(print-row row-2)
	)
	(format t "~%")
)


;Program Synthesis


;PS helper functions
(defun turn (grid)
	;returns `X or `O, depending on whose turn it is
	(let 
		(
			(circles 0)
			(crosses 0)
		)
		(dolist (row grid)
			(dolist (pos row)
				(if (eql pos `X)
					(setf crosses (+ crosses 1))
				)
				(if (eql pos `O)
					(setf circles (+ circles 1))
				)
			)
		)
		;if same number, crosses turn: else, circle
		(if (eql circles crosses)
			`Cross
			`Circle
		)
	)
)

(defun switch-turn (turn)
	(if (eql turn `Circle)
		`Cross
		`Circle
	)
)

;returns number of matching positions
(defun score (grid1 grid2)
	(let
		(
			(cnt 0)
		)
		(dotimes (x 3)
			(dotimes (y 3)
				(if (eql (nth x (nth y grid1)) (nth x (nth y grid2)))
					(setf cnt (+ cnt 1)) 
				)
			)
		)
		cnt 
	)
)

(defun recurse-program-generation (input-grid output-grid curr-turn program)
	;if we have achieved desired program, we're done
	(setf *grid* (copy-tree input-grid))
	(program-eval program)
	(when (equal *grid* output-grid)
		(return-from recurse-program-generation program)
	)
	;program not perfect yet
	(let 
		(
			(curr-score (score *grid* output-grid))
		)
		;let's find a valid new instruction that evolves input-grid toward output-grid
		(dotimes (y 3)
			(dotimes (x 3)
				;only if position is empty
				(when (eql (nth x (nth y *grid*)) nil)
					(let 
						(
							(new-instruction (list (list curr-turn x y)))
						)
						(setf *grid* (copy-tree input-grid))
						(program-eval (append program new-instruction))
						;is it better?
						(if (> (score *grid* output-grid) curr-score)
							;recurse with new program, and new turn
							(return-from recurse-program-generation
								(recurse-program-generation 
									input-grid 
									output-grid 
									(switch-turn curr-turn) 
									(append program new-instruction)
								)
							)
						)
					) 
				)
				;restore grid to prior state
				(setf *grid* (copy-tree input-grid))
				(program-eval program)
			)
		)
		(format t "Error: could not find valid instruction~%")
	)
)



;PS main entry point, given input/output pair
(defun PS (input-grid output-grid)
	(format t "Synthesizing program to match input:~%")
	(setf *grid* (copy-tree input-grid))
	(print-grid)
	(format t "Into output:~%")
	(setf *grid* (copy-tree output-grid))
	(print-grid)
	;determine whether we should play Cross or Circle first
	(let
		(
			(curr-turn (turn input-grid))
		) 
		;until output is matched: determine instruction that gets us closer to output
		(recurse-program-generation input-grid output-grid curr-turn nil)
	)
)


;Real test
(format t "~a~%" (PS 	(list 
							(list nil nil nil) 
							(list nil `X nil) 
							(list nil nil nil)) 
						(list 
							(list nil `O `X) 
							(list nil `X `O) 
							(list nil nil nil))))
