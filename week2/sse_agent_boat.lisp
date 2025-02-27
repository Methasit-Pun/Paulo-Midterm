
        )
        (format t "(ML ~a CL ~a SIDE ~a MR ~a CR ~a)~%" 
                ml cl (if (= side 0) "L" "R") mr cr)
    )
)

(defun move-r-m (env)
	(let 