(defun c:testedelim()
  	  (setq pt1 (list 0.0 0.0 0.0)) ; Specify the first corner of the bounding box
  (setq pt2 (list 10.0 10.0 0.0)) ; Specify the opposite corner of the bounding box

	(setq ss (ssget "_C" pt1 pt2))
	(command "_-select" ss "")
	(princ "Bu bir testtir")
  	(princ)
)

