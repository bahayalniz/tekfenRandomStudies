(defun c:selectIt
  (setq ss (ssget "_W" (0.0 0.0) (100.0 100.0)))
  (command "_-select" ss "")
)

(princ)