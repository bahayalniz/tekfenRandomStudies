;; Displays a message box containing the entered text
(defun c:hello (/ msg) 
  (setq msg (getstring T "\nEnter a message: "))
  (alert msg)
)

;; Draws a line between two points
(defun c:drawline (/ pt1 pt2 sv_clayer)  ;; Declared local variables
  ;; Store the current and create a new layer
  (setq sv_clayer (getvar "clayer"))
  (command "_.-layer" "_m" "Object" "_c" "5" "" "")

  ;; Prompt for two points
  (setq pt1 (getpoint "\nSpecify start point of line: ")
        pt2 (getpoint pt1 "\nSpecify end point of line: ")
  )

  ;; Check to see if the user specified two points
  (if (and pt1 pt2) 
    (command "_.line" pt1 pt2 "")
    (prompt "\nInvalid or missing point(s)")
  )

  ;; Restore the previous layer
  (setvar "clayer" sv_clayer)

  ;; Exit quietly
  (princ)
)

(alert "\nAutoLISP Tutorial file loaded.")
(princ) ; Suppress the return value of the prompt function

(setq deneme 35)

(defun c:select_all_blocks () 
  (setq allofit (ssget "_A" 
                       (list 
                         '(0 . "INSERT")
                         (if (= 1 (getvar 'cvport)) 
                           (cons 410 (getvar 'ctab))
                           '(410 . "Model")
                         )
                       )
                )
  )
)