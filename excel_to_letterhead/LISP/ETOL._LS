; (setq file "C:/Users/BAHA-LAPTOP/Documents/GitHub/tekfenRandomStudies/excel_to_letterhead/documents/ANTET-VARIABLES.csv")

; (setq path (LM:readcsv file))

; (setq n 0)
; (setq counter 0)
; (setq flag 0)


(defun c:deneme (data)
 (foreach line data 
  (setq counter (+ counter 1))
  
  (if (= (nth 0 line) "ENDTABLE") 
    (alert "BULDUM")
    (princ "|Counter: ")
    (princ counter)
    (princ "|")
    (setq flag T)
  )
)
)

(defun c:testicin ()
  (alert "CALISIYOR!")
)

