; ██╗███╗   ███╗██████╗  ██████╗ ██████╗ ████████╗
; ██║████╗ ████║██╔══██╗██╔═══██╗██╔══██╗╚══██╔══╝
; ██║██╔████╔██║██████╔╝██║   ██║██████╔╝   ██║   
; ██║██║╚██╔╝██║██╔═══╝ ██║   ██║██╔══██╗   ██║   
; ██║██║ ╚═╝ ██║██║     ╚██████╔╝██║  ██║   ██║   
; ╚═╝╚═╝     ╚═╝╚═╝      ╚═════╝ ╚═╝  ╚═╝   ╚═╝    

(vl-load-com)


; ███████╗██╗   ██╗███╗   ██╗ ██████╗████████╗██╗ ██████╗ ███╗   ██╗███████╗
; ██╔════╝██║   ██║████╗  ██║██╔════╝╚══██╔══╝██║██╔═══██╗████╗  ██║██╔════╝
; █████╗  ██║   ██║██╔██╗ ██║██║        ██║   ██║██║   ██║██╔██╗ ██║███████╗
; ██╔══╝  ██║   ██║██║╚██╗██║██║        ██║   ██║██║   ██║██║╚██╗██║╚════██║
; ██║     ╚██████╔╝██║ ╚████║╚██████╗   ██║   ██║╚██████╔╝██║ ╚████║███████║
; ╚═╝      ╚═════╝ ╚═╝  ╚═══╝ ╚═════╝   ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚══════╝

(defun get-active-doc nil
    (cond ( acdoc ) ((setq acdoc (vla-get-activedocument (vlax-get-acad-object)))))
  )
;------------------------------------------------------------------------------------------------------------

(defun get-remote-doc (file-path)
  (setq doc (vla-open (vla-get-documents (vlax-get-acad-object)) file-path )) 
  )

;------------------------------------------------------------------------------------------------------------

(defun get-dwg-files (directory)
  (setq result nil)
  (foreach file (vl-directory-files directory "*.dwg" 1)
    (setq result (cons (strcat directory file) result)))
  result)

;------------------------------------------------------------------------------------------------------------

(defun modelspacer (doc)
       (vla-get-modelspace doc))

;------------------------------------------------------------------------------------------------------------

(defun paperspacer (doc)
       (vla-get-paperspace doc))

;------------------------------------------------------------------------------------------------------------

(defun number_of_docs ()
  (setq acad* (vlax-get-acad-object));_ the acad program
  (setq documents (vla-get-Documents acad*));_ all documents collection
  (progn
  (setq docs-open (vla-get-count documents));_ opened dwgs
  )
  )

;------------------------------------------------------------------------------------------------------------

(defun close_others()
    ;; This example cycles through the documents collection
    ;; and closes all open drawings using the Close method
    ;; except the current drawing.
    (setq acadObj (vlax-get-acad-object))
    (setq curDoc (vla-get-ActiveDocument acadObj))
    (setq docs (vla-get-Documents acadObj))

    ;; Close all open documents and discard changes, except for the current drawing
    (vlax-for doc docs
        (if (/= (vla-get-Name doc) (vla-get-Name curDoc))
	    (progn
                (vla-Close doc :vlax-true)
	    )
        )
    )
  )

;------------------------------------------------------------------------------------------------------------

(defun delay_it ( msec )
 (
   (lambda ( start )
     (while (< (- (getvar 'MILLISECS) start) msec))
   )
   (getvar 'MILLISECS)
 )
  )

;------------------------------------------------------------------------------------------------------------

(defun LM:readcsv ( csv / des lst sep str )
    (if (setq des (open csv "r"))
        (progn
            (setq sep (cond ((vl-registry-read "HKEY_CURRENT_USER\\Control Panel\\International" "sList")) (",")))
            (while (setq str (read-line des))
                (setq lst (cons (LM:csv->lst str sep 0) lst))
            )
            (close des)
        )
    )
    (reverse lst)
  )

;------------------------------------------------------------------------------------------------------------

(defun LM:csv->lst ( str sep pos / s )
    (cond
        (   (not (setq pos (vl-string-search sep str pos)))
            (if (wcmatch str "\"*\"")
                (list (LM:csv-replacequotes (substr str 2 (- (strlen str) 2))))
                (list str)
            )
        )
        (   (or (wcmatch (setq s (substr str 1 pos)) "\"*[~\"]")
                (and (wcmatch s "~*[~\"]*") (= 1 (logand 1 pos)))
            )
            (LM:csv->lst str sep (+ pos 2))
        )
        (   (wcmatch s "\"*\"")
            (cons
                (LM:csv-replacequotes (substr str 2 (- pos 2)))
                (LM:csv->lst (substr str (+ pos 2)) sep 0)
            )
        )
        (   (cons s (LM:csv->lst (substr str (+ pos 2)) sep 0)))
    )
  )

;------------------------------------------------------------------------------------------------------------

(defun LM:csv-replacequotes ( str / pos )
    (setq pos 0)
    (while (setq pos (vl-string-search  "\"\"" str pos))
        (setq str (vl-string-subst "\"" "\"\"" str pos)
              pos (1+ pos)
        )
    )
    str
  )

;------------------------------------------------------------------------------------------------------------

(defun change_it_old (old new) ;ESKİ VERSİYON
	(setq OldTxt old
		  NewTxt new)
	
	(setq ss (ssget "x" '((0 . "TEXT,MTEXT"))))
	
	(setq i (sslength ss))
	
	(while (not (minusp (setq i (1- i))))
		(setq oText (vlax-ename->vla-object (ssname ss i)))
		(setq Txt (vlax-get-property oText 'TextString))
		
		(if (vl-string-search OldTxt txt)
			(progn
				(setq newChg (vl-string-subst NewTxt OldTxt txt))
				(vlax-put-property oText 'TextString newchg)
				(vlax-invoke-method oText 'Update)
			)
		)
	)
	(princ)
  )

;-----------------------------------------------------------------------------------------------------------

(defun change_it (OldTxt NewTxt doc / txt) 

  (vlax-for layout (vla-get-layouts doc) 
    (vlax-for entry (vla-get-block layout) 
      (if 
        (or (eq (vla-get-objectname entry) "AcDbMText") 
            (eq (vla-get-objectname entry) "AcDbText")
        )
        (progn 

          (setq txt (vlax-get-property entry 'TextString))

          (if (vl-string-search OldTxt txt) 
            (progn 
              (setq newChg (vl-string-subst NewTxt OldTxt txt))
              (vlax-put-property entry 'TextString newchg)
              (vlax-invoke-method entry 'Update)
            )
          )
        )
      )
    )
  )
  (princ)
 )

;-----------------------------------------------------------------------------------------------------------

(defun LM:StringWrap ( str len / pos )
    (if (< len (strlen str))
        (cons
            (substr str 1
                (cond
                    (   (setq pos (vl-string-position 32 (substr str 1 len) nil t)))
                    (   (setq pos (1- len)) len)
                )
            )
            (LM:StringWrap (substr str (+ 2 pos)) len)
        )
        (list str)
    )
  )

;-----------------------------------------------------------------------------------------------------------

(defun LM:lst->str ( lst del )
    (if (cdr lst)
        (strcat (car lst) del (LM:lst->str (cdr lst) del))
        (car lst)
    )
  )

;-----------------------------------------------------------------------------------------------------------

(defun create_new_files (doc) 
  (foreach line csv_data 
      (progn 
        (setq pafta_no (nth 0 line)) ;Gets the first column to get the file name
        (setq pafta_no_wrap (LM:StringWrap (nth 0 line) 1))
        (setq pafta_no_check (strcat (car pafta_no_wrap) (cadr pafta_no_wrap)))
        (setq new_pafta_name (LM:lst->str (cdr (cddddr line)) "-"))
        (if (eq pafta_no_check project_code) 
          (progn 
            (setq import (strcat import_path pafta_no ".dwg"))
            (setq export (strcat export_path new_pafta_name ".dwg"))
            (vla-saveas doc export)
            ; (vl-file-copy import export T) ;SAVE REFERENCE AS EXPORT
          )
        )
      )
    
  )
  (princ)
)

;-----------------------------------------------------------------------------------------------------------

(defun replace_all_text (doc / file_name pafta_no pafta_no_check pafta_no_wrap new_pafta_name)
  (setq file_name (vl-filename-base (vla-get-name doc)))
      (foreach line csv_data 
        
        (if (= (car line) file_name) ;Gets the line where the filename matches.
          (progn
        
            (change_it "QDT"  (nth 2 line) doc)  ;DATE       QDT
            (change_it "QSC"  (nth 3 line) doc)  ;SCALE      QSC
            (change_it "QFM"  (nth 4 line) doc)  ;FORMAT     QFM
            (change_it "QAE"  (nth 8 line) doc)  ;AREA       QAE
            (change_it "QSAE" (nth 9 line) doc) ;SUBAREA    QSAE
            (change_it "QSY"  (nth 10 line) doc) ;SYSTEM     QSY
            (change_it "QSSY" (nth 11 line) doc);SUBSYSTEM  QSSY
            (change_it "QLN"  (nth 12 line) doc) ;LANGUAGE   QLN
            (change_it "QDCT" (nth 13 line) doc);DOCTYPE    QDCT
            (change_it "QDCN" (nth 14 line) doc);DOCNO      QDCN
            (setq pafta_no (nth 0 line)) ;Gets the first column to get the file name
            (setq pafta_no_wrap (LM:StringWrap (nth 0 line) 1)) 
            (setq pafta_no_check (strcat (car pafta_no_wrap) (cadr pafta_no_wrap)))
            (setq new_pafta_name (LM:lst->str (cdr (cddddr line)) "-") )
            (if (= pafta_no_check project_code) 
              (progn
                (setq import (strcat import_path pafta_no ".dwg"))
                (setq export (strcat export_path new_pafta_name ".dwg"))
                (vla-saveas doc export)
                ; (vla-close doc :vlax-true)
                ; (vl-file-copy import export :vlax-false) ;SAVE REFERENCE AS EXPORT
                )
              )             
            )
          )
        ) 
      )



;-----------------------------------------------------------------------------------------------------------


; ██╗   ██╗ █████╗ ██████╗ ██╗ █████╗ ██████╗ ██╗     ███████╗███████╗
; ██║   ██║██╔══██╗██╔══██╗██║██╔══██╗██╔══██╗██║     ██╔════╝██╔════╝
; ██║   ██║███████║██████╔╝██║███████║██████╔╝██║     █████╗  ███████╗
; ╚██╗ ██╔╝██╔══██║██╔══██╗██║██╔══██║██╔══██╗██║     ██╔══╝  ╚════██║
;  ╚████╔╝ ██║  ██║██║  ██║██║██║  ██║██████╔╝███████╗███████╗███████║
;   ╚═══╝  ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝╚═╝  ╚═╝╚═════╝ ╚══════╝╚══════╝╚══════╝

(setq doc (get-active-doc))
(setq modelspace (vla-get-modelspace (get-active-doc)))
(setq csv_file "C:/Users/BAHA-LAPTOP/Documents/GitHub/tekfenRandomStudies/excel_to_letterhead/documents/ANTET-VARIABLES-V2.csv")
(setq csv_data (LM:readcsv csv_file)) ;WHOLE TABLE
(setq project_code "M7")
(setq import_path "C:/Users/BAHA-LAPTOP/Documents/GitHub/tekfenRandomStudies/excel_to_letterhead/documents/IMPORT_ANTET/")
(setq export_path "C:/Users/BAHA-LAPTOP/Documents/GitHub/tekfenRandomStudies/excel_to_letterhead/documents/EXPORT_ANTET/")
(setq files (get-dwg-files import_path))

;EXPORT PATH YANLIŞTI


; ███╗   ███╗ █████╗ ██╗███╗   ██╗
; ████╗ ████║██╔══██╗██║████╗  ██║
; ██╔████╔██║███████║██║██╔██╗ ██║
; ██║╚██╔╝██║██╔══██║██║██║╚██╗██║
; ██║ ╚═╝ ██║██║  ██║██║██║ ╚████║
; ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝


(defun c:wrap_it ()

  (LM:ODBX
    (function
        (lambda ( doc )
          ; (print (vl-filename-base (vla-get-name doc)))
          ; (create_new_files doc)
          (replace_all_text doc)
        )
    )
    nil nil
  )
  (princ)  

)


