; ██╗███╗   ███╗██████╗  ██████╗ ██████╗ ████████╗
; ██║████╗ ████║██╔══██╗██╔═══██╗██╔══██╗╚══██╔══╝
; ██║██╔████╔██║██████╔╝██║   ██║██████╔╝   ██║   
; ██║██║╚██╔╝██║██╔═══╝ ██║   ██║██╔══██╗   ██║   
; ██║██║ ╚═╝ ██║██║     ╚██████╔╝██║  ██║   ██║   
; ╚═╝╚═╝     ╚═╝╚═╝      ╚═════╝ ╚═╝  ╚═╝   ╚═╝    

(vl-load-com)
(vl-load-reactors)


; ███████╗██╗   ██╗███╗   ██╗ ██████╗████████╗██╗ ██████╗ ███╗   ██╗███████╗
; ██╔════╝██║   ██║████╗  ██║██╔════╝╚══██╔══╝██║██╔═══██╗████╗  ██║██╔════╝
; █████╗  ██║   ██║██╔██╗ ██║██║        ██║   ██║██║   ██║██╔██╗ ██║███████╗
; ██╔══╝  ██║   ██║██║╚██╗██║██║        ██║   ██║██║   ██║██║╚██╗██║╚════██║
; ██║     ╚██████╔╝██║ ╚████║╚██████╗   ██║   ██║╚██████╔╝██║ ╚████║███████║
; ╚═╝      ╚═════╝ ╚═╝  ╚═══╝ ╚═════╝   ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚══════╝

(defun change_it_old (old new)
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


;------------------------------------------------------------------------------------------------

(defun activate-external-document (file-path)
  (setq external-document (vla-get-activedocument
                           (vla-get-documents (vlax-get-acad-object))))
  (setq external-document (vla-open external-document file-path))
  (vla-activate external-document))

;------------------------------------------------------------------------------------------------

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

;------------------------------------------------------------------------------------------------

(defun AT:NoSpaces (str / str)
 ;; Remove all spaces from string
 ;; str - valid string
 ;; Alan J. Thompson, 03.20.09
 (if (eq (type str) 'STR)
   (while (vl-string-search " " str)
     (setq str (vl-string-subst "-" " " str))
   )
 )
)

;------------------------------------------------------------------------------------------------

(defun get-dwg-files (directory)
  (setq result nil)
  (foreach file (vl-directory-files directory "*.dwg" 1)
    (setq result (cons (strcat directory file) result)))
  result)

;------------------------------------------------------------------------------------------------

; ██╗   ██╗ █████╗ ██████╗ ██╗ █████╗ ██████╗ ██╗     ███████╗███████╗
; ██║   ██║██╔══██╗██╔══██╗██║██╔══██╗██╔══██╗██║     ██╔════╝██╔════╝
; ██║   ██║███████║██████╔╝██║███████║██████╔╝██║     █████╗  ███████╗
; ╚██╗ ██╔╝██╔══██║██╔══██╗██║██╔══██║██╔══██╗██║     ██╔══╝  ╚════██║
;  ╚████╔╝ ██║  ██║██║  ██║██║██║  ██║██████╔╝███████╗███████╗███████║
;   ╚═══╝  ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝╚═╝  ╚═╝╚═════╝ ╚══════╝╚══════╝╚══════╝

(defun call_modelsapce ()
(setq *ModelSpace* (vla-get-ModelSpace 
                     (vla-get-ActiveDocument (vlax-get-acad-object))
                   ) ;_ end of vla-get-ModelSpace
) ;_ end of setq
)
(call_modelsapce)

(setq reference_file "C:/Users/BAHA-LAPTOP/Documents/GitHub/tekfenRandomStudies/excel_to_letterhead/documents/REFERENCE-ANTET.dwg")

(setq file "C:/Users/BAHA-LAPTOP/Documents/GitHub/tekfenRandomStudies/excel_to_letterhead/documents/ANTET-VARIABLES.csv")

(setq import_path "C:/Users/BAHA-LAPTOP/Documents/GitHub/tekfenRandomStudies/excel_to_letterhead/documents/IMPORT_ANTET/")
(setq export_path "C:/Users/BAHA-LAPTOP/Documents/GitHub/tekfenRandomStudies/excel_to_letterhead/documents/EXPORT_ANTET/")
(setq edit_path "C:/Users/BAHA-LAPTOP/Documents/GitHub/tekfenRandomStudies/excel_to_letterhead/documents/IMPORT_ANTET_EDIT/")

(setq csv_data (LM:readcsv file)) ;WHOLE TABLE

(setq project_code "M7")

(setq counter 0)
;LENGTH OF THE TABLE  
(foreach line csv_data 
  (setq counter (+ counter 1))
  (if (= (nth 0 line) "ENDTABLE") 
    (setq len (- counter 1))
  )
)

; ███╗   ███╗ █████╗ ██╗███╗   ██╗
; ████╗ ████║██╔══██╗██║████╗  ██║
; ██╔████╔██║███████║██║██╔██╗ ██║
; ██║╚██╔╝██║██╔══██║██║██║╚██╗██║
; ██║ ╚═╝ ██║██║  ██║██║██║ ╚████║
; ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝

; (foreach line csv_data 
;   (setq pafta_no (nth 0 line))

;   (if (/= pafta_no "") 
;     (progn 
;           (setq pafta_no_wrap (LM:StringWrap (nth 0 line) 1)) 
;           (setq pafta_no_check (strcat (car pafta_no_wrap) (cadr pafta_no_wrap)))
;           (setq new_pafta_name (AT:NoSpaces (vl-princ-to-string (cdr (cddddr line)))) )
;             (if (= pafta_no_check project_code) 
;               (progn
;                 (setq import (strcat import_path pafta_no ".dwg"))
;                 (setq export (strcat export_path new_pafta_name ".dwg"))
;                 (vl-file-copy import export T) ;SAVE REFERENCE AS EXPORT
;               )
;             )
          
;     )
;   )
; )

(setq file_list (get-dwg-files edit_path))

(defun c:isimci () 
  
  (defun _getattributes ( doc / data item name values )
        (vlax-for layout (vla-get-layouts doc)
            (vlax-for object (vla-get-block layout)
                (if
                    (and
                        (= "AcDbBlockReference" (vla-get-objectname object))
                        (= :vlax-true (vla-get-hasattributes object))
                        (setq values
                            (mapcar
                                (function
                                    (lambda ( att )
                                        (cons
                                            (vla-get-tagstring att)
                                            (vla-get-textstring att)
                                        )
                                    )
                                )
                                (vlax-invoke object 'getattributes)
                            )
                        )
                    )
                    (if
                        (setq item
                            (assoc
                                (setq name
                                    (if (vlax-property-available-p object 'effectivename)
                                        (vla-get-effectivename object)
                                        (vla-get-name object)
                                    )
                                )
                                data
                            )
                        )
                        (setq data
                            (subst
                                (append item (list values))
                                item
                                data
                            )
                        )
                        (setq data (cons (list name values) data))
                    )
                )
            )
        )
        data
    )
  
  (defun _change_letterhead_data (doc / meta_data file_name)

    (setq meta_data (_getattributes doc))
    (setq file_name (vl-filename-base (car meta_data)))
    
    (foreach line csv_data
      (if
      ((= (car line) file_name))
        (progn
          (vla-activate doc file_name)
          (change_it "QDT"  (nth 2  line) );DATE       QDT
          (change_it "QSC"  (nth 3  line) );SCALE      QSC
          (change_it "QFM"  (nth 4  line) );FORMAT     QFM
          (change_it "QAE"  (nth 8  line) );AREA       QAE
          (change_it "QSAE" (nth 9  line) );SUBAREA    QSAE
          (change_it "QSY"  (nth 10 line) );SYSTEM     QSY
          (change_it "QSSY" (nth 11 line) );SUBSYSTEM  QSSY
          (change_it "QLN"  (nth 12 line) );LANGUAGE   QLN
          (change_it "QDCT" (nth 13 line) );DOCTYPE    QDCT
          (change_it "QDCN" (nth 14 line) );DOCNO      QDCN
        )
      )
    )
    )

  
  (setq big_data (LM:ODBX '_change_letterhead_data file_list T))
  (princ)
)


; (setq active_file (vla-open (vla-get-documents (vlax-get-acad-object)) export))

; (command "_.delay" 5000)

; (vla-activate active_file)

; (command "_.delay" 5000)

; (change_it "QDT"  (nth 2  line) );DATE       QDT
; (change_it "QSC"  (nth 3  line) );SCALE      QSC
; (change_it "QFM"  (nth 4  line) );FORMAT     QFM
; (change_it "QAE"  (nth 8  line) );AREA       QAE
; (change_it "QSAE" (nth 9  line) );SUBAREA    QSAE
; (change_it "QSY"  (nth 10 line) );SYSTEM     QSY
; (change_it "QSSY" (nth 11 line) );SUBSYSTEM  QSSY
; (change_it "QLN"  (nth 12 line) );LANGUAGE   QLN
; (change_it "QDCT" (nth 13 line) );DOCTYPE    QDCT
; (change_it "QDCN" (nth 14 line) );DOCNO      QDCN

; (command "_.delay" 5000)

; (vla-close active_file :vlax-True)
; (call_modelsapce)

; (alert (itoa len))

(alert "KOD ÇALIŞTI")

