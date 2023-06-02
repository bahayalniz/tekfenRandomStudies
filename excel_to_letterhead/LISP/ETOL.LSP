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

(defun change_it (old new)
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

(setq export_path "C:/Users/BAHA-LAPTOP/Documents/GitHub/tekfenRandomStudies/excel_to_letterhead/documents/EXPORT_ANTET/")

(setq data (LM:readcsv file))

(setq counter 0)

;LENGTH OF THE TABLE  
(foreach line data 
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

(foreach line data
  
)
(setq line (nth 2 data))
(setq pafta_no (nth 0  line) )
(setq export (strcat export_path pafta_no ".dwg"))
(vl-file-copy reference_file export T) ;SAVE REFERENCE AS EXPORT
(setq active_file (vla-open (vla-get-documents (vlax-get-acad-object)) export))

(command "_.delay" 5000)

(vla-activate active_file)

(command "_.delay" 5000)

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

(command "_.delay" 5000)

(vla-close active_file :vlax-True)
(call_modelsapce)

; (alert (itoa len))




