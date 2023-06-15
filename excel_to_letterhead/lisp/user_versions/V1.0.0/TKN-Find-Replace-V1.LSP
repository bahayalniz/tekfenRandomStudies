;████████╗██╗   ██╗████████╗ ██████╗ ██████╗ ██╗ █████╗ ██╗     
;╚══██╔══╝██║   ██║╚══██╔══╝██╔═══██╗██╔══██╗██║██╔══██╗██║     
;   ██║   ██║   ██║   ██║   ██║   ██║██████╔╝██║███████║██║     
;   ██║   ██║   ██║   ██║   ██║   ██║██╔══██╗██║██╔══██║██║     
;   ██║   ╚██████╔╝   ██║   ╚██████╔╝██║  ██║██║██║  ██║███████╗
;   ╚═╝    ╚═════╝    ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚═╝╚═╝  ╚═╝╚══════╝

;TEKFEN FIND & REPLACE V1
;by bahayalniz: 
;bahayalniz@hotmail.com 

; 1 - Open the LSP file called "TKN-Find-Replace-V1.LSP"

; 2 - Set all the variables under "INPUT" section
; 	    csv_file      :The directory where the table is located
; 	    project_code  :The project code. This variable is used to check 
;                      if the table rows contain valid information 
;                      when getting the project names from the table.
; 	    import_path   :The directory where all files will be processed.
; 	    export_path   :The directory where the processed files will be exported.

; 3 - Open one of the DWG files to be processed.

; 4 - Write "APPLOAD" to the prompt interface.  

; 5 - Select the LSP file ("TKN-Find-Replace-V1.LSP") and click load. You can either click "Always Load" or "Load Once".

; 6 - Write "TKN-FIND-REPLACE" to call the function

; 7 - Select the directory of the files to be processed.

; 8 - You can monitor the process by looking at the "export" directory you have specified.                                                 

;IMPORTANT NOTES:

; ***DON'T FORGET TO USE QUOTE (") WHILE SETTING THE VARIABLES.
; 	WRONG CASE: 	C:/TKN/DATA/Table.csv
; 	CORRECT CASE: 	"C:/TKN/DATA/Table.csv"

; ***USE "/" INSTEAD OF "\" WHILE SETTING THE DIRECTORY VARIABLES. OTHERWISE THE CODE WON'T WORK.
; 	WRONG CASE:	"C:\TKN\DATA\Table.csv"
; 	CORRECT CASE:	"C:/TKN/DATA/Table.csv"

; ***PUT "/" END OF THE IMPORT AND EXPORT PATHS. 
; 	WRONG CASE: 	"C:/TKN/DATA"
; 	CORRECT CASE: 	"C:/TKN/DATA/"                                      

;------------------------------------------------------------------------------------------------------------

; ██╗███╗   ██╗██████╗ ██╗   ██╗████████╗
; ██║████╗  ██║██╔══██╗██║   ██║╚══██╔══╝
; ██║██╔██╗ ██║██████╔╝██║   ██║   ██║   
; ██║██║╚██╗██║██╔═══╝ ██║   ██║   ██║   
; ██║██║ ╚████║██║     ╚██████╔╝   ██║   
; ╚═╝╚═╝  ╚═══╝╚═╝      ╚═════╝    ╚═╝   

;1 -  The directory where the table is located:
(setq csv_file "C:/Users/BAHA-LAPTOP/Documents/GitHub/tekfenRandomStudies/excel_to_letterhead/documents/DATA/ANTET-VARIABLES-V3.csv")

;2 -  The project code. This variable is used to check 
;     if the table rows contain valid information 
;     when getting the project names from the table.
(setq project_code "M7")

;3 -  The directory where all files will be processed.
(setq import_path "C:/Users/BAHA-LAPTOP/Documents/GitHub/tekfenRandomStudies/excel_to_letterhead/documents/IMPORT_ANTET/")

;4 -  The directory where the processed files will be exported.
(setq export_path "C:/Users/BAHA-LAPTOP/Documents/GitHub/tekfenRandomStudies/excel_to_letterhead/documents/EXPORT_ANTET/")

;------------------------------------------------------------------------------------------------------------

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

(defun LM:ODBX ( fun lst sav / *error* app dbx dir doc dwl err rtn vrs )

    (defun *error* ( msg )
        (if (and (= 'vla-object (type dbx)) (not (vlax-object-released-p dbx)))
            (vlax-release-object dbx)
        )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (cond
        (   (not
                (or lst
                    (and (setq dir (LM:browseforfolder "Select Folder of Drawings to Process" nil 512))
                         (setq lst (mapcar '(lambda ( x ) (strcat dir "\\" x)) (vl-directory-files dir "*.dwg" 1)))
                    )
                )
            )
            nil
        )
        (   (progn
                (setq dbx
                    (vl-catch-all-apply 'vla-getinterfaceobject
                        (list (setq app (vlax-get-acad-object))
                            (if (< (setq vrs (atoi (getvar 'acadver))) 16)
                                "objectdbx.axdbdocument" (strcat "objectdbx.axdbdocument." (itoa vrs))
                            )
                        )
                    )
                )
                (or (null dbx) (vl-catch-all-error-p dbx))
            )
            (prompt "\nUnable to interface with ObjectDBX.")
        )
        (   t
            (vlax-for doc (vla-get-documents app)
                (setq dwl (cons (cons (strcase (vla-get-fullname doc)) doc) dwl))
            )
            (foreach dwg lst
                (if (or (setq doc (cdr (assoc (strcase dwg) dwl)))
                        (and (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-open (list dbx dwg))))
                             (setq doc dbx)
                        )
                    )
                    (progn
                        (setq rtn
                            (cons
                                (cons dwg
                                    (if (vl-catch-all-error-p (setq err (vl-catch-all-apply fun (list doc))))
                                        (prompt (strcat "\n" dwg "\t" (vl-catch-all-error-message err)))
                                        err
                                    )
                                )
                                rtn
                            )
                        )
                        (if sav (vla-saveas doc dwg))
                    )
                    (princ (strcat "\nError opening file: " (vl-filename-base dwg) ".dwg"))
                )
            )
            (if (= 'vla-object (type dbx))
                (vlax-release-object dbx)
            )
            (reverse rtn)
        )
    )
  )

;------------------------------------------------------------------------------------------------------------

(defun LM:browseforfolder ( msg dir flg / err fld pth shl slf )
    (setq err
        (vl-catch-all-apply
            (function
                (lambda ( / app hwd )
                    (if (setq app (vlax-get-acad-object)
                              shl (vla-getinterfaceobject app "shell.application")
                              hwd (vl-catch-all-apply 'vla-get-hwnd (list app))
                              fld (vlax-invoke-method shl 'browseforfolder (if (vl-catch-all-error-p hwd) 0 hwd) msg flg dir)
                        )
                        (setq slf (vlax-get-property fld 'self)
                              pth (vlax-get-property slf 'path)
                              pth (vl-string-right-trim "\\" (vl-string-translate "/" "\\" pth))
                        )
                    )
                )
            )
        )
    )
    (if slf (vlax-release-object slf))
    (if fld (vlax-release-object fld))
    (if shl (vlax-release-object shl))
    (if (vl-catch-all-error-p err)
        (prompt (vl-catch-all-error-message err))
        pth
    )
  )
;------------------------------------------------------------------------------------------------------------

(defun get-active-doc nil
    (cond ( acdoc ) ((setq acdoc (vla-get-activedocument (vlax-get-acad-object)))))
  )
;------------------------------------------------------------------------------------------------------------

(defun get-dwg-files (directory)
  (setq result nil)
  (foreach file (vl-directory-files directory "*.dwg" 1)
    (setq result (cons (strcat directory file) result)))
  result)

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
            (if (vl-string-search pafta_no_check project_code) 
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

(setq files (get-dwg-files import_path))
(setq csv_data (LM:readcsv csv_file))


; ███╗   ███╗ █████╗ ██╗███╗   ██╗
; ████╗ ████║██╔══██╗██║████╗  ██║
; ██╔████╔██║███████║██║██╔██╗ ██║
; ██║╚██╔╝██║██╔══██║██║██║╚██╗██║
; ██║ ╚═╝ ██║██║  ██║██║██║ ╚████║
; ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝


(defun c:TKN-Find-Replace ()

  (LM:ODBX
    (function
        (lambda ( doc )
          (replace_all_text doc)
        )
    )
    nil nil
  )
  (princ)  

 )

(prompt "Use 'TKN-Find-Replace' command to execute the function on DWG files that you will select.")
(princ)