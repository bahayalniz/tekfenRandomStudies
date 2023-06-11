;;-----------------------=={ ObjectDBX Wrapper }==----------------------;;
;;                                                                      ;;
;;  Evaluates a supplied function on all drawings in a given list or    ;;
;;  selected directory.                                                 ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Arguments:                                                          ;;
;;                                                                      ;;
;;  fun [SYM]                                                           ;;
;;  ---------------------------------                                   ;;
;;  A function requiring a single argument (the VLA Document object),   ;;
;;  and following the 'rules' of ObjectDBX:                             ;;
;;                                                                      ;;
;;  - No SelectionSets               (ssget, ssname, ssdel, etc)        ;;
;;  - No Command Calls               (command "_.line" ... etc)         ;;
;;  - No ent* methods                (entget, entmod, entupd, etc)      ;;
;;  - No Access to System Variables  (setvar, getvar, setvariable, etc) ;;
;;                                                                      ;;
;;  lst [LIST] [Optional]                                               ;;
;;  ---------------------------------                                   ;;
;;  List of DWG Filenames; if nil, BrowseForFolder Dialog is displayed. ;;
;;                                                                      ;;
;;  sav [SYM]                                                           ;;
;;  ---------------------------------                                   ;;
;;  Boolean flag determining whether drawings should be saved following ;;
;;  function evaluation (T=saved, nil=not saved).                       ;;
;;----------------------------------------------------------------------;;
;;  Returns:                                                            ;;
;;                                                                      ;;
;;  List of:                                                            ;;
;;  (                                                                   ;;
;;      (<Drawing Filename> . <Function Result>)                        ;;
;;      (<Drawing Filename> . <Function Result>)                        ;;
;;      ...                                                             ;;
;;      (<Drawing Filename> . <Function Result>)                        ;;
;;  )                                                                   ;;
;;                                                                      ;;
;;  Where:                                                              ;;
;;  <Drawing Filename>                                                  ;;
;;  is the filename of drawing that has been processed.                 ;;
;;                                                                      ;;
;;  <Function Result>                                                   ;;
;;  is the result of evaluating the supplied function on the Document   ;;
;;  Object representing the associated drawing filename.                ;;
;;                                                                      ;;
;;  If an error occurs when evaluating the supplied function the        ;;
;;  Function Result will be nil and the error message will be printed   ;;
;;  to the command-line.                                                ;;
;;----------------------------------------------------------------------;;

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

;;------------------=={ Browse for Folder }==-----------------;;
;;                                                            ;;
;;  Displays a dialog prompting the user to select a folder.  ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2013 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  msg - message to display at top of dialog                 ;;
;;  dir - root directory (or nil)                             ;;
;;  flg - bit-coded flag specifying dialog display settings   ;;
;;------------------------------------------------------------;;
;;  Returns:  Selected folder filepath, else nil.             ;;
;;------------------------------------------------------------;;

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

(vl-load-com) (princ)