;;-------------------------=={ Delete Blocks }==------------------------;;
;;                                                                      ;;
;;  This program enables the user to quickly remove multiple blocks     ;;
;;  from a drawing.                                                     ;;
;;                                                                      ;;
;;  After invoking the program with the command 'delblocks' at the      ;;
;;  AutoCAD command-line, the user is prompted to select a block to     ;;
;;  delete. At this prompt, the user may either select a block          ;;
;;  reference from the drawing, type 'N' to choose a block by name, or  ;;
;;  exit the program by typing 'E' or by dismissing the prompt.         ;;
;;                                                                      ;;
;;  By opting to choose a block by name, the user is presented with a   ;;
;;  simple dialog interface consisting of a list box and filter. The    ;;
;;  dialog provides a list of all standard & dynamic blocks defined     ;;
;;  in the active drawing, excluding anonymous blocks and xrefs. From   ;;
;;  this list, the user may select multiple blocks to be deleted from   ;;
;;  the active drawing.                                                 ;;
;;                                                                      ;;
;;  Upon the user selecting a block from the drawing or selecting one   ;;
;;  or more block names from the dialog, the program will delete all    ;;
;;  primary & nested references of the blocks from all layouts of the   ;;
;;  active drawing, and will proceed to delete the respective block     ;;
;;  definitions, if possible.                                           ;;
;;                                                                      ;;
;;  The user will be informed at the command-line of those blocks       ;;
;;  successfully deleted by the program, and those blocks that the      ;;
;;  program was unable to remove.                                       ;;
;;                                                                      ;;
;;  The program also defines the function 'LM:deleteblocks' so that     ;;
;;  users who wish to remove multiple blocks from several drawings      ;;
;;  in a batch processing operation may bypass the dialog and call      ;;
;;  this function with a VLA Document Object and a list of block names  ;;
;;  (case-insensitive) from a Script or another LISP application.       ;;
;;  This function is also compatible with ObjectDBX.                    ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright � 2012  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2012-07-14                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2016-11-05                                      ;;
;;                                                                      ;;
;;  - Program modified to prompt user for selection of a block to       ;;
;;    delete, with the option of displaying a list of all blocks        ;;
;;    defined in the drawing.                                           ;;
;;----------------------------------------------------------------------;;

(defun c:delblocks ( / *error* def lst sel tmp )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (while (setq def (tblnext "block" (not def)))
        (if (zerop (logand 125 (cdr (assoc 70 def))))
            (setq lst (cons (cdr (assoc 2 def)) lst))
        )
    )
    (setq lst (vl-sort lst '<))
    
    (while
        (and (or lst (prompt "\nNo blocks defined in the current drawing."))
            (progn
                (setvar 'errno 0)
                (initget "Name Exit")
                (setq sel (entsel "\nSelect block to delete [Name/Exit] <Exit>: "))
                (cond
                    (   (= 7 (getvar 'errno))
                        (princ "\nMissed, try again.")
                    )
                    (   (or (null sel) (= "Exit" sel))
                        nil
                    )
                    (   (= "Name" sel)
                        (if (setq tmp (LM:filtlistbox "Select Blocks to Delete" lst t))
                            (setq tmp (LM:deleteblocks:acdoc tmp)
                                  lst (vl-remove-if '(lambda ( x ) (member (strcase x) tmp)) lst)
                            )
                        )
                        nil
                    )
                    (   (/= "INSERT" (cdr (assoc 0 (entget (car sel)))))
                        (princ "\nThe selected object is not a block.")
                    )
                    (   t
                        (setq tmp (LM:deleteblocks:acdoc (list (LM:blockname (vlax-ename->vla-object (car sel)))))
                              lst (vl-remove-if '(lambda ( x ) (member (strcase x) tmp)) lst)
                        )
                        t
                    )
                )
            )
        )
    )
    (princ)
)

(defun LM:deleteblocks:acdoc ( del / rtn )
    (LM:startundo (LM:acdoc))
    (setq rtn (LM:deleteblocks (LM:acdoc) del))
    (vla-regen (LM:acdoc) acallviewports)
    (foreach blk del
        (if (member (strcase blk) rtn)
            (princ (strcat "\nDeleted block " blk "."))
            (princ (strcat "\nUnable to delete block " blk "."))
        )
    )
    (LM:endundo (LM:acdoc))
    rtn
)
        
;; Delete Blocks  -  Lee Mac
;; Deletes all references of a list of blocks from a drawing (including nested references, nested to any level).
;; Proceeds to delete the associated block definitions from the drawing, if possible.
;; doc - [vla] Document object
;; lst - [lst] List of block names (case insensitive)
;; Returns: [lst] List of blocks that were successfully deleted.

(defun LM:deleteblocks ( doc lst / blc lck )
    (setq blc (vla-get-blocks doc))
    (if (setq lst (mapcar 'strcase (vl-remove-if-not '(lambda ( blk ) (LM:catchapply 'vla-item (list blc blk))) lst)))
        (progn
            (vlax-for lay (vla-get-layers doc)
                (if (= :vlax-true (vla-get-lock lay))
                    (progn
                        (setq lck (cons lay lck))
                        (vla-put-lock lay :vlax-false)
                    )
                )
            )
            (vlax-for def blc
                (vlax-for obj def
                    (if (and (= "AcDbBlockReference" (vla-get-objectname obj))
                             (member (strcase (LM:blockname obj)) lst)
                        )
                        (vl-catch-all-apply 'vla-delete (list obj))
                    )
                )
            )
            (setq lst (vl-remove-if-not '(lambda ( blk ) (LM:catchapply 'vla-delete (list (vla-item blc blk)))) lst))
            (foreach lay lck (vla-put-lock lay :vlax-true))
            lst
        )
    )
)

;; Catch Apply  -  Lee Mac
;; Applies a function to a list of parameters and catches any exceptions.
 
(defun LM:catchapply ( fnc prm / rtn )
    (if (not (vl-catch-all-error-p (setq rtn (vl-catch-all-apply fnc prm))))
        (cond ( rtn ) ( t ))
    )
)

;; Filtered List Box  -  Lee Mac
;; Displays a list box interface from which the user may select one or more items.
;; Includes an edit box filter to enable the user to filter the displayed list of items.
;; msg - [str] List box dialog title
;; lst - [lst] List of strings to display in the list box
;; mtp - [bol] T=Allow multiple items; nil=Single item selection
;; Returns: [lst] List of selected items, else nil

(defun LM:filtlistbox ( msg lst mtp / _addlist dch dcl des rtn sel tmp )

    (defun _addlist ( key lst )
        (start_list key)
        (foreach x lst (add_list x))
        (end_list)
        lst
    )

    (if
        (and
            (setq dcl (vl-filename-mktemp nil nil ".dcl"))
            (setq des (open dcl "w"))
            (write-line
                (strcat
                    "filtlistbox : dialog { label = \"" msg "\"; spacer;"
                    ": list_box { key = \"lst\"; width = 50; fixed_width = true; height = 15; fixed_height = true; allow_accept = true; "
                    "multiple_select = " (if mtp "true" "false") "; }"
                    ": edit_box { key = \"flt\"; width = 50; fixed_width = true; label = \"Filter:\"; }"
                    "spacer; ok_cancel; }"
                )
                des
            )
            (not (close des))
            (< 0 (setq dch (load_dialog dcl)))
            (new_dialog "filtlistbox" dch)
        )
        (progn
            (_addlist "lst" (setq tmp lst))
            (set_tile "lst" (setq rtn "0"))
            (set_tile "flt" "*")
            (action_tile "lst" "(setq rtn $value)")
            (action_tile "flt"
                (vl-prin1-to-string
                   '(progn
                        (setq flt (strcat "*" (strcase $value) "*")
                              sel (mapcar '(lambda ( n ) (nth n tmp)) (read (strcat "(" rtn ")")))
                        )
                        (_addlist "lst" (setq tmp (vl-remove-if-not '(lambda ( x ) (wcmatch (strcase x) flt)) lst)))
                        (set_tile "lst"
                            (setq rtn
                                (vl-string-trim "()"
                                    (vl-princ-to-string
                                        (cond
                                            (   (vl-sort (vl-remove nil (mapcar '(lambda ( x ) (vl-position x tmp)) sel)) '<))
                                            (  '(0)   )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
            (setq rtn
                (if (= 1 (start_dialog))
                    (mapcar '(lambda ( x ) (nth x tmp)) (read (strcat "(" rtn ")")))
                )
            )
        )
    )
    (if (< 0 dch)
        (setq dch (unload_dialog dch))
    )
    (if (and (= 'str (type dcl)) (findfile dcl))
        (vl-file-delete dcl)
    )
    rtn
)

;; Block Name  -  Lee Mac
;; Returns the true (effective) name of a supplied block reference
                        
(defun LM:blockname ( obj )
    (if (vlax-property-available-p obj 'effectivename)
        (defun LM:blockname ( obj ) (vla-get-effectivename obj))
        (defun LM:blockname ( obj ) (vla-get-name obj))
    )
    (LM:blockname obj)
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns a global pointer to the VLA Active Document Object

(defun LM:acdoc nil
    (cond ( acdoc ) ((setq acdoc (vla-get-activedocument (vlax-get-acad-object)))))
)

;;----------------------------------------------------------------------;;

(vl-load-com) (princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;

;;------------------------=={ Get Files Dialog }==----------------------;;
;;                                                                      ;;
;;  An analog of the 'getfiled' function for multiple file selection.   ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright � 2012  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Arguments:                                                          ;;
;;  msg - [str/nil] Dialog box label; 'Select Files' if nil or "".      ;;
;;  def - [str/nil] Default directory; dwgprefix if nil or "".          ;;
;;  ext - [str/nil] File extension filter (e.g. "dwg;lsp"); "*" if nil  ;;
;;----------------------------------------------------------------------;;
;;  Returns:  List of selected files, else nil                          ;;
;;----------------------------------------------------------------------;;
;;  Version 1.6    -    2016-03-21                                      ;;
;;----------------------------------------------------------------------;;

(defun LM:getfiles ( msg def ext / *error* dch dcl des dir dirdata lst rtn )

    (defun *error* ( msg )
        (if (= 'file (type des))
            (close des)
        )
        (if (and (= 'int (type dch)) (< 0 dch))
            (unload_dialog dch)
        )
        (if (and (= 'str (type dcl)) (findfile dcl))
            (vl-file-delete dcl)
        )
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )    
    
    (if
        (and
            (setq dcl (vl-filename-mktemp nil nil ".dcl"))
            (setq des (open dcl "w"))
            (progn
                (foreach x
                   '(
                        "lst : list_box"
                        "{"
                        "    width = 40.0;"
                        "    height = 20.0;"
                        "    fixed_width = true;"
                        "    fixed_height = true;"
                        "    alignment = centered;"
                        "    multiple_select = true;"
                        "}"
                        "but : button"
                        "{"
                        "    width = 20.0;"
                        "    height = 1.8;"
                        "    fixed_width = true;"
                        "    fixed_height = true;"
                        "    alignment = centered;"
                        "}"
                        "getfiles : dialog"
                        "{"
                        "    key = \"title\"; spacer;"
                        "    : row"
                        "    {"
                        "        alignment = centered;"
                        "        : edit_box { key = \"dir\"; label = \"Folder:\"; }"
                        "        : button"
                        "        {"
                        "            key = \"brw\";"
                        "            label = \"Browse\";"
                        "            fixed_width = true;"
                        "        }"
                        "    }"
                        "    spacer;"
                        "    : row"
                        "    {"
                        "        : column"
                        "        {"
                        "            : lst { key = \"box1\"; }"
                        "            : but { key = \"add\" ; label = \"Add Files\"; }"
                        "        }"
                        "        : column {"
                        "            : lst { key = \"box2\"; }"
                        "            : but { key = \"del\" ; label = \"Remove Files\"; }"
                        "        }"
                        "    }"
                        "    spacer; ok_cancel;"
                        "}"
                    )
                    (write-line x des)
                )
                (setq des (close des))
                (< 0 (setq dch (load_dialog dcl)))
            )
            (new_dialog "getfiles" dch)
        )
        (progn
            (setq ext (if (= 'str (type ext)) (LM:getfiles:str->lst (strcase ext) ";") '("*")))
            (set_tile "title" (if (member msg '(nil "")) "Select Files" msg))
            (set_tile "dir"
                (setq dir
                    (LM:getfiles:fixdir
                        (if (or (member def '(nil "")) (not (vl-file-directory-p (LM:getfiles:fixdir def))))
                            (getvar 'dwgprefix)
                            def
                        )
                    )
                )
            )
            (setq lst (LM:getfiles:updatefilelist dir ext nil))
            (mode_tile "add" 1)
            (mode_tile "del" 1)

            (action_tile "brw"
                (vl-prin1-to-string
                   '(if (setq tmp (LM:getfiles:browseforfolder "" nil 512))
                        (setq lst (LM:getfiles:updatefilelist (set_tile "dir" (setq dir tmp)) ext rtn)
                              rtn (LM:getfiles:updateselected dir rtn)
                        )                              
                    )
                )
            )

            (action_tile "dir"
                (vl-prin1-to-string
                   '(if (= 1 $reason)
                        (setq lst (LM:getfiles:updatefilelist (set_tile "dir" (setq dir (LM:getfiles:fixdir $value))) ext rtn)
                              rtn (LM:getfiles:updateselected dir rtn)
                        )
                    )
                )
            )

            (action_tile "box1"
                (vl-prin1-to-string
                   '(
                        (lambda ( / itm tmp )
                            (if (setq itm (mapcar '(lambda ( n ) (nth n lst)) (read (strcat "(" $value ")"))))
                                (if (= 4 $reason)
                                    (cond
                                        (   (equal '("..") itm)
                                            (setq lst (LM:getfiles:updatefilelist (set_tile "dir" (setq dir (LM:getfiles:updir dir))) ext rtn)
                                                  rtn (LM:getfiles:updateselected dir rtn)
                                            )
                                        )
                                        (   (vl-file-directory-p (setq tmp (LM:getfiles:checkredirect (strcat dir "\\" (car itm)))))
                                            (setq lst (LM:getfiles:updatefilelist (set_tile "dir" (setq dir tmp)) ext rtn)
                                                  rtn (LM:getfiles:updateselected dir rtn)
                                            )
                                        )
                                        (   (setq rtn (LM:getfiles:sort (append rtn (mapcar '(lambda ( x ) (strcat dir "\\" x)) itm)))
                                                  rtn (LM:getfiles:updateselected dir rtn)
                                                  lst (LM:getfiles:updatefilelist dir ext rtn)
                                            )
                                        )
                                    )
                                    (if (vl-every '(lambda ( x ) (vl-file-directory-p (strcat dir "\\" x))) itm)
                                        (mode_tile "add" 1)
                                        (mode_tile "add" 0)
                                    )
                                )
                            )
                        )
                    )
                )
            )

            (action_tile "box2"
                (vl-prin1-to-string
                   '(
                        (lambda ( / itm )
                            (if (setq itm (mapcar '(lambda ( n ) (nth n rtn)) (read (strcat "(" $value ")"))))
                                (if (= 4 $reason)
                                    (setq rtn (LM:getfiles:updateselected dir (vl-remove (car itm) rtn))
                                          lst (LM:getfiles:updatefilelist dir ext rtn)
                                    )
                                    (mode_tile "del" 0)
                                )
                            )
                        )
                    )
                )
            )

            (action_tile "add"
                (vl-prin1-to-string
                   '(
                        (lambda ( / itm )
                            (if
                                (setq itm
                                    (vl-remove-if 'vl-file-directory-p
                                        (mapcar '(lambda ( n ) (nth n lst)) (read (strcat "(" (get_tile "box1") ")")))
                                    )
                                )
                                (setq rtn (LM:getfiles:sort (append rtn (mapcar '(lambda ( x ) (strcat dir "\\" x)) itm)))
                                      rtn (LM:getfiles:updateselected dir rtn)
                                      lst (LM:getfiles:updatefilelist dir ext rtn)
                                )
                            )
                            (mode_tile "add" 1)
                            (mode_tile "del" 1)
                        )
                    )
                )
            )

            (action_tile "del"
                (vl-prin1-to-string
                   '(
                        (lambda ( / itm )
                            (if (setq itm (read (strcat "(" (get_tile "box2") ")")))
                                (setq rtn (LM:getfiles:updateselected dir (LM:getfiles:removeitems itm rtn))
                                      lst (LM:getfiles:updatefilelist dir ext rtn)
                                )
                            )
                            (mode_tile "add" 1)
                            (mode_tile "del" 1)
                        )
                    )
                )
            )
         
            (if (zerop (start_dialog))
                (setq rtn nil)
            )
        )
    )
    (*error* nil)
    rtn
)

(defun LM:getfiles:listbox ( key lst )
    (start_list key)
    (foreach x lst (add_list x))
    (end_list)
    lst
)

(defun LM:getfiles:listfiles ( dir ext lst )
    (vl-remove-if '(lambda ( x ) (member (strcat dir "\\" x) lst))
        (cond
            (   (cdr (assoc dir dirdata)))
            (   (cdar
                    (setq dirdata
                        (cons
                            (cons dir
                                (append
                                    (LM:getfiles:sortlist (vl-remove "." (vl-directory-files dir nil -1)))
                                    (LM:getfiles:sort
                                        (if (member ext '(("") ("*")))
                                            (vl-directory-files dir nil 1)
                                            (vl-remove-if-not
                                                (function
                                                    (lambda ( x / e )
                                                        (and
                                                            (setq e (vl-filename-extension x))
                                                            (setq e (strcase (substr e 2)))
                                                            (vl-some '(lambda ( w ) (wcmatch e w)) ext)
                                                        )
                                                    )
                                                )
                                                (vl-directory-files dir nil 1)
                                            )
                                        )
                                    )
                                )
                            )
                            dirdata
                        )
                    )
                )
            )
        )
    )
)

(defun LM:getfiles:checkredirect ( dir / itm pos )
    (cond
        (   (vl-directory-files dir) dir)
        (   (and
                (=  (strcase (getenv "UserProfile"))
                    (strcase (substr dir 1 (setq pos (vl-string-position 92 dir nil t))))
                )
                (setq itm
                    (cdr
                        (assoc (substr (strcase dir t) (+ pos 2))
                           '(
                                ("my documents" . "Documents")
                                ("my pictures"  . "Pictures")
                                ("my videos"    . "Videos")
                                ("my music"     . "Music")
                            )
                        )
                    )
                )
                (vl-file-directory-p (setq itm (strcat (substr dir 1 pos) "\\" itm)))
            )
            itm
        )
        (   dir   )
    )
)

(defun LM:getfiles:sort ( lst )
    (apply 'append
        (mapcar 'LM:getfiles:sortlist
            (vl-sort
                (LM:getfiles:groupbyfunction lst
                    (lambda ( a b / x y )
                        (and
                            (setq x (vl-filename-extension a))
                            (setq y (vl-filename-extension b))
                            (= (strcase x) (strcase y))
                        )
                    )
                )
                (function
                    (lambda ( a b / x y )
                        (and
                            (setq x (vl-filename-extension (car a)))
                            (setq y (vl-filename-extension (car b)))
                            (< (strcase x) (strcase y))
                        )
                    )
                )
            )
        )
    )
)

(defun LM:getfiles:sortlist ( lst )
    (mapcar (function (lambda ( n ) (nth n lst)))
        (vl-sort-i (mapcar 'LM:getfiles:splitstring lst)
            (function
                (lambda ( a b / x y )
                    (while
                        (and
                            (setq x (car a))
                            (setq y (car b))
                            (= x y)
                        )
                        (setq a (cdr a)
                              b (cdr b)
                        )
                    )
                    (cond
                        (   (null x) b)
                        (   (null y) nil)
                        (   (and (numberp x) (numberp y)) (< x y))
                        (   (numberp x))
                        (   (numberp y) nil)
                        (   (< x y))
                    )
                )
            )
        )
    )
)

(defun LM:getfiles:groupbyfunction ( lst fun / tmp1 tmp2 x1 )
    (if (setq x1 (car lst))
        (progn
            (foreach x2 (cdr lst)
                (if (fun x1 x2)
                    (setq tmp1 (cons x2 tmp1))
                    (setq tmp2 (cons x2 tmp2))
                )
            )
            (cons (cons x1 (reverse tmp1)) (LM:getfiles:groupbyfunction (reverse tmp2) fun))
        )
    )
)

(defun LM:getfiles:splitstring ( str )
    (
        (lambda ( l )
            (read
                (strcat "("
                    (vl-list->string
                        (apply 'append
                            (mapcar
                                (function
                                    (lambda ( a b c )
                                        (cond
                                            (   (member b '(45 46 92))
                                                (list 32)
                                            )
                                            (   (< 47 b 58)
                                                (list b)
                                            )
                                            (   (list 32 34 b 34 32))
                                        )
                                    )
                                )
                                (cons nil l) l (append (cdr l) '(( )))
                            )
                        )
                    )
                    ")"
                )
            )
        )
        (vl-string->list (strcase str))
    )
)

(defun LM:getfiles:browseforfolder ( msg dir flg / err fld pth shl slf )
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
                              pth (LM:getfiles:fixdir (vlax-get-property slf 'path))
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

(defun LM:getfiles:full->relative ( dir path / p q )
    (setq dir (vl-string-right-trim "\\" dir))
    (cond
        (   (and
                (setq p (vl-string-position 58  dir))
                (setq q (vl-string-position 58 path))
                (/= (strcase (substr dir 1 p)) (strcase (substr path 1 q)))
            )
            path
        )
        (   (and
                (setq p (vl-string-position 92  dir))
                (setq q (vl-string-position 92 path))
                (= (strcase (substr dir 1 p)) (strcase (substr path 1 q)))
            )
            (LM:getfiles:full->relative (substr dir (+ 2 p)) (substr path (+ 2 q)))
        )
        (   (and
                (setq q (vl-string-position 92 path))
                (= (strcase dir) (strcase (substr path 1 q)))
            )
            (strcat ".\\" (substr path (+ 2 q)))
        )
        (   (= "" dir)
            path
        )
        (   (setq p (vl-string-position 92 dir))
            (LM:getfiles:full->relative (substr dir (+ 2 p)) (strcat "..\\" path))
        )
        (   (LM:getfiles:full->relative "" (strcat "..\\" path)))
    )
)

(defun LM:getfiles:str->lst ( str del / pos )
    (if (setq pos (vl-string-search del str))
        (cons (substr str 1 pos) (LM:getfiles:str->lst (substr str (+ pos 1 (strlen del))) del))
        (list str)
    )
)

(defun LM:getfiles:updatefilelist ( dir ext lst )
    (LM:getfiles:listbox "box1" (LM:getfiles:listfiles dir ext lst))
)

(defun LM:getfiles:updateselected ( dir lst )
    (LM:getfiles:listbox "box2" (mapcar '(lambda ( x ) (LM:getfiles:full->relative dir x)) lst))
    lst
)

(defun LM:getfiles:updir ( dir )
    (substr dir 1 (vl-string-position 92 dir nil t))
)

(defun LM:getfiles:fixdir ( dir )
    (vl-string-right-trim "\\" (vl-string-translate "/" "\\" dir))
)

(defun LM:getfiles:removeitems ( itm lst / idx )
    (setq idx -1)
    (vl-remove-if '(lambda ( x ) (member (setq idx (1+ idx)) itm)) lst)
)

(vl-load-com) (princ)

;;-----------------------=={ ObjectDBX Wrapper }==----------------------;;
;;                                                                      ;;
;;  Evaluates a supplied function on all drawings in a given list or    ;;
;;  selected directory.                                                 ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright � 2013  -  www.lee-mac.com              ;;
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
;;  Author: Lee Mac, Copyright � 2013 - www.lee-mac.com       ;;
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

(defun c:dbtest ( / lst )
   (if (setq lst (LM:getfiles "Select Drawings to Process" nil "dwg;dws;dwt"))
       (LM:odbx '(lambda ( doc ) (LM:deleteblocks doc '("A_Sheet_MBE - A1-3564001-TEMA PARK İSTASYONU ANAHTAR PLANLAR"))) lst t)
       (princ "\n*Cancel*")
   )
   (princ)
)