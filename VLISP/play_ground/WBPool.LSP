(defun rh:lll (c / lst)
  (and (= "AcDbLayerTable" (vlax-get-property c 'objectname))
       (vlax-map-collection c '(lambda (x) (if (= :vlax-true (vlax-get-property x 'lock)) (setq lst (cons x lst)))))
  );end_and
  (if lst lst nil)
);end_defun

(defun rh:unlock_lyrs (lst) (mapcar '(lambda (x) (vlax-put-property x 'lock :vlax-false)) lst))

(defun rh:relock_lyrs (lst) (mapcar '(lambda (x) (vlax-put-property x 'lock :vlax-true)) lst))

(vl-load-com)

(defun c:WBPool (/ *error* cdoc clyrs lklst vl ov dnme dcnt ss cnt ent el lyr lent flg ass dfile ans)

  (defun *error* ( msg )
    (rh:relock_lyrs lklst)
    (if (and cdoc (= 8 (logand 8 (getvar 'UNDOCTL)))) (vla-endundomark cdoc))
    (and ov (mapcar 'setvar vl ov))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*") (princ (strcat "\n** Error: " msg " **")))
    (command-s "undo" 1)
    (princ)
  );end_defun
  
  (setq cdoc (vla-get-activedocument (vlax-get-acad-object))
        clyrs (vla-get-layers cdoc)
        vl (list 'cmdecho 'dynmode 'dynprompt)
        ov (mapcar 'getvar vl)
        lklst (rh:lll clyrs)
        dnme "Pool"
        dcnt 1
  );end_setq

  (mapcar 'setvar vl '(0 3 1))

  (if (and cdoc (= 8 (logand 8 (getvar 'UNDOCTL)))) (vla-endundomark cdoc))
  (vla-startundomark cdoc)

  (rh:unlock_lyrs lklst)

  (foreach x (cons "Model" (layoutlist))
    (setvar 'ctab x)
    (setq ss (ssget "_X" (list '(0 . "MULTILEADER") (cons 410 x))))
    (cond (ss
            (repeat (setq cnt (sslength ss))
              (setq el (entget (setq ent (ssname ss (setq cnt (1- cnt)))))
                    lyr (cdr (assoc 8 el))
                    lent (tblsearch "layer" lyr)
              );end_setq
              (if (not (= 4 (logand 4 (cdr (assoc 70 lent))))) (command "_.explode" ent ""))
            );end_repeat
          )
    );end_cond
  );end_foreach

  (setq ss nil)
  (setvar 'ctab "Model")

  (rh:relock_lyrs lklst)

  (while (not flg)
    (prompt (strcat "\nSelect Objects for " dnme ".dwg by Window"))
    (setq ss (ssget ":L"))
    (cond (ss
            (setq ass (vlax-get-property cdoc 'activeselectionset)
                  dfile (strcat (getvar 'dwgprefix) dnme ".dwg")
            );end_setq
            (vlax-for itm ass (vlax-put itm 'color 7))
            (vlax-invoke cdoc 'wblock dfile ass)
            (setq dcnt (1+ dcnt) dnme (strcat dnme (itoa dcnt)))
          )
          (t (alert "Nothing Selected"))
    );end_cond
    (vla-delete ass)
    (initget 7 "Yes No")
    (setq ans (cond ( (getkword "\nAnother WBlock Selection :? [Yes/No] <Yes>"))))
    (if (= ans "No") (setq flg T))
  );end_while

  (if (and cdoc (= 8 (logand 8 (getvar 'UNDOCTL)))) (vla-endundomark cdoc))
  (and ov (mapcar 'setvar vl ov))
  (command "undo" 1)
  (princ)
);end_defun


