(vl-load-com)

(defun c:cpl ( / acobj acdoc model docs dbx acver file path doc l)
  (setq acobj (vlax-get-acad-object)
        acdoc (vla-get-activedocument acobj)
        model (vla-get-modelspace acdoc)
        docs  (vla-get-documents acobj)
  )
  (setq dbx
    (vla-GetInterfaceObject acobj
      (if
        (< (setq acver (atoi (getvar "ACADVER"))) 16)
        "ObjectDBX.AxDbDocument"
        (strcat "ObjectDBX.AxDbDocument." (itoa acver))
      )
    )
  )
;;;  (setq reference_point (vlax-3d-point 0.0 0.0 0.0))
  (while
;;;    (and
      (progn
        (princ)
        (setq file (getfiled "Select file to import lines" (cond (path) ((getvar 'dwgprefix))) "dwg" 2))
      )
;;;      (setq insertion_point (getpoint "\nSpecify insertion point: "))
;;;      (setq insertion_point (vlax-3d-point insertion_point))
;;;    )
    (setq path (strcat (vl-filename-directory file) "\\")
          doc nil
          l nil
    )
    (vlax-for x docs
      (if
        (eq (vla-get-fullname x) file)
        (setq doc x)
      )
    )
    (or doc (vla-open dbx file :vlax-true))
    (vlax-for obj (vla-get-modelspace (cond (doc) (dbx)))
      (if
        (eq (vla-get-objectname obj) "AcDbLine")
        (setq l (cons obj l))
      )
    )
    (if l
;;;      (foreach x
        (vlax-invoke (cond (doc) (dbx)) 'copyobjects l model)
;;;        (vla-move x reference_point insertion_point)
;;;      )
    )
    (princ (strcat "\n" (itoa (length l)) " lines imported from " (strcase (vl-filename-base file))))
  )
  (vlax-release-object dbx)
  (princ)
)

(alert "I'm here")