;;; Import
(vl-load-com)

;;; Global Variables
;;;--------------------------------------------------------------
;;; First step is to load ActiveX functionality. If ActiveX support 
;;; already exists in document (can occur when Bonus tools have been 
;;; loaded into AutoCAD), nothing happens. Otherwise, ActiveX 
;;; support is loaded.                                
;;;---------------------------------------------------------------
(vl-load-com)
;;; In Lesson 4, the following comment and code is moved to utils.lsp
;;;---------------------------------------------------------------
;;; For ActiveX functions, we need to define a global variable that 
;;; "points" to the Model Space portion of the active drawing. This 
;;; variable, named *ModelSpace* will be created at load time.      
;;;---------------------------------------------------------------
(setq *ModelSpace*
      (vla-get-ModelSpace
        (vla-get-ActiveDocument (vlax-get-acad-object))
      ) ;_ end of vla-get-ModelSpace
) ;_ end of setq


(defun gp:list->variantArray (ptsList / arraySpace sArray)
  ; allocate space for an array of 2d points stored as doubles
  (setq arraySpace (vlax-make-safearray
              vlax-vbdouble ; element type
              (cons 0
                    (- (length ptsList) 1) ; listenin boyutundan -1 
                    ) ; array dimension
              )
  )

  (setq sArray (vlax-safearray-fill arraySpace ptsList))

  ; return array variant
  (vlax-make-variant sArray)
)

(defun gp:getPointInput (/ StartPt EndPt HalfWidth) 
  (if (setq StartPt (getpoint "\nStart point of path: ")) 
    (if (setq EndPt (getpoint StartPt "\nEnd point of path: "))  ; The endpoint is then acquired in relation to the chosen start point.
      (if (setq HalfWidth (getdist EndPt "\nHalf-Width of path: ")) 
        (list 
          (cons 10 StartPt)
          (cons 11 EndPt)
          (cons 40 (* HalfWidth 2.0))
          (cons 41 (distance StartPt EndPt))
          (cons 50 (angle StartPt EndPt))
        )
      )
    )
  )
)

;;;--------------------------------------------------------------;
;;; Function: Degrees->Radians                                   ;
;;;--------------------------------------------------------------;
;;; Description: This function converts a number representing an ;
;;;              angular measurement in degrees, into its radian ;
;;;              equivalent. There is no error checking on the   ;
;;;              numberOfDegrees parameter -- it is always       ;
;;;              expected to be a valid number.                  ;
;;;--------------------------------------------------------------;

(defun Degrees->Radians (numberOfDegrees) 
  (* pi (/ numberOfDegrees 180.0))
)                                                                

;;;--------------------------------------------------------------;

;;;--------------------------------------------------------------;
;;; Function: 3dPoint->2dPoint                                   ;
;;;--------------------------------------------------------------;
;;; Description: This function takes one parameter representing a;
;;;              3D point (list of three integers or reals), and ;
;;;              converts it into a 2D point (list of two reals).;
;;;              There is no error checking on the 3D point      ;
;;;              parameter -- it is assumed to be a valid point. ;
;;;--------------------------------------------------------------;
;;; To do: Add some kind of parameter checking so that this      ;
;;;        function won't crash a program if it is passed a      ;
;;;        null value, or some other kind of data type than a    ;
;;;        3D point.                                             ;
;;;        (listp '(1 1 0)) => T                                 ;
;;;        (numberp 3.4) => T                                    ;
;;;--------------------------------------------------------------;

(defun 3dPoint->2dPoint (3dpt) 
  (list (float (car 3dpt)) (float (cadr 3dpt))) ;car takes the first argument
)                                             ;cadr takes the second

;;;--------------------------------------------------------------;


(defun gp:getDilaogInput () 
  (prompt "getDilaogInput function is working")
  T
)

;;;--------------------------------------------------------------;
;;;     Function: gp:drawOutline                                 ;
;;;--------------------------------------------------------------;
;;; Description: This function draws the outline of the          ;
;;;              garden path.                                    ;
;;;--------------------------------------------------------------;

;;;---------------------------------------------------------------
;;;     Function: gp:drawOutline                                  
;;;---------------------------------------------------------------
;;;  Description: This function will draw the outline of the garden
;;;               path.                                            
;;;--------------------------------------------------------------- 
;;;  Note: No error checking or validation is performed on the     
;;;  BoundaryData parameter.  The sequence of items within this
;;;  parameter does not matter, but it is assumed that all sublists
;;;  are present and contain valid data.                           
;;; --------------------------------------------------------------
(defun gp:drawOutline (BoundaryData / VLADataPts PathAngle
                                      Width HalfWidth StartPt PathLength
                                      angm90 angp90 p1 p2
                                      p3 p4 polypoints pline
                      )
  ;; extract the values from the list BoundaryData
  (setq PathAngle (cdr (assoc 50 BoundaryData))
        Width (cdr (assoc 40 BoundaryData))
        HalfWidth (/ Width 2.00)
        StartPt (cdr (assoc 10 BoundaryData))
        PathLength (cdr (assoc 41 BoundaryData))
        angp90 (+ PathAngle (Degrees->Radians 90))
        angm90 (- PathAngle (Degrees->Radians 90))
        p1 (polar StartPt angm90 HalfWidth)
        p2 (polar p1 PathAngle PathLength)
        p3 (polar p2 angp90 Width)
        p4 (polar p3 (+ PathAngle (Degrees->Radians 180)) PathLength)
        polypoints (apply 'append
                     (mapcar '3dPoint->2dPoint (list p1 p2 p3 p4))
                   )
  )

  ;; ***** data conversion *****
  ;; Notice, polypoints is in AutoLISP format, consisting of a list
  ;; of the 4 corner points for the garden path.
  ;; The variable needs to be converted to a form of input parameter
  ;; acceptable to ActiveX calls.
  (setq VLADataPts (gp:list->variantArray polypoints))

  ;; Add polyline to the model space using ActiveX automation.
  (setq	pline (vla-addLightweightPolyline
                 *ModelSpace*; Global Definition for Model Space
                 VLADataPts
              ) ;_ end of vla-addLightweightPolyline
  ) ;_ end of setq

  (vla-put-closed pline T)
  ;; Return the ActiveX object name for the outline polyline
  ;; The return value should look something like this:
  ;; #<VLA-OBJECT IAcadLWPolyline 02351a34> 
  pline
) ;_ end of defun

;;;--------------------------------------------------------------;


;;;**************************************************************;
;;;     Function: C:GPath        The Main Garden Path Function   ;
;;;--------------------------------------------------------------;
;;;  Description: This is the main garden path function. It is a ;
;;;               C: function, meaning that it is turned into an ;
;;;               AutoCAD command called GPATH. This function    ;
;;;               determines the overall flow of the garden path ;
;;;               program.                                       ;
;;;**************************************************************;
;;; The gp_PathData variable is an association list of the form: ;
;;;  (10 . Starting Point) - List of 3 reals (a point) denoting  ;
;;;                           starting point of the garden path. ;
;;;  (11 . Ending Point)   - List of 3 reals (a point) denoting  ;
;;;                           endpoint of the garden path.       ;
;;;  (40 . Width)          - Real number denoting boundary       ;
;;;                           width.                             ;
;;;  (41 . Length)         - Real number denoting boundary       ;
;;;                           length.                            ;
;;;  (50 . Path Angle)     - Real number denoting the angle of   ;
;;;                           the path, in radians.              ;
;;;  (42 . Tile Size)      - Real number denoting the size       ;
;;;                           (radius) of the garden path tiles. ;
;;;  (43 . Tile Offset)    - Spacing of tiles, border to border. ;
;;;  ( 3 . Object Creation Style)                                ;
;;;                        - Object creation style indicates how ;
;;;                          the tiles are to be drawn.  The     ;
;;;                          expected value is a string and one  ;
;;;                          one of three values (string case    ;
;;;                          is unimportant):                    ;
;;;                              "ActiveX"                       ;
;;;                              "Entmake"                       ;
;;;                              "Command"                       ;
;;;  ( 4 . Polyline Border Style)                                ;
;;;                        - Polyline border style determines    ;
;;;                          the polyline type to be used for    ;
;;;                          path boundary.  The expected value  ;
;;;                          one of the following (string case is;
;;;                          unimportant):                       ;
;;;                              "Pline"                         ;
;;;                              "Light"                         ;
;;;**************************************************************;

(defun c:GPath (/ gp_PathData) 
  (if (setq gp_PathData (gp:getPointInput))  ;Fonkisyonun çalışıp çalışmadığını kontrol eder.
    ;gp: to indicate they are specific to the garden path application. Not a requirement
    (if (gp:getDilaogInput)  ;Fonkisyonun çalışıp çalışmadığını kontrol eder.
      (progn  ;progn bloğunun içinde bulunan hesaplmalardan dışarıya yalnızca son argüman çıkar. Diğerleri çıkmaz.
             (setq PolyLineName (gp:drawOutLine gp_PathData))
             (princ "\nThe gp:drawOutLine function returned")
             (princ PolyLineName)
             (princ ">")
             (prompt "Congratulations - your program is complete")
      ) ;progn end
      (princ "\nFunction cancelled")
    ) ;end if
    (princ "\nIncomplete information to draw a boundary")
  ) ; end if
  (princ)
) ;enddefun


(prompt "\nType GPATH to draw a garden path.") ;Fonksiyon çağırıldığında kullanıcıya talimat verir.
(princ)

;;;**************************************************************;