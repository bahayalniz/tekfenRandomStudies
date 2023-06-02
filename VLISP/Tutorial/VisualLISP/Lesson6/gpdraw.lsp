;;;                                                                    ;
;;;  GPDRAW.LSP                                                        ;
;;;                                                                    ;
;;;  Copyright 1987, 1988, 1990, 1992, 1994, 1996, 1997, 1998          ;
;;;  by Autodesk, Inc. All Rights Reserved.                            ;
;;;                                                                    ;
;;;  You are hereby granted permission to use, copy and modify this    ;
;;;  software without charge, provided you do so exclusively for       ;
;;;  your own use or for use by others in your organization in the     ;
;;;  performance of their normal duties, and provided further that     ;
;;;  the above copyright notice appears in all copies and both that    ;
;;;  copyright notice and the limited warranty and restricted rights   ;
;;;  notice below appear in all supporting documentation.              ;
;;;                                                                    ;
;;;  Incorporation of any part of this software into other software,   ;
;;;  except when such incorporation is exclusively for your own use    ;
;;;  or for use by others in your organization in the performance of   ;
;;;  their normal duties, is prohibited without the prior written      ;
;;;  consent of Autodesk, Inc.                                         ;
;;;                                                                    ;
;;;  Copying, modification and distribution of this software or any    ;
;;;  part thereof in any form except as expressly provided herein is   ;
;;;  prohibited without the prior written consent of Autodesk, Inc.    ;
;;;                                                                    ;
;;;  AUTODESK PROVIDES THIS SOFTWARE "AS IS" AND WITH ALL FAULTS.      ;
;;;  AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF           ;
;;;  MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK,       ;
;;;  INC. DOES NOT WARRANT THAT THE OPERATION OF THE SOFTWARE          ;
;;;  WILL BE UNINTERRUPTED OR ERROR FREE.                              ;
;;;                                                                    ;
;;;  Restricted Rights for US Government Users.  This software         ;
;;;  and Documentation are provided with RESTRICTED RIGHTS for US      ;
;;;  US Government users.  Use, duplication, or disclosure by the      ;
;;;  Government is subject to restrictions as set forth in FAR         ;
;;;  12.212 (Commercial Computer Software-Restricted Rights) and       ;
;;;  DFAR 227.7202 (Rights in Technical Data and Computer Software),   ;
;;;  as applicable.  Manufacturer is Autodesk, Inc., 111 McInnis       ;
;;;  Parkway, San Rafael, California 94903.                            ;
;;;                                                                    ;

;;;--------------------------------------------------------------------;
;;;  This file is from the Garden Path tutorial, and represents the    ;
;;;  state of the application at the end of Lesson 6.  Use this file   ;
;;;  to check your work, or to start off Lesson 7 with the code as it  ;
;;;  appears in the tutorial.                                          ;
;;;--------------------------------------------------------------------;


;;;--------------------------------------------------------------------;
;;;     Function: gp:drawOutline                                       ;
;;;--------------------------------------------------------------------;
;;;  Description: This function will draw the outline of the garden    ;
;;;               path.                                                ;
;;;--------------------------------------------------------------------;
;;;  Note: no error checking or validation is performed on the         ;
;;;  BoundaryData parameter.  The sequence of items within this        ;
;;;  parameter do not matter, but it is assumed that all sublists      ;
;;;  are present, and contain valid data.                              ;
;;;--------------------------------------------------------------------;
;;;  Note: This function uses Activex as a means to produce the garden ;
;;;  path boundary.  The reason for this will become apparent during   ;
;;;  future lessons.  But here is a hint:  certain entity creation     ;
;;;  methods will not work from within a reactor-triggered function    ;
;;;--------------------------------------------------------------------;
;;;  The BoundaryData parameter is expected to be of the form:         ;
;;;   (10 . Starting Point) -- A list of 3 reals (a point) denotes     ;
;;;                              the starting point of the garden path ;
;;;   (11 . Ending Point)   -- A list of 3 reals (a point) denotes     ;
;;;                              the ending point of the garden path   ;
;;;   (40 . Width)          -- A real number denoting boundary width   ;
;;;   (41 . Length)         -- A real number denoting boundary length  ;
;;;   (50 . Path Angle)     -- A real number denoting the angle of the ;
;;;                              path, in radians                      ;
;;;   (42 . Tile Size)      -- A real number denoting the size         ;
;;;                              (radius) of the garden path tiles     ;
;;;   (43 . Tile Offset)    -- Spacing of tiles, border to border      ;
;;;   ( 3 . Object Creation Style)                                     ;
;;;                         -- The object creation style indicates how ;
;;;                               the tiles are to be drawn.  The      ;
;;;                               expected value is a string and one   ;
;;;                               one of three values (string case is  :
;;;                               unimportant):                        ;
;;;                                "ActiveX"                           ;
;;;                                "Entmake"                           ;
;;;                                "Command"                           ;
;;;   ( 4 . Polyline Border Style)                                     ;
;;;                          -- The polyline border style determines   ;
;;;                               the polyline type to be used for the ;
;;;                               path boundary.  The expected value   ;
;;;                               one of two values (string case is    :
;;;                               unimportant):                        ;
;;;                                "Pline"                             ;
;;;                                "Light"                             ;
;;;--------------------------------------------------------------------;
;;;  NOTE: No validity checking is performed on BoundaryData!!!        ;
;;;--------------------------------------------------------------------;
(defun gp:drawOutline (BoundaryData	       /	   PathAngle
		       Width	   HalfWidth   StartPt	   PathLength
		       angm90	   angp90      p1	   p2
		       p3	   p4	       poly2Dpoints
		       poly3Dpoints	       plineStyle  pline
		       )
  ;; extract the values from the list BoundaryData
  (setq	PathAngle    (cdr (assoc 50 BoundaryData))
	Width	     (cdr (assoc 40 BoundaryData))
	HalfWidth    (/ Width 2.00)
	StartPt	     (cdr (assoc 10 BoundaryData))
	PathLength   (cdr (assoc 41 BoundaryData))
	angp90	     (+ PathAngle (Degrees->Radians 90))
	angm90	     (- PathAngle (Degrees->Radians 90))
	p1	     (polar StartPt angm90 HalfWidth)
	p2	     (polar p1 PathAngle PathLength)
	p3	     (polar p2 angp90 Width)
	p4	     (polar p3 (+ PathAngle (Degrees->Radians 180)) PathLength)
	poly2Dpoints (apply 'append
			    (mapcar '3dPoint->2dPoint (list p1 p2 p3 p4))
			    )
	poly3Dpoints (mapcar 'float (append p1 p2 p3 p4))
	;; get the polyline style
	plineStyle   (strcase (cdr (assoc 4 BoundaryData)))
	;; Add polyline to the model space using ActiveX automation
	pline	     (if (= plineStyle "LIGHT")

		       ;; create a lightweight polyline
		       (vla-addLightweightPolyline
			 *ModelSpace*	; Global Definition for Model Space
			 (gp:list->variantArray poly2Dpoints)
					;data conversion
			 ) ;_ end of vla-addLightweightPolyline



		       ;; or create a regular polyline
		       (vla-addPolyline
			 *ModelSpace*
			 (gp:list->variantArray poly3Dpoints)
					;data conversion
			 ) ;_ end of vla-addPolyline
		       ) ;_ end of if



	) ;_ end of setq
  (vla-put-closed pline T)
  ;; Return the ActiveX object name for the outline polyline
  ;; The return value should look something like this:
  ;; #<VLA-OBJECT IAcadLWPolyline 02351a34> 
  pline
  ) ;_ end of defun


;;;--------------------------------------------------------------------;
;;;     Function: gp:calculate-Draw-TileRow                            ;
;;;--------------------------------------------------------------------;
;;;  Description: This function is responsible for drawing individual  ;
;;;               rows of tiles.  It is called from the main tile      ;
;;;               drawing function gp:Calculate-and-Draw-Tiles.        ;
;;;--------------------------------------------------------------------;
;;;  NOTE: No validity checking is performed on BoundaryData!!!        ;
;;;--------------------------------------------------------------------;
(defun gp:calculate-Draw-TileRow (startPoint	    TileRadius
				  TileSpace	    pathWidth
				  pathAngle	    offsetFromCenter
				  ObjectCreationStyle
				  /		    HalfWidth
				  TileDiameter	    TileSpacing
				  ObjectCreationFunction
				  angp90	    angm90
				  firstCenterPt	    TileCenterPt
				  TileList
				  )

  (setq	HalfWidth (- (/ pathWidth 2.00) TileRadius)
	Tilespacing
	 (+ (* TileRadius 2.0) TileSpace)
	TileDiameter
	 (* TileRadius 2.0)
	angp90 (+ PathAngle (Degrees->Radians 90))
	angm90 (- PathAngle (Degrees->Radians 90))
	firstCenterPt
	 (polar startPoint angp90 offsetFromCenter)
	tileCenterPt
	 firstCenterPt
	ObjectCreationStyle
	 (strcase ObjectCreationStyle)
	ObjectCreationFunction
	 (cond
	   ((equal ObjectCreationStyle "ACTIVEX")
	    gp:Create_activeX_Circle
	    )
	   ((equal ObjectCreationStyle "ENTMAKE")
	    gp:Create_entmake_Circle
	    )
	   ((equal ObjectCreationStyle "COMMAND")
	    gp:Create_command_Circle
	    )
	   (T
	    (alert
	      (strcat
		"ObjectCreationStyle in function gp:calculate-Draw-TileRow"
		"\nis invalid.  Please contact the developer for assistance."
		"\n        ObjectCreationStyle set to ACTIVEX"
		)
	      )
	    (setq objectCreationStyle "ACTIVEX")
	    )
	   )

	)
  ;; Draw the circles to the left of the center
  (while (< (distance startPoint tileCenterPt) HalfWidth)
    ;; Add each tile to the list to return
    (setq tileList
	   (cons
	     (ObjectCreationFunction tileCenterPt TileRadius)
	     tileList
	     )
	  )


    ;; Calculate the center point for the next tile
    (setq tileCenterPt
	   (polar tileCenterPt
		  angp90
		  TileSpacing
		  )
	  )
    ) ;_ end of while

  ;; Draw the circles to the right of the center
  (setq	tileCenterPt
	 (polar	firstCenterPt
		angm90
		TileSpacing
		)
	)
  (while (< (distance startPoint tileCenterPt) HalfWidth)
    ;; Add each tile to the list to return
    (setq tileList
	   (cons
	     (ObjectCreationFunction tileCenterPt TileRadius)
	     tileList
	     )
	  )

    ;; Calculate the center point for the next tile
    (setq tileCenterPt
	   (polar tileCenterPt
		  angm90
		  TileSpacing
		  )
	  )
    ) ;_ end of while

  ;; Return the list of tiles
  tileList
  ) ;_ end of defun

;;;--------------------------------------------------------------------;
;;;     Function: gp:Calculate-and-Draw-Tiles                          ;
;;;--------------------------------------------------------------------;
;;;  Description: This is the main tile drawing function.  It is       ;
;;;               called from C:GPATH.  The function sets up a loop,   ;
;;;               based on the path length and calls the               ;
;;;               gp:calculate-Draw-TileRow function as many times as  ;
;;;               necessary to "fill up" the path boundary.            ;
;;;--------------------------------------------------------------------;
;;;  NOTE: No validity checking is performed on BoundaryData!!!        ;
;;;--------------------------------------------------------------------;
(defun gp:Calculate-and-Draw-Tiles (BoundaryData     /
				    PathLength	     TileSpace
				    TileRadius	     SpaceFilled
				    SpaceToFill	     RowSpacing
				    offsetFromCenter rowStartPoint
				    pathWidth	     pathAngle
				    ObjectCreationStyle
				    TileList
				    )
  (setq
    PathLength		(cdr (assoc 41 BoundaryData))
    TileSpace		(cdr (assoc 43 BoundaryData))
    TileRadius		(cdr (assoc 42 BoundaryData))
    SpaceToFill		(- PathLength TileRadius)
    RowSpacing		(* (+ TileSpace (* TileRadius 2.0))
			   (sin (Degrees->Radians 60))
			   ) ;_ end of *
    SpaceFilled		RowSpacing
    offsetFromCenter	0.0
    offsetDistance	(/ (+ (* TileRadius 2.0) TileSpace) 2.0)
    rowStartPoint	(cdr (assoc 10 BoundaryData))
    pathWidth		(cdr (assoc 40 BoundaryData))
    pathAngle		(cdr (assoc 50 BoundaryData))
    ObjectCreationStyle	(strcase (cdr (assoc 3 BoundaryData)))
    ) ;_ end of setq

  ;; Compensate for the very first Start Point!
  (setq	rowStartPoint
	 (polar	rowStartPoint
		(+ pathAngle pi)
		(/ TileRadius 2.0)
		) ;_ end of polar
	) ;_ end of setq
  ;; Draw each row of tiles
  (while (<= SpaceFilled SpaceToFill)
    ;; Get the list of tiles created, adding them to our list
    (setq tileList	   (append tileList
				   (gp:calculate-Draw-TileRow
				     (setq rowStartPoint
					    (polar rowStartPoint
						   pathAngle
						   RowSpacing
						   ) ;_ end of polar
					   ) ;_ end of setq
				     TileRadius
				     TileSpace
				     pathWidth
				     pathAngle
				     offsetFromCenter
				     ObjectCreationStyle
				     ) ;_ end of gp:calculate-Draw-TileRow
				   ) ;_ end of append
	  ;; Calculate the distance along the path for the next row
	  SpaceFilled	   (+ SpaceFilled RowSpacing)
	  ;; Alternate between a zero and a positive offset
	  ;; (causes alternate rows to be indented)
	  offsetFromCenter
			   (if (= offsetFromCenter 0.0)
			     offsetDistance
			     0.0
			     ) ;_ end of if
	  ) ;_ end of setq
    ) ;_ end of while
  ;; Return the list of tiles created
  tileList
  ) ;_ end of defun

;;;--------------------------------------------------------------------;
;;;  The following three functions correspond to the three different   ;
;;;  methods by which the tiles (circles) are drawn in the path.       ;
;;;  NOTE:  Each of these tile creation functions must return the      ;
;;;         ActiveX name of the circle created                         ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;     Function: gp:Create_activeX_Circle                             ;
;;;--------------------------------------------------------------------;
;;;  Description: Use ActiveX Automation to create a circular tile     ;
;;;--------------------------------------------------------------------;
(defun gp:Create_activeX_Circle	(center radius)
  (vla-addCircle *ModelSpace*
    (vlax-3d-point center) ; creates ActiveX-compatible 3D point structure 
    radius)
  )
;;;--------------------------------------------------------------------;
;;;     Function: gp:Create_entmake_Circle                             ;
;;;--------------------------------------------------------------------;
;;;  Description: Use (entmake) to create a circular tile              ;
;;;--------------------------------------------------------------------;
(defun gp:Create_entmake_Circle	(center radius)
  (entmake
    (list (cons 0 "CIRCLE") (cons 10 center) (cons 40 radius))
    ) ;_ end of entmake
  (vlax-ename->vla-object (entlast))
  ) ;_ end of defun

;;;--------------------------------------------------------------------;
;;;     Function: gp:Create_command_Circle                             ;
;;;--------------------------------------------------------------------;
;;;  Description: Use (command) to create a circular tile              ;
;;;--------------------------------------------------------------------;
(defun gp:Create_command_Circle	(center radius)
  (command "_CIRCLE" center radius)
  (vlax-ename->vla-object (entlast))
  ) ;_ end of defun
;|«Visual LISP© Format Options»
(72 2 40 1 nil "end of " 60 9 0 0 0 T T nil T)
;*** DO NOT add text below the comment! ***|;

;;;-----BEGIN-SIGNATURE-----
;;; UAoAADCCCkwGCSqGSIb3DQEHAqCCCj0wggo5AgEBMQ8wDQYJKoZIhvcNAQELBQAw
;;; CwYJKoZIhvcNAQcBoIIHaDCCB2QwggVMoAMCAQICEA2+4xGUyzuWyBkNEt7WBCEw
;;; DQYJKoZIhvcNAQELBQAwaTELMAkGA1UEBhMCVVMxFzAVBgNVBAoTDkRpZ2lDZXJ0
;;; LCBJbmMuMUEwPwYDVQQDEzhEaWdpQ2VydCBUcnVzdGVkIEc0IENvZGUgU2lnbmlu
;;; ZyBSU0E0MDk2IFNIQTM4NCAyMDIxIENBMTAeFw0yMTA4MDIwMDAwMDBaFw0yMjA4
;;; MDIyMzU5NTlaMGkxCzAJBgNVBAYTAlVTMRMwEQYDVQQIEwpDYWxpZm9ybmlhMRMw
;;; EQYDVQQHEwpTYW4gUmFmYWVsMRcwFQYDVQQKEw5BdXRvZGVzaywgSW5jLjEXMBUG
;;; A1UEAxMOQXV0b2Rlc2ssIEluYy4wggIiMA0GCSqGSIb3DQEBAQUAA4ICDwAwggIK
;;; AoICAQDY65QEUknk2Yy87p4LaXMXOT7AasB4BhNRJKfabtvF7wt0+TnhDOnKHKB1
;;; NvRywfn6n9qqIXN2pGjRuzWhJmjzb0C3+jA+c2Zlcp3VvisPdlizGFNzrL37XYoE
;;; yv7vg9fTKpDqiQS513cmJ8Kj38XWO55bEhAsiH6xgE9HiiD/XEUW8FUGAamdUIDD
;;; cq+NhdYsI5wgFyQM/CioZ8wttF0qJqSE4hbTaw8j2UFkDEkiFex4mCB99g7Dbzw+
;;; erXCEQCJuFYCQQN8OB8pxvTT/m8yYLYPwg9DzqVjn5SlhjLGdiPyOocuteb4QiM/
;;; JHZpRk8MQUs+wopTGDpYDhR8jfnbldfwvtwHfSPtKvq5QzErTVv9okB34Z0SaM86
;;; 518EZwUkrNfymt45CNmfa80uqC2xS+N7g4sg87EDbRCxvAnhJ6btFYRHhKfW6oAT
;;; YZFSU/4W3NFmX27Pnx8ZjATVPzoZ47rNm0JT2W4omIgfdq07Iu3SQp/e5a1QJpBY
;;; yaoT1ueqtxTdhBRHwjC5rTjVxuIQ24r9KU4ysH8R7d8skbBWGhyw2/9MB8rc6S7b
;;; g224JNJjgn0bM3cXOZRyBj8MkwWRU0XUV2wf6L1DDD9E8kBaagDv5J04VfScvtYK
;;; I+blbu8sT8is2fCnHptcPv0G4DFWOmkwFgOM6+OQoS5KsORz3wIDAQABo4ICBjCC
;;; AgIwHwYDVR0jBBgwFoAUaDfg67Y7+F8Rhvv+YXsIiGX0TkIwHQYDVR0OBBYEFDo+
;;; 4eCS86cJ84X74B6P0/A+TOO9MA4GA1UdDwEB/wQEAwIHgDATBgNVHSUEDDAKBggr
;;; BgEFBQcDAzCBtQYDVR0fBIGtMIGqMFOgUaBPhk1odHRwOi8vY3JsMy5kaWdpY2Vy
;;; dC5jb20vRGlnaUNlcnRUcnVzdGVkRzRDb2RlU2lnbmluZ1JTQTQwOTZTSEEzODQy
;;; MDIxQ0ExLmNybDBToFGgT4ZNaHR0cDovL2NybDQuZGlnaWNlcnQuY29tL0RpZ2lD
;;; ZXJ0VHJ1c3RlZEc0Q29kZVNpZ25pbmdSU0E0MDk2U0hBMzg0MjAyMUNBMS5jcmww
;;; PgYDVR0gBDcwNTAzBgZngQwBBAEwKTAnBggrBgEFBQcCARYbaHR0cDovL3d3dy5k
;;; aWdpY2VydC5jb20vQ1BTMIGUBggrBgEFBQcBAQSBhzCBhDAkBggrBgEFBQcwAYYY
;;; aHR0cDovL29jc3AuZGlnaWNlcnQuY29tMFwGCCsGAQUFBzAChlBodHRwOi8vY2Fj
;;; ZXJ0cy5kaWdpY2VydC5jb20vRGlnaUNlcnRUcnVzdGVkRzRDb2RlU2lnbmluZ1JT
;;; QTQwOTZTSEEzODQyMDIxQ0ExLmNydDAMBgNVHRMBAf8EAjAAMA0GCSqGSIb3DQEB
;;; CwUAA4ICAQBgAlhYjdQROM/ERZDYg0G6w3OPW7R5964dUS85Ijbkghpqi1KZB88n
;;; 0ItnNMz8nm0juZI7Jk1Jz+3fs4bcpfJrt4NQFaD9K1SAszXwe1zfBs0KTMqNkr7u
;;; Ji2ySyK6eFkO+ZRbKLIufwXPmY8uopzwjqn2BSoX/Q4ZOhw1H7tBxcudzOivMoL1
;;; ouUFPwAq3rN9mUl4G6nXrDvd31z24Q+dWtAA16cJbu2OgX2Tv7m7NPZIQ002iQCa
;;; ke59VqhiiUveM5UJ7Rr+Kdp732ZnGuKcGcbNl3B4KUjE1z6+wWaVJlygJX4EHZDn
;;; W+vtPcGRR3IHDWconSphlRZC7P1HhnAnfJqu7v5zyDv9+KyNL0hNNdWf0epK22HS
;;; BDC68W1DhC0ocWCFRHttRDqvvRyUhaAQBhIu7MoUzpi6hgg1S3sqM3u1D4f/Zn2C
;;; ocvEH9FOV0bq3ZOnCZjpH2HURTINElaDgM+hSfGN2zpbJSf1UKZXjkujYul75tk8
;;; 6ogI3b44wb4QdZskaIKxhw4/VZPbt31BHY2HbYCjFmvtpObX9qRwhG57EwK+o5mh
;;; KwkWifU7a8/5P1zyIwJfKdutGdB20wX9HRYPF+Bb87nKGJV/bM1tqzyRAIMGBWp/
;;; LLIee8R+FRMe7RT+v8/hRYsjPU2EVqSqweN2Fbz/rDeKSCr6Nl7XPjGCAqgwggKk
;;; AgEBMH0waTELMAkGA1UEBhMCVVMxFzAVBgNVBAoTDkRpZ2lDZXJ0LCBJbmMuMUEw
;;; PwYDVQQDEzhEaWdpQ2VydCBUcnVzdGVkIEc0IENvZGUgU2lnbmluZyBSU0E0MDk2
;;; IFNIQTM4NCAyMDIxIENBMQIQDb7jEZTLO5bIGQ0S3tYEITANBgkqhkiG9w0BAQsF
;;; ADANBgkqhkiG9w0BAQEFAASCAgA6i3uV3pmb6sHmM2jO1G9HduBcHJTJpUHpBCQ3
;;; Eu4tssMXVc3LMAh1iw3HKq47pl2LLpx9tenfabSdX9siPFhN8pVf8db+JNu2h+vA
;;; uhRaDXSaqTo6EpFpPorFcGfE2sBXDiuab5jSZpoYT3DwagaXEm/Dm3P+QhZ2btDD
;;; 8J82UyuZOGSmDVaABqsUSe6tl8q2mGKhfUYrit5/mAY1og5mkIe7WpxBP17x2H1g
;;; W0bDJsS6mKV/Qpf3Wd3JIr+XWqVTao7z32K9XS4b3d1wYqZ7CNothYOp7S/c9rPd
;;; fxN1Q5lJgcWyf6Vb001k3ld+Tn6jS3RFxCib/7zVDQiQNdcm+IqeYgH28n1GMU5z
;;; owhp7lDwkODcTa8R71p/maC72YMUZjuQa1mi+c5lf4UXaOwBmI+HK/JMdOU8N6jT
;;; Tsmhm7zw3CTDDjZVnDRwuD7OLWbTdTXZnQ2DoJwWxHA+ts8NKQcsSdWTWo8cIfIO
;;; kreq/bqRkEshOkDloiDd9Hg/EOeX1T2wr/MIseBCF3ODvOjh3aj0ke4Yty2BF/Tc
;;; 5fGhpBi8+XIBHPLZ6mA3upVQ1nGkowOkIErY8oJevCXroBGVFJ8CD8WWNdZn9k0T
;;; l1xdhVM25bESYiufpygym8yfaMO8S/LuGWYemd6CM1fl9elM6NdI/zcvPWDj3Ge2
;;; h4OcIQ==
;;; -----END-SIGNATURE-----