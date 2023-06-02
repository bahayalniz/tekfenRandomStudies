;;;--------------------------------------------------------------------;
;;;                                                                    ;
;;;  GPMAIN.LSP                                                        ;
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
;;;  final state of the application at the end of Lesson 7. Use this   ;
;;;  file to check your work.                                          ;
;;;--------------------------------------------------------------------;


;;;********************************************************************;
;;;     Function: C:GPath        The Main Garden Path Function         ;
;;;--------------------------------------------------------------------;
;;;  Description: This is the main garden path function.  It is a C:   ;
;;;               function, meaning that it is turned into an AutoCAD  ;
;;;               command called GPATH.  This function determines the  ;
;;;               overall flow of the Garden Path program              ;
;;;********************************************************************;
;;;  The gp_PathData variable is an association list of the form:      ;
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
;;; In lesson 6, the following field was added to the list             ;
;;;    (100 . tileList  ) --  the tiles in the path                    ;
;;;--------------------------------------------------------------------;
;;; In lesson 7, the following fields were added to the list           ;
;;; StartingPoint                                                      ;
;;;	(12 . BottomStartingPoint)      15------------------------14   ;
;;;	(15 . TopStartingPoint)         |                          |   ;
;;; EndingPoint                         10    ----pathAngle--->   11   ;
;;;	(13 .  BottomEndingPoint)       |                          |   ;
;;;	(14 . TopEndingPoint)           12------------------------13   ;
;;;                                                                    ;
;;;********************************************************************;
(defun C:GPath ( / gp_PathData gp_dialogResults 
		PolylineName gp_PathData tileList PolylineList
		)
  (setvar "OSMODE" 0)
  ;; Ask the user for input: first for path location and
  ;; direction, then for path parameters.  Continue only if you have
  ;; valid input.  Store the data in gp_PathData
  (if (setq gp_PathData (gp:getPointInput))
    (if	(setq gp_dialogResults
	       (gp:getDialogInput
		 (cdr (assoc 40 gp_PathData))
	       ) ;_ end of gp:getDialogInput
	) ;_ end of setq
      (progn

;;;	(princ "\nReceived gp_dialogResults: ") (princ gp_dialogResults)

	;; Now take the results of gp:getPointInput and append this to
	;; the added information supplied by gp:getDialogInput
	(setq gp_PathData (append gp_PathData gp_DialogResults))

	;; At this point, you have all the input from the user.
	;; In lesson 7, the gp:drawOutline function was modified to
	;; return a list of the pointer to the polyline as well as
	;; the list of boundary points (the 12, 13, 14, 15 lists)
	;; Draw the outline, storing the resulting polyline "pointer"
	;; in the variable called PolylineName, and the boundary 
	;; points into gp_pathData
	; (trace gp:drawOutline)
	(setq PolylineList     (gp:drawOutline gp_PathData)
	      PolylineName     (car PolylineList)
	      gp_pathData      (append gp_pathData (cadr PolylineList))
	) ;_ end of setq


	;; Next, it is time to draw the tiles within the boundary.
	;; The tileList contains a list of the object pointers for
	;; the tiles.  By counting up the number of points (using the
	;; length function), we can print out the results of how many
	;; tiles were drawn.
	(princ "\nThe path required ")
	(princ
	  (length
	    (setq tileList
		   (gp:Calculate-and-Draw-Tiles
		     ;; path data list
		     gp_PathData
		     ;; object creation style to use - should be nil
		     ;; when drawing initial path.  Subsequent calls
		     ;; from reactor will provide the function to use.
		     nil
		   ) ;_ end of gp:Calculate-and-Draw-Tiles
	    ) ;_ end of setq
	  ) ;_ end of length
	) ;_ end of princ
	(princ " tiles.")

	;; Add the list of pointers to the tiles (returned by
	;; gp:Calculate-and-Draw-Tiles) to the gp_PathData variable.
	;; This is stored in the reactor data for the reactor attached
	;; to the boundary polyline.  With this data, the polyline
	;; "knows" what tiles (circles) belong to it.
	(setq gp_PathData   (append (list (cons 100 tileList))
					; all the tiles
				    gp_PathData
			    ) ;_ end of append
	) ;_ end of setq

	;; Before we attach reactor data to an object let's look at
	;; the function vlr-object-reactor.
	;; vlr-object-reactor has the following arguments:
	;;	(vlr-object-reactor owners data callbacks)
	;;      The callbacks Argument is a list comprising of
	;; 		owner , Reactor_Object list
	;; 		For further explanation see Help system
	;; For this exercise we will use all arguments
	;; associated with vlr-object-reactor

	;; These reactor functions will excecute only if
	;; the polyline in  PolylineName is modified or erased
 
	(vlr-object-reactor

	  ;; The first argument for vlr-object-reactor is
	  ;; the "Owners List" argument.  This is where to
	  ;; place the object to be associated with the
	  ;; reactor.  In this case it is the vlaObject
	  ;; stored in PolylineName

	  (list PolylineName)

	  ;; The second argument contains the data for the path

	  gp_PathData

	  ;; The third argument is the list of specific reactor
	  ;; types that we are interested in dealing with

	  '(
	    ;; reactor that is called upon modification of the object
	    (:vlr-modified . gp:outline-changed)
	    ;; reactor that is called upon erasure of the object
	    (:vlr-erased . gp:outline-erased)
	   )
	) ;_ end of vlr-object-reactor



	;; Next, register a command reactor to adjust the polyline
	;; when the changing command is finished.
	(if (not *commandReactor*)
	  (setq	*commandReactor*
		 (VLR-Command-Reactor

		   nil				; No data is associated with the editor reactor
		   ;; call backs
		   '
		    (
		     (:vlr-commandWillStart . gp:command-will-start)
		     (:vlr-commandEnded . gp:command-ended)
		     )
		   ) ;_ end of vlr-editor-reactor
		)
	  )

	(if (not *DrawingReactor*)
	  (setq *DrawingReactor*
		 (VLR-DWG-Reactor

		   nil				; No data is associated with the editor reactor
		   ;; call backs
		   '
		    (
		     ;; This is extremely important!!!!!!!!!
		     ;; Without this notification, AutoCAD will 
		     ;; crash upon exiting.
		     (:vlr-beginClose . gp:clean-all-reactors)
		     )
		   ) ;_ end of vlr-editor-reactor
		)
	  )



  
	
      ) ;_ end of progn
      (princ "\nFunction cancelled.")
    ) ;_ end of if
    (princ "\nIncomplete information to draw a boundary.")
  ) ;_ end of if
  (princ)				; exit quietly
) ;_ end of defun

;;; Display a message to let the user know the command name
(princ "\nType GPATH to draw a garden path.")
(princ)
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgAdhSY/1oTBhEKhEbvZ5kq4h2KfGLxkBjg4qvgr
;;; tbLwTDy1VjodG3/acbNgh9K5qMWtwudqUovuIFs6+SyzuQPjQJ15IwmNo3L8RVT5
;;; jAWju3HvtuiND+AVTrPlKT6cr7nbX9vEuV/Z7fBUt1ByoxGfQ9qFqxbHQsLB9TsE
;;; fYUdMIkan/isZIJl9ZeOKvjAlh/aQ0sNENVA/NS3XqW10z34ZJE8ppylhQS7xLdr
;;; 8ifZe8PD6N6APbwce4CxznF8SmE+su4QzD5shR8oyOKap0tukFYx8mcxOBkDG93M
;;; +g0fmJZvnwi1wOYSrqjyHHltSKfLDsSYMxMjxjAMDVpXYpLwGKBGepGkMniSy5H8
;;; 3u4gDma0Ui72KDq0bBhIyVQ27vcyPpM9BmCIYO0fsIkL+He9PtknlHhwvuJNeHks
;;; L5xD5vAnrUaQHLQuGZPqW9aC18o3z7r426dOc0G4kMWfp1rIYrUxOHfPcv6i/7oP
;;; mr+H0q3ONhmMCTyK6aCP1aeyeWrKnBHdJqv/XEdB+4oxn0fdT3N3m3fovJO019ON
;;; 0WvvV6PuOhN5YeocFdAODeXQ9oEYim7zOygKfeb9KvWu6o2u2JX9MGiHvr+xdk1s
;;; aLXJ8WOs506Fg1zdPcBP72WzktZj27y6nozCheZgAcOTXrK/EaPhF5I54+bG7QsF
;;; EZsQVw==
;;; -----END-SIGNATURE-----