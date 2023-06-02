;;;                                                                    ;
;;;  UTILS.LSP                                                         ;
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


;;;--------------------------------------------------------------------;
;;;  First step is to load ActiveX functionality.  If ActiveX support  ;
;;;  already exists in document (can occur when Bonus tools have been  ;
;;;  loaded into AutoCAD), nothing happens.                            ;
;;;--------------------------------------------------------------------;

(progn
    (vl-load-com)
    (vl-load-reactors)
)

;;;--------------------------------------------------------------------;
;;;  For ActiveX functions, we need to define a global variable which  ;
;;;  "points" to the Model Space portion of the active drawing.  This  ;
;;;  variable, named *ModelSpace* will be created at load time.        ;
;;;--------------------------------------------------------------------;
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       ) ;_ end of vla-get-ModelSpace
) ;_ end of setq



;;;--------------------------------------------------------------------;
;;;     Function: Degrees->Radians                                     ;
;;;--------------------------------------------------------------------;
;;;  Description: This function converts a number representing an      ;
;;;               angular measurement in degrees, into its radian      ;
;;;               equivalent.  There is no error checking done on the  ;
;;;               numberOfDegrees parameter -- it is always expected   ;
;;;               to be a valid number.                                ;
;;;--------------------------------------------------------------------;
(defun Degrees->Radians	(numberOfDegrees)
  (* pi (/ numberOfDegrees 180.0))
) ;_ end of defun


;;;--------------------------------------------------------------------;
;;;     Function: 3dPoint->2dPoint                                     ;
;;;--------------------------------------------------------------------;
;;;  Description: This function takes one parameter representing a     ;
;;;               3D point (a list of three integers or reals), and    ;
;;;               converts it into a 2D point (a list of two reals).   ;
;;;               There is no error checking done on the 3dpt          ;
;;;               parameter --  it is always expected to be a valid    ;
;;;               point.                                               ;
;;;--------------------------------------------------------------------;
;;;   Work to do: Add some kind of parameter checking so that this     ;
;;;               function won't crash a program if it is passed a     ;
;;;               null value, or some other kind of data type than a   ;
;;;               3D point.                                            ;
;;;--------------------------------------------------------------------;
(defun 3dPoint->2dPoint	(3dpt)
  (list (float(car 3dpt)) (float(cadr 3dpt)))
) ;_ end of defun

;;;--------------------------------------------------------------------;
;;;     Function: list->variantArray                                   ;
;;;--------------------------------------------------------------------;
;;;  Description: This function takes one parameter representing a     ;
;;;               list of double values, e.g. a list of 2D points:     ;
;;;               '(p1.X p1.Y p2.X p2.Y p3.X p3.Y p4.X p4.Y).          ;
;;;		  The list is converted into an ActiveX                ;
;;;		  variant based on a safearray.                        ;
;;;               No error checking is performed on the parameter --   ;
;;;               it is assumed to consist of a list of doubles.       ;
;;;------------------------------------------------------------------- ;
(defun gp:list->variantArray (ptsList / arraySpace sArray)
					; allocate space for an array of 2d points stored as doubles
  (setq	arraySpace
	 (vlax-make-safearray
	   vlax-vbdouble		; element type
	   (cons 0
		 (- (length ptsList) 1)
	   )				; array dimension
	 )

  )
  (setq sArray (vlax-safearray-fill arraySpace ptsList))
					; return array variant
  (vlax-make-variant sArray)
)


;;;--------------------------------------------------------------------;
;;;     Function: xyzList->ListOfPoints                                ;
;;;--------------------------------------------------------------------;
;;;  Description: This function extracts and formats 3D point lists    ;
;;;               from one big list of reals, in the form:             ;
;;;                   (x y z x y z x y z ...)                          ;
;;;               This is the format of the data returned by the       ;
;;;               vla-get-coordinates function when applied to a       ;
;;;               standard polyline object.                            ;
;;;--------------------------------------------------------------------;
;;;               The return value will be a list in the format:       ;
;;;                    ((x y z) (x y z) (x y z) ... )                  ;
;;;--------------------------------------------------------------------;
(defun xyzList->ListOfPoints (coordList / ptlist)
  (while coordList
    (setq ptlist (append ptlist
			 (list (list (car coordList) (cadr coordList) (caddr coordList)))
		 ) ;_ end of append
	  coordList	 (cdddr coordList)
    ) ;_ end of setq
  ) ;_ end of while
  ptlist
  ;;; (setq ptlist ptlist)
) ;_ end of defun


;;;--------------------------------------------------------------------;
;;;     Function: xyList->ListOfPoints                                 ;
;;;--------------------------------------------------------------------;
;;;  Description: This function extracts and formats 2D point lists    ;
;;;               from one big list of reals, in the form:             ;
;;;                   (x y x y x y ...)                                ;
;;;               This is the format of the data returned by the       ;
;;;               vla-get-coordinates function when applied to a       ;
;;;               lightweight polyline object.                         ;
;;;--------------------------------------------------------------------;
;;;               The return value will be a list in the format:       ;
;;;                    ((x y) (x y) (x y) ... )                        ;
;;;--------------------------------------------------------------------;
(defun xyList->ListOfPoints (coordList / ptlist)
  (while coordList
    (setq ptlist (append ptlist
			 (list (list (car coordList) (cadr coordList)))
		 ) ;_ end of append
	  coordList	 (cddr coordList)
    ) ;_ end of setq
  ) ;_ end of while
    ptlist
  ;;; (setq ptlist ptlist)
) ;_ end of defun


;;;--------------------------------------------------------------------;
;;;     Function: CleanReactors                                        ;
;;;--------------------------------------------------------------------;
;;;  Description: This is a general utility function used for cleaning ;
;;;               up reactors. It can be used during debugging, as     ;
;;;               well as cleaning up any open reactors before a       ;
;;;               drawing is closed.                                   ;
;;;--------------------------------------------------------------------;
(defun CleanReactors ()
  (setq	*commandReactor* nil		; clear the variable
	*DrawingReactor* nil		; clear the variable
	)

  (mapcar 'vlr-remove-all
	  '(:VLR-AcDb-reactor		 :VLR-Editor-reactor
	    :VLR-Linker-reactor		 :VLR-Object-reactor
	    :VLR-Command-Reactor	 :VLR-DeepClone-Reactor
	    :VLR-DocManager-Reactor	 :VLR-DWG-Reactor
	    :VLR-DXF-Reactor		 :VLR-Editor-reactor
	    :VLR-Insert-Reactor		 :VLR-Linker-Reactor
	    :VLR-Lisp-Reactor		 :VLR-Miscellaneous-Reactor
	    :VLR-Mouse-Reactor		 :VLR-Object-Reactor
	    :VLR-SysVar-Reactor		 :VLR-Toolbar-Reactor
	    :VLR-Undo-Reactor		 :VLR-Wblock-Reactor
	    :VLR-Window-Reactor		 :VLR-XREF-Reactor
	    )
	  ) ;_ end of mapcar
  ) ;_ end of defun


;;;--------------------------------------------------------------------;
;;;     Function: square                                               ;
;;;--------------------------------------------------------------------;
;;;  Description: This function returns the square of a number         ;
;;;               example:  (square 7) returns 49                      ;
;;;--------------------------------------------------------------------;
(defun square (aNumber)
  (* aNumber aNumber)
) ;_ end of defun


;;;--------------------------------------------------------------------;
;;;     Function: getPerp-Distance-and-Angle                           ;
;;;--------------------------------------------------------------------;
;;;  Description: This function returns a list with the distance and   ;
;;;               perpendicular angle to user pt3, and is determined   ;
;;;               by supplied points pt1 pt2.  Pt3 is "user input"     ;
;;;               and need not be at right angles.  This allows us to  ;
;;;               solve for cases where ortho mode is off.             ;
;;;  Example usage:                                                    ;
;;;        (setq Data  (getPerp-Distance-and-Angle pt1 pt2 pt3) )      ;
;;;--------------------------------------------------------------------;
;;;      Arguments:                                                    ;
;;;          pt1  seed point                                           ;
;;;          pt2  seed point                                           ;
;;;      Note:  pt1 and pt2 denote a "line" segment                    ;
;;;          pt3  "user point" (point to solve for)                    ;
;;;--------------------------------------------------------------------;
(defun getPerp-Distance-and-Angle (linept1 linept2 userpt3 / dist:pt1->pt2
				   dist:pt1->pt3 dist:pt2->pt3
				   dist:pt2->ToPerpPt)
  (setq	dist:pt1->pt2    (distance linept1 linept2)
	dist:pt1->pt3    (distance linept1 userpt3)
	dist:pt2->pt3    (distance linept2 userpt3)
	dist:pt2->ToPerpPt  (/ (- (+ (SQUARE dist:pt2->pt3)
				  (SQUARE dist:pt1->pt2))
			       (SQUARE dist:pt1->pt3))
			    (* 2.0 dist:pt1->pt2))  
  ) ;_ end of setq

  ;; return a list of the point perpendicular from userpt3
  ;; on line segment between linept1 and linept2, as well
  ;; as the angle & distance between userpt3 and perpPt
  (list
    (setq perpPt(polar linept2 (angle linept2 linept1) dist:pt2->ToPerpPt))
    (distance userpt3 perpPt)
    (angle userpt3 perpPt)
  ) ;_ end of list
) ;_end of defun


;;;--------------------------------------------------------------------;
;;;     Function: midPoint                                             ;
;;;--------------------------------------------------------------------;
;;;  Description: This function returns the point at the middle        ;
;;;               between two give points                              ;
;;;--------------------------------------------------------------------;
(defun midPoint	(pt1 pt2)
  (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2.00))
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgA8u7w4yEV7YJGI7vp96rXEQHJKfe8KM41INcYR
;;; 3WMCjaUJIQLlqZh4uhBsgH1WVjJyLorY6ag9r6A5nnzxZqwQgIyq0u1CSflwW+w2
;;; IflRT8ujb8Osg/B4gzRriq3DqLVnoT2j84FFs39dMQfo6pas0PC5tvuys0pXGghR
;;; koZy1+jWJEYH8ao9Z8sqqssdJGOOXpLEAUNODRODWZ3MFJ9MEaXzQREyp+I84HXl
;;; Sz27OiOB3bsSIZelbEV+gv6EKKTpgeqXfeNdPx2MbPntuQHab8LraG+D/1TNKaHa
;;; T2GjpwyQiK6D0NEHnyJZ35hD2JijSPPCt1vsfDv1d1pSDBXyZpruXpLBczfl37XD
;;; MZtX87bibuXlX4esSfSZMYlxfzrCvxz0KosC+ylcdTwF0FFvpadKdcjTW47J6uc0
;;; 8YrIGOYiX1SAdDM7cBneL8AfAkIPcb566/dV/BjKKJOi6TOQ2VQ84cnkiILCAAit
;;; bMHgwkBx59leQNNH4iiPk4QT6GoovK1VSc2BpQo2HZrUF9XbcDKNfoHsuAYnZoWf
;;; D1utn7M0pKqtcsiKi0tRH3AO6GcgUUu9DbXt0lezJYhIMM+Sjd+6LuDZdEW2hnmc
;;; ZWebxPrRfJve2tIjPNgUP8w/4N61Gw43djWKjAJp4Y17LAMtv/9QpKWuOLwG2m1E
;;; Zym1YQ==
;;; -----END-SIGNATURE-----