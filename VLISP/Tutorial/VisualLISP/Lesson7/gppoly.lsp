;;;                                                                    ;
;;;  GPPOLY.LSP                                                        ;
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
;;;     Function: gp:ZeroSmallNum                                      ;
;;;--------------------------------------------------------------------;
;;;  Description:  This function tests and "fixes" very small numbers  ;
;;;                This is required when comparing reals to take care  ;
;;;                of rounding problems.  The granularity of the       ;
;;;                comparison is very fine, which should guarantee     ;
;;;                that this function will work for any garden path    ;
;;;                situation.  However, if this is applied to other    ;
;;;                applications, be sure to check for tolerances and   ;
;;;                numeric accuracy.                                   ;
;;;--------------------------------------------------------------------;
(defun gp:ZeroSmallNum (num)
  (setq num (rtos num 2 24))
  (distof
    (if	(or (wcmatch num "*e-*")
	    (wcmatch num "*E-*")
	) ;_ end of or
      "0.0"
      num
    ) ;_ end of if
  ) ;_ end of distof
) ;_ end of defun


;;;--------------------------------------------------------------------;
;;;     Function: gp:Rtos2                                             ;
;;;--------------------------------------------------------------------;
;;;  Description:  This function converts any real or interger number  ;
;;;                into a string decimal.                              ;
;;;--------------------------------------------------------------------;
(defun gp:Rtos2 (AnyNumber) (rtos AnyNumber 2))


;;;--------------------------------------------------------------------;
;;;     Function: gp:PointEqual                                        ;
;;;--------------------------------------------------------------------;
;;;  Description:  This function tests if points p1 and p2 are equal,  ;
;;;                given the accuracy defined in gp:ZeroSmallNum.      ;
;;;--------------------------------------------------------------------;
(defun gp:PointEqual (p1 p2 /)
  (setq	p1 (mapcar 'gp:ZeroSmallNum p1)	; check for very small numbers in p1 and fix them
	p2 (mapcar 'gp:ZeroSmallNum p2)	; check for very small numbers in p2 and fix them
	p1 (mapcar 'gp:Rtos2 p1)
	p2 (mapcar 'gp:Rtos2 p2)
  ) ;_ end of setq
  ;; Does every element the list p1 and p2 equal T
  (Vl-Every '(lambda (x) (equal x 'T)) (mapcar 'equal p1 p2))
) ;_ end of defun


;;;--------------------------------------------------------------------;
;;;     Function: gp:recalcPolyCorners                                 ;
;;;--------------------------------------------------------------------;
;;;  Description:  This function sorts out the changes made to the     ;
;;;                polyline border when one of its corner points has   ;
;;;                been dragged to a new location.  What we know going ;
;;;                in to this function is the point value of the moved ;
;;;                point, which we can compare to the keyed List of    ;
;;;                that record how the polyline had been arranged      ;
;;;                prior to the move.                                  ;
;;;--------------------------------------------------------------------;
(defun gp:recalcPolyCorners (MovedPoint KeyedList / movedKey testSet1 testSet2
		    result1 result2 )
  ;; From the MovedPoint, i.e., (15 (3.45 2.32)), extract just the
  ;; key value
  (setq movedKey (car MovedKeyPoint))

  ;; Set up the points that need to be recalculated.  The first value
  ;; in each test set is a point adjacent to the moved point.  The
  ;; second value in each test set is the point opposite to the moved
  ;; point.
  (cond	((equal movedKey 12)
	 (setq testSet1 '(13 14)
	       testSet2 '(15 14)))
	((equal movedKey 13)
	 (setq testSet1 '(12 15)
	       testSet2 '(14 15)))
	((equal movedKey 14)
	 (setq testSet1 '(15 12)
	       testSet2 '(13 12)))
	((equal movedKey 15)
	 (setq testSet1 '(14 13)
	       testSet2 '(12 13)))
	)
  (setq result1 (getPerp-Distance-and-Angle
		  (cdr (assoc (nth 0 testSet1) KeyedList))
		  (cdr (assoc (nth 1 testSet1) KeyedList))
		  (cdr MovedPoint))
	result2 (getPerp-Distance-and-Angle
		  (cdr (assoc (nth 0 testSet2) keyedList))
		  (cdr (assoc (nth 1 testSet2) keyedList))
		  (cdr MovedPoint))
	
	;; replace the moved point
  	keyedList (subst MovedPoint
			 (assoc movedKey KeyedList) keyedList)
	;; replace the first "opposite the moved point" point
  	keyedList (subst (cons (nth 0 testSet1) (car Result1))
			 (assoc (nth 0 testSet1) keyedList) keyedList)
	;; replace the second "opposite the moved point" point
	;; this will be the return value of the entire function
  	keyedList (subst (cons (nth 0 testSet2) (car Result2))
			 (assoc (nth 0 testSet2) keyedList) keyedList))
) ;_ end of defun





;;;--------------------------------------------------------------------;
;;;     Function: gp:FindPointInList                                   ;
;;;--------------------------------------------------------------------;
;;;  Description: This function will search for a testpoint, ex.       ;
;;;               '(4.05253 6.28481), searching in a gpath boundary    ;
;;;               list which is in the form:                           ;
;;;                     ((12 4.05253 3.62658)                          ;
;;;                      (15 4.05253 6.28481)                          ;
;;;                      (13 12.6903 3.62658)                          ;
;;;                      (14 12.6903 6.28481)                          ;
;;;                     )                                              ;
;;;--------------------------------------------------------------------;
;;;  If the point is found this function returns the matching assoc    ;
;;;  value within a list, for example: ((15 4.05253 6.28481))          ;
;;;  If no match is made, the function returns nil                     ;
;;;--------------------------------------------------------------------;
;;;  Required Arguments:                                               ;
;;;        TestPoint is a 2D point (list of 2 reals                    ;
;;;        TestList contains the fields and the points to test against ;
;;;--------------------------------------------------------------------;
(defun gp:FindPointInList (TestPoint TestList / result)
  ;; Since every point in TestList needs to be evaluated, use mapcar
  ;; to apply a function to be evaluated to every point in the TestList. 
  ;; Mapcar has the advantage over foreach and while due to its ability
  ;; to apply a function to all items in a list without having to determine
  ;; a loop test condition, or to determine the number of iterations required.
  (setq	result (mapcar '(lambda	(pointToExamine)
			  (if (gp:PointEqual
				TestPoint
				(cdr pointToExamine)
			      ) ;_ end of gp:PointEqual
			    ;; this will return the point with the assoc key
			    ;; if the TestPoint is equal to the cdr of pointToExamine
			    pointToExamine
			  ) ;_ end of if
			) ;_ end of lambda
		       Testlist
	       ) ;_ end of mapcar
  ) ;_ end of setq
  ;; --------------------------------------------------------------------------
  ;;  What has happened above:
  ;;     We passed FindPoint two arguments.
  ;;     The first was the testPoint to look for, such as 
  ;;         was '(4.05253 6.28481).
  ;;     The second argument was a list of consed points such as
  ;;  		((12 4.05253 3.62658)
  ;;  		  (15 4.05253 6.28481)
  ;;  		  (13 12.6903 3.62658)
  ;;  		  (14 12.6903 6.28481)
  ;;  		)
  ;;  The purpose of this function was to determine if the point to be
  ;;  searched <testPoint> is present in the list Testlist. In this case
  ;;  the function FindPoint would return:
  ;;          (nil (15 4.05253 6.28481) nil nil)
  ;;  As you can see TestPoint matches the point in the field 15 above.

  ;;  At this moment we are not interested in the nils present in the list.
  ;;  So we remove the nils using Visual Lisp's vl-remove function.
  ;;   (vl-remove nil Result) 

  ;;  Now the return value would be:
  ;;  ( (15 4.05253 6.28481) )
  ;;  which is excactly what we are expecting

  ;; Note if the variable <Result> is full of nils the expression
  ;; (vl-remove nil Result) would return nil
  ;; Example:
  ;;  _$ (vl-remove nil '( nil nil nil nil ))
  ;;     nil
  ;;  _$ 
  ;; So the duty of this function is to return a found Point or nil

  ;; remove any nil value from the resulting list
  (vl-remove nil Result)
) ;_ end of defun



;;;--------------------------------------------------------------------;
;;;     Function: gp:FindMovedPoint                                    ;
;;;--------------------------------------------------------------------;
;;;  Description: This function's responsibility is to find the        ;
;;;               specific polyline border corner point that moved by  ;
;;;               comparing the presentPoints (the corner points after ;
;;;               the user has moved one of them) with the keyed point ;
;;;               list representing the previous configuration of the  ;
;;;               path border.                                         ;
;;;                                                                    ;
;;;   Arguments:  keyListToLookFor = list of keyindexes '( 12 13 14 15);
;;;               PresentPoints    = a list of points to examine       ;
;;;               KeyedList        = a list comprised of consed key    ;
;;;                                  values and points.                ;
;;;--------------------------------------------------------------------;

(defun gp:FindMovedPoint (keyListToLookFor PresentPoints KeyedList /
			  keyliststatus	missingkey movedpoint result
			  returnvalue)
  (setq	result (apply 'append
		      (mapcar '(lambda (PointToLookFor)
				 ;; since Findpoint only returns a value when found
				 ;; lets test the result. If its nil we'll place

				 (if (setq ReturnValue
					    (gp:FindPointInList PointToLookFor KeyedList)
				     ) ;_ end of setq
				   ReturnValue
				   ;; the function above returned
				   ;; something like ( (15 4.05253 6.28481) )
				   ;; the structure above is a list within a list

				   ;; It failed (returned nil)
				   ;; So we return a new value using the 
				   (list (cons nil PointToLookFor))
				 ) ;_ end of if
			       ) ;_ end of lambda
			      PresentPoints
		      ) ;_ end of mapcar
	       ) ;_ end of apply
  ) ;_ end of setq

  ;; The expression above returned something like this:
  ;; ((12 #.# #.#) (14 #.# #.#) (15 #.# #.#))
  
  ;; Now run a test to find out which assoc key is missing
  (setq	KeylistStatus
	 (mapcar '(lambda (IndexKey)
		    (if	(assoc IndexKey result)
		      (list T (assoc IndexKey result))
		      (list nil IndexKey)
		    ) ;_ end of if
		  ) ;_ end of lambda
		 keyListToLookFor
	 ) ;_ end of mapcar
  ) ;_ end of setq

  ;; The expression above returned something like this:
  ;; ((T (12 #.# #.#))  (nil 13)  (T (14 #.# #.#))   (T (15 #.# #.#)))

  ;; Now return just the key value for the missing point
  (setq	MissingKey
	 (cadr
	   (car
	     (vl-remove-if	'(lambda (argument)
			   (not (null (car Argument)))
			 ) ;_ end of lambda
			KeylistStatus
	     ) ;_ end of vl-remove-if

	   ) ;_ end of car
	 ) ;_ end of cadr
  ) ;_ end of setq

  ;; the expression above returned 13

  ;; Now, using the key, get the point that moved
  (setq	MovedPoint (cdar
	     ;; vl-remove-if-not in this case is testing for the car
	     ;; to be anything that is not nil. 
		     (vl-remove-if-not
		       '(lambda	(argument)
			  (null (car Argument))
			  ) ;_ end of lambda
		       Result
		       ) ;_ end of vl-remove-if-not
	 ) ;_ end of cdar
  ) ;_ end of setq

  ;; The expression above returned a point, something like this:
  ;;     (##.##  ##.##)

  ;; Just incase we end up with a 2D list, add the current elevation
  ;; to turn it into a 3D point
  (if (< (length MovedPoint) 3)
    ;; Princ we'll fix it here
    (setq MovedPoint
	   (list (car MovedPoint)
		 (cadr MovedPoint)
		 (getvar "elevation")
	   ) ;_ end of list
    ) ;_ end of setq
  ) ;_ end of if

  ;; This is the last evaluation that will be returned by this function.
  ;; This will now return the missing key with the moved point
  ;; something like: (13 ##.##  ##.##) 
  (cons	MissingKey
	MovedPoint
  ) ;_ end of cons
) ;_ end of defun



;;;--------------------------------------------------------------------;
;;;     Function: gp:RedefinePolyBorder                                ;
;;;--------------------------------------------------------------------;
;;;  Description:  This function is responsible for redefining the     ;
;;;                garden path polyline border, and returning the new  ;
;;;                reactor data which will be placed in the reactor    ;
;;;                data properties.                                    ;
;;;--------------------------------------------------------------------;
(defun gp:RedefinePolyBorder (PresentPoints Reactordata	/
			     KeyedList MovedKeyPoint NewKeyedList)
  ;; The keyedList contains information on the previous configuration
  ;; of the polyline.  This information is stored in the reactor data,
  ;; and was attached immediately after the polyline was created.
  (setq	KeyedList (list	(assoc 12 reactorData)
			(assoc 15 reactorData)
			(assoc 13 reactorData)
			(assoc 14 reactorData)
		  ) ;_ end of list
  ) ;_ end of setq
  
  ;; Since we know one of the points has moved we need to check
  ;; the points supplied against the saved points in reactorData
  ;; those points keys will be 12 13 14 15. We will test every point in
  ;; the supplied present point list and test it against every point
  ;; in the presentPoints list, which contains the 12 13 14 15 point
  ;; lists from the current (modified by the user) polyline configuration.
  ;; This way we can check for which point failed (i.e., did not match.)

  ;; The history of 12 13 14 15
  ;; StartingPoint
  ;;	(12 . BottomStartingPoint )
  ;;	(15 . TopStartingPoint )
  ;; EndingPoint 
  ;;	(13 .  BottomEndingPoint )
  ;;	(14 . TopEndingPoint )
  (setq	MovedKeyPoint
	 (gp:FindMovedPoint
	   '(12 13 14 15)
	   PresentPoints
	   KeyedList
	 ) ;_ end of gp:FindMovedPoint
  ) ;_ end of setq

  ;; We use the path angle to determine where and what to do
  (setq NewKeyedList (gp:recalcPolyCorners MovedKeyPoint KeyedList))

  ;; Now that we have the keyed list, all 12 13 14 15 keyindeces
  ;; are updated.  We need to update the reactor data  with the
  ;; new begining ending points and other values. 
  ;; We will now use foreach since we do not need the entire
  ;; reactordata structure, only its final result

  (foreach NewItem NewKeyedList
    (setq reactorData
	   (subst NewItem
		  (assoc (car newItem) reactorData)
		  reactorData
	   ) ;_ end of subst
    ) ;_ end of setq
  ) ;_ end of foreach

  ;; Now that we have changed the value of reactorData
  ;; with the returning values supplied by gp:recalcPolyCorners,
  ;; we can update other important information in reactorData.

  ;; Take care of placing new starting point, ending point, Width,
  ;; Path Length and Path Angle.
  (setq
    ;; Starting Point
    reactorData
		(subst (cons 10
			     (midpoint (cdr (assoc 15 reactorData))
				       (cdr (assoc 12 reactorData))
			     ) ;_ end of midpoint
		       ) ;_ end of cons
		       (assoc 10 reactorData)
		       reactorData
		) ;_ end of subst
    ;; Ending Point	
    reactorData	(subst (cons 11
			     (midpoint (cdr (assoc 14 reactorData))
				       (cdr (assoc 13 reactorData))
			     ) ;_ end of midpoint
		       ) ;_ end of cons
		       (assoc 11 reactorData)
		       reactorData
		) ;_ end of subst
    ;; Width 
    reactorData
		(subst (cons 40
			     (distance (cdr (assoc 14 reactorData))
				       (cdr (assoc 13 reactorData))
			     ) ;_ end of distance
		       ) ;_ end of cons
		       (assoc 40 reactorData)
		       reactorData
		) ;_ end of subst
    ;; pathLength
    reactorData
		(subst (cons 41
			     (distance (cdr (assoc 10 reactorData))
				       (cdr (assoc 11 reactorData))
			     ) ;_ end of distance
		       ) ;_ end of cons
		       (assoc 41 reactorData)
		       reactorData
		) ;_ end of subst
    ;; PathAngle
    reactorData
		(subst (cons 50
			     (angle (cdr (assoc 10 reactorData))
				    (cdr (assoc 11 reactorData))
			     ) ;_ end of angle
		       ) ;_ end of cons
		       (assoc 50 reactorData)
		       reactorData
		) ;_ end of subst
  ) ;_ end of setq
  reactorData				; return the updated reactor data
) ;_ end of defun



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
;;; ADANBgkqhkiG9w0BAQEFAASCAgAxAlQKSEcjFmJG72GnjaqFP07Cg8vm95QiwNMG
;;; xyW98meoKUddmkcLooovJvN2nu9UCkCmARKhGm/Oa4IDWIvJEq3a6mJ7xJyOcEQT
;;; xqfwPBVF2ikx7OC7ezkASHDBn53G+myk6iU2zkp+JgJgIqu0i8zTx2Xf7QtwFHyn
;;; SSU8tTqfWIxbhwDK2B/u0BJvfWgaC/3KkHctSlwaBYGRxWzcL59dUo3AIZLMzcwV
;;; 7pIvjEE0f6rW7JzXSwOgMGVvh9/nlRYHBiBKw4dUVOo1aGIjHvMAey8BmpNB5Coj
;;; yThfvvmQ/wEs04Dpvw9j30fqz0/cm278EVpXsjvxYOWSZ5/wJx31no40EkfSSd/K
;;; punBsuV7WnSrgvNK86HiSmaN1Sx0AIPs4qk2ejY6nMUJQtArDgCXUrY9dzHWeC9U
;;; jCekCEvYELKpEaSiBEzOAThFsdM0AtDcP38bVCQMDlo7/2ciYqP5iaw09KV2SNQQ
;;; vBC36RDs9tspzaOlhE7Yhe6BrGHKwui2Pq0yxmZZC+WUeAvkLAuBwv2Pn4C5ZyYW
;;; CSCFdr/jqUJhOQ+dJev9B6725IRYczzdwz4f7NyhD+qsCm5BLK4j1q5PCQicK92K
;;; oDGzr0BGV3laUJtnAgMuKw6DZdB6BnPVMKhOJC70skbh3qQMupTj52XvRMxHqJhZ
;;; ZFlAzg==
;;; -----END-SIGNATURE-----