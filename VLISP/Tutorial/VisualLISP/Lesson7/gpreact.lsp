;;;                                                                    ;
;;;  GPREACT.LSP                                                       ;
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
;;;  final state of the application at the end of Lesson 7.  Use this  ;
;;;  file to check your work.                                          ;
;;;--------------------------------------------------------------------;


;;;--------------------------------------------------------------------;
;;;  General Notes:                                                    ;
;;;--------------------------------------------------------------------;
;;;  After the execution of these reactor functions, you might         ;
;;;  experience difficulty in returning to Visual Lisp.  If this does  ;
;;;  happen, type VLide at the AutoCAD command prompt and focus will   ;
;;;  be returned to Visual Lisp.                                       ;
;;;--------------------------------------------------------------------;
;;;  There are three types of reactors which we will be using:         ;
;;;          1. an object reactor                                      ;
;;;          2. a command reactor                                      ;
;;;          3. a drawing reactor                                      ;
;;;  We will define two functions that will notify us when the user    ;
;;;  has modified or changed the garden path.                          ;
;;;--------------------------------------------------------------------;
;;; Object Reactor                                                     ;
;;;----------------------|---------------------|-----------------------;
;;; Event                |Function to call     | Description           ;
;;;----------------------|---------------------|-----------------------;
;;; :vlr-modified        |gp:outline-changed   | Function called       ;
;;;                      |                     | when object declared  ;
;;;                      |                     | in owners is modified ;
;;;----------------------|---------------------|-----------------------;
;;; :vlr-erased	         |gp:outline-erased    | Function called       ;
;;;                      |                     | when object is erased ;
;;;----------------------|---------------------|-----------------------;
;;;                                                                    ;
;;; Command Reactor                                                    ;
;;;----------------------|---------------------|-----------------------;
;;; Event                |Function to call     | Description           ;
;;;----------------------|---------------------|-----------------------;
;;; :vlr-commandWillStart|gp:command-will-start| Function called when  ;
;;;                      |                     | a command is typed    ;
;;;                      |                     | at the command prompt ;
;;;----------------------|---------------------|-----------------------;
;;; :vlr-commandEnded	 |gp:command-ended     | Function called when  ;
;;;                      |                     | a command has ended   ;
;;;----------------------|---------------------|-----------------------;
;;;                                                                    ;
;;; Drawing Reactor                                                    ;
;;;----------------------|---------------------|-----------------------;
;;; Event                |Function to call     | Description           ;
;;;----------------------|---------------------|-----------------------;
;;; :vlr-beginClose      |gp:clean-all-reactors| Function to clean all ;
;;;                      |                     | existing reactors     ;
;;;                      |                     | before ACAD exits     ;
;;;--------------------------------------------------------------------;
;;; Since reactor events occur in sequence (commandWillStart occuring  ;
;;; before the object modified reactor, for example), we need a few    ;
;;; global variables to keep track of what changes are occuring to the ;
;;; path.  The following globals are used:                             ;
;;;          *lostAssociativity*                                       ;
;;;          *polyToChange*                                            ;
;;;          *reactorsToRemove*                                        ;
;;;          *reactorToChange*                                         ;
;;;          *Safe-to-Delete*                                          ;
;;;--------------------------------------------------------------------;


;;;--------------------------------------------------------------------;
;;;     Function: Gp:Safe-Delete                                       ;
;;;--------------------------------------------------------------------;
;;;  Description: This function is used to clean-up (erase) any objects;
;;;               that have not been erased in a previous command/     ;
;;;               reactor sequence.  This function is called only from ;
;;;               the :vlr-commandEnded reactor event callback function;
;;;               gp:Command-ended.  Items (tiles) to delete are       ;
;;;               collected within the global variable *Safe-to-Delete*;
;;;--------------------------------------------------------------------;
;;;  The parameter activeCommand is the name of the command that has   ;
;;;  just ended.  Because of the command/reactor sequence within any of;
;;;  the GRIP_* commands, entity may only be done if the command is NOT;
;;;  a GRIP_* command.                                                 ;
;;;--------------------------------------------------------------------;
(defun Gp:Safe-Delete (activeCommand / owner trl)
  (if (not (equal
	     (strcase (substr activeCommand 1 5))
	     "GRIP_"
	     )
	   )
    (progn
      (if *Safe-to-Delete*
	(foreach Item *Safe-to-Delete*
	  (if (not (vlax-erased-p Item))
	    (vla-erase item)
	    )
	  )
	)
    (setq *Safe-to-Delete* nil)

    (setq trl (assoc :VLR-OBJECT-REACTOR (vlr-reactors)))

    (if trl (setq trl (cdr trl)))
      (while trl
        (progn
          (foreach owner *OwnerReactorsToRemove*
	        (if owner
              (vlr-owner-remove (car trl) owner)
	        )
          )
          (setq trl (cdr trl))
        )
      )

    )
  )
)



;;;--------------------------------------------------------------------;
;;;     Function: gp:erase-tiles                                       ;
;;;--------------------------------------------------------------------;
;;;  Description: A utility function for erasing all of the tiles in a ;
;;;               garden path.                                         ;
;;;--------------------------------------------------------------------;
;;;  The parameter "reactor" passed to this function is a pointer to a ;
;;;  reactor associated with a garden path.  This reactor should have  ;
;;;  reactor data stored within it which contains VLA-object ID's for  ;
;;;  each of the tiles (circles) within the path.  By retrieving this  ;
;;;  data, the tiles can be erased.                                    ;
;;;  After erasing the tiles, a nil value is put back into the reactor ;
;;;  data (otherwise, the reactor would have bad pointers to tiles that;
;;;  have just been erased.)                                           ;
;;;--------------------------------------------------------------------;
(defun gp:erase-tiles (reactor / reactorData tiles tile)
  (if (setq reactorData (vlr-data reactor))
    (progn
      ;; Tiles in the path are stored as data in the reactor
      (setq tiles (cdr (assoc 100 reactorData)))
      ;; Erase all the existing tiles in the path
      (foreach tile tiles
	(if (and (null (member tile *Safe-to-Delete*))
		 (not (vlax-erased-p tile))
		 )
	  (progn
	    (vla-put-visible tile 0)
	    (setq *Safe-to-Delete* (cons tile *Safe-to-Delete*))
	    )
	  )
	)
      (vlr-data-set reactor nil)
      )
    )
  )


;;;--------------------------------------------------------------------;
;;;     Function: gp:command-will-start                                ;
;;;--------------------------------------------------------------------;
;;;  Description: This is a reactor to any command starting            ;
;;;--------------------------------------------------------------------;
;;;  This is the function where we figure out what will be happening   ;
;;;  to the garden path.  The various reactor globals are set to       ;
;;;  flag the changes that we expect to be made, so that subsequent    ;
;;;  reactor events will perform the correct actions.                  ;
;;;--------------------------------------------------------------------;
(defun gp:command-will-start (reactor command-list)
  ;; Reset all four reactor globals to nil
  (setq	*lostAssociativity*
	 nil
	*polyToChange*
	 nil
	*reactorsToRemove*
	 nil
	*reactorsToChange*
	 nil
	)
  (if (member (setq currentCommandName (car command-list))
	      '("U"	      "UNDO"	    "STRETCH"	  "MOVE"
		"ROTATE"      "SCALE"	    "BREAK"	  "GRIP_MOVE"
		"GRIP_ROTATE" "GRIP_SCALE"  "GRIP_MIRROR"
		)
	      )
    (progn
      (setq *lostAssociativity* T)
      (princ "\nNOTE: The ")
      (princ currentCommandName)
      (princ " command will break a path's associativity.")
      )
    )
  (princ)
  )



;;;--------------------------------------------------------------------;
;;;     Function: gp:outline-erased                                    ;
;;;--------------------------------------------------------------------;
;;;  Description: This reactor function is triggered if the path       ;
;;;               outline is being erased.  In this case, store the    ;
;;;               reactor to the global list *reactorsToRemove*        ;
;;;--------------------------------------------------------------------;
(defun gp:outline-erased (outlinePoly reactor parameterList)
  (setq	*reactorsToRemove*
	 (cons reactor *reactorsToRemove*)
	*OwnerReactorsToRemove*
	 (cons outlinePoly *OwnerReactorsToRemove*)
	)
  )



;;;--------------------------------------------------------------------;
;;;     Function: gp:outline-changed                                   ;
;;;--------------------------------------------------------------------;
;;;  Description: This reactor function is fired if the path outline   ;
;;;               is changed, or if the path is being moved, rotated,  ;
;;;               or altered in some way (other than being erased).    ;
;;;               If the command is a grip stretch, the path will      ;
;;;               retain its associativity with the tiles, so the      ;
;;;               function must act differently in this situation,     ;
;;;               by saving a pointer to the polyline border, so that  ;
;;;               it can be updated during the commandEnded callback.  ;
;;;--------------------------------------------------------------------;
(defun gp:outline-changed (outlinePoly reactor parameterList)
  (if *lostAssociativity*
    (setq *reactorsToRemove*
	   (cons reactor *reactorsToRemove*)
	  )
    (setq *polytochange*     outlinePoly
	  *reactorsToChange* (cons reactor *reactorsToChange*)
	  )
    )
  )



;;;--------------------------------------------------------------------;
;;;     Function: gp:command-ended                                     ;
;;;--------------------------------------------------------------------;
;;;  Description: This reactor function is called at the end of any    ;
;;;               command.                                             ;
;;;--------------------------------------------------------------------;
;;;  This is where the majority of work is done.  Once the command     ;
;;;  that the user is performing has ended, we can get to work.  (We   ;
;;;  cannot modify entities while they are being modified by AutoCAD   ;
;;;  itself, so we have to wait until we get a notification that the   ;
;;;  command in progress is complete, and we can have access to the    ;
;;;  entities.)                                                        ;
;;;--------------------------------------------------------------------;
(defun gp:command-ended	(reactor	  command-list
			 /		  objReactor
			 reactorToChange  reactorData
			 coordinateValues currentPoints
			 newReactorData	  newPts
			 tileList
			 )

  (cond
    ;; CONDITION 1 - POLYLINE ERASED (Erase command)
    ;; If one or more polyline borders are being erased (indicated
    ;; by the presence of *reactorsToRemove*), erase the tiles within
    ;; the border, then remove the reactor.
    (*reactorsToRemove*
     (foreach objReactor *reactorsToRemove*
       (gp:erase-tiles objReactor)
       )
     (setq *reactorsToRemove* nil)
     )

    ;; CONDITION 2 - LOST ASSOCIATIVITY (Move, Rotate, etc.)
    ;; If the associatvity has been lost (undo, move, etc.) then erase
    ;; the tiles within each border
    ;; 
    ((and *lostassociativity* *reactorsToChange*)
     (foreach reactorToChange *reactorsToChange*
       (gp:erase-tiles reactorToChange)
       )
     (setq *reactorsToChange* nil)
     )

    ;; CONDITION 3 - GRIP_STRETCH 
    ;; In this case, we are keeping the associativity of the tiles to
    ;; the path, but the path and the tiles will need to be recalculated
    ;; and redrawn.  A GRIP_STRETCH can only be performed on a single
    ;; POLYLINE at a time.
    ((and (not *lostassociativity*)
	  *polytochange*
	  *reactorsToChange*
	  (member "GRIP_STRETCH" command-list)
	  ;; for a GRIP_STRETCH, there will be only one reactor in
	  ;; the global *reactorsToChange*
	  (setq	reactorData
		 (vlr-data (setq reactorToChange
				  (car *reactorsToChange*)
				 )
			   )
		)
	  )

     ;; First, erase the tiles within the polyline border
     (gp:erase-tiles reactorToChange)

     ;; Next, get the current coordinate values of the polyline
     ;; vertices
     (setq coordinateValues
	    (vlax-safearray->list
	      (vlax-variant-value
		(vla-get-coordinates *polyToChange*)
		)
	      )
	   )


     ;; If the outline is a lightweight polyline, you'll have 2d points,
     ;; so use the utility function xyList->ListOfPoints to convert the
     ;; list of coordinate data into lists of ((x y) (x y) ...) points.
     ;; Otherwise, use the xyzList->ListOfPoints function that deals
     ;; with 3d points, and converts the coordinate data into lists of
     ;; ((x y z) (x y z) ... ) points.
     (setq CurrentPoints
	    (if	(= (vla-get-ObjectName *polytochange*) "AcDbPolyline")
	      (xyList->ListOfPoints coordinateValues)
	      (xyzList->ListOfPoints coordinateValues)
	      )
	   )

     ;; Send this new information to RedefinePolyBorder -- this will
     ;; return the new Polyline Border
     (setq NewReactorData
	    (gp:RedefinePolyBorder CurrentPoints reactorData)
	   )

     ;; Get all the border Points and ...
     (setq newpts (list	(cdr (assoc 12 NewReactorData))
			(cdr (assoc 13 NewReactorData))
			(cdr (assoc 14 NewReactorData))
			(cdr (assoc 15 NewReactorData))
			)
	   )

     ;; ...update the outline of the polyline with the new points
     ;; calculated above.  If you're dealing with a lightweight polyline,
     ;; convert these points to 2d (since all of the points in newpts are
     ;; 3D) otherwise leave them alone.
     (if (= (cdr (assoc 4 NewReactorData)) "LIGHT")
       (setq newpts (mapcar '(lambda (point)
			       (3dPoint->2dPoint Point)
			       )
			    newpts
			    )
	     )
       )


     ;; Now update the polyline with the correct points
     (vla-put-coordinates
	  *polytochange*
	  ;; For description of the list->variantArray see utils.lsp
	  (gp:list->variantArray (apply 'append newpts))
	)

       
     ;; We now use the current definition of the NewReactorData which is
     ;; really the same as the Garden path data structure. The only
     ;; exception is that the field (100) containing the list of
     ;; tiles is nil.  This is ok since gp:Calculate-and-Draw-Tiles
     ;; does not require this field to draw the tiles. In fact this
     ;; function creates the tiles and returns a list of drawn tiles.
     (setq tileList (gp:Calculate-and-Draw-Tiles
		      ;; path data list without correct tile list
		      NewReactorData
		      ;; Object creation function
		      ;; Within a reactor this *MUST* be ActiveX
		      "ActiveX"
		      )
	   )


     ;; Now that we have received all the tiles drawn we'll rebuild
     ;; the data structure with the correct tileList value and reset
     ;; the data property in the reactor

     ;; Update the tiles associated with the polyline border
     (setq NewReactorData
	    (subst (cons 100 tileList)
		   (assoc 100 NewReactorData)
		   NewReactorData
		   )
	   )

     ;; By now we have the new data associated with the polyline.
     ;; All there is left to do is associate it with the reactor
     ;; using vlr-data-set
     (vlr-data-set (car *reactorsToChange*) NewReactorData)

     ;; remove all references to the temporary
     ;; variables *polytochange* and *reactorsToChange*
     (setq *polytochange*     nil
	   *reactorsToChange* nil
	   )
     )
    )
  ;; safely delete any items in the *Safe-to-Delete* global if you can!!!
  (Gp:Safe-Delete (car command-list))
  (setq *OwnerReactorsToRemove* nil)
  (princ)
  )



;;;--------------------------------------------------------------------;
;;;     Function: gp:clean-all-reactors                                ;
;;;--------------------------------------------------------------------;
;;;  Description: Used to clean all reactors before exiting AutoCAD.   ;
;;;               This is a Very Important Function!                   ;
;;;--------------------------------------------------------------------;
(defun gp:clean-all-reactors (reactor command-list)
  (terpri)
  (princ (list 'gp:clean-all-reactors reactor command-list))
  (terpri)
  (princ (setq reactorData (vlr-data reactor)))
  (terpri)
  (princ (list command-list " has been issued"))
  (cleanReactors)
  )



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
;;; ADANBgkqhkiG9w0BAQEFAASCAgA4ROAKDmW2yPaXoFbb0RWf0ClXTyEFCeNFiOoe
;;; M97aKRqYA0vlQnPDgU/rQ0GHddg5IlLGpNyNU+6W4AKO56QQjy3de1GBhd09U+lP
;;; 0pE42hIXuOIpGQXjC/uLJ4NRItBEvqDZEpfAMG9oSwxKKG0s6jzFGCZr6t9My46q
;;; nLuB9zmZFJXRzyw5hP9oFRiueLH5SW2RJ7QDX8YOzLZSReFP3A5ir8aY1aSlMX2x
;;; AHXBm8TcIUHaafwJsW/8lnRTDrvui9PK7aDKVzadHK2y+Od1fHGoSg3D+s2IZnXt
;;; r0YjffRLDwb2UEqVQqr6JEC/2Q6S5EOmG67amN7JCzrMHCQqgY2OBfIbvJws6YWx
;;; Ircf8ATjFFBG7iBiX7dI+uQvCOYNRGvrwOwWrY7CKUeAQp5qpcAnauYeDSLjqe+0
;;; ThmXqSMLGvKWgB2Hqi48jnzlnRA5Edpn7yRquFkzvxo/ttFZXe+2RPjL2Cdc5kRc
;;; Dj2OhWGX48VKOV49MEk0hv3GU9YWjp57WflJhOHBCyVLGQM2ruwzHDqKSCQika1x
;;; qk7emCUtfbu1sgbXwAQJgErw7dRIIL2ZQCfGc2dfr5xtb/cF+a+Oe6Yd1ID8tWp9
;;; lCXT9OEEWBZrmcuXkiuQWyP1JrSliou557S0XrY47nCxAX3CJNjKQbaB6d7UPuZ8
;;; 77pkQg==
;;; -----END-SIGNATURE-----