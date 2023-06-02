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
;;;  state of the application at the end of Lesson 6.  Use this file   ;
;;;  to check your work, or to start off Lesson 7 with the code as it  ;
;;;  appears in the tutorial.                                          ;
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
;;;          *polyToChange*                                            ;
;;;          *reactorsToRemove*                                        ;
;;;--------------------------------------------------------------------;


;;;--------------------------------------------------------------------;
;;;     Function: gp:command-will-start                                ;
;;;--------------------------------------------------------------------;
;;;  Description: This is a reactor to any command starting            ;
;;;--------------------------------------------------------------------;
;;;  This is the function where we figure out what *will* be happening ;
;;;  to the garden path (not what *is* happening).  Reset the global   ;
;;;  variables *polyToChange* and *reactorsToRemove* so that           ;
;;;  subsequent reactor events will perform the correct actions.       ;
;;;  (This is necessary since this function may be called more than    ;
;;;  once, and the *polyToChange* pointer could be pointing to a       ;
;;;  polyline other than the one the user just selected for editing!)  ;
;;;  Also, reset the *reactorsToRemove* global, for the same reason.   ;
;;;--------------------------------------------------------------------;
;;;  THIS FUNCTION IS CURRENTLY IN A STUBBED-OUT STATE!!!              ;
;;;--------------------------------------------------------------------;
(defun gp:command-will-start (reactor command-list)
  ;; Reset the global variable
  (setq	*polyToChange*  nil
	*reactorsToRemove* nil
  ) ;_ end of setq

  ;; Print to the console to see the results of the incoming data
  (terpri)
  (princ (list 'gp:command-will-start reactor command-list))
  (terpri)
  (princ (setq reactorData (vlr-data reactor)))
  (alert
    (strcat
      (format-reactor-message 'gp:command-will-start)
      "\n\tThis reactor-callback function's responsibility will be to:\n"
      "\n\tReset any of the global variables used within reactor functions"
      "\n\tto an initial nil state.  It will also note what AutoCAD command"
      "\n\thas been issued and respond accordingly.\n"
      "\n\tAssociated Actions:"
      "\n\tIf a U, UNDO, STRETCH, MOVE, ROTATE, or SCALE command is being"
      "\n\tstarted, break the associativity between the tiles and the "
      "\n\tpolyline boundary."
    ) ;_ end of strcat
  ) ;_ end of alert
  (princ)
) ;_ end of defun


;;;--------------------------------------------------------------------;
;;;     Function: gp:outline-erased                                    ;
;;;--------------------------------------------------------------------;
;;;  Description: This reactor function is triggered when the path     ;
;;;               outline is being erased.  If this happens, we need to;
;;;                    1) Erase all of the tiles (the user is taking   ;
;;;                       care of the rest of the work)                ;
;;;                    2) Set a global variable that stores the        ;
;;;                       reactor assigned to this polyline, so that   ;
;;;                       it can be removed when command-ended fires   ;
;;;--------------------------------------------------------------------;
;;;  THIS FUNCTION IS CURRENTLY IN A STUBBED-OUT STATE!!!              ;
;;;--------------------------------------------------------------------;
(defun gp:outline-erased (outlinePoly reactor parameterList)
  ;; Store the reactor assigned to this entity to the global
  ;; *reactorsToRemove* so that it can be removed later
  (setq	*reactorsToRemove*
	 (append *reactorsToRemove* (list reactor))
  ) ;_ end of setq

  ;; Print to the console to see the results of the incoming data
  (terpri)
  (princ
    (list 'gp:outline-erased outlinePoly reactor parameterList)
  ) ;_ end of princ
  (terpri)
  (princ (setq reactorData (vlr-data reactor)))
  (alert
    (strcat
      (format-reactor-message 'gp:outline-erased)
      "\nThis reactor-callback function's responsibility will be to:\n"
      "\n\tBuild upon a list that records pointers to any reactors for"
      "\n\tany polyline or polylines being erased by the user.  This is "
      "\n\tdone so the reactors can be removed once the erase command "
      "\n\thas ended."
    ) ;_ end of strcat
  ) ;_ end of alert
  (princ)
) ;_ end of defun


;;;--------------------------------------------------------------------;
;;;     Function: gp:outline-changed                                   ;
;;;--------------------------------------------------------------------;
;;;  Description: This reactor function is fired if the path outline   ;
;;;               is changed.  If this happens we need to:             ;
;;;                    1) Erase the tiles                              ;
;;;                    2) Remove the tile information from the reactor ;
;;;                       data (information stored to the reactor)     ;
;;;                    3) Save a pointer to the polyline for further   ;
;;;                       processing when the command-ended reactor    ;
;;;                       fires.                                       ;
;;;--------------------------------------------------------------------;
;;;  THIS FUNCTION IS CURRENTLY IN A STUBBED-OUT STATE!!!              ;
;;;--------------------------------------------------------------------;
(defun gp:outline-changed
       (outlinePoly reactor parameterList / tile tiles reactorData)
  ;; Set up the global variable that stores the pointer to the
  ;; polyline (as described in the description above)
  (setq *polyToChange* outlinePoly)

  ;; Print to the console to see the results of the incoming data
  (terpri)
  (princ
    (list 'gp:outline-changed outlinePoly reactor parameterList)
  ) ;_ end of princ
  (terpri)
  (princ (setq reactorData (vlr-data reactor)))

  (alert
    (strcat
      (format-reactor-message 'gp:outline-changed)
      "\n\tThis reactor-callback function's responsibility will be to:\n"
      "\n\tAct upon the notification that the outline has been modified."
      "\n\tAssociated Actions:"
      "\n\t\t1. Erase the tiles"
      "\n\t\t2. Remove any associativity to field 100"
      "\n\t\t   (the field that holds a list of tile objects)"
      "\n\t\t3. Save the Reactor and Polyline for further processing"
      "\n\t\t   once the command now in progress has ended."
    ) ;_ end of strcat
  ) ;_ end of alert

  (princ)
) ;_ end of defun




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
;;;  THIS FUNCTION IS CURRENTLY IN A STUBBED-OUT STATE!!!              ;
;;;--------------------------------------------------------------------;
(defun gp:command-ended	(reactor command-list)
  ;; Print to the console to see the results of the incoming data
  (terpri)
  (princ (list 'gp:command-ended reactor command-list))
  (terpri)
  (if *polyToChange*
    (progn
      (princ "\nPolyline being modified is ")
      (princ *polyToChange*)
    ) ;_ end of progn
  ) ;_ end of if
  (if *reactorsToRemove*
    (progn
      (princ "\nReactors that need to be removed: ")
      (princ *reactorsToRemove*)
    ) ;_ end of progn
  ) ;_ end of if
  (terpri)
  (princ (setq reactorData (vlr-data reactor)))
  (alert
    (strcat
      (format-reactor-message 'gp:command-ended)
      "\nThis reactor-callback function's responsibility will be to:\n"
      "\n\tNote what AutoCAD command has ended and respond accordingly."
      "\n\tAssociated Actions:"
      "\n\t\t1. If the polyline has been erased, remove associated reactors"
      "\n\t\t2. If the associatvity has been lost, then erase application"
      "\n\t\t   data from the reactor."
      "\n\t\t3. If the outline has not lost associativity and has been "
      "\n\t\t   stretched using Grips, then straighten it up."
    ) ;_ end of strcat
  ) ;_ end of alert
  (princ)
) ;_ end of defun

;;;--------------------------------------------------------------------;
;;;     Function: format-reactor-message                               ;
;;;--------------------------------------------------------------------;
;;;  Description: This is a temporary function used to format the      ;
;;;               messages that appear in the stubbed-out reactor      ;
;;;               callback function alerts.                            ;
;;;               It uses the vl-symbol-name function to convert the      ;
;;;               reactorFunction symbol into a string, and returns    ;
;;;               this as a formatted message presentable for the      ;
;;;               alert dialog box.                                    ;
;;;--------------------------------------------------------------------;
(defun format-reactor-message (reactorFunction)
  (strcat "****************************| Callback function: "
	  (vl-symbol-name reactorFunction)
	  " |***************************\n"
  ) ;_ end of strcat
) ;_ end of defun


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
;;; ADANBgkqhkiG9w0BAQEFAASCAgDPjXhiOGb1NnQdZo41vYa1oBfJ0WeUlZS+U7qq
;;; VyaOgFM1vdGNevFqYOcHMlM66cC6j0oForIXWxAoxLWrgebVRWaSib6Xt07q6C5t
;;; HW1mxBwm+6Bq1h3RDitf1zweOIdzq6T0XXgZoPbviGTyydnNU8wH1O4yZcGVmLcW
;;; /miha9Y06gz75CUF07fW1ApAQ+aMZon2a9HhVsKNdCHcENd5oRJPRg8GQlcUitV7
;;; t14Umyr0XNN6qMCPPtdD4jyebvI7QzbsX5kEIK3/HrnyA4TY1VEKYQ3TWmNXuEUm
;;; RYoeCEjI3BEJw8QsjOeuTbh9BTi43WD7ZI90mdLLzfIpoKDPFYw1dipB3l13DPdu
;;; J8ghJA0iG69Az9MkbFMEItt0R3BWorOnX+FGu4JsDFiE1HBczmOH/Ep+QpB4/HuJ
;;; NIcDvu/E2d81aHG9P7VGD+oyczv4F9KwyVjBYvkoqLWlz3lx7CP0JhsHTZIT2/UZ
;;; hXCI+TbAO8lrmvTvS+j40tb01Fj8K9/ioqDrDbmNtSJ/xRBoFbyDf/QLgoksRBQl
;;; IjU8TzbFEiCrX3T/cqk7q2qYzwT9UNZhujqMxoeErLl1xBwxcMLken7srC3WmUvc
;;; WZ9fr1odo3EaesdC9IAd0Mn1sY7mVP7kpPBn0pGg6FC9rh2woSVrz7PeZpQOjVrL
;;; 5pM50w==
;;; -----END-SIGNATURE-----