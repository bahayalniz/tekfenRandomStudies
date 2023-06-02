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
;;;  state of the application at the end of Lesson 1.  Use this file   ;
;;;  to check your work, or to start off Lesson 2 with the code as it  ;
;;;  appears in the tutorial.                                          ;
;;;--------------------------------------------------------------------;


;;;--------------------------------------------------------------------;
;;;     Function: gp:getPointInput                                     ;
;;;--------------------------------------------------------------------;
;;;  Description: This function will ask the user to select three      ;
;;;               points in the drawing, which will determine the      ;
;;;               path location, direction, and size.                  ;
;;;--------------------------------------------------------------------;
(defun gp:getPointInput	()
  (alert
    "Function gp:getPointInput will get user drawing input"
  )
  ;; For now, return T, as if every task in the function worked correctly
  T
) ;_ end of defun


;;;--------------------------------------------------------------------;
;;;     Function: gp:getDialogInput                                    ;
;;;--------------------------------------------------------------------;
;;;  Description: This function will ask the user to determine the     ;
;;;               following path parameters:                           ;
;;;                   Tile size, Tile spacing                          ;
;;;                   Boundary polyline type                           ;
;;;                   Entity creation method                           ;
;;;--------------------------------------------------------------------;
(defun gp:getDialogInput ()
  (alert
    "Function gp:getDialogInput will get user choices via a dialog"
  )
  ;; For now, return T, as if every task in the function worked correctly
  T
) ;_ end of defun


;;;--------------------------------------------------------------------;
;;;     Function: gp:drawOutline                                       ;
;;;--------------------------------------------------------------------;
;;;  Description: This function will draw the outline of the garden    ;
;;;               path.                                                ;
;;;--------------------------------------------------------------------;
(defun gp:drawOutline ()
  (alert
    (strcat "This function will draw the outline of the polyline "
	    "\nand return a polyline entity name/pointer."
    ) ;_ end of strcat
  ) ;_ end of alert
  ;; For now, simply return a quoted symbol.  Eventually, this function
  ;; will return an entity name or pointer
  'SomeEname
) ;_ end of defun


;;;********************************************************************;
;;;     Function: C:GPath        The Main Garden Path Function         ;
;;;--------------------------------------------------------------------;
;;;  Description: This is the main garden path function.  It is a C:   ;
;;;               function, meaning that it is turned into an AutoCAD  ;
;;;               command called GPATH.  This function determines the  ;
;;;               overall flow of the Garden Path program              ;
;;;********************************************************************;
(defun C:GPath (/ PolylineName)
  ;; Ask the user for input: first for path location and
  ;; direction, then for path parameters.  Continue only if you have
  ;; valid input.
  (if (gp:getPointInput)
    (if	(gp:getDialogInput)
      (progn
	;; At this point, you have valid input from the user.
	;; Draw the outline, storing the resulting polyline "pointer"
	;; in the variable called PolylineName
	(setq PolylineName (gp:drawOutline))
	(princ "\nThe gp:drawOutline function returned <")
	(princ PolylineName)
	(princ ">")
	(Alert "Congratulations - your program is complete!")
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
;;; ADANBgkqhkiG9w0BAQEFAASCAgAZSs5eAG8jgd0c9aOcQPufHHCKf+y0YUQijRvY
;;; 62cw6TXJq5KA253vGX3DTx9JlxqOHB+gdS2WgN12Ynilq6Ac5uRtKIxS9bx/hhKe
;;; CSwr1PlvE90hYAWvad8Zb+h2x+Ef2h07/LD5N3Xmv0dfasyT/Cf8k9xgwwFKLqNI
;;; KxobBl37g42RDS5eC0kGXMuavpAgUNpJM3EF4SbTFBeovy3QxTeObX2wiKYzyOWA
;;; fcKOjVjx0DG8cSZAl+LZ0GU5dzv+ftdjOc5dIOVGosophg3X4TQrr/Pu8Hnt+3MQ
;;; Nfcun0kRMMBREn/l4yPg5+mkgOUvjbcxn5tWa64nF8Y6xgWXInLSfI2RqWI6Oou6
;;; nWtlKvjXSaQucgl+j6zxqlYFto3MZh5voBcYRTBkwtnsDWZMHA2laUsSU+uSpIgl
;;; bvDYZs1Nj2K74AoPaKiB9IaHjRwk+v6mwGy8Q7Q1RsSoAB+gpBwOKMHreWUtuF+6
;;; TiJcEpCQzVKt+Ef6O5Q4ghc5sCP4Bzemkjm8IRN5qv0s1u3aiwBCqjVw+uijoMcC
;;; JHbKW4IZEMB0io4toa+Cl9TByOT+hNMyvGf/6fonFmsE4kKaq871oP2HpZs2mA+l
;;; 8VmiPHsWG0Nr5g+ySTKuLa8BxxhCrqU+18ZNMpgpqr692OwPYKqdDRFn7bLFSa0S
;;; qce8Kg==
;;; -----END-SIGNATURE-----