; ██╗███╗   ███╗██████╗  ██████╗ ██████╗ ████████╗
; ██║████╗ ████║██╔══██╗██╔═══██╗██╔══██╗╚══██╔══╝
; ██║██╔████╔██║██████╔╝██║   ██║██████╔╝   ██║   
; ██║██║╚██╔╝██║██╔═══╝ ██║   ██║██╔══██╗   ██║   
; ██║██║ ╚═╝ ██║██║     ╚██████╔╝██║  ██║   ██║   
; ╚═╝╚═╝     ╚═╝╚═╝      ╚═════╝ ╚═╝  ╚═╝   ╚═╝                                            

;Import VL Load
(vl-load-com)


; ██╗   ██╗ █████╗ ██████╗ ██╗ █████╗ ██████╗ ██╗     ███████╗███████╗
; ██║   ██║██╔══██╗██╔══██╗██║██╔══██╗██╔══██╗██║     ██╔════╝██╔════╝
; ██║   ██║███████║██████╔╝██║███████║██████╔╝██║     █████╗  ███████╗
; ╚██╗ ██╔╝██╔══██║██╔══██╗██║██╔══██║██╔══██╗██║     ██╔══╝  ╚════██║
;  ╚████╔╝ ██║  ██║██║  ██║██║██║  ██║██████╔╝███████╗███████╗███████║
;   ╚═══╝  ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝╚═╝  ╚═╝╚═════╝ ╚══════╝╚══════╝╚══════╝

(setq *ModelSpace* (vla-get-ModelSpace 
                     (vla-get-ActiveDocument (vlax-get-acad-object))
                   ) ;_ end of vla-get-ModelSpace
) ;_ end of setq

;TAKE THE FOLDER DIRECTORY OF THE PROJECT FILES TO BE CHANGED
(setq project_files_directory "C:/Users/BAHA-LAPTOP/Documents/GitHub/tekfenRandomStudies/VLISP/interDWG/files/project_files/")
(setq project_files ())

;MERGES PROJECT FILE NAMES AND THE DIRECTORIES TO GET THE ABSOULUTE PATH OF EACH FILE
(foreach file (vl-directory-files project_files_directory nil 1)
  (setq project_files (cons (strcat project_files_directory file) project_files))
)

;TAKE THE REFERENCE DWG FILE PATH
(setq reference_file "C:/Users/BAHA-LAPTOP/Documents/GitHub/tekfenRandomStudies/VLISP/interDWG/files/reference_file_test_01_singlebox.dwg")


; ███████╗██╗   ██╗███╗   ██╗ ██████╗████████╗██╗ ██████╗ ███╗   ██╗
; ██╔════╝██║   ██║████╗  ██║██╔════╝╚══██╔══╝██║██╔═══██╗████╗  ██║
; █████╗  ██║   ██║██╔██╗ ██║██║        ██║   ██║██║   ██║██╔██╗ ██║
; ██╔══╝  ██║   ██║██║╚██╗██║██║        ██║   ██║██║   ██║██║╚██╗██║
; ██║     ╚██████╔╝██║ ╚████║╚██████╗   ██║   ██║╚██████╔╝██║ ╚████║
; ╚═╝      ╚═════╝ ╚═╝  ╚═══╝ ╚═════╝   ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝

;OPENS ALL THE PROJECT FILES IN THE DIRECTORY -> COPY AND APPEND THE DATA -> CLOSES THE FILES
(foreach file project_files
  (setq f (open file "w"))
  
  (vl-file-copy reference_file file T)
  
  (close f)
)





;FOR EACH FILE IN PROJECT FILES FOLDER
    ;OPEN PROJECT FILE
        ;REMOVE THE LETERHEAD BLOCK
        ;PLACE THE REFERENCE LETTERHEAD INTO PROJECT FILE
        ;ADJUST SOME VALUES


(defun c:custfiles () 
  (command "shell" 
           (strcat "explorer \"" project_files_path "\"")
  )
  (princ)
)

(defun c:reffile () 
  (command "shell" 
           (strcat "explorer \"" reference_file "\"")
  )
  (princ)
)



(prompt "Function has been loaded")