You must identify or change the location of the tutorial's DCL file.

Visual LISP will search the AutoCAD support directories during a load of a dialog.
The tutorial directories are not included in this search path, so if you
do not move the DCL file or do not provide the full path name to the
load_dialog expression in "gp-io.lsp", the function gp:getDialogInput
will fail with an error.

1) Move the dialog file "gpdialog.dcl" to the /Support subdirectory under
your main AutoCAD directory.  

OR

2) Modify the Support File Search Path to include the directory in which
you are editing tutorial code.  From AutoCAD's main menu, choose Tools->Options
and select the "Files" tabbed dialog.