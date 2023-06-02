///                                                                    /
///  GPDIALOG.DCL                                                      /
///                                                                    /
///  Copyright 1987, 1988, 1990, 1992, 1994, 1996, 1997, 1998          /
///  by Autodesk, Inc. All Rights Reserved.                            /
///                                                                    /
///  You are hereby granted permission to use, copy and modify this    /
///  software without charge, provided you do so exclusively for       /
///  your own use or for use by others in your organization in the     /
///  performance of their normal duties, and provided further that     /
///  the above copyright notice appears in all copies and both that    /
///  copyright notice and the limited warranty and restricted rights   /
///  notice below appear in all supporting documentation.              /
///                                                                    /
///  Incorporation of any part of this software into other software,   /
///  except when such incorporation is exclusively for your own use    /
///  or for use by others in your organization in the performance of   /
///  their normal duties, is prohibited without the prior written      /
///  consent of Autodesk, Inc.                                         /
///                                                                    /
///  Copying, modification and distribution of this software or any    /
///  part thereof in any form except as expressly provided herein is   /
///  prohibited without the prior written consent of Autodesk, Inc.    /
///                                                                    /
///  AUTODESK PROVIDES THIS SOFTWARE "AS IS" AND WITH ALL FAULTS.      /
///  AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF           /
///  MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK,       /
///  INC. DOES NOT WARRANT THAT THE OPERATION OF THE SOFTWARE          /
///  WILL BE UNINTERRUPTED OR ERROR FREE.                              /
///                                                                    /
///  Restricted Rights for US Government Users.  This software         /
///  and Documentation are provided with RESTRICTED RIGHTS for US      /
///  US Government users.  Use, duplication, or disclosure by the      /
///  Government is subject to restrictions as set forth in FAR         /
///  12.212 (Commercial Computer Software-Restricted Rights) and       /
///  DFAR 227.7202 (Rights in Technical Data and Computer Software),   /
///  as applicable.  Manufacturer is Autodesk, Inc., 111 McInnis       /
///  Parkway, San Rafael, California 94903.                            /
///                                                                    /

///--------------------------------------------------------------------/
///  This is the dialog file for the Garden Path tutorial.             /
///--------------------------------------------------------------------/


gp_mainDialog : dialog {
	label = "Garden Path Tile Specifications"; 
	: boxed_radio_column {        // defines the radio button areas
		label = "Outline Polyline Type";
		: radio_button {         // defines the Lightweight radio button
			label = "&Lightweight";
			key = "gp_lw";
			value = "1";
		}
		: radio_button {         // defines the old-style polyline radio button
			label = "&Old-style";
			key = "gp_hw";
		}
	}

	
	
	: boxed_radio_column {        // defines the radio button areas
		label = "Tile Creation Method";
		: radio_button {         // defines the ActiveX radio button
			label = "&ActiveX Automation";
			key = "gp_actx";
			value = "1";
		}
		: radio_button {         // defines the (entmake) radio button
			label = "&Entmake";
			key = "gp_emake";
		}
		: radio_button {         // defines the (command) radio button
			label = "&Command";
			key = "gp_cmd";
		}
	}


	: edit_box {      // defines the Radius of Tile edit box
		label = "&Radius of tile";
		key = "gp_trad";
		edit_width = 6;
	}
	: edit_box {      // defines the Spacing Between Tiles edit box
		label = "&Spacing between tiles";
		key = "gp_spac";
		edit_width = 6;
	}
	: row {          // defines the OK/Cancel button row
		: spacer { width = 1; }
		: button {    // defines the OK button
			label = "OK";
			is_default = true;
			key = "accept";
			width = 8;
			fixed_width = true;
		}
		: button {    // defines the Cancel button
			label = "Cancel";
			is_cancel = true;
			key = "cancel";
			width = 8;
			fixed_width = true;
		}
		: spacer { width = 1;}
	}

}