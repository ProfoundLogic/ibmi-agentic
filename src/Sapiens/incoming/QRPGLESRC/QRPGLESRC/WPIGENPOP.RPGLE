     H Copyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE WPIGENPOP - Generic Popups Service Program
     H DFTNAME(WPIGENPOP)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H
     H**********************************************************************
    *===============================================================*
    *  Created   : 06/18/12                                         *
    *  Programmer: C:191                                            *
    *  Project   : Transform                                        *
    *===============================================================*
      *  Description:  Service program which contains the screen files
      *                and processing for Generic popups.
      *
      *  The popups included are:
      *      Save success
      *      Need Save
      *
      *********************************************************************
      * REVISIONS:
     *
      * 06/01/16 C:298 Phaseware: 41499                               *
      *                Corrected spelling of "successfully".          *
      *                                                               *
      *
      *********************************************************************
      *
      *
     H nomain
    F*
     FWPIGENPOP CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     USROPN
      *
      /copy wprGenPop
     p*=====================================================================
     p* PopDataSaved - Data saved popup window
     p*  Returns - Ok button Response
     P*=====================================================================
     PPopDataSaved     B                   Export
     DPopDataSaved     PI              n
      *
     D xxMessage       s            256a   inz
     D xxTitle         s             50a   inz
      /Free
       //***** Beg Chg ***** 06/01/16 **************************************
       xxMessage = 'Data sucessfully saved';
       xxMessage = 'Data successfully saved';
       //***** End Chg ***** 06/01/16 **************************************
       xxTitle = 'Data saved';
       Return PopOneBtn(xxMessage:xxTitle);
      /End-Free
     PPopDataSaved     E
     p*=====================================================================
     p* PopSaveNeed - Data save needed popup window
     p*  Returns - Ok button Response
     P*=====================================================================
     PPopSaveNeed      B                   Export
     DPopSaveNeed      PI              n
      *
     D xxMessage       s            256a   inz
     D xxTitle         s             50a   inz
      /Free
       xxMessage = 'Data changes must be saved first';
       xxTitle = 'Data changes';
       Return PopOneBtn(xxMessage:xxTitle);
      /End-Free
     PPopSaveNeed      E
     p*=====================================================================
     p* PopOneBtn   - One button popup
     p*  Parm1 - Message (256A)
     p*  Parm2 - Panel Title (50A)
     p*  Parm3 - Optional Button text (10A)
     p*  Parm4 - Optional Left (x-coordinate) pixel value (5,0)
     p*  Parm5 - Optional Top (y-coordinate)r pixel value (5,0)
     p*  Returns - button Response
     P*=====================================================================
     PPopOneBtn        B                   Export
     DPopOneBtn        PI              n
     D inMessage                    256a   const
     D inTitle                       50a   const
     D inBtnTxt                      10a   const options(*nopass)
     D inLeft                         5  0 options(*nopass) const
     D inTop                          5  0 options(*nopass) const
      *
      /Free
       If Not %Open(wpiGenPop);
         Open wpiGenPop;
       Endif;
       scMessage = inMessage;
       scPnlTitle = inTitle;
       If %parms > 2 And inBtnTxt <> *Blanks;
         scBtnText = inBtnTxt;
       Else;
         scBtnText = 'OK';
       EndIf;

       If %parms > 3 And inLeft > 0;
         scPLeft = inLeft;
       Else;
         scPLeft = 350;
       EndIf;

       If %parms > 4 And inTop > 0;
         scPTop = inTop;
       Else;
         scPTop = 215;
       Endif;

       scMLeft = scPLeft + 10;
       scMTop = scPTop + 45;

       scBLeft = scMLeft + 165;
       scBTop = scMTop + 100;

       ExFmt rcdOneBtn;
       If %Open(wpiGenPop);
         Close wpiGenPop;
       Endif;
       Return BtnOk;
      /End-Free
     PPopOneBtn        E
     p*=====================================================================
     p* PopupTwoBtn - Popup to show Two buttons
     p*  Parm1 - Message (256A)
     p*  Parm2 - Panel Title (50A)
     p*  Parm3 - Button 1 text (10A) optional
     p*  Parm4 - Button 2 text (10A) optional
     p*  Parm3 - Optional Left (x-coordinate) pixel value (5,0)
     p*  Parm4 - Optional Top (y-coordinate)r pixel value (5,0)
     p*  Returns - *On for button 1 and *Off for button 2
     P*=====================================================================
     PPopTwoBtn        B                   Export
     DPopTwoBtn        PI              n
     D inMessage                    256a   const
     D inTitle                       50a   const
     D inBtn1Txt                     10a   options(*nopass) const
     D inBtn2Txt                     10a   options(*nopass) const
     D inLeft                         5  0 options(*nopass) const
     D inTop                          5  0 options(*nopass) const
      *
     D xxResponse      s               n   inz
      /Free
       If Not %Open(wpiGenPop);
         Open wpiGenPop;
       Endif;
       scOkCnMsg = inMessage;
       scOkCnTtl= inTitle;
       If %parms > 2 And inBtn1Txt <> *Blanks;
         scBtn1Txt = inBtn1Txt;
       Else;
         scBtn1Txt = 'OK';
       EndIf;
       If %parms > 3 And inBtn2Txt <> *Blanks;
         scBtn2Txt = inBtn2Txt;
       Else;
         scBtn2Txt = 'Cancel';
       EndIf;

       If %parms > 4 And inLeft >0;
         scPLeft = inLeft;
       Else;
         scPLeft = 350;
       EndIf;

       If %parms > 5 And inTop >0;
         scPTop = inTop;
       Else;
         scPTop = 215;
       Endif;

       scMLeft = scPLeft + 10;
       scMTop = scPTop + 65;

       scOkLeft = scMLeft + 115;
       scOkTop = scMTop + 80;
       scCnLeft = scMLeft + 225;
       scCnTop = scMTop + 80;

       ExFmt rcdTwoBtn;
       Select;
       When BtnOk = *On;
         xxResponse = *On;
       When BtnCancel = *On;
         xxResponse = *Off;
       EndSl;
       If %Open(wpiGenPop);
         Close wpiGenPop;
       Endif;
       Return xxResponse;
      /End-Free
     PPopTwoBtn        E
     p*=====================================================================
     p* PopThreeBtn  - Popup to show Three buttons             with message
     p*  Parm1 - Message (256A)
     p*  Parm2 - Panel Title (50A)
     p*  Parm3 - Button 1 text (10A) optional
     p*  Parm4 - Button 2 text (10A) optional
     p*  Parm5 - Button 3 text (10A) optional
     p*  Parm6 - Optional Left (x-coordinate) pixel value (5,0)
     p*  Parm7 - Optional Top (y-coordinate)r pixel value (5,0)
     p*  Returns - *On for button 1, *Off for 2, and blanks for 3
     P*=====================================================================
     PPopThreeBtn      B                   Export
     DPopThreeBtn      PI             1a
     D inMessage                    256a   const
     D inTitle                       50a   const
     D inBtn1Txt                     10a   options(*nopass) const
     D inBtn2Txt                     10a   options(*nopass) const
     D inBtn3Txt                     10a   options(*nopass) const
     D inLeft                         5  0 options(*nopass) const
     D inTop                          5  0 options(*nopass) const
      *
     D xxResponse      s              1a   inz
      /Free
       If Not %Open(wpiGenPop);
         Open wpiGenPop;
       Endif;
       scYNCMsg = inMessage;
       scYNCTtl= inTitle;
       If %parms > 2 And inBtn1Txt <> *Blanks;
         scBtn1Txt = inBtn1Txt;
       Else;
         scBtn1Txt = 'Yes';
       EndIf;
       If %parms > 3 And inBtn2Txt <> *Blanks;
         scBtn2Txt = inBtn2Txt;
       Else;
         scBtn2Txt = 'No';
       EndIf;
       If %parms > 4 And inBtn3Txt <> *Blanks;
         scBtn3Txt = inBtn3Txt;
       Else;
         scBtn3Txt = 'Cancel';
       EndIf;
       If %parms > 5 And inLeft > 0;
         scPLeft = inLeft;
       Else;
         scPLeft = 350;
       EndIf;
       If %parms > 6 And inTop > 0;
         scPTop = inTop;
       Else;
         scPTop = 215;
       Endif;

       scMLeft = scPLeft + 10;
       scMTop = scPTop + 65;

       scYLeft = scMLeft + 60;
       scYTop = scMTop + 80;
       scNLeft = scMLeft + 170;
       scNTop = scYTop;
       scCnLeft = scMLeft + 280;
       scCnTop = scYTop;

       ExFmt rcdThreBtn;
       Select;
       When BtnYes = *On;
         xxResponse = *On;
       When BtnNo = *On;
         xxResponse = *Off;
       When BtnCancel = *On;
         xxResponse = ' ';
       EndSl;
       If %Open(wpiGenPop);
         Close wpiGenPop;
       Endif;
       Return xxResponse;
      /End-Free
     PPopThreeBtn      E
