     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE SPIGENPOP - Generic Popups Service Program
     H DFTNAME(SPIGENPOP)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H
     H**********************************************************************
    *===============================================================*
    *  Created   : 06/18/12                                         *
    *  Programmer: C:191  C:33                                      *
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
      * 03/16/20 C:212 Project : WC2447
      *                Add new procedure PopOneButtonAgain and related
      *                processing in DSPF/Pgm
     *
      * 06/01/16 C:298 Phaseware: 41499
      *                Corrected spelling of "successfully".
      *
      * 11/20/12  C:33  Project: WC2116
      *                 Add RcdUpdatedErr
      *********************************************************************
      *
      *
     H nomain
    F*
     FSPIGENPOP CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     f***** Beg Add *** 03/16/20 *******************************************
     fsmdndl    uf a e           k disk
     fsmupl     if   e           k disk
     f***** End Add *** 03/16/20 *******************************************
      *
     f***** Beg Add *** 03/16/20 *******************************************
     d psds          esds                  extname(szq1p)
     f***** End Add *** 03/16/20 *******************************************
      *
      /copy sPrGenPop
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
       // ***** Beg Chg ***** 06/01/16 *************************************
       //xxMessage = 'Data sucessfully saved';
       xxMessage = 'Data successfully saved';
       // ***** End Chg ***** 06/01/16 *************************************
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
       Return BtnOk;
      /End-Free
     PPopOneBtn        E
     p*=====================================================================
     p****** Beg Add *** 03/16/20 *************************************
     p*=====================================================================
     p* PopOneBtnAgain - One button popup with Do not display option
     p*  Parm1 - Message (256A)
     p*  Parm2 - Panel Title (50A)
     p*  Parm3 - Key Value (25A)
     p*  Parm4 - Optional Button text (10A)
     p*  Parm5 - Optional Left (x-coordinate) pixel value (5,0)
     p*  Parm6 - Optional Top (y-coordinate)r pixel value (5,0)
     p*  Returns - button Response
     P*=====================================================================
     PPopOneBtnAgain   B                   Export
     DPopOneBtnAgain   PI              n
     D inMessage                    256a   const
     D inTitle                       50a   const
     D inKeyValue                    25a   const
     D inBtnTxt                      10a   const options(*nopass)
     D inLeft                         5  0 options(*nopass) const
     D inTop                          5  0 options(*nopass) const
      *
     D strUserId       s             10    inz
     D intCo#          s              3s 0 inz
     D indShowWdw      s               n   inz(*off)
      *
      /Free
       scMessage = inMessage;
       scPnlTitle = inTitle;
       If %parms > 3 And inBtnTxt <> *Blanks;
         scBtnText = inBtnTxt;
       Else;
         scBtnText = 'OK';
       EndIf;

       If %parms > 4 And inLeft > 0;
         scPLeft = inLeft;
       Else;
         scPLeft = 350;
       EndIf;

       If %parms > 5 And inTop > 0;
         scPTop = inTop;
       Else;
         scPTop = 215;
       Endif;

       scMLeft = scPLeft + 10;
       scMTop = scPTop + 45;

       scBLeft = scMLeft + 165;
       scBTop = scMTop + 100;

       // check box for Do not display Again
       scCBLeft = scMLeft;
       scCBTop = scBTop + 20;

       // checkbox label
       scCLLeft = scCBLeft + 20;
       scCLTop = scCBTop;

       // get UserId and Company number
       strUserId = q1user;
       Chain(n) strUserID pup01;
       If %found(smupl);
          intCo# = UPco#;
       EndIf;

       // check for the flag value to show or not show the window
       Chain(n) (intCo#:strUserId:inKeyValue) pdn01;
       If %found(smdndl);
          indShowWdw = *off;
       Else;
          indShowWdw = *on;
       EndIf;

       If indShowWdw = *on;
          ExFmt rcdOneBtnA;

          // write to file if user checked for 'Do not display Again'
          If scDntDsp = 'Y';
             DNco# = intCo#;
             DNusr = strUserId;
             DNpopid = inKeyValue;

             DNstat = 'A';
             DNuser = q1user;
             DNpgm  = q1pgm;
             DNdt   = %dec(%char(%date():*cymd0):7:0);
             DNtime = %dec(%char(%time():*iso0):6:0);
             DNiusr = q1user;
             DNipgm = q1pgm;
             DNidt  = %dec(%char(%date():*cymd0):7:0);
             DNitim = %dec(%char(%time():*iso0):6:0);

             write pdn01;
          EndIf;

       EndIf;

       Return BtnOk;
      /End-Free
     PPopOneBtnAgain   E
     p*=====================================================================
     p****** End Add *** 03/16/20 *************************************
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
       scMTop = scPTop + 45;

       scOkLeft = scMLeft + 115;
       scOkTop = scMTop + 100;
       scCnLeft = scMLeft + 225;
       scCnTop = scMTop + 100;

       ExFmt rcdTwoBtn;
       Select;
       When BtnOk = *On;
         xxResponse = *On;
       When BtnCancel = *On;
         xxResponse = *Off;
       EndSl;
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
       Return xxResponse;
      /End-Free
     PPopThreeBtn      E

     p*=====================================================================
     p* RcdUpdatedErr - This wil check the record to determine if it has
     p*                  been updated since the values were loaded.
     p*  Parm1 - File (10A)
     p*  Parm2 - RRN (9.0)
     p*  Parm3 - File date field (10A)
     p*  Parm4 - File time field (10A)
     p*  Parm5 - File user field (10A)
     p*  Parm6 - Comparison date (7.0)
     p*  Parm7 - Comparison time (6.0)
     p*  Parm8 - Allow 'Refresh' button (1n) optional
     p*  Returns - '0' for no error
     p*            '1' for error
     p*            '2' for refresh
     P*=====================================================================
     PRcdUpdatedErr    B                   Export
     DRcdUpdatedErr    PI             1a
     D inFile                        10a   const
     D inRRN                          9  0 const
     D inDateField                   10a   const
     D inTimeField                   10a   const
     D inUserField                   10a   const
     D inOriginalDate                 7  0 const
     D inOriginalTime                 6  0 const
     D inAllowRefresh                 1n   const options(*nopass)
      *
     DflgResponse1     s              1    inz
     DflgResponse2     s              1    inz
     DstrMessage       s            256
     DstrTitle         s             50    inz('File Updated')
     DstrBtn1          s             10    inz('OK')
     DstrBtn2          s             10    inz('Refresh')
     DindAllowRefresh  s              1n   inz(*off)
     DstrSqlCursor     s           5000

     dresultsDS        ds                  inz
     d fileDate                       7  0
     d fileTime                       6  0
     d fileUser                      10

      /Free
       If %parms >= 8;
          indAllowRefresh = inAllowRefresh;
       Endif;

       strSqlCursor = 'select ' + %trim(inDateField) + ', ' +
        %trim(inTimeField) + ', ' + %trim(inUserField) +
        ' from ' + %trim(inFile) +
        ' where rrn(' + %trim(inFile) + ') = ' + %char(inRRN);

       Exec SQL Prepare sql1 From :strSqlCursor;
       Exec SQL Declare cursor1 Cursor For sql1;
       Exec SQL Open cursor1;
       Exec SQL Fetch next From cursor1 into :resultsDS;
       Exec SQL Close cursor1;

       If (inOriginalDate <> fileDate or inOriginalTime <> fileTime);
          strMessage = 'This record has already been updated by ' +
           %Trim(fileUser) + '.';
          Select;
          When indAllowRefresh;
             flgResponse2 = PopTwoBtn(strMessage:strTitle:strBtn1:strBtn2);
          Other;
             flgResponse1 = PopOneBtn(strMessage:strTitle:strBtn1);
          Endsl;
          Select;
          When flgResponse2 = '0';
             Return '2';
          Endsl;
          Return '1';
       Endif;

       Return '0';


      /End-Free
     PRcdUpdatedErr    E
