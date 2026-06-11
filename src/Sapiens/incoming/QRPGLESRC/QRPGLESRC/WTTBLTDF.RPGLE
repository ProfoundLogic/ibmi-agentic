     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title WTTBLTDF - Territorial Differential Factor Maintenance
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program Will Maintain the Territorial Differential
     h*                Factors by Zip code File
     h*
     h*  Called from: Menu
     h*  Calls to:
     h*
     h**********************************************************************
     h*  Created on 7/23/2017 by C:062 for Project: 150WAL
     h**********************************************************************
     h*  Revisions:
     h*                                                                    *
     h* 03/25/22 C:333 Project: SY434                                      *
     h*                - Update Grids for Sorting Filtering Upgrade        *
     h*
      * 08/19/19 C:291 Project: 150WEA                                     *
      *                Add Effective and Expiration date. Rename From Zip  *
      *                code column and delete To Zip code column.          *
      *                                                                    *
      **********************************************************************

     fwttbltdf  cf   e             workstn extfile('WTTBLTDF')
     f                                     Handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflTerDif:rrnTdfCod)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)

     fwmtzl     if   e           k disk    rename(ptz01:ptz00) InfDs(dbfds)
     fwmtzl1    if   e           k disk    Prefix(T2:2)
     fwmtzp     uf a e             disk    rename(ptz01:ptz0p)
     fsmcol     if   e           k disk    rename(pco01:pco00)
     fwtgtl     if   e           k disk    rename(pgt01:pgt00)
     fwdf2l     if   e           k disk    rename(pf201:pf200)

     d psds          esds                  extname(szq1p)
      * constant
     d intFlgDim       c                   10

     d dbfds           ds
     d  dbrrn                397    400b 0

     d intCount        s              3  0
     d @@mode          s              1
     d @@TabIndex      s              5
     d inmode          s              1
     d inTabIndex      s              5  0
     d intCar#         s              5  0 Inz(0)
     d intCo#          s              3  0
     d clientID        s              3    Inz('000')

     d*** Beg Add ***** 08/19/19 *****************************
     d SetCriteria     pr
     d*** End Add ***** 08/19/19 *****************************

     d* Check for Overlaps
     d ChkOverlap      pr              n
     D  carrier#                      5  0
     D  group#                        3  0
     D  fromZipCode                   5  0
     D  toZipCode                     5  0

     d* Check for Duplicates
     d ChkDuplicate    pr              n
     D  carrier#                      5  0
     D  group#                        3  0
     D  ZipCode                       5  0
     D  EffDate                       8  0

     d* Clear Subfiles
     d ClrSubFiles     pr

     d* Edit/Add/Delete Territorial Differential Factor
     D EdtAddDltTdf    PR
     D inRRN                          9  0
     D inMode                         1

     d* Error Check
     D ErrorCheck      PR              n
     d  inMode                        1

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3

     d* Load Territoril Differential Factor
     d LoadTdfValue    pr

     d* Main Processing
     d Main            pr

     d* Program Start
     d ProgramStart    pr

     d* Process other options
     d ProcOther       pr

     d  ##CltId        s              3

     d/copy sPrDate
     d/copy sPrString
     d/copy SPrCompany

     d/copy wPrGroup
     d/copy wPrUsrPrms
     d/copy wTclmTblPr
    
     c* Entry of Program.
     c     *entry        plist
     c                   parm                    @@Mode
     c                   parm                    @@TabIndex

      /free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *inlr = *on;
       return;

      /end-free
       //===================================================================
       // EdtAddDltTdf   - Edit/Add/Delete Territorial Differential Factor
       //===================================================================
     p EdtAddDltTdf    b                   Export
     d EdtAddDltTdf    pi
     d inRRN                          9  0
     d inMode                         1

     d error           s               n

      /Free

       // Set Conditions
       Select;
       // Display Mode
       When inmode = 'D';
          visible1 = *Off;
          disable1 = *On;
          disable2 = *On;
       // Add Mode
       When inmode = 'A';
          visible1 = *On;
          disable1 = *Off;
          disable2 = *Off;
       // Change Mode
       When inmode = 'C';
          visible1 = *Off;
          disable1 = *On;
          disable2 = *Off;
       // Delete Mode
       When inmode = 'E';
          visible1 = *Off;
          disable1 = *On;
          disable2 = *On;
       Endsl;

       // Load Screen Fields
       ExSr MoveDbSc;

       //*** Beg Add ***** 08/19/19 *****************************
       SetCriteria();
       //*** End Add ***** 08/19/19 *****************************

       DoU (btnEntAED = *On And error = *Off) Or btnCanAED = *On;
          error = *Off;
          ExFmt RcdAEDTdf;
          // Don't edit if in Delete Mode
          If btnCanAED = *On;
             Leave;
          Else;
             If inMode = 'A' Or inMode ='C';
                error = ErrorCheck(inMode);
             EndIf;
          EndIf;
          // No errors so Lets Add/Edit/Delete
          If error = *Off;
             ExSr MoveScDb;
          EndIf;
       EndDo;

       return;
       //===================================================================
       // MoveDbSc - Move database record to screen
       //===================================================================
       BegSr MoveDbSc;

          // Add Mode
          If inMode = 'A';
             scCar#AED  = 0;
             scGrpAED   = 0;
             //*** Beg Chg ***** 08/19/19 *****************************
             //scFZipAED  = 0;
             //scTZipAED  = 0;
             scZipAED  = 0;
             scEffDtAED = 0;
             scExpDtAED = 0;
             //*** Beg End ***** 08/19/19 *****************************
             scTdfAED   = 0;
             scStatAED  = 'A';
             scTdfTitle = 'Add Territorial Differential Factor';
             scBtnType  = 'Add';
          // Edit or Delete Mode
          Else;
             Chain(n) inRRN ptz0p;
             If %Found(wmtzp);
                scCar#AED  = TZcar#;
                scGrpAED   = TZfnd;
                //*** Beg Chg ***** 08/19/19 *****************************
                //scFZipAED  = TZfrmZip;
                //scTZipAED  = TZtoZip;
                scZipAED   = TZZip;
                scEffDtAED = dtMDYY(TZEfDt);
                scExpDtAED = dtMDYY(TZExDt);
                //*** End Chg ***** 08/19/19 *****************************
                scTdfAED   = TZtdf;
                scStatAED  = TZstat;
             EndIf;
             // Set the title and the button label
             If inMode = 'E';
                scTdfTitle = 'Delete Territorial Differential Factor';
                scBtnType  = 'Delete';
             Else;
                scTdfTitle = 'Edit Territorial Differential Factor';
                scBtnType  = 'Save';
             EndIf;
          EndIf;

          // Initialize error fields
          mgCar#AED = ' ';
          erCar#AED = *Off;
          mgGrpAED  = ' ';
          erGrpAED  = *Off;
          //*** Beg Chg ***** 08/19/19 *****************************
          //mgFZipAED = ' ';
          //erFZipAED = *Off;
          //mgTZipAED = ' ';
          //erTZipAED = *Off;
          mgZipAED = ' ';
          erZipAED = *Off;
          mgEffDtAED = ' ';
          erEffDtAED = *Off;
          mgExpDtAED = ' ';
          erExpDtAED = *Off;
          //*** End Chg ***** 08/19/19 *****************************
          mgTdfAED  = ' ';
          erTdfAED  = *Off;

       EndSr;
       //===================================================================
       // MoveScDb - Move screen data to database record
       //===================================================================
       BegSr MoveScDb;

          If inMode = 'A';
             Clear ptz0p;
          Else;
             Chain inRRN ptz0p;
             If %Found(wmtzp) And inMode = 'E';
                Delete ptz0p;
                Return;
             EndIf;
          EndIf;

          TZcar#   = scCar#AED;
          TZfnd    = scGrpAED;
          //*** Beg Chg ***** 08/19/19 *****************************
          //TZfrmZip = scFZipAED;
          //TZtoZip  = scTZipAED;
          TZZip    = scZipAED;
          TZEfDt   = dtCYMD(scEffDtAED);
          TZExDt   = dtCYMD(scExpDtAED);
          //*** End Chg ***** 08/19/19 *****************************
          TZtdf    = scTdfAED;
          TZstat   = scStatAED;

          TZuser   = Q1user;
          TZpgm    = Q1pgm;
          TZtime   = TimeTo6(SystemTime());
          TZdt     = DateTo7(SystemDate());

          If inMode = 'A';
             TZiUsr   = Q1user;
             TZiPgm   = Q1pgm;
             TZiTim   = TimeTo6(SystemTime());
             TZiDt    = DateTo7(SystemDate());
             Write ptz0p;
          Else;
             Update ptz0p;
          EndIf;

       EndSr;

      /end-free
     p EdtAddDltTdf    e
       //===================================================================
       // ErrorCheck - ErrorCheck for Add and Edit Modes
       //===================================================================
     p ErrorCheck      b
     d ErrorCheck      pi              n
     d  inMode                        1

     d error           s               n   Inz(*Off)
     d keyError        s               n   Inz(*Off)
      /free

       // Reset Error Indicators and such
       ExSr ResetErrorInd;

       // Carrier # is Required
       If scCar#AED = 0;
          //*** Beg Chg ***** 08/19/19 *****************************
          //mgCar#AED = 'Carrier # must be entered';
          //erCar#AED = *On;
          //error     = *On;
          //keyError  = *On;
          //*** End Chg ***** 08/19/19 *****************************
       Else;
          // If Group Selected, then it Carrier # must match
          If scGrpAED <> 0;
             Chain (intCo#:scGrpAED) pf200;
             If %Found(wdf2l) And scCar#AED <> F2car#;
                mgCar#AED = 'Carrier # must match the selected group.';
                erCar#AED = *On;
                error     = *On;
                keyError  = *On;
             EndIf;
          EndIf;
       EndIf;

       // From Zip code is required
       //*** Beg Del ***** 08/19/19 *****************************
       //If scFZipAED = 0;
       //   mgFZipAED = 'From Zip Code must be entered';
       //   erFZipAED = *On;
       //   error     = *On;
       //  keyError  = *On;
       //EndIf;

       // To Zip code is required
       //If scTZipAED = 0;
       //   mgTZipAED = 'To Zip Code must be entered';
       //   erTZipAED = *On;
       //   error     = *On;
       //   keyError  = *On;
       //EndIf;

       // From Zip code is required
       //If scFZipAED <> 0 And scTZipAED <> 0 And scFZipAED > scTZipAED;
       //   mgFZipAED = 'Zip Codes must be in ascending order.';
       //   erFZipAED = *On;
       //   mgTZipAED = 'Zip Codes must be in ascending order.';
       //   erTZipAED = *On;
       //   error     = *On;
       //   keyError  = *On;
       //EndIf;

       // Don't Allow Range Overlap
       //If inMode = 'A' And keyError = *Off;
       //   If ChkOverlap(scCar#AED:scGrpAED:scFZipAED:scTZipAED) = *On;
       //      mgFZipAED = 'Zip Code Range Overlaps.';
       //      erFZipAED = *On;
       //      mgTZipAED = 'Zip Code Range Overlaps.';
       //      erTZipAED = *On;
       //      error     = *On;
       //   EndIf;
       //EndIf;

       // If an entry is reactivated, prevent if overlap exists
       //If TZstat = 'I' and scStatAED = 'A';
       //   If ChkOverlap(scCar#AED:scGrpAED:scFZipAED:scTZipAED) = *On;
       //      mgFZipAED = 'Zip Code Range Overlaps.';
       //      erFZipAED = *On;
       //      mgTZipAED = 'Zip Code Range Overlaps.';
       //      erTZipAED = *On;
       //      error     = *On;
       //   EndIf;
       //EndIf;

       // Territorial Differential Factor Must be entered
       //If scTdfAED < 0;
       //   mgTdfAED = 'Territorial Differential Factor cannot be negative.';
       //   erTdfAED = *On;
       //   error    = *On;
       //EndIf;
       //*** End Del ***** 08/19/19 *****************************

       //*** Beg Add ***** 08/19/19 *****************************
       // Zip code is required
       If scZipAED = 0;
          mgZipAED = 'Zip Code must be entered.';
          erZipAED = *On;
          error     = *On;
          keyError  = *On;
       EndIf;

       // Effective date is required
       If scEffDtAED = 0;
          mgEffDtAED = 'Effective Date must be entered.';
          erEffDtAED = *On;
          error     = *On;
          keyError  = *On;
       Else;
          scEffDtAED = DtCheck(scEffDtAED);
          If scEffDtAED = *zero;
             mgEffDtAED = 'Invalid Effective date entered.';
             erEffDtAED = *On;
             error     = *On;
             keyError  = *On;
          Endif;
       EndIf;

       //Expiration date must be valid
       If scExpDtAED > 0;
          scExpDtAED = DtCheck(scExpDtAED);
          If scExpDtAED = *zero;
             mgExpDtAED = 'Invalid Expiration date entered.';
             erExpDtAED = *On;
             error     = *On;
             keyError  = *On;
          Endif;
       EndIf;

       //Expiration date must be later than or equal to the Effective date
       If scExpDtAED <> *zero and CvtToDate(scExpDtAED) < CvtToDate(scEffDtAED);
          mgExpDtAED = 'Expiration date must be greater than Effective date.';
          erExpDtAED = *On;
          error     = *On;
          keyError  = *On;
       EndIf;

       // Don't Allow duplicate entry
       If inMode = 'A' And keyError = *Off;
          If ChkDuplicate(ScCar#AED:scGrpAED:scZipAED:scEffDtAED) = *On;
             mgZipAED = 'Zip Code already exist.';
             erZipAED = *On;
             error    = *On;
          EndIf;
       EndIf;

       // If an entry is reactivated, prevent duplicate entry
       If TZstat = 'I' and scStatAED = 'A';
          If ChkDuplicate(scCar#AED:scGrpAED:scZipAED:scEffDtAED) = *On;
             mgZipAED = 'Zip Code already exist.';
             erZipAED = *On;
             error     = *On;
          EndIf;
       EndIf;

       // Territorial Factor must be a numeric value greater than 0 and less than 10
       If scTdfAED <= 0 or scTdfAED > 10;
          mgTdfAED = 'Territorial Differential Factor must be greater than 0 +
                       and less than 10.';
          erTdfAED = *On;
          error    = *On;
       EndIf;
       //*** End Chg ***** 08/19/19 *****************************

       Return error;
       //===================================================================
       // ResetErrorInd - Resets Error Indicators and such
       //===================================================================
       BegSr ResetErrorInd;

          mgCar#AED = ' ';
          erCar#AED = *Off;
          mgGrpAED  = ' ';
          erGrpAED  = *Off;
          //*** Beg Chg ***** 08/19/19 *****************
          //mgFZipAED = ' ';
          //erFZipAED = *Off;
          //mgTZipAED = ' ';
          //erTZipAED = *Off;
          mgZipAED = ' ';
          erZipAED = *Off;
          mgEffDtAED = ' ';
          erEffDtAED = *Off;
          mgExpDtAED = ' ';
          erExpDtAED = *Off;
          //*** End Chg ***** 08/19/19 *****************
          mgTdfAED  = ' ';
          erTdfAED  = *Off;

       EndSr;

      /end-free

     p ErrorCheck      e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /free

       // Program Mode.
       If %parms() >= 1;
          inMode = @@Mode;
       Endif;

       Select;
       When inmode = 'D';
          dspMode  = *On;
          dspVisb  = *Off;
       When inmode = 'A';
          dspMode  = *Off;
          dspVisb  = *On;
       When inmode = 'C';
          dspMode  = *Off;
          dspVisb = *On;
       Endsl;

       // Tab Index Mode.
       If %parms() >= 2;
          intabIndex = %dec(@@TabIndex:5:0);
       Else;
          intabIndex = 0;
       EndIf;

       // Load Action panel with the Company Name.
       clear scCompName;
       intCo# = userCo#(q1user);
       // Get company name
       scCompName = GetCompNam(intCo#);
       aPnlHeight = 610;

       // Get the client id
       GetClientID(##CltId);

       tabClick = *Off;
       Return;

      /end-free
     p ProgramStart    e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

     d inRrn           s              9  0 Inz(0)
     d inMode          s              1    Inz('A')
      /free

       // Process until exit button is pressed.
       Dou ExitNow = *On ;

          // Clear Sub-files
          ClrSubfiles();

          // Load Territorial Differential Factor
          LoadTDFValue();

          // Process Payment Value 1 code Screens
          Exsr ExfmtScreens;

          Select;
          // do nothing - catch tab click event
          When tabClick = *On;
             tabClick = *Off;

          When BtnRefresh = *On;
             BtnRefresh =*Off;

          // Add Payment value
          When scTdfAdd = *On;
             EdtAddDltTdf(inRRN:inMode);

          Other;
             ProcOther();

          Endsl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 550;
          Write rcdComPnl;
          Write rcdTerDif;
          Exfmt rcdTerDif0;
          Read rcdComPnl;
          Read rcdTerDif;
       EndSl;

       Endsr;

      /end-free
     p Main            e
       //===================================================================
       // ChkOverlap - Checks for range overlaps
       //===================================================================
     p ChkOverlap      b
     d ChkOverlap      pi              n
     d  inCarrier#                    5  0
     d  inGroup#                      3  0
     d  inFromZip                     5  0
     d  inToZip                       5  0

     d  carrier#       s              5  0
     d  group#         s              3  0
     d  fromZip        s              5  0
     d  toZip          s              5  0
     d  zeroGroup#     s              3  0 Inz(0)

      /Free

       carrier# = inCarrier#;
       group#   = inGroup#;
       fromZip  = inFromZip;
       toZip    = inToZip;

       // Lets first look at group entries for overlaps
       //SetLL  (carrier#:group#:fromZip) ptz01;
       //ReadPE (carrier#:group#) ptz01;
       //If Not %Eof(wmtzl1);
       //   If fromZip <= T2toZip;
       //      Return *On;
       //   Else;
       //      SetLL (carrier#:group#:fromZip) ptz01;
       //      ReadE (carrier#:group#) ptz01;
       //      If Not %Eof(wmtzl1);
       //         If toZip >= T2frmZip;
       //            Return *On;
       //         EndIf;
       //      EndIf;
       //   EndIf;
       //Else;
       //   SetLL (carrier#:group#:fromZip) ptz01;
       //   ReadE (carrier#:group#) ptz01;
       //   If Not %Eof(wmtzl1);
       //      If toZip >= T2frmZip;
       //         Return *On;
       //      EndIf;
       //   EndIf;
       //EndIf;

       // "Zero" Group
       //If group# = 0;
       //   // Need to look thru all non zero groups for overlaps
       //   SetLL (carrier#:group#) ptz01;
       //   ReadE (carrier#) ptz01;
       //   DoW Not %Eof(wmtzl1);
       //      If (fromZip >= T2frmZip And fromZip <= T2toZip) Or
       //         (toZip >= T2frmZip And toZip <= T2toZip);
       //         Return *On;
       //      EndIf;
       //      ReadE (carrier#) ptz01;
       //   EndDo;
       // Single Group
       //Else;
       //   // Need to look thru all "zero" group entries for overlaps
       //   SetLL  (carrier#:zeroGroup#:fromZip) ptz01;
       //   ReadPE (carrier#:zeroGroup#) ptz01;
       //   If Not %Eof(wmtzl1);
       //      If fromZip <= T2toZip;
       //         Return *On;
       //      Else;
       //         SetLL (carrier#:zeroGroup#:fromZip) ptz01;
       //         ReadE (carrier#:zeroGroup#) ptz01;
       //         If Not %Eof(wmtzl1);
       //            If toZip >= T2frmZip;
       //               Return *On;
       //            EndIf;
       //         EndIf;
       //      EndIf;
       //   Else;
       //      SetLL (carrier#:zeroGroup#:fromZip) ptz01;
       //      ReadE (carrier#:zeroGroup#) ptz01;
       //      If Not %Eof(wmtzl1);
       //         If toZip >= T2frmZip;
       //            Return *On;
       //         EndIf;
       //      EndIf;
       //   EndIf;
       //EndIf;

       Return *Off;

      /End-Free
     p ChkOverlap      e
       //*** Beg Add ***** 08/19/19 *************************************
       //======================================================================
       // ChkDuplicate - Checks for Duplicate active Zip code for Carrier/Fund
       //======================================================================
     p ChkDuplicate    b
     d ChkDuplicate    pi              n
     d  inCarrier#                    5  0
     d  inGroup#                      3  0
     d  inZipCode                     5  0
     d  inEffDate                     8  0

     d  carrier#       s              5  0
     d  group#         s              3  0
     d  ZipCode        s              5  0
     d  EffDate        s              8  0

      /Free

       carrier# = inCarrier#;
       group#   = inGroup#;
       ZipCode  = inZipCode;
       EffDate  = dtCYMD(inEffDate);

       Setll (carrier#:group#:ZipCode:EffDate) ptz01;
       ReadE (carrier#:group#:ZipCode:EffDate) ptz01;
       If Not %Eof(wmtzl1);
             Return *On;
       EndIf;

       Return *Off;

      /End-Free
     p ChkDuplicate    e
       //*** End Add ***** 08/19/19 *************************************
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi
      /Free

       rrnTdfCod  = 0;
       clrTdfCod  = *On;
       dspTdfCtl  = *Off;
       Write rcdTerDif0;

      /End-Free
     p ClrSubfiles     e
       //===================================================================
       // Load Territorial Differential Factor Grid
       //===================================================================
     p LoadTdfValue    b
     d LoadTdfValue    pi

     d strTbid         s                   like (GTtbid)

      /free

       // clear the subfile.
       clear sflTerDif;
       clrTdfCod = *Off;
       dspTdfCtl = *On;
       clear dbRrn;

       Setll intCar# ptz00;
       Read  ptz00;
       dow not %eof(wmtzl);

          // Write Territorial Differential Factor

          // Carrier #
          //*** Beg Chg ***** 08/19/19 *****************
          //scCarr# = TZcar#;
          If TZcar# = 0;
             scCarr# = '*All Carriers';
          Else;
             scCarr# = %Char(TZcar#);
          Endif;
          //*** End Chg ***** 08/19/19 *****************

          // Group
          If TZfnd = 0;
             scGroup = '*All Groups';
          Else;
             scGroup = %Char(TZfnd) + '-' + GetGrpName(TZfnd);
          EndIf;

          //*** Beg Chg ***** 08/19/19 *****************
          // From Zip Code
          //scFromZip = TZfrmZip;

          // To Zip Code
          //scToZip = TZtoZip;

          // Zip Code
          scZipCd = TZZip;
          // Effective Date
          scEffDate = CvtToDate(TZEfDt);
          // Expiration Date
          scExpDate = CvtToDate(TZExDt);
          //*** End Chg ***** 08/19/19 *****************

          // Territorial Differential Factor
          scTDF = TZtdf;

          // Status
          If TZstat = 'A';
             scTdfStat = 'Active';
          Else;
             scTdfStat = 'Inactive';
          Endif;

          scTdfRRN# = dbRrn;

          rrnTdfCod = rrnTdfCod + 1;

          If inmode = 'D';
             dspVisb = *Off;
          Else;
             dspVisb = *On;
          Endif;

          Write sflTerDif;

          Read ptz00;
       Enddo;

       rrnTdfCod  = 1;

      /end-free
     p LoadTdfValue    e
       //===================================================================
       // ProcOther - process other options
       //===================================================================
     p ProcOther       b
     d ProcOther       pi

     d  inRRN          s              9  0
     d  inMode         s              1
      /free
       // Edit/Delete Territorial Differential Factor
       Readc sflTerDif;
       Dow not %eof;
          Select;
          When scTdfEdt = *On;
             scTdfEdt = *Off;
             inRRN  = scTdfRRN#;
             inMode = 'C';
             EdtAddDltTdf(inRRN:inMode);
          When scTdfDlt = *On;
             scTdfDlt = *Off;
             inRRN  = scTdfRRN#;
             inMode = 'E';
             EdtAddDltTdf(inRRN:inMode);
          EndSl;

          Readc sflTerDif;
       Enddo;

      /end-free
     p ProcOther       E

       //*** Beg Add ***** 08/19/19 *****************************
       //===================================================================
       // SetCriteria
       //===================================================================
     p SetCriteria     b
     d SetCriteria     pi

      /Free
       GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');

       //Select Statement
       carrSelect = 'DIGITS(F2CAR#)';

       If grpFrom = 'WMFNP';
          carrFrom='WMfnp join WDf2p on FNco#=F2co# and FNfnd=F2fnd';
          carrWhere = grpWhere;
       Else;
          carrFrom='WDF2P join SMAZP A on F2co#=AZco# and F2fnd=AZfnd '  +
                    %trim(%subst(grpFrom:50)) +
                    ' join WMFNP on F2co#=FNco# and F2fnd=FNfnd';
          carrWhere = %trim(grpWhere) + ' and F2car# <> 0';
       Endif;

      /End-Free

     p SetCriteria     e
       //*** End Add ***** 08/19/19 *****************************
