     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/title STEPAYVMNT - E-Pay Vendor Maintenance Program
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program Will Allow Setup of E-pay Vendors
     h*
     h*  Called from: Operation Drop Down Menu (Sapiens Use Only)
     h*  Calls to: None
     h*
     h*  Created on 7/20/2020 by C:079 for Project: WC2749
     h**********************************************************************
     h*  Revisions:
     h*
     h* 03/21/22 C:333 Project: SY434                                 *
     h*               - Update Grids for Sorting Filtering Upgrade    *
     h*                                                               *
     h**********************************************************************

     fstEpayVmntcf   e             workstn extfile('STEPAYVMNT')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflEpayVnd:rrnEpayVnd)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fwmevml1   uf a e           k disk    InfDs(dbfds1)
     fwmevmp    uf a e             disk    rename(pevm01:pevm02)
     fwtabl     uf a e           k disk    rename(pb201:pb200)
     f***smcol     if   e           k disk    rename(pco01:pco00)
     fsmupl     if   e           k disk
     fwtgtl     if   e           k disk    rename(pgt01:pgt00)
     fssylf     if   e           K disk
     f                                     IGNORE(SSY01)
     f                                     IGNORE(SPC01)
     f                                     IGNORE(SRC01)
     f                                     IGNORE(SBC01)
     f                                     IGNORE(SCS01)

     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  dbrrn1               397    400b 0

     d intCo#          s              3  0
     d ##rrn           s              9  0
     d clientID        s              3    inz('000')
     d sys_date        s               D   datfmt(*USA)
     d sys_time        s               T
     d isValid         s               n
     d  ##CltId        s              3
     d isEdit          s               n
     d strMessage      s            256
     d xxDate7         s              7  0

     d* Main procedure
     d stEpayVmntprms  pr                    Extpgm('STEPAYVMNT')
     d inMode                         1
     d inTabIndex                     5  0

     d stEpayVmntprms  pi
     d inMode                         1
     d inTabIndex                     5  0

     d* Clear Error Indicators
     d ClearErrors     pr

     d* Clear Subfiles
     d ClrSubFiles     pr

     d* Edit/Add Vendor
     d EditAddVend     pr
     d  rrn                           9  0

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3

     d* Invalid Password popup
     d InvalidPwd      pr

     d* Load payment value grid
     d LoadEpayVendor  pr

     d* Main Processing
     d Main            pr

     d* Program Start
     d ProgramStart    pr

     d* Process other options
     d ProcOther       pr

     d* Program end
     d ProgramEnd      pr

     d* Save record to database file
     d SaveRecord      pr

     d* Validate screen fields
     d Validate        pr


     d/copy sPrDate
     d/copy sPrString
     d/copy SPrCompany
     d/copy wPrGroup
     d/copy wPrUsrPrms
     d/copy sprModule
     d/copy sPrSecure
     D/copy sPrGenPop
    
      /free
       // Start Of Program

       ProgramStart();

       TabHeight = 550;
       Write rcdComPnl;
       Write rcdTabEpay;
       Exfmt rcdKeyCode;

       // Validate Key Code
       If not Valid_KeyCode(scKeyCode);
          strMessage = 'Invalid Key Code entered. Security key required. +
          Program will close.';
          InvalidPwd();
          *inlr = *on;
          Return;
       Endif;

       // Main Procesing
       Main();

       *inlr = *on;
       return;
      /end-free
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /free

       // Process until exit button is pressed.
       Dou ExitNow = *On ;

          // Clear Sub-files
          ClrSubfiles();

          // Load E-Pay Vendors grid
          LoadEpayVendor();

          // Process E-Pay Vendor Maintenance Screen
          ExfmtScreens();

          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When BtnRefresh = *On;
             BtnRefresh =*Off;

          // Add Vendor
          When BtnVendAdd = *on;
             isEdit = *off;
             ##rrn = 0;
             EditAddVend(##rrn);
             BtnVendAdd =*Off;

          Other;

             ProcOther();

          Endsl;

       Enddo;
      /End-free
     p Main            e
       //===================================================================
       // ClearErrors - Clears Error Indicators
       //===================================================================
     p ClearErrors     b
     d ClearErrors     pi
      /Free

       errVendor  = *off;
       errVenFmt  = *off;
       errVenPt   = *off;
       errVenPt2  = *off;
       errVenAppl = *off;
       errVenEfDt = *off;
       errVenExDt = *off;
       errInfld   = *off;
       errOutFld1 = *off;
       errOutFld2 = *off;
       errOrig    = *off;
       errTPA     = *off;

      /End-Free
     p ClearErrors     e
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi
      /Free

       rrnEpayVnd = 0;
       clrEpayCod  = *On;
       dspEpaySub  = *Off;
       dspEpayCtl  = *Off;
       Write rcdEpayVnd;

      /End-Free
     p ClrSubfiles     e
       //===================================================================
       // EditAddVend - Edit and Add Vendors
       //===================================================================
     p EditAddVend     b
     d EditAddVend     pi
     d inRRN                          9  0
      /Free
       If BtnVendAdd = *on;
          Clear rcdAddEdit;
          isEdit = *off;
       Else;
          Chain(n) inRRN pevm02;
          If %Found(WMEVMP);
             isEdit = *on;
             scAddVnNm  = EVMvend;
             scAddVnFmt = EVMfmt;
             scAddVnPty = EVMpTyp;
             scAddVappl = EVMappl;
             scAddVefDt = DtMdYY(EVMefDt);
             scAddVexDt = DtMdYY(EVMexDt);
             scAddInFld = EVMiFldFil;
             scAddOutF1 = EVMoFldFil;
             scAddOutF2 = EVMoFldAtt;
             scAddOrig  = EVMorigin;
             scAddTPA   = EVMtpaCode;
          Endif;
       Endif;
       Dou BtnAddCan = *on;
          ExFmt rcdAddEdit;
          Select;
             When BtnAddSav = *on;
                BtnAddSav = *off;
                Validate();
                If isValid = *on;
                   SaveRecord();
                   Leave;
                Endif;
             When BtnAddCan = *on;
                BtnAddCan = *off;
                Clear rcdAddEdit;
                Leave;
             Other;
          Endsl;
       Enddo;
      /End-Free
     p EditAddVend     e
       //===================================================================
       // Invalid Password Pop Up
       //===================================================================
     p InvalidPwd      b
     d InvalidPwd      pi
      /Free
       PopOneBtn(strMessage:'*** Invalid Password ***');
      /End-free
     p InvalidPwd      e
       //===================================================================
       // Write Screen Formats;
       //===================================================================
     p ExFmtScreens    b
     d ExFmtScreens    pi
      /Free

       Select;

       When TabIndex = 0;
          TabHeight = 550;
          Write rcdComPnl;
          Write rcdTabEpay;
          Exfmt rcdEpayVnd;
          Read rcdComPnl;
          Read rcdTabEpay;
       EndSl;

      /End-free
     p ExFmtScreens    e

       //===================================================================
       // Load E-Pay Vendor grid
       //===================================================================
     p LoadEpayVendor  b
     d LoadEpayVendor  pi
      /Free

       // clear the subfile.
       clear sflEpayVnd;
       clrEpayCod = *Off;
       dspEpaySub = *On;
       dspEpayCtl = *On;
       clear dbRrn1;

       If inMode <> 'U';
          dspMode = *on;
       Else;
          dspMode = *off;
          dspVisB = *on;
       Endif;

       Setll (*blanks:*blanks:*loval) pevm01;
       Read pevm01;
       dow not %eof(WMEVML1);

             // Write E-Pay Vendor Maintance Data

             // E-Pay Vendor
             scEpayVend = EVMvend;

             // E-Pay Vendor Format
             Chain (intCo#:'EPFM') pgt00;
             If %Found(WTGTL);
                scEpVnFmt = %Trim(EVMfmt) + ' - ' + %Trim(GTdesc);
             Else;
                scEpVnFmt = *blanks;
             Endif;

             // E-Pay Payment Type
             Chain (intCo#:'EPPT':EVMpTyp) pgt00;
             If %Found(WTGTL);
                scEpVnPt = %Trim(EVMpTyp) + ' - ' + %Trim(GTdesc);
             Else;
                scEpVnpt = *blanks;
             Endif;

             // E-Pay Vendor Effective Date
             scEpVnEfDt = dtMDYY(EVMefDt);

             // E-Pay Vendor Expiration Date
             scEpVnExDt = dtMDYY(EVMexDt);

             // E-Pay Vendor Inbound File Folder Path
             scEPInbFld = EVMiFldFil;

             // E-Pay Vendor Outbound Payment File Folder Path
             scEOutFld1 = EVMoFldFil;

             // E-Pay Vendor Outbound Document Folder Path
             scEOutFld2 = EVMoFldAtt;


             rrnEpayVnd = rrnEpayVnd + 1;
             scEpayRrn# = dbRrn1;
             If rrnEpayVnd = 1;
                scEpyRowSl = 'A';
             Else;
                clear scEpyRowSl;
             Endif;

             Write sflEpayVnd;
          Read pevm01;
       Enddo;

       rrnEpayVnd = 1;

      /End-free
     p LoadEpayVendor  e
       //===================================================================
       // ProcOther - process other options
       //===================================================================
     p ProcOther       b
     d ProcOther       pi
      /Free
       // Edit Vendor
          Readc sflEpayVnd;

       Dow not %eof;
          If scVendEdt = *on;
             scVendEdt = *off;
             ##rrn = scEpayRrn#;
             EditAddVend(##rrn);
          Endif;

             Readc sflEpayVnd;
       Enddo;

      /End-free
     p ProcOther       E
       //===================================================================
       // ProgramEnd - End the Program
       //===================================================================
     p ProgramEnd      b
     d ProgramEnd      pi
      /Free
       *inlr = *on;
       Return;
      /End-free
     p ProgramEnd      E
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free


       // Load Action panel with the Company Name.
       clear scCompName;
       Chain Q1user pup01;
       If %Found(SMUPL);
          intCo# = UPco#;
       Endif;
       // Get company name
       scCompName = GetCompNam(intCo#);
       aPnlHeight = 610;

       // Get the client id
       GetClientID(##CltId);

       tabClick = *Off;
       ##rrn = 0;
       Return;

      /End-free
     p ProgramStart    e
       //===================================================================
       // SaveRecord - Save Screen to Database File
       //===================================================================
     p SaveRecord      b
     d SaveRecord      pi
      /Free
       Sys_date = %Date();
       Sys_Time = %Time();

       // Add the record
       If isEdit <> *on;
          Clear pevm02;
          EVMvend    = scAddVnNm;
          EVMfmt     = scAddVnFmt;
          EVMpTyp    = scAddVnPty;
          EVMappl    = scAddVappl;
          EVMefDt    = DtCyMd(scAddVefDt);
          EVMexDt    = DtCyMd(scAddVexDt);
          EVMiFldFil = %Trim(scAddInFld);
          EVMoFldFil = %Trim(scAddOutF1);
          EVMoFldAtt = %Trim(scAddOutF2);
          EVMorigin  = scAddOrig;
          EVMtpaCode = scAddTPA;
          // Maintenance fields
          EVMpgm  = Q1pgm;
          EVMuser = Q1User;
          EVMdt   = DateTo7(Sys_Date);
          EVMtime = Timeto6(Sys_Time);
          EVMiPgm = EVMpgm;
          EVMiUsr = EVMuser;
          EVMiDt  = EVMdt;
          EVMiTim = EVMtime;
          Write pevm02;
       Endif;
       // Update the existing record
       If isEdit = *on;
          Chain scEpayRrn# pevm02;
          If %Found(WMEVMP);
             EVMexDt    = DtCyMd(scAddVexDt);
             EVMiFldFil = %Trim(scAddInFld);
             EVMoFldFil = %Trim(scAddOutF1);
             EVMoFldAtt = %Trim(scAddOutF2);
             EVMorigin  = scAddOrig;
             EVMtpaCode = scAddTPA;
            // Maintenance fields
             EVMpgm  = Q1pgm;
             EVMuser = Q1User;
             EVMdt   = DateTo7(Sys_Date);
             EVMtime = Timeto6(Sys_Time);
             Update pevm02;
          Endif;
       Endif;
      /End-Free
     p SaveRecord      e
       //===================================================================
       // Validate - Validate Screen
       //===================================================================
     p Validate        b
     d Validate        pi
      /Free
       ClearErrors();
       isValid = *on;
       // Vendor cannot be blanks
       If scAddVnNm = *blanks;
           isValid = *off;
           errVendor = *on;
       Endif;
       // Vendor format cannot be blanks
       If scAddVnFmt = *blanks and isValid = *on;
           isValid = *off;
           errVenFmt = *on;
       Endif;
       // Vendor payment type cannot be blanks for WC Application
       If scAddVnPty = *blanks and scAddVappl = 'WC' and isValid = *on;
           isValid = *off;
           errVenPt = *on;
       Endif;
       // Vendor application code cannot be blanks
       If scAddVappl = *blanks and isValid = *on;
           isValid = *off;
           errVenAppl = *on;
       Endif;
       // If Vendor Application Code <> WC Payment Type must be
       // blank
       If scAddVappl <> 'WC' and scAddVnPty <> ' ' and isValid = *on;
           isValid = *off;
           errVenPt2 = *on;
           msgVenPt2 = 'Payment Type is only allowed for WC Application';
       Endif;
       // Effective date must be a valid date and must be entered
       If DtCheck(scAddVEfDt) = 0 and isValid = *on;
           isValid = *off;
           errVenEfDt = *on;
       Endif;
       // Expiration date must be a valid date if entered
       If DtCheck(scAddVExDt) = 0 and isValid = *on and scAddVExDt <> 0;
           isValid = *off;
           errVenExDt = *on;
       Endif;
       // Inbound files folder path cannot be blank
       If scAddInFld = *blanks and isValid = *on;
           isValid = *off;
           errInFld = *on;
       Endif;
       // Outbound files folder path 1 cannot be blank
       If scAddOutF1 = *blanks and isValid = *on;
           isValid = *off;
           errOutFld1 = *on;
       Endif;
       // Outbound files folder path 2 cannot be blank
       If scAddOutF2 = *blanks and isValid = *on;
           isValid = *off;
           errOutFld2 = *on;
       Endif;
       // Vendor VPay specific Edits
       Select;
          When scAddVnNm = 'VPAY';
             If scAddOrig = *blanks and isValid = *on;
                isValid = *off;
                errOrig = *on;
             Endif;
             If scAddTPA = *blanks and isValid = *on;
                isValid = *off;
                errTPA = *on;
             Endif;
       Endsl;
       // Check for Duplicate entry on Add
       If isValid = *on and isEdit = *off;
          xxDate7 = DtCyMd(scAddVefDt);
          Chain(n) (scAddVnNm:scAddVnPty:xxDate7) pevm01;
          If %Found(WMEVML1);
             isValid = *off;
             strMessage = 'Record already exists with same Vendor, Format, +
                           Type and Effective Date';
             PopOneBtn(strMessage:'*** Duplicate Record ***');
          Endif;
       Endif;

      /End-free
     p Validate        E
