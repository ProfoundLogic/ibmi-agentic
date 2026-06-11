     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/title WTCOMCHK - Check Schedule Maintenance
     h dftname(WTCOMCHK)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h/define   PROFOUNDUI
     h bnddir('WTBNDDIR')
     h**********************************************************************
    *====================================================================*
    *  Created    : 08/05/14                                             *
    *  Programmer : C:53                                                 *
    *  Project    : 114WNC                                               *
    *  Description: Check Schedule Maintenance                           *
    *                                                                    *
    *====================================================================*
    *  Revisions:                                                        *
      *                                                                    *
      *  03/21/22 C:333 Project: SY434                                     *
      *                 - Update Grids for Sorting Filtering Upgrade       *
      *                                                                    *
      *  07/20/20 C:079 Project: WC2749                                    *
      *                 Add Starting EFT and E-Pay# and E-Pay Vendor       *
      *                                                                    *
      *  05/05/17 C:306 Project: WC2408                                    *
      *                 Check Schedule Clean-up                            *
      *                                                                    *
      *  01/07/16 C:65  Project: WC2401                                    *
      *                 Recompile.                                         *
      *                                                                    *
      *  01/08/16 C:310 Support: 39099                                     *
      *                          Modified code to display only the most    *
      *                          recent 500 records.                       *
      *  09/17/15 C:302 Project: wc2254                                    *
      *                 Recompile only                                     *
      *                                                                    *
    *                                                                    *
    * ------------------------------------------------------------------ *
    *  NOTE: Please add comments to the top of Revisions.                *
    * ------------------------------------------------------------------ *
    *====================================================================*

     fwtcomchk  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(sflNote:rrnNote)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fwmupl     if   e           k disk    rename(pup01:pup00)
     fwacpl5    if   e           k disk    rename(pcp01:pcp00)
     f                                     infds(dbfds)
     fwacpp     uf a e             disk    rename(pcp01:pcp0p)
     f*** Beg Add *** 07/20/20 *********************************************
     fwsckl     if   e           k disk
     f*** End Add *** 07/20/20 *********************************************
     fssylf     if   e           k disk
     f                                     ignore(ssy01)
     f                                     ignore(spc01)
     f                                     ignore(sfb01)
     f                                     ignore(src01)
     f                                     ignore(sdr01)
     f                                     ignore(sbc01)
     f                                     ignore(scs01)
     f                                     ignore(sgl01)
     f                                     ignore(sp201)

      * data structure
     d psds          esDs                  extname(szq1p)
     d dbfds           ds
     d  dbrrn                397    400b 0

     d groupDA         ds             1    dtaara(group)
     d  groupflag              1      1

     d svLda           ds          1024    dtaara(*LDA)

      * work variables
     d beginDate       s              7  0 inz(0)
     d clientId        s              3    inz('000')
     d endDate         s              7  0 inz(0)
     d fold            s              1    dim(255)
     d isError         s               n   inz(*off)
     d isReload        s               n   inz(*off)
     d isEdi           s               n   inz(*off)
     d isEft           s               n   inz(*off)
     d intFrDt         s              7  0
     d intToDt         s              7  0
     d process         s               n   inz(*off)
     d policyLevel     s              1    inz(' ')
     d svFiltStat      s            100    inz(' ')
     d svFiltUser      s             10    inz(' ')
     d svFiltFnd       s              3  0 inz(0)
     d svFiltBgDt      s              8  0 inz(0)
     d svFiltEnDt      s              8  0 inz(0)
     d svFiltType      s              1    inz(' ')
     d svFiltRenw      s              1    inz(' ')
     d systemFlag      s              1    inz(' ')
     d userEmail       s            100    inz(' ')
     d urlRRN          s              9
     d urlName         s             50
     d xxRrn           s              9  0 inz(0)
     d x#Rrn           s              9  0 inz(0)
     d xxCO#           s                   like(UPco#)
     d xxCall          s              1    inz(' ')
     d xxStatus        s              1    inz(' ')
     d xxFold          s            243
     d xxDcid          s             12
     d y               s              3  0

     d kkPrdt          s                   like(CPPrdt)
     d kkFnd           s                   like(CPFnd)
     d kkAcct          s                   like(CPAcct)
     d kkCktp          s                   like(CPCktp)
     d Program         s             10

     d scntkey         s             30
     d scdtlkey        s             30
     d isGroup         s               n   inz(*off)
     d isNonGrp        s               n   inz(*off)
     d isMeetFlt       s               n   inz(*off)
     d isFltrErr       s               n   inz(*off)
     d intCurDate      s              8  0 inz(0)
     d str9999Msg      s            256
     d OutGrpchk       s              1a   inz(*blanks)
     d OutCompany      s              3a   inz(*blanks)
     d OutStartDate    s              7a   inz(*blanks)
     d OutEndDate      s              7a   inz(*blanks)
     d OutGroup        s              3a   inz(*blanks)
     d dsChkFltr       ds                  likerec(RcdChkFltr)
     d                                     inz
     d*** Beg Add *** 07/20/20 ***********************************
     d isEPay          s               n   inz(*off)
     d*** End Add *** 07/20/20 ***********************************

     d* Proto-types

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

      * System Flag API
     d GetSystemFlag   pr                  extpgm('SA046R')
     d   outSysFlag                   1

      * Group Select
     d GroupSelect     pr                  extpgm('W7944C')
     d   Program                     10

      * Program Start
     d ProgramStart    pr
     d processMe                       n

     d* Main Processing
     d Main            pr

     ** Clear check schedule filter
     d loadClrFlt      pr

     ** Meet Check schedule filter
     d MeetFilter      pr

     ** Purge Process Procedure prototype.
     d PurgeProcess    pr

     ** Purge Process Submitter prototype.
     d PurgeSubmitter  pr                  extpgm('WTPRGCHKR')
     d  OutGrpchk                     1
     d  OutCompany                    3
     d  OutStartDate                  7
     d  OutEndDate                    7
     d  OutGroup                      3


     d/copy sCopyCompr
     d/copy sprCompany
     d/copy sprDate
     d/copy sprGenPop
     d/copy sprNotice
     d/copy sPrAtrium
     d/copy wPrUsrPrms
     d/copy wPrEmpLr
     d*** Beg Add *** 07/20/20 ************************************
     d/copy sPrModule
     d*** End Add *** 07/20/20 ************************************

     c     keyWACPL5     KList
     c                   KFld                    xxCo#
     c                   KFld                    kkPrDt
     c                   KFld                    kkFnd
     c                   KFld                    kkAcct
     c                   KFld                    kkCkTp

      *   Key List for the User Parameter File
     c     keyWMUPL      Klist
     c                   Kfld                    q1User

     c     key1WMUPL     Klist
     c                   Kfld                    scFiltUser

      /free

       ProgramStart(process);
       Main();

       *inLr = *On;
      /end-free

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
     d processMe                       n
      /free

       kkFnd  = *loval;
       kkPrDt = *hival;
       kkAcct = *loval;
       kkCkTp = *loval;

       // check if daily or group check schedule maintenance
       IN GroupDA;
       If Groupflag = 'Y';
          isGroup = *on;
       Else;
          isNonGrp = *on;
       Endif;

       // client id
       GetClientID(clientID);

       Chain keyWMUPL pup00;
       If not %found or UPco# = *zero;
          processMe = *off;
          Return;
       Endif;

       xxCo# = UPco#;
       scCo# = UPco#;

       // Get system flags
       Chain 'WC' swc01;

       // Claims EFT flag - Controls payment type display
       If WCfl35 = 'Y';
          isEFT = *On;
       EndIf;

       // EDI flag
       If WCfl09 = 'Y';
          isEDI = *On;
       EndIf;

       TabHeight  = 850;
       APnlHeight = TabHeight + 60;
       inquiry    = *off;
       isReload   = *on;

       scActRrn   = xxRrn;
       scActCltid = clientId;
       scEnv      = ' ';
       scInvokeUP = 'Y';


       If isEDI = *on;
          scChkHdg = 'Process Date, Group, Bank Code, Payment Type, Starting +
           Check #, EDI Mailing Date, Printed, Processed,';
       Else;
          scChkHdg = 'Process Date, Group, Bank Code, Payment Type, Starting +
           Check #, , Printed, Processed,';
       Endif;

       clear dsChkFltr;

      // Purge button will be display only when user has 999 * security.
       isPurgeVis = *off;
       If GetUsrLvl(q1User) = 999 and GetUsrType(q1User) = '*';
          isPurgeVis = *on;
       EndIf;

       //*** Beg Add *** 07/20/20 ************************************
       isEPay = isModEnabled('E-Pay');
       //*** End Add *** 07/20/20 ************************************

       processMe  = *on;
       Return;

      /end-free
     p ProgramStart    e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
      /free

       // Load screen fields
       LoadActionBar();

       // Process until exit button is pressed.
       DoU exitNow = *On;

          // Load screen fields
          If isReload = *on;
             Exsr LoadRecords;
             isReload = *off;
          Endif;

          // Process policy detail screen.
          Exsr ExfmtScreens;

          //Perform actions on screen.
          Select;

          // Exit the program
          When exitNow = *on;
             Leave;

          // Refresh the screen
          When BtnRefresh = *on;
             btnRefresh = *off;
             Chain keyWMUPL PUP00;
             xxCO# = UPCO#;
             scCO# = UPCO#;
             LoadActionBar();
             Exsr LoadRecords;

          // Add button
          When RecordAdd = *on;
             RecordAdd = *off;
             Exsr AddRecord;

         // Check schedule filter button
          When isBtnFiltr = *On;
               isBtnFiltr = *off;
               Exsr ProcFilter;

         // filter clear processing
          When isBtnClFlt = *on;
             clear dsChkFltr;
             clear rcdChkFltr;
             dspclrfltr = *off;
             isReload = *on;

         // Purge button
          When isBtnPurge = *On;
               isBtnPurge = *Off;
               PurgeProcess();

          Other;
             // Check for clicked links on grid.
             Readc sflNote;
             Dow not %eof();

                // Update the record.
                If RecordUpd = *on;
                   RecordUpd = *off;
                   Exsr UpdateRecord;
                   isReload = *on;
                Endif;

                // Delete the record.
                If RecordDel = *on;
                   RecordDel = *off;
                   Exsr DeleteRecord;
                   isReload = *on;
                Endif;

                selNote = *on;
                Update sflNote;
                Readc sflNote;
             Enddo;

          Endsl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Write rcdComPnl;
       Exfmt rcdPanel;
       Read rcdComPnl;

       Endsr;
       //===================================================================
       // Delete the record
       //===================================================================
       BegSr DeleteRecord;

       Chain scNtRRN pcp0p;
       If %found;

          scDelPrdt = CvtToDate(CPPrDt);
          scDelFnd = CPfnd;
          scDelCkTp = CPCkTp;
          scDelCmDt =  CvtToDate(CPCmDt);
          scDelMAcct = ' ';
          scDelEAcct = ' ';
          //*** Beg Del *** 07/20/20 ***************************************
          //***scDelChk# =  CPChk#;
          //*** End Del *** 07/20/20 ***************************************
          scDelFrDt =  dtMDYY(CPfrDt);
          scDelToDt =  dtMDYY(CPToDt);
          //*** Beg Add *** 07/20/20 ***************************************
          Clear scDelEfTc;
          Clear scDelChk#;
          Clear scDelEFT#;
          Clear scDelEpay#;
          Clear scDelMAcct;
          Clear scDelEAcct;
          Clear scDelVAcct;
          //*** End Add *** 07/20/20 ***************************************

          Select;
          When CPEftC = 'E';
             scDelEfTc = 'EFT';
          //*** Beg Add *** 07/20/20 ***************************************
             scDelEft# =  scEFT#;
          //*** End Add *** 07/20/20 ***************************************
             scDelEAcct = CPAcct;
          When CPEftC = ' ';
          //*** Beg Add *** 07/20/20 ***************************************
             scDelChk# =  CPchk#;
          //*** End Add *** 07/20/20 ***************************************
             scDelEfTc = 'MICR';
             scDelMAcct = CPAcct;
          When CPEftC = 'B';
             scDelEfTc = 'BOTH';
          //*** Beg Add *** 07/20/20 ***************************************
             scDelChk# =  CPchk#;
             scDelEft# =  scEFT#;
          //*** End Add *** 07/20/20 ***************************************
             scDelMAcct = CPAcct;
             scDelEAcct = CPAcct;
          //*** Beg Add *** 07/20/20 ***************************************
          When CPEftC = 'V';
             scDelEfTc = 'E-Pay';
             scDelEpay#=  CPePay#;
             scDelVAcct = CPAcct;
          When CPEftC = 'A';
             scDelChk# =  CPchk#;
             scDelEft# =  scEFT#;
             scDelEpay#=  CPePay#;
             scDelEfTc = 'All';
             scDelMAcct = CPAcct;
             scDelEAcct = CPAcct;
             scDelVAcct = CPAcct;
          //*** End Add *** 07/20/20 ***************************************
          Endsl;

          Clear btnCancelD;
          Clear btnDelete;

          Dow btnCancelD <> *on and btnDelete <> *on;
             Exfmt rcdDelete;

             If btnDelete = *on;
                If %found;
                   Delete pcp0p;
                Endif;
             Endif;
          Enddo;

       Endif;

       Endsr;
       //===================================================================
       // Add the record
       //===================================================================
       BegSr AddRecord;

       Program = 'WTCOMCHK  ';
       GroupSelect(Program);
       isReload = *on;

       Endsr;
       //===================================================================
       // Update the record.
       //===================================================================
       BegSr UpdateRecord;

       Chain scNtRRN pcp0p;
       If %found;

          scUpdPrdt  = CvtToDate(CPPrDt);
          scUpdFnd   = CPfnd;
          scUpdCkTp  = CPCkTp;
          scUpdCmDt  = CvtToDate(CPCmDt);
          scUpdMAcct = ' ';
          scUpdEAcct = ' ';
          //*** Beg Del *** 07/20/20 **************************************
          //***scUpdChk#  = CPChk#;
          //*** End Del *** 07/20/20 **************************************
          scUpdFrDt  = dtMDYY(CPFrDt);
          scUpdToDt  = dtMDYY(CPToDt);
          //*** Beg Add *** 07/20/20 ***************************************
          Clear scUpdEfTc;
          Clear scUpdChk#;
          Clear scUpdEFT#;
          Clear scUpdEpay#;
          Clear scUpdMAcct;
          Clear scUpdEAcct;
          Clear scUpdVAcct;
          //*** End Add *** 07/20/20 ***************************************
          Select;
          When CPEftC = 'E';
             scUpdEfTc = 'EFT';
          //*** Beg Add *** 07/20/20 ***************************************
             scUpdEFT#  = scEFT#;
          //*** End Add *** 07/20/20 ***************************************
             scUpdEAcct = CPAcct;
          When CPEftC = ' ';
             scUpdEfTc = 'MICR';
          //*** Beg Add *** 07/20/20 ***************************************
             scupdChk# = CPchk#;
          //*** End Add *** 07/20/20 ***************************************
             scUpdMAcct = CPAcct;
          //*** Beg Chg *** 07/20/20 ***************************************
          //***Other;
          When CPeftC = 'B';
          //*** End Chg *** 07/20/20 ***************************************
             scUpdEfTc = 'BOTH';
          //*** Beg Add *** 07/20/20 ***************************************
             scUpdChk#  = CPChk#;
             scUpdEFT#  = scEFT#;
          //*** End Add *** 07/20/20 ***************************************
             scUpdMAcct = CPAcct;
             scUpdEAcct = CPAcct;
          //*** Beg Add *** 07/20/20 ***************************************
          When CPEftC = 'V';
             scUpdEfTc = 'E-Pay';
             scUpdEpay# = CPePay#;
             scUpdVAcct = CPAcct;
          When CPEftC = 'A';
             scUpdEfTc = 'All';
             scUpdChk#  = CPchk#;
             scUpdEFT#  = scEFT#;
             scUpdEpay# = CPePay#;
             scUpdMAcct = CPAcct;
             scUpdEAcct = CPAcct;
             scUpdVAcct = CPAcct;
          //*** End Add *** 07/20/20 ***************************************
          Endsl;

          btnCancelU = '0';
          btnSaveU   = '0';

          Dow btnSaveU = *off or (btnSaveU = *on and isError = *on);
             Exfmt rcdUpdate;
             erUpdFrDt = *off;
             erUpdToDt = *off;

             If btnCancelU = *on;
                btnCancelU = *off;
                Leave;
             Endif;

             If btnSaveU = *on;
                Exsr Validate;
                If isError = *off;
                   Chain scNtRRN pcp0p;
                   If %found;
                      CPChk#   = scUpdChk#;
                      CPCMDt   = Dateto7(scUpdCmDt);
                      scChk#   = scUpdChk#;
                      scMailDt = scUpdCmDt;
                      CPfrDt   = dtCYMD(scUpdFrDt);
                      CPToDt   = dtCYMD(scUpdToDt);

                      CPPgm = Q1PGM;
                      CPUser = Q1User;
                      CPDt   = Dateto7(SystemDate());
                      CPTime = Timeto6(SystemTime());
                      Update pcp0p;
                   Endif;
                Endif;
             Endif;

          Enddo;
       Endif;

       Endsr;

       //===================================================================
       // Load Records
       //===================================================================
       BegSr LoadRecords;

       // Clear the subfile.
       dspNote = *off;
       dspNoteCtl = *off;
       clrNote = *on;
       Write rcdPanel;
       clrNote = *off;
       dspNote = *on;
       dspNoteCtl = *on;
       rrnNote = 0;

       Clear scPrdt;
       Clear scFnd;
       Clear scDtlKey;
       Clear scDtlCkTp;
       Clear scDtlPmtTy;
       Clear scDtlAcctM;
       Clear scDtlAcctE;
       Clear scDtlMail;
       Clear scDtlStChk;
       Clear scDtlPrint;
       Clear scDtlUpdat;
       Clear scDtlFrmDt;
       Clear scDtlToDt;

       // Load Filter Clear Button.
       loadClrFlt();

       // Claims EFT flag - Controls payment type display
       If WCfl35 = 'Y';
          isEFT = *On;
       EndIf;

       // EDI flag
       If WCfl09 = 'Y';
          isEDI = *On;
       EndIf;

       If scFiltUser <> ' ';
          Chain key1WMUPL pup00;
          If %found;
             userEmail = UPMail;
          Endif;
          Chain keyWMUPL pup00;
       Endif;

       Setll keyWACPL5 pcp00;
       Read pcp00;

       Dow not %eof;
       //*** Beg Add *** 07/20/20 ************************************
          Clear scChk#;
          Clear scEFT#;
          Clear scEpay#;
          Clear scEpayVn;
       //*** End Add *** 07/20/20 ************************************
          If rrnNote >= 9999;

               str9999msg = 'Your search returned too many records. ' +
                'Click Continue to see the first 9999 records. ' +
                'Click Filter on the display screen to narrow your '+
                'search results.';

               PopOneBtn(str9999msg:'Message':'Continue');

             leave;
          else;

          ismeetFlt = *on;
         // Validate for filter
          MeetFilter();
          If ismeetFlt = *on;


          scNtKeyRRN = dbrrn;

          scPrdt   = CvtToDate(CPPrDt);
          scFnd    = CPfnd;
          scCkTp   = CPCkTp;
          scMailDt = CvtToDate(CPCmDt);
          scAcct   = CPAcct;
          //*** Beg Del *** 07/20/20 ***************************************
          //*****scChk#   = CPChk#;
          //*** End Del *** 07/20/20 ***************************************
          //*** Beg Add *** 07/20/20 ***************************************
          Chain (CPco#:CPacct) pck01;
          If %Found(WSCKL);
             Select;
             When CPeftC = ' ';
                scChk#   = CPchk#;
             When CPeftC = 'E';
                scEFT#   = CKeft#;
             When CPeftC = 'B';
                scChk#   = CPchk#;
                scEFT#   = CKeft#;
                scEpay#  = *zeros;
             When CPeftC = 'V';
                scEpay#  = CKePay#;
                scEpayVn = CKePayVn;
             When CPeftC = 'A';
                scChk#   = CPchk#;
                scEft#   = CKeft#;
                scEpay#  = CKePay#;
                scEpayVn = CKePayVn;
             Endsl;
          Endif;
          //*** End Add *** 07/20/20 ***************************************

          inquiry = *off;
          If CPSta0 = 'Y';
             scPrinted = 'Yes';
             inquiry = *on;
          Else;
             scPrinted = 'No';
          Endif;
          If CPSta1 = 'Y';
             scUpdated = 'Yes';
             inquiry = *on;
          Else;
             scUpdated = 'No';
          Endif;

          Select;
          When CPEftC = 'E';
             scPmtType = 'EFT';
             scAcctEFT = scAcct;
             scAcctMICR = ' ';
          //*** Beg Add *** 07/20/20 ***************************************
             scAcctEPay = *blanks;
          //*** End Add *** 07/20/20 ***************************************
          When CPEftC = ' ';
             scPmtType = 'MICR';
             scAcctMICR = scAcct;
             scAcctEFT = ' ';
          //*** Beg Add *** 07/20/20 ***************************************
             scAcctEPay = *blanks;
          //*** End Add *** 07/20/20 ***************************************
          //*** Beg Chg *** 07/20/20 **********************************
          //***Other;
          When CPeftC = 'B';
          //*** End Chg *** 07/20/20 **********************************
             scPmtType = 'BOTH';
             scAcctMICR = scAcct;
             scAcctEFT = scAcct;
          //*** Beg Add *** 07/20/20 ***************************************
             scAcctEPay = *blanks;
          //*** End Add *** 07/20/20 ***************************************
          //*** Beg Add *** 07/20/20 **********************************
          When CPeftC = 'V';
             scPmtType = 'E-Pay';
             scAcctEPay = scAcct;
             scAcctMICR = *blanks;
             scAcctEFT = *blanks;
          When CPeftC = 'A';
             scPmtType = 'All';
             scAcctMICR = scAcct;
             scAcctEFT = scAcct;
             scAcctEPay = scAcct;
          //*** End Add *** 07/20/20 **********************************
          Endsl;

          scNtRRN  = dbrrn;
          rrnNote  =  rrnNote + 1;

          If rrnNote = 1;
             selNote = *On;
          Else;
             selNote = *Off;
          Endif;

          Write sflNote;
          Endif;

          Read  pcp00;
          Endif;
       Enddo;

       // Position grid at top at all times.
       If rrnNote <> 0;
          rrnNote = 1;
       Endif;

       Endsr;

       //===================================================================
       // Validate
       //===================================================================
       Begsr Validate;

       isError = *off;
       clear intFrDt;
       clear intToDt;

       If isGroup = *on;
          // If From date is valid.
          intFrDt = DtCYMD(scUpdFrDt);
          If intFrDt = 0;
             msUpdFrDt = 'WCR0500';
             erUpdFrDt = *on;
             isError = *on;
          Endif;

          // If To date is valid.
          intToDt = DtCYMD(scUpdToDt);
          If intToDt = 0;
             msUpdToDt = 'WCR0500';
             erUpdToDt = *on;
             isError = *on;
          Endif;

          // If only To date entered.
          If erUpdFrDt = *off and erUpdToDt = *off and scUpdFrDt = 0 and
           scUpdToDt > 0;
             msUpdFrDt = 'WCR2246';
             erUpdFrDt = *on;
             isError = *on;
          Endif;

          // If only From date entered.
          If erUpdFrDt = *off and erUpdToDt = *off and scUpdFrDt > 0 and
           scUpdToDt = 0;
             msUpdToDt = 'WCR2247';
             erUpdToDt = *on;
             isError = *on;
          Endif;

          // If From date > To date
          If erUpdFrDt = *off and erUpdToDt = *off and scUpdFrDt > 0 and
           scUpdToDt > 0 and scUpdFrDt > scUpdToDt;
             msUpdFrDt = 'WCR1502';
             msUpdToDt = 'WCR1502';
             erUpdFrDt = *on;
             erUpdToDt = *on;
             isError = *on;
          Endif;
       Endif;

       Endsr;

      //===================================================================
      // Process Filter.
      //===================================================================
       BegSr ProcFilter;
         Dou BtnChkAply = *on or BtnChkCan = *on;
             ExFmt RcdChkFltr;
             If BtnChkAply = *on;

                msChkStrDt = *blanks;
                erChkStrDt = *off;
                msChkEndDt = *blanks;
                erChkEndDt = *off;
                isFltrErr  = *off;

                If scChkStrDt > *zeros and dtCheck(scChkStrDt) = *zeros;
                   BtnChkAply = *off;
                   msChkStrDt = 'WCR0500';
                   erChkStrDt = *On;
                   isFltrErr  = *On;
                Endif;

                If scChkEndDt > *zeros and dtCheck(scChkEndDt) = *zeros;
                   BtnChkAply = *off;
                   msChkEndDt = 'WCR0500';
                   erChkEndDt = *On;
                   isFltrErr  = *On;
                Endif;

                If isFltrErr = *off and dtCheck(scChkStrDt) <> *zeros and
                 dtCheck(scChkEndDt) <> *zeros and
                 dtYYMD(dtCheck(scChkEndDt)) < dtYYMD(dtCheck(scChkStrDt));
                   BtnChkAply = *off;
                   msChkStrDt = 'WCR0529';
                   erChkStrDt = *On;
                   msChkEndDt = 'WCR0529';
                   erChkEndDt = *On;
                   isFltrErr  = *On;
                EndIf;

                If isFltrErr  = *Off;
                   dsChkFltr.scChkStrDt = scChkStrDt;
                   dsChkFltr.scChkEndDt = scChkEndDt;
                   dsChkFltr.scChkFnd   = scChkFnd;
                   dsChkFltr.scChkProcs = scChkProcs;
                   isReload = *on;
                Endif;
             Endif;

             If BtnChkCan = *on;
                scChkStrDt = dsChkFltr.scChkStrDt;
                scChkEndDt = dsChkFltr.scChkEndDt;
                scChkFnd   = dsChkFltr.scChkFnd;
                scChkProcs = dsChkFltr.scChkProcs;
             Endif;
         Enddo;
       Endsr;

      /end-free

     p Main            e
       //***************************************************************************//
     P loadClrFlt      B
     D loadClrFlt      PI

      /free

     // display clear filter button
       If dsChkFltr.scChkStrDt = *zeros and
          dsChkFltr.scChkEndDt = *zeros and
          dsChkFltr.scChkFnd   = *blanks and
          dsChkFltr.scChkProcs = *blanks;
          dspclrfltr = *off;
       Else;
          dspclrfltr = *on;
       Endif;

      /End-free
     P loadClrFlt      E

       //==================================================================
       // MeetPolFlt - Determine if the records meet the Filter criteria
       //==================================================================
     P MeetFilter      b
     d MeetFilter      PI
      /free

      // Check Group number.
       If scChkFnd <> *Blanks;
          If %dec(scChkFnd:3:0) <> CPfnd;
             ismeetFlt = *off;
          Endif;
       Endif;

      // Processed flag (Yes).
       If scChkProcs = 'Y' and scChkProcs <> CPsta1;
          ismeetFlt = *off;
       Endif;

      // Processed flag (No).
       If scChkProcs = 'N';
          If CPsta1 <> *blanks;
             ismeetFlt = *off;
          Endif;
       Endif;

      // Check date range
       If dtCheck(scChkStrDt) <> *zeros and dtCheck(scChkEndDt) <> *zeros;

          If dtCYMD(%dec(scChkStrDt:8:0)) > CPprDt Or
           dtCYMD(%dec(scChkEndDt:8:0)) < CPprDt;
             ismeetFlt = *off;
          Endif;
       Endif;

      /end-free
     P MeetFilter      e

      //==================================================================
      // Purge Process Procedure.
      //==================================================================
     P PurgeProcess    b
     d isPurgErr       s               n   inz(*off)
     d isPurgExit      s               n   inz(*off)
     d isoDate         s               d
     d intdate         s              8  0
     d intDateDif      s              8  0
     d intPurgeCnt     s              8  0
     d intStartDate    s              7  0
     d intEndDate      s              7  0
     d intGroup        s              3  0

      /free

       clear RcdCkPurge;
       BtnPrgCan  = *off;
       isPurgExit = *off;

       Isodate = %Date();
       IntDate = %Int( %Char( Isodate : *USA0 ) );
       intDateDif = dtPast(IntDate:365);

       Dow isPurgExit = *off and BtnPrgCan = *off;
          ExFmt RcdCkPurge;

          Select;
             When BtnPrgSubm = *on;     // Purge submit button.
                 BtnPrgSubm = *off;
                 msPrgStrDt = *blanks;
                 erPrgStrDt = *off;
                 msPrgEndDt = *blanks;
                 erPrgEndDt = *off;

                 msPrgEndD2 = *blanks;
                 erPrgEndD2 = *off;
                 isPurgErr  = *off;

                 If ((scPrgStrDt > *zeros and dtCheck(scPrgStrDt) = *zeros))
                    or dtCheck(scPrgStrDt) = *zeros;
                    msPrgStrDt = 'WCR0500';
                    erPrgStrDt = *On;
                    isPurgErr  = *On;
                 Endif;

                 If (scPrgEndDt > *zeros and dtCheck(scPrgEndDt) = *zeros)
                    Or dtCheck(scPrgEndDt) = *zeros;
                     msPrgEndDt = 'WCR0500';
                     erPrgEndDt = *On;
                     isPurgErr  = *On;
                 Endif;

                 If isPurgErr = *off and dtCheck(scPrgStrDt) <> *zeros and
                  dtCheck(scPrgEndDt) <> *zeros and
                  dtYYMD(dtCheck(scPrgEndDt)) < dtYYMD(dtCheck(scPrgStrDt));
                    msPrgStrDt = 'WCR0529';
                    erPrgStrDt = *On;
                    msPrgEndDt = 'WCR0529';
                    erPrgEndDt = *On;
                    isPurgErr  = *On;
                 EndIf;


                 If isPurgErr = *off and
                  dtYYMD(intDateDif) < dtYYMD(dtCheck(scPrgEndDt));

                    msPrgEndD2 = 'Ending Date more recent than 1' +
                     ' year prior to the Current Date.';
                    erPrgEndD2 = *On;
                    isPurgErr  = *On;
                 EndIf;

                 If Not isPurgErr;
                    ExSr Confirmation;
                    If isPurgExit = *on;
                       isReload = *on;
                    Endif;
                 Endif;

             When BtnPrgCan = *on;    //Cancel button
                 BtnPrgCan = *off;
                 Leave;

          EndSl;

       Enddo;

      // Purge Confirmation subroutine.
       BegSr Confirmation;
          scConStrDt = scPrgStrDt;
          scConEndDt = scPrgEndDt;
          scConPgFnd = scPurgeFnd;
          Dou BtnConPSb = *On or BtnConPCan = *On;
              ExFmt RcdConfPur;
              Select;
                 When BtnConPSb = *On;
                    ExSr PurgeSbm;
                    isPurgExit = *on;

                 When BtnConPCan = *On;
                    Leave;

              EndSl;
           EndDo;
       EndSr;

      // Purging Submitter subroutine.
       BegSr PurgeSbm;

          OutGrpchk    = Groupflag;
          OutCompany   = %editc(xxCO#:'X');
          OutStartDate = %editc(DateTo7(cvtToDate(scPrgStrDt)):'X');
          OutEndDate   = %editc(DateTo7(cvtToDate(scPrgEndDt)):'X');
          OutGroup  = scPurgeFnd;

          PurgeSubmitter(OutGrpchk:OutCompany:OutStartDate:OutEndDate:OutGroup);

       EndSr;

      /End-free
     P PurgeProcess    e

      * utility procedure definitions
      /copy sCopyComPI
