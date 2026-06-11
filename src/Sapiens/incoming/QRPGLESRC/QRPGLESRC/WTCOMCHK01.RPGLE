     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/title WTCOMCHK01 - Process Group Add for Check Schedule Maintenance
     h dftname(WTCOMCHK01)
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
    * ------------------------------------------------------------------ *
    *  NOTE: Please add comments to the top of Revisions.                *
    * ------------------------------------------------------------------ *
      *                                                                    *
      * 07/20/20 C:79 Project: WC2749                                      *
      *               Add E-Pay and All three to Payment Type drop down    *
      *                                                                    *
      * 01/04/17 c:62 Ticket 44937(90)                                     *
      *               Correct date compare                                 *
      *                                                                    *
    *====================================================================*

     fwtcomchk01cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(sflErrMsg:rrnErrMsg)
     fwmupl     if   e           k disk    rename(pup01:pup00)
     fwacpl5    if   e           k disk    rename(pcp01:pcp00)
     fwacpp     uf a e             disk    rename(pcp01:pcp0p)
     fwacpl4    if   e           k disk    rename(pcp01:pcp04)
     fwxgsp     if   e             disk    rename(pgs01:pgs0p)
     fwxgsl     if   e           k disk    rename(pgs01:pgs0l)
      *  Group master
     fwmfnl     if   e           k disk    rename(pfn01:pfn0l)
      *  Group Auxiliary      key=CO#/GRP
     fwdf2l     if   e           k disk    rename(pf201:pf20l)
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
     fwmcxl     if   e           k disk
      *  Bank Account Master  co#/acct#
     fwsckl     if   e           k disk

     d pdDs          esDs                  extname(szq1p)

     d groupDA         ds             1    dtaara(group)
     d  groupflag              1      1

     d bnkCdDA         ds            10    dtaara(bnkCd)
     d  BankCode               1     10

      * work variables
     d clientID        s              3    inz('000')
     d intCo#          s                   like(UPco#)
     d isError         s               n
     d isNotVld        s               n
     d isGroup         s               n   inz(*off)
     d isEdi           s               n   inz(*off)
     d isEft           s               n   inz(*off)
     d intFrDt         s              7  0
     d intToDt         s              7  0
     d intFnd          s                   like(CPFnd)
     d intEDIm         s              5  0
     d outCo#          s              3
     d outDti          s              8
     d outSrd          s              5
     d outMod          s              1
     d outDto          s              8
     d outRtcd         s              1
     d strKey          s              2

     d #2eftc          s              1
     d #2rtcd          s              1
     d #2fyr           s              3
     d #2case          s              7
     d #2data          s             75
     d #2coid          s             20
     d #2chkq          s             10
     d #2ftyp          s             10
     d #2hdcy          s              1
     d #2cpyq          s             10
     d #2efdt          s              7
     d #2Acct          s                   like(CPAcct)
     d #2Appl          s              2
     d #2Call          s              1
     d #2CkTp          s                   like(CPCkTp)
     d #2Co#           s              3
     d #2Fnd           s              3
     d #2SrDt          s              8
     d*** Beg Add *** 07/20/20 *********************************************
     d isEPay          s               n   inz(*off)
     d*** End Add *** 07/20/20 *********************************************

      * Proto-types

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

      * Next Working Day Date
     d GetNextWorkD    pr                  extpgm('WA072R')
     d   outco#                       3
     d   outdti                       8
     d   outsrd                       5
     d   outmod                       1
     d   outdto                       8
     d   outrtcd                      1

      * Program Start
     d ProgramStart    pr              n

      * Main Processing
     d Main            pr

     d/copy sCopyCompr
     d/copy sprCompany
     d/copy sprDate
     d/copy sprGenPop
     d/copy sprNotice
     d/copy sPrAtrium
     d/copy wPrUsrPrms
     d/copy wPrEmpLr
     d/copy wPrBank
     d*** Beg Add *** 07/20/20 *********************************************
     d/copy sPrModule
     d*** End Add *** 07/20/20 *********************************************

     c     *entry        plist
     c                   parm                    @@errFnd          1

      /free

       If ProgramStart() = *on;
          Main();
       Endif;

       *inLr = *On;
       Return;
      /end-free

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi              n

     d processMe       s               n
      /free

       isGrpChk = *off;
       intCo# = UserCo#(q1user);
       GetClientID(clientID);

       // Group or non group
       IN GroupDA;
       If Groupflag = 'Y';
          isGroup  = *on;
          isGrpChk = *on;
       Endif;

       IN bnkCdDA;

       // Get system flags
       strKey = 'WC';
       Chain strKey swc01;

       // Claims EFT flag - Controls payment type display
       If WCfl35 = 'Y';
          isEFT = *On;
       EndIf;

       // EDI flag
       If WCfl09 = 'Y';
          isEDI = *On;
       EndIf;
       //Beg Add *** 07/20/20 **********************************************
       // E-Pay Module - Controls payment type display
       isEPay = isModEnabled('E-Pay');
       If isEFT or isEPay;
          isEFTEpay = *on;
       Else;
          isEFTEpay = *off;
       Endif;

       // Setup the drop down choices and values depending on EFT and E-Pay install flags
       Select;
       When isEft and isEpay;
          PtChoices = 'MICR,EFT,MICR and EFT, E-Pay, All';
          PtValues = ' ,E,B,V,A';
       When isEft and Not isEpay;
          PtChoices = 'MICR,EFT,MICR and EFT';
          PtValues = ' ,E,B';
       When Not isEft and isEpay;
          PtChoices = 'MICR,E-Pay,MICR and E-Pay';
          PtValues = ' ,V,P';
       Endsl;
       //End Add *** 07/20/20 **********************************************

       processMe = *off;
       If q1prms >= 1;
          processMe = *on;
       Endif;

       Return processMe;
      /end-free
     p ProgramStart    e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
      /free

       Clear scAddPrDt;
       Clear scAddFrDt;
       Clear scAddToDt;
       Clear scAddEfTc;
       scAddCkTp = BankCode;

       btnCancelA = '0';
       btnSaveA   = '0';

       Dow btnSaveA = *off or (btnSaveA = *on and isError = *on);

          Exfmt rcdAdd;
          erAddPrDt = *Off;
          erAddFrDt = *Off;
          erAddToDt = *Off;
          erAddCkTp = *Off;
          isError   = *Off;

          // Cancel
          If btnCancelA = *on;
             btnCancelA = *off;
             Leave;
          Endif;

          // Save
          If btnSaveA = *on;
             Exsr Validate;
             If isError = *off;
                If isNotVld = *off;
                   Exsr SaveRecord;
                Else;
                   If Q1Prms > 0;
                      @@errFnd = 'Y';
                   Endif;
                Endif;
             Endif;
          Endif;
       Enddo;


       //===================================================================
       // Retrieve Bank Accout#
       //===================================================================
       BegSr RetrieveBankAcc#;

       scAcct = *Blanks;
       scAcce = *Blanks;
       //*** Beg Add *** 07/20/20 ******************************************
       scAccV = *Blanks;
       //*** End Add *** 07/20/20 ******************************************
       #2appl = 'WC';
       #2acct = *Blanks;
       #2hdcy = *Blanks;
       #2chkq = *Blanks;
       #2ftyp = *Blanks;
       #2rtcd = *Blanks;
       #2efdt = *Zeros ;
       #2cpyq = *Zeros ;
       #2coid = *Blanks;
       #2eftc = scAddEftc ;
       #2co#  = %Editc(intco#:'X');
       #2fnd  = %EditC(GSfnd:'X');
       #2srdt = %EditC(scAddPrDt:'X');
       #2cktp = scAddCkTp;

       // Retrieve the check/voucher flag from the Group Aux file.
       #2call = *blank;
       Chain (intCo#:GSfnd) pf20l;
       If %found(wdf2l);
          Select;
          When ClientID = '114';
             If F2Ctyp <> *blank;
                #2call = F2Ctyp;
             Else;
                #2call = 'C';
             Endif;
          Other;
             If F2Ctyp = 'V';
                #2call = 'V';
             Else;
                #2call = 'C';
             Endif;
          Endsl;
       Endif;

       Select;
       When #2eftc = *blank;
          #2acct= GetBankAcc(#2co#: #2fnd: #2appl: #2srdt: #2call:#2hdcy:
           #2chkq: #2ftyp: #2efdt: #2cpyq:#2cktp: #2eftc: #2coid: #2rtcd);
          If #2Acct <> ' ' ;
             scAcct = #2acct;
          Else;
             // No MICR Account exist
             scErrMsg = 'The group is not set up for MICR.';
             Exsr writeRec;
          Endif;

       When #2eftc = 'E';
          #2acct= GetBankAcc(#2co#: #2fnd: #2appl: #2srdt: #2call:#2hdcy:
           #2chkq: #2ftyp: #2efdt: #2cpyq:#2cktp: #2eftc: #2coid: #2rtcd);
          If #2Acct <> ' ' ;
             scAcce = #2acct;
          Else;
            // No EFT Account exist
             scErrMsg = 'The group is not up for EFT.';
             Exsr writeRec;
          Endif;

       When #2eftc = 'B';
         // Check for MICR set up
          #2eftc = *blank;
          #2acct= GetBankAcc(#2co#: #2fnd: #2appl: #2srdt: #2call:#2hdcy:
           #2chkq: #2ftyp: #2efdt: #2cpyq:#2cktp: #2eftc: #2coid: #2rtcd);
          If #2acct <> ' ';
             scAcct = #2Acct;
          Else ;
             scErrMsg = 'The group is not set up for MICR.';
             Exsr writeRec;
          EndIf;
          #2eftc = 'E';
          #2acct= GetBankAcc(#2co#: #2fnd: #2appl: #2srdt: #2call:#2hdcy:
           #2chkq: #2ftyp: #2efdt: #2cpyq:#2cktp: #2eftc: #2coid: #2rtcd);
          If #2Acct <> ' ' ;
             scAcce = #2acct;
          Else;
             scErrMsg = 'The group is not up for EFT.';
             Exsr writeRec;
          Endif;
         // Restore original value of eft flag
          #2eftc = 'B';

       //*** Beg Add *** 07/20/20 ******************************************
       When #2eftc = 'V';
         // Check for E-Pay set up
          #2eftc = 'V';
          #2acct= GetBankAcc(#2co#: #2fnd: #2appl: #2srdt: #2call:#2hdcy:
           #2chkq: #2ftyp: #2efdt: #2cpyq:#2cktp: #2eftc: #2coid: #2rtcd);
          If #2acct <> ' ';
             scAccv = #2Acct;
          Else ;
             scErrMsg = 'The group is not set up for E-Pay.';
             Exsr writeRec;
          EndIf;
       When #2eftc = 'A';
         // Check for All three (MICR, EFT and E-Pay)
         // First, check for MICR setup
          #2eftc = ' ';
          #2acct= GetBankAcc(#2co#: #2fnd: #2appl: #2srdt: #2call:#2hdcy:
           #2chkq: #2ftyp: #2efdt: #2cpyq:#2cktp: #2eftc: #2coid: #2rtcd);
          If #2acct <> ' ';
             scAcct = #2Acct;
          Else ;
             scErrMsg = 'The group is not set up for MICR.';
             Exsr writeRec;
          EndIf;
         // Check for EFT setup
          #2eftc = 'E';
          #2acct= GetBankAcc(#2co#: #2fnd: #2appl: #2srdt: #2call:#2hdcy:
           #2chkq: #2ftyp: #2efdt: #2cpyq:#2cktp: #2eftc: #2coid: #2rtcd);
          If #2acct <> ' ';
             scAcce = #2Acct;
          Else ;
             scErrMsg = 'The group is not set up for EFT.';
             Exsr writeRec;
          EndIf;
         // Check for E-Pay setup
          #2eftc = 'V';
          #2acct= GetBankAcc(#2co#: #2fnd: #2appl: #2srdt: #2call:#2hdcy:
           #2chkq: #2ftyp: #2efdt: #2cpyq:#2cktp: #2eftc: #2coid: #2rtcd);
          If #2acct <> ' ';
             scAccV = #2Acct;
          Else ;
             scErrMsg = 'The group is not set up for E-Pay.';
             Exsr writeRec;
          EndIf;
         // Restore original value of eft flag
          #2eftc = 'A';
       When #2eftc = 'P';
         // Check for (MICR and E-Pay)
         // First, check for MICR setup
          #2eftc = ' ';
          #2acct= GetBankAcc(#2co#: #2fnd: #2appl: #2srdt: #2call:#2hdcy:
           #2chkq: #2ftyp: #2efdt: #2cpyq:#2cktp: #2eftc: #2coid: #2rtcd);
          If #2acct <> ' ';
             scAcct = #2Acct;
          Else ;
             scErrMsg = 'The group is not set up for MICR.';
             Exsr writeRec;
          EndIf;
         // Check for E-Pay setup
          #2eftc = 'V';
          #2acct= GetBankAcc(#2co#: #2fnd: #2appl: #2srdt: #2call:#2hdcy:
           #2chkq: #2ftyp: #2efdt: #2cpyq:#2cktp: #2eftc: #2coid: #2rtcd);
          If #2acct <> ' ';
             scAccV = #2Acct;
          Else ;
             scErrMsg = 'The group is not set up for E-Pay.';
             Exsr writeRec;
          EndIf;
         // Restore original value of eft flag
          #2eftc = 'P';
       //*** End Add *** 07/20/20 ******************************************
       Endsl;

       EndSr;

       //===================================================================
       // Validate
       //===================================================================
       Begsr Validate;

       isError   = *off;
       isNotVld  = *off;
       clear intFrDt;
       clear intToDt;

       intFrDt = DtCYMD(scAddPrDt);
       If intFrDt = 0;
          msAddPrDt = 'WCR0500';
          erAddPrDt = *on;
          isError = *on;
       Endif;

       If isGrpChk = *on;
          clear intFrDt;
          // If From date is valid.
          intFrDt = DtCYMD(scAddFrDt);
          If intFrDt = 0;
             msAddFrDt = 'WCR0500';
             erAddFrDt = *on;
             isError = *on;
          Endif;

          // If To date is valid.
          intToDt = DtCYMD(scAddToDt);
          If intToDt = 0;
             msAddToDt = 'WCR0500';
             erAddToDt = *on;
             isError = *on;
          Endif;

          // If only To date entered.
          If erAddFrDt = *off and erAddToDt = *off and scAddFrDt = 0 and
           scAddToDt > 0;
             msAddFrDt = 'WCR2246';
             erAddFrDt = *on;
             isError = *on;
          Endif;

          // If only From date entered.
          If erAddFrDt = *off and erAddToDt = *off and scAddFrDt > 0 and
           scAddToDt = 0;
             msAddToDt = 'WCR2247';
             erAddToDt = *on;
             isError = *on;
          Endif;

          // If From date > To date
          If erAddFrDt = *off and erAddToDt = *off and scAddFrDt > 0 and
           scAddToDt > 0 and intFrDt > intToDt;
             msAddFrDt = 'WCR1502';
             msAddToDt = 'WCR1502';
             erAddFrDt = *on;
             erAddToDt = *on;
             isError = *on;
          Endif;
       Endif;

       // Validte all groups
       If isError = *off;
          intFnd = 0;
          #2co#  = %Editc(intco#:'X');
          #2fyr  = *zeros;
          #2case = *zeros;
          #2appl = 'WC';

          dspErrMsg = *off;
          clrErrMsg = *on;
          Write rcdEdit;
          dspErrMsg = *on;
          clrErrMsg = *off;
          rrnErrMsg = 0;
          btnOk = *off;

          Setll (intCo#:intFnd) pgs0l;
          Read pgs0l;
          Dow not %eof(wxgsl);

             If GSiefl = 'I';
                #2fnd  = %Editc(GSfnd:'X');
                #2data = *Blanks;
                #2cktp = scAddCkTp;
                #2eftc = scAddEftc;
                scGrpErr = GSFnd;
                scErrMsg = ' ';

                // Group must not be expired
                Chain (intCo#:GSfnd) pfn0l;
                If %found(wmfnl) and FNExDt > 0 and erAddPrDt = *off and
                 FNExDt < DtCYMD(scAddPrDt);
                   scErrMsg = 'The Process date is > than the Group expiration +
                    date.';
                   Exsr writeRec;
                Endif;

                If Not ValidBankCode(#2co#: #2fnd: #2fyr: #2case: #2appl:
                 #2data: #2cktp: #2eftc);
                   scErrMsg = 'Bank Code is not valid for this group.';
                   Exsr writeRec;
                Else;
                Exsr RetrieveBankAcc#;
                EndIf;


                intFrDt = DtCYMD(scAddPrDt);
                Chain (intCo#:intFrDt:#2Acct:GSfnd:scAddCkTp) pcp04;
                If %found(wacpl4);
                   scErrMsg = 'Duplicate record found.';
                   Exsr writeRec;
                Endif;

                Chain (intCo#:scAcct) pck01;
                //*** Beg Chg *** 08/03/20 *********************************
                //***If %found(wsckl) and scAddEftC <> 'E' and CKlChk = 0;
                If %found(wsckl) and scAddEftC <> 'E' and scAddEftc <> 'V'
                   and CKlChk = 0;
                //*** End Chg *** 08/03/20 *********************************
                   scErrMsg = 'Starting check# is zero for +
                    MICR payment type.';
                   Exsr writeRec;
                Endif;
             Endif;

             Read pgs0l;
          Enddo;

          If rrnErrMsg > 0;
             rrnErrMsg = 1;
             Exfmt rcdEdit;
             isNotVld = *on;
          Endif;
       Endif;

       EndSr;
       //===================================================================
       // SaveRecord
       //===================================================================
       Begsr SaveRecord;

       Clear pcp0p;
       CPco#  = intCo#;
       CPPrDt = dtCYMD(scAddPrDt);
       CPFrDt = dtCYMD(scAddFrDt);
       CPToDt = dtCYMD(scAddToDt);
       CPCkTp = scAddCkTp;

       //*** Beg Chg *** 07/20/20 ******************************************
       //***If isEFT;
       If isEFT or isEpay;
       //*** End Chg *** 07/20/20 ******************************************
          CPEftC = scAddEfTc;
       Else;
          CPEftC = *blank;
       Endif;

       // Add Group Processing
       CPPgm = q1Pgm;
       CPUser = q1User;
       CPDt   = Dateto7(SystemDate());
       CPTime = Timeto6(SystemTime());

       intFnd = 0;
       Setll (intCo#:intFnd) pgs0l;
       Read pgs0l;
       Dow not %eof;

          If GSiefl = 'I';
             CPfnd = GSfnd;

             // If EDI mailing date is zero and it is add mode, default to
             // working date(CXEDIM+Processing date) after processing date
             If isEDI = *on;
                // Get EDI mailing date increment #
                intEDIm = *zeros;
                Chain (intCo#:GSfnd) pcx01;
                If %found;
                   intEDIm = CXEDIm;
                Endif;

                // Get EDI Mailing date
                outCo#  = %editc(intco#:'X');
                outDti  = %editc(scAddPrDt:'X');
                outSrd  = %editc(intEDIm:'X');
                outMod  = '2';
                outDto  = *blanks;
                outRtcd = *blank;

                GetNextWorkD(outCo#:outDti:outSrd:outMod:outDto:outRtcd);
                CPCmDt = dtCYMD(%int(outDtO));
             Endif;

             // add setup (114 mod)
             Exsr RetrieveBankAcc#;

             CPAcct = scAcct;
             Chain (intCo#:CPAcct) pck01;
             //*** Beg Chg *** 07/20/20 ******************************
             //***If %found;
             If %found and (scAddEFTc = ' ' or scAddEFTc = 'B' or
               scAddEFTc = 'A' or scAddEFTc = 'P');
             //*** End Chg *** 07/20/20 ******************************
                CPChk# = CKLChk;
             Endif;

             If scAcct <> ' ' and scAcct <> scAcce;
                CPeftc =  ' ';
             //*** Beg Add *** 07/20/20 *****************************
             Else;
                If scAcct <> ' ' and scAcct = scAcce and scAcct <> scAccV
                 and scAddEFTc = 'A';
                   CPefTc = 'B';
                Endif;
             //*** End Add *** 07/20/20 *****************************
             Endif;

             If CPAcct <> ' ';
                Write pcp0p;
             Endif;

             If CPacct <> scAcce and scAcce <> ' ';
                CPAcct = scAcce;
                CPAcct = scAcce;
                Chain (intCo#:CPAcct) pck01;
                CPeftc = 'E';
                CPChk# = CKeft#;
                Write pcp0p;
             EndIf;
             //*** Beg Add *** 07/20/20 *****************************
             If CPacct <> scAccV and scAccV <> ' ' or (CPacct = scAccV
              and scAccV <> ' ' and scAddEFTc = 'P');
                CPAcct = scAccV;
                CPAcct = scAccV;
                Chain (intCo#:CPAcct) pck01;
                CPeftc = 'V';
                CPePay# = CKePay#;
                Write pcp0p;
             EndIf;
             //*** End Add *** 07/20/20 *****************************
          Endif;

          Read pgs0l;
       Enddo;
       IN *lock bnkCdDA;
       BankCode = ' ';
       OUT bnkCdDA;

       EndSr;
       //===================================================================
       // WriteRec
       //===================================================================
       BegSr WriteRec;

       isNotVld = *on;
       rrnErrMsg = rrnErrMsg + 1;
       Write sflErrMsg;

       Endsr;
       //===================================================================
      /end-free
     p Main            e
       //***************************************************************************//

      * utility procedure definitions
      */Copy sCopyComPI
