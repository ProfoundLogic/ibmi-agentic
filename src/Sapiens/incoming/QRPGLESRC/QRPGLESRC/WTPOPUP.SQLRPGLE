     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H BNDDIR('WTBNDDIR')
     H*====================================================================
     H/TITLE WTPOPUP - API to process/call popup window programs/procedures.
     H*====================================================================
     H*  Description:
     H*
     H*  This program is a gateway to call WCMS programs/procedures which
     H*  display popup windows.  It accepts a standard set of parameters:
     H*     40A - Program/Procedure/Process Name
     H*    9.0S - Relative Record #
     H*      2A - Mode (type of calling screen to interpret RRN)
     H*    500A - Data (flat field that can be parsed via data structure)
     H*
     H*  Processes:
     H*
     H*     'Documents' - Call Scriptura Department Letters API
     H*
     H*     'Copy Employer' - Call Copy Employer API
     H*
     H*
     H*
     H*
     H*  Created:  01/07/16  C:65  Project: WC2401
     H*====================================================================
     H*  Revisions:
     h*
     h*  08/01/24 C:291 Project: WC2976
     h*                 Add call to Audit Reversal from Audit toolbar
     h*
     h*  03/04/23 C:340 Project: WC2430
     h*                 Display Claim Payment, Single Payment & Recovery
     h*                 Tracking as Window.
     h*
     H*  06/02/23 C:300 Support: 110463
     h*                 Rename 'Copy Configuration' toolbar option to
     h*                 'Rating Factor Copy'
     h*
     H*  06/21/22 C:300 Project: WC2196
     h*                 Add call to Change FEIN from Agency Maintenance toolbar
     h*
     H*  01/17/22 C:291 Project: WC2788
     h*                 Add call to Copy Configuration from Group Configuration
     H*
     h*    08/18/21  C:33   Support: 93938
     h*                     Load the rrn for the request for later processing.
     H*
     H*  05/03/21 C:062 Project:WC2663
     H*                 Add Third Party quote submission
     H*
     H*  12/16/19 C:180 Project:150WEH
     H*                 Add update audit payrolls.
     H*
     H*  11/28/19 C:298 Project: WC2696
     H*                 Self-Reporting Worksheet-Scriptura/PolicyGO
     H*
     H*  03/29/19 C:136 Project: 150WDW
     H*                 Remove 150WBV per client request.
     H*
     H*  08/08/18 C:33  Project : C150WCS
     H*           C:62  Add call Billing programs.
     H*                  'Audit Bill'
     H*                  'Estimated Bills'
     H*                  'Quote Bill'
     H*
     H*  06/21/18 C:291 Project: WC2600
     h*                 Allow Dept Letter access based on Application
     H*                 Config setup.
     H*
     H*  04/17/18 C:291 Project : WC2593
     H*                 Pass mode from the 1st position of the data parm
     H*                 to WtCertHoldr program.
     H*
     H*  04/06/18 C:291 Project : C150WBV
     H*                 Add call for Billing Invoice for SUNZ.
     H*
     H*  03/12/18 C:33  Support: 64765
     H*                 Check Documents for inquiry mode.
     H*
     H*  05/22/17 C:309 Project : WC2434
     H*                 To call Final Audit Bill as popup from Policy Tool bar.
     H*
     H*  10/13/16 C:307 Project : WC2400
     H*                 To call Policy Page screen as popup from
     H*                 Policy Tool bar.
     H*
     H*  11/19/16 C:300 Project : WC2456
     H*                 To call Late Notice Report - Est screen as popup
     H*                 from Policy Tool bar.
     H*
     H*  11/18/16 C:310 Project : WC2435
     H*                 To call Certificate Holders screen as popup from
     H*                 Policy Tool bar.
     H*
     H*  10/25/16 C:306 Project : WC2436
     H*                 To call Policy Renewal screen as popup from
     H*                 Policy Tool bar.
     H*
     H*  10/11/16 C:310 Project : WC2401
     H*                 To call Premium Notice report screen as popup from
     H*                 Policy Tool bar.
     H*
     H*  06/23/16 C:308 Support:42154
     H*                 To call Payment history report screen as popup from
     H*                 claim tool bar.
     H*====================================================================
     fstscp     if   e             disk    rename(psc01:psc0p)
      *** Beg Add *** 03/04/23 *** WC2430 *************************************
     Fwmcmp     if   e             disk    rename(pcm01:pcm0p)
      *** End Add *** 03/04/23 *** WC2430 *************************************

     d xxQte           s              1
     d***  Begin Add  ***  08/18/21  ***********************************
     d decCount        s              9  0 inz
     d***  End   Add  ***  08/18/21  ***********************************
     d inCall          s              1A
     d inRRn           s              9A
     d inRRn1          s              9A
     d inProcess       s             40A
     d inRRn2          s              9A
     d inKC            s              1A
     d inKL            s              1A
     d inPrds          s             40A
     d inEmpNum        s              9
     d inRtc           s              1A
     d inCo#           s              3
     d inFnd           s              3
     d inFyr           s              3
     d inMode          s              1A
     d clientID        s              3A   Inz
     d str999Msg       s            256A   Inz
     d*** Begin Add *** 06/21/22 ****************************************
     d @@kl            s              1
     d @@lc            s              1
     d @@mode          s              1
     d @@rrn2          s              9  0
     d*** End   Add *** 06/21/22 ****************************************

     d***  Begin Add  ***  08/18/21  ***********************************
     D psds          esds                  ExtName(szq1p)

     DDelayJob         pr            10u 0 extproc('sleep')
     D interval                      10u 0 value
     d***  End   Add  ***  08/18/21  ***********************************

     d*** Begin Add *** 05/03/21 ****************************************
      * Quote Bill
     d QuoteBill       pr                  ExtPgm('W8001C')
     d chrCo#                         3A
     d chrFnd                         3A
     d chrFyr                         3A
     d chrEmp#                        9A
     d chrDiv                         5A
     d chrDate                        8A
     d chrEsCd                        2A
     d chrCom1                        2A
     d chrCom2                        2A
     d chrCom3                        2A
     d chrDue                         8A
     d chrDep                         1A
     d chrStl                         1A
     d chrPrt                         1A
     d chrOvr                         1A
     d chrSrCd                        2A
     d chrAjCd                        2A
     d chrQte                         1A
     d chrQue                        10A
     d chrRnb                         1A
     d chrDart                        1A
     d chrHard                        1A
     d chrAgto                        1A
     d chrFQt                         1A
     d chrRmCd                        5A
     d chrAR                          2A
     d chrEfDt                        8A
     d chrFCm1                       30A
     d chrFCm2                       30A
     d chrFCm3                       30A
     d chrAR2                         2A
     d*** End   Add *** 05/03/21 ****************************************

      * Payment History Report
     d WtrHstRpt       pr                  ExtPgm('WTRPTPHST')
     d inProcess                     40A
     d inCall                         1A
     d inRRn                          9A

      * Premium Notice Report
     d WtrPremNtc      pr                  ExtPgm('WTRPTPRNTC')
     d inProcess                     40A
     d inCall                         1A
     d inRRn                          9A

     d WtPolPgSbmR     pr                  Extpgm('WTPOLPGSBM')
     d  inPrds                       40A   const
     d  inCall                        1A
     d* inEmpNum                      9
     d  inrrn1                        9
     d  inRtc                         1A
     d  inCo#                         3
     d  inFnd                         3
     d  inFyr                         3

      * Policy Renewal
     d WtPolRen        pr                  ExtPgm('WTPOLREN')
     d inProcess                     40A
     d inCall                         1A
     d inRrn                          9A

      * Certificate Holders
     d WtCertHoldr     pr                  ExtPgm('WTRPTCERTH')
     d inKC                           1A
     d inKL                           1A
     d inCall                         1A
     d inrrn                          9A
     d inrrn2                         9A

      * Late Notice Report - Est
     d WtLatNotEst     pr                  ExtPgm('WTRPTLTNOT')
     d inProcess                     40A
     d inCall                         1A
     d inRrn                          9A

      * Final Audit Bill
     d WtPolAudBl      pr                  ExtPgm('WTPOLAUDBL')
     d inKC                           1A
     d inKL                           1A
     d inMode                         1A
     d inRrn                          9A

      * Estimated Billing
     d WtPolEstBl      pr                  ExtPgm('WTPOLESTBL')
     d  inCall                        1A
     d  inQte                         1A
     d  inKC                          1A
     d  inKL                          1A
     d  inRrn                         9A

      * Retrieve Client ID
     d RtvCltID        pr                  ExtPgm('RTVCLTID')
     d  clientID                      3a

     d*** Begin Add *** 06/21/22 ****************************************
      // Change FEIN
     d ChangeFEIN      pr                  extpgm('W4020R')
     d @@kl                           1
     d @@lc                           1
     d @@mode                         1
     d @@rrn2                         9  0
     d*** End   Add *** 06/21/22 ****************************************

      * Call to Billing Programs
     d Bills           pr
     d  billType                     10a   Const

     d SRWkSbm         pr                  Extpgm('W7220R')
     d  inPrds                       40A   const
     d  inCall                        1A
     d  inRrn                         9A

      * Audit with S/R
     d AuditWithSR     pr                  ExtPgm('WTPOLAUDSR')
     d  audRrn                        9  0

      //*** Beg Add ***** 08/01/24 *****************************************
      * Audit Reversal
     d PolAudRvs       pr                  ExtPgm('WTPOLAUDRV')
     d  audRvsRrn                     9  0
      //*** End Add ***** 08/01/24 *****************************************

     d/copy sPrGenPop
     d/copy sPrLetter
     d/copy wPrPolCpy
     d/copy sPrTable
     d*** Begin Add *** 05/03/21 ****************************************
     d/copy sPrDate
     d/copy sPrModule
     d/copy sPrSQL
     d/copy wPrEmplr
     d*** Beg Add ***** 01/17/22 *************************************
     d/copy wPrGrpPop
     d*** End Add ***** 01/17/22 *************************************

     d dsWDELP       e ds                  extname(WDELP) qualified inz
     d strPopMsg       s            256
     d strPopTitle     s             50
     d*** End   Add *** 05/03/21 ****************************************
      *** Beg Add *** 03/04/23 *** WC2430 *************************************
     d Payments        pr                  ExtPgm('WTCLMPMT')
     d  pMode                         1A
     d  pClmRrn                       9  0
     d  pPmtRrn                       9  0
     d  pTaxId                        9  0
     d  pModifier                     3  0
     d  pFromDt                       7  0
     d  pToDt                         7  0
     d  pEOB#                         9  0
     d  pRecon#                       3  0
     d  pSysType                      1
     d  pBillType                     1
     d  pPayment                      9  2
     d  pTransCode                    2
     d  pPayStatCode                  2
     d  pPayValue1                    2
     d  pPayValue2                    2
     d  pRecvDt                       7  0
     d  pReturnCode                   1

     d RecoveryTrack   pr                  ExtPgm('WTRCVTRK')
     d   prds                        40
     d   mode                         1
     d   year#                        3  0
     d   case#                        7  0
     d   request#                     5  0
     d   rtcd                         1
     d   group#                       3  0
     d   claimRrn                     9  0

     d  pMode          s              1A   Inz(*blanks)
     d  pClmRrn        s              9  0 Inz(*zeros)
     d  pPmtRrn        s              9  0 Inz(*zeros)
     d  pTaxId         s              9  0 Inz(*zeros)
     d  pModifier      s              3  0 Inz(*zeros)
     d  pFromDt        s              7  0 Inz(*zeros)
     d  pToDt          s              7  0 Inz(*zeros)
     d  pEOB#          s              9  0 Inz(*zeros)
     d  pRecon#        s              3  0 Inz(*zeros)
     d  pSysType       s              1    Inz(*blanks)
     d  pBillType      s              1    Inz(*blanks)
     d  pPayment       s              9  2 Inz(*zeros)
     d  pTransCode     s              2    Inz(*blanks)
     d  pPayStatCode   s              2    Inz(*blanks)
     d  pPayValue1     s              2    Inz(*blanks)
     d  pPayValue2     s              2    Inz(*blanks)
     d  pRecvDt        s              7  0 Inz(*zeros)
     d  pReturnCode    s              1    Inz(*blanks)
     d  prds           s             40    Inz(*blanks)
     d  mode           s              1    Inz(*blanks)
     d  year#          s              3  0 Inz(*zeros)
     d  case#          s              7  0 Inz(*zeros)
     d  request#       s              5  0 Inz(*zeros)
     d  rtcd           s              1    Inz(*blanks)
     d  group#         s              3  0 Inz(*zeros)
     d  claimRrn       s              9  0 Inz(*zeros)
      *** End Add *** 03/04/23 *** WC2430 *************************************



     C*
     C* Entry list to program.
     C*
     C     *entry        plist
     C                   parm                    QQProcess        40
     C                   parm                    QQRRN             9 0
     C                   parm                    QQMode            2
     C                   parm                    QQData          500
     C*
      /free
       //***  Begin Add  ***  08/18/21  ******************************
       // This must be the first line in the program
       Exec Sql Set Option Commit = *None;
       //***  End   Add  ***  08/18/21  ******************************

       RtvCltID(clientID);

       Select;
       When QQProcess = 'Documents';
          Exsr Documents;
       When QQProcess = 'Copy Employer';
          Exsr CopyEmployer;
       When QQProcess = 'PaymentHistory';
          Exsr PmtHisRpt;
       When QQProcess = 'Premium Notice';
          Exsr PremNtcRpt;
       When QQProcess = 'Policy Page';
          Exsr PolPageRpt;
       When QQProcess = 'Renew Policy';
          Exsr PolRenewal;
       When QQProcess = 'Certificate Holders';
          Exsr CertHoldr;
       When QQProcess = 'LateNoticeEst';
          Exsr LatNotEst;
       When QQProcess = 'Estimated Bills';
          Bills('Estimated');
       When QQProcess = 'Quote Bill';
          Bills('Quote');
       When QQProcess = 'Audit Bill';
          Bills('Audit');
      *** Beg Add *** 03/04/23 *** WC2430 *************************************
       When QQProcess = 'Payments';
          ExSr ClaimPayment;
       When QQProcess = 'SingleAdjustment';
          ExSr SingleAdjustment;
       When QQProcess = 'RecoveryTracking';
          ExSr RecoveryTracking;
      *** End Add *** 03/04/23 *** WC2430 *************************************
       When QQProcess = 'S/R Worksheet';
          Exsr SRWorksheet;
       When QQProcess = 'Audit with S/R';
          Exsr UpdateAuditPayrolls;
       //*** Begin Add *** 05/03/21 ****************************************
       When QQProcess = 'ThirdPartyPayment';
          Exsr SubmitQuoteThirdParty;
       //*** End   Add *** 05/03/21 ****************************************
       //*** Beg Add ***** 01/17/22 ****************************************
       //*** Beg Chg ***** 06/02/23 ****************************************
       //*** When QQProcess = 'Copy Configuration' and QQMode = 'GS';
       When QQProcess = 'Rating Factor Copy' and QQMode = 'GS';
       //*** End Chg ***** 06/02/23 ****************************************
          Exsr GrPCopySetup;
       //*** End Add ***** 01/17/22 ****************************************
       //*** Beg Add ***** 06/21/22 ****************************************
       When QQProcess = 'Change FEIN';
          Exsr AgtChgFEIN;
       //*** End Add ***** 06/21/22 ****************************************
       //*** Beg Add ***** 08/01/24 *****************************************
       When QQProcess = 'Audit Reversal';
          Exsr AuditReversal;
       //*** End Add ***** 08/01/24 *****************************************

       Endsl;

       //***  Begin Add  ***  08/18/21  ******************
       SetRunCriteria(1);
       //***  End   Add  ***  08/18/21  ******************

       *InLr = *On;

       //*********************************************************************
       //*
       //* Process Documents (Scriptura Department Letters)
       //*
       //*********************************************************************
       Begsr Documents;

          If %subst(QQData:1:1) = 'D';
             Setll 1 psc0p;
             Read psc0p;
             Select;
             When qqMode = 'PS' and %scan('P':SCkey3) <> 0;
             When qqMode = 'CS' and %scan('C':SCkey3) <> 0;
             Other;
                PopOneBtn('Document option is not available in Inquiry mode.':
                 'Option Not Available');
                Leavesr;
             Endsl;
          Endif;

          DocWindow(QQRRN:QQMode);

       Endsr;
       //*********************************************************************
       //*
       //* Process Copy Employer
       //*
       //*********************************************************************
       Begsr CopyEmployer;

          xxQte = %subst(QQData:1:1);

          CopyPolicy(QQRRN:xxQte);

       Endsr;

       //*********************************************************************
       //*
       //* Process Payment History Report
       //*
       //*********************************************************************
       Begsr PmtHisRpt;
          inRRn = %EditC(QQrrn:'X');
          inCall = *Blanks;
          //inProcess = QQProcess;
          WtrHstRpt(QQProcess:inCall:inRRN);

       Endsr;

       //********************************************************************************
       //* Process Premium Notice
       //********************************************************************************
       Begsr PremNtcRpt;

          inRRn = %EditC(QQrrn:'X');
          inCall = *Blanks;
          WtrPremNtc(QQProcess:inCall:inRRN);

       Endsr;

       //********************************************************************************
       //* Policy Page Report
       //********************************************************************************
       Begsr PolPageRpt;
        inCall = *Blanks;
        inRRn1 = %EditC(QQrrn:'X');
        inRtc = *Blanks;
        inCo# = *Blanks;
        inFnd = *Blanks;
        inFyr = *Blanks;
          WtPolPgSbmR(QQProcess:inCall:inRRn1:inRtc:inCo#:inFnd:inFyr);

       Endsr;
      *** Beg Add *** 03/04/23 *** WC2430 *************************************
       //********************************************************************************
       //* Claim Payments
       //********************************************************************************
       Begsr ClaimPayment;
        pMode   = 'P' ;
        pClmRrn = QQrrn;
        Payments(pMode: pClmRrn: pPmtRrn: pTaxId: pModifier:
                 pFromDt: pToDt: pEOB#: pRecon#: pSysType:
                 pBillType: pPayment: pTransCode: pPayStatCode:
                 pPayValue1: pPayValue2: pRecvDt:pReturnCode);
       Endsr;
       //********************************************************************************
       //* Claim Payments
       //********************************************************************************
       Begsr RecoveryTracking;
        Chain(n) QQrrn pcm0p;
        prds     = ' ';
        mode     = ' ';
        year#    = CMfyr;
        case#    = CMcase;
        request# = 0;
        group#   = CMfnd;
        rtcd     = ' ';
        pClmRrn = QQrrn;
        RecoveryTrack(prds:mode:year#:case#:request#:rtcd:group#:pClmRrn);
       Endsr;
       //********************************************************************************
       //* Single Adjustment
       //********************************************************************************
       Begsr SingleAdjustment;
        pMode   = 'S' ;
        pClmRrn = QQrrn;
        Payments(pMode: pClmRrn: pPmtRrn: pTaxId: pModifier:
                 pFromDt: pToDt: pEOB#: pRecon#: pSysType:
                 pBillType: pPayment: pTransCode: pPayStatCode:
                 pPayValue1: pPayValue2: pRecvDt:pReturnCode);
       Endsr;
      *** End Add *** 03/04/23 *** WC2430 *************************************
       //********************************************************************************
       //* Policy Renewal Process.
       //********************************************************************************
       Begsr PolRenewal;

        inCall = *Blanks;
        inRrn = %EditC(QQrrn:'X');
        WtPolRen(QQProcess:inCall:inRrn);

       Endsr;

       //********************************************************************************
       //* Certificate Holders
       //********************************************************************************
       Begsr CertHoldr;

        inKC = ' ';
        inKL = ' ';
        inCall = %subst(QQData:1:1);
        inRrn = *Blanks;
        inRrn2 = %EditC(QQrrn:'X');
        WtCertHoldr(inKC:inKL:inCall:inRrn:inRrn2);

       Endsr;

       //********************************************************************************
       //* Late Notice Report - Est
       //********************************************************************************
       Begsr LatNotEst;

        inRrn = %EditC(QQrrn:'X');
        inCall = *Blanks;
        WtLatNotEst(QQProcess:inCall:inRRN);

       Endsr;

       //********************************************************************************
       //* S/R Worksheet
       //********************************************************************************
       Begsr SRWorksheet;
        inCall = *Blanks;
        inRRn = %EditC(QQrrn:'X');
        SRWkSbm(QQProcess:inCall:inRRN);

       Endsr;

       //*** Begin Add *** 05/03/21 ****************************************
       //********************************************************************************
       //* Submit Quote Third Party
       //********************************************************************************
       Begsr SubmitQuoteThirdParty;

        strPopMsg   = 'Submit Quote to ' +
                      %Trim(GetModuleProperty('Invoice Cloud':'Name'));
        strPopTitle = strPopMsg;

        If PopTwoBtn(StrPopMsg:strPopTitle:'Submit':'Cancel');
           SubmitQuote(qqRRN);
        Endif;

       Endsr;
       //*** End   Add *** 05/03/21 ****************************************
       //*** Beg Add ***** 01/17/22 ****************************************
       //********************************************************************************
       //* Copy Group Setup
       //********************************************************************************
       Begsr GrpCopySetup;
        GroupCopySetup(QQRrn:QQData);
       Endsr;
       //*** End Add ***** 01/17/22 ****************************************

       //*** Beg Add ***** 06/21/22 ****************************************
       //********************************************************************************
       //* Change FEIN from Agency Maintenance toolbar (More Actions)
       //********************************************************************************
       Begsr AgtChgFEIN;
        @@kl = *off;
        @@lc = *off;
        @@mode = ' ';
        @@rrn2 = QQRRN;
        ChangeFEIN(@@kl:@@lc:@@mode:@@rrn2);
       Endsr;
       //*** End Add ***** 06/21/22 ****************************************

       //********************************************************************************
       //* Update Audit Payrolls
       //********************************************************************************
       Begsr UpdateAuditPayrolls;

        str999Msg = 'Set all payroll records on all divisions to audited ' +
                    'with total of S/R payroll. Press Confirm to continue or ' +
                    'Cancel to exit';
        If PopTwoBtn(Str999Msg:'Confirmation':'Confirm':'Cancel');
           AuditWithSR(QQrrn);
        Endif;

       Endsr;
       //*** Beg Add ***** 08/01/24 ***************************************************
       //******************************************************************************
       //* Audit Reversal
       //******************************************************************************
       Begsr AuditReversal;

        strPopMsg = 'Perform an Audit Reversal on this policy?';
        If PopTwoBtn(strPopMsg:'Audit Reversal':'Confirm':'Cancel');
           PolAudRvs(QQrrn);
        Endif;

       Endsr;
       //*** End Add ***** 08/01/24 ***************************************************
      /end-free
       //===================================================================
       // Bills - Call Billing Programs
       //===================================================================
     p Bills           b
     d Bills           pi
     D   billType                    10a   const

     d inCall          s              1a
     d inQte           s              1a
     d inRRn           s              9a
     d inMode          s              1a
     d inKC            s              1a
     d inKL            s              1a

      /free

       // Option not available for claims only systems.
       If ClaimsOnly() And clientID <> '025';
          PopOneBtn('Estimated bill is not available, system is set    ' +
            'for claim only.':'Option Not Available');
       Else;
          Select;
          // Estimated Bill
          When billType = 'Estimated';
             inCall = ' ';
             inQte  = ' ';
             inKC   = ' ';
             inKL   = ' ';
             inRrn  = %EditC(qqRrn:'X');
             WtPolEstBl(inCall:inQte:inKC:inKL:inRRN);
          // Quote Bill
          When billType = 'Quote';
             inCall = ' ';
             inQte  = 'Q';
             inKC   = ' ';
             inKL   = ' ';
             inRrn  = %EditC(qqRrn:'X');
             WtPolEstBl(inCall:inQte:inKC:inKL:inRRN);
          // Audit Bill
          When billType = 'Audit';
             inKC   = ' ';
             inKL   = ' ';
             inMode = %Subst(QQData:1:1);
             inRrn  = %EditC(QQRrn:'X');
             WtPolAudBl(inKC:inKL:inMode:inRrn);
          EndSl;
       EndIf;

      /end-free
     p Bills           e

      //***  Begin Add  ***  08/18/21  ******************
      //===================================================================
      // Set criteria for usage by WTGETTBFLG
      //===================================================================
     PSetRunCriteria   b
     DSetRunCriteria   pi
     D inCounter                      3  0 const

     DconDelaySec      c                   1
     DconRetries       c                   9
     DoutType          s             40    inz
      /Free
       Select;
       When qqMode = 'PS';
          outType = 'Policy';
       When qqMode = 'CS';
          outType = 'Claim';
       Endsl;
       Reset decCount;
       Exec sql
        select count(*) into :decCount
         from WTGETTBFLG
         where TBuser = :q1User;
       // The data will be cleared in WTGETTBFLG program, after use
       Select;
       When inCounter = conRetries;
          Exec sql
           update WTGETTBFLG
            set TBrrn = :qqRRN,
                TBrrnType = :outType
            where TBuser = :q1User;
       When decCount <> 0;
          DelayJob(conDelaySec);
          SetRunCriteria(inCounter+1);
       Other;
          Exec sql
           insert into WTGETTBFLG
           values (:q1User, :qqRRN, :outType);
       Endsl;
       Return;
      /End-free
     PSetRunCriteria   e
      //***  End   Add  ***  08/18/21  ******************

       //*** Begin Add *** 05/03/21 ****************************************
       //===================================================================
       // SubmitQuote - Submit Quote Bill
       //===================================================================
     p SubmitQuote     b
     d SubmitQuote     pi
     D   rrnWDELP                     9  0

      * Quote Bill Fields
     d chrCo#          s              3A
     d chrFnd          s              3A
     d chrFyr          s              3A
     d chrEmp#         s              9A
     d chrDiv          s              5A
     d chrDate         s              8A
     d chrEsCd         s              2A   Inz(' ')
     d chrCom1         s              2A   Inz(' ')
     d chrCom2         s              2A   Inz(' ')
     d chrCom3         s              2A   Inz(' ')
     d chrDue          s              8A   Inz('00000000')
     d chrDep          s              1A   Inz('Y')
     d chrStl          s              1A   Inz('E')
     d chrPrt          s              1A   Inz(' ')
     d chrOvr          s              1A   Inz(' ')
     d chrSrCd         s              2A   Inz(' ')
     d chrAjCd         s              2A   Inz(' ')
     d chrQte          s              1A   Inz('Q')
     d chrQue          s             10A   Inz('QPRINT')
     d chrRnb          s              1A   Inz(' ')
     d chrDart         s              1A   Inz('N')
     d chrHard         s              1A   Inz(' ')
     d chrAgto         s              1A   Inz('N')
     d chrFQt          s              1A   Inz('I')
     d chrRmCd         s              5A   Inz(' ')
     d chrAR           s              2A   Inz(' ')
     d chrEfDt         s              8A   Inz('00000000')
     d chrFCm1         s             30A   Inz(' ')
     d chrFCm2         s             30A   Inz(' ')
     d chrFCm3         s             30A   Inz(' ')
     d chrAR2          s              2A   Inz(' ')

     d decCo#          s              3  0
     d decFnd          s              3  0

     d dateMDYY        s               d   DatFmt(*usa)
     d intDays         s              2s 0
     d strSqlStmt      s            256


      /free

       // Retieve Quote Information
       strSqlStmt = 'Select * From ' + %Trim(RtvQuoteLib('*QTE')) +
                    '/WDELP Where rrn(wdelp)=' + %EditC(rrnWDELP:'X');

       Exec sql close cursorWDELP;
       Exec Sql Prepare sqlWDELP From :strSqlStmt;
       Exec SQL Declare cursorWDELP Cursor For sqlWDELP;
       Exec SQL Open cursorWDELP;

       Exec SQL fetch next from cursorWDELP into :dsWDELP;
       If IsValidFetch(sqlStt);

          chrCo#  = %EditC(dsWDELP.ELco#:'X');
          chrFnd  = %EditC(dsWDELP.ELfnd:'X');
          chrFyr  = %EditC(dsWDELP.ELfYr:'X');
          chrEmp# = %EditC(dsWDELP.ELemp#:'X');
          chrDiv  = %EditC(dsWDELP.ELdiv:'X');

          decCo#  = dsWDELP.ELco#;
          decfnd  = dsWDELP.ELfnd;

          // Get curremt date
          dateMDYY = SystemDate();

          // Get the Interim days
          Exec Sql Select EXrvID Into :intDays from WMEXP
                   Where EXco# = :decCo# And EXfnd = :decFnd;

          // Add the days to the current date
          dateMDYY = dateMDYY + %Days(intDays);

          // Populate Date Parameter
          chrDate = %Char(dateMDYY:*usa0);

          QuoteBill(chrCo#:chrFnd:chrFyr:chrEmp#:chrDiv:chrDate:chrEsCd:
                    chrCom1:chrCom2:chrCom3:chrDue:chrDep:chrStl:chrPrt:
                    chrOvr:chrSrCd:chrAjCd:chrQte:chrQue:chrRnb:chrDart:
                    chrHard:chrAgto:chrFQt:chrRmCd:chrAR:chrEfDt:chrFCm1:
                    chrFCm2:chrFCm3:chrAR2);
       EndIf;

      /end-free

     p SubmitQuote     e
       //*** End   Add *** 05/03/21 ****************************************
