     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Claim ToolBar Wrapper Program
     h*
     h*  Called from: Tool Bar Java Script
     h*  Calls to:
     h*
     h*  Created on 10/08/2019 by C:062 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h*  03/04/23 C:340 Project: WC2430
     h*                 Display Options based on New Payment system flag
     h*                 Call new Claim Payment process.
     H*  04/05/21 C:180 Project: WC2732
     h*
     h*  06/13/17 C:062 Support: 47766(114)
     h*                 Access FROI information via 148 instread of RAT.
     h*
     h*  12/06/16 C:306 Support: 44002
     h*                 Need to pass the mode as parameter to claim Action pr.
     h*
     h*  06/23/16 C:298 PhaseWare 42154
     h*                 Correct call of Payment History Report.
     h*
     h*  07/07/15 C:180 Suppport 35725(TASB)
     h*                 Correct call of WageCalc.
     h*
     h*  06/18/15 C:062 Support: 35406 (25)
     h*                 -Restore Reserve Worksheet for TASB.
     h*
     h*  03/03/15 C:079 Support: 32727 (25)
     h*                 Case management should get opened in Update mode
     h*
     h*  12/09/14 C:189 Redmine: 4763 (MPE)
     h*                 For MPE , Case management should get opened in
     h*                 update mode as happening with CoveRisk
     h*
     h*  12/01/14 C:279 Project: 114WLF
     h*                 Modified the processing for Reserve Worksheet.
     h*
     h*  07/29/14 C:282 Support: WA129 86177
     h*                 On selecting the 'Recovery Request' option
     h*                 intially W0600R program will be called.
     h*
     h*  07/28/14 C:279 Project: 114WMD
     h*                 Added new toolbar option 'Claim Action Plan'.
     h*
     h*  07/11/14 C:279 Project: 111WBL
     h*                 Added new toolbar option 'Excess Recoveries'.
     h*
     h*  06/06/14 C:298 Redmine: 4174
     h*                 Add edit to enable 'Change' capability On Medical
     h*                 Case management through Claims Toolbar for
     h*                 client # 129.
     h*
     h*  02/10/14 C:062 Redmine: 3887 (TASB)
     h*                 Correct call of W0595R.
     h*
     h*  10/01/13 C:53  Project: 25WSS
     h*                 Add client programs.
     h*
     h*  03/22/13 C:65  Project: WC2118
     h*                 Open the managed care file FMFNL1 for only the two
     h*                 options that use it - some clients do not have MC.
     h*
     h**********************************************************************
      * Customer Master File
     Fwmcmp     if   e             disk    rename(pcm01:pcm0p)
     F*** begin chg ***  03/22/13  *****************************************
     F*****fmfnl1    if   e           k disk
     Ffmfnl1    if   e           k disk    usropn
     F*** end chg *****  03/22/13  *****************************************
     Fststl1    if   e           k disk

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

     d* Get System Flags
     d GetSysFlags     pr                  ExtPgm('SA001R')
     d   wcmsInst                     1
     d   wcmcInst                     1
     d   wcmcInt                      1

     d* Get client ID
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d   outCltID                     3

     d* Department Letters
     d DeptLetters     pr                  ExtPgm('W4110R')
     d   title                       40
     d   call                         1
     d   returnCode                   1
     d   group                        3
     d   year                         3
     d   case                         7
     d   department                   2

     d* Check Requests
     d ChkRequests     pr                  ExtPgm('W0080R')
     d   title                       40
     d   call                         1
     d   group                        3
     d   year                         3
     d   case                         7
     d   fein                         9
     d   modifier                     3
     d   fromDate                     6
     d   toDate                       6
     d   amount                       9
     d   eob#                         9
     d   providerName                40
     d   tranCode                     2
     d   cancel                       1

     d* Manual Payments
     d ManualPmt       pr                  ExtPgm('W0120R')
     d   exit                         1
     d   cancel                       1
     d   mode                         1
     d   claimRrn                     9  0

      ******* beg add ******* 07/11/14 ********************************
     d* Excess Recoveries
     d ExcRecv         pr                  ExtPgm('W0125R')
     d   exit                         1
     d   cancel                       1
     d   call                         1
     d   group                        3
     d   year                         3
     d   case                         7
      ******* end add ******* 07/11/14 ********************************

      ******* beg add ******* 07/28/14 ********************************
      * Claim Action Plan
     d ClmActPln       pr                  ExtPgm('WTCLMACTP')
     d   inRRN                        9
     d   env                          1
     d   TabIndex                     5
      ******* beg add ******* 12/06/16 ********************************
     d   mode                         1
      ******* end add ******* 12/06/16 ********************************
      ******* end add ******* 07/28/14 ********************************

     d* Recurring CompRequests
     d ReoccurComp     pr                  ExtPgm('W8080R')
     d   title                       40
     d   call                         1
     d   group#                       3  0
     d   year#                        3  0
     d   case#                        7  0
     d   exit                         1

      *** begin add ***  07/29/14  *****************************************
     d* Recovery Request Search
     d RecoveryReqSr   pr                  ExtPgm('W0600R')
     d   prds                        40
     d   mode                         1
     d   year#                        3  0
     d   case#                        7  0
     d   request#                     5  0
     d   rtcd                         1
     d   group#                       3  0
      *** end   add ***  07/29/14  *****************************************
     d* Recovery Requests
     d RecoveryReq     pr                  ExtPgm('W0100R')
     d   exit                         1
     d   cancel                       1
     d   mode                         1
     d   claimRrn                     9  0
     d   request#                     5  0

     d* Work With Provider Bills
     d ProviderBills   pr                  ExtPgm('F0350R')
     d   call                         1
     d   subscriber                   5  0
     d   group#                       3  0
     d   year#                        3  0
     d   case#                        7  0
     d   eomb#                        9  0
     d   cert#                       15

     d* Work With Medical Cases
     d MedicalCases    pr                  ExtPgm('F0269R')
     d   call                         1
     d   subscriber                   5  0
     d   group#                       3  0
     d   year#                        3  0
     d   case#                        7  0
     d   socsec#                      9
     d   accDate                      8  0
     d   returnCode                   1

     d* Delete Claim
     d deleteClaim     pr                  ExtPgm('W0070R04')
     d   exit                         1
     d   cancel                       1
     d   mode                         1
     d   claimRrn                     9  0

     d* Delete Claim Validate
     d delClmVal       pr                  ExtPgm('WTCLMDEL')
     d   claimRrn                     9  0
     d   isCancel                      n

     d* Payment History
      ***** Beg Chg ***** 06/23/16 ****************************************
     d*** PmtHistory      pr                  ExtPgm('W7060R')
     d PmtHistory      pr                  ExtPgm('WTRPTPHST')
      ***** End Chg ***** 06/23/16 ****************************************
     d   title                       40
     d   call                         1
     d   inRRN                        9

     d* EDI Control
     d GetControlInfo  pr                  ExtPgm('WA076R')
     d   company#                     3  0
     d   jurState                     2
     d   tranID                       3
     d   group#                       3  0
     d   returnCode                   1
     d   ediFlag                      3
     d   IXeLt                        1
     d   IXeLtD                       3  0
     d   IXfroi                       2
     d   IXsDue                       3  0
     d   IXrls                        1
     d   IXcsDt                       7  0
     d   IXeaDc                       5
     d   IXhcPr                       5
     d   IXfnRc                       5
     d   IXhcEd                       1
     d   IXcrEr                       5
     d   IXceIr                       5
     d   IXceCr                       5
     d   IXctAr                       5
     d   IXctRr                       5
     d   IXmo                         1
     d   IXmLtD                       3  0

     d* Display Message Program
     d DisplayMessage  pr                  ExtPgm('WA020R')
     d   exit                         1
     d   cancel                       1
     d   mode                         1
     d   claimRrn                     9  0
     d   info1                       40
     d   info2                       40
     d   number1                     13  2
     d   number2                     13  2
     d   date1                        8
     d   date2                        8
     d   call                         1
     d   returnCode                   1

     d* Noi Program
     d FroiPrint       pr                  ExtPgm(froiPrintPgm)
     d   exit                         1
     d   cancel                       1
     d   mode                         1
     d   claimRrn                     9  0
     d   jurState                     2

     d* Recurring Comp Info
     d RecurCmpInfo    pr                  ExtPgm('W8082R')
     d   call                         1
     d   exit                         1
     d   cancel                       1
     d   claimRrn                     9  0

     d CreateWRKF      pr                  ExtPgm('FA710C')
     d   returnCode                   1
     d   fromName                    10
     d   toName                      10

     d DelClmMC        pr                  ExtPgm('F7090R')
     d   call                         1
     d   cancel                       1
     d   intCaseRrn                   9  0
     d   intCaseRrn1                  9  0
     d   intCaseRrm2                  9  0
     d   intCaseRrn3                  9  0
     d   strTranDel                   1
     d   strFlag                      1
     d   strWcms                      1

     d* Get the RRN of the WCMC case
     d GetCaseRRN      pr                  ExtPgm('FA201R')
     d   strCompany#                  3
     d   strGroup                     3
     d   strYear                      3
     d   strCase                      7
     d   intCaseRrn                   9  0
     d   returnCode                   1

      * *ENTRY
     DWtClmTWrp        pr                  ExtPgm('WtClmTWrp')
     D                                9
     D                               20
     D                              256
      ****** beg add ****** 07/28/11 *****************************************
     D inEnv                          1    options(*nopass)
      ****** end add ****** 07/28/11 *****************************************
       //** begin add ***  10/01/13  *****************************************

     d Attorney        pr                  ExtPgm('W4343R')
     d   call                         1
     d   company                      3
     d   group                        3
     d   year                         3
     d   case                         7
     d   seq#                         5  0
     d   returnCode                   1

     d RsvWorksheet    pr                  ExtPgm('W0590R')
     d   exit                         1
     d   cancel                       1
     d   mode                         1
     d   claimRrn                     9  0

      ***** beg add ***** 12/01/14 ***********************************
     d DriverPgm       pr                  extpgm('STDRVPGM')
     d inRrn                          9
     d mode                           1
     d next                          10
     d TabIndex                       5
      ***** end add ***** 12/01/14 ***********************************

     d WageCalc        pr                  ExtPgm('W0595R')
     d   exit                         1
     d   cancel                       1
     d   mode                         1
     d   claimRrn                     9  0

     d SeasonAdj       pr                  ExtPgm('W0325R')
     d   exit                         1
     d   cancel                       1
     d   mode                         1
     d   claimRrn                     9  0

     d LifetBen        pr                  ExtPgm('W4344R')
     d   call                         1
     d   company#                     3
     d   group#                       3
     d   year#                        3
     d   case#                        7
     d   returnCode                   1
      ***** Beg Add ***** 04/05/21 ****************************************
     d MultiClaim      pr                  Extpgm('WTMLTCLM')
     d  inPrds                       40A   const
     d  inCall                        1A
     d  inMltRrn                      9  0
      ***** End Add ***** 04/05/21 ****************************************
      *** Beg Add *** 03/04/23 *** WC2430 *************************************
     d* Payments
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

      * Recovery Tracking
     d RecoveryTrack   pr                  ExtPgm('WTRCVTRK')
     d   prds                        40
     d   mode                         1
     d   year#                        3  0
     d   case#                        7  0
     d   request#                     5  0
     d   rtcd                         1
     d   group#                       3  0
     d   claimRrn                     9  0

      *** End Add *** 03/04/23 *** WC2430 *************************************

       //** end add *****  10/01/13  *****************************************
     DWtClmTWrp        pi
     D inRRN                          9
     D inOption                      20
     D inMode                       256
      ****** beg add ****** 07/28/11 *****************************************
     D inEnv                          1    options(*nopass)
      ****** end add ****** 07/28/11 *****************************************

       // SA001R parameters
     d   wcmsInst      s              1    Inz(*Blanks)
     d   wcmcInst      s              1    Inz(*Blanks)
     d   wcmcInt       s              1    Inz(*Blanks)

     D   claimRrn      s              9  0
     D   option        s             20
     d   title         s             40
     d   call          s              1
     d   returnCode    s              1
     d   group         s              3
     d   year          s              3
     d   case          s              7
     d   department    s              2
     d   fein          s              9
     d   modifier      s              3
     d   fromDate      s              6
     d   toDate        s              6
     d   amount        s              9
     d   eob#          s              9
     d   providerName  s             40
     d   tranCode      s              2
     d   cancel        s              1
     d   exit          s              1
     d   mode          s              1
     d   request#      s              5  0
     d   subscriber#   s              5  0
     d   group#        s              3  0
     d   year#         s              3  0
     d   case#         s              7  0
     d   eomb#         s              9  0
     d   cert#         s             15
     d   froiPrintPgm  s             10
     d   info1         s             40
     d   info2         s             40
     d   number1       s             13  2
     d   number2       s             13  2
     d   date1         s              8
     d   date2         s              8
     d   clientId      s              3
     d   stateKey      s             10
     d   socsec#       s              9
     d   accDate       s              8  0
     d   isCancel      s               n
      *** Begin add ***  07/29/14  *****************************************
     d   prds          s             40
     d   rtcd          s              1
      *** End   add ***  07/29/14  *****************************************
      ***** beg add ***** 08/05/14 *****************************************
     d @@env           s              1
     d @@TabIndex      s              5
      ***** end add ***** 08/05/14 *****************************************
      ***** beg add ***** 12/01/14 *****************************************
     d @@Next          s             10
      ***** end add ***** 12/01/14 *****************************************

     d   strCompany#   s              3
     d   strGroup      s              3
     d   strYear       s              3
     d   strCase       s              7
       //** Begin add ***  10/01/13  *****************************************
     d   seq#          s              5  0
     d   company       s              3
       //** End   add ***  10/01/13  *****************************************

     d   company#      s              3  0
     d   jurState      s              2
     d   tranID        s              3
     d   ediFlag       s              3    Inz(*Blanks)
     d   IXeLt         s              1    Inz(*Blanks)
     d   IXeLtD        s              3  0 Inz(*Zeros)
     d   IXfroi        s              2    Inz(*Blanks)
     d   IXsDue        s              3  0 Inz(*Zeros)
     d   IXrls         s              1    Inz(*Blanks)
     d   IXcsDt        s              7  0 Inz(*Zeros)
     d   IXeaDc        s              5    Inz(*Blanks)
     d   IXhcPr        s              5    Inz(*Blanks)
     d   IXfnRc        s              5    Inz(*Blanks)
     d   IXhcEd        s              1    Inz(*Blanks)
     d   IXcrEr        s              5    Inz(*Blanks)
     d   IXceIr        s              5    Inz(*Blanks)
     d   IXceCr        s              5    Inz(*Blanks)
     d   IXctAr        s              5    Inz(*Blanks)
     d   IXctRr        s              5    Inz(*Blanks)
     d   IXmo          s              1    Inz(*Blanks)
     d   IXmLtD        s              3  0 Inz(*Zeros)

     d   intCaseRrn    s              9  0 Inz(*zeros)
     d   intCaseRrn1   s              9  0 Inz(*zeros)
     d   intCaseRrn2   s              9  0 Inz(*zeros)
     d   intCaseRrn3   s              9  0 Inz(*zeros)
     d   strTranDel    s              1    Inz(*blanks)
     d   strFlag       s              1    Inz(*blanks)
     d   strWcms       s              1    Inz(*blanks)
     d*
       // FA710C parameters
     d   fromName      s             10    Inz(*blanks)
     d   toName        s             10    Inz(*blanks)
      *** Begin Add *** 04/05/21 ********************************
     d  inPrds         s             40A
     d  inCall         s              1A
     d  inMltRrn       s              9  0 Inz
      *** End   Add *** 04/05/21 ********************************
       //*** Beg Add *** 03/04/23 *** WC2430 *******************************
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
       //*** End Add *** 03/04/23 *** WC2430 *******************************


     C     keyFMFNL1     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd

      /Free

       //*** Begin Add *** 06/18/15 ************************************
       GetClientId(clientId);
       //*** End   Add *** 06/18/15 ************************************
       claimRrn=%Dec(inRRN:9:0);
       mode=%Subst(inMode:1:1);
       option=inOption;

       //********* beg add ******* 07/28/14 **************************
       If %parms() >= 4;
          @@env = inEnv;
       Endif;
       //********* end add ******* 07/28/14 **************************

       // Get System Flags
       GetSysFlags(wcmsInst:wcmcInst:wcmcInt);

       Chain(n) claimRrn pcm0p;
       //
       // Call Items
       Select;
       //
       // Call Claim Department Letters
       When Option='DeptLetters';
          title='Claim Department Letters';
          call='2';
          returnCode=' ';
          group=%EditC(CMfnd:'X');
          year=%EditC(CMfYr:'X');
          case=%EditC(CMcase:'X');
          department='CS';
          DeptLetters(title:call:returnCode:group:year:case:department);
       //
       // Call Check Request Programsrs
       When Option='PayRequests';
          title='Payment Requests';
          call='A';
          group=%EditC(CMfnd:'X');
          year=%EditC(CMfYr:'X');
          case=%EditC(CMcase:'X');
          fein=*Blanks;
          modifier=*Blanks;
          fromDate=*Blanks;
          toDate=*Blanks;
          amount=*Blanks;
          eob#=*Blanks;
          providerName=*Blanks;
          tranCode=*Blanks;
          cancel='0';
          ChkRequests(title:call:group:year:case:fein:modifier:
           fromDate:toDate:amount:eob#:providerName:tranCode:cancel);
       //
       // Call Manual Payment
       When Option='ManualPayment';
          exit='0';
          cancel='0';
          mode='A';
          ManualPmt(exit:cancel:mode:claimRrn);
       //
       //******** beg add ********** 07/11/14 **************************
       // Call Excess Recoveries
       When Option='ExcessRec';
          exit   = '0';
          cancel = '0';
          group  = %EditC(CMfnd:'X');
          year   = %EditC(CMfYr:'X');
          case   = %EditC(CMcase:'X');
          If mode = 'D';
             call = '1';
          Else;
             call = ' ';
          Endif;

          ExcRecv(exit:cancel:call:group:year:case);
       //******** end add ********** 07/11/14 **************************

       //******** beg add ********** 07/28/14 **************************
       // Call Clam Action Plan
       When Option = 'ClaimActionPln';
          If @@env <> 'C' and @@env <> 'D';
             @@env = 'C';
          Endif;
          @@TabIndex = '00000';

       //******** beg add ********** 12/06/16 **************************
       //   ClmActPln(inRrn:@@env:@@TabIndex);
          ClmActPln(inRrn:@@env:@@TabIndex:mode);
       //******** End add ********** 12/06/16 **************************
       //******** end add ********** 07/28/14 **************************

       // Call Recurring Comp Request
       When Option='RecurringComp';
          title=' ';
          call=' ';
          group#=CMfnd;
          year#=CMfYr;
          case#=CMcase;
          exit='0';
          ReoccurComp(title:call:group#:year#:case#:exit);
       //
       // Call Recurring Comp Info
       When Option='RecurringCompInfo';
          call=' ';
          exit='0';
          cancel='0';
          RecurCmpInfo(call:exit:cancel:claimRrn);
       //
       // Call Recovery Requests
       When Option='RecoveryRequest';
       // *** begin chg ***  07/29/14  *****************************************
          // exit='0';
          // cancel='0';
          // mode='A';
          // request#=0;
          // RecoveryReq(exit:cancel:mode:claimRrn:request#);

          Dow exit <> '1';
              exit     = '0';
              cancel   = '0';
              prds     = ' ';
              mode     = ' ';
              year#    = CMfyr;
              case#    = CMcase;
              request# = 0;
              group#   = CMfnd;
              rtcd     = ' ';
              RecoveryReqSr(prds:mode:year#:case#:request#:rtcd:group#);

              If rtcd = 'B';
                Leave;
              EndIf;

              If request# <> 0 Or rtcd = '6';
                 claimRrn = %Dec(inRRN:9:0);
                 mode = 'D';
                 RecoveryReq(exit:cancel:mode:claimRrn:request#);
              Endif;

          Enddo;
       //  *** end   chg ***  07/29/14  *****************************************
       //
       // Call Provider Bills
       When Option='ProviderBills';
          If mode='D';
             call='1';
          Else;
             call=' ';
          Endif;
       //** begin add ***  03/22/13  *****************************************
          Open FMFNL1;
       //** end add *****  03/22/13  *****************************************
          Chain keyFMFNL1 pfn01;
       //** begin add ***  03/22/13  *****************************************
          Close FMFNL1;
       //** end add *****  03/22/13  *****************************************
          subscriber#=FNsub#;
          group#=CMfnd;
          year#=CMfYr;
          case#=CMcase;
          eomb#=0;
          ProviderBills(call:subscriber#:group#:year#:case#:
           eomb#:cert#);
       //
       // Call Medical Cases
       When Option='MedicalCases';
          //*** Beg Del *** 03/03/15 ***************************************
          //****** call='W'; **********************************************
          //*** End Del *** 03/03/15 ***************************************
          //**** Beg Add ***** 06/06/14 **************************************
          GetClientId(clientId);
       //** beg chg ***  12/09/14  *****************************************
       //**********   If clientId = '129'; *********************************
          //*** Beg Del *** 03/03/15 ***************************************
          //*** If clientId = '129' or clientId = '111';
          //*** End Del *** 03/03/15 ***************************************
       //** End chg ***  12/09/14  *****************************************
          //*** Beg Del *** 03/03/15 ***************************************
          //*****   call = ' ';
          //***** Endif;
          //*** End Del *** 03/03/15 ***************************************
          //**** End Add ***** 06/06/14 **************************************
          //*** Beg Add *** 03/03/15 ***************************************
          If mode='D';
             call='W';
          Else;
             call=' ';
          Endif;
          //*** End Add *** 03/03/15 ***************************************
       //** begin add ***  03/22/13  *****************************************
          Open FMFNL1;
       //** end add *****  03/22/13  *****************************************
          Chain keyFMFNL1 pfn01;
       //** begin add ***  03/22/13  *****************************************
          Close FMFNL1;
       //** end add *****  03/22/13  *****************************************
          subscriber#=FNsub#;
          group#=CMfnd;
          year#=CMfYr;
          case#=CMcase;
          socSec#=' ';
          accDate=0;
          returnCode=' ';
          MedicalCases(call:subscriber#:group#:year#:case#:
           socSec#:accDate:returnCode);
       //
       // Call Print FROI's
       When Option='PrintFROI';
          company#=CMco#;
          jurState=CMjrSt;
          //*** Begin Chg *** 06/13/17 ****************************
          //***tranId='RAT';
          tranId='148';
          //*** End   Chg *** 06/13/17 ****************************
          group#=CMfnd;
          returnCode=*Blanks;

          GetControlInfo(company#:jurState:tranId:group#:returnCode:ediFlag:
           IXeLt:IXeLtD:IXfroi:IXsDue:IXrls:IXcsDt:IXeaDc:IXhcPr:IXfnRc:IXhcEd:
           IXcrEr:IXceIr:IXceCr:IXctAr:IXctRr:IXmo:IXmLtD);
          If returnCode='Y' And IXfroi<>*Blanks;
             jurState=IXfroi;
          Else;
             jurState=CMjrSt;
          EndIf;
          stateKey=CMjrSt;
          Chain stateKey pst01;
          If %Found(STSTL1) And ST1a6='Y';
             exit=*Off;
             cancel=*Off;
             mode=' ';
             GetClientId(clientId);
             If clientId='133' And CMjrSt='LA';
                froiPrintPgm='W0070RLU';
                FroiPrint(exit:cancel:mode:claimRrn:jurState);
             Else;
                If jurState<>' ';
                   froiPrintPgm='W0070R31';
                   FroiPrint(exit:cancel:mode:claimRrn:jurState);
                Else;
                   // Needs to call some window like WDWNOI in W0070R
                EndIf;
             EndIf;
          Else;
             exit=*Off;
             cancel=*Off;
             mode=' ';
             info1=jurState;
             info2=*Blanks;
             number1=*Zeros;
             number2=*Zeros;
             date1=*Blanks;
             date2=*Blanks;
             call='3';
             returnCode=*Blanks;
             DisplayMessage(exit:cancel:mode:claimRrn:info1:info2:number1:
              number2:date1:date2:call:returnCode);
          EndIf;
       // Call Delete Claims
       When Option='DeleteClaim';
          exit=*Off;
          cancel=*Off;
          delClmVal(claimRrn:isCancel);
          If isCancel = *off;
             mode='E';
             If wcmcInst='Y' And wcmcInt='Y';
                strCompany#=%editc(CMco#:'X');
                strGroup=%editc(CMfnd:'X');
                strYear=%editc(CMfyr:'X');
                strCase=%editc(CMcase:'X');

                GetCaseRrn(strCompany#:strGroup:strYear:strCase:intCaseRrn:
                 returnCode);
                If returnCode <> 'F';
                   returnCode=' ';
                   fromName='FAT3PQ';
                   toName='FAT3PQ';
                   createWRKF(returnCode:fromName:toName);

                   returnCode=' ';
                   fromName='FAT3LQ';
                   toName='FAT3LQ';
                   createWRKF(returnCode:fromName:toName);

                   //Now you call F7090R to do the delete
                   call=' ';
                   cancel=*blanks;
                   intCaseRrn1=*Zeros;
                   intCaseRrn2=*Zeros;
                   intCaseRrn3=*Zeros;
                   strTranDel='D';
                   strFlag='X';
                   strWcms='Y';

                   delClmMc(call:cancel:intCaseRrn:intCaseRrn1:intCaseRrn2:
                    intCaseRrn3:strTranDel:strFlag:strWcms);
                Endif;
             Endif;
             If cancel=*Off;
                deleteClaim(exit:cancel:mode:claimRrn);
             EndIf;
          EndIf;
       // Payment History
       When Option='PaymentHistory';
          title='Payment History';
          call=*Blanks;
          pmtHistory(title:call:inRRN);
       //** begin add ***  10/01/13  *****************************************
       When Option='Attorney';
          call=*Blanks;
          company=%EditC(CMco#:'X');
          group=%EditC(CMfnd:'X');
          year=%EditC(CMfYr:'X');
          case=%EditC(CMcase:'X');
          seq#=0;
          returnCode=*Blanks;
          Attorney(call:company:group:year:case:seq#:returnCode);

       When Option='ReserveWorksheet';
          //****** beg chg ****** 12/01/14 ********************************
          // exit='0';
          // cancel='0';
          // mode='C';
          // RsvWorkSheet(exit:cancel:mode:claimRrn);

          //*** Begin Add *** 06/18/15 ************************************
          If clientID = '025';
             exit='0';
             cancel='0';
             mode='C';
             RsvWorkSheet(exit:cancel:mode:claimRrn);
          Else;
          //*** End   Add *** 06/18/15 ************************************
             If mode <> 'C' and mode <> 'D';
                mode = 'C';
             Endif;
             @@TabIndex = '00000';
             @@Next     = 'WTCLMRWK';

             DriverPgm(inRrn:mode:@@Next:@@TabIndex);
          //*** Begin Add *** 06/18/15 ************************************
          EndIf;
          //*** End   Add *** 06/18/15 ************************************
          //****** end chg ****** 12/01/14 ********************************

       When Option='WageCalculation';
          exit='0';
          cancel='0';
          //*** Begin Chg *** 02/10/14 *************
          //mode=' ';
          //*** Begin Delete 07/07/15 **************
          //*** If mode='D';
          //***   call='1';
          //*** Else;
          //***   call=' ';
          //**  Endif;
          //*** End   Delete 07/07/15 **************
          //*** End   Chg *** 02/10/14 *************
          //*** Begin Add    07/07/15 **************
          If mode='D';
             mode='1';
          Else;
             mode=' ';
          Endif;
          //*** End   Add    07/07/15 **************
          WageCalc(exit:cancel:mode:claimRrn);

       When Option='SeasonalAdjustment';
          exit='0';
          cancel='0';
          mode='C';
          SeasonAdj(exit:cancel:mode:claimRrn);

       When Option='LifetimeBenefit';
          call=*Blanks;
          company=%EditC(CMco#:'X');
          group=%EditC(CMfnd:'X');
          year=%EditC(CMfYr:'X');
          case=%EditC(CMcase:'X');
          returnCode=*Blanks;
          LifetBen(call:company:group:year:case:returnCode);

       //** end add *****  10/01/13  *****************************************
       //*** Begin Add *** 04/05/21 ****************************************
       When Option  = 'MultipleClaim';
          inPrDs = 'Multiple Claim Maintenance';
          inCall = '0';
          inMltRrn = %Dec(inRRN:9:0);
          MultiClaim(inPrDs:inCall:inMltRrn);
       //*** End   Add *** 04/05/21 ****************************************
       //*** Beg Add *** 03/04/23 *** WC2430 *******************************
       When Option  = 'RecoveryTracking';
          prds     = ' ';
          mode     = ' ';
          year#    = CMfyr;
          case#    = CMcase;
          request# = 0;
          group#   = CMfnd;
          rtcd     = ' ';
          pClmRrn = claimRrn ;
          RecoveryTrack(prds:mode:year#:case#:request#:rtcd:group#:pClmRrn);

       When Option  = 'Payments';
            pMode   = 'P' ;
            pClmRrn = claimRrn ;
          Payments(pMode: pClmRrn: pPmtRrn: pTaxId: pModifier: pFromDt: pToDt:
                   pEOB#: pRecon#: pSysType: pBillType: pPayment: pTransCode:
                   pPayStatCode: pPayValue1: pPayValue2: pRecvDt:pReturnCode);

       When Option  = 'SingleAdjustment';
            pMode   = 'S' ;
            pClmRrn = claimRrn ;
          Payments(pMode: pClmRrn: pPmtRrn: pTaxId: pModifier: pFromDt: pToDt:
                   pEOB#: pRecon#: pSysType: pBillType: pPayment: pTransCode:
                   pPayStatCode: pPayValue1: pPayValue2: pRecvDt:pReturnCode);
       //*** End Add *** 03/04/23 *** WC2430 *******************************
       EndSl;

       Return;

      /End-Free
