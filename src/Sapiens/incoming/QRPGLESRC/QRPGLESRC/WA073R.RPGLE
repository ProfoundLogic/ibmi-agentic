     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H*/TITLE WA073R - API Write EFT holding file
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
    H*********************************************************************
    H*  Description: API Write to EFT holding file                       *
    H*                                                                   *
    H*  Called from: W7013R, W3030R1, W1261R                             *
    H*                                                                   *
    H*  Calls to: None                                                   *
    H*                                                                   *
    H*  Created: C:191  Project: WC1806  Dated: 06/29/09                 *
    H*********************************************************************
    H*  Revisions:                                                       *
    H*                                                                   *
    H* 03/21/25 C:079 Support: 152/118722                                *
    H*                Recompile and install to fix Non Group VPay issue  *
    H*                                                                   *
    H* 04/19/21 C:079 Support: 90/95537                                  *
    H*                Add Bank Code to API                               *
     H*                                                                   *
     H* 08/13/20 C:62  Project WC2749                                     *
     H*                Add V-Pay                                          *
     H*                                                                   *
     H* 09/01/15 C:33  36667                                              *
     H*                Remove WF0 hardcoding.                             *
     H*                                                                   *
     H* 08/25/15 C:136 Support: 36153                                     *
     H*                Account for bank code for claim records            *
     H*                                                                   *
     H*  06/03/15 C:136 Project: MC396                                    *
     H*                 Add processing to return next available EFT       *
     H*                 sequence # for agent and policy.                  *
     H*                                                                   *
     H*  05/31/12 C:191 Project: WC2044                                   *
     H*                 Remove Chk# from EFT                              *
     H*                                                                   *
     H*  04/17/12 C:255 Project: WC2044                                   *
     H*                 Used EFTC= 'E' instead of KKCKTP = 'EFT'.         *
     H*                 Called GetBankAcc procedure instead of WA023R.    *
     H*                                                                   *
     H*  04/13/12 C:255 Project: 114WDE Priss#37                          *
     H*                 Added call of SrGetBnk in SrPrcClm subroutine.    *
     H*                                                                   *
     H*  03/12/12 C:255 Project: 114WDE                                   *
     H*                 Modify program to do following.                   *
     H*                 1) Remove coding associated 78466.                *
     H*                 2) Translate Bank Name to upper case.             *
     H*                                                                   *
     H*  01/19/12 C:255 Project: WC2112                                   *
     H*                 Use New alpha fields for processing Immediate     *
     H*                 destination and origin values.                    *
     H*                                                                   *
     H*  11/10/11 C:191 Support: WA114 78466                              *
     H*                 Temp fixes for CSI:                               *
     H*                 Make batch header company name UPPER case for 114 *
     H*                 Do not insert blanks in immediate origin for 114  *
     H*                 Make all alpha caps                               *
     H*                 Debit and credit transactions to have different   *
     H*                 company id                                        *
     H*                                                                   *
     H*  04/12/11 C:136 Support: WA025 76529                              *
     H*                 Implement 25WQY from V9                           *
     H*                                                                   *
     H*  08/31/10 C:136 Support: WA146 74073                              *
     H*                 Correct formatting for FEIN so that leading       *
     H*                 zeroes are not suppressed.                        *
     H*                                                                   *
     H*  06/22/10  C:136 Support: WA139 73291                             *
     H*                 -Ensure that a blank is written to position 14 of *
     H*                  the file header except for GIC and SCS. Refer to *
    H*                  V9R1PNSRC/W1261R for more details.               *
    H*                 -Ensure that the Immed Org # is used instead of   *
    H*                  the FEIN for the Company ID (EFCID). This        *
    H*                  functionality should have carried over from      *
    H*                  V9R1PNSRC/W1261R (Support item 65401) to the     *
    H*                  V10 EFT process. If GIC requests a change to this*
     H*                  in the future, a project should be requested.    *
    H*********************************************************************
    F*  file definition
     Fwaefp     o    e             disk
    F*
     Fwdpap     if   e             disk
     Fwar2p     if   e             disk
     Fwdeap     if   e             disk
     Fwmefp     uf   e             disk    rename(pef01:pef01m)
     F                                     prefix(mf:2)
    F*
     Fwsckl     if   e           k disk
     Fwmfnl     if   e           k disk
     Fwdf2l     if   e           k disk
     Fsmcol     if   e           k disk
     Fwtgtl     if   e           k disk
      *** Begin Add *** 08/13/20 ***************************************
     Fwdrpl3    if   e           k disk    Rename(prp01:prp03)
     Fwmcml1    if   e           k disk
     Fwmeml     if   e           k disk    Rename(pem01:pem00)
      *** End   Add *** 08/13/20 ***************************************
    F*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields
    D*---------------------------------------------------------------------
      *
     D @@type          s              1
     D @@rrn           s              9  0
     D @@rrn1          s              9  0
     D @@rtn           s              1
     d @@nxtEFT#       s              7  0
      *
     D xxbdt           s              7  0 inz                                  Batch Date
     D xxamt           s             10  2 inz
     D xxpre           s              2    inz
     D xxtrcd          s              2    inz
     D xxtrtp          s              1    inz
     D isApproved      s               N   inz
     D isPrenote       s               N   inz
     D isUpdEFT        s               N   inz
      *
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
      *
     D kkco#           s              3  0
     D kkfnd           s              3  0
     D kkappl          s              2
     D kkcktp          s             10
     D kkacct          s             20
     D strKey          s                   like(gtkey)
     D strtbid         s                   like(gttbid)
     D strUpper        s              1
     D strCrdCompId    s             10
     D strDebCompId    s             10
     D*** Begin Add *** 08/13/30 *************************************
     d isVPay          s               n   Inz(*Off)
     d vendor          s              5
     d payerID         s             50
     D inPaymentID     s            100

     D* Claims
     D inClaim#        s             20
     D inMemberName    s             50
     D inPolicyNumber  s            100
     D inAccidentDate  s             10
     D inAdjusterName  s             50
     D inPaymentPrd    s             25
     D inWeeksDays     s              6
     D inPV1Desc       s             40
     D inRsvDesc       s             40
     D inMemo1         s             40
     D inMemo2         s             40
     D inMemo3         s             40
     D inMemo4         s             40

     D* Payee Fields
     D inPayeeType     s              1
     D inPayeeName     s            100
     D inPayeeAdr1     s             40
     D inPayeeAdr2     s             40
     D inPayeeAdr3     s             40
     D inPayeeCity     s             50
     D inPayeeState    s              3
     D inPayeeCntry    s              3
     D inPayeeZip      s             15
     D inPayeeFein     s              9

     D* Payment Fields
     D inClientRefID   s             16
     D inPayerID       s             50
     D inPayerTime     s             26
     D inPaymentAmt    s             18
     D inPaymentCurr   s              3
     D inRemitMethod   s             10
     D inJurState      s              3
     D*** End   Add *** 08/13/30 *************************************
      *
     D chkTC           s              2    dim(4) ctdata perrcd(1)
     D chkDS           s             50    dim(4) alt(chkTC)
     D savTC           s              2    dim(4) ctdata perrcd(1)
     D savDS           s             50    dim(4) alt(savTC)
      *
     D mdyDate         s               d   datfmt(*mdy)
     D LO              c                   const('abcdefghijklmnopqrstuvwxyz')
     D HI              c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
    D*---------------------------------------------------------------------
    D* Data Structures
    D*---------------------------------------------------------------------
      *
     D psds          esds                  extname(szq1p)
      *
     D                 DS
     Ddsdttm                         14  0
     Ddstime                          6  0 overlay(dsdttm)
     Ddsdate                          8  0 overlay(dsdttm:7)
     D/copy WPRBANK
     D*** Begin Add *** 08/13/30 *************************************
     D/copy WPREPAY
     D/copy SPRCOMPANY
     D/copy SPRDATE
     D/copy WPRTABLE
     D/copy WPRUSRPRMS

     d GenerateEOB     PR                  ExtPgm('F0346C')
     d  faeopRRN                      9
     d  adjState                      2
     d  docPath                     255

     DGetAdjState      pr                  EXTPGM('FA787R')
     D inCo#                          3  0
     D inFnd                          3  0
     D inFyr                          3  0
     D inCase                         7  0
     D OutAdjState                    2

     D GetFaeopRrn     pr                  EXTPGM('FA788R')
     D inCo#                          3  0
     D inFnd                          3  0
     D inFyr                          3  0
     D inCase                         7  0
     D inEob#                         9  0
     D OutFaeopRrn                    9
     D*** End   Add *** 08/13/30 *************************************
     d ##co#           s              3
     d ##fnd           s              3
     d ##rtcd          s              1
     d ##call          s              1
     d ##eftc          s              1
     d kkeftc          s              1
     d ##appl          s              2
     d ##fyr           s              3
     d @@coid          s             20
     d ##chkq          s             10
     d ##ftyp          s             10
     d ##hdcy          s              1
     d ##cpyq          s             10
     d ##efdt          s              7
     d ##srdt          s              8
     d ##acct          s             20
     D ##cktp          s             10
     D*** Begin Add *** 08/13/20 *******************************************
     d faeopRRN        s              9
     d adjState        s              2
     d docPath         s            255
     d documentID      s             50
     d format          s                   Like(GTcf01)
     D*** End   Add *** 08/13/20 *******************************************
      *
    C*---------------------------------------------------------------------
    C* PLIST - Definitions
    C*---------------------------------------------------------------------
    C*
     C*** Begin Del *** 08/13/20 *******************************************
     C*****dtcymd        plist
     C**********         parm                    srdti
     C**********         parm      *blanks       srffmt
     C**********         parm      '*CYMD'       srtfmt
     C**********         parm                    srdto
     C**********         parm                    srerr
    C*
     C*****dtmdyy        plist
     C**********         parm                    srdti
     C**********         parm      '*YYMD'       srffmt
     C**********         parm      '*MDYY'       srtfmt
     C**********         parm                    srdto
     C**********         parm      *BLANK        srerr
     C*** End   Del *** 08/13/20 *******************************************
      *
     C     dty1md        plist
     C                   parm                    srdti
     C                   parm      '*YYMD'       srffmt
     C                   parm      '*YMD'        srtfmt
     C                   parm                    srdto
     C                   parm      *BLANK        srerr
      *
    C*---------------------------------------------------------------------
    C* KLIST - Definitions
    C*---------------------------------------------------------------------
      *
     C     ckky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkacct
      *
     C     fnky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
      *
     C     keyWTGTL      Klist
     C                   Kfld                    kkco#
     C                   Kfld                    strTbId
     C                   Kfld                    strKey
    C*---------------------------------------------------------------------
    C* MAINLINE
    C*---------------------------------------------------------------------
      *
     C     *entry        plist
     C                   parm                    @@type                         EFT type
     C                   parm                    @@rrn                          RRN of payment rec
     C                   parm                    @@rrn1                         RRN of EFT A/C file
     C                   parm                    @@rtn                          retun status
     C                   parm                    @@NxtEft#
      *                                                              ' ' - Normal function
      *                                                              '1' - Bank Info rec error
      *                                                              '2' - Account Master rec error
      *                                                              '3' - Approved/Valid EFT not fo
     c                   call      'RTVCLTID'
     c                   parm                    cltid             3
     C                   clear                   pef01
     C                   eval      @@rtn = ' '
      *
     C                   if        @@type = 'X'
      * Called to clean up - seton LR and return to caller
     C                   eval      *inlr = *on
     C                   return
     C                   endif                                                  @@type = 'X'
     c*** Begin Add *** 08/13/20 **************************************
      /free
       // If this is V-Pay then need to get the vendor.
       isVpay = *Off;
       Select;
       When @@Type = 'A';
          Chain(n) @@rrn pr201;
          kkCo#  = R2co#;
          kkAcct = R2acct;
          kkFnd  = R2fnd;
          kkAppl = 'AG';
       // Is this a Vpay payment
          If R2eftC = 'V';
             isVpay = *On;
             vendor = GetEpayVendor(R2co#:R2acct);
             payerID =
        //*** Beg Chg *** 04/19/21 ***************************************
        //*** GetPayerID(R2co#:'AG':R2fnd:R2acct:DateTo7(systemDate()));
              GetPayerID(R2co#:'AG':R2fnd:R2acct:DateTo7(systemDate()):
               R2ckTp);
        //*** End Chg *** 04/19/21 ***************************************
          EndIf;
       When @@Type = 'C';
          Chain(n) @@rrn ppa01;
          kkCo#  = PAco#;
          kkAcct = PAacct;
          kkFnd  = PAfnd;
          kkAppl = 'WC';
          If PAeftC = 'V';
             isVpay = *On;
             vendor = GetEpayVendor(PAco#:PAacct);
             payerID =
        //*** Beg Chg *** 04/19/21 ***************************************
        //*** GetPayerID(PAco#:'WC':PAfnd:PAacct:DateTo7(systemDate()));
              GetPayerID(PAco#:'WC':PAfnd:PAacct:DateTo7(systemDate()):
               PAckTp);
        //*** End Chg *** 04/19/21 ***************************************
          EndIf;
       When @@Type = 'E';
          Chain(n) @@rrn pea01;
          kkCo#  = EAco#;
          kkFnd  = EAfnd;
          kkappl = 'WP';
          ExSr srGetBnk;
          If EAachF = 'V';
             isVpay = *On;
             vendor = GetEpayVendor(EAco#:##acct);
             payerID =
        //*** Beg Chg *** 04/19/21 ***************************************
        //*** GetPayerID(EAco#:'WP':EAfnd:##acct:DateTo7(systemDate()));
              GetPayerID(EAco#:'WP':EAfnd:##acct:DateTo7(systemDate()):
               ##CkTp);
        //*** End Chg *** 04/19/21 ***************************************
          EndIf;
       EndSl;
      /end-free
     c*** End   Add *** 08/13/20 **************************************
      *
      * get EFT file record
     c*** Begin Add *** 08/13/20 **************************************
      /free
       If not isVPay;
      /end-free
     c*** End   Add *** 08/13/20 **************************************
     C     @@rrn1        chain     pef01m
     C                   if        not %found
     C                   eval      @@rtn = '3'
     C                   return
     C                   endif                                                  not %found
     c*** Begin Add *** 08/13/20 **************************************
      /free
       EndIf;
      /end-free
     c*** End   Add *** 08/13/20 **************************************
      *
      * Check type to determine process to run
     C                   select
     C                   when      @@rrn = *zeros
      * Called from w/w EFT account to create pre-note
     C                   exsr      srPreNote
     C                   when      @@type = 'A'
      * Process for Agent EFT
     C                   exsr      srPrcAgt
     C                   when      @@type = 'C'
      * Process for Claims/Provider EFT
     C                   exsr      srPrcClm
     C                   when      @@type = 'E'
      * Process for Employer ACH
     C                   exsr      srPrcEmp
     C                   other
     C                   eval      @@rtn = '9'                                  unknown type
     C                   eval      *inlr = *on
     C                   return
     C                   endsl
      *
     C                   if        not *in90
     C*** Begin Add *** 08/13/20 *******************************************
      /free
       If isVPay = *On;
          Select;
          When @@Type = 'A';
             Select;
             When vendor = 'VPAY';
                ExSr WritePaymentRcdAgency;
                ExSr WritePayeeRcdAgency;
             EndSl;
          When @@Type = 'C';
             Select;
             When vendor = 'VPAY';
                ExSr WritePaymentRcdClaims;
                ExSr WritePayeeRcdClaims;
                ExSr WriteClaimRcd;
             EndSl;
          When @@Type = 'E';
             Select;
             When vendor = 'VPAY';
                ExSr WritePaymentRcdPolicy;
                ExSr WritePayeeRcdPolicy;
             EndSl;
          EndSl;
       Else;
      /end-free
     C*** End   Add *** 08/13/20 *******************************************
     C                   exsr      srPrcEft
      *
     C                   if        isApproved = *on
      * Respective file record processed, write to central holding file
     C                   exsr      srWrite
      /free
       If Q1prms > 4;
          @@NxtEft# = GetNextEFT#(kkCo#:##acct);
       Endif;
      /end-free
     C                   endif                                                  isApproved
     C*** Begin Add *** 08/13/20 *******************************************
      /free
       EndIf;
      /end-free
     C*** End   Add *** 08/13/20 *******************************************
     C                   endif                                                  not *in90
      *
      * Return to calling program
     C                   return
      *
     C*** Begin Add *** 08/13/20 *******************************************
      /free
       //*==================================================================
       //* Print Eob and convert to PDF
       //*==================================================================
       BegSr PrintEob;

          // Get the EOBs RRN
          If PAxFyr<>0 Or PAxCas<>0;
             GetFaeopRrn(PAco#:PAfnd:PAxfYr:PAxCas:PAeob#:faeopRRN);
             If faeopRrn='000000000';
                GetFaeopRrn(PAco#:PAfnd:PAfYr:PAcase:PAeob#:faeopRRN);
             Endif;
          Else;
             GetFaeopRrn(PAco#:PAfnd:PAfYr:PAcase:PAeob#:faeopRRN);
          EndIf;

          // Get the Eob's Adjudication State
          GetAdjstate(PAco#:PAfnd:PAfYr:PAcase:adjState);

          // Create the path statement
          docPath = %Trim(GetEPayOutFolderForDocuments(vendor:
                DateTo7(systemDate()))) + '/' + documentID;

          // Create the EOB PDF
          GenerateEOB(faeopRRN:adjState:docPath);

       EndSr;
      /end-free
     C*** End   Add *** 08/13/20 *******************************************
    C*---------------------------------------------------------------------
    C* srPrcAgt - Process agent EFT
    C*---------------------------------------------------------------------
     C     srPrcAgt      begsr
      *
     C                   eval      *in90 = *off
      * get commission payment record
     C     @@rrn         chain(N)  pr201
      *
     C                   eval      kkco# = r2co#
     C                   eval      kkacct = r2acct
     C                   eval      kkfnd = r2fnd
      * Retrieve bank account number from Group Bank info, use application as 'WP'
      /Free
       kkAppl = 'AG';

       //Get Bank account from API
       Exsr      srGetBnk;
       If  ##rtcd = '1' ;
          *in90 = *on;
          @@rtn = '1';
          Leavesr;
       Endif;
       //*** Begin Add *** 08/13/20 **************************************
       //*
       //* Don't process further if V-Pay
       If isVpay;
          LeaveSR;
       EndIf;
       //*** End   Add *** 08/13/20 **************************************
      /End-Free
      *
      * load holding file variables
     C                   eval      eftype = @@type
     C                   eval      efacct = r2acct
     C                   move      r2podt        efcddt
      *
     C                   eval      xxamt  = r2amt
      * Set transaction type credit/debit
      * +ve commission is credit
     C                   eval      xxtrtp = 'C'
     C                   if        xxamt < 0
     C                   eval      xxtrtp = 'D'
     C                   endif                                                  xxamt < 0
      *
     C                   monitor
     C                   eval      mdyDate = %date(R2PODT : *CYMD)
     c                   on-error
     c                   eval      mdyDate = *loval
     c                   endmon
     C                   eval      efprin = %editc(R2FND  : 'X') + '/' +
     C                                      %editc(R2AGT# : 'X') + ' ' +
     c                                      'PODT: ' +
     C                                      %char(mdyDate) + ' ' +
     c                                      'Amt: ' +
     C                                      %editc(R2AMT  : 'J')
      *
     C                   endsr
      *
    C*---------------------------------------------------------------------
    C* srPrcClm - Process claims/provider EFT
    C*---------------------------------------------------------------------
     C     srPrcClm      begsr
      *
     C                   eval      *in90 = *off
      * get payment record
     C     @@rrn         chain     ppa01
      *
     C                   eval      kkco# = paco#
     C                   eval      kkacct = paacct
     C                   eval      kkfnd = pafnd
     C                   eval      kkcktp = PAcktp
      * Retrieve bank account number from Group Bank info, use application as 'WP'
      /Free
       kkAppl = 'WC';
       //kkcktp = 'EFT';

       //Get Bank account from API
       Exsr      srGetBnk;
       If  ##rtcd = '1' ;
          *in90 = *on;
          @@rtn = '1';
          Leavesr;
       Endif;
       //*** Begin Add *** 08/13/20 **************************************
       //*
       //* Don't process further if V-Pay
       If isVpay;
          LeaveSR;
       EndIf;
       //*** End   Add *** 08/13/20 **************************************
      /End-Free
      *
      * load holding file variables
     C                   eval      eftype = @@type
     C                   eval      efacct = paacct
     C                   eval      efcdta = *blanks
     C                   move      papodt        efcddt
      *
     C                   eval      xxamt  = paamt
      * Set transaction type credit/debit
      * +ve payment is credit
     C                   eval      xxtrtp = 'C'
     C                   if        xxamt < 0
     C                   eval      xxtrtp = 'D'
     C                   endif                                                  xxamt < 0
      *
     C                   eval      eftrc# = 0
     C                   monitor
     C                   eval      mdyDate = %date(PATRDT : *CYMD)
     c                   on-error
     c                   eval      mdyDate = *loval
     c                   endmon
     C                   eval      efprin = %editc(PAFND  : 'X') + '/' +
     C                                      %editc(PAFYR  : 'X') + '/' +
     C                                      %editc(PACASE : 'X') + ' ' +
     c                                      'Seq#: ' +
     C                                      %editc(PASEQ# : 'X') + ' ' +
     c                                      'PODT: ' +
     C                                      %char(mdyDate)  + ' ' +
     c                                      'Amt: ' +
     C                                      %editc(PAAMT  : 'J')
      *
     C                   endsr
    C*---------------------------------------------------------------------
    C* srPrcEmp - Process Employer ACH
    C*---------------------------------------------------------------------
     C     srPrcEmp      begsr
      *
     C                   eval      *in90 = *off
      * get employer AR file
     C     @@rrn         chain     pea01
      *
     C                   eval      kkco# = eaco#
     C                   eval      kkfnd = eafnd
      *
      * Retrieve bank account number from Group Bank info, use application as 'WP'
     C                   eval      kkappl = 'WP'
      * get bank account from api
     C                   exsr      srGetBnk
     C                   if        ##rtcd = '1'
     C                   eval      *in90 = *on
     C                   eval      @@rtn = '1'
     C                   leavesr
     C                   endif
      *
     C                   eval      kkacct = ##acct
     C*** Begin Add *** 08/13/20 **************************************
      /free
       //*
       //* Don't process further if V-Pay
       If isVpay;
          LeaveSR;
       EndIf;
      /end-free
     C*** End   Add *** 08/13/20 **************************************
      *
      * load holding file variables
     C                   eval      eftype = @@type
     C                   eval      efacct = kkacct
     C                   move      eapodt        efcddt
      *
     C                   eval      xxamt  = eaamt
      * Set transaction type credit/debit
      * +ve premium is debit
     C                   eval      xxtrtp = 'D'
     C                   if        xxamt < 0
     C                   eval      xxtrtp = 'C'
     C                   endif                                                  xxamt < 0
      *
     C                   monitor
     C                   eval      mdyDate = %date(EAPODT : *CYMD)
     c                   on-error
     c                   eval      mdyDate = *loval
     c                   endmon
     C                   eval      efprin = %editc(EAFND : 'X')  + '/' +
     C                                      %editc(EAEMP#: 'X')  + '/' +
     C                                      %editc(EADIV : 'X')  + '/' +
     C                                      %editc(EAFYR : 'X')  + ' ' +
     c                                      'Bat#: ' +
     C                                      %editc(EABAT#: 'X')  + ' ' +
     c                                      'PODT: ' +
     C                                      %char(mdyDate)  + ' ' +
     C                                      'A/R Cd: '  +
     c                                      EAARCD + ' ' +
     c                                      'Amt: ' +
     C                                      %editc(EAAMT  : 'J')
      *
     C                   endsr
      *
    C*---------------------------------------------------------------------
    C* srPrcEft - Check EFT record for Valid EFT and Prenote
    C*---------------------------------------------------------------------
     C     srPrcEft      begsr
     C*
     C                   eval      isApproved = *on
     C                   eval      isPrenote = *off
     C                   eval      isUpdEFT = *off
      *
      * get bank account master record
     C     ckky          chain     pck01
      /Free
       strKey = ckefsp;
       strTbId = 'BNK';

       Clear strUpper;
       Clear GTfg01;
       Chain  keyWTGTL  pgt01 ;
       If  %Found(wtgtl);
          If GTfg01 = 'U' ;
             strUpper = 'Y' ;
          Endif;
       Endif;
      /end-Free
     C                   if        not %found (wsckl) or
     C                             (ckidsa = '0000000000' or ckidsa= *blanks)
     C                             and
     C                             (ckidst =  0)
     C                   eval      isApproved = *off
     C                   eval      @@rtn = '2'
     C                   leavesr
     C                   endif
      *
      * Do not check pre-note date if Bypass prenote processing flag is set
     C                   if        ckbypn <> 'Y'
     C                   select
      *
     C* Skip transaction if pre-note is sent or approval is not recieved
     C* Skip pre-note transaction if pre-note is already sent
     C* Skip payment transaction if pre-note is not sent
     C                   when         (mfpdt <> 0  and mfadt  = 0)
     C                             or (mfpdt <> 0  and xxamt  = 0)
     C                             or (mfpdt  = 0  and xxamt <> 0)
     C                   eval      isApproved = *off
     C                   eval      @@rtn = '3'
      *
     C* Update pre-notification date if pre-note transaction (amount =0) is being sent
     C                   when      mfpdt = 0  and xxamt = 0
     C                   eval      mfpdt = xxbdt
     C                   eval      isPrenote = *on
     C                   eval      isUpdEFT = *on
     C                   endsl
     C                   endif                                                  ckbypn<>'Y'
      *
      * Checking or Savings account - Assign Transaction codes
     C                   if        mfactp = 'C'
      * Checking Account
      * Debit or Credit transaction
     C                   if        xxtrtp = 'D'
      * Checking Debit transaction codes 28 and 27
     C                   eval      xxpre = chkTC(4)
     C                   eval      xxtrcd = chkTC(3)
     C                   else
      * Checking Credit transaction codes 23 and 22
     C                   eval      xxpre = chkTC(2)
     C                   eval      xxtrcd = chkTC(1)
     C                   endif                                                  xxtrtp='D'
     C                   else
      * Savings Account
     C                   if        xxtrtp = 'D'
      * Savings Debit transaction codes 38 and 37
     C                   eval      xxpre = savTC(4)
     C                   eval      xxtrcd = savTC(3)
     C                   else
      * Savings Credit transaction codes 33 and 32
     C                   eval      xxpre = savTC(2)
     C                   eval      xxtrcd = savTC(1)
     C                   endif                                                  xxtrtp='D'
     C                   endif                                                  mfactp='C'
      *
      * Load EFT central holding file fields
     C                   if        isPrenote
     C
     C* Prenotification
     C                   eval      eftrcd = xxpre
     C                   else
     C                   eval      eftrcd = xxtrcd
     C                   endif                                                  isPrenote
      *
     C                   eval      efdfi  = mfid#
     C                   eval      efckdg = mfdgit
     C                   eval      efdact = mfacct
     C                   movel(p)  mffein        efid#
     C                   eval      efinam = mfpnam
      *
      * Update dates of first and last transfer if transaction is approved and
      * not sending prenote transaction
     C                   if        isApproved = *on
     C                             and isPrenote = *off
      * Update dates of first and last transfer
     C                   if        mfftdt = 0
     C                   eval      mfftdt = xxbdt
     C                   endif                                                  mfftdt = 0
     C                   eval      mfltdt = xxbdt
     C                   eval      isUpdEFT = *on
     C                   endif                                                  xxapprvd = *on
      *
     C                   if        isUpdEFT = *on
     C                   eval      mfpgm = q1pgm
     C                   eval      mfuser = q1user
     C                   eval      mfdt = xxbdt
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   eval      mftime = dstime
     C                   update    pef01m
     C                   endif                                                  isUpdEFT
      *
     C                   endsr
    C*---------------------------------------------------------------------
    C* srWrite - Write to central holding file
    C*---------------------------------------------------------------------
     C     srWrite       begsr
      *
     C                   eval      efco# = kkco#
     C                   if        (ckidsa= '0000000000' or ckidsa= *blanks)
     C                             and
     C                             (ckiora= '0000000000' or ckiora= *blanks)
     C                   movel(p)  ckidst        efidst
     C                   movel(p)  ckiorg        efiorg
     c                   movel     ' '           efidst
     c                   select
     c                   when      cltid = '139' or cltid = '146'
     c                   other
     c                   movel     ' '           efiorg
     c                   endsl
     C                   else
     C                   movel(p)  ckidsa        efidst
     C                   movel(p)  ckiora        efiorg
     C                   endif
      *
     C                   eval      efdest = ckbnam
     C                   eval      efcnam = ckconm
     C                   eval      efref  = ckrfcd
      *
     C                   movel(p)  cksvcl        efserv
     C                   movel(p)  ckordi        efbdfi
     C                   eval      efedes = 'WORKCOMP'
      *
      * Get Fund master record for batch header info
     C     fnky          chain     wmfnl
     C                   eval      efbnam = %subst(fnname:1:16)                 Co Group Name
     C                   eval      efcdta = %subst(fnname:17:20)                Rest of Group Name
      /Free
       If strUpper = 'Y';
          EFbnam = %xlate(lo:hi:EFbnam);
          EFcdta = %xlate(lo:hi:EFcdta);
       Endif;
      /End-Free
     c                   if        cltID = '025'
     c     kkco#         chain     smcol
     c                   eval      efbnam = %subst(coanam:1:16)
     c                   eval      efcdta = %subst(coanam:17:20)
     c                   endif
      *
      * Get Fund auxillary record for Fund Fein (Batch Header Co. Indentifier)
     C     fnky          chain     wdf2l
     c                   select
     c                   when      cltid = '139'
     c                   eval      efcid = efiorg
      /Free
       when xxtrtp = 'C' and strCrdCompId <> ' ' ;
          EFcid = strCrdCompId;

       when xxtrtp = 'D' and strDebCompId <> ' ' ;
          EFcid = strDebCompId;
      /End-Free
     c                   other
     c                   eval      efcid =  '1' + %editc(f2fein:'X')
     c                   endsl
      *
      * Entry Effective date will be loaded will writing EFT batch
     C                   eval      efeedt = 0
      *
     C                   move      xxamt         efamt
      *
     C                   eval      eftrc# = 0
      *
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   eval      eftime = dstime
     C                   eval      efdt = xxbdt
     C                   eval      efpgm  = q1pgm
     C                   eval      efuser = q1user
      *
     C                   write     pef01
      *
     C                   endsr
    C*---------------------------------------------------------------------
    C* srPreNote - Process to create pre-note transaction
    C*---------------------------------------------------------------------
     C     srPreNote     begsr
      * Load data for pre-note creation
     C                   eval      eftype = @@type
      *
      * Retrieve bank account number from Group Bank info, set application as per type
     C                   eval      kkco# = mfco#
     C                   eval      kkfnd = mffnd
      *
     C                   select
     C                   when      @@type = 'A'
      * Agent Commission
     C                   eval      kkappl = 'AG'
     C                   eval      xxtrtp = 'C'
     C                   when      @@type = 'C' or @@type = 'D' or @@type = 'P'
      * Claims Payments
     C                   eval      kkappl = 'WC'
     C                   eval      eftype = 'C'
     C                   eval      xxtrtp = 'C'
     C                   when      @@type = 'E'
      * Premium ACH
     C                   eval      kkappl = 'WP'
     C                   eval      xxtrtp = 'D'
     C                   endsl
      *
     C                   exsr      srGetBnk
     C                   if        ##rtcd = '1'
     C                   eval      *in90 = *on
     C                   eval      @@rtn = '1'
     C                   leavesr
     C                   endif
      *
     C                   eval      kkacct = ##acct
     C                   eval      efacct = kkacct
     C                   move      xxbdt         efcddt
     C                   eval      xxamt = 0
     C                   eval      efprin =   'Pre-Note transaction'
     C*
     C                   endsr
    C*---------------------------------------------------------------------
    C* srGetBnk - Get bank API
    C*---------------------------------------------------------------------
     C     srGetBnk      begsr
     C* Call WA023R to retrieve Bank account#
     C                   move      kkco#         ##co#
     C                   move      kkfnd         ##fnd
     C                   move      kkappl        ##appl
     C*** Begin Chg *** 08/13/20 **************************************
      /Free
       // Is this a Vpay payment
       isVpay = *Off;
       Select;
       When @@type = 'A';
          If R2eftC = 'V';
             isVpay = *On;
          EndIf;
       When @@type = 'C';
          If PAeftC = 'V';
             isVpay = *On;
             LeaveSR;
          EndIf;
       When @@type = 'E';
          If EAachF = 'V';
             isVpay = *On;
          EndIf;
       EndSl;

       //*** ##eftc = 'E';
       // Is this Vpay or Not
       If isVpay = *On;
          ##eftC = 'V';
       Else;
          ##eftc = 'E';
       EndIf;
       //*** End   Chg *** 08/13/20 **********************************
      /End-Free
     C*
     C                   time                    dsdttm
     C                   movel     dsdate        ##srdt
     C*
      /Free
          ##acct = *Blanks;
          ##call = *Blanks;
          ##hdcy = *Blanks;
          ##chkq = *Blanks;
          ##ftyp = *Blanks;
          ##rtcd = *Blanks;
          ##efdt = *Zeros ;
          ##cpyq = *Zeros ;
          ##cktp = kkcktp;
          @@coid = *Blanks;

          ##acct= GetBankAcc(##co#: ##fnd: ##appl: ##srdt: ##call:
                   ##hdcy: ##chkq: ##ftyp: ##efdt: ##cpyq:
                   ##cktp: ##eftc: @@coid: ##rtcd);
      /End-Free
      /Free
       strCrdCompId = %subst(@@CoId:1:10);
       strDebCompId = %subst(@@CoId:11:10);
      /End-Free
     C*
     C                   endsr
     C*** Begin Add *** 08/13/20 *******************************************
      /free
       //*==================================================================
       //* WritePayeeRcdAgency
       //*==================================================================
       BegSr WritePayeeRcdAgency;

          CrtVPayPayeeRecord(inPaymentID:inPayeeType:inPayeeName:
            inPayeeAdr1:inPayeeAdr2:inPayeeAdr3:inPayeeCity:inPayeeState:
            inPayeeCntry:inPayeeZip:inPayeeFein);

       EndSr;
       //*==================================================================
       //* WritePayeeRcdClaims
       //*==================================================================
       BegSr WritePayeeRcdClaims;

          If PAfein = 0;
             inPayeeType = 'C';
          Else;
             inPayeeType = 'B';
          Endif;

          Chain (PAco#:PAfnd:PAfYr:PAcase:PAseq#) prp03;
          If %Found(wdrpl3);
             inPayeeName  = RPpNam;
             inPayeeAdr1  = RPadr1;
             inPayeeAdr2  = RPadr2;
             inPayeeAdr3  = RPadr3;
             inPayeeCity  = RPcty;
             inPayeeState = RPst;
             inPayeeCntry = RPctry;
             inPayeeZip   = %EditC(RPzip:'X');
             inPayeeFein  = %EditC(PAfein:'X');
          Else;
             inPayeeName  = ' ';
             inPayeeAdr1  = ' ';
             inPayeeAdr2  = ' ';
             inPayeeAdr3  = ' ';
             inPayeeCity  = ' ';
             inPayeeState = ' ';
             inPayeeCntry = ' ';
             inPayeeZip   = ' ';
             inPayeeFein  = ' ';
          EndIf;

          CrtVPayPayeeRecord(inPaymentID:inPayeeType:inPayeeName:
            inPayeeAdr1:inPayeeAdr2:inPayeeAdr3:inPayeeCity:inPayeeState:
            inPayeeCntry:inPayeeZip:inPayeeFein);

       EndSr;
       //*==================================================================
       //* WritePayeeRcdPolicy
       //*==================================================================
       BegSr WritePayeeRcdPolicy;

          CrtVPayPayeeRecord(inPaymentID:inPayeeType:inPayeeName:
            inPayeeAdr1:inPayeeAdr2:inPayeeAdr3:inPayeeCity:inPayeeState:
            inPayeeCntry:inPayeeZip:inPayeeFein);

       EndSr;
       //*==================================================================
       //* WritePaymentRcdAgency
       //*==================================================================
       BegSr WritePaymentRcdAgency;

          CrtVPayPaymentRecord(inPaymentID:inClientRefID:inPayerID:
            inPayerTime:inPaymentAmt:inPaymentCurr:inRemitMethod:
            inJurState);

       EndSr;
       //*==================================================================
       //* WritePaymentRcdClaims
       //*==================================================================
       BegSr WritePaymentRcdClaims;

          If PAeob# = 0;
             inRemitMethod = 'NONE';
          Else;
             format = GetEpayFormat(vendor:PAtrDt);
             Chain (PAco#:'EPFM':format) pgt01;
             If %Found(wtgtl);
                inRemitMethod = GTcf01;
             Else;
                inRemitMethod = 'NONE';
             EndIf;
          EndIf;

          // Not an EOB Payment
          If PAeob# = 0;
             inPaymentID = %EditC(PAfnd:'X')  + %EditC(PAfYr:'X') +
                           %EditC(PAcase:'X') + %EditC(PAchk#:'X') +
                           %EditC(PAseq#:'X');
          Else;
             Select;
             When inRemitMethod = 'PDF';
                inPaymentID = %EditC(PAeob#:'X') +
                          %Subst(%EditC(PAeobs:'X'):3:3) +
                           %EditC(PAchk#:'X') + %EditC(PAseq#:'X');
                documentID  = %Trim(inPaymentID) + '.PDF';
                ExSr PrintEob;
             Other;
             EndSl;
          EndIf;

          inClientRefID = 'EPAY# ' + %EditC(PAchk#:'X');
          inPayerID     = payerID;
          inPayerTime   = %Subst(%EditC(dtMDYY(PAtrDt):'X'):5:4) + '-' +
                          %Subst(%EditC(dtMDYY(PAtrDt):'X'):1:2) + '-' +
                          %Subst(%EditC(dtMDYY(PAtrDt):'X'):3:2) + '-' +
                          %Subst(%EditC(PAiTim:'X'):1:2) + '.' +
                          %Subst(%EditC(PAiTim:'X'):3:2) + '.' +
                          %Subst(%EditC(PAiTim:'X'):5:2);
          inPaymentAmt  = %EditC(PAamt:'P');
          inPaymentCurr = 'USD';

          Chain (PAco#:PAfnd:PAfYr:PAcase) pcm01;
          If %Found(wmcml1);
             inJurState = CMjrSt;
          Else;
             inJurState = RPst;
          EndIf;

          CrtVPayPaymentRecord(inPaymentID:inClientRefID:inPayerID:
            inPayerTime:inPaymentAmt:inPaymentCurr:inRemitMethod:
            inJurState);

       EndSr;
       //*==================================================================
       //* WritePaymentRcdPolicy
       //*==================================================================
       BegSr WritePaymentRcdPolicy;

          CrtVPayPaymentRecord(inPaymentID:inClientRefID:inPayerID:
            inPayerTime:inPaymentAmt:inPaymentCurr:inRemitMethod:
            inJurState);

       EndSr;
       //*==================================================================
       //* WriteClaimRcd
       //*==================================================================
       BegSr WriteClaimRcd;

          inClaim# = %EditC(CMfnd:'X') + %EditC(CMfYr:'X') +
                     %EditC(CMcase:'X');

          Chain (PAco#:PAfnd:CMemp#:CMdiv) pem00;
          inMemberName   = %Trim(CMfsn) + ' ' + CMlsn;
          inPolicyNumber = %EditC(CMfnd:'X') + %EditC(CMemp#:'X') +
                           %EditC(CMfYr:'X') + ': ' + EMname;
          inAccidentDate = alphaUSA(cvtToDate(CMacDt));
          inAdjusterName = GetUsrName(GetUserId(CMclex));
          inPaymentPrd   = alphaUSA(cvtToDate(PAfrDt)) + '-' +
                           alphaUSA(cvtToDate(PAtoDt));
          inWeeksDays    = %Trim(%EditW(PAwks:' 0 ')) + '/' +
                           %EditC(PAdays:'X');
          inPV1Desc      = BR2CodeDs(PAco#:PAbr2);
          inRsvDesc      = ResTypCdDs(PAco#:PAreCd);
          inMemo1        = RemarkCdDs(PAco#:RPrmk1);
          inMemo2        = RemarkCdDs(PAco#:RPrmk2);
          inMemo3        = RemarkCdDs(PAco#:RPrmk3);
          inMemo4        = RPrmDs;

          CrtVPayClaimRecord(inPaymentID:inClaim#:inMemberName:
             inPolicyNumber:inAccidentDate:inAdjusterName:inPaymentPrd:
             inWeeksDays:inPV1Desc:inRsvDesc:inMemo1:inMemo2:inMemo3:
             inMemo4);


       EndSr;
      /end-free
     C*** End   Add *** 08/13/20 *******************************************
      *
    C*---------------------------------------------------------------------
    C* *inzsr - Initialization
    C*---------------------------------------------------------------------
     C     *inzsr        begsr
      *
     C* get system date
     C                   time                    dsdttm
     C*** Begin Chg *** 08/13/20 *******************************************
     C**********         z-add     dsdate        srdti
     C**********         call      'SRCVTDAT'    dtcymd
     C**********         movel     srdto         xxbdt
      /free
       xxbdt = dtCYMD(dsDate);
      /end-free
     C*** End   Chg *** 08/13/20 *******************************************
      *
     C                   endsr
      *
      *
** Checking account transaction codes
22Credit Transaction - Checking Account                                         Deposit
23Credit Prenotification - Checking Account                                     Deposit Pre-Note
27Debit Transaction - Checking Account                                          Credit
28Debit Prenotification - Checking Account                                      Credit Pre-note
** Savings account transaction codes
32Credit Transaction - Savings Account                                          Deposit
33Credit Prenotification - Savings Account                                      Deposit Pre-Note
37Debit Transaction - Savings Account                                           Credit
38Debit Prenotification - Savings Account                                       Credit Pre-note
