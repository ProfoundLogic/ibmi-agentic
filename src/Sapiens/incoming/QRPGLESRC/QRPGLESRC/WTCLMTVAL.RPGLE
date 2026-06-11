     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Toolbar action validation - perform edits and return error messages.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: Action Panel buttons via Javascript
     h*  Calls to:
     h*
     h*  Created on 10/18/2012 by C:065 for Project Transform - WC2116
     h**********************************************************************
     h* Revisions:
     h*
     H* 04/17/23 C:340 Project: WC2430
     h*                Validate Claim Toolbar actions based on Type/level
     h*                # Payments :
     h*                   Verify User's Auth against new Payment Type/level
     h*                # SingleAdjustment :
     h*                   Verify User'a Auth against new Adjustment Type/level
     h*                # RecoveryTracking :
     h*                   Verify User's Auth against new Recovery Tracking
     h*
     H* 06/20/18 C:291 Project: WC2600
     h*                Department Letter edits are now performed
     h*                in WTPOPUP.
     h*
     H* 11/04/16 C:180 Support: 43803
     h*                Add security to Dept Letters.
     H*
     h* 03/31/16 C:308 Project: Elevate
     h*          C:33  Genericize the toolbar programming
     h*
     h* 10/01/13 C:53  Project: 25WSS
     h*                Add client programs.
     h*
    * 07/22/13 C:189 Redmine: 3441
    *                Work with provider bills and medical cases options
    *                are not checking the user authority
     h*
     h* 04/29/13 C:136 Project: SY307
     h*                Add application security to toolbar selections
     h*
     h*
     h**********************************************************************
     Fwmcmp     if   e             Disk    Rename(pcm01:pcm0p)
     fwmcxl     if   e           k Disk    ReName(pcx01:pcx00)
     fwmfnl     if   e           k Disk    ReName(pfn01:pfn00)
     fwdpal3    if   e           k Disk    ReName(ppa01:ppa03)
     fsdcml     if   e           k Disk    ReName(pcm01:pcm00)
     f********** Beg Add ***** 07/22/13 ***********************************
     fsdofl1    if   e           k disk
     f********** End Add ***** 07/22/13 ***********************************

      /copy sPrDate
      /copy sPrAtrium
      /copy wPrUsrPrms
      *** beg add **** 04/29/13 *************************
      /copy sPrSecure
     d ChkAppSecurity  pr
     d  inAction                     20

     d aCTxRef         s             25    Dim(25) Ctdata Perrcd(1)
     d oFDxRef         s             13    Dim(25) Alt(aCTxRef)
      *** end add **** 04/29/13 *************************

     D***  Begin Add  ***  03/31/16  **************************************
     dExtractParm      pr                  like(inData)
     d inParm                              like(inData) const
     d inDelimitor                    1                 const
     d inStart                        5  0
     D***  End   Add  ***  03/31/16  **************************************

     d* Retrieve System Values
     d RtvSysVal       pr                  ExtPgm('SA001R')
     d   wcmsInst                     1
     d   wcmcInst                     1
     d   wcmcInter                    1

     d* Check Security
     d ChkSecLvl       pr                  ExtPgm('SRCCSC')
     d   ##Utyp                       1
     d   ##Ulvl                       3
     d   ##Ctp1                       1
     d   ##Clvl                       3
     d   ##exec                       1
     d   ##res                        1
     d   ##Ctp2                       1
     d   ##Ctp3                       1
     d   ##Ctp4                       1
     d   ##Ctp5                       1
     d   ##Ctp6                       1

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

     d* Check for payments
     d ChkPayments     pr
     d   paidNot0                      n
     d   currActivity                  n

     d* Has Authority to Delete Claim
     d HasAuthority    pr              n

     d********** Beg Add ***** 07/22/13 ***********************************
     d* Has Authority in option/function security
     d HasAuthorityO   pr              n
     d********** End Add ***** 07/22/13 ***********************************

      * *ENTRY
     DClaimToolVal     pr                  ExtPgm('WTCLMTVAL')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DClaimToolVal     pi
     D inRRN                          9  0
     D inData                       512
     D outJSON                    32767
     D rtnCode                        1

     D fids01        e ds                  ExtName(szq2p)
     D psds          esds                  ExtName(szq1p)

     d returnCode      s              1
     d company#        s              3  0
     d group#          s              3  0
     d jurState        s              2
     d tranID          s              3
     d ediFlag         s              3    Inz(*Blanks)
     d IXeLt           s              1    Inz(*Blanks)
     d IXeLtD          s              3  0 Inz(*Zeros)
     d IXfroi          s              2    Inz(*Blanks)
     d IXsDue          s              3  0 Inz(*Zeros)
     d IXrls           s              1    Inz(*Blanks)
     d IXcsDt          s              7  0 Inz(*Zeros)
     d IXeaDc          s              5    Inz(*Blanks)
     d IXhcPr          s              5    Inz(*Blanks)
     d IXfnRc          s              5    Inz(*Blanks)
     d IXhcEd          s              1    Inz(*Blanks)
     d IXcrEr          s              5    Inz(*Blanks)
     d IXceIr          s              5    Inz(*Blanks)
     d IXceCr          s              5    Inz(*Blanks)
     d IXctAr          s              5    Inz(*Blanks)
     d IXctRr          s              5    Inz(*Blanks)
     d IXmo            s              1    Inz(*Blanks)
     d IXmLtD          s              3  0 Inz(*Zeros)
     d ##Utyp          s              1
     d ##Ulvl          s              3
     d ##Ctp1          s              1
     d ##Clvl          s              3
     d ##exec          s              1
     d ##res           s              1
     d ##Ctp2          s              1
     d ##Ctp3          s              1
     d ##Ctp4          s              1
     d ##Ctp5          s              1
     d ##Ctp6          s              1

     d paidNot0        s               n   Inz(*Off)
     d currActivity    s               n   Inz(*Off)
      *
     D***  Begin Chg  ***  03/31/16  **************************************
     D*                DS
     D*data                    1    512
     D* option                 1     20
     D* env                   21     21
     D* mode                  22    277
     D                 DS
     D dsData                  1    512
     D  dsOption               1     20
     D  dsEnv                 21     21
     D  dsMode                22    277
     D***  End   Chg  ***  03/31/16  **************************************
     D*
     d message         s            200
     d wcmsInst        s              1
     d wcmcInst        s              1
     d wcmcInter       s              1
     D*
     D***  Begin Add  ***  03/31/16  **************************************
     d intStart        s              5  0 inz(1)
     d option          s             20    Inz
     d env             s              1    Inz
     d mode            s            256    Inz
     D***  End   Add  ***  03/31/16  **************************************

      /Free

       //*** Beg Del ***** 03/31/16 ****************************
       //data = inData;
       //*** End Del ***** 03/31/16 ****************************
       //*** Beg Add ***** 03/31/16 ****************************
       option = ExtractParm(inData:',':intStart);
       env    = ExtractParm(inData:',':intStart);
       mode   = ExtractParm(inData:',':intStart);
       // Check to see if not comma delimited
       If option = *blanks;
          dsData = inData;
          option = dsOption;
          env    = dsEnv;
          mode   = dsMode;
       Endif;
       //*** End Add ***** 03/31/16 ****************************

       Chain(n) inRRN PCM0P;

       outJSON = ' ';
       rtnCode = ' ';
       message = ' ';

       //*** beg add ***** 04/29/13 ****************************
       ChkAppSecurity(option);
       //*** end add ***** 04/29/13 ****************************

       Select;
       //
       // Call Claim Department Letters
       When Option='DeptLetters';
          // *** Beg Add ***** 06/20/18 **************************************
          //This Ajax program is not getting called by Javascript for Documents
          //action from Claims Toolbar. Following edit for Inquiry mode is now
          //performed in WTPOPUP with additional conditions applied under
          //Project WC2600.
          // *** End Add ***** 06/20/18 **************************************
         //*** Begin Add 11/04/16 ***************************************
          If mode ='D';
             message='Option not available in Inquiry mode.';
          EndIf;
         //*** End Add 11/04/16 *****************************************
       //
       // Call Check Request Programsrs
       When Option='PayRequests';
          If mode='D';
             message='Option not available in Inquiry mode.';
          EndIf;
       //
       // Call Manual Payment
       When Option='ManualPayment';
          If mode='D';
             message='Option not available in Inquiry mode.';
          EndIf;
       //
       // Call Recurring Comp Request
       When Option='RecurringComp';
          If mode='D';
             message='Option not available in Inquiry mode.';
          Else;
             If CMstat='C';
                message='Option not available for closed Claims.';
             Endif;
          EndIf;
       //
       // Call Recovery Requests
       When Option='RecoveryRequest';
          If mode='D';
             message='Option not available in Inquiry mode.';
          EndIf;
       //
       // Call Provider Bills
       When Option='ProviderBills';
          If wcmcInst='Y' And wcmcInter='Y';
             message='Managed Care must be installed.';
          EndIf;
       //********* Beg Add ***** 07/22/13 ***********************************
       Chain(n) ('W0070R':'L':'1':50) pof01;
          If HasAuthorityO()=*Off;
             message='The user does not have sufficient authority' +
             ' to run provider bills.';
          EndIf;
       //********* End Add ***** 07/22/13 ***********************************
       //
       // Call Medical Cases
       When Option='MedicalCases';
          If wcmcInst='Y' And wcmcInter='Y';
             message='Managed Care must be installed.';
          EndIf;
       //********* Beg Add ***** 07/22/13 ***********************************
       Chain(n) ('W0070R':'L':'1':81) pof01;
          If HasAuthorityO()=*Off;
             message='The user does not have sufficient authority' +
             ' to run Medical Cases.';
          EndIf;
       //********* End Add ***** 07/22/13 ***********************************
       //
       // Call Print FROI's
       When Option='PrintFROI';
          company#=CMco#;
          jurState=CMjrSt;
          tranId='RAT';
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
          If jurState=' ';
             message='The Notice of Injury (NOI) can not be ' +
              'reprinted for the selected claim.';
          EndIf;
       //
       // Call Delete Claims
       When Option='DeleteClaim';
          Chain (CMco#:CMfnd) pfn00;
          Chain (CMco#:CMfnd) pcx00;
          // Check to see if user has enough authority to delete
          If HasAuthority()=*Off;
             message='Insufficient authority to delete claim.';
          Else;
             // Warning about Expired Group
             If FNexDt<>0 And FNexdt<=DateTo7(SystemDate());
                Message='Group ' + %EditC(FNfnd:'X') + ' ' + %Trim(FNname) +
                 ' expired on ' + AlphaUSA(cvtToDate(FNexDt))  + '.';
             Else;
                ChkPayments(paidNot0:currActivity);
                // Paid on claim must be 0
                If paidNot0=*On;
                   message='There are outstanding payments on this claim.';
                Else;
                   // Is there any payment activity
                   If currActivity=*On;
                      message='There are payments in the current month.';
                   EndIf;
                EndIf;
             EndIf;
          Endif;
       //
       // Payment History
       When Option='PaymentHistory';
          // No error messages
       //
       //*** Beg Add *** 10/01/13 **********************************        *
       // Seasonal Adjustment
       When Option='SeasonalAdjustment';
             If CMseae <> 'Y';
                message='Option only available for seasonal employee.';
             Endif;
       // Lifetime Benefit
       When Option='LifetimeBenefit';
             If CMlben <> 'Y';
                message='Option only available for lifetime benefit employee.';
             Endif;
       //*** End Add *** 10/01/13 **********************************        *
       //*** Beg Add *** 04/17/23 *** WC2430 ***********************        *
       // Payments
       //When Option='PaymentRqst';
       When Option='Payments';
            If ChkUsrAut('PaymentRqst') = *Off;
               Message='The user does not have authority' +
                ' to run Payment Request.';
            EndIf;
       // SingleAdjustment
       //When Option='Adjustment';
       When Option='Single Adjustment';
            If ChkUsrAut('Adjustment') = *Off;
               Message='The user does not have authority' +
                ' to run Single Adjustment.';
            EndIf;
       // RecoveryTracking
       //When Option='RecovTrack';
       When Option='Recovery Tracking';
            If ChkUsrAut('RecovTrack') = *Off;
               Message='The user does not have authority' +
                ' to run Recovery Tracking.';
            EndIf;
       //*** End Add *** 04/17/23 *** WC2430 ***********************        *
       EndSl;

       BuildJSON(outJSON :'errorMsg' : %trim(message));

       Return;
      /End-Free
       //===================================================================
       // HasAuthority - Does the user Have Authority
       //===================================================================
     P HasAuthority    B
     D HasAuthority    PI              n
      /Free

          ##Utyp=GetUsrType(q1user);
          ##Ulvl=%EditC(GetUsrLvl(q1user):'X');
          ##Ctp1=CXcdT;
          ##Clvl=%EditC(CXsecl:'X');
          ##exec='1';
          ##res=' ';
          ##Ctp2=CXcdT2;
          ##Ctp3=CXcdT3;
          ##Ctp4=CXcdT4;
          ##Ctp5=CXcdT5;
          ##Ctp6=' ';
          ChkSecLvl(##Utyp:##Ulvl:##Ctp1:##Clvl:##exec:##res:##Ctp2:
           ##Ctp3:##Ctp4:##Ctp5:##Ctp6);

          If ##res='1';
             Return *Off;
          Else;
             Return *On;
          EndIf;

      /End-Free
     P HasAuthority    E
       //********* Beg Add ***** 07/22/13 ***********************************
       //===================================================================
       // HasAuthorityO- Does the user Have Authority in option/function
       //===================================================================
     P HasAuthorityO   B
     D HasAuthorityO   PI              n
      /Free

          ##Utyp=GetUsrType(q1user);
          ##Ulvl=%EditC(GetUsrLvl(q1user):'X');
          ##exec='1';
          ##Clvl=%EditC(OFlvl:'X');

       If %found(sdofl1);
          ChkSecLvl(##Utyp:##Ulvl:OFty01:##Clvl:##exec
          :##res:OFty02:OFty03:OFty04:OFty05:OFty06);
       EndIf;

       If ##res='1';
          Return *Off;
          Else;
          Return *On;
       EndIf;

      /End-Free
     P HasAuthorityO   E
       //********* End Add ***** 07/22/13 ***********************************
       //===================================================================
       // ChkPayments - Check for Current Activity and Outstanding Payments
       //===================================================================
     P ChkPayments     B
     D ChkPayments     PI
     d  paidNot0                       n
     d  currActivity                   n

     d  totalAmount    s             15  2 Inz(0)
     d  currCount      s              9  0 Inz(0)
     d  currMnYr       s              5
i     /Free

       currMnYr=%Subst(%EditC(DateTo7(SystemDate()):'X'):1:5);

       Chain (CMco#:CMfnd:CMfYr:CMcase) ppa03;
       If %Found(WDPAL3);
          DoU %Eof(WDPAL3);
             If PAtype='R' or PAtype='S' or PAtype='V';
                totalAmount=totalAmount-PAamt;
             Else;
                totalAmount=totalAmount+PAamt;
             EndIf;
             // See if payment is in current month
             If %Subst(%EditC(PAtrDt:'X'):1:5)=CurrMnYr;
                currCount=currCount+1;
             EndIf;
             Reade (CMco#:CMfnd:CMfYr:CMcase) ppa03;
          EndDo;
       EndIf;

       // Is there outstanding payments
       If totalAmount<>0;
          eval paidNot0=*On;
       EndIf;

       // Is there any current activity
       If currCount<>0;
          eval currActivity=*On;
       EndIf;

      /End-Free
     P ChkPayments     E
      *** beg add ***** 04/29/13 ****************************************
      *==================================================================
      * ChkAppSecurity - Compare user and option/function security settings
      *                  and return *on if user is authorized access. The
      *                  default table will translate CompSuite actions to
      *                  old Option/Function Security table.
      *==================================================================
     p ChkAppSecurity  b
     d ChkAppSecurity  pi
     d  inAction                     20

     d  outPgmID       s             10
     d  outType        s              1
     d  outNbr         s              2  0
     d  outEnv         s              1    inz('L')
     d  intIdx         s              2  0
     d  inMessage      s            100
     d  isAllowed      s               n   inz(*on)

      /Free
       If env = 'Q';
         outEnv = 'Q';
       Endif;
       intIdx = %Lookup(inAction : aCTxRef);
       If intIdx <> 0;
          outPgmID = %subst(oFDxRef(intIdx) : 1 : 10);
          outType  = %subst(oFDxRef(intIdx) : 11 : 1);
          outNbr   = %int(%subst(oFDxRef(intIdx) : 12 : 2));
       Endif;
       isAllowed = CheckOFSec(outPgmID : outType : outNbr :
        outEnv : inMessage);

       // If user is not authorized, blank out inAction so that program
       //  skips to BuildJSON statement.
       If IsAllowed = *off;
          message = inMessage;
          inAction = *blanks;
       Endif;

      /End-Free

     p ChkAppSecurity  e
      *** end add ***** 04/29/13 ****************************************
      //*** Begin Add ***  03/31/16  *************************************
      *==================================================================
      * Extract entry parms from comma delimited
      *==================================================================
     pExtractParm      b
     dExtractParm      pi                  like(inData)
     d inParm                              like(inData) const
     d inDelimitor                    1                 const
     d inStart                        5  0

     d intLen          s              5  0 inz
     d intPos          s              5  0 inz
     d strParm         s                   like(inData) inz

      /Free
       intPos = %Scan(',':inData:intStart);
       If intPos <= *zero;
          Return *blanks;
       Endif;
       intLen = intPos - intStart;
       strParm = %SubSt(inData:inStart:intLen);
       inStart = intPos + 1;

       Return %trim(strParm);
      /End-free

     pExtractParm      e
      //*** End   Add ***  03/31/16  *************************************
** aCTxRef ------------->oFDxRef
PayRequests              W0070R    151
ManualPayment            W0070R    152
RecurringComp            W0070R    153
RecoveryRequest          W0070R    154
DeptLetters              W0070R    130
