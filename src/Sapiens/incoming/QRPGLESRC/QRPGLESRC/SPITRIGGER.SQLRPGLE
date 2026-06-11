     hCopyright('(c) 2024 Sapiens. All Rights Reserved.')

     h/title SPITRIGGER - API - Auto Diary Trigger Processing
     h nomain
     h DFTNAME(SPITRIGGER)
     h option(*nodebugio: *srcstmt)
      **********************************************************************
      *  Description:  This copybook will be used for auto diary trigger
      *  processing
      *
      *  Called from: SPITRIGGER
      *  Calls to:
      *
      *  Created: 05/22/15 C:279   Project: 114WKA
      **********************************************************************
      * Revisions:
      *
      *  08/28/25 C:062 Project 153WBE
      *                 Add more information to email for name change       n
      *
      *  11/04/24 C:062 Ticket: 115320(129)
      *                 Only apply escape values once.                      n
      *
      *  09/03/24 C:291 Project: WC2968
      *                 Policy Rule - Failed Address/Name Change Notification
      *
      *  01/23/24 C:340 Support: 114936(099)
      *                 Escaping apostrophe marks.
      *
      *  12/19/23 C:340 Project: WC2935
      *                 Policy Rule - Failed BIF Notification
      *
      *  10/18/23 C:079 Support: 114377(099)
      *                 SPITRIGGER is Leaving Records Locked in SMTRPDRT
      *
      *  09/14/23 C:062 Support: 113477(129)
      *                 Add DBA name to email subject.
      *
      *  04/14/23 C:291 Support: 107738
      *                 Generate Audit Diary for % decrease in pemium or
      *                 $ Value decrease in premium.
      *
      *  02/01/22 C:33  Support: 97790
      *                 Encopass email addresses in ''
      *
      *  10/19/21 C:33  Project: WC2663  #98816
      *                 Add processing for 3PRTPMTCX
      *
      *  08/30/21  C:33  Support: Send the suffix to the email processor
      *
      *  05/13/21 C:62  Project: WC2663
      *                 Add invoice cloud processes
      *
      *  12/17/20 C:33  Support: 89324
      *                 Validate against all NPCs.
      *
      *  11/04/19 C:33  Project: 146WJJ
      *                 Check claim flags for split wages.
      *
      *  10/17/19 C:291 Project: 133WRI
      *                 Add Status change trigger for Status Description.
      *
      *  05/22/19 C:33  Support: 79577
      *                 Move more global routines to SPITRIG
      *
      *  04/12/19 C:79  Support: 146/77518
      *                 Correct the Nature/Part/Code rules logic
      *
      *  11/20/18 C:62  Ticket: 74809(146)
      *                 Make sure trigger doesn't change
      *                 Applied by C:079
      *
      *  11/28/17 C:33  Project: WC2405
      *                -User open on SMTRLREC
      *
      *  09/06/17 C:33  Project: WC2405
      *                -Account for Inactive triggers
      *
      *  08/16/17 C:62  Ticket: 48812(114)
      *                 Correct final settlement process
      *
      *  08/15/17 C:62  Project: WC2501
      *                 Recurring Diaries
      *
      *  08/02/17 C:62  Support: 48161(114)
      *                 Diaries should not be secured if no security level
      *
      *  06/22/17 C:33  Project: WC2405
      *                -Account for Profound environments
      *
      *  04/10/17 C:136 Project: 129WUL
      *                 Add experience mod date and policy
      *                 address change triggers
      *
      *  03/31/17 C:33  Project: WC2405
      *                -Check for trigger existence
      *                -Update status if all trigger exist
      *                -Account for DVP environments
      *
      *  12/06/16 C:33  Project: WC2405
      *                -Include generic file changes.
      *
      * 11/29/16  C:62  Support 44360(99)
      *                 Add parm to opnqryf to ignore data decimal errors
      *
      *  11/22/16 C:33  Project: WC2405
      *                -Include agent of record change.
      *
      * 11/09/16  C:33  Project 114WYS   44177
      *                 Check the trigger setup flag before validating
      *                  retro-settlement
      *
      *  07/06/16 C:33  Project: WC2405
      *                -Allow for more actions (other than diary).
      *
      *  06/09/16 C:33  Project: 114WKA
      *                -Allow for future exprired groups to run.
      *
      * 06/01/16  C:242 Project 114WKA
      *                 Do not run for expired Groups.                          iS
      *
      * 05/24/16  C:302 Project 114WYS
      *                 Create procedure to determine if the trigger should
      *                 set for Retrospective rating.
      *
      * 01/25/16  C:33  39507
      *                 Account for 'Claims Only'
      **********************************************************************
     fsdcmlu    if   e           k disk    rename(pcm01:pcm0s)
     f                                     prefix(s)
     fsmtrl     uf   e           k disk    rename(ptr01:ptr00)
     fsmtrpgrp  if   e           k disk    rename(ptr01:ptr0grp)
     f                                     prefix(g)
     fsmtrpclm  if   e           k disk    rename(ptr01:ptr0clm)
     f                                     prefix(c)
     fsmtrppol  if   e           k disk    rename(ptr01:ptr0pol)
     f                                     prefix(p)
     fsmtrlrec  if   e           k disk    rename(ptr01:ptr0rec)
     f                                     prefix(r)
     f                                     usropn
     fsmtrptbl  if   e           k disk    rename(ptr01:ptr0tbl)
     f                                     prefix(t)
     fsmtrplog  if   e           k disk    rename(ptr01:ptr0log)
     f                                     prefix(l)
     fsmtrpdrt  uf a e           k disk    rename(ptr01:ptr0drt)
     f                                     prefix(m)
     fsmtrpact  if   e           k disk    rename(ptr01:ptr0act)
     f                                     prefix(a)
     fsmtrpfil  if   e           k disk    rename(ptr01:ptr0fil)
     fsmtrpfld  uf   e           k disk    rename(ptr01:ptr0fld)
     fsmtrlfrq  if   e           k disk    rename(ptr01:ptr0frq)
     fsmtrpfrq  if   e           k disk    rename(ptr01:ptrpfrq)
     fsmupl     if   e           k disk    rename(pup01:pup00)
     fsmupl1    if   e           k disk
     fsmcol     if   e           k disk    rename(pco01:pco00) usropn
     fstgtl     if   e           k disk    rename(pgt01:pgt00)
     fstctl1    if   e           k disk
     fwdf1l     if   e           k disk    rename(pf101:pf100)
     fwdelp     if   e             disk    rename(pel01:pel0p)  usropn
     fwdelpq    if   e             disk    rename(pel01:pel00q) usropn
     fwdell     if   e           k disk    rename(pel01:pel00)
     fwdell1    if   e           k disk    prefix(d)
     fwdell2    if   e           k disk    rename(pel01:pel02) prefix(el2)
     f                                     usropn
     fwdellb    if   e           k disk    rename(pel01:pel0b)
     f                                     prefix(b)
     fwdehl14   if   e           k disk    rename(peh01:peh14)
     fwdpal     if   e           k disk    rename(ppa01:ppa00)
     fwmeml     if   e           k disk    rename(pem01:pem00)  usropn
     fwmcml1    if   e           k disk    rename(pcm01:pcml1)
     fwmcmla    if   e           k disk    rename(pcm01:pcm0a)
     fwmcmld    if   e           k disk    rename(pcm01:pcm0d)
     fwmcmp     if   e             disk    rename(pcm01:pcm0p)  usropn
     fwmcdp     if   e             disk    rename(pcd01:pcd0p)
     fwmlll     if   e           k disk    rename(pll01:pll00)  usropn
     fwmcdl     if   e           k disk    rename(pcd01:pcd00)  usropn
     fwmpdl     if   e           k disk    rename(ppd01:ppd00)  usropn
     fwmecl1    if   e           k disk                         usropn
     fwamcl     if   e           k disk    rename(pmc01:pmc00)
     fwamcl1    if   e           k disk    rename(pmc01:pmc01)
     fwamcl2    if   e           k disk    rename(pmc01:pmc02)
     f                                     prefix(a)
     fwxnhl     if   e           k disk    rename(pnh01:pnh00)
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     fsmrdl     uf a e           k disk    rename(prd01:prd00)
     fwmchl     if   e           k Disk    ReName(pch01:pch00)
     fwtgtl     if   e           k disk    rename(pgt01:pgtw0)
     f                                     prefix(w)

     d psds          esds                  extname(szq1p)
     d empDs1          ds                  likerec(pem00)
     d dsClaim         ds                  likerec(pcm0p)
     d dsSS#           ds                  likerec(pcd0p)

     d dsSpiTrigger    ds                  dtaara('SPITRIGGER')
     d  dsFromEmail                  60

      * work variables
     d ##key           s             40
     d ##comm          s            256
     d ##cDat          s              8
     d ##dcId          s             12    inz(*blanks)
     d ##pNam          s             10    inz
     d ##secl          s              3    inz
     d ##user          s             10
     d ##pgm           s             10
     d ##fold          s             80    inz(*blanks)
     d ##diFl          s              1    inz
     d ##iPgm          s             10
     d ##fl01          s              1    inz(*blanks)
     d ##fl02          s              1    inz(*blanks)
     d ##rmCd          s              5
     d ##seq#          s              5    inz('00000')
     d*##sbyl          s              1    inz('Y')
     d ##sbyl          s              1    inz('N')
     d ##com2          s             19    inz(*blanks)
     d ##apId          s              2    inz(*blanks)
     d ##iDoc          s             12    inz(*blanks)
     d ##iQue          s             10    inz(*blanks)
     d ##imgF          s              1    inz(*blanks)
     d ##iPth          s             10    inz(*blanks)
     d ##pIdx          s              5    inz('00000')
     d ##qPty          s              1    inz(*blanks)
     d ##dcTp          s              5    inz(*blanks)
     d ##csId          s              2    inz(*blanks)
     d ##code          s              2    inz(*blanks)
     d intCase         s              7  0
     d intLastRun      s              7  0 inz
     d isTrgSet        s               n   inz
     d isOpen          s               n   inz
     d isClaim         s               n   inz
     d Ptr             s               *   procptr
     d PtrBfrAddr      s               *
     d strBody         s          10000
     d strCommand      s            255
     d*** Begin Chg *** 09/14/23 *******************************************
     d*strCmd***       s            255
     d*** Begin Chg *** 12/19/23 *******************************************
     d*strCmd          s            355
     d strCmd          s            455
     d*** End   Chg *** 12/19/23 *******************************************
     d*** End   Chg *** 09/14/23 *******************************************
     d strQry          s            255
     d strFile         s             10    inz
     d strMsg          s            700
     d strSubject      s            200
     d GroupStatus     s              1
     d createAll       s               n   Inz(*Off)
     d i               s              5  0
     d rec#            s              5  0
     d recurDate       s               d   DatFmt(*Usa)
     d lastRecurDate   s               d   DatFmt(*Usa)

     d ChkObjwLib      pr                  extpgm('SRCHKOB2')
     d  outLib                       10    const
     d  outFile                      10    const
     d  outType                       7    const
     d  inError                       1
     d RecurringDiary  pr                  extpgm('STRECDRYC')
     d  inTriggerName                10
     d  inDartsKey                   41
     d  inSequence#                   5
     d  inOcur                        5

     d GetObjLib       pr                  extpgm('RTVOBJLIB')
     d Object                        10    const
     d Type                           7    const
     d Library                       10

     d QCmdExc         pr                  extpgm('QCMDEXC')
     d command                             const like(strCommand)
     d length                        15  5 const

     d  rslvsp         pr              *   extproc('rslvsp') procptr
     d  PR_objType                    2    value
     d  PR_objName                     *   value options(*string)
     d  PR_libName                     *   value options(*string)
     d  PR_reqAuth                    2

     d RtvSuffix       pr                  extpgm('RTVSUFFIX')
     d  inSuffix                      3

     d QleActBndPgm    pr            10I 0 extproc('QleActBndPgm')
     d  BndPgmPtr                      *   const procptr
     d  ActMark                      10I 0 options(*omit)
     d  ActInfo                   32767A   options(*omit:*varsize)
     d  ActInfoLen                   10I 0 const options(*omit)
     d  ErrorInfo                 32767A   options(*omit:*varSize)

     d QleGetExp       pr              *   extproc('QleGetExp') procptr
     d  ActMark                      10I 0 const
     d  ExportNumber                 10I 0 const
     d  ExportNameLen                10I 0 const
     d  ExportName                32767A   const options(*varsize)
     d  ExportPtr                      *   procptr
     d  ExportType                   10I 0
     d  ErrorInfo                 32767A   options(*varsize)

     d RtnProcPtr      pr              *   procptr
     d  PR_srvPgm                    10    value
     d  PR_exportName               256    value

     d ProcTrgname     pr              n   extproc(ptr)

      * Write Darts Remark
     d WrtDrtRmk       pr                  extpgm('DA700R')
     d ##key                         40
     d ##comm                       256
     d ##cDat                         8
     d ##dcId                        12
     d ##pNam                        10
     d ##secl                         3
     d ##user                        10
     d ##pgm                         10
     d ##fold                        80
     d ##diFl                         1
     d ##iPgm                        10
     d ##fl01                         1
     d ##fl02                         1
     d ##rmCd                         5
     d ##seq#                         5
     d ##sbyl                         1
     d ##com2                        19
     d ##apId                         2
     d ##iDoc                        12
     d ##iQue                        10
     d ##imgF                         1
     d ##iPth                        10
     d ##pIdx                         5
     d ##qPty                         1
     d ##dcTp                         5
     d ##csId                         2
     d ##code                         2

      * copy books
      ***  Begin Add  ***  12/17/20  ************************************
      /copy sPrSql
      ***  End   Add  ***  12/17/20  ************************************
      /copy sPrTable
      /copy sPrTrig
      /copy sPrTrigger
      /copy sPrDate
      /copy sPrString
      /copy wPrEmplr
      /copy sPrModule
      /copy sPrRunCmd

     d CheckActions    pr
     d intCo#                         3  0
     D* --------------------------------------------------
     D* Prototype for procedure: ValidIncurredLoss
     D* --------------------------------------------------
     D ValidIncurredLoss...
     D                 PR              N
     d WriteEmail      pr
     d WriteText       pr
     d StsDescExist    pr              n

     d DateQualifies...
     d                 pr              n
     d  inDate                        7  0
     d  compDate                     10
     d  compOper                     12

       //***  Begin Add  ***  12/17/20  ***************
       //===================================================================
       // AaaFirstProcedure - Needs to be first procedure but doens't do anything
       //===================================================================
     pAaaFirstProcedure...
     p                 b
     dAaaFirstProcedure...
     d                 pi
      /Free
       Exec SQL set option COMMIT=*NONE;
      /End-free
     pAaaFirstProcedure...
     p                 e
       //***  End   Add  ***  12/17/20  ***************

       //===================================================================
       // ProcessTrigger - Process Auto Diary Trigger
       //===================================================================
     p ProcessTrigger  b                   export
     d ProcessTrigger  pi
     d inTrgName                     10    const
     d inCo#                          3
     d inFnd#                         3
     d inPtrAfter                      *   options(*nopass)
     d inPtrBefore                     *   options(*nopass)

     d isTrgRslt       s               n   inz(*off)
     d strTrgNmUp      s             10
     d*** Begin Add *** 05/13/21 *************************************
     d hldTrgNmUp      s             10
     d*** End   Add *** 05/13/21 *************************************
     d isProcess       s               n   inz
     d dartsKey        s             41
     d sequence#       s              5
     d occurrence      s              5
     d strTrgName      s             10    Inz
     d intCo#          s              3  0
     d intFnd          s              3  0
      /free

       // Initilaize
       strTrgName = inTrgName;
       strTrgNmUp = Upper(strTrgName);
       isTrgSet   = *on;
       intCo# = %int(inCo#);
       intFnd = %int(inFnd#);
       ptrWMCMPbfr = %addr(dsClaim);
       ptrWMCMPaft = %addr(dsClaim);
       ptrWMCDPbfr = %addr(dsSS#);
       clear dsClaim;
       clear dsSS#;
       isClaim = *off;

       //* Load Data Areas by assigning pointers
       ExSr LoadDataAreas;

       // Trigger processing (iSeries/ Batch dependent)
       Exsr TrigProcess;

       //===================================================================
       // Load Data Areas by assigning pointers
       //===================================================================
       Begsr LoadDataAreas;

       Select;
       When %parms >= 5;
       // set file pointer for before & after record images
          Select;
          When %trim(strTrgNmUp) = 'NEWCLAIM' or %trim(strTrgNmUp) = 'CLMCHG02'
           or %trim(strTrgNmUp)= 'CLMCHG03' or %trim(strTrgNmUp) = 'CLMCHG04'
           or %trim(strTrgNmUp) = 'LOWRSV' or %trim(strTrgNmUp) = 'LARGERSV'
           or %trim(strTrgNmUp) = 'CLMNCCI' or %trim(strTrgNmUp) = 'CLMNPC'
           or %trim(strTrgNmUp) = 'TOTALINC';
             ptrWMCMPaft = inPtrAfter;
             ptrWMCMPbfr = inPtrBefore;

          When %trim(strTrgNmUp) = 'CLMCHG01';
             ptrWMCDPaft = inPtrAfter;
             ptrWMCDPbfr = inPtrBefore;

          When %trim(strTrgNmUp) = 'POLADDRCHG';
             ptrWMEMPaft = inPtrAfter;
             ptrWMEMPbfr = inPtrBefore;
          // Recurring Diary Completion
          When %trim(strTrgNmUp) = 'RECDRYCOMP';
             ptrSDCMPaft = inPtrAfter;
             ptrSDCMPbfr = inPtrBefore;
          EndSL;

       When %parms >= 4;
       // set file pointer for after record image

          Select;
          When %trim(strTrgNmUp) = 'LARGEPMT' or %trim(strTrgNmUp) = 'PAYMENT';
             ptrWDPAPaft = inPtrAfter;

          When %trim(strTrgNmUp) = 'MULTIEMP';
             ptrWAMCPaft = inPtrAfter;

          When %trim(strTrgNmUp) = 'AUDPREM$' or %trim(strTrgNmUp)= 'AUDPREM'
            or %trim(strTrgNmUp) = 'CALCPREM' or %trim(strTrgNmUp) = 'POLCONC';
             ptrWDEHPaft = inPtrAfter;

          When %trim(strTrgNmUp) = 'QTEENTRY' or %trim(strTrgNmUp) = 'QTERVW';
             ptrWXELPaft = inPtrAfter;

          When %trim(strTrgNmUp) = 'POLPANEL';
             ptrWMSHPaft = inPtrAfter;

          When %trim(strTrgNmUp) = 'EXPMODDATE';
             ptrWDEHPaft = inPtrAfter;
          // Recurring Diary Add
          When %trim(strTrgNmUp) = 'RECDRYADD';
             ptrSDCMPaft = inPtrAfter;
          //*** Begin Add *** 05/13/21 *************************************
          // Quote Third Party Payment
          When %trim(strTrgNmUp) = 'QTE3PRTPMT';
             ptrWDEAP_HLDaft = inPtrAfter;
          // 3rd Party Payment Dispute
          When %trim(strTrgNmUp) = '3PRTPMTDIS';
             ptrWMEAPaft = inPtrAfter;
          //*** End   Add *** 05/13/21 *************************************
          //***  Begin Add  ***  10/19/21  *************
          // 3rd Party Payment on CX Policy
          When %trim(strTrgNmUp) = '3PRTPMTCX';
             ptrWMEAPaft = inPtrAfter;
          //***  End   Add  ***  10/19/21  *************
       //*** Beg Add *** 12/19/23 *** WC2935 ***************************
          When %trim(strTrgNmUp) = 'BIFFAILURE';
             ptrWIBIFPaft = inPtrAfter;
       //*** End Add *** 12/19/23 *** WC2935 ***************************
          //*** Beg Add ***** 09/03/24 **************************************
          When %trim(strTrgNmUp) = 'IVCNAMADDR';
             ptrWIBIFPaft = inPtrAfter;
          //*** End Add ***** 09/03/24 **************************************
          EndSL;
       EndSL;

       Endsr;
       //===================================================================
       // Trigger processing
       //===================================================================
       Begsr TrigProcess;

       Select;
       // process all claims with same ss# when claimant name/adress is changed
       When %trim(strTrgNmUp) = 'CLMCHG01';
          Exsr ProcSS#;

       // process claims payment triggers
       When %trim(strTrgNmUp) = 'LARGEPMT' or %trim(strTrgNmUp) = 'PAYMENT';
          Exsr ProcPayment;

       // process multiple employer claim trigger
       When %trim(strTrgNmUp) = 'MULTIEMP';
          Exsr ProcMultiEmp;

       // process claims triggers (other than the ones above)
       When %trim(strTrgNmUp) = 'NEWCLAIM' or %trim(strTrgNmUp) = 'CLMCHG02'
         or %trim(strTrgNmUp) = 'CLMCHG03' or %trim(strTrgNmUp) = 'CLMCHG04'
         or %trim(strTrgNmUp) = 'LOWRSV' or %trim(strTrgNmUp) = 'LARGERSV'
         or %trim(strTrgNmUp) = 'CLMNCCI'
         or %trim(strTrgNmUp) = 'CLMNPC' or %trim(strTrgNmUp) = 'TOTALINC';
          Exsr ProcClaim;

       // process policy batch triggers
       When %trim(strTrgNmUp) = 'RQTESTS' or %trim(strTrgNmUp) = 'NQTESTS';
          Exsr ProcPolBtch;

       // process policy batch trigger 'PolPend'
       When %trim(strTrgNmUp) = 'POLPEND';
          Exsr ProcPolPend;

       // process Claim batch trigger 'ClmReten'
       When %trim(strTrgNmUp) = 'CLMRETEN';
          Exsr ProcClmReten;

       // process Claim batch trigger 'Payment2' and 'WeekdPd'
       When %trim(strTrgNmUp) = 'PAYMENT2' or %trim(strTrgNmUp) = 'WEEKSPD';
          Exsr ProcClmBtch;

       When %Trim(strTrgNmUp) = 'POLADDRCHG' Or
        %Trim(strTrgNmUp) = 'EXPMODDATE';
          Exsr ProcGenPolTrig;
       When %Trim(strTrgNmUp) = 'RECDRYADD' Or
        %Trim(strTrgNmUp) = 'RECDRYCOMP';
          Exsr ProcRecDryTrig;
       //*** Begin Add *** 05/13/21 *************************************
       When %Trim(strTrgNmUp) = 'QTE3PRTPMT';
          Exsr QuoteThirdPartyPayment;
       When %Trim(strTrgNmUp) = '3PRTPMTDIS';
          Exsr ThirdPartyPaymentDisputed;
       //*** End   Add *** 05/13/21 *************************************
       //***  Begin Add  ***  10/19/21  *************
       When %Trim(strTrgNmUp) = '3PRTPMTCX';
          Exsr ThirdPartyPaymentCx;
       //***  End   Add  ***  10/19/21  *************
       //*** Beg Add *** 12/19/23 *** WC2935 ***************************
       When %Trim(strTrgNmUp) = 'BIFFAILURE';
          Exsr BIFFailed;
       //*** End Add *** 12/19/23 *** WC2935 ***************************
       //*** Beg Add ***** 09/03/24 ************************************
       When %Trim(strTrgNmUp) = 'IVCNAMADDR';
          Exsr BIFFailed;
       //*** End Add ***** 09/03/24 ************************************

       Other;
          Exsr MainProc;
       Endsl;

       If isOpen = *on;
          isOpen = *off;
          Close WMEML;
       Endif;

       Endsr;

       //===================================================================
       // Get trigger procedure and execute
       //===================================================================
       BegSr RunTrgProc;

       //*** Begin Add *** 05/13/21 *************************************
       // Need to point to procedures that is associated with trigger
       hldTrgNmUp = strTrgNmUp;
       Select;
       When strTrgNmUp = '3PRTPMTDIS';
          strTrgNmUp = 'TPRTPMTDIS';
       //***  Begin Add  ***  10/19/21  *************
       When strTrgNmUp = '3PRTPMTCX';
          strTrgNmUp = 'TPRTPMTCX';
       //***  End   Add  ***  10/19/21  *************
       EndSl;
       //*** End   Add *** 05/13/21 *************************************

       // get pointer for auto diary triggers.
       Ptr = RtnProcPtr('SPITRIGGER':%trim(strTrgNmUp));
       //*** Begin Add *** 05/13/21 *************************************
       // Need to restored held trigger.
       strTrgNmUp = hldTrgNmUp;
       //*** End   Add *** 05/13/21 *************************************
       Monitor;
          isTrgRslt = ProcTrgName();
       On-Error;
       Endmon;

       Endsr;

       //===================================================================
       // Main logic for checking trigger setup and execute process
       //===================================================================
       Begsr MainProc;

       isTrgSet = *on;

       // Read trigger table file
       Setll (intCo#:strTrgName) ptr00;
       Reade(n) (intCo#:strTrgName) ptr00;
       If %eof(SMTRL);
          isTrgSet = *off;
       Endif;

       Dow not %eof(SMTRL);
          isProcess   = *off;
          Chain(n) (intCo#:intFnd) pfn00;
          groupStatus = 'A';
          Select;
       //*** Beg Add *** 12/19/23 *** WC2935 ***************************
          When intFnd = 0;
             groupStatus = 'X';
       //*** End Add *** 12/19/23 *** WC2935 ***************************
          When not %found(WMFNL);
             groupStatus = 'I';
          When FNexDt <> 0 and FNexDt <= DateTo7(SystemDate);
             groupStatus = 'I';
          Endsl;

          If TRstat = 'A' and GroupStatus = 'A';
             // check for group in trigger group setup file
             Setll (intCo#:TRname:TRseq#) ptr0grp;
             If %Equal(SMTRPGRP);
                Setll (intCo#:TRname:TRseq#:intFnd) ptr0grp;
                If %Equal(SMTRPGRP);
                   // group is setup for auto diary trigger
                   isProcess = *on;
                Endif;
             Else;
                // all groups are set
                isProcess = *on;
             Endif;
          Endif;

       //*** Beg Add *** 12/19/23 *** WC2935 ***************************
          If TRstat = 'A' and GroupStatus = 'X';
             isProcess = *on;
          Endif;
       //*** End Add *** 12/19/23 *** WC2935 ***************************

          If isProcess = *on;
             Exsr RunTrgProc;
             If isTrgRslt = *on;
                CheckActions(intCo#);
              //* Reset Data Areas by assigning pointers
                ExSr LoadDataAreas;
             Endif;
          Endif;

          Reade(n) (intCo#:strTrgName) ptr00;
       Enddo;

       Endsr;

       //===================================================================
       // Process all claims for Claimant Name/Address change (same SS#)
       //===================================================================
       BegSr ProcSS#;

       // read from WMCMP file for all records with same SS#
       Setll aftCDss# pcm0a;
       Reade aftCDss# pcm0a;
       Dow not %eof(WMCMLA);
          intCo# = %int(CMco#);
          intFnd = %int(CMfnd);
          // Set empds1 for employer state division staff
          ELco# = CMco#;
          ELfnd = CMfnd;
          ELemp# = CMemp#;
          ELdiv = CMdiv;
          isClaim = *on;
          SetStRptDv();

          Exsr MainProc;

          // leave processing if no trigger setup
          If isTrgSet = *off;
             leave;
          Endif;

          Reade aftCDss# pcm0a;
       Enddo;

       Endsr;

       //===================================================================
       // Process payment triggers.
       //===================================================================
       BegSr ProcPayment;

       // read WMCMP file for claim payment being processed
       Chain (aftPAco#:aftPAfnd:aftPAfyr:aftPAcase) pcml1;
       If not %eof(WMCML1);
          intCo# = %int(CMco#);
          intFnd = %int(CMfnd);
          // Set empds1 for employer state division staff
          ELco# = CMco#;
          ELfnd = CMfnd;
          ELemp# = CMemp#;
          ELdiv = CMdiv;
          isClaim = *on;
          SetStRptDv();

          Exsr MainProc;

       Endif;

       Endsr;

       //===================================================================
       // Process multiple employer claim
       //===================================================================
       BegSr ProcMultiEmp;

       intCase = %int(aftMCcase);
       // read WMCMP file for WAMCP record being processed
       Chain (aftMCco#:aftMCfnd:aftMCfyr:intCase) pcml1;
       If not %eof(WMCML1);
          intCo# = %int(CMco#);
          intFnd = %int(CMfnd);
          // Set empds1 for employer state division staff
          ELco# = CMco#;
          ELfnd = CMfnd;
          ELemp# = CMemp#;
          ELdiv = CMdiv;
          isClaim = *on;
          SetStRptDv();

          Exsr MainProc;

       Endif;

       Endsr;

       //===================================================================
       // Process claims for Claim Triggers
       //===================================================================
       BegSr ProcClaim;

       // read WMCMP file for claim
       Chain (aftCMco#:aftCMfnd:aftCMfyr:aftCMcase) pcml1;
       If not %eof(WMCML1);
          // Set empds1 for employer state division staff
          ELco# = CMco#;
          ELfnd = CMfnd;
          ELemp# = CMemp#;
          ELdiv = CMdiv;
          isClaim = *on;
          SetStRptDv();

          Exsr MainProc;
       Endif;

       Endsr;

       //===================================================================
       // Process for policy batch triggers
       //===================================================================
       Begsr ProcPolBtch;

       // Do not process for Claims Only Clients
       If ClaimsOnly;
          Leavesr;
       Endif;

       // override files from Quote environment
       clear strCommand;
       strCommand = 'OVRDBF FILE(WDELPQ) TOFILE(' +
        %trim(RtvQuoteLib('*QTE')) + '/WDELPQ) SHARE(*YES)';
       Callp QCMDEXC(strCommand:%len(strCommand));

       strFile = 'WDELPQ';
       Exsr BldQry;
       Open WDELPQ;

       clear strCommand;
       strCommand = 'OVRDBF FILE(WMEML) TOFILE(' +
        %trim(RtvQuoteLib('*QTE')) + '/WMEML) SHARE(*YES)';
       Callp QCMDEXC(strCommand:%len(strCommand));
       If isOpen <> *on;
          isOpen = *on;
          Open WMEML;
       Endif;

       clear empDs1;

       // read from WDELPQ file
       Read pel00q;
       Dow not %eof(WDELPQ);
          intCo# = %int(ELco#);
          intFnd = %int(ELfnd);
          Exsr MainProc;

          // leave processing if no trigger setup
          If isTrgSet = *off;
             leave;
          Endif;

          Read pel00q;
       Enddo;

       // close oveeridden files
       ClsOvrRd(strFile:strQry);
       ClsOvrRd('WMEML');

       Close WDELPQ;
       If isOpen = *on;
          Close WMEML;
          isOpen = *off;
       Endif;

       Endsr;

       //===================================================================
       // Process for policy batch trigger 'PolPend'
       //===================================================================
       Begsr ProcPolPend;

       // override files from Quote environment
       clear strCommand;
       strCommand = 'OVRDBF FILE(WDELP) TOFILE(' +
        %trim(RtvQuoteLib('*LIV')) + '/WDELP) SHARE(*YES)';
       Callp QCMDEXC(strCommand:%len(strCommand));

       strFile = 'WDELP';
       Exsr BldQry;
       Open WDELP;
       If isOpen <> *on;
          Open WMEML;
          isOpen = *on;
       Endif;

       clear empDs1;

       // read from WDELP file
       Read pel0p;
       Dow not %eof(WDELP);
          intCo# = %int(ELco#);
          intFnd = %int(ELfnd);
          Exsr MainProc;

          // leave processing if no trigger setup
          If isTrgSet = *off;
             leave;
          Endif;

          Read pel0p;
       Enddo;

       // close oveeridden files
       ClsOvrRd(strFile:strQry);

       Close WDELP;
       If isOpen = *on;
          Close WMEML;
          isOpen = *off;
       Endif;

       Endsr;

       //===================================================================
       // Build query
       //===================================================================
       Begsr BldQry;

       // Check if last run detail exist in log file (key - 000/'Batch')
       strQry = ' ';

       intLastRun = 0;
       Chain (0:'Batch') ptr0log;
       If %found(SMTRPLOG);
          intLastRun = lTRiDt;
          If %trim(strFile) <> 'WMCMP';
             strQry = '''ELIDT *GE '+%editc(intLastRun:'X')+' *OR ELDT *NE 0 ' +
              ' *AND ELDT *GE ' + %editc(intLastRun:'X') + '''';
          Else;
             strQry = '''CMIDT *GE '+%editc(intLastRun:'X')+' *OR CMDT *NE 0 ' +
              ' *AND CMDT *GE ' + %editc(intLastRun:'X') + '''';
          Endif;

          strCommand = 'OPNQRYF FILE(' + %trim(strFile) + ') QRYSLT(' +
           %trim(strQry) + ') IGNDECERR(*YES)';
          Callp QCMDEXC(strCommand:%len(strCommand));
       Endif;

       Endsr;

       //===================================================================
       // Percentage of Specific Retention
       //===================================================================
       BegSr ProcClmReten;

       Open SMCOL;
       Setll *loval pco00;
       Read pco00;
       Dow not %eof(SMCOL);
          intCo# = COco#;
          Exsr MainProc;

          Read pco00;
       Enddo;
       Close SMCOL;

       Endsr;

       //===================================================================
       // Process Claim batch triggers
       //===================================================================
       BegSr ProcClmBtch;

       // override Claim master file
       clear strCommand;
       strCommand = 'OVRDBF FILE(WMCMP) TOFILE(WMCMP) SHARE(*YES)';
       Callp QCMDEXC(strCommand:%len(strCommand));

       strFile = 'WMCMP';
       Exsr BldQry;
       Open WMCMP;

       // read from WMCMP file
       Read pcm0p;
       Dow not %eof(WMCMP);
          intCo# = %int(CMco#);
          intFnd = %int(CMfnd);
          // Set empds1 for employer state division staff
          ELco# = CMco#;
          ELfnd = CMfnd;
          ELemp# = CMemp#;
          ELdiv = CMdiv;
          isClaim = *on;
          SetStRptDv();

          Exsr MainProc;

          // leave processing if no trigger setup
          If isTrgSet = *off;
             leave;
          Endif;

          Read pcm0p;
       Enddo;

       // close oveeridden files
       ClsOvrRd(strFile:strQry);

       Close WMCMP;

       Endsr;

       //===================================================
       // Process Generic Policy Trigger
       //===================================================
       Begsr ProcGenPolTrig;

       // Get policy year information to write DARTS and
       //  run edits with
       Select;
       When strTrgNmUp = 'POLADDRCHG';
          Open WDELL2;
          Setgt (aftEMco#:aftEMfnd:aftEMemp#:
           aftEMdiv:DateTo7(SystemDate)) pel02;
          Readpe (aftEMco#:aftEMfnd:aftEMemp#:
           aftEMdiv) pel02;
          Close WDELL2;
          // Bail out if no policy info exists
          If %Eof;
             LeaveSr;
          Endif;
          ELco#  = el2Elco#;
          ELfnd  = el2ELfnd;
          ELemp# = el2ELemp#;
          ELdiv  = el2ELdiv;
          ELfyr  = el2ELfyr;
       When strTrgNmUp = 'EXPMODDATE';
          ELco#  = aftEHco#;
          ELfnd  = aftEHfnd;
          ELemp# = aftEHemp#;
          ELdiv  = aftEHdiv;
          ELfyr  = aftEHfyr;
          // Bail out if no policy info exists
          If ELfyr = 0;
             LeaveSr;
          Endif;
       EndSl;

       Exsr MainProc;

       EndSr;
       //===================================================
       // Process Recurring Diary Triggers
       //===================================================
       Begsr ProcRecDryTrig;

       // Get policy year information to write DARTS and
       //  run edits with
       Select;
       // Check for Completed Diaries
       When strTrgNmUp = 'RECDRYCOMP';
       // Was the diary completed
       If preCMdcDt = 0 And posCMdcDt <> 0;
          dartsKey   =  posCMkey + '*';
          sequence#  = %EditC(posCMseq#:'X');
          occurrence = '00001';
          RecurringDiary(strTrgNmUp:dartsKey:sequence#:occurrence);
       EndIf;
       // Check for Add Diaries
       When strTrgNmUp = 'RECDRYADD';
          // Ignore diaries created by SPITRIGGER
          If posCMiPgm <> 'STRECDRY' And posCMdiFl = 'Y';
             // Access Remark Type code
             Chain posCMrmCd pct01;
             If %Found(stctl1);
                If CTcrAd = 'Y'; // Create all recurring diaries
                   // Create all recurring diaries
                   dartsKey   =  posCMkey + '*';
                   sequence#  = %EditC(posCMseq#:'X');
                   occurrence = %EditC(CTrd#:'X');
                   RecurringDiary(strTrgNmUp:dartsKey:sequence#:occurrence);
                Else;
                   If CTrdPl <> 0; // Create tracking record
                      // Write Recurring Diary Tracking Record
                      ExSr WriteSMRDP;
                   EndIf;
                EndIf;
             EndIf;
          EndIf;
       EndSl;

       isProcess = *Off;
       Return;

       EndSr;
      //*** Begin Add *** 05/13/21 *************************************
       //===================================================
       // Process Quote Third Party Trigger
       //===================================================
       Begsr QuoteThirdPartyPayment;

       intco#  = aftEAco#;
       intfnd  = aftEAfnd;
       ELco#   = aftEAco#;
       ELfnd   = aftEAfnd;
       ELemp#  = aftEAemp#;
       ELdiv   = aftEAdiv;
       ELfyr   = aftEAfyr;

       Exsr MainProc;

       EndSr;

       //***  Begin Add  ***  10/19/21  *************
       //===================================================
       // Process Third Party Payment on Cx Policy Trigger
       //===================================================
       Begsr ThirdPartyPaymentCx;

       intco#  = posEAco#;
       intfnd  = posEAfnd;
       ELco#   = posEAco#;
       ELfnd   = posEAfnd;
       ELemp#  = posEAemp#;
       ELdiv   = posEAdiv;
       ELfyr   = posEAfyr;

       Exsr MainProc;

       EndSr;
       //***  End   Add  ***  10/19/21  *************
       //*** Beg Add *** 12/19/23 *** WC2935 ***************************
       //===================================================
       // Process BIF Failure Trigger
       //===================================================
       Begsr BIFFailed;

         intco#  = posBIco#;
         intfnd  = 0;

         Exsr MainProc;

       EndSr;
       //*** End Add *** 12/19/23 *** WC2935 ***************************

       //===================================================
       // Process Third Party Payment Dispute Trigger
       //===================================================
       Begsr ThirdPartyPaymentDisputed;

       intco#  = posEAco#;
       intfnd  = posEAfnd;
       ELco#   = posEAco#;
       ELfnd   = posEAfnd;
       ELemp#  = posEAemp#;
       ELdiv   = posEAdiv;
       ELfyr   = posEAfyr;

       Exsr MainProc;

       EndSr;
      //*** End   Add *** 05/13/21 *************************************
       //===================================================================
       // WriteSMRDP -  Writes Recurring Diary Tracking Record
       //===================================================================
       BegSr WriteSMRDP;

          Clear prd00;
          RDkey  = posCMkey;
          RDseq# = posCMseq#;
          RDrmCd = posCMrmCd;
          RDrd#  = CTrd#;
          RDstat = 'A';
          RDpgm  = q1Pgm;
          RDuser = posCMuser;
          RDdt   = DateTo7(systemDate());
          RDtime = TimeTo6(systemTime());
          RDiPgm = q1Pgm;
          RDiUsr = posCMuser;
          RDiDt  = DateTo7(systemDate());
          RDiTim = TimeTo6(systemTime());
          Write prd00;

       Endsr;

      /end-free
     p ProcessTrigger  e
       //*** Beg Add *** 12/19/23 *** WC2935 ***************************
       //===================================================================
       // BIF Failure Trigger Procedure
       //===================================================================
     pBifFailure       b                   Export
     dBifFailure       pi              n

       Return *on;

     pBifFailure       e
       //*** End Add *** 12/19/23 *** WC2935 ***************************
       //*** Beg Add ***** 09/03/24 **************************************
       //===================================================================
       // BIF Failure Trigger Procedure
       //===================================================================
     pIVCNamAddr       b                   Export
     dIVCNamAddr       pi              n

       Return *on;

     pIVCNamAddr       e
       //*** End Add ***** 09/03/24 **************************************
       //===================================================================
       // Payment2 - Payment (Totals)
       //===================================================================
     p Payment2        b                   Export
     d Payment2        pi              n

     d intAmount       s             11  2 inz(0)
      /Free

       If MeetsTrgClm = *on;
          // read from WDPAL file
          Setll (CMco#:CMfnd:CMfyr:CMcase) ppa00;
          Reade (CMco#:CMfnd:CMfyr:CMcase) ppa00;
          Dow not %eof(WDPAL);
             If MeetsTrgTbl('Payment':PAbr2) = *on;
                intAmount = intAmount + PAamt;
             Endif;

             Reade (CMco#:CMfnd:CMfyr:CMcase) ppa00;
          Enddo;

          If (intAmount <> 0  or cTRtotPaid <> 0) and intAmount >= cTRtotPaid;
             CheckActions(CMco#);
          Endif;
       Endif;

       Return *off;

      /End-Free
     p Payment2        e
       //===================================================================
       // WeeksPd - Compensation Weeks Paid
       //===================================================================
     p WeeksPd         b                   Export
     d WeeksPd         pi              n

     d intTotWks       s              7  0
     d intWeeks        s              6  0
     d intDys          s              5  0

      /free

       intTotWks = 0;
       intWeeks  = 0;
       intDys    = 0;

       If MeetsTrgClm = *on;
          // Read all payments from WDPAL for claim
          Setll (CMco#:CMfnd:CMfyr:CMcase) ppa00;
          Reade (CMco#:CMfnd:CMfyr:CMcase) ppa00;
          Dow not %eof(WDPAL);
             If MeetsTrgTbl('Payment':PAbr2) = *on and
                MeetsTrgTbl('Reserve':PArecd) = *on;
                If ((PAtype = 'P' or PAtype = 'M' or PAtype= 'A' or Patype ='Z')
                 and PAamt > 0) or ((PAtype = 'S' or PAtype= 'V' or PAtype= 'R')
                 and PAamt < 0);
                   intWeeks = intWeeks + PAwks;
                   intDys   = intDys + PAdays;
                Else;
                   intWeeks = intWeeks - PAwks;
                   intDys = intDys - PAdays;
                Endif;
             Endif;

             Reade (CMco#:CMfnd:CMfyr:CMcase) ppa00;
          Enddo;

          intTotWks = intWeeks + (intDys/7);
          If (cTRweeksPd <> 0 or intTotWks <> 0) and intTotWks >= cTRweeksPd;
             CheckActions(CMco#);
          Endif;

       Endif;

       Return *off;

      /End-Free
     p WeeksPd         e

       //===================================================================
       // AgtRcdChg - Agent of Record Change
       //===================================================================
     p AgtRcdChg       b                   Export
     d AgtRcdChg       pi              n
      /free

       Chain (aftAHco#:aftAHfnd:aftAHemp#:aftAHdiv:aftAHfyr) pel00;
       If %found(WDELL) and MeetsTrgPolicy = *on;
         // Need to check if there is actually an agt of rcd chg
          Return *on;
       Endif;


       Return *off;

      /End-Free
     p AgtRcdChg       e

       //===================================================================
       // ClmReten - Percentage of Specific Retention
       //===================================================================
     p ClmReten        b                   Export
     d ClmReten        pi              n

     d intAccm         s             11  2 inz(0)
     d svCo#           s                   like(MCco#) inz
     d svFnd           s                   like(MCfnd) inz
     d svFyr           s                   like(MCfyr) inz
     d svClm#          s                   like(MCclm#) inz
     d kkCase          s             14    inz

      /free

       //Single Claims, Exclude Multiple Claims
       Setll *loval pcm0d;
       Read pcm0d;
       Dow not %eof(WMCMLD);
          If (svCo# <> CMco# or svFnd <> CMfnd or svFyr <> CMfyr);
          svCo# = CMco#;
          svFnd = CMfnd;
          svFyr = CMfyr;
          Chain (CMco#:CMfnd:CMfyr) pf100;
          Endif;

          If %found(WDF1L) and F1emrt <> 0;
             kkCase = '0000000' + %EditC(CMcase:'X');
             Chain (CMco#:CMfnd:CMfyr:kkCase) pmc00;
             // Skip if Claim part of Multiple Claim
             if not %found(WAMCL) and MeetsTrgClm = *on;
                intAccm = 0;
                Exsr AccumIncur;

                If intAccm <> 0 and ((intAccm/F1emrt)*100) >= cTRreten;
                   ELco# = CMco#;
                   ELfnd = CMfnd;
                   ELemp# = CMemp#;
                   ELdiv = CMdiv;
                   isClaim = *on;
                   SetStRptDv();
                   CheckActions(ELco#);
                Endif;
             Endif;
          Endif;

          Read  pcm0d;
       Enddo;

       //Process Multiple Claim File
       svCo# = 0;
       svFnd = 0;
       svFyr = 0;
       intAccm = 0;

       Setll *loval pmc01;
       Read pmc01;
       svCLM# = MCclm#;

       Dow not %eof(WAMCL1);
          If (svCo# <> MCco# or svFnd <> MCfnd or svFyr <> MCfyr);
             svCo# = MCco#;
             svFnd = MCfnd;
             svFyr = MCfyr;
             Chain (MCco#:MCfnd:MCfyr) pf100;
          Endif;

          If %found(WDF1L) and F1emrt <> 0;
             intCase = %int(MCcase);
             Chain (MCco#:MCfnd:MCfyr:intCase) pcml1;
             If %found(WMCML1);
                Exsr AccumIncur;
             Endif;
          Endif;

          Read pmc01;

          If (svCo# <> MCco# or svFnd <> MCfnd or svFyr <> MCfyr
          or svCLM# <> MCclm#) or %eof(WAMCL1);
             If intAccm <> 0 and ((intAccm/F1emrt)*100) >= cTRreten;
                Exsr LoopMCclaim;
             Endif;
             intAccm = 0;
             svCLM# = MCclm#;
          Endif;

       Enddo;

       Return *off;

       //===================================================================
       // AccumIncur -  Accumuluate Incurred Loss
       //===================================================================
       BegSr AccumIncur;
          If MeetsTrgTbl('Reserve':CMrt01) = *on;
             intAccm = intAccm + CMrr01 + CMar01;
          Endif;
          If MeetsTrgTbl('Reserve':CMrt02) = *on;
             intAccm = intAccm + CMrr02 + CMar02;
          Endif;
          If MeetsTrgTbl('Reserve':CMrt03) = *on;
             intAccm = intAccm + CMrr03 + CMar03;
          Endif;
          If MeetsTrgTbl('Reserve':CMrt04) = *on;
             intAccm = intAccm + CMrr04 + CMar04;
          Endif;
          If MeetsTrgTbl('Reserve':CMrt05) = *on;
             intAccm = intAccm + CMrr05 + CMar05;
          Endif;
          If MeetsTrgTbl('Reserve':CMrt06) = *on;
             intAccm = intAccm + CMrr06 + CMar06;
          Endif;
          If MeetsTrgTbl('Reserve':CMrt07) = *on;
             intAccm = intAccm + CMrr07 + CMar07;
          Endif;
          If MeetsTrgTbl('Reserve':CMrt08) = *on;
             intAccm = intAccm + CMrr08 + CMar08;
          Endif;
          If MeetsTrgTbl('Reserve':CMrt09) = *on;
             intAccm = intAccm + CMrr09 + CMar09;
          Endif;
          If MeetsTrgTbl('Reserve':CMrt10) = *on;
             intAccm = intAccm + CMrr10 + CMar10;
          Endif;
       Endsr;

       //===================================================================
       // LoopMCclaim -  Loop Multiple Claim for MCClaim#
       //===================================================================
       BegSr LoopMCclaim;

       Setll (svCo#:svFnd:svClm#:svFyr) pmc02;
       Reade (svCo#:svFnd:svClm#:svFyr) pmc02;
       Dow not %eof(WAMCL2);

          intCase = %int(aMCcase);
          Chain (aMCco#:aMCfnd:aMCfyr:intCase) pcml1;
          if %found(WMCML1) and MeetsTrgClm = *on;
             ELco# = CMco#;
             ELfnd = CMfnd;
             ELemp# = CMemp#;
             ELdiv = CMdiv;
             isClaim = *on;
             SetStRptDv();
             CheckActions(ELco#);
          Endif;
          Reade (svCo#:svFnd:svClm#:svFyr) pmc02;
       Enddo;

       Endsr;

      /end-free
     p ClmReten        e

       //===================================================================
       // Check claim flags for split wages
       //===================================================================
     pClmSplitWage     b
     dClmSplitWage     pi              n
      /Free
       Select;
       When cTRspltWg0 = 'Y' and CMspltWg <> 0;
          Return *off;
       When cTRspltWg = 'Y' and CMspltWg = 0;
          Return *off;
       Endsl;
       Return *on;
      /End-free
     pClmSplitWage     e

       //===================================================================
        // RtnProcPtr - Return Subprocedure Pointer
       //===================================================================
     p RtnProcPtr      b                   export

     d rtnProcPtr      pi              *   procptr
     d  srvPgm                       10    value
     d  exportName                  256    value

     d AUTH_EXECUTE    s              2    Inz(x'0010')
     d WLI_SRVPGM      s              2    Inz(x'0203')

     d actMark         s             10i 0
     d actInfoLn       s             10i 0 Inz(%size(QLEABPI))
     d exportNo        s             10i 0
     d exportNameLn    s             10i 0
     d exportType      s             10i 0

     d exportPtr       s               *   procptr
     d programPtr      s               *   procptr

      /copy qsysinc/qrpglesrc,qleawi
      /copy qsysinc/qrpglesrc,qusec

      /free

       exportNameLn = %len(%trim(exportName));

       // Resolve a  system pointer to the service program
       // containing the exported subprocedure to use
       programPtr = rslvsp(WLI_SRVPGM:srvPgm:'':AUTH_EXECUTE);

       // Activate the service program whose pointer we just resolved to
       QleActBndPgm(programPtr:actMark:QLEABPI:actInfoLn:QUSEC);

       // Retrieve a pointer to the procedure that we wish to call
       QleGetExp(actMark:exportNo:exportNameLn:exportName:exportPtr:
        exportType:QUSEC);

       Return exportPtr;

      /end-free

     p RtnProcPtr      e
       //===================================================================
       // CheckActions - Check all actions to be processed
       //===================================================================
     p CheckActions    b
     d CheckActions    pi
     d inCo#                          3  0

     d intCo#          s              3  0
     dstrDash          s              2    inz('- ')
     d*** Begin Chg *** 09/14/23 *******************************************
     d chrDbaName      s            100    Inz
     d chrMode         s              1    Inz
     d decEmpRRN       s              9  0 Inz
     d*** End   Chg *** 09/14/23 *******************************************
     d*** Begin Add *** 08/28/25 *******************************************
     d chrCompany#     s              3
     d decCompany#     s              3  0
     d chrGroup#       s              3
     d decGroup#       s              3  0
     d chrEmployer#    s              9
     d decEmployer#    s              9  0
     d chrEmpName      s             40
     d wmempRRN        s              9  0 Inz(0)
     d jsonIC          s                   Sqltype(clob:64000) Inz
     d pos             s              5  0 Inz(0)
     d*** End   Add *** 08/28/25 *******************************************
      /Free
       intCo# = inCo#;
       If not %open(SMTRLREC);
          Open SMTRLREC;
       Endif;
       // Determine what action(s) to take
       Clear ATRaction;
       Setll (intCo#:TRname:TRseq#) ptr0act;
       Dou %eof(smtrpact);
          Reade (intCo#:TRname:TRseq#) ptr0act;
          Select;
          When %eof and ATRaction = *blanks;
             ATRaction = 'Send Diary';
          When %eof;
             Iter;
          Endsl;

          // comments
          If TRdesc = *blanks;
             strDash = *blanks;
          Endif;
          ##comm = TRdart;
          Chain TRdart pct01;
          If %found(STCTL1) and CTdesc <> *blanks;
             ##comm = CTdesc;
          Endif;
          If TRdesc = *blanks
           or ##comm = *blanks;
             strDash = *blanks;
          Endif;
          ##comm = %trim(##comm) + ' ' + %triml(strDash) + TRdesc;
          ##comm = %trim(##comm);

          Clear strMsg;
          Select;
          When TRtype = 'Claims';
             strSubject = 'Claim: ' + %trim(%editc(CMfnd:'X')) +
              '-' + %trim(%editc(CMfyr:'X')) +
              '-' + %trim(%editc(CMcase:'X'));
          When TRtype = 'Policy';
             //*** Begin Add *** 09/14/23 **********************************
             chrMode    = GetEmpEnv(ELco#:ELfnd:ELemp#:ELdiv);
             chrDbaName = GetEmpFullName(ELemp#:ELdiv:decEmpRRN:ELfnd:
                                         ELco#:'DBA':chrMode);
             //*** End   Add *** 09/14/23 **********************************
             //*** Begin Chg *** 09/14/23 *********************************
             //*** strSubject = 'Policy: ' + %trim(%editc(ELfnd:'X')) +
             //***  '-' + %trim(%editc(ELemp#:'X')) +
             //***  '-' + %trim(%editc(ELdiv:'X')) +
             //***  '-' + %trim(%editc(ELfyr:'X'));
             If chrDbaName = ' ';
                strSubject = 'Policy: ' + %trim(%editc(ELfnd:'X')) +
                 '-' + %trim(%editc(ELemp#:'X')) +
                 '-' + %trim(%editc(ELdiv:'X')) +
                 '-' + %trim(%editc(ELfyr:'X'));
             Else;
                strSubject = 'Policy: ' + %trim(%editc(ELfnd:'X')) +
                 '-' + %trim(%editc(ELemp#:'X')) +
                 '-' + %trim(%editc(ELdiv:'X')) +
                 '-' + %trim(%editc(ELfyr:'X')) + '-' + %Trim(chrDbaName);
             EndIf;
             //*** End   Chg *** 09/14/23 *********************************
          Endsl;
       //*** Beg Add *** 12/19/23 *** WC2935 ***************************
          //*** Beg Chg ***** 09/03/24 **********************************
          //***If   TRName = 'BIFFAILURE';
          //***      strSubject = 'BIF Failed to Process For the '     +
          //***                   'Transaction Date & Time of '        +
          //***                   %EditC(dtMDYY(PosBiTrDt):'Y') + ' '  +
          //***                   %Char(%Time(PosBiTrTm):*hms)         ;
          //***
          //***      ##comm = 'BIF Failed with the following message: ' +
          //***               %trim(PosBiMsg) + '<BR>'                  +
          //***               ' Please find in the Failed BIF Listing'  +
          //***               ' menu option using '                     +
          //***               ' the Transaction Date & Time of '        +
          //***                   %EditC(dtMDYY(PosBiTrDt):'Y') + ' '   +
          //***                   %Char(%Time(PosBiTrTm):*hms)          ;
          //***
          //***      If PosBIReSub = 'E';
          //***         ##comm = %Trim(##comm) + ' <BR>' +
          //***                  ' BIF cannot be Re-Submitted.';
          //***      EndIf;
          //***EndIf;
          Select;
          When TRName = 'BIFFAILURE';
             //*** Begin Add *** 08/28/25 **********************************
             // Get Data
             Exec Sql
                Select BIdata
                Into :jsonIC
                From wibifp
                Where BICO#   = :posBIco#     and
                      BITRDT  = :posBItrDt    and
                      BITRTM  = :posBItrTm    and
                      BIRESUB = :posBIresub   and
                      BIMSG   = :posBImsg     and
                      BISTAT  = 'E'           and
                      BIDT    = :posBIdt      and
                      BITIME  = :posBItime    and
                      BIPGM   = :posBIpgm     and
                      BIUSER  = :posBIuser    and
                      BITYPE  = :posBItype;

             pos  = %Scan('"accountNumber"':jsonIC_data);

             pos += 18;
             chrCompany#  = %Subst(jsonIC_data:pos:3);
             decCompany#  = %Dec(chrCompany#:3:0);
             pos += 3;
             chrGroup#    = %Subst(jsonIC_data:pos:3);
             decGroup#    = %Dec(chrGroup#:3:0);
             pos += 3;
             chrEmployer# = %Subst(jsonIC_data:pos:9);
             decEmployer# = %Dec(chrEmployer#:9:0);
             chrEmpName   = GetEmpName(decEmployer#:0:wmempRRN:decGroup#);
             //*** End   Add *** 08/28/25 **********************************
             strSubject = 'BIF Failed to Process For the ' +
              'Transaction Date & Time of ' +
              %EditC(dtMDYY(PosBiTrDt):'Y') + ' '  +
              //*** bEGIN Chg *** 08/28/25 *********************************
              //*** %Char(%Time(PosBiTrTm):*hms);
              %Char(%Time(PosBiTrTm):*hms) +
              ' Group#: ' + chrGroup# + ' Employer#: ' + chrEmployer# +
              '-' + %Trim(chrEmpName);
              //*** End   Chg *** 08/28/25 *********************************

             ##comm = 'BIF Failed with the following message: ' +
              %trim(PosBiMsg) + '<BR>'                  +
              ' Please find in the IVC Failure Listing'  +
              ' menu option using ' +
              ' the Transaction Date & Time of ' +
              %EditC(dtMDYY(PosBiTrDt):'Y') + ' ' +
              %Char(%Time(PosBiTrTm):*hms);

              //*** Begin Del *** 08/28/25 *********************************
              //*** If PosBIReSub = 'E';
              //***    ##comm = %Trim(##comm) + ' <BR>' +
              //***     ' BIF cannot be Re-Submitted.';
              //*** EndIf;
              //*** End   Del *** 08/28/25 *********************************
          When TRName = 'IVCNAMADDR';
             //*** Begin Add *** 08/28/25 **********************************
             // Get Data
             Exec Sql
                Select BIdata
                Into :jsonIC
                From wibifp
                Where BICO#   = :posBIco#     and
                      BITRDT  = :posBItrDt    and
                      BITRTM  = :posBItrTm    and
                      BIRESUB = :posBIresub   and
                      BIMSG   = :posBImsg     and
                      BISTAT  = 'E'           and
                      BIDT    = :posBIdt      and
                      BITIME  = :posBItime    and
                      BIPGM   = :posBIpgm     and
                      BIUSER  = :posBIuser    and
                      BITYPE  = :posBItype;

             chrCompany#  = %Subst(jsonIC_data:89:3);
             decCompany#  = %Dec(chrCompany#:3:0);
             chrGroup#    = %Subst(jsonIC_data:92:3);
             decGroup#    = %Dec(chrGroup#:3:0);
             chrEmployer# = %Subst(jsonIC_data:95:9);
             decEmployer# = %Dec(chrEmployer#:9:0);
             chrEmpName   = GetEmpName(decEmployer#:0:wmempRRN:decGroup#);
             //*** End   Add *** 08/28/25 **********************************
             strSubject = 'Address/Name Change failed for the ' +
              'Transaction Date & Time of ' +
              %EditC(dtMDYY(PosBiTrDt):'Y') + ' '  +
              //*** Begin Chg *** 08/28/25 *********************************
              //*** %Char(%Time(PosBiTrTm):*hms);
              %Char(%Time(PosBiTrTm):*hms) +
              ' Group#: ' + chrGroup# + ' Employer#: ' + chrEmployer# +
              '-' + %Trim(chrEmpName);
              //*** End   Chg *** 08/28/25 *********************************

             ##comm = 'Address/Name Failed with the following message: ' +
              %trim(PosBiMsg) + '<BR>'                  +
              ' Please find in the IVC Failure Listing'  +
              ' menu option using ' +
              ' the Transaction Date & Time of ' +
              %EditC(dtMDYY(PosBiTrDt):'Y') + ' ' +
              %Char(%Time(PosBiTrTm):*hms);

              //*** Begin Del *** 08/28/25 *********************************
              //*** If PosBIReSub = 'E';
              //***    ##comm = %Trim(##comm) + ' <BR>' +
              //***     ' BIF cannot be Re-Submitted.';
              //*** EndIf;
              //*** End   Del *** 08/28/25 *********************************
          Endsl;
          //*** End Chg ***** 09/03/24 **********************************
       //*** Beg Add *** 12/19/23 *** WC2935 ***************************

          // process all recipients
          Setll (TRco#:TRname:TRseq#) ptr0rec;
          Dou %eof(smtrlrec);
             Reade (TRco#:TRname:TRseq#) ptr0rec;
             Select;
             When %eof;
                Iter;
             Endsl;
             Select;
             When ATRaction = 'Send Email';
                strBody = %trim(##comm);
                WriteEmail();
             When ATRaction = 'Send Text';
                strMsg = %trim(strSubject) + ' -- ' + %trim(##comm);
                WriteText();
             When ATRaction = 'Send Diary';
                WriteDARTS(TRname);
             Endsl;
          Enddo;
       Enddo;
       If %open(SMTRLREC);
          Close SMTRLREC;
       Endif;
       Return;
      /End-free
     p CheckActions    e
       //===================================================================
       // WriteDARTS - Write Diary Records into Darts file
       //===================================================================
     p WriteDARTS      b
     d WriteDARTS      pi
     d triggerName                   10

     d charTrgType     s              1
     d strKey          s             40    inz

      /free

       // check for multi occurrence setup for diary items
       If CheckMultOccur(triggerName) = *on;


             // process DARTS remark
             Exsr ProcDarts;
       Endif;

       //===================================================================
       // ProcDarts -  Process writing DARTS remark
       //===================================================================
       BegSr ProcDarts;

       charTrgType = GetTrgInfo(triggerName:strKey);

       // Write Diary
       ##key = strKey;

       // date
       Select;
       When TRdiaryDt = 'SystemDt';
          ##cDat = %editc(dtFuture(DateTo8(systemDate()):TRaddlDays):'X');
       When TRdiaryDt = 'DtRcvd';
          ##cDat = %editc(dtFuture(dtMDYY(CMrcvd):TRaddlDays):'X');
       When TRdiaryDt = 'AccDate';
          ##cDat = %editc(dtFuture(dtMDYY(CMacDt):TRaddlDays):'X');
       When TRdiaryDt = 'RptDate';
          ##cDat = %editc(dtFuture(dtMDYY(CMdtRp):TRaddlDays):'X');
       Endsl;

       ##pnam = 'W0050R';
       If charTrgType = 'C';
          ##pnam = 'W0070R';
       Endif;
       ##pgm  = Q1pgm;
       ##iPgm = Q1pgm;
       ##secl = %editc(TRsecLvl:'X');
       If TRsecLvl <> 0;
          ##sbyl = 'Y';
       Else;
          ##sbyl = 'N';
       EndIf;
       ##diFl = 'Y';
       ##fl01 = ' ';
       ##fl02 = ' ';
       ##seq# = '00000';
       ##com2 = ' ';
       ##apId = ' ';
       ##iDoc = ' ';
       ##iQue = ' ';
       ##iPth = ' ';
       ##pIdx = ' ';
       ##qPty = ' ';
       ##dcTp = ' ';
       ##csId = ' ';
       ##code = ' ';
       ##user = GetRecipient();
       ##rmcd = TRdart;
       ##dcid = ' ';
       ##fold = ' ';
       ##imgF = ' ';

       WrtDrtRmk(##key:##comm:##cDat:##dcId:##pNam:##secl:##user:
        ##pgm:##fold:##diFl:##iPgm:##fl01:##fl02:##rmCd:##seq#:
        ##sbyl:##com2:##apId:##iDoc:##iQue:##imgF:##iPth:##pIdx:
        ##qPty:##dcTp:##csId:##code);

       mTRco#   = TRco#;
       mTRname  = TRname;
       mTRseq#  = TRseq#;
       mTRcmKey = ##key;
       mTRcmSeq#= %int(##seq#);
       mTRstat  = 'A';
       mTRiUsr  = Q1user;
       mTRipgm   = Q1pgm;
       mTRiTim  = Timeto6(SystemTime());
       mTRiDt   = Dateto7(SystemDate());
       write ptr0drt;

       Endsr;

      /end-free
     p WriteDARTS      e
       //===================================================================
       // WriteEmail- Write email message
       //===================================================================
     p WriteEmail      b
     d WriteEmail      pi

       //***  Begin Chg  ***  02/01/22  *************
     d*emailFrom**     s             60    inz
     d*emailTo****     s             60    inz
     d emailFrom       s             80    inz varying
     d emailTo         s             80    inz varying
       //***  End   Chg  ***  02/01/22  *************
       //***  Begin Add  ***  05/13/21  ***********************************
     D strSuffix       s              3    inz
     D strSuffixCmd    s             15    inz varying
       //***  End   Add  ***  05/13/21  ***********************************
     d strUser         s             10    inz
       // SNDEMAIL FROM(FROM@EMAIL.COM) TO(TO@EMAIL.COM) SUBJECT(SUBJECT)
       // BODY(BODY)
      /Free
       In dsSpiTrigger;
       //***  Begin Chg  ***  02/01/22  *************
       //emailFrom = dsFromEmail;
       emailFrom = '''' + %trim(dsFromEmail) + '''';
       //***  End   Chg  ***  02/01/22  *************
       Select;
       When ATRexternal = 'I';
          strUser = GetRecipient();
          Clear UPmail;
          Chain strUser SMUPL;
          emailTo = UPmail;
       When ATRexternal = 'E';
          emailTo = GetRecipientE('Email');
       Other;
          Return;
       Endsl;
       If emailTo = *blanks;
          Return;
       Endif;
       //***  Begin Chg  ***  05/13/21  ***********************************
       //strCmd = 'SNDEMAIL FROM(' + %trim(emailFrom) + ') ' +
       // 'TO(' + %trim(emailTo) + ') ' +
       // 'SUBJECT(''' + %trim(strSubject) + ''') ' +
       // 'BODY(''' + %trim(##comm) + ''') ';
       RtvSuffix(strSuffix);
       Select;
       When strSuffix <> *blanks;
          strSuffixCmd = ' SUFFIX(' + strSuffix + ')';
       Endsl;
       //***  Beg Chg  *** 01/23/24 *** 114936 ***************************
       //* Escape aprostrophe marks for the email command.
       //*** Begin Del *** 11/04/24 **************************************
       //*strSubject = %scanrpl('''':'''''':strSubject);
       //*##comm  = %scanrpl('''':'''''':##comm);
       //*** End   Del *** 11/04/24 **************************************
       //***  End Chg  *** 01/23/24 *** 114936 ***************************
       //***  Begin Chg  ***  02/01/22  *************
       //strCmd = 'SNDEMAIL FROM(' + %trim(emailFrom) + ') ' +
       // 'TO(' + %trim(emailTo) + ') ' +
       // 'SUBJECT(''' + %trim(strSubject) + ''') ' +
       // 'BODY(''' + %trim(##comm) + ''') ' + strSuffixCmd;
       emailTo = '''' + %trim(emailTo) + '''';
       //*** Begin Chg *** 11/04/24 **************************************
       //*** strCmd = 'SNDEMAIL FROM(' + emailFrom + ') ' +
       //***  'TO(' + emailTo + ') ' +
       //***  'SUBJECT(''' + %trim(strSubject) + ''') ' +
       //***  'BODY(''' + %trim(##comm) + ''') ' + strSuffixCmd;
       strCmd = 'SNDEMAIL FROM(' + emailFrom + ') ' +
        'TO(' + emailTo + ') ' +
        'SUBJECT(''' + %trim(%scanrpl('''':'''''':strSubject)) + ''') ' +
        'BODY(''' + %trim(%scanrpl('''':'''''':##comm)) + ''') ' +
        strSuffixCmd;
       //*** End   Chg *** 11/04/24 **************************************
       //***  End   Chg  ***  02/01/22  *************
       //***  End   Add  ***  05/13/21  ***********************************
       RunCmd(strCmd:%len(%trim(strCmd)));
      /End-free
     p WriteEmail      e

       //===================================================================
       // WriteText - Write text message
       //===================================================================
     p WriteText       b
     d WriteText       pi

     d smsTo           s             15    inz
     d strUser         s             10    inz
       // SNDSMS TO(SMS#) MESSAGE(MESSAGE)
      /Free
       Select;
       When ATRexternal = 'I';
        // Rogulski
        //Clear UPtext;
          strUser = GetRecipient();
          Chain strUser SMUPL;
          Select;
          When UPphnTxt = 'Y';
             smsTo = %trim(%editc(UPphnA:'Z')) + %trim(%editc(UPphn#:'Z'));
          When UPaltTxt = 'Y';
             smsTo = %trim(%editc(UPalt#:'Z'));
          Endsl;
       When ATRexternal = 'E';
          smsTo = GetRecipientE('Text');
       Other;
          Return;
       Endsl;
       If smsTo = *blanks;
          Return;
       Endif;
       strCmd = 'SNDSMS TO(' + %trim(smsTo) + ') ' +
        'MESSAGE(''' + %trim(strMsg) + ''') ';
       RunCmd(strCmd:%len(%trim(strCmd)));
      /End-free
     p WriteText       e
       //===================================================================
       // CheckMultOccur - Check multiple occurrences
       //===================================================================
     p CheckMultOccur  b
     d CheckMultOccur  pi              n
     d  triggerName                  10

     d intCntAll       s              5  0 inz
     d intCntOpn       s              5  0 inz
     d strKey          s             40    inz

      /free

       GetTrgInfo(triggerName:strKey);
       intCntAll = 0;
       intCntOpn = 0;

       // get diary items count
       Setll (TRco#:TRname:TRseq#:strkey) ptr0drt;
       Reade (TRco#:TRname:TRseq#:strkey) ptr0drt;
       Dow not %eof(SMTRPDRT);
          If TRstat <> 'D';
             Chain (strKey:TRdart:mTRcmSeq#) pcm0s;
             Select;
             When not %found(SDCMLU);
                mTRstat = 'D';
             When sCMdcDt <> 0;
                mTRstat = 'I';
             EndSL;

             If (mTRstat = 'D') or (mTRstat = 'I');
                mTRuser  = Q1user;
                mTRpgm   = Q1pgm;
                mTRtime  = Timeto6(SystemTime());
                mTRdt    = Dateto7(SystemDate());
                update ptr0drt;
             Endif;
          Endif;
          //*** Beg Add *** 10/18/23 **********************************
          // Make sure the record does not hang in locked status
          Monitor;
          Update ptr0Drt;
          On-Error;
          Endmon;
          //*** End Add *** 10/18/23 **********************************

          Select;
          When mTRstat = 'I';
             intCntAll = intCntAll + 1;
          When mTRstat = 'A';
             intCntAll = intCntAll + 1;
             intCntOpn = intCntOpn + 1;
          EndSL;
          If intCntAll > 0 and intCntOpn > 0;
             Leave;
          Endif;

          Reade (TRco#:TRname:TRseq#:strkey) ptr0drt;
       Enddo;

       Select;
       When %trim(TR#Occur) = 'One' And intCntAll > 0;
          Return *off;
       When %trim(TR#Occur) = 'OneOpen' And intCntOpn > 0;
          Return *off;
       Endsl;

       Return *on;

      /end-free
     p CheckMultOccur  e
       //===================================================================
       // GetTrgInfo - Get Auto Diary Trigger Info
       //===================================================================
     p GetTrgInfo      b
     d GetTrgInfo      pi             1
     d inTrgName                     10
     d instrKey                      40    options(*nopass)

      * Claims DARTS Key data structure
     d claimsKey       ds            40
     d  charCo                 1      3
     d  charFnd                4      6
     d  charYr                 7      9
     d  charClaim             10     16

      * Policy DARTS Key data structure
     d policyKey       ds            40
     d  charCo##               1      3
     d  charFnd##              4      6
     d  charEmp                7     15
     d  charDiv               16     20
     d  charQteFlg            40     40

     d strType         s              1    inz
     d strTbId         s             10    inz
     d intCo#          s              3  0 inz
      /free

       charQteFlg = ' ';
       strType  = ' ';
       strTbId  = 'AXKY';

       Select;
       When CMco# <> 0;
          intCo# = CMco#;
       When ELco# <> 0;
          intCo# = ELco#;
       EndSl;

       Chain (intCo#:strTbId:inTrgName) pgt00;
       If %found(STGTL);
          strType = GTfg01;
       Endif;

       If %parms >= 2;
          inStrKey = ' ';
          If strType = 'C';
          // get claims DARTS key
             charCo  = %editc(CMco#:'X');
             charFnd = %editc(CMfnd:'X');
             charYr  = %editc(CMfyr:'X');
             charClaim = %editc(CMcase:'X');
             inStrKey  = claimsKey;

          ElseIf strType = 'P';
          // get policy DARTS key
             charCo##  = %editc(ELco#:'X');
             charFnd## = %editc(ELfnd:'X');
             charEmp   = %editc(ELemp#:'X');
             charDiv   = %editc(ELdiv:'X');

             //*** Begin Chg *** 05/13/21 **********************************
             //If Upper(inTrgName) = 'QTEENTRY' or Upper(inTrgName)= 'QTERVW'or
             //Upper(inTrgName) = 'NQTESTS' or Upper(inTrgName) = 'RQTESTS';
             If Upper(inTrgName) = 'QTEENTRY' or Upper(inTrgName)= 'QTERVW' or
              Upper(inTrgName) = 'NQTESTS' or Upper(inTrgName) = 'RQTESTS' or
              Upper(inTrgName) = 'QTE3PRTPMT';
             //*** End   Chg *** 05/13/21 **********************************
                charQteFlg = 'Q';
             Endif;
             inStrKey = policyKey;
          Endif;
       Endif;
       Return strType;

      /end-free
     p GetTrgInfo      e
       //===================================================================
       // GetRecipient
       //===================================================================
     p GetRecipient    b
     d GetRecipient    pi            10

     d strUser         s             10    inz

      /free

       clear strUser;

       Select;
       When rTRtype = 'Individual';
          strUser = ValidUser(rTRrecip);
       When rTRtype = 'Role';
          strUser = GetRoleUser(rTRrecip);
          strUser = ValidUser(strUser);
       Endsl;

       If strUser = *blank;
          Select;
          When rTRaltType = 'Individual';
             strUser = ValidUser(rTRaltRecip);
          When rTRaltType = 'Role';
             strUser = GetRoleUser(rTRaltRecip);
             strUser = ValidUser(strUser);
          Endsl;
       Endif;

       Return strUser;

      /end-free
     p GetRecipient    e

       //===================================================================
       // Get external recipient information
       //===================================================================
     p GetRecipientE   b
     d GetRecipientE   pi           100
     d  inType                       10    const

     d strDefEmail     s            100    inz
     d strDefText      s            100    inz
     d strOutEmail     s            100    inz
     d strOutInfo      s            100    inz
     d strOutText      s            100    inz
     d strPlaEmail     s            100    inz
     d strPlaText      s            100    inz

      /free

       clear strOutInfo;
       clear strOutEmail;
       clear strOutText;

       Select;
       When %subst(rTRrecip:1:7) = 'PolCont';
          Exsr GetPolicyCont;
       When %subst(rTRrecip:1:7) = 'ClmAtty';
          Exsr GetClaimAtys;
       When rTRrecip = 'Claimant';
          Exsr GetClaimaint;
       Endsl;

       Select;
       When inType = 'Text';
          strOutInfo = %trim(strOutText);
       When inType = 'Email';
          strOutInfo = %trim(strOutEmail);
       Endsl;

       Return strOutInfo;

       //====================================================================
       Begsr GetClaimaint;
         If not %open(WMCDL);
            Open WMCDL;
         Endif;
         Clear CDcAra;
         Clear CDcPhn;
         Clear CDarea;
         Clear CDphn#;
         Clear CDemAd;
         Chain (CMss#) WMCDL;
         Select;
         When CDcAra <> *zero and CDcPhn <> *zero;
            strOutText = %trim(%editc(CDcAra:'Z')) + %trim(%editc(CDcPhn:'Z'));
         Other;
            strOutText = %trim(%editc(CDArea:'Z')) + %trim(%editc(CDPhn#:'Z'));
         Endsl;
         strOutEmail = %trim(CDemAd);
       Endsr;

       //====================================================================
       Begsr GetClaimAtys;
         If not %open(WMLLL);
            Open WMLLL;
         Endif;
         If not %open(WMPDL);
            Open WMPDL;
         Endif;
         Clear LLdefA;
         Clear LLdefM;
         Clear LLdefR;
         Clear LLdefP;
         Clear LLemDD;
         Clear LLplaA;
         Clear LLplaM;
         Clear LLplaR;
         Clear LLplaP;
         Clear LLemDP;
         Chain (CMco#:CMfnd:CMfyr:CMcase) wmlll;
         strDefText = %trim(%editc(LLdefR:'Z')) + %trim(%editc(LLdefP:'Z'));
         strPlaText = %trim(%editc(LLplaR:'Z')) + %trim(%editc(LLplaP:'Z'));
         strDefEmail = %trim(LLemDD);
         strPlaEmail = %trim(LLemDP);
         // Load defense attorney info from payee demographics if not at claim
         Clear PDemAd;
         Clear PDarea;
         Clear PDphn#;
         Chain (LLdefA:LLdefM) wmpdl;
         If strDefText = *blank;
            strDefText = %trim(%editc(PDarea:'Z')) + %trim(%editc(PDphn#:'Z'));
         Endif;
         If strDefEmail = *blanks;
            strDefEmail = %trim(PDemAd);
         Endif;
         // Load plantiff attorney info from payee demographics if not at claim
         Clear PDemAd;
         Clear PDarea;
         Clear PDphn#;
         Chain (LLplaA:LLplaM) wmpdl;
         If strPlaText = *blank;
            strPlaText = %trim(%editc(PDarea:'Z')) + %trim(%editc(PDphn#:'Z'));
         Endif;
         If strPlaEmail = *blanks;
            strPlaEmail = %trim(PDemAd);
         Endif;
         Select;
         When %subst(rTRrecip:8:3) = 'Def';
            strOutEmail = strDefEmail;
            strOutText = strDefText;
         When %subst(rTRrecip:8:3) = 'Pln';
            strOutEmail = strPlaEmail;
            strOutText = strPlaText;
         Endsl;
       Endsr;

       //====================================================================
       Begsr GetPolicyCont;
         If not %open(WMECL1);
            Open WMECL1;
         Endif;
         Clear ECcAra;
         Clear ECcPhn;
         Clear ECarea;
         Clear ECphn#;
         Clear ECemal;
         Chain (EMco#:EMfnd:EMemp#:EMdiv:%subst(rTRrecip:8:1)) wmecl1;
         Select;
         When ECcAra <> *zero and ECcPhn <> *zero;
            strOutText = %trim(%editc(ECcAra:'Z')) + %trim(%editc(ECcPhn:'Z'));
         Other;
            strOutText = %trim(%editc(ECArea:'Z')) + %trim(%editc(ECPhn#:'Z'));
         Endsl;
         strOutEmail = %trim(ECemal);
       Endsr;

      /end-free
     p GetRecipientE   e

       //===================================================================
       // ValidUser
       //===================================================================
     p ValidUser       b
     d ValidUser       pi            10
     d inStrUser                     10

      /free

       Chain inStrUser pup00;
       If not %found(SMUPL) or UPstat = 'I';
          Return *blank;
       Endif;

       Return inStrUser;

      /end-free
     p ValidUser       e
       //===================================================================
       // GetRoleUser
       //===================================================================
     p GetRoleUser     b
     d GetRoleUser     pi            10
     d inRole                        10

     d strStaffCode    s              3    inz
      /free

       // Get Staff Code
       Select;
       When inRole = 'ClmAdj';
          strStaffCode = CMclex;
       When inRole = 'ClmSuper';
          strStaffCode = CMsupr;
       When inRole = 'CaseNurse';
          strStaffCode = CMnrse;
       When inRole = 'AcctMgr';
          strStaffCode = CMaMan;
       When inRole = 'ClmMgr';
          strStaffCode = CMuMan;
       When inRole = 'OutAdj';
          strStaffCode = CMfeAd;
       When inRole = 'SDFAdj';
          strStaffCode = CMsdfA;
       When inRole = 'ExcessAdj';
          strStaffCode = CMeSup;
       When inRole = 'LossCtl';
          strStaffCode = empDs1.EMlRep;
       When inRole = 'UnderWtr';
          strStaffCode = empDs1.EMuwtr;
       When inRole = 'PolSvcs';
          strStaffCode = empDs1.EMpSrv;
       When inRole = 'PremAcctng';
          strStaffCode = empDs1.EMpAcc;
       When inRole = 'AuditRvwr';
          strStaffCode = empDs1.EMaRvw;
       When inRole = 'AuditMgr';
          strStaffCode = empDs1.EMaMgr;
       When inRole = 'PolClmCont';
          strStaffCode = empDs1.EMclmC;
       Endsl;

       Chain strStaffCode pup01;
       If not %found(SMUPL1) or UPstat = 'I';
          Return *blank;
       Endif;

       Return UPusr;

      /end-free
     p GetRoleUser     e
       //===================================================================
       // MeetsTrgPolicy - Validate for policy trigger criteria
       //===================================================================
     p MeetsTrgPolicy  b
     d MeetsTrgPolicy  pi              n

     d strStat         s              1    inz('W')

      /free

       // read policy trigger control file
       Chain (TRco#:TRname:TRseq#) ptr0pol;
       If not %found(SMTRPPOL);
          Return *on;
       Endif;

       //*** Begin Chg *** 05/13/21 ***************************************
       //*** If Upper(TRname) = 'QTEENTRY' or Upper(TRname)= 'QTERVW' or
       //***  Upper(TRname) = 'NQTESTS' or Upper(TRname) = 'RQTESTS';
       If Upper(TRname) = 'QTEENTRY' or Upper(TRname)= 'QTERVW' or
        Upper(TRname) = 'NQTESTS' or Upper(TRname) = 'RQTESTS' or
        Upper(TRname) = 'QTE3PRTPMT';
       //*** End   Chg *** 05/13/21 ***************************************
          strStat = 'Q';
       Endif;

       Select;
       When (pTRpolSts = 'Active' and GetCxDate(ELco#:ELfnd:ELemp#:ELdiv) <> 0)
        or (pTRpolSts= 'Cancelled' and GetCxDate(ELco#:ELfnd:ELemp#:ELdiv) = 0);
          Return *off;
       When (pTRqteType = 'New' and ELnwRw <> 'N') or (pTRqteType = 'Renewal'
        and ELnwRw <> 'R');
          Return *off;
       When not MeetsTrgTbl('Quote Status':GetQuoteSts(ELco#:ELfnd:ELemp#:ELdiv:
        ELfyr:strStat)) and (Upper(TRname) <> 'NQTESTS' and Upper(TRname) <>
        'RQTESTS');
          Return *off;
       Endsl;

       Return *on;

      /end-free
     p MeetsTrgPolicy  e
       //===================================================================
       // MeetsTrgClm - Validate for claims trigger criteria
       //===================================================================
     p MeetsTrgClm     b
     d MeetsTrgClm     pi              n

      /free

       Chain (TRco#:TRname:TRseq#) ptr0clm;
       If not %found(SMTRPCLM);
          Return *on;
       Endif;

       Select;
       When cTRclmSts <> ' ' and
        ((cTRclmSts = 'Open' and CMstat <>'O')
        or (cTRclmSts = 'Closed' and CMstat <> 'C'));
          Return *off;
       When cTRclmType <> ' ' and CMlTFl <> ' ' and %scan(CMlTFl:CTRclmType) =0;
          Return *off;
       When not MeetsTrgTbl('States':CMjrSt);
          Return *off;
       When cTRrSet = 'Y' and not ValidIncurredLoss();
          Return *off;
       When cTRclmSts <> ' ' and
         ((cTRclmSts = 'Open' and CMstat ='O')
         or (cTRclmSts = 'Closed' and CMstat ='C')) and StsDescExist() = *Off;
         Return *off;
       When ClmSplitWage() = *off;
          Return *off;
       Endsl;

       Return *on;

      /end-free
     p MeetsTrgClm     e
       //===============================================================
       // StsDescExist - Status description Exist
       //===============================================================
     p StsDescExist    b
     d StsDescExist    pi              n

     d flgStsDesc      s              1A

      /free

       // check table file setup with Status Description
       flgStsDesc = 'N';
       Chain (TRco#:TRname:TRseq#:'Status Description') ptr0tbl;
       If %found(SMTRPTBL);
          flgStsDesc = 'Y';
       Else;
          Return *on;
       Endif;

       If flgStsDesc = 'Y';
          Setgt  (CMco#:CMfnd:CMfyr:CMcase) pch00;
          Readpe (CMco#:CMfnd:CMfyr:CMcase) pch00;

          Setll (TRco#:TRname:TRseq#:'Status Description') ptr0tbl;
          Reade (TRco#:TRname:TRseq#:'Status Description') ptr0tbl;
          Dow not %eof(SMTRPTBL);
             Chain (tTRco#:'OCST':tTRkey) pgtw0;
                If %found(wtgtl) and wGTdesc = CHexpl;
                   Return *on;
                Endif;
             Reade (TRco#:TRname:TRseq#:'Status Description') ptr0tbl;
          Enddo;
       Endif;

       Return *off;

      /end-free
     p StsDescExist    e
       //===================================================================
       // MeetsTrgTbl - Validate for table trigger id/key
       //===================================================================
     p MeetsTrgTbl     b
     d MeetsTrgTbl     pi              n
     d inTable                       25    const
     d inKey                         10    const

      /free

       // check table file setup
       Setll (TRco#:TRname:TRseq#:inTable) ptr0tbl;
       Reade (TRco#:TRname:TRseq#:inTable) ptr0tbl;
       If %eof(SMTRPTBL) and (inTable <> 'NCCI Injury' and
         inTable <> 'Nature' and inTable <> 'Part' and inTable <> 'Cause');
          Return *on;
       Endif;

       Dow not %eof(SMTRPTBL);
          If %trim(tTRkey) = %trim(inKey);
             Return *on;
          Endif;
          Reade (TRco#:TRname:TRseq#:inTable) ptr0tbl;
       Enddo;

       Return *off;

      /end-free
     p MeetsTrgTbl     e
       //===================================================================
       // MeetsTrgTblNPC - Validate for table trigger id/key for NPC
       //===================================================================
     p MeetsTrgTblNPC  b
     d MeetsTrgTblNPC  pi              n
       //***  Begin Del  ***  12/17/20  ***********************
     d*inTable                       25    const
     d*inKey                         10    const
       //***  End   Del  ***  12/17/20  ***********************
     d
       //***  Begin Chg  ***  12/17/20  ***********************
     d*NPCmatch        s              1n
     d*NPCnextRule     s              1n
     ddsSMTRPTBL     e ds                  extname(SMTRPTBL) qualified inz
       //***  End   Chg  ***  12/17/20  ***********************

      /free
       //***  Begin Chg  ***  12/17/20  ***********************
       //NPCmatch = *off;
       //NPCnextrule = *on;
       // Does the rule use Nature?  If so compare to claim Nature.
       //Chain (TRco#:TRname:TRseq#:'Nature') ptr0tbl;
       //If %Found(smtrptbl);
       //   If %trim(tTrkey) = aftCMNatr;
       //      NPCmatch = *on;
       //   Else;
       //      NPCnextRule = *off;
       //   Endif;
       //Endif;

       // Does the rule use Part?  If so compare to claim Part.
       //Chain (TRco#:TRname:TRseq#:'Part') ptr0tbl;
       //If %Found(smtrptbl) and NPCnextRule = *on;
       //   If %trim(tTrkey) = aftCMPart;
       //      NPCMatch = *on;
       //   Else;
       //      NPCmatch = *off;
       //      NPCnextRule = *off;
       //   Endif;
       //Endif;

       // Does the rule use Cause?  If so compare to claim Cause.
       //Chain (TRco#:TRname:TRseq#:'Cause') ptr0tbl;
       //If %Found(smtrptbl) and NPCnextRule = *on;
       //   If %trim(tTrkey) = aftCMCaus;
       //      NPCMatch = *on;
       //   Else;
       //      NPCMatch = *off;
       //   Endif;
       //Endif;

       //If NPCmatch = *on;
       //   Return *on;
       //Else;
       //   Return *off;
       //Endif;
       // Does the rule use Nature?  If so compare to claim Nature.
       Exec sql
        select * into :dsSMTRPTBL
         from SMTRPTBL
         where TRco# = :TRco# and TRname = :TRname and TRseq# = :TRseq#
          and TRtable = 'Nature'
         limit 1;
       If IsValidFetch(sqlstt);
          Exec sql
           select * into :dsSMTRPTBL
            from SMTRPTBL
            where TRco# = :TRco# and TRname = :TRname and TRseq# = :TRseq#
             and TRtable = 'Nature' and TRkey = :aftCMNatr
            limit 1;
          If not IsValidFetch(sqlstt);
             Return *off;
          Endif;
       Endif;

       // Does the rule use Part?  If so compare to claim Part.
       Exec sql
        select * into :dsSMTRPTBL
         from SMTRPTBL
         where TRco# = :TRco# and TRname = :TRname and TRseq# = :TRseq#
          and TRtable = 'Part'
         limit 1;
       If IsValidFetch(sqlstt);
          Exec sql
           select * into :dsSMTRPTBL
            from SMTRPTBL
            where TRco# = :TRco# and TRname = :TRname and TRseq# = :TRseq#
             and TRtable = 'Part' and TRkey = :aftCMPart
            limit 1;
          If not IsValidFetch(sqlstt);
             Return *off;
          Endif;
       Endif;

       // Does the rule use Cause?  If so compare to claim Cause.
       Exec sql
        select * into :dsSMTRPTBL
         from SMTRPTBL
         where TRco# = :TRco# and TRname = :TRname and TRseq# = :TRseq#
          and TRtable = 'Cause'
         limit 1;
       If IsValidFetch(sqlstt);
          Exec sql
           select * into :dsSMTRPTBL
            from SMTRPTBL
            where TRco# = :TRco# and TRname = :TRname and TRseq# = :TRseq#
             and TRtable = 'Cause' and TRkey = :aftCMCaus
            limit 1;
          If not IsValidFetch(sqlstt);
             Return *off;
          Endif;
       Endif;

       Return *on;
       //***  End   Chg  ***  12/17/20  ***********************

      /end-free
     p MeetsTrgTblNPC  e
       //===================================================================
       // GetPremium - Get accumulated premium
       //===================================================================
     p GetPremium      b
     d GetPremium      pi            13  2
     d inCo##                         3  0
     d inFnd##                        3  0
     d inFyr##                        3  0
     d inEmp##                        9  0
     d inEorA                         1    const
     d inPremLvl                     10

     d intAccAmt       s             13  2 inz
      /free

       intAccAmt = 0;
       Setll (inCo##:inFnd##:inFyr##:inEmp##) pel01;
       Reade (inCo##:inFnd##:inFyr##:inEmp##) pel01;
       Dow not %eof(WDELL1);
          Select;
          When inEorA = 'E' and inPremLvl = 'Normal';
             intAccAmt = intAccAmt + dELeNmP;
          When inEorA = 'E' and inPremLvl = 'Annual';
             intAccAmt = intAccAmt + dELaNPr;
          When inEorA = 'A' and inPremLvl = 'Normal';
             intAccAmt = intAccAmt + dELaNmP;
          When inEorA = 'A' and inPremLvl = 'Annual';
             intAccAmt = intAccAmt + dELaAPr;
          Endsl;
          Reade (inCo##:inFnd##:inFyr##:inEmp##) pel01;
       Enddo;

       Return intAccAmt;

      /end-free
     p GetPremium      e
       //===================================================================
       // AudPrem$ - Audited Premium $ Value Change
       //===================================================================
     p AudPrem$        b                   export
     d AudPrem$        pi              n

      /free

       Return AudPremium('$');

      /end-free
     p AudPrem$        e
       //===================================================================
       // AudPrem - Audited Premium % Change
       //===================================================================
     p AudPrem         b                   export
     d AudPrem         pi              n

      /free

       Return AudPremium('%');

      /end-free
     p AudPrem         e
       //===================================================================
       // AudPremium - Audited Premium
       //===================================================================
     p AudPremium      b
     d AudPremium      pi              n
     d inMode                         1    const

     d intAcmEst       s             13  2 inz
     d intAcmAud       s             13  2 inz
      /free

       Chain (aftEHco#:aftEHfnd:aftEHemp#:aftEHdiv:aftEHfyr) pel00;
       If %found(WDELL) and MeetsTrgPolicy = *on;
          If aftEHfAF <> 'A';
             Return *off;
          Endif;

          // accumulated estimated premium
          intAcmEst = GetPremium(aftEHco#:aftEHfnd:aftEHfyr:aftEHemp#:'E':
           pTRpremLvl);
          // accumulated audited premium
          intAcmAud = GetPremium(aftEHco#:aftEHfnd:aftEHfyr:aftEHemp#:'A':
           pTRpremLvl);

          //*** Beg Chg ***** 04/14/23 *******************************************
          //***If (inMode = '$' and ((intAcmAud - intAcmEst) >= pTRchgVal)) or
           //***(inMode = '%' and intAcmEst <> 0 and ((((intAcmAud - intAcmEst)/
           //***intAcmEst)*100) >= pTRchg));
          If (inMode = '$' and (((intAcmAud - intAcmEst) >= pTRchgVal) or
           (((intAcmAud - intAcmEst)*-1) >= pTRchgVal))) or
           (inMode = '%' and intAcmEst <> 0 and
           (((((intAcmAud - intAcmEst)/intAcmEst)*100) >= pTRchg) or
           (((((intAcmAud - intAcmEst)/intAcmEst)*100)*-1) >= pTRchg)));
           //*** End Chg ***** 04/14/23 *******************************************
             SetStRptDv();

             Return *on;
          Endif;
       Endif;

       Return *off;

      /end-free
     p AudPremium      e
       //===================================================================
       // CalcPrem - Mid-term Calculated Premium % Change
       //===================================================================
     p CalcPrem        b                   export
     d CalcPrem        pi              n

     d intAcmEst       s             13  2 inz
     d intAcmChg       s             13  2 inz
      /free

       Chain (aftEHco#:aftEHfnd:aftEHemp#:aftEHdiv:aftEHfyr) pel00;
       If %found(WDELL) and MeetsTrgPolicy = *on;
          If aftEHfAF <> 'R';
             Return *off;
          Endif;

          intAcmEst = 0;
          intAcmChg = 0;

          Setll (aftEHco#:aftEHfnd:aftEHfyr:aftEHemp#) pel01;
          Reade (aftEHco#:aftEHfnd:aftEHfyr:aftEHemp#) pel01;
          Dow not %eof(WDELL1);
             Setll (aftEHco#:aftEHfnd:aftEHemp#:dELdiv:aftEHfyr) peh14;
             Reade (aftEHco#:aftEHfnd:aftEHemp#:dELdiv:aftEHfyr) peh14;
             Dow not %eof(WDEHL14);
                If EHefdt > aftEHefdt or EHdt > aftEHdt or
                 (EHdt = aftEHdt and EHtime > aftEHtime);
                   Leave;
                Endif;
                Select;
                When pTRpremLvl = 'Normal';
                   intAcmEst = intAcmEst + EHwNmP;
                   intAcmChg = intAcmChg + EHxNmP;
                When pTRpremLvl = 'Annual';
                   intAcmEst = intAcmEst + EHwApr;
                   intAcmChg = intAcmChg + EHxApr;
                Endsl;
                Reade (aftEHco#:aftEHfnd:aftEHemp#:dELdiv:aftEHfyr) peh14;
             Enddo;
             Reade (aftEHco#:aftEHfnd:aftEHfyr:aftEHemp#) pel01;
          Enddo;

          If intAcmEst <> 0 and (((intAcmChg/intAcmEst)*100) >= pTRchg);
             Chain (aftEHco#:aftEHfnd:aftEHemp#:aftEHdiv:aftEHfyr) pel00;
             SetStRptDv();

             Return *on;
          Endif;
       Endif;

       Return *off;

      /end-free
     p CalcPrem        e
       //===================================================================
       // PolConc - Policy Concentration Information
       //===================================================================
     p PolConc         b                   export
     d PolConc         pi              n

      /free

       Chain (aftEHco#:aftEHfnd:aftEHemp#:aftEHdiv:aftEHfyr) pel00;
       If %found(WDELL) and MeetsTrgPolicy = *on;
          Select;
          When aftEHfAF <> 'E';
             Return *off;

          When (pTRpremLvl = 'Normal' and aftEHwNmP >= pTRpremAmt) or
           (pTRpremLvl = 'Annual' and aftEHwAPr >= pTRpremAmt);
             SetStRptDv();

             Return *on;
          Endsl;
       Endif;

       Return *off;

      /end-free
     p PolConc         e
       //===================================================================
       // PolPend - Policy Issuance Pending
       //===================================================================
     p PolPend         b                   export
     d PolPend         pi              n

      /free

       If MeetsTrgPolicy = *on;
          ProcQteSts(' ');
       Endif;
       Return *off;

      /end-free
     p PolPend         e
       //===================================================================
       // NQteSts - New Business Quote Status
       //===================================================================
     p NQteSts         b                   export
     d NQteSts         pi              n

      /free

       If MeetsTrgPolicy = *on and not MeetsTrgTbl('Quote Status':
        GetQuoteSts(ELco#:ELfnd:ELemp#:ELdiv:ELfyr:'Q'));
          ProcQteSts('N');
       Endif;
       Return *off;

      /end-free
     p NQteSts         e
       //===================================================================
       // RQteSts - Renewal Business Quote Status
       //===================================================================
     p RQteSts         b                   export
     d RQteSts         pi              n

      /free

       If MeetsTrgPolicy = *on and not MeetsTrgTbl('Quote Status':
        GetQuoteSts(ELco#:ELfnd:ELemp#:ELdiv:ELfyr:'Q'));
          ProcQteSts('R');
       Endif;
       Return *off;

      /end-free
     p RQteSts         e
       //===================================================================
       // ProcQteSts - Process for Quote Status
       //===================================================================
     p ProcQteSts      b
     d ProcQteSts      pi              n
     d inType                         1    const

      /free

       If (inType = ' ' and not PolPagProcessed(ELco#:ELfnd:ELemp#:ELfyr)) or
        (inType <> ' ' and inType = ELnwRw);
          If dtDiff(DateTo8(systemDate()):dtMDYY(ELpfDt)) <= pTR#daysPr;
             SetStRptDv();
             CheckActions(ELco#);
          Endif;
       Endif;

       Return *off;

      /end-free
     p ProcQteSts      e
       //===================================================================
       // QteEntry - Quote Entry
       //===================================================================
     p QteEntry        b                   export
     d QteEntry        pi              n

      /free

       Chain (aftELco#:aftELfnd:aftELemp#:aftELdiv:aftELfyr) pel00;
       If %found(WDELL) and MeetsTrgPolicy = *on;
          SetStRptDv();
          CheckActions(ELco#);
       Endif;

       Return *off;

      /end-free
     p QteEntry        e
       //===================================================================
       // QteRvw - Quote Review
       //===================================================================
     p QteRvw          b                   export
     d QteRvw          pi              n

      /free

       Chain (aftELco#:aftELfnd:aftELemp#:aftELdiv:aftELfyr) pel00;
       If %found(WDELL) and MeetsTrgPolicy = *on;
          SetStRptDv();
          CheckActions(ELco#);
       Endif;

       Return *off;

      /end-free
     p QteRvw          e
       //===================================================================
       // PolPanel - Panel Request to Provider Relations for New Business
       //===================================================================
     p PolPanel        b                   export
     d PolPanel        pi              n

      /free

       Chain (aftSHco#:aftSHfnd:aftSHemp#:aftSHdiv:aftSHfyr) pel00;
       If %found(WDELL) and MeetsTrgPolicy = *on and (aftSHcode = 'O' or
        aftSHcode = 'R');
          // get count of records in file WXNHL
          Setll (aftSHco#:aftSHfnd:aftSHemp#:aftSHfyr) pnh00;
          Reade (aftSHco#:aftSHfnd:aftSHemp#:aftSHfyr) pnh00;
          Dow not %eof(WXNHL);
             If NHreis = 'Y';
                Return *off;
             Endif;
             Reade (aftSHco#:aftSHfnd:aftSHemp#:aftSHfyr) pnh00;
          Enddo;

          SetStRptDv();
          Return *on;
       Endif;

       Return *off;

      /end-free
     p PolPanel        e
       //===================================================================
       // NewClaim - New Claim Entry
       //===================================================================
     p NewClaim        b                   export
     d NewClaim        pi              n

      /free

       If MeetsTrgClm = *on and bfrCMclex <> aftCMclex and bfrCMclex = ' ';
          Return *on;
       Endif;

       Return *off;

      /end-free
     p NewClaim        e
       //===================================================================
       // ClmChg01 - Claimant Name or Address Change
       //===================================================================
     p ClmChg01        b                   export
     d ClmChg01        pi              n

      /free

       If MeetsTrgClm = *on;
          If (bfrCDfsN <> aftCDfsN or bfrCDlsN <> aftCDlsN
           or bfrCDmid <> aftCDmid) and bfrCDlsN <> ' ';
             Return *on;
          Endif;
          If (bfrCDadr1 <> aftCDadr1 or bfrCDadr2 <> aftCDadr2
           or bfrCDcty <> aftCDcty or bfrCDst <> aftCDst
           or bfrCDzip <> aftCDzip) and bfrCDadr1 <> ' ';
             Return *on;
          Endif;
       Endif;

       Return *off;

      /end-free
     p ClmChg01        e
       //===================================================================
       // ClmChg02 - Indemnity/Medical/Record Only Status Change
       //===================================================================
     p ClmChg02        b                   export
     d ClmChg02        pi              n

      /free

       If MeetsTrgClm = *on and bfrCMlTFl <> aftCMlTFl and bfrCMlTFl <> ' ';
          Return *on;
       Endif;

       Return *off;

      /end-free
     p ClmChg02        e
       //===================================================================
       // ClmChg03 - Claim Indicators
       //===================================================================
     p ClmChg03        b                   export
     d ClmChg03        pi              n

      /free

       If MeetsTrgClm = *on;
          // Denied
          If aftCMdny <> bfrCMdny  and MeetsTrgTbl('Claim Indicator':'01')
             and aftCMdny <> ' ' and aftCMdny <> 'N';
             Return *on;
          Endif;
          // Controverted
          If aftCMcvFl <> bfrCMcvFl and MeetsTrgTbl('Claim Indicator':'02')
             and aftCMcvFl <> ' ' and aftCMcvFl <> 'N';
             Return *on;
          Endif;
          // Subrogation
          If aftCMsubr <> bfrCMsubr and MeetsTrgTbl('Claim Indicator':'03')
             and aftCMsubr <> ' ' and aftCMsubr <> 'N';
             Return *on;
          Endif;
          // Excess
          If aftCMexcs <> bfrCMexcs and MeetsTrgTbl('Claim Indicator':'04')
             and aftCMexcs <> ' ' and aftCMexcs <> 'N';
             Return *on;
          Endif;
          // Acquired
          If aftCMacqr <> bfrCMacqr and MeetsTrgTbl('Claim Indicator':'05')
             and aftCMacqr <> ' ' and aftCMacqr <> 'N';
             Return *on;
          Endif;
          // OSHA Reportable
          If aftCMosha <> bfrCMosha and MeetsTrgTbl('Claim Indicator':'06')
             and aftCMosha <> ' ' and aftCMosha <> 'N';
             Return *on;
          Endif;
          // 2nd Disability Fund
          If aftCMsDf <> bfrCMsDf and MeetsTrgTbl('Claim Indicator':'07')
             and aftCMsDf <> ' ' and aftCMsDf <> 'N';
             Return *on;
          Endif;
          // USL&H Exposure
          If aftCMuslh <> bfrCMuslh and MeetsTrgTbl('Claim Indicator':'08')
             and aftCMuslh <> ' ' and aftCMuslh <> 'N';
             Return *on;
          Endif;
          // Death
          If aftCMdth <> bfrCMdth and MeetsTrgTbl('Claim Indicator':'09')
             and aftCMdth <> ' ' and aftCMdth <> 'N';
             Return *on;
          Endif;
          // CMS
          If aftCMmBS <> bfrCMmBS and MeetsTrgTbl('Claim Indicator':'10')
             and aftCMmBS <> ' '  and aftCMmBS <> 'N';
             Return *on;
          Endif;
       Endif;

       Return *off;

      /end-free
     p ClmChg03        e
       //===================================================================
       // ClmChg04 - Claim Status
       //===================================================================
     p ClmChg04        b                   export
     d ClmChg04        pi              n

      /free

       If MeetsTrgClm = *on and bfrCMstat <> aftCMstat and bfrCMstat <> ' ';
          Return *on;
       Endif;

       Return *off;

      /end-free
     p ClmChg04        e
       //===================================================================
       // LowRsv - Low Reserves
       //===================================================================
     p LowRsv          b                   export
     d LowRsv          pi              n

     d intAcmRsvAft    s             13  2 inz
     d intAcmRsvBfr    s             13  2 inz
      /free

       If MeetsTrgClm = *on;
          If MeetsTrgTbl('Reserve':aftCMrt01);
             intAcmRsvAft = intAcmRsvAft + aftCMrr01;
             intAcmRsvBfr = intAcmRsvBfr + bfrCMrr01;
          Endif;
          If MeetsTrgTbl('Reserve':aftCMrt02);
             intAcmRsvAft = intAcmRsvAft + aftCMrr02;
             intAcmRsvBfr = intAcmRsvBfr + bfrCMrr02;
          Endif;
          If MeetsTrgTbl('Reserve':aftCMrt03);
             intAcmRsvAft = intAcmRsvAft + aftCMrr03;
             intAcmRsvBfr = intAcmRsvBfr + bfrCMrr03;
          Endif;
          If MeetsTrgTbl('Reserve':aftCMrt04);
             intAcmRsvAft = intAcmRsvAft + aftCMrr04;
             intAcmRsvBfr = intAcmRsvBfr + bfrCMrr04;
          Endif;
          If MeetsTrgTbl('Reserve':aftCMrt04);
             intAcmRsvAft = intAcmRsvAft + aftCMrr05;
             intAcmRsvBfr = intAcmRsvBfr + bfrCMrr05;
          Endif;
          If MeetsTrgTbl('Reserve':aftCMrt06);
             intAcmRsvAft = intAcmRsvAft + aftCMrr06;
             intAcmRsvBfr = intAcmRsvBfr + bfrCMrr06;
          Endif;
          If MeetsTrgTbl('Reserve':aftCMrt07);
             intAcmRsvAft = intAcmRsvAft + aftCMrr07;
             intAcmRsvBfr = intAcmRsvBfr + bfrCMrr07;
          Endif;
          If MeetsTrgTbl('Reserve':aftCMrt08);
             intAcmRsvAft = intAcmRsvAft + aftCMrr08;
             intAcmRsvBfr = intAcmRsvBfr + bfrCMrr08;
          Endif;
          If MeetsTrgTbl('Reserve':aftCMrt09);
             intAcmRsvAft = intAcmRsvAft + aftCMrr09;
             intAcmRsvBfr = intAcmRsvBfr + bfrCMrr09;
          Endif;
          If MeetsTrgTbl('Reserve':aftCMrt10);
             intAcmRsvAft = intAcmRsvAft + aftCMrr10;
             intAcmRsvBfr = intAcmRsvBfr + bfrCMrr10;
          Endif;
          If intAcmRsvAft <> intAcmRsvBfr and intAcmRsvAft < cTRamt;
             Return *on;
          endif;
       Endif;

       Return *off;

      /end-free
     p LowRsv          e
       //===================================================================
       // LargeRsv - Large Reserve Change
       //===================================================================
     p LargeRsv        b                   export
     d LargeRsv        pi              n

      /free

       If MeetsTrgClm = *on;
          If MeetsTrgTbl('Reserve':aftCMrt01) and
             aftCMrr01 <> bfrCMrr01 and (aftCMrr01 - bfrCMrr01) >= cTRamt;
             Return *ON;
          Endif;
          If MeetsTrgTbl('Reserve':aftCMrt02) and
             aftCMrr02 <> bfrCMrr02 and (aftCMrr02 - bfrCMrr02) >= cTRamt;
             Return *ON;
          Endif;
          If MeetsTrgTbl('Reserve':aftCMrt03) and
             aftCMrr03 <> bfrCMrr03 and (aftCMrr03 - bfrCMrr03) >= cTRamt;
             Return *ON;
          Endif;
          If MeetsTrgTbl('Reserve':aftCMrt04) and
             aftCMrr04 <> bfrCMrr04 and (aftCMrr04 - bfrCMrr04) >= cTRamt;
             Return *ON;
          Endif;
          If MeetsTrgTbl('Reserve':aftCMrt05) and
             aftCMrr05 <> bfrCMrr05 and (aftCMrr05 - bfrCMrr05) >= cTRamt;
             Return *ON;
          Endif;
          If MeetsTrgTbl('Reserve':aftCMrt06) and
             aftCMrr06 <> bfrCMrr06 and (aftCMrr06 - bfrCMrr06) >= cTRamt;
             Return *ON;
          Endif;
          If MeetsTrgTbl('Reserve':aftCMrt07) and
             aftCMrr07 <> bfrCMrr07 and (aftCMrr07 - bfrCMrr07) >= cTRamt;
             Return *ON;
          Endif;
          If MeetsTrgTbl('Reserve':aftCMrt08) and
             aftCMrr08 <> bfrCMrr08 and (aftCMrr08 - bfrCMrr08) >= cTRamt;
             Return *ON;
          Endif;
          If MeetsTrgTbl('Reserve':aftCMrt09) and
             aftCMrr09 <> bfrCMrr09 and (aftCMrr09 - bfrCMrr09) >= cTRamt;
             Return *ON;
          Endif;
          If MeetsTrgTbl('Reserve':aftCMrt10) and
             aftCMrr10 <> bfrCMrr10 and (aftCMrr10 - bfrCMrr10) >= cTRamt;
             Return *ON;
          Endif;
       Endif;

       Return *off;

      /end-free
     p LargeRsv        e
       //===================================================================
       // LargePmt - Large Payment
       //===================================================================
     p LargePmt        b                   export
     d LargePmt        pi              n

      /free

       If MeetsTrgClm = *on;
          If MeetsTrgTbl('Payment':aftPAbr2) and
             MeetsTrgTbl('Reserve':aftPAreCd) and aftPAamt >= cTRamt;
             Return *ON;
          Endif;
       Endif;

       Return *off;

      /end-free
     p LargePmt        e
       //===================================================================
       // Payment - Payment
       //===================================================================
     p Payment         b                   export
     d Payment         pi              n

     d #OfPayments     s             13  2 inz
      /free

       If MeetsTrgClm = *on;
       // Check if this payment Meets-Trigger-Table setup.
          If MeetsTrgTbl('Payment':aftPAbr2) and
             MeetsTrgTbl('Reserve':aftPAreCD) and aftPAamt >= cTRamt;

             // Read all payments for this claim from WDPAL
             Setll (aftPAco#:aftPAfnd:aftPAfyr:aftPAcase) ppa00;
             Reade (aftPAco#:aftPAfnd:aftPAfyr:aftPAcase) ppa00;
             Dow not %eof(WDPAL);
                If MeetsTrgTbl('Payment':PAbr2) and
                   MeetsTrgTbl('Reserve':PAreCD) and PAamt >= cTRamt;
                   #OfPayments +=1;
                Endif;
             Reade (aftPAco#:aftPAfnd:aftPAfyr:aftPAcase) ppa00;
             Enddo;

          Endif;
          If #OfPayments >= cTRPayCount and #OfPayments > 0;
             Return *on;
          Endif;
       Endif;

       Return *off;

      /end-free
     p Payment         e
       //===================================================================
       // ClmNCCI - NCCI Injury Code
       //===================================================================
     p ClmNCCI         b                   export
     d ClmNCCI         pi              n

      /free

       If MeetsTrgClm = *on and aftCMncci <> bfrCMncci and
          MeetsTrgTbl('NCCI Injury':aftCMncci);
          Return *on;
       Endif;

       Return *off;

      /end-free
     p ClmNCCI         e
       //===================================================================
       // MultiEmp - Multiple Employee Incident
       //===================================================================
     p MultiEmp        b                   export
     d MultiEmp        pi              n

      /free

       If MeetsTrgClm = *on;
          Return *on;
       Endif;

       Return *off;

      /end-free
     p MultiEmp        e
       //===================================================================
       // ClmNPC - Nature / Part / Cause Code Trigger
       //===================================================================
     p ClmNPC          b                   export
     d ClmNPC          pi              n

      /free

       If MeetsTrgClm = *on;

          Select;
          When
           (aftCMnatr <> bfrCMnatr or aftCMpart <> bfrCMpart
            or aftCMcaus <> bfrCMcaus) and MeetsTrgTblNPC();
             Return *on;
          EndSL;

       Endif;

       Return *off;

      /end-free
     p ClmNPC          e
       //===================================================================
       // TotalInc - Total Incurred
       //===================================================================
     p TotalInc        b                   export
     d TotalInc        pi              n

     d TotIncAft       s             13  2 inz
     d TotIncBfr       s             13  2 inz
      /free

       If MeetsTrgClm = *on;
          TotIncAft=(aftCMrr01 + aftCMrr02 + aftCMrr03 + aftCMrr04 + aftCMrr05 +
                     aftCMrr06 + aftCMrr07 + aftCMrr08 + aftCMrr09 + aftCMrr10)+
                    (aftCMar01 + aftCMar02 + aftCMar03 + aftCMar04 + aftCMar05 +
                     aftCMar06 + aftCMar07 + aftCMar08 + aftCMar09 + aftCMar10);
          TotIncBfr=(bfrCMrr01 + bfrCMrr02 + bfrCMrr03 + bfrCMrr04 + bfrCMrr05 +
                     bfrCMrr06 + bfrCMrr07 + bfrCMrr08 + bfrCMrr09 + bfrCMrr10)+
                    (bfrCMar01 + bfrCMar02 + bfrCMar03 + bfrCMar04 + bfrCMar05 +
                     bfrCMar06 + bfrCMar07 + bfrCMar08 + bfrCMar09 + bfrCMar10);
          If (TotIncAft <> TotIncBfr) and TotIncAft >= cTRtotInc;
             Return *on;
          Endif;

       Endif;

       Return *off;

      /end-free
     p TotalInc        e
       //===================================================================
       // SetStRptDv - Set State Reporting division
       //===================================================================
     p SetStRptDv      b
     d SetStRptDv      pi

      /free

       if not isClaim;
          ELdiv = GetStRptDv(ELco#:ELfnd:ELemp#);
       endif;

       If isOpen <> *on;
          Open WMEML;
          isOpen = *on;
       Endif;
       Setll (ELco#:ELfnd:ELemp#:ELdiv) pem00;
       Reade (ELco#:ELfnd:ELemp#:ELdiv) pem00 empDs1;

      /end-free
     p SetStRptDv      e
       //===================================================================
       // ClsOvrRd - Close overridden file
       //===================================================================
     p ClsOvrRd        b
     d ClsOvrRd        pi
     d inFile                        10    const
     d inQry                        255    options(*nopass)

      /free

       If %parms >= 2 and inQry <> ' ';
          strCommand = 'CLOF OPNID(' + %trim(inFile) + ')';
          Callp QCMDEXC(strCommand:%len(strCommand));
       Endif;

       //strCommand = 'DLTOVR FILE(' + %trim(inFile) + ')';
       //Callp QCMDEXC(strCommand:%len(strCommand));

      /end-free
     p ClsOvrRd        e

     P* --------------------------------------------------
     P* Build QADSPFFD
     P* --------------------------------------------------
     PBuildFFD         b
     DBuildFFD         pi
     D inLib                         10
     D inFile                        10

     dstrCmd           s            255
      /free
       strCmd = 'DSPFFD FILE(' + %trim(inLib) + '/' + %trim(inFile) +
        ') OUTPUT(*OUTFILE) OUTFILE(QTEMP/QADSPFFD) OUTMBR(*FIRST *REPLACE)';
        RunCmd(strCmd:%len(strCmd));
       Return;
      /end-free
     PBuildFFD         e

     P* --------------------------------------------------
     P* Check for the existence of QADSPFFD
     P* --------------------------------------------------
     PCheckFFDExists   b
     DCheckFFDExists   pi              n

     dstrError         s              1
      /free
       ChkObjwLib('QTEMP':'QADSPFFD':'*FILE':strError);
       Return strError = '0';
      /end-free
     PCheckFFDExists   e

     P* --------------------------------------------------
     P* Create file triggers
     P* --------------------------------------------------
     PCreateFileTrgDeleted...
     P                 b
     DCreateFileTrgDeleted...
     D                 pi             1n
     D inTrgName                    128

     D isDevMachine    s               n
     D isError         s               n
     D strField        s             20
     D strFieldChg     s           5000    inz
     D strFieldChgN    s           5000    inz
     D strFieldChgO    s           5000    inz
     D strFieldRpt     s           5000    inz
     D strFieldRptO    s           5000    inz
     D strFieldRptN    s           5000    inz
     D strLibFile      s             10    inz
     D strLibObj       s             10    inz
     D strLibEnv       s             10    inz
     D strPDSOBJ       s             10
     D strPgm          s             10
     D strSqlCursor    s           5000
     D strSql          s           5000
     D strTrgEvent     s                   like(TRtrgEvent)
     D strTrgTime      s                   like(TRtrgTime)
     D strTrigger      s            150
     D strSuffix       s              3
     D decPos          s              5  0
     D decPosChg       s              5  0 inz(1)
     D decPosRpt       s              5  0 inz(1)

     D dsFFD           ds                  inz
     D  fldB                          5  0
     D  fldD                          2  0
     D  fldP                          2  0
     D  fldT                          1
     D dsFil           ds                  likeRec(ptr0fil:*input)
     D dsFld           ds                  likeRec(ptr0fld:*input)
     D dsFrq           ds                  likeRec(ptr0frq:*input)

     DChkDevMachine    pr                  ExtPgm('SACHKPDS')
     D dev                            1n
     dChkObj           pr                  extpgm('SRCHKOBJ')
     d outObj                        10    const
     d outType                        7    const
     d inError                        1
     DSetDevEnv        pr                  ExtPgm('LODWC2405')

      /free
       //***  Begin Del  ***  12/17/20  ***************
       //Exec SQL set option COMMIT=*NONE;
       //***  End   Del  ***  12/17/20  ***************
       RtvSuffix(strSuffix);
       strPDSOBJ = 'PDSOBJ' + strSuffix;
       strLibObj = strPDSOBJ;
       GetObjLib('SMTRP':'*FILE':strLibFile);
       ChkDevMachine(isDevMachine);
       If (isDevMachine);
          strPDSOBJ = %subst(strLibFile:1:6);
          ChkObj(strPDSOBJ:'*LIB':isError);
          If isError;
             strPDSOBJ = %trim(strPDSOBJ) + 'OBJ';
          Endif;
          GetObjLib('M0002C':'*PGM':strLibObj);
          SetDevEnv();
       Endif;
       GetObjLib('ATNAVP':'*FILE':strLibEnv);

       Chain inTrgName ptr0frq dsFrq;
       If not %found;
          Return *off;
       Endif;
       // Get associated record in SMTRPFIL
       Chain (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) ptr0fil dsFil;
       If not CheckFFDExists;
          BuildFFD(dsFil.TRlibrary:dsFil.TRfile);
       Endif;
       strTrigger = %trim(strPDSOBJ) + '.' + %trim(inTrgName);
       Monitor;
       // strSql = 'DROP TRIGGER ' + %trim(strTrigger);
       // Exec SQL execute immediate :strSQL;
       On-Error;
       Endmon;

       // Read through SMTRPFLD
       Setll (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) ptr0fld;
       Dou %eof(smtrpfld);
          Reade (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) ptr0fld dsFld;
          Select;
          When %eof;
             Iter;
          Endsl;
          Clear dsFld.TRposition;
          Clear dsFld.TRfldB;
          Clear dsFld.TRfldD;
          Clear dsFld.TRfldP;
          strSqlCursor = 'select WHFLDB, WHFLDD, WHFLDP, WHFLDT' +
           ' from QTEMP/QADSPFFD' +
           ' where WHLIB  = ''' + %trim(dsFil.TRlibrary) + '''' +
           '   and WHFILE = ''' + %trim(dsFil.TRfile) + '''' +
           '   and (WHFLDI = ''' + %trim(dsFld.TRfield) + '''' +
           '     or WHFLDE = ''' + %trim(dsFld.TRfield) + ''')';
          Exec SQL Prepare sql1 From :strSqlCursor;
          Exec SQL Declare cursor1 Cursor For sql1;
          Exec SQL Open cursor1;
          Exec SQL Fetch next From cursor1 into :dsFFD;
          If SQLSTT = '00000';
             Select;
             When fldD > fldB;
                decPos = fldD;
             Other;
                decPos = fldB;
             Endsl;
             Select;
             When dsFld.TRfldT = 'P' or dsFld.TRfldT = 'S' or
              dsFld.TRfldT = 'L' or dsFld.TRfldT = 'T' or dsFld.TRfldT = 'Z';
                strField = 'DIGITS(*.' + %trim(dsFld.TRfield) + ')';
             Other;
                strField = '*.' + dsFld.TRfield;
             Endsl;
             Select;
             When dsFld.TRtype = 'Change';
                If strFieldChg <> *blanks;
                   strFieldChg = %trim(strFieldChg) + ' ||';
                Endif;
                strFieldChg = %trim(strFieldChg) + ' ' + strField;
                dsFld.TRposition = decPosChg;
                decPosChg += decPos;
             When dsFld.TRtype = 'Report';
                If strFieldRpt <> *blanks;
                   strFieldRpt = %trim(strFieldRpt) + ' ||';
                Endif;
                strFieldRpt = %trim(strFieldRpt) + ' ' + strField;
                dsFld.TRposition = decPosRpt;
                decPosRpt += decPos;
             Endsl;
             dsFld.TRfldB = fldB;
             dsFld.TRfldD = fldD;
             dsFld.TRfldP = fldP;
             dsFld.TRfldT = fldT;
          Endif;
          Exec SQL Close cursor1;
          // Update SMTRPFLD
          dsFld.TRuser = q1User;
          dsFld.TRpgm  = q1Pgm;
          dsFld.TRtime = TimeTo6(SystemTime);
          dsFld.TRdt   = DateTo7(SystemDate);
          Update ptr0fld dsFld;
       Enddo;

       Select;
       When dsFrq.TRtrgEvent = '*INSERT';
          strPgm = 'QDBPUT';
          strFieldChgN = ' || ' + %trim(ScanReplace('*.':'N.':strFieldChg));
          strFieldChgO = *blanks;
          strFieldRptN = ' || ' + %trim(ScanReplace('*.':'N.':strFieldRpt));
          strFieldRptO = *blanks;
       When dsFrq.TRtrgEvent = '*DELETE';
          strPgm = 'QDBUDR';
          strFieldChgN = *blanks;
          strFieldChgO = ' || ' + %trim(ScanReplace('*.':'O.':strFieldChg));
          strFieldRptN = *blanks;
          strFieldRptO = ' || ' + %trim(ScanReplace('*.':'O.':strFieldRpt));
       When dsFrq.TRtrgEvent = '*UPDATE';
          strPgm = 'QDBUDR';
          strFieldChgN = ' || ' + %trim(ScanReplace('*.':'N.':strFieldChg));
          strFieldChgO = ' || ' + %trim(ScanReplace('*.':'O.':strFieldChg));
          strFieldRptN = ' || ' + %trim(ScanReplace('*.':'N.':strFieldRpt));
          strFieldRptO = ' || ' + %trim(ScanReplace('*.':'O.':strFieldRpt));
       Endsl;

       strTrgTime = %trim(ScanReplace('*':' ':dsFrq.TRtrgTime));
       strTrgEvent = %trim(ScanReplace('*':' ':dsFrq.TRtrgEvent));
       strSql = 'CREATE or REPLACE TRIGGER ' + %trim(strTrigger) + ' ' +
        %trim(strTrgTime) + ' ' + %trim(strTrgEvent) +
        ' on ' + %trim(dsFil.TRlibrary) + '.' + %trim(dsFil.TRfile) +
        ' REFERENCING NEW ROW AS N OLD ROW AS O' +
        ' FOR EACH ROW MODE DB2SQL' +
        ' SET OPTION COMMIT=*NONE' +
        ' BEGIN' +
        '  declare UpdatePgm  char(10)  default ''' + strPgm + ''';'        +
        '  declare CalledPgm  char(10) default '''';' +
        '  declare outTrgName char(128) default ''' + inTrgName + ''';' +
        '  declare outLibFile char(10) default ''' + strLibFile + ''';' +
        '  declare outLibObj  char(10) default ''' + strLibObj + ''';' +
        '  declare outLibEnv  char(10) default ''' + strLibEnv + ''';' +
        '  declare outPgm    char(10);' +
        '  declare outUser   char(10);' +
        '  declare outChgO char(32765);' +
        '  declare outChgN char(32765);' +
        '  declare outRptO char(32765);' +
        '  declare outRptN char(32765);' +
        '  call ' + %trim(strPDSOBJ) + '.GetPrvCall(CalledPgm,UpdatePgm);' +
        '  set outPgm = CalledPgm;' +
        '  set outUser = User;' +
        '  set outChgO = (''O''' + %trimr(strFieldChgO) + ');' +
        '  set outChgN = (''N''' + %trimr(strFieldChgN) + ');' +
        '  set outRptO = (''O''' + %trimr(strFieldRptO) + ');' +
        '  set outRptN = (''N''' + %trimr(strFieldRptN) + ');' +
        '  call ' + %trim(strPDSOBJ) + '.PROCFILECHG' +
        '  (outTrgName, outUser, outPgm, outLibFile, outLibObj, outLibEnv,' +
        '   outChgO, outChgN, outRptO, outRptN);' +
        ' END';
       Exec SQL execute immediate :strSQL;
       Select;
       When sqlStt <> *zero;
          Return *on;
       Other;
          Chain (dsFrq.TRco#:dsFrq.TRname:dsFrq.TRseq#) ptr00;
          If %found;
             Select;
             When TRstat = 'P' and TriggerExists(TRco#:TRname:TRseq#);
                TRstat = 'A';
                TRpgm  = q1Pgm;
                TRuser = q1User;
                TRtime = Timeto6(SystemTime());
                TRdt   = Dateto7(SystemDate());
             Endsl;
             Update ptr00;
          Endif;
          Return *off;
       Endsl;
      /end-free
     PCreateFileTrgDeleted...
     P                 e

     P* --------------------------------------------------
     P* Drop file triggers
     P* --------------------------------------------------
     PDropFileTrgDeleted...
     P                 b
     DDropFileTrgDeleted...
     D                 pi             1n
     D inTrgName                    128

     D isDevMachine    s               n
     D isError         s               n
     D strLibFile      s             10    inz
     D strLibObj       s             10    inz
     D strPDSOBJ       s             10
     D strSql          s           5000
     D strTrigger      s            150
     D strSuffix       s              3
     D dsFrq           ds                  likeRec(ptr0frq:*input)

     DChkDevMachine    pr                  ExtPgm('SACHKPDS')
     D dev                            1n
     dChkObj           pr                  extpgm('SRCHKOBJ')
     d outObj                        10    const
     d outType                        7    const
     d inError                        1
     DSetDevEnv        pr                  ExtPgm('LODWC2405')

      /free
       RtvSuffix(strSuffix);
       strPDSOBJ = 'PDSOBJ' + strSuffix;
       strLibObj = strPDSOBJ;
       ChkDevMachine(isDevMachine);
       If (isDevMachine);
          strPDSOBJ = %subst(strLibFile:1:6);
          ChkObj(strPDSOBJ:'*LIB':isError);
          If isError;
             strPDSOBJ = %trim(strPDSOBJ) + 'OBJ';
          Endif;
          GetObjLib('M0002C':'*PGM':strLibObj);
          SetDevEnv();
       Endif;

       Chain inTrgName ptr0frq dsFrq;
       If not %found;
          Return *off;
       Endif;
       strTrigger = %trim(strPDSOBJ) + '.' + %trim(inTrgName);
       Monitor;
          strSql = 'DROP TRIGGER ' + %trim(strTrigger);
          Exec SQL execute immediate :strSQL;
       On-Error;
          Return *on;
       Endmon;

       Select;
       When sqlStt <> *zero;
          Return *on;
       Other;
          Return *off;
       Endsl;
      /end-free
     PDropFileTrgDeleted...
     P                 e

     P* --------------------------------------------------
     P* Get trigger name
     P* --------------------------------------------------
     PGetTriggerName   b                   export
     DGetTriggerName   pi           128
     D inLib                         10
     D inFile                        10
     D inEvent                        8

     ddecIndex         s              5  0 inz(0)
     dstrEvent         s             10
     dstrIndex         s              5
     dstrName          s            128
     dstrNamePrefix    s            128

      /free
       strEvent = %trim(ScanReplace('*':' ':inEvent));
       strNamePrefix = %trim(inLib)+'_'+%trim(inFile)+'_'+%trim(strEvent)+'_';
       Dou not %found;
          decIndex += 1;
          strName = %trim(strNamePrefix) + %editc(decIndex:'X');
          Chain strName ptr0frq;
       Enddo;
       Return strName;

       strName = %trim(inLib)+'_'+%trim(inFile)+'_'+%trim(inEvent)+'_';
       Setgt strName ptr0frq;
       Readp ptr0frq;
       If not %eof and
        %subst(TRtrgName:1:%len(%trim(strName))) = %trim(strName);
          strIndex = ParseString(TRtrgName:'_':4);
          decIndex = %int(strIndex);
       Endif;
       decIndex += 1;
       strName = %trim(strName) + %editc(decIndex:'X');
       Return strName;
      /end-free
     PGetTriggerName   e

       //===================================================================
       // Check for trigger existence
       //===================================================================
     pTriggerExistsDeleted...
     p                 b
     dTriggerExistsDeleted...
     d                 pi             1n
     d inCo#                          3  0
     d inName                        10
     d inSeq#                         5  0

     D outExists       s              1n   inz(*on)
     D strSqlCursor    s           5000
     D strSql          s           5000

     D dsTrigger       ds                  inz
     D  name                        130
      /Free
       Chain(n) (inCo#:inName:inSeq#) smtrl;
       Select;
       When TRname <> 'FILE RULES';
          Return *on;
       Endsl;
       Setll (inCo#:inName:inSeq#) smtrpfrq;
       Dou %eof;
          Reade (inCo#:inName:inSeq#) smtrpfrq;
          Select;
          When %eof;
             Iter;
          Endsl;
          strSqlCursor = 'select TRIGNAME ' +
           'from SYSTRIGGERS ' +
           'where TRIGNAME = ''' + %trim(TRtrgName) + '''';
          Exec SQL Prepare sql2 From :strSqlCursor;
          Exec SQL Declare cursor2 Cursor For sql2;
          Exec SQL Open cursor2;
          Exec SQL Fetch next From cursor2 into :dsTrigger;
          If SQLSTT <> '00000';
             outExists = *off;
          Endif;
          Exec SQL Close cursor2;
       Enddo;
       Return outExists;
      /End-free
     pTriggerExistsDeleted...
     p                 e

     P* --------------------------------------------------
     P* Procedure name: ValidIncurredLoss
     P* --------------------------------------------------
     P ValidIncurredLoss...
     P                 B
     D ValidIncurredLoss...
     D                 PI              N

      /free

       //*** Begin Chg *** 08/16/17 **************************************
       //*If isModEnabled('Incurred Loss') and bfrCMstat <> 'O' and aftCMstat ='O'
       //*** Chain(n)(aftCMco#:aftCMfnd:aftCMfyr:aftCMEMP#) wdell1;
       //*** If %Found and dELfset = 'Y';
       //***    Return *on;
       //*** Endif;
       //*Endif;
       If isModEnabled('Incurred Loss');
          If bfrCMstat <> 'O' and aftCMstat = 'O';
             Chain(n)(aftCMco#:aftCMfnd:aftCMfyr:aftCMEMP#) wdell1;
             If %Found and dELfset = 'Y';
                Return *On;
             Endif;
          Endif;
       Else;
          Return *On;
       Endif;
       //*** End   Chg *** 08/16/17 **************************************

       Return *off;

      /end-free

     P ValidIncurredLoss...
     P                 E

      //*** End Add * 05/24/16
      //*** beg add ***** 04/10/17 ************************
      //===================================================
      // Process Experience Mod Date Trigger
      //===================================================
     p ExpModDate      b                   export
     d ExpModDate      pi              n
      /Free
       Select;
       When MeetsTrgPolicy = *Off;
          Return *Off;
       When aftEHemd1 <> 0 and
        MatchesDate(%Date(aftEHemd1:*Cymd):pTRCompDate:
        '*USA/':Upper(pTRCompOper));
          Return *On;
       When aftEHemd2 <> 0 and
        MatchesDate(%Date(aftEHemd2:*Cymd):pTRCompDate:
        '*USA/':Upper(pTRCompOper));
          Return *On;
       When aftEHemd3 <> 0 and
        MatchesDate(%Date(aftEHemd3:*Cymd):pTRCompDate:
        '*USA/':Upper(pTRCompOper));
          Return *On;
       Other;
          Return *Off;
       EndSl;
      /End-Free
     p ExpModDate      e
      //===================================================
      // Process Policy Address Change Trigger
      //===================================================
     p PolAddrChg      b                   export
     d PolAddrChg      pi              n
      /Free
       Select;
       When MeetsTrgPolicy = *Off;
          Return *Off;
       When (bfrEMmad1 + bfrEMmad2 + bfrEMmcty + bfrEMmst +
        %editc(bfrEMmzip:'X')) <>
        (aftEMmad1 + aftEMmad2 + aftEMmcty + aftEMmst +
        %editc(aftEMmzip:'X'));
          Return *On;
       When (bfrEMpad1 + bfrEMpad2 + bfrEMpcty + bfrEMpst +
        %editc(bfrEMpzip:'X')) <>
        (aftEMpad1 + aftEMpad2 + aftEMpcty + aftEMpst +
        %editc(aftEMpzip:'X'));
          Return *On;
       Other;
          Return *Off;
       EndSl;
      /End-Free
     p PolAddrChg      e
      //*** end add ***** 04/10/17 ************************
      //*** Begin Add *** 05/13/21 ****************************************
      //===================================================
      // Process Quote 3rd Party Payment
      //===================================================
     p Qte3PrtPmt      b                   export
     d Qte3PrtPmt      pi              n

     d passed          s               n

      /Free

       passed = MeetsTrgPolicy();

       // Is there a quote record?
       If passed;
       EndIf;

       Return passed;

      /End-Free

     p Qte3PrtPmt      e

      //***  Begin Add  ***  10/19/21  *************
      //===================================================
      // Process 3rd Party Payment on Cx Policy
      //===================================================
     p TPrtPmtCx       b                   export
     d TPrtPmtCx       pi              n

      /Free

       Return MeetsTrgPolicy();

      /End-Free

     p TPrtPmtCx       e
      //***  End   Add  ***  10/19/21  *************

      //===================================================
      // Process 3rd Party Payment Dispute
      //===================================================
     p TPrtPmtDis      b                   export
     d TPrtPmtDis      pi              n

      /Free

       Return MeetsTrgPolicy();

      /End-Free

     p TPrtPmtDis      e
      //*** End   Add *** 05/13/21 ****************************************
