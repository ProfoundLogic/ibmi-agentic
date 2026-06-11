     hcopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h******************************************************************
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h*****************************************************************
     h/TITLE WTCLMPMT - Claim Payment Processing
     h*****************************************************************
     H*  Created:  01/03/23  C:079/333 Project: WC2430
     h**************************************************************
     h*
     h*****************************************************************
     h* Revisions
     h*
     h* 11/17/25 C:333  Support: 120105 (WC2494 QA)
     h*                 Deductible Detail Report is not pulling the correct information.
     h*                 - Noticed it's passing blank reserve type from the payment
     h*                   screen.
     h*
     h* 10/02/25 C:079  Support: 119820 (OIC)
     h*                 Populate posting date correctly on recurring
     h*                 payments
     h*
     h* 09/10/25 C:053  Support: 118981  (Prescient)
     h*                 Edit issue
     h*
     h* 08/13/25 C:053  Support: 119456  (Prescient)
     h*                 Date changes in 118410/118579 for CLT129 should be
     h*                 qualified for Post Date/Date Received.
     h*
     h* 08/08/25 C:053  Support: 117755  (S1)
     h*                 Bypass reserve edit for refunds
     h*
     h* 06/09/25 C:349  Support: 119065 (Prescient)
     h*                 Allow entry of additional payee in addition to FEIN#
     h*                 to allow payment to be mailed to adtl payee and
     h*                 tracked to FEIN # for 1099 purposes.
     h*
     h* 05/22/25 C:344  Support: 118323 (CompSuite)
     h*                 - Changed claim field names with the 'L3_'
     h*                 prefix that specifically involve mult adjustment
     h*                 transactions.
     h*                 - Changed the payee edit to not check entries
     h*                 having a 'Multiple Adjustments Mode' ('A').
     h*
     h* 05/06/25 C:342  Support: 118543 (S1 Med)
     h*                 Correct case number and Year when same claim
     h*                 is Adjusted.
     h*
     h* 05/02/25 C:349  Support: 118852 (UBIC)
     h*                 Correct duplicate blank spaces in payee name
     h*
     h* 04/29/25 C:079  Support: 118657 (Prescient)
     h*                 DSPF changes to remove PUI.CLICK from Payment
     h*                 description field
     h*
     h* 04/21/25 C:349  Support: 118915 (Prescient)
     h*                 Correct duplicate address line 2 when payment is a DBA
     h*
     h* 04/16/25 C:079  Support: 118899 (Cove Risk)
     h*                 Do not default Payment Status Code to PY
     h*
     h* 04/14/25 C:079  Support: 118805 (Cove Risk)
     h*                 Correct logic that updates Reserve Type
     h*
     h* 04/07/25 C:079  Support: 118435 (Cove Risk)
     h*                 Correct logic that write DARTS for recurring pmts
     h*
     h* 04/03/25 C:079  Support: 118824 (Cove Risk)
     h*                 Correct logic that calls apportionment
     h*
     h* 03/31/25 C:079  Support: 118794 (Cove Risk)
     h*                 Claim Payment Enhancement Request
     h*                 1) Do not default reserve type to CO
     h*
     h* 03/26/25 C:300  Support: 118579 (Cove Risk)
     h*                 Claim Payment Enhancement Request
     h*                 1) Default Date Received with Current System Date
     h*                 2) Default Payment status code to PY
     h*                 3) Default Payment Description to 'Comp System Payment'
     h*
     h* 02/28/25 C:333  Support: 118410 (Cove Risk)
     h*                 In certain cases, PayCode/Description values are not pop
     h*                 correctly based on the selected reserve type. This happe
     h*                 even though the reserve type is being changed just befor
     h*                 calling PayDescription(). This could be due to a UI rend
     h*                 causing the value to still reflect the old reserve type
     h*                 PayDescription() runs.
     h*                 To mitigate this, we are adding a 50 millisecond delay b
     h*                 each call to PayDescription() to ensure the UI updates h
     h*                 completed.
     h*
     h* 02/28/25 C:333  Support: 118508 (Cove Risk)
     h*                 Issues Posting Recovery
     h*                 - DSPF changes:
     h*                   rcdPayment onload event Recovery() defaulting to M
     h*                   for mode 'R' or 'T'. It should be R.
     h*
     h* 01/30/25 C:300  Support: 118410 (Cove Risk)
     h*                 Payments Screen Missing Values
     h*                 1) Default Posting Date with Current System Date
     h*                 2) Allow to change the Payment Description
     h*
     h* 11/22/24 C: 079 Support: 117666
     h*                 On reserve cushion edit read from closed date
     h*                 forward not backwards (in the past)
     h*
     h* 10/29/24 C: 342 Support: 117792
     h*                 1.Disable Payee1 FEIN when InMode is C and FEIN is
     h*                 not zero.
     h*                 2.Do not clear Claim information entered.
     h*                 3.Write new adjusted record to "To Claim" number if ente
     h*                   Changes done in WrtCorrAdjRcd
     h*                 4.valid remaining reserve of "To Claim" before payement
     h*                   when claim is open.
     h*                 5.valid reserve cushion of "To Claim" before payement tr
     h*                   when claim is closed.
     h*                 6.Effective date populated in Reserve history file
     h*                 DSPF Changes :
     h*                 1. Added changes for onselect event for "Other Claim"
     h*                    (TxbClaimant) to not Blank out value of "Payee1" (Txb
     h*                    when Inmode is C.
     h*                 2. Added disable indicator for Payee1 (TxbPayee).i.e.dsb
     h*
     h* 10/03/24 C: 053 Support: 117774
     h*                 Remove edit for Payment code against Deductible
     h*                 codes for group on recoveries.
     h*
     h* 09/25/24 C: 053 Support: 117694
     h*                 Recompile for DSPF change - PV1 active only.
     h*
     h* 09/10/24 C: 079 Support: 140/117506
     h*                 Recurring Payment DARTS Remark Check Boxes
     h*
     h* 08/27/24 C: 079 Support: 140/117165
     h*                 Display file changes in PDSDVP (6.3.3). On PV1
     h*                 condition calling function pv1values to avoid
     h*                 clering this field.
     h*
     h* 12/04/23 C: 079 Support: 99/114342
     h*                 Set focus on key fields for Multiple Adjustment
     h*
     h*****************************************************************
     fWTCLMPMT  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f*
     f* Company Master File
     f*
     fsmcol     IF   E           k Disk
     f*
     f* User Parameter Master File
     f*
     fSmupl     IF   E           K Disk
     f*
     f* User Parameter Master File
     f*
     fSmupl1    IF   E           K Disk    Rename(pup01:pup02)
     f*
     f* System Wide State Table file
     f*
     fStstl1    if   e           k Disk
     f*
     f* Work Comp Table file
     f*
     fWtabl     if   e           k Disk
     f*
     f* Generic Table file
     f*
     fWtgtl     if   e           k Disk
     f*
     f* Claim Master file
     f*
     fWmcmp     if   e             Disk
     f*
     f*
     fWmcml1    uf   e           k Disk    Rename(pcm01:pcm02)
     f*
     f* Claim Demographic file
     f*
     fWmcdl     if   e           k Disk
     f*
     f* Employer Detail file
     f*
     fWdell     uf   e           k Disk
     f*
     f* Employer Rating file
     f*
     fWmrsl     if   e           k Disk    Rename(prs01:prs02)
     f*
     f* Deductible Discount Code file
     f*
     fWtddl3    if   e           k Disk
     f*
     f* Group Master file
     f*
     fWmfnl     if   e           k Disk
     f*
     f* Group Auxiliary file
     f*
     fWdf2l     if   e           k Disk
     f*
     f* Group Detail file
     f*
     fWdf1l     if   e           k Disk
     f*
     f* Provider Master file
     f*
     fWmpdl     if   e           k Disk
     f*
     f* Group Provider file
     f*
     fWmnrl     if   e           k Disk
     f*
     f* Additional Payee
     f*
     fWdacl1    if   e           k Disk
     f*
     f* Check Request file
     f*
     fWarpl     uf a e           k Disk    InfDs(DbfDs1)
     f*
     f* Check Payment file
     f*
     fWdpal     uf a e           k Disk    Rename(ppa01:ppa00) InfDs(DbfDs2)
     f*
     f* Check# Control File
     f*
     fWsckl     uf   e           k Disk
     f*
     f* Check Request file
     f*
     fwarplt    if   e           k Disk    Rename(prp01:prp02)
     f*
     f* Check Request file
     f*
     fwarpln    if   e           k Disk    Rename(prp01:prp03)
     f*
     f* Check Payment file
     f*
     fwdpalt    if   e           k Disk    Rename(ppa01:ppa02)
     f*
     f* Check Payment file
     f*
     fwdpal5    if   e           k Disk    Rename(ppa01:ppa04)
     f*
     f* Refund file
     f*
     fwmrrl     uf a e           k Disk
     f*
     f* Refund file
     f*
     fwmrrp     if   e             Disk    Rename(prr01:prr02)
     f*
     f* Check Payment file
     f*
     fwdpap     uf   e             Disk    Rename(ppa01:ppa03)
     f*
     f* Payment Status Code file
     f*
     fWtpsl     if   e           k Disk
     f*
     f* Claims DARTS control file
     f*
     fWmcxl     if   e           k Disk
     f*
     f* WCMC Check Issuance control file
     f*
     fWmcil     if   e           k Disk
     f*
     f* Claims Legal file
     f*
     fWmlll     if   e           k Disk
     f*
     f* Provider Bills EOB transaction file
     f*
     fFaeal1    if   e           k Disk    UsrOpn
     f*
     f* AWW files
     f*
     fWmawl6    if   e           k Disk
     fWmawl4    if   e           k Disk    Rename(paw01:paw02)
     fWmawl1    if   e           k Disk    Rename(paw01:paw03)
     f*
     f* Work History files
     f*
     fWmwhl     if   e           k Disk
     f*
     f* Reserve cushion file
     f*
     fWdrcl     if   e           k Disk    Rename(prc01:prc02)
     f*
     f* Claim status history file
     f*
     fWmchl     if   e           k Disk
     f*
     f* Comp Payment Control File
     f*
     fWmpcl     if   e           k Disk    Rename(ppc01:ppc02)
     f*
     f* DARTS Remark Code
     f*
     fStctl1    if   e           k Disk
     f*
     f* Check View File
     f*
     fWDRPL     if a e           k Disk    Rename(prp01:prp06)
     f*
     fWDRPL3    if   e           k Disk    Rename(prp01:prp05)
     f*
     f* Payment code table file
     f*
     fWTPCP     if   e           k Disk    Rename(ppc01:wpc0p)
     f*
     f* Reserve history file
     f*
     fWMRHP     O    E             DISK

     dpsds           esds                  extname(szq1p)
      //*** Beg Add *** 10/29/24 ***************************************
     fWMCML2    if   e           k Disk    Rename(pcm01:pcm03)
     f                                     prefix(L3_)
      //*** End Add *** 10/29/24 ***************************************
       //===================================================================
       // Standalone top
       //===================================================================
       Dcl-s inCall Char(1);
       Dcl-s intCo# Like(RPco#);
       Dcl-s intFnd Like(RPfnd);
       Dcl-s intFyr Like(RPfyr);
       Dcl-s intCase Like(RPcase);
       Dcl-s inCo#Int Like(RPco#);
       Dcl-s inSub#Int Like(EAsub#);
       Dcl-s inFndInt Like(RPfnd);
       Dcl-s inFyrInt Like(RPfyr);
       Dcl-s inCaseInt Like(RPcase);
       Dcl-s inCo#Chr Char(3);
       Dcl-s inFndChr Char(3);
       Dcl-s inFyrChr Char(3);
       Dcl-s inCaseChr Char(7);
       Dcl-s inEmp#Chr Char(9);
       Dcl-s inDivChr Char(5);
       Dcl-s outRtDtChr Char(7);
       Dcl-s inAmt Like(RPamt);
       Dcl-s inAamt Like(RPamt);
       Dcl-s svAmt Like(RPamt);
       Dcl-s type char(20);
       Dcl-s CltId char(3);
       Dcl-s isInvalid ind Inz(*off);
       Dcl-s isInvalidWdw ind Inz(*off);
       Dcl-s isInvoice# ind Inz(*off);
       Dcl-s outSuffix char(3);
       Dcl-s sysDt date;
       Dcl-s sysDt7 zoned(7:0);
       Dcl-s sysDt8 zoned(8:0);
       Dcl-s sysTm6 zoned(6:0);
       Dcl-s numDate8 zoned(8:0);
       Dcl-s outRrnChr char(9);
       Dcl-s outEobType char(9);
       Dcl-s outCkTp char(10);
       Dcl-s keyFein Like(PDfein);
       Dcl-s keyMod Like(PDmod);
       Dcl-s keyTbId Like(GTtbId);
       Dcl-s intPaySeq# Like(RPseq#);
       Dcl-s inAppl char(2);
       Dcl-s strCtrd Like(CDadr2);
       Dcl-s strTbId Like(GTtbId);
       Dcl-s strNotFnd Like(CDadr3) Inz('Not Found');
       Dcl-s strZip char(10);
       Dcl-s intDsq# Like(ACseq#);
       Dcl-s strPsig char(1);
       Dcl-s strBr2 char(2);
       Dcl-s outRtCd char(1);
       Dcl-s outDupRtCd char(1);
       Dcl-s outRtCd2 char(2);
       Dcl-s inData char(75);
       Dcl-s outEftC char(1);
       Dcl-s outPre char(1);
       Dcl-s isEft ind;
       Dcl-s isPre ind;
       Dcl-s allowNegFees ind;
       Dcl-s strWrk10 char(10);
       Dcl-s strWrk12 char(12);
       Dcl-s strWrk15 char(15);
       Dcl-s inEkey char(31);
       Dcl-s inPtyp char(1);
       Dcl-s inRcn# Packed(3:0);
       Dcl-s inDateChr char(7);
       Dcl-s inFrDt8 zoned(8:0);
       Dcl-s inToDt7 zoned(7:0);
       Dcl-s inFrDt7 zoned(7:0);
       Dcl-s inToDt8 zoned(8:0);
       Dcl-s inFrDt8Pck Packed(8:0);
       Dcl-s inToDt8Pck Packed(8:0);
       Dcl-s inType char(1);
       Dcl-s inTSET char(3);
       Dcl-s inTRiD char(3);
       Dcl-s inSdUE Zoned(3:0);
       Dcl-s inRls Char(1);
       Dcl-s inMtc Char(2);
       Dcl-s inJrSt Char(2);
       Dcl-s inHcPr Char(5);
       Dcl-s inHcEd Char(1);
       Dcl-s inFroi Char(2);
       Dcl-s inFnRc Char(5);
       Dcl-s inElTd Zoned(3:0);
       Dcl-s inElt  Char(1);
       Dcl-s inEdiFmt Char(3);
       Dcl-s inEaDc Char(5);
       Dcl-s inCsDt Zoned(7:0);
       Dcl-s inRrn1 Packed(9:0);
       Dcl-s inReCd char(2);
       Dcl-s strMsg char(256);
       Dcl-s isIdc ind;
       Dcl-s isWCMC ind;
       Dcl-s isWCMCintFc ind;
       Dcl-s isCallPbills ind;
       Dcl-s isComplete ind;
       Dcl-s isOvr ind;
       Dcl-s isVer ind;
       Dcl-s isDupl ind;
       Dcl-s isVldDays ind;
       Dcl-s isFirstCmpPmt Char(1);
       Dcl-s inUser char(10);
       Dcl-s inTrCd char(2);
       Dcl-s inSeq# Zoned(5:0);
       Dcl-s inSeq#2 Packed(5:0);
       Dcl-s inEob#Int Packed(9:0);
       Dcl-s inUtyp Char(1);
       Dcl-s inUlvl Zoned(3:0);
       Dcl-s inCtyp Char(1);
       Dcl-s inClvl Zoned(3:0);
       Dcl-s inExec Char(1);
       Dcl-s outRes Char(1);
       Dcl-s inCtyp2 Char(1);
       Dcl-s inCtyp3 Char(1);
       Dcl-s inCtyp4 Char(1);
       Dcl-s inCtyp5 Char(1);
       Dcl-s inCtyp6 Char(1);
       Dcl-s intdays Zoned(5:0);
       Dcl-s svWeeks Zoned(3:0);
       Dcl-s svDays Zoned(1:0);
       Dcl-s inWeeks Char(3);
       Dcl-s inDays Char(1);
       Dcl-s inPv1 Char(2);
       Dcl-s outAmount Packed(9:2);
       Dcl-s outSub#Chr Char(5);
       Dcl-s outFndChr Char(3);
       Dcl-s inFkey Char(2);
       Dcl-s inInv# Char(10);
       Dcl-s inFein Like(PDfein);
       Dcl-s inMod Like(PDmod);
       Dcl-s chrFein Char(9);
       Dcl-s outRsvAutL Packed(11:2);
       Dcl-s isGrpChk Char(1);
       Dcl-s isGrpProv Ind;
       Dcl-s isFoundRsvCd Char(1);
       Dcl-s x Zoned(3:0);
       Dcl-s y Zoned(3:0);
       Dcl-s intFein Like(RPfein);
       Dcl-s intMod Like(RPmod);
       Dcl-s ClaimRemRsv Like(RPamt);
       Dcl-s decRsvCushion Like(RCmAmt);
       Dcl-s decRsvBal Like(RPamt);
       Dcl-s strReCd Char(2);
       Dcl-s inDfund Char(51);
       Dcl-s inDcName Char(36);
       Dcl-s inDpName Char(36);
       Dcl-s in90 Ind;
       Dcl-s inReTp Char(1);
       Dcl-s outDupComment Char(40);
       Dcl-s isPendPmt Char(1);
       Dcl-s decFrDt7 Zoned(7:0);
       Dcl-s decToDt7 Zoned(7:0);
       Dcl-s decPoDt7 Zoned(7:0);
       Dcl-s decDate7 Zoned(7:0);
       Dcl-s decDate8 Zoned(8:0);
       Dcl-s decDays Zoned(5:0);
       Dcl-s chrSday Char(4);
       Dcl-s erEstRtw Ind;
       Dcl-s erRtwDt Ind;
       Dcl-s scFrDt10 Char(10);
       Dcl-s scToDt10 Char(10);
       Dcl-s scErtwDt10 Char(10);
       Dcl-s scRtwDt10 Char(10);
       Dcl-s scLwDt10 Char(10);
       Dcl-s erLwDt Ind;
       Dcl-s decRtwDt Zoned(7:0);
       Dcl-s strSrvDtMsg Char(200);
       Dcl-s decStartDt Zoned(7:0);
       Dcl-s decAmount Zoned(9:2);
       Dcl-s leavePgm Ind;
       Dcl-s inKey Char(40);
       Dcl-s inComm Char(256);
       Dcl-s inCdat Char(8);
       Dcl-s inDcId Char(12);
       Dcl-s inPnam Char(10);
       Dcl-s inSecL Char(3);
       Dcl-s inPgm Char(10);
       Dcl-s inFold Char(80);
       Dcl-s inDiFl Char(1);
       Dcl-s iniPgm Char(10);
       Dcl-s inFl01 Char(1);
       Dcl-s inFl02 Char(1);
       Dcl-s inRmCd Char(5);
       Dcl-s inseq#Chr Char(5);
       Dcl-s inSbYl Char(1);
       Dcl-s inCom2 Char(19);
       Dcl-s inApId Char(2);
       Dcl-s inIdoc Char(12);
       Dcl-s inIque Char(10);
       Dcl-s inImgF Char(1);
       Dcl-s inIpth Char(10);
       Dcl-s inPidx Char(5);
       Dcl-s inQpty Char(1);
       Dcl-s inDcTp Char(5);
       Dcl-s inCsId Char(2);
       Dcl-s inCode Char(2);
       Dcl-s strStaf Like(UPstaf);
       Dcl-s decRmkDt7 Zoned(7:0);
       Dcl-s strFrDt Char(150);
       Dcl-s strToDt Char(150);
       Dcl-s outFtyp Char(10);
       Dcl-s inFtyp Char(10);
       Dcl-s outHdCy Char(1);
       Dcl-s outChkQ Char(10);
       Dcl-s inChkQ Char(10);
       Dcl-s outCpyQ Char(10);
       Dcl-s outEfDt Char(7);
       Dcl-s inCoId Char(20);
       Dcl-s outAcct Char(20);
       Dcl-s inSrDt Char(8);
       Dcl-s inCkTp Char(10);
       Dcl-s inTrDtChr Char(7);
       Dcl-s intDcEx Like(ELdcEx);
       Dcl-s int2Dex Like(EL2Dex);
       Dcl-s chrDdCd Like(ELdcD2);
       Dcl-s intDdEf Like(EL2Def);
       Dcl-s hldSeq# Like(PAseq#);
       Dcl-s intReq#Rec Like(RRreq#);
       Dcl-s svRemRsv Like(CMrr01);
       Dcl-s svELout Like(ELouts);
       Dcl-c sFlag  const('Decrease reserve for Single Adjustment on closed +
                           claim');
       Dcl-c aFlag  const('Increase reserve for Single Adjustment on closed +
                           claim');
       Dcl-s strPayCd Char(2);
       Dcl-s payCdOld Char(2);
       Dcl-s pv1Old Char(2);
       Dcl-s xxCo# Like(PAco#);
       Dcl-s xxTrdt Packed(7:0);
       Dcl-s xxFein Like(PAfein);
       Dcl-s xxMod Like(PAmod);
       Dcl-s xxLast Char(56);
       Dcl-s xxFrst Char(15);
       Dcl-s xxTerm Char(1);
       Dcl-s svOuts Like(ELouts);
       Dcl-s svRR Like(CMrr01);
       Dcl-s xxseq# Like(PAseq#);
       Dcl-s svDsq# Like(PAdsq#);
       Dcl-s svAccd Like(PAcktp);
       Dcl-s svAcct Like(PAacct);
       Dcl-s svLtcd Like(PAltcd);
       Dcl-s svFnal Like(PAfnal);
       Dcl-s svPgtp Like(PApgtp);
       Dcl-s svEob# Like(PAeob#);
       Dcl-s svEobs Like(PAeobs);
       Dcl-s svDpcd Like(PAdpcd);
       Dcl-s svDpcu Like(PAdpcu);
       Dcl-s xxAM Packed(11:2);
       Dcl-s xxCush Packed(9:2);
       Dcl-s I1 Packed(3:0);
       Dcl-s J1 Packed(3:0);
       Dcl-s B Packed(3:0);
       Dcl-s Svtype Char(1);
       Dcl-s Sv1rcd Char(2);
       Dcl-s xxRem Zoned(11:2);
       Dcl-s xxname Char(40);
       Dcl-s xxdate Packed(8:0);
       Dcl-s xxAmt Like(scOfAmount);
       Dcl-s xxExpl Like(RHexpl);
       Dcl-c incAdj const('INCREASE RESERVE FOR ADJUSTMENT TO CLOSED CLAIM');
       Dcl-c decAdj const('DECREASE RESERVE FOR ADJUSTMENT TO CLOSED CLAIM');
       Dcl-s xxintApayee Ind;
       //*** Beg Add *** 12/04/23 *************************************
       Dcl-s pv12Old Char(2);
       Dcl-s pv13Old Char(2);
       Dcl-s pv14Old Char(2);
       Dcl-s adPay2Old Char(40);
       Dcl-s adPay3Old Char(40);
       Dcl-s adPay4Old Char(40);
       Dcl-s Payee2Old Zoned(12:0);
       Dcl-s Payee3Old Zoned(12:0);
       Dcl-s Payee4Old Zoned(12:0);
       Dcl-s Claim2Old Zoned(7:0);
       Dcl-s Claim3Old Zoned(7:0);
       Dcl-s Claim4Old Zoned(7:0);
       Dcl-s PayClm2Old Char(1);
       Dcl-s PayClm3Old Char(1);
       Dcl-s PayClm4Old Char(1);
       //*** End Add *** 12/04/23 *************************************
       //*** Beg Add *** 10/29/24 ***************************************
       Dcl-s decCo# Like(CMco#) Inz;
       Dcl-s decFnd Like(CMFnd) Inz;
       Dcl-s decFyr Like(CMFyr) Inz;
       Dcl-s decEmp# Like(CMemp#) Inz;
       Dcl-s decDiv Like(CMdiv) Inz;
       Dcl-s decCase Like(CMcase) Inz;
       Dcl-s charSS# Like(CMss#) Inz;
       Dcl-s isToClmFnd ind;
       //*** EndSS#  *** 10/29/24 ***************************************
       //*** Beg *** 09/10/25 ***************************************
       Dcl-s xxPPmt Zoned(11:2);
       Dcl-s ClaimRsvTyp Char(2);
       //*** End *** 09/10/25 ***************************************
       //===================================================================
       // Standalone bottom
       //===================================================================
       // Arrays
       //===================================================================
       dcl-s arrRemRsv Zoned(9:2) dim(10);
       dcl-s arrRemRsvGroup Zoned(9:2) dim(10);
       dcl-s arrRsvCode Char(2) dim(10);
       dcl-s RCD Char(2) dim(4);
       dcl-s RTP Char(1) dim(4);
       //===================================================================
       // Data Structures
       //===================================================================
     d                 ds
     d  dsData                 1     75
     d  dsTrDt                 1      7  0
     d  dsFrDt                 8     14  0
     d  dsToDt                15     21  0
     d  dsTrCd                22     23
     d  dsReCd                24     25
     d  dsPv1                 26     27
     d  dsPv2                 28     29
     d  dsSeq#                30     34  0
     d  dsEob#                35     43
     d  dsFein                44     52  0
     d  dsMod                 53     55  0
     d  dsDsq#                56     60  0

     d                 ds
     d  taxID#                 1     12  0
     d  fein                   1      9  0
     d  modifier              10     12  0

     d* Claim/DARTS control file reserve type
     d                 ds
     d arrRt                   1      4    Dim(4)
     d
     d  CXrt01                 1      1
     d  CXrt02                 2      2
     d  CXrt03                 3      3
     d  CXrt04                 4      4

     d* Claim file reserve type
     d                 ds
     d dsClaimRsvTyp           1     20    Dim(10)
     d
     d  CMrt01                 1      2
     d  CMrt02                 3      4
     d  CMrt03                 5      6
     d  CMrt04                 7      8
     d  CMrt05                 9     10
     d  CMrt06                11     12
     d  CMrt07                13     14
     d  CMrt08                15     16
     d  CMrt09                17     18
     d  CMrt10                19     20

     d* Claim file payment against reserve
     d                 ds
     d dsClaimPmtRsv           1     90  2 Dim(10)
     d
     d  CMar01                 1      9  2
     d  CMar02                10     18  2
     d  CMar03                19     27  2
     d  CMar04                28     36  2
     d  CMar05                37     45  2
     d  CMar06                46     54  2
     d  CMar07                55     63  2
     d  CMar08                64     72  2
     d  CMar09                73     81  2
     d  CMar10                82     90  2
     d*
     d* Claim file remaining reserve
     d                 ds
     d dsClaimRemRsv           1     90  2 Dim(10)
     d
     d  CMrr01                 1      9  2
     d  CMrr02                10     18  2
     d  CMrr03                19     27  2
     d  CMrr04                28     36  2
     d  CMrr05                37     45  2
     d  CMrr06                46     54  2
     d  CMrr07                55     63  2
     d  CMrr08                64     72  2
     d  CMrr09                73     81  2
     d  CMrr10                82     90  2
     d*
     d* Payment code
     d                 ds
     d  CD                     1      8
     d                                     DIM(4)
     d  scPayCd                1      2
     d  scPayCd2               3      4
     d  scPayCd3               5      6
     d  scPayCd4               7      8
     d*
     d* Year
     d*                ds
     d* YR                     1     12  0
     d*                                    DIM(4)
     d* SCYR01                 1      3  0
     d* SCYR02                 4      6  0
     d* SCYR03                 7      9  0
     d* SCYR04                10     12  0
     d*
     d* Case number
     d*                ds
     d* CA                     1     28  0
     d*                                    DIM(4)
     d* SCCA01                 1      7  0
     d* SCCA02                 8     14  0
     d* SCCA03                15     21  0
     d* SCCA04                22     28  0
     d*
     d* Fein
     d*                ds
     d* FE                     1     36  0
     d*                                    DIM(4)
     d* SCFE01                 1      9  0
     d* SCFE02                10     18  0
     d* SCFE03                19     27  0
     d* SCFE04                28     36  0
     d*
     d* Modifier
     d*                ds
     d* MO                     1     12  0
     d*                                    DIM(4)
     d* SCMO01                 1      3  0
     d* SCMO02                 4      6  0
     d* SCMO03                 7      9  0
     d* SCMO04                10     12  0
     d*
     d* Description
     d                 ds
     d  DS                     1    120
     d                                     DIM(4)
     d  scPayCdDsc             1     30
     d  scPayCdDs2            31     60
     d  scPayCdDs3            61     90
     d  scPayCdDs4            91    120
     d*
     d* Payment Value 1
     d                 ds
     d  B2                     1      8
     d                                     DIM(4)
     d  scPv1                  1      2
     d  scPv12                 3      4
     d  scPv13                 5      6
     d  scPv14                 7      8
     d*
     d* Payment Value 2
     d                 ds
     d  B7                     1      8
     d                                     DIM(4)
     d  scPv2                  1      2
     d  scPv22                 3      4
     d  scPv23                 5      6
     d  scPv24                 7      8
     d*
     d* Posting Date
     d                 ds
     d  PO                     1     32  0 Inz
     d                                     DIM(4)
     d  scPoDt                 1      8  0
     d  scPoDt2                9     16  0
     d  scPoDt3               17     24  0
     d  scPoDt4               25     32  0
     d*
     d* Date of Service - From Date
     d                 ds
     d  FR                     1     32  0 Inz
     d                                     DIM(4)
     d  scFrDt                 1      8  0
     d  scFrDt2                9     16  0
     d  scFrDt3               17     24  0
     d  scFrDt4               25     32  0
     d*
     d* Date of Service - To Date
     d                 ds
     d  TO                     1     32  0 Inz
     d                                     DIM(4)
     d  scToDt                 1     08  0
     d  scToDt2                9     16  0
     d  scToDt3               17     24  0
     d  scToDt4               25     32  0
     d*
     d* Weeks
     d                 ds
     d  WK                     1     12  0 Inz
     d                                     DIM(4)
     d  scWeeks                1      3  0
     d  scWeeks2               4      6  0
     d  scWeeks3               7      9  0
     d  scWeeks4              10     12  0
     d*
     d* Days
     d                 ds
     d  DY                     1      4  0 Inz
     d                                     DIM(4)
     d  scDays                 1      1  0
     d  scDays2                2      2  0
     d  scDays3                3      3  0
     d  scDays4                4      4  0
     d*
     d* Amount
     d                 ds
     d  AM                     1     36  2 Inz
     d                                     DIM(4)
     d  scAmount               1      9  2
     d  scAmount2             10     18  2
     d  scAmount3             19     27  2
     d  scAmount4             28     36  2
     d*
     d                 ds
     d  RRT                    1     20    Inz
     d                                     DIM(10)
     d  RHrt01                 1      2
     d  RHrt02                 3      4
     d  RHrt03                 5      6
     d  RHrt04                 7      8
     d  RHrt05                 9     10
     d  RHrt06                11     12
     d  RHrt07                13     14
     d  RHrt08                15     16
     d  RHrt09                17     18
     d  RHrt10                19     20
     d*
     d                 ds
     d  RSU                    1     90  2 Inz
     d                                     DIM(10)
     d  RHsu01                 1      9  2
     d  RHsu02                10     18  2
     d  RHsu03                19     27  2
     d  RHsu04                28     36  2
     d  RHsu05                37     45  2
     d  RHsu06                46     54  2
     d  RHsu07                55     63  2
     d  RHsu08                64     72  2
     d  RHsu09                73     81  2
     d  RHsu10                82     90  2

     d*
     d  DbfDs1         ds
     d  DbRrn1               397    400B 0

     d*
     d  DbfDs2         ds
     d  DbRrn2               397    400B 0
     d*** Beg Add *** 10/29/24 ***************************************
     d* Claim file reserve type
     d                 ds
     d dsToClmRsvTyp           1     20    Dim(10)
     d
     d  L3_CMrt01              1      2
     d  L3_CMrt02              3      4
     d  L3_CMrt03              5      6
     d  L3_CMrt04              7      8
     d  L3_CMrt05              9     10
     d  L3_CMrt06             11     12
     d  L3_CMrt07             13     14
     d  L3_CMrt08             15     16
     d  L3_CMrt09             17     18
     d  L3_CMrt10             19     20

     d* Claim file remaining reserve
     d                 ds
     d dsToClmRemRsv           1     90  2 Dim(10)
     d
     d  L3_CMrr01              1      9  2
     d  L3_CMrr02             10     18  2
     d  L3_CMrr03             19     27  2
     d  L3_CMrr04             28     36  2
     d  L3_CMrr05             37     45  2
     d  L3_CMrr06             46     54  2
     d  L3_CMrr07             55     63  2
     d  L3_CMrr08             64     72  2
     d  L3_CMrr09             73     81  2
     d  L3_CMrr10             82     90  2
     d*** End Add *** 10/29/24 ***************************************
       //===================================================================
       // Copybooks
       //===================================================================
     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrCompany
     d/copy sPrSql
     d/copy sPrXML
     d/copy sPrGenPop
     d/copy sPrJobInfo
     d/copy sPrModule
     d/copy wPrGroup
     d/copy wPrBank
     d/copy wPrCarrier
     d/copy wPrUsrPrms
     d/copy wPrPmtPop
     d/copy wPrIdc
     d/copy wPrClmRsv
       //===================================================================
       // Prototype Declaration
       //===================================================================

      *Entry
     d ClmPayments     pr                  Extpgm('WTCLMPMT')
     d                                1
     d                                9  0
     d                                9  0
     d                                9  0
     d                                3  0
     d                                7  0
     d                                7  0
     d                                9  0
     d                                3  0
     d                                1
     d                                1
     d                                9  2
     d                                2
     d                                2
     d                                2
     d                                2
     d                                7  0
     d                                1

     d ClmPayments     pi
     d  inMode                        1A
     d  inClmRrn                      9  0
     d  inPmtRrn                      9  0
     d  inTaxId                       9  0
     d  inModifier                    3  0
     d  inFromDt                      7  0
     d  inToDt                        7  0
     d  inEOB#                        9  0
     d  inRecon#                      3  0
     d  inSysType                     1
     d  inBillType                    1
     d  inPayment                     9  2
     d  inTransCode                   2
     d  inPayStatCode                 2
     d  inPayValue1                   2
     d  inPayValue2                   2
     d  inRecvDt                      7  0
     d  outReturnCode                 1

      * Intialize the Screen fields
     d LoadInVal       pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Validate Panel Sreen Values
     d ValidPmt        pr

      * Validate Adjustment Screen Values
     dAdjustmentValid  pr

      * Write Payment Request
     d WriteRqstPmt    pr

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d    cltId                       3

     d GetSuffix       PR                  extpgm('SA030R')
     d outsuffix                      3

      * Signature
     d ApprActPgm      pr                  ExtPgm('WA901R')
     d intCo#                         3  0
     d intFnd                         3  0
     d intFyr                         3  0
     d intCase                        7  0
     d intAmt                         9  2
     d charBr2                        2
     d*** Beg Chg *** 04/03/25 *********************************************
     d*** inFrDt8                        8s 0
     d inFrDt8Pck                     8p 0
     d*** End Chg *** 04/03/25 *********************************************
     d intAmt                         9  2
     d outRtCd                        1

      * Allocate Claim Reserves
     d AllocResClsClm  pr                  extpgm('SRPRCC')
     d intCo#                         3  0
     d intFnd                         3  0
     d intFyr                         3  0
     d intCase                        7  0
     d inReCd                         2
     d inType                         1
     d intAMT                         9  2

      * Process EDI A49's
     d ProcEdiA49      pr                  extpgm('WA076R')
     d intCo#                         3  0
     d inJrst                         2
     d inTrId                         3
     d intFnd                         3  0
     d outRtCd                        1
     d inEdiFmt                       3
     d inElt                          1
     d inElTd                         3s 0
     d inFroi                         2
     d insDue                         3s 0
     d inRls                          1
     d inCsDt                         7s 0
     d inEaDc                         5
     d inHcPr                         5
     d inFnRc                         5
     d inHcEd                         1

      * Generic Log API
     d GenlogApi       pr                  extpgm('W8070R4')
     d intCo#Chr                      3
     d intFndChr                      3
     d intFyrChr                      3
     d intCaseChr                     7
     d inTset                         3
     d inMtc                          2
     d inJrSt                         2
     d outRtCd                        1

      * EDI State Maintenance screen for Florida
     d ProcEdiFl       pr                  extpgm('W0070RFL')
     d inCall                         1
     d intCo#Chr                      3
     d intFndChr                      3
     d intFyrChr                      3
     d intCaseChr                     7
     d outRtCd                        1

      * Duplicate Payment Auto-Diary
     d DupPmntAutoDry  pr                  extpgm('WA061R')
     d inRrn1                         9  0
     d outRtCd                        1

      * Claims Security API
     d ClaimsSecure    pr                  extpgm('WA067R')
     d inCo#Chr                       3
     d inFndChr                       3
     d inFyrChr                       3
     d inCaseChr                      7
     d inUser                        10
     d outRtCd                        1

      * Batch Lockout API
     d BatchLockout    pr                  extpgm('WA044R')
     d outRtCd                        1

      * User Authority for Payment Type Code
     d ChkUserAuth     pr                  extpgm('SRCCSC')
     d inUtyp                         1
     d inUlvl                         3s 0
     d inCtyp                         1
     d inClvl                         3s 0
     d inExec                         1
     d outRes                         1
     d inCtyp2                        1
     d inCtyp3                        1
     d inCtyp4                        1
     d inCtyp5                        1
     d inCtyp6                        1

      * EOB Exists
     d EobExists       pr                  extpgm('FA031R')
     d intCo#                         3  0
     d intFnd                         3  0
     d intFyr                         3  0
     d intCase                        7  0
     d inEob#Int                      9p 0
     d inRcn#                         3p 0
     d outRtCd                        1

      * Auto Calculate Comp
     d AutoCalcComp    pr                  extpgm('WA058R')
     d inCo#Chr                       3
     d inFndChr                       3
     d inFyrChr                       3
     d inCaseChr                      7
     d inWeeks                        3
     d inDays                         1
     d inPv1                          2
     d outAmount                      9p 2

      * Get WCMC Subscriber and Group
     d getSub#Fnd      pr                  extpgm('FA700R')
     d outRtCd                        1
     d intCo#Chr                      3
     d intFndChr                      3
     d outSub#Chr                     5
     d outFndChr                      3

      * Check Invoice Number and Duplicate Payment
     d chkDupPmntInv#  pr
     d inCo#Int                       3  0
     d inFndInt                       3  0
     d inFyrInt                       3  0
     d inCaseInt                      7  0
     d inSeq#                         5s 0
     d inFein                         9  0
     d inmod                          3  0
     d inInv#                        10
     d inFkey                         2

      * Verify Payment Limit API
     d ReserveLimit    pr                  extpgm('SRRSVL')
     d intCo#                         3  0
     d intFnd                         3  0
     d intFyr                         3  0
     d intCase                        7  0
     d inReCd                         2
     d outRsvAutL                    11p 2

      * Harcopy Check
     d PrcChkVoucher   pr                  ExtPgm('W7017C')
     d inCo#Chr                       3
     d inFndChr                       3
     d inFyrChr                       3
     d inCaseChr                      7
     d inTrdtChr                      7
     d inSeq#Chr                      5
     d inFtyp                        10
     d inChkQ                        10

      * Duplicate Payment
     d chkDupPmnt      pr                  ExtPgm('WA019R')
     d intCo#                         3  0
     d intFnd                         3  0
     d intFyr                         3  0
     d intCase                        7  0
     d inSeq#2                        5p 0
     d inFrDt8Pck                     8p 0
     d inToDt8Pck                     8p 0
     d intAmt                         9  2
     d inReCd                         2
     d inFein                         9  0
     d inDFund                       51
     d inDcName                      36
     d inDpName                      36
     d in90                           1
     d inReTy                         1
     d inMod                          3  0
     d outRtCd                        1
     d outDupComment                 40

      * DARTS Remark
     d WriteDARTS      pr                  ExtPgm('DA700R')
     d inKey                         40
     d inComm                       256
     d inCdat                         8
     d inDcId                        12
     d inPnam                        10
     d inSecL                         3
     d inUser                        10
     d inPgm                         10
     d inFold                        80
     d inDiFl                         1
     d iniPgm                        10
     d inFl01                         1
     d inFl02                         1
     d inRmCd                         5
     d inseq#Chr                      5
     d inSbYl                         1
     d inCom2                        19
     d inApId                         2
     d inIdoc                        12
     d inIque                        10
     d inImgF                         1
     d inIpth                        10
     d inPidx                         5
     d inQpty                         1
     d inDcTp                         5
     d inCsId                         2
     d inCode                         2

      * Get appropriate Rating Date
     d GetRateDate     pr                  ExtPgm('SREFFD')
     d inCo#Chr                       3
     d inFndChr                       3
     d inFyrChr                       3
     d inEmp#Chr                      9
     d inDivChr                       5
     d outRtDtChr                     7

      * RESERVE CUSHION REMAINING CALCULATOR
     d chkRemCush      pr                  ExtPgm('SRCUSH')
     d                                3  0
     d                                3  0
     d                                3  0
     d                                7  0
     d                                2
     d                                1
     d                                9  2

      * Combine name fields into first name then last name
     d SRFRLS          pr                  Extpgm('SRFRLS')
     d                               56
     d                               12
     d                                1
      *

       //===================================================================
       // Begin processing
       //===================================================================

       // Start Of Program
       ProgramStart();
       // Main Procesing
       Main();
       // End Procesing
       ProgramEnd();
       //===================================================================
       // End processing
       //===================================================================


       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

       // Return and leave if errors encountered during ProgramStart
       If leavePgm = *on;
          Return;
       Endif;

       // Process until exit button is pressed.
       Dou BtnCancel = *On ;

          // Load screen fields that are common between record formats and
          // need to be reloaded on pui.click from the screen
          LoadCommon();

          // Display Screen
          Exsr ExfmtScreens;
          ClearErrors();

          Select;

          When BtnSubmit = *on;
             // Validate the panel
             ClearErrors();
             ValidPmt();
             If isInvalid = *Off and isInvalidWdw = *off;
                Select;
                When inMode = 'A' or inMode = 'C';   // Need to identify code no
                   UpdWdpap();          // update the selected transaction
                   WrtWdpap();          // Write the Offsetting Adjustment recor
                   UpdWmcmp();          // Process the Offsetting Claim master r
                   WrtCorrAdjRcd();     // Write the CORRECTING ADJUSTMENT recor

                // Normal payment mode
                When inMode = 'P'or inMode = 'I';
                   OtherProcesses();
                   If leavePgm = *on;
                      Return;
                   Endif;
                   If scType = 'P';
                      If scRecurr <> 'Y';
                         DupPmt();
                         If  outDupRtCd = '4';
                            Return;
                         Endif;
                         WriteRqstPmt();
                         ProcAdditional();
                      Else;
                         ProcessRecur();
                      Endif;
                   Endif;
                   If scType = 'M';
                      DupPmt();
                      If  outDupRtCd = '4';
                         Return;
                      Endif;
                      WriteManualPmt();
                      WriteViewCheck();
                      If outHdcy = 'Y';
                         PrintHardCopy();
                      Endif;
                      ProcAdditional();
                   Endif;
                // Recovery Mode
                When inMode = 'R' or inMode = 'T';
                   PreProcessRec();
                   OtherProcesses();
                   If leavePgm = *on;
                      Return;
                   Endif;
                   WriteRecPmt();
                   WriteViewCheck();
                   WriteRefund();
                   ProcAdditional();
                // Single Adjustment Mode
                When inMode = 'S';
                   DupPmt();
                   If  outDupRtCd = '4';
                      Return;
                   Endif;
                   WriteSinglePmt();
                   WriteViewCheck();
                   ProcAddtSingle();
                Endsl;
                Leave;
             Endif;
          When BtnCancel = *on;
             If cltId = '133' and inMode = 'I' and CMltFl <> 'R';
                isCallPbills = PgmInStack('F0393R');
                If isCallPbills;
                   strMsg = 'This bill cannot be completed without a check +
                    request being issued.  Press Enter to complete this +
                    bill/create the check request.  Press Cancel to return +
                    to the Provider Bills Complete/Pend screen';
                   isComplete = PopTwoBtn(strMsg:'Complete/Cancel Prompt':
                    'Enter':'Cancel');
                   If isComplete = *on;
                      BtnCancel = *off;
                   Else;
                      outReturnCode = 'P';
                      Leave;
                   Endif;
                Else;
                   Leave;
                Endif;

             Else;
                Leave;
             Endif;

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When BtnRefresh = *On;
             BtnRefresh =*Off;
             ProgramStart();

          Endsl;

       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;
       // P = Request for Payment.  I = Provider Bills
       // R = Recovery
       When inMode = 'P' or inMode = 'I' or inMode = 'R' or inMode = 'T';
          TabHeight = 750;
          Exfmt rcdPayment;
       // S = Single Adjustments
       When inMode = 'S';
          TabHeight = 600;
          LoadSingle();
          Exfmt rcdSingAdj;
       // A = Multiple Adjustments
       When inMode = 'A' or inMode = 'C';
          TabHeight = 700;
          LoadMultAdj();
          Exfmt rcdMultAdj;

       EndSl;

       Endsr;

     p Main            e

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

       // Batch is running so exit
       Callp BatchLockout(outRtCd);
       If outRtCd = 'F';
          strMsg = 'The Daily Batch is running so no payments can be made. +
             Press Return.';
          PopOneBtn(strMsg:'Batch Error':'Return');
          leavePgm = *on;
          Return;
       Endif;

       // Called from Provider Bills without a payment
       If inMode = 'I' and inPayment <= 0;
          leavePgm = *on;
          Return;
       Endif;

       sysDt = SystemDate();
       sysDt7 = DateTo7(sysDt);
       sysDt8 = DateTo8(sysDt);
       sysTm6 = TimeTo6(SystemTime());

       // Intialize the Screen fields
       LoadInVal();

       // Call claims security API to check claim authority
       // Common
       inUser = Q1user;
       Callp ClaimsSecure(inCo#Chr:inFndChr:inFyrChr:inCaseChr:inUser:outRtCd);
       If outRtCd = '2';
          strMsg = 'User is not authorized to this claimant.';
          PopOneBtn(strMsg:'Authority Error':'Return');
          leavePgm = *on;
          Return;
       Endif;

       // Open files and Read files
       // If Managed Care is Installed Open FAEAP
       If isModEnabled('Managed Care');
          isWCMC = *on;
          If Not %Open(FAEAL1);
             Open FAEAL1;
          Endif;
       Else;
          isWCMC = *off;
       Endif;

       If isModEnabled('WCMCInterface');
          isWCMCintFc = *on;
       Else;
          isWCMCintFc = *off;
       Endif;

       // User Parm file
       Chain (Q1user) pup01;

       // Intialize module flags
       If isModEnabled('IDC');
          isIdc = *on;
       Else;
          isIdc = *off;
       Endif;

       Return;

     p ProgramStart    e

       //===================================================================
       // ProgramEnd - End of program processing
       //===================================================================

     p ProgramEnd      b
     d ProgramEnd      pi
       // If Managed Care is Installed Open FAEAP
       If isModEnabled('Managed Care');
          If %Open(FAEAL1);
             Close FAEAL1;
          Endif;
       Endif;

       *inlr = *on;
       Return;

     p ProgramEnd      e

       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

       scMode = inMode;
       intDsq# = *zeros;

       // Get the user company number
       intCo# = UserCo#(Q1User);

       // Get the client id
       GetClientID(CltId);
       scCltId = CltId;
       If CltId = '114';
          is114 = *on;
       Else;
          is114 = *off;
       Endif;

       // Get the Claim Heading and demographics fields
       // Claim Master
       Chain inClmRrn pcm01;
       If %Found(WMCMP);
          intFnd = CMfnd;
          Chain (CMjrSt) pst01;
          If %Found(STSTL1);
             scJrSt = CMJrSt + ' - ' + %Trim(STname);
             scFnd  = CMfnd;
             scFyr  = CMfYr;
             scCase = CMcase;
             xxCo#  = CMco#;
          Endif;

          // Claim demographics
          Chain (CMss#) pcd01;

          // Employer Detail
          Chain(N) (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) pel01;

          // Claims DARTS Control file
          Chain (CMco#:CMfnd) pcx01;

          // Other Claim heading fields
          Chain (intCo#:'INJT':CMncci) pgt01;
          If %Found(WTGTL);
             scNcInj = %Trim(CMncci) + ' - ' + %Trim(GTdesc);
          Endif;
          Chain (intCo#:'LTFL':CMltFl) pgt01;
          If %Found(WTGTL);
             scLtStat = %Trim(CMltFl) + ' - ' + %Trim(GTdesc);
          Endif;
          scAww = CMaWw;
          scCompRt = CMcrt;
          Chain (intCo#:CMfnd:CMfyr:CMcase) paw01;
          If %Found(WMAWL6);
             Chain (intCo#:AWbr2) pb201;
             If %Found(WTABL);
                scBnType = AWBR2 + ' - ' + B2desc;
             Endif;
          Else;
             Setgt (intCo#:CMfnd:CMfyr:CMcase) paw02;
             Readpe (intCo#:CMfnd:CMfyr:CMcase) paw02;
             If Not %Eof(WMAWL4) and AWefDt <> 0;
                Chain (intCo#:AWbr2) pb201;
                If %Found(WTABL);
                   scBnType = AWBR2 + ' - ' + B2desc;
                Endif;
             Else;
                Setgt (intCo#:CMfnd:CMfyr:CMcase) paw03;
                Readpe (intCo#:CMfnd:CMfyr:CMcase) paw03;
                If Not %Eof(WMAWL1) and AWtrDt <> 0;
                   Chain (intCo#:AWbr2) pb201;
                   If %Found(WTABL);
                      scBnType = AWBR2 + ' - ' + B2desc;
                   Endif;
                Else;
                   scBnType = *blanks;
                Endif;
             Endif;

             // Get the Company Settings for the claim
             // Lost Time Flag (PV2)
             Chain intCo# pco01;
             If %Found(SMCOL);
                If COdPv2 = 'Y';
                   Select;
                   When CMltFl = 'I';
                      scPv2 = 'LT';
                   When CMltFl = 'M';
                      If CMpLfl = 'Y';
                         scPv2 = 'PL';
                      Else;
                         scPv2 = 'NL';
                      Endif;
                   When CMltFl = 'R';
                      scPv2 = 'NL';
                   Endsl;

                Endif;
             Endif;
          Endif;

          inCo#Int   = CMco#;
          inFndInt   = CMfnd;
          inFyrInt   = CMfyr;
          inCaseInt  = CMcase;
          inCo#Chr   = %Editc(CMco#:'X');
          inFndChr   = %Editc(CMfnd:'X');
          inFyrChr   = %Editc(CMfyr:'X');
          inCaseChr  = %Editc(CMcase:'X');
          TaxId# = 0;

          // Correcting
          If inMode = 'C' or inMode = 'A';
             visManPmt = *on;
             visRecurr = *off;

             scPoDtOff = sysDt8;
             scNoPayee = 1;


             Chain(N) inPmtRrn ppa03;
             If %Found(WDPAP);
                // Hidden Fields  - Beg
                scOfFe = PAfein;
                scOfMo = PAmod;
                scOfB2 = PAbr2;
                scOfB7 = PAbr7;
                scOfFr = PAfrdt;
                scOfTo = PAtodt;
                scOfWk = PAwks;
                scOfDy = PAdays;
                scPmtType = PAtype;
                scRsvType = PAretp;
                scPayCode = PAtrcd;
                scReCd = PArecd;
                xxSeq# = PAseq#;
                // Hidden Fields  - End
                // Population of Offset and Correcting Payment code/Desc
                scOffPayCd = inTransCode;
                Setll *start WTPCP;
                Dou %eof(WTPCP);
                   Read WTPCP;
                   If PCkey = scPayCode;
                      scDescOff = PCDesc;
                      scOfDs = PCDesc;
                      Leave;
                   Endif;
                Enddo;
                scPayCd = scOffPayCd;
                scPayCdDsc = scDescOff;

                scPv1 = PAbr2;
                scPv2 = PAbr7;
                scPoDt = sysDt8;
                scFrDt = dtMDYY(PAfrdt);
                scToDt = dtMDYY(PAtodt);
                scWeeks = PAwks;
                scDays = PAdays;
                scChk# = PAchk#;
                scAmount = PAamt;
                If inMode = 'A'or inMode = 'C';
                   scOfAmount = PAamt;
                Endif;

                // Population of Payee Field
                SelectPayee();
                // Load Payee Address from WDRPP
                LoadPayAdr();
             Endif;
          Endif;
e-sau

          // Fields that conditionally display for recurring
          // DARTS: Each Payment and Last Payment
          // Request Recurring
          Chain (CMco#:CMfnd) ppc02;
          If %Found(WMPCL);
             If PCrcEp <> *blanks;
                isRcEp = *on;
                //*** Beg Add *** 09/10/24 *********************************
                scDrtEaPmt  = 'Y';
                //*** End Add *** 09/10/24 *********************************
             Else;
                isRcEp = *off;
                //*** Beg Add *** 09/10/24 *********************************
                scDrtEaPmt  = ' ';
                //*** End Add *** 09/10/24 *********************************
             Endif;
             If PCrcLp <> *blanks;
                isRcLp = *on;
                //*** Beg Add *** 09/10/24 *********************************
                scDrtLsPmt  = 'Y';
                //*** End Add *** 09/10/24 *********************************
             Else;
                isRcLp = *off;
                //*** Beg Add *** 09/10/24 *********************************
                scDrtLsPmt  = ' ';
                //*** End Add *** 09/10/24 *********************************
             Endif;
             If PCarFl = 'Y';
                scInitSts = CXpsc2;
             Else;
                scInitSts = CXpmts;
             Endif;
          Endif;

          // Weeks/Days per Check and Number of Checks
          Chain (CMco#:'RCDF':CMjrSt) pgt01;
          If %Found(WTGTL);
             scWeeksChk = GTn501;
             scDaysChk  = GTn502;
             scNumFChk  = GTn503;
          Endif;

          // Set some property defaults
          readChk# = *on;

          // Load fields common to the screen rcdPayment
          If inMode = 'P' or inMode = 'R' or inMode = 'I' or inMode = 'T';
             LoadPayment();
          Endif;
       Endif;

       // Set intitial Payment Code values to be used for set focus property
       payCdOld = scPayCd;
       pv1Old = scPv1;
       //*** Beg Add *** 12/04/23 **************************************
       pv12Old = scPv12;
       pv13Old = scPv13;
       pv14Old = scPv14;
       //*** End Add *** 12/04/23 **************************************

       // If called from Provider Bills load key fields for the provider
       If inMode = 'I';
          keyFein = inTaxId;
          keyMod  = inModifier;
       Endif;

       // Display Group Check checkbox for group providers
       If scPayee <> *zeros or inMode = 'I';
          Chain (intCo#:keyFein:keyMod) pnr01;
          If %Found(WMNRL) and NRstat <> 'I';
             isGrpProv = *on;
             scPgTp = 'G';
          Else;
             isGrpProv = *off;
             scPgTp = ' ';
          Endif;
       Endif;
       //*** Beg Add *** 06/09/25 ******************************************
       If scAdPayee <> *Blanks;
          isGrpProv = *off;
          scPgTp = ' ';
       Endif;
       //*** End Add *** 06/09/25 ******************************************

       //*** Beg Add *** 01/30/25 *******************************************
       // Default Posting Date with Current System Date
       //*** Beg Add *** 08/13/25 *******************************************
       If cltId = '129';
       //*** End Add *** 08/13/25 *******************************************
       If scPoDt = 0;
          scPoDt = sysDt8;
       Endif;
       //*** Beg Add *** 08/13/25 *******************************************
       Endif;
       //*** End Add *** 08/13/25 *******************************************
       //*** End Add *** 01/30/25 *******************************************
       //*** Beg Add *** 03/26/25 *******************************************
       // Default Date Received with Current System Date
       //*** Beg Add *** 08/13/25 *******************************************
       If cltId = '129';
       //*** End Add *** 08/13/25 *******************************************
       If scDtRcv = 0;
          scDtRcv = sysDt8;
       Endif;
       //*** Beg Add *** 08/13/25 *******************************************
       Endif;
       //*** End Add *** 08/13/25 *******************************************
       // Default Payment Status Code to PY
       //*** Beg Del *** 04/16/25 ******************************************
       //***If scPayStCd = ' ';
       //***   scPayStCd = 'PY';
       //***Endif;
       //*** End Del *** 04/16/25 ******************************************
       // Default Payment Description to 'Comp System Payment'
       //*** Beg Del *** 03/31/25 ***************************************
       //***If scReTp = ' ';
       //***   scReTp = 'C';
       //***   scPayCd = 'CP';
       //***Endif;
       //*** End Add *** 03/26/25 *******************************************
       //*** End Del *** 03/31/25 ***************************************

     p LoadInVal       e

       //===================================================================
       // SelectPayee - Determine payee from WDPAP fields
       //===================================================================
     p SelectPayee     b
     d SelectPayee     pi
       If PAfein <> 0;
          Chain (PAfein:PAMod) ppd01;
          If %Found(WMPDL);
             scPayee = %Dec(%Editc(pdfein:'X')+%Editc(pdmod:'X')
              :12:0);
             keyFein = PDfein;
             keyMod  = PDmod;
          Else;
             If %Editc(PAfein:'X') = CMss#;
                scPayClm = 'Y';
             Endif;
          Endif;
       //*** Beg Chg *** 06/09/25 ******************************************
       //*** Else;
       Endif;
       // Search for adtnl payee even when FEIN # was entered
       //*** End Chg *** 06/09/25 ******************************************
          // Population of Additional Payee Field
          Chain(n) (paco#:pafnd:pafyr:pacase:pachk#) prp06;
          If %found(WDRPL);
             Setll (Rpco#:RPfnd:RPfyr:RPcase) pac01;
             Reade (Rpco#:RPfnd:RPfyr:RPcase) pac01;
             Dow not %eof(WDACL1);
                If RPpNam = ACpNam;
                   scAdPayee = RPpNam;
                   xxintApayee = *on;
                   Leave;
                Else;
                   xxname = %trim(ACfNam)+' '+ %trim(ACpNam);
                   If RPpNam = xxname;
                      scAdPayee = RPpNam;
                      xxintApayee = *on;
                      Leave;
                   Endif;
                Endif;
                Reade (Rpco#:RPfnd:RPfyr:RPcase) pac01;
             EndDo;

             // Population of Claimant Field
             If xxintApayee <> *on;
                scPayClm = 'Y';
             Endif;
          Endif;
       //*** Beg Del *** 06/09/25 ******************************************
       //*** Endif;
       //*** End Del *** 06/09/25 ******************************************

     p SelectPayee     e
       //===================================================================
       // LoadPayAdrRec - Load Payee Address for recoveries
       //===================================================================
     p LoadPayAdrRec   b
     d LoadPayAdrRec   pi

       Clear scPayNam;
       Clear scPayAdr1;
       Clear scPayAdr2;
       Clear scPayCsz;

       If scPayee <> *zeros;
          Chain (RRfein:RRmod) ppd01;
          If %Found(WMPDL);

          Endif;
       Endif;

       If scPayClm = 'Y';

       Endif;

     p LoadPayAdrRec   e
       //===================================================================
       // LoadPayAdr - Load Payee Address from WDRPP
       //===================================================================
     p LoadPayAdr      b
     d LoadPayAdr      pi

       // Load Address fields from WDRPP
       Clear scPayNam;
       Clear scPayAdr1;
       Clear scPayAdr2;
       Clear scPayCsz;
       Chain(n) (paco#:pafnd:pafyr:pacase:pachk#) prp06;
       If %Found(WDRPL);
          scPayNam  = RPpnam;
          scPayAdr1 = RPadr1;
          scPayAdr2 = RPadr2;
          strZip    = %Editc(RPzip:'X');
          strZip    = %Subst(strZip:1:5) + '-' + %Subst(strZip:6:4);
          If RPctry <> *blank and RPctry <> 'USA';
             strTbId = %Trim(RPctry);
             Chain (intCo#:'CTRY':strTbId) pgt01;
             If %found(WTGTL);
                strCtrd  = GTdesc;
             Else;
                strCtrd  = strNotFnd;
             Endif;
             If scPayAdr2 = *blanks;
                scPayAdr2 = RPadr3;
                scPayCsz     = strCtrd;
             Else;
                scPayCsz     = RPadr3;
             Endif;
          Else;
             scPayCsz = %Trim(RPcty) + ', ' + %Trim(RPst) + ' ' +
              %Trim(strZip);
          Endif;
          scPayCty = RPcty;
          scPaySt  = RPst;
          scPayZip = RPzip;
          scCtry   = RPctry;
          scAdr3   = RPadr3;
       Endif;

     p LoadPayAdr      e
       //===================================================================
       // LoadCommon - Load common Screen fields
       //===================================================================
     p LoadCommon      b
     d LoadCommon      pi
       // Load common screen fields
       payeeChoi='DIGITS(PDFEIN) || '' '' || DIGITS(PDMOD) || '' - '' || ' +
        'CASE WHEN PDDBA1=''Y'' THEN TRIM(PDADR1) || '' - '' || ' +
        'TRIM(PDADR2) ELSE ' +
        'TRIM(PDNAME) || '' - '' || TRIM(PDADR1) || '' '' || ' +
        'TRIM(PDADR2) END ' +
        ' || '' - '' || TRIM(PDCTY) || '', '' || ' +
        'PDST || '' '' || SUBSTR(DIGITS(PDZIP),1,5)';

       // EOB Properties
       strFrdt = 'Substr(EOfrDt+19000000,5,2) || ''/'' || +
        Substr(EOfrDt+19000000,7,2) || ''/'' || Substr(EOfrDt+19000000,1,4) +
           || ''-''';
       strToDt = 'Substr(EOtoDt+19000000,5,2) || ''/'' || +
        Substr(EOtoDt+19000000,7,2) || ''/'' || Substr(EOtoDt+19000000,1,4)';
       eobChoices = 'Digits(EOEOB#) || '' '' || Digits(EORCN) || '' - '' || ' +
        'Trim(EOPNAM) || '' - '' || Digits(EOFEIN) || '' - '' || ' +
         'Digits(EOMOD)';
       taxID# = scPayee;
       chrFein = %Editc(fein:'X');
       eobSelect = 'EOFND = ' + %TRIM(inFndChr) + ' and EOCASE = ' +
          %Trim(inCaseChr) + ' and EOFYR = ' + %TRIM(inFyrChr) +
          ' and EOfein = ' + %trim(chrFein);

       BnkSelect = 'BIFND = ' + %TRIM(inFndChr) + ' and BISTAT <> ''I''';

       claimChoi='CMCASE || '' - '' || TRIM(CMLSN) || '', '' || TRIM(CMFSN) ';

       // Load Pay to Name and Address based on Payee.  This logic is activated
       // by Javascript function pui.click() when Claimant, Additional Payye
       // or Payee is selected
       Clear scPayNam;
       Clear scPayAdr1;
       Clear scPayAdr2;
       Clear scPayCsz;

       Select;
       //*** Beg Chg *** 06/09/25 ******************************************
       //*** When scPayee <> 0;
       When scPayee <> 0 and scAdPayee = *Blanks;
       //***  Clear scAdPayee;
       //*** End Chg *** 06/09/25 ******************************************
          Clear scPayClm;
          //*** Beg Chg *** 10/29/24 ***************************************
          If inMode <> 'C';
          //*** Clear scClaim;
             Clear scClaim;
          Endif;
          //*** End Chg *** 10/29/24 ***************************************
          taxID#=scPayee;
          keyFein=fein;
          keyMod=modifier;
          Chain (keyFein:keyMod) ppd01;
          If %Found(WMPDL);
             If PDfsn <> *Blanks;
                scPayNam  = %Trim(PDfsn) + ' ' + %Trim(PDname);
             Else;
                scPayNam = PDname;
             EndIf;

             If PDdba1 = 'Y';
                scPayAdr1 = PDadr2;
             Else;
                scPayAdr1 = PDadr1;
                //*** Beg Add *** 04/21/25 *************************************
                scPayAdr2= PDadr2;
                //*** End Add *** 04/21/25 *************************************
             EndIf;
             //*** Beg Del *** 04/21/25 ****************************************
             //*** scPayAdr2= PDadr2;
             //*** End Del *** 04/21/25 ****************************************
             strZip    = %Editc(PDzip:'X');
             strZip    = %Subst(strZip:1:5) + '-' + %Subst(strZip:6:4);
             If PDctry <> *blank and PDctry <> 'USA';
                strTbId = %Trim(PDctry);
                Chain (intCo#:'CTRY':strTbId) pgt01;
                If %found(WTGTL);
                   strCtrd  = GTdesc;
                Else;
                   strCtrd  = strNotFnd;
                Endif;
                If scPayAdr2 = *blanks;
                   scPayAdr2 = PDadr3;
                   scPayCsz     = strCtrd;
                Else;
                   scPayCsz     = PDadr3;
                Endif;
             Else;
                scPayCsz = %Trim(PDcty) + ', ' + %Trim(PDst) + ' ' +
                 %Trim(strZip);
             Endif;
             scPayCty = PDcty;
             scPaySt  = PDst;
             scPayZip = PDzip;
             scCtry   = PDctry;
             scAdr3   = PDadr3;

             // Check to see if this is a Group Check Provider
             Chain (intCo#:keyFein:keyMod) pnr01;
             If %Found(WMNRL) and NRstat <> 'I';
                isGrpProv = *on;
                scPgTp = 'G';
             Else;
                isGrpProv = *off;
                scPgTp = ' ';
             Endif;

          EndIf;

       When scAdPayee <> *blanks;
          //*** Beg Chg *** 06/09/25 ******************************************
          //***  Clear scPayee;
          isGrpProv = *off;
          scPgTp = ' ';
          //*** End Chg *** 06/09/25 ******************************************
          Clear scPayClm;
          Clear scClaim;
          Chain (CMco#:CMfnd:CMfyr:CMcase:scAdPayee) pac01;
          If %Found(WDACL1);

             If ACfNam <> *Blanks;
                scPayNam  = %Trim(ACfNam) + ' ' + %Trim(ACpNam);
             Else;
                scPayNam = ACpNam;
             EndIf;

             scPayAdr1 = ACadr1;
             scPayAdr2 = ACadr2;
             strZip    = %Editc(ACzip:'X');
             strZip    = %Subst(strZip:1:5) + '-' + %Subst(strZip:6:4);
             If ACctry <> *blank and ACctry <> 'USA';
                strTbId = %Trim(ACctry);
                Chain (intCo#:'CTRY':strTbId) pgt01;
                If %found(WTGTL);
                   strCtrd  = GTdesc;
                Else;
                   strCtrd  = strNotFnd;
                Endif;
                If scPayAdr2 = *blanks;
                   scPayAdr2 = ACadr3;
                   scPayCsz     = strCtrd;
                Else;
                   scPayCsz     = ACadr3;
                Endif;
             Else;
                scPayCsz = %Trim(ACcty) + ', ' + %Trim(ACst) + ' ' +
                 %Trim(strZip);
             Endif;

             scPayCty = ACcty;
             scPaySt  = ACst;
             scPayZip = ACzip;
             scCtry   = ACctry;
             scAdr3   = ACadr3;
             intDsq#  = ACseq#;

          EndIf;

       When scPayClm = 'Y';
          Clear scPayee;
          Clear scAdPayee;
          isGrpProv = *off;
          Clear scClaim;
          Chain inClmRrn pcm01;
          If %Found(WMCMP);
             Chain (CMss#) pcd01;
             If %found(WMCDL);
               //*** Beg Add *** 05/02/25 **************************
               If CDmid = *Blanks;
                scPayNam  = %Trim(CDfsn) + ' ' + %Trim(CDlsn);
               Else;
               //*** End Add *** 05/02/25 **************************
                scPayNam  = %Trim(CDfsn) + ' ' + %Trim(CDmid) + ' ' +
                             %Trim(CDlsn);
               //*** Beg Add *** 05/02/25 **************************
               Endif;
               //*** End Add *** 05/02/25 **************************
                scPayAdr1 = CDadr1;
                scPayAdr2 = CDadr2;
                strZip    = %Editc(CDzip:'X');
                strZip    = %Subst(strZip:1:5) + '-' + %Subst(strZip:6:4);
                If CDctry <> *blank and CDctry <> 'USA';
                    strTbId = %Trim(CDctry);
                   Chain (intCo#:'CTRY':strTbId) pgt01;
                   If %found(WTGTL);
                      strCtrd  = GTdesc;
                   Else;
                      strCtrd  = strNotFnd;
                   Endif;
                   If scPayAdr2 = *blanks;
                      scPayAdr2 = CDadr3;
                      scPayCsz     = strCtrd;
                   Else;
                      scPayCsz     = CDadr3;
                   Endif;
                Else;
                   scPayCsz = %Trim(CDcty) + ', ' + %Trim(CDst) + ' ' +
                    %Trim(strZip);
                Endif;

                scPayCty = CDcty;
                scPaySt  = CDst;
                scPayZip = CDzip;
                scCtry   = CDctry;
                scAdr3   = CDadr3;
             Endif;
          Endif;

       When scClaim <> 0;
          Clear scPayee;
          Clear scAdPayee;
          Clear scPayClm;

          Chain (CMss#) pcd01;
          If %found(WMCDL);
            //*** Beg Add *** 05/02/25 **************************
            If CDmid = *Blanks;
             scPayNam  = %Trim(CDfsn) + ' ' + %Trim(CDlsn);
            Else;
            //*** End Add *** 05/02/25 **************************
             scPayNam  = %Trim(CDfsn) + ' ' + %Trim(CDmid) + ' ' + %Trim(CDlsn);
            //*** Beg Add *** 05/02/25 **************************
            Endif;
            //*** End Add *** 05/02/25 **************************

             scPayAdr1 = CDadr1;
             scPayAdr2 = CDadr2;
             strZip    = %Editc(CDzip:'X');
             strZip    = %Subst(strZip:1:5) + '-' + %Subst(strZip:6:4);
             If CDctry <> *blank and CDctry <> 'USA';
                strTbId = %Trim(CDctry);
                Chain (intCo#:'CTRY':strTbId) pgt01;
                If %found(WTGTL);
                   strCtrd  = GTdesc;
                Else;
                   strCtrd  = strNotFnd;
                Endif;
                If scPayAdr2 = *blanks;
                   scPayAdr2 = CDadr3;
                   scPayCsz     = strCtrd;
                Else;
                   scPayCsz     = CDadr3;
                Endif;
             Else;
                scPayCsz = %Trim(CDcty) + ', ' + %Trim(CDst) + ' ' +
                 %Trim(strZip);
                Endif;

             scPayCty = CDcty;
             scPaySt  = CDst;
             scPayZip = CDzip;
             scCtry   = CDctry;
             scAdr3   = CDadr3;
          Endif;

       Endsl;

       // Set field properties
       If cltId = '045' and scPayStCd <> *blanks;
          isReadStCd = *on;
       Else;
          isReadStCd = *off;
       Endif;

       If scType = ' ';
          scType    = 'P';
       Endif;

       If inMode = 'C';
          visNmPayee = *off;
          conPayeTop = 320;
       Endif;
       If inMode = 'A';
          visNmPayee = *on;
          conPayeTop = 385;
       Endif;


       Select;
       When inMode = 'P';
          If scType = 'P';
             scPmtType = 'P';
          Endif;
          If sctype = 'M';
             scPmtType = 'M';
          Endif;
       When inMode = 'S' or inMode = 'A' or inMode = 'C';
          scPmtType = 'A';
       When inMode = 'R' or inMode = 'T';
          scPmtType = 'R';
       When inMode = 'I';
          scPmtType = 'P';
       Endsl;

       scRsvType = scReTp;
       scRecurrG = scRecurr;

       // Load Pay Code Description, Reserve Code and Type based on the
       // Payment Transaction Code
sau    If inMode <> 'A' and inMode <> 'C' and inMode <> 'R';
          Chain (CMco#:scPayCd) ppc01;
          If %Found(WTABL);
             scPayCdDsc = PCdesc;
             scReCd = PCreCd;
             Chain (intCo#:PCreCd) prt01;
             If %Found(WTABL);
                scRsvType = RTtype;
                scReTp    = RTtype;
             Endif;
          Endif;
       Endif;

       // Set field positions based on Adr1 or Adr2 = blank
       Select;
       When inMode <> 'A';
          If scPayAdr1 = *blanks;
             topAdr2 = 345;
          Else;
             topAdr2 = 360;
          Endif;
          If scPayAdr2 = *blanks;
             topCsz = 360;
          Else;
             topCsz = 375;
          Endif;
       Endsl;
       // Payment Code has changed.  On screen re-load set focus to this field
       If scPayCd <> *blanks and scPayCd <> payCdOld;
          focPayCd = *on;
          payCdOld = scPayCd;
       Else;
          focPayCd = *off;
       Endif;
       // PV1 has changed.  On screen re-load set focus to this field       ld
       If scPv1 <> *blanks and scPv1 <> Pv1Old;
          focPv1 = *on;
          Pv1Old = scPv1;
       Else;
          focPv1 = *off;
       Endif;
       //*** Beg Add *** 04/16/25 *******************************************
       // Load the initial Payment Status if Recurring
       If scRecurr = 'Y' and scPayStCd = *blanks;
          scPayStCd =  scInitSts;
       Endif;
       //*** End Add *** 04/16/25 *******************************************

     p LoadCommon      e

       //===================================================================
       // LoadPayment - Load Payment Screen Fields
       //===================================================================
     p LoadPayment     b
     d LoadPayment     pi

       // Set field positions for client 114 fields
       If is114 = *on;
          LeftPmthLb = 480;
          TopPmthLb  = 475;
          LeftPrcMth = 480;
          TopPrcMth  = 490;
       Endif;

       // Selection criteria for Additional Payee
       SelAddPay = 'ACfnd=' + %Editc(CMfnd:'X') + ' and ACfyr=' +
        %Editc(CMfyr:'X') + ' and ACcase=' + %Editc(CMcase:'X') +
        ' and ACstat <> ''I''';

       // Load the Exclude from Weeks/Days flag
       Chain (CMco#:scPv1) pb201;
       If %Found(WTABL) and B2stat <> 'I';
          scExWdC = B2exWdC;
       Endif;

       // Set fields/Modes based on how the program is called
       // Provider Bills
       If inMode = 'I';
          visEomb#   = *on;
          readClmnt  = *on;
          readAddtl  = *on;
          readEob    = *on;
          readRcn    = *on;
          readPayee  = *on;
          readAmt    = *on;
          readDtRcv  = *on;
          readFrmDos = *on;
          readToDos  = *on;
          visManPmt   = *off;
          scEob# = inEob#;
          If Q1prms > 8;
             scRcn# = inRecon#;
          Else;
             scRcn# = 0;
          Endif;
          scPayee = %Dec(%Editc(inTaxId:'X') + %Editc(inModifier:'X'):12:0);
          scAmount  = inPayment;
          scFrDt    = DtMdYy(inFromDt);
          scToDt    = DtMdYy(inToDt);
          scDtRcv   = DtMdYy(inRecvDt);
          scPayCd   = inTransCode;

          If Q1prms >= 10;
             Chain (intCo#:IntFnd:'P':inSysType) pci01;
             If %Found(WMCIL);
                scPayStCd = CIsts;
                If CMcvFl = 'Y';
                   scPayCd = CItrCv;
                   scPv1   = CIp1Cv;
                Else;
                   scPayCd = CItrCd;
                   If inBillType = 'X';
                      scPv1 = CIxBr2;
                   Else;
                      scPv1 = CIbr2;
                   Endif;
                Endif;
                If CMltFl = 'I';
                   scPv2 = CIbr7L;
                Else;
                   scPv2 = CIbr7N;
                Endif;
             Endif;
          Endif;
          If scPayCd <> *blanks;
             isAutoChk = *on;
             readPayCd = *on;
             readReCd  = *on;
          Else;
             isAutoChk = *off;
             readPayCd = *off;
             readReCd  = *off;
          Endif;
       Endif;

       // Standard Check Request mode (NOT Provider Bills)
       If inMode = 'P';
          visEomb#   = *off;
          visCheck#  = *off;
          visPayRqst = *on;
          visManPmt  = *on;
          visLumpSum = *on;
          visRecurr  = *on;
          scRecurr   = 'N';
          visPdOnTm  = *on;
          If is114 = *on;
             visVoucher = *on;
          Else;
             visVoucher = *off;
          Endif;
       Endif;

       // Recovery
       If inMode = 'R';
          visPayRqst = *off;
          visManPmt  = *off;
          visLumpSum = *off;
          visRecurr  = *off;
          visFinal   = *off;
          visPdOnTm  = *off;
          visVoucher = *on;
          visCheck#  = *on;
          visEomb#   = *on;
          //*** Beg Del *** 01/30/25 *******************************************
          //*** readPayCd  = *on;
          //*** End Del *** 01/30/25 *******************************************
          readReCd   = *on;
          readChk#   = *off;
          readRcn    = *on;
          Chain (intCo#:inTransCode) ppc01;
          If %Found(WTABL) and PCstat <> 'I';
             scPayCdDsc = PCdesc;
             Chain (intCo#:PCreCd) prt01;
             If %Found(WTABL) and rtStat <> 'I';
                If RTtype = 'M';
                   visEomb# = *on;
                Endif;
             Endif;
          Endif;
          // Load fields from the payment file
          Chain(N) inPmtRrn ppa03;
          If %Found(WDPAP);
             scPmtType = 'R';
             scRsvType = PAretp;
             scReTp    = PAretp;
             scPayCd   = inTransCode;
             scPayCode = inTransCode;
             scReCd    = PArecd;
             scPv1     = PAbr2;
             scPv2     = PAbr7;
             scPoDt    = Dtmdyy(PApoDt);
             scDtRcv   = Dtmdyy(PAtrDt);
             scFrDt    = DtMdYy(PAfrDt);
             scToDt    = DtMdYy(PAtoDt);
             scWeeks   = PAwks;
             scDays    = PAdays;
             scAmount  = PAamt;
             scEob#    = PAeob#;
             scRcn#    = PAeobS;
             hldSeq#   = PAseq#;

             // Get the next available request number;
             Setgt (CMco#:CMfnd:CMfyr:CMcase) prr01;
             Readpe(N) (CMco#:CMfnd:CMfyr:CMcase) prr01;
             If %Found(WMRRL);
                intReq#Rec = RRreq# + 1;
             Else;
                intReq#Rec = 1;
             Endif;

             // Get a reserve code of category Type X or Cat X
             RRstCd = *blanks;
             Setll (PAco#) prs01;
             Reade (PAco#) prs01;
             Dow not %Eof(WTABL);
                If RSstat <> 'I' and (RSctGy = 'X' or RStype = 'X');
                   RRstCd = RSkey;
                   Leave;
                Endif;
                Reade (PAco#) prs01;
             Enddo;

             RRstTp = *blanks;
             RRatTy = *zeros;
             RRaMod = *zero;
             RReAmt = *zero;

             // Population of Payee Field
             If PAfein <> 0;
                If %Editc(PAfein:'X') = CMss#;
                   scPayClm = 'Y';
                Else;
                   Chain (PAfein:PAMod) ppd01;
                   If %Found(WMPDL);
                      scPayee = %Dec(%Editc(pdfein:'X')+%Editc(pdmod:'X')
                                 :12:0);
                   Endif;
                Endif;
             Else;
                scPayClm = 'Y';
             Endif;

          Endif;
       Endif;

       // Recovery Tracking
       If inMode = 'T';
          visPayRqst = *off;
          visManPmt  = *off;
          visLumpSum = *off;
          visRecurr  = *off;
          visFinal   = *off;
          visPdOnTm  = *off;
          visVoucher = *on;
          visCheck#  = *on;
          visEomb#   = *on;
          readPayCd  = *off;
          readReCd   = *off;
          readChk#   = *off;
          readRcn    = *on;
          readClmnt  = *on;
          readAddtl  = *on;
          readPayee  = *on;

          // Load fields from the Refund Request file
          Chain(N) inPmtRrn prr02;
          If %Found(WMRRP);
             scPmtType = 'T';
             scRsvType = *blanks;
             scReTp    = *blanks;
             scPayCd   = *blanks;
             scPayCode = inTransCode;
             scReCd    = PArecd;
             scPv1     = *blanks;
             scPoDt    = Dtmdyy(sysDt7);
             scFrDt    = 0;
             scToDt    = 0;
             scWeeks   = 0;
             scDays    = 0;
             scAmount  = RRrAmt;

             intReq#Rec = RRreq#;

             // Population of Payee Field
             If RRfein <> 0;
                If %Editc(RRfein:'X') = CMss#;
                   scPayClm = 'Y';
                Else;
                   Chain (RRfein:RRMod) ppd01;
                   If %Found(WMPDL);
                      scPayee = %Dec(%Editc(pdfein:'X')+%Editc(pdmod:'X')
                                 :12:0);
                   Endif;
                Endif;
             Else;
                scPayClm = 'Y';
             Endif;
          Endif;
       Endif;

     p LoadPayment     e

       //===================================================================
       // LoadPayment - Load Payment Screen Fields
       //===================================================================
     p LoadSingle      b
     d LoadSingle      pi

       // Set field positions based on Adr1 or Adr2 = blank
       If scPayAdr1 = *blanks;
          topAdr2 = 320;
       Else;
          topAdr2 = 335;
       Endif;
       If scPayAdr2 = *blanks;
          topCsz = 335;
       Else;
          topCsz = 350;
       Endif;

       // Selection criteria for Additional Payee
       SelAddPay = 'ACfnd=' + %Editc(CMfnd:'X') + ' and ACfyr=' +
        %Editc(CMfyr:'X') + ' and ACcase=' + %Editc(CMcase:'X') +
        ' and ACstat <> ''I''';

     p LoadSingle      e
       //===================================================================
       // LoadMultAdj - Load Multiple Adjustment Screen Fields
       //===================================================================
     p LoadMultAdj     b
     d LoadMultAdj     pi

       // Selection criteria for Additional Payee
       SelAddPay = 'ACfnd=' + %Editc(CMfnd:'X') + ' and ACfyr=' +
        %Editc(CMfyr:'X') + ' and ACcase=' + %Editc(CMcase:'X') +
        ' and ACstat <> ''I''';

       // Selection criteria for Other Claims
       SelOClaims = 'CMss# = ' + '''' + CMss# + '''' + ' and CMcase <>' +
        %Editc(CMcase:'X');

       // Selection criteria for Other Claims
       If PDfein <> 0;
          SelPayeeCh = 'PDfein = ' + %Editc(PDfein:'X') + ' and PDstat <> +
                        ''I''';
       Else;
          SelPayeeCh = 'PDstat <> ''I''';
       Endif;


       //scDescOff = PAtrcd + ' - ' + PAdesc;
       //scPayCd = 'CA';              // Hardcoded as of now.
       //scPayCdDsc = scDescOff;      // as offset Description.

       //*** Beg Add *** 10/29/24 *********************************
       dsbPayee1 = *off;
       If inmode ='C' and scPayee <> *zeros;
          dsbPayee1 = *on;
       Endif;
       //*** End Add *** 10/29/24 *********************************

       // Change in Payee2
       Clear scPayNam2;
       Clear scPayAdr12;
       Clear scPayAdr22;
       Clear scPayCsz2;

       Select;
       //*** Beg Chg *** 06/09/25 ******************************************
       //*** When scPayee2 <> 0;
       When scPayee2 <> 0 and scAdPayee2 = *blanks;
       //*** End Chg *** 06/09/25 ******************************************
          taxID#=scPayee2;
          keyFein=fein;
          keyMod=modifier;
          Exsr PayeeAddress;
          scPayNam2  = scPayNamx;
          scPayAdr12 = scPayAdr1x;
          scPayAdr22 = scPayAdr2x;
          scPayCsz2  = scPayCszx;
          scPayCty2  = scPayCty;
          scPaySt2   = scPaySt;
          scPayZip2  = scPayZip;
          scCtry2    = scCtry;
          scAdr32    = scAdr3;

       When scAdPayee2 <> *blanks;
          Chain (CMco#:CMfnd:CMfyr:CMcase:scAdPayee2) pac01;
          Exsr AddPayeeAddress;
          scPayNam2 = scPayNamx;
          scPayAdr12 = scPayAdr1x;
          scPayAdr22 = scPayAdr2x;
          scPayCsz2 = scPayCszx;
          scPayCty2  = scPayCty;
          scPaySt2   = scPaySt;
          scPayZip2  = scPayZip;
          scCtry2    = scCtry;
          scAdr32    = scAdr3;

       When scPayClm2 = 'Y';
          Exsr ClaimantAddress;
          scPayNam2 = scPayNamx;
          scPayAdr12 = scPayAdr1x;
          scPayAdr22 = scPayAdr2x;
          scPayCsz2 = scPayCszx;
          scPayCty2  = scPayCty;
          scPaySt2   = scPaySt;
          scPayZip2  = scPayZip;
          scCtry2    = scCtry;
          scAdr32    = scAdr3;

       When scClaim2 <> 0;
          Exsr NewClaimantAddress;
          scPayNam2 = scPayNamx;
          scPayAdr12 = scPayAdr1x;
          scPayAdr22 = scPayAdr2x;
          scPayCsz2 = scPayCszx;
          scPayCty2  = scPayCty;
          scPaySt2   = scPaySt;
          scPayZip2  = scPayZip;
       Endsl;

       // Change in Payee3
       Clear scPayNam3;
       Clear scPayAdr13;
       Clear scPayAdr23;
       Clear scPayCsz3;

       Select;
       //*** Beg Chg *** 06/09/25 ******************************************
       //*** When scPayee3 <> 0;
       When scPayee3 <> 0 and scAdPayee3 = *blanks;
       //*** End Chg *** 06/09/25 ******************************************
          taxID#=scPayee3;
          keyFein=fein;
          keyMod=modifier;
          Exsr PayeeAddress;
          scPayNam3 = scPayNamx;
          scPayAdr13 = scPayAdr1x;
          scPayAdr23 = scPayAdr2x;
          scPayCsz3 = scPayCszx;
          scPayCty3  = scPayCty;
          scPaySt3   = scPaySt;
          scPayZip3  = scPayZip;
          scCtry3    = scCtry;
          scAdr33    = scAdr3;

       When scAdPayee3 <> *blanks;
          Chain (CMco#:CMfnd:CMfyr:CMcase:scAdPayee3) pac01;
          Exsr AddPayeeAddress;
          scPayNam3 = scPayNamx;
          scPayAdr13 = scPayAdr1x;
          scPayAdr23 = scPayAdr2x;
          scPayCsz3 = scPayCszx;
          scPayCty3  = scPayCty;
          scPaySt3   = scPaySt;
          scPayZip3  = scPayZip;
          scCtry3    = scCtry;
          scAdr33    = scAdr3;

       When scPayClm3 = 'Y';
          Exsr ClaimantAddress;
          scPayNam3 = scPayNamx;
          scPayAdr13 = scPayAdr1x;
          scPayAdr23 = scPayAdr2x;
          scPayCsz3 = scPayCszx;
          scPayCty3  = scPayCty;
          scPaySt3   = scPaySt;
          scPayZip3  = scPayZip;
          scCtry3    = scCtry;
          scAdr33    = scAdr3;

       When scClaim3 <> 0;
          Exsr NewClaimantAddress;
          scPayNam3 = scPayNamx;
          scPayAdr13 = scPayAdr1x;
          scPayAdr23 = scPayAdr2x;
          scPayCsz3 = scPayCszx;
          scPayCty3  = scPayCty;
          scPaySt3   = scPaySt;
          scPayZip3  = scPayZip;
          scCtry3    = scCtry;
          scAdr33    = scAdr3;
       Endsl;

       // Change in Payee4
       Clear scPayNam4;
       Clear scPayAdr14;
       Clear scPayAdr24;
       Clear scPayCsz4;

       Select;
       //*** Beg Chg *** 06/09/25 ******************************************
       //*** When scPayee4 <> 0;
       When scPayee4 <> 0 and scAdPayee4 = *blanks;
       //*** End Chg *** 06/09/25 ******************************************
          taxID#=scPayee4;
          keyFein=fein;
          keyMod=modifier;
          Exsr PayeeAddress;
          scPayNam4 = scPayNamx;
          scPayAdr14 = scPayAdr1x;
          scPayAdr24 = scPayAdr2x;
          scPayCsz4 = scPayCszx;
          scPayCty4  = scPayCty;
          scPaySt4   = scPaySt;
          scPayZip4  = scPayZip;
          scCtry4    = scCtry;
          scAdr34    = scAdr3;

       When scAdPayee4 <> *blanks;
          Chain (CMco#:CMfnd:CMfyr:CMcase:scAdPayee4) pac01;
          Exsr AddPayeeAddress;
          scPayNam4 = scPayNamx;
          scPayAdr14 = scPayAdr1x;
          scPayAdr24 = scPayAdr2x;
          scPayCsz4 = scPayCszx;
          scPayCty4  = scPayCty;
          scPaySt4   = scPaySt;
          scPayZip4  = scPayZip;
          scCtry4    = scCtry;
          scAdr34    = scAdr3;

       When scPayClm4 = 'Y';
          Exsr ClaimantAddress;
          scPayNam4 = scPayNamx;
          scPayAdr14 = scPayAdr1x;
          scPayAdr24 = scPayAdr2x;
          scPayCsz4 = scPayCszx;
          scPayCty4  = scPayCty;
          scPaySt4   = scPaySt;
          scPayZip4  = scPayZip;
          scCtry4    = scCtry;
          scAdr34    = scAdr3;

       When scClaim4 <> 0;
          Exsr NewClaimantAddress;
          scPayNam4 = scPayNamx;
          scPayAdr14 = scPayAdr1x;
          scPayAdr24 = scPayAdr2x;
          scPayCsz4 = scPayCszx;
          scPayCty4  = scPayCty;
          scPaySt4   = scPaySt;
          scPayZip4  = scPayZip;
          scCtry4    = scCtry;
          scAdr34    = scAdr3;
       Endsl;

       // Determine Reserve Type
       Chain (CMco#:scPayCd) ppc01;
       If %Found(WTABL);
          Chain (intCo#:PCreCd) prt01;
          If %Found(WTABL);
             scRsvType = RTtype;
             scReTp    = RTtype;
          Endif;
       Endif;
       If scPayAdr1 = *blanks;
          topAdr2 = 120;
       Else;
          topAdr2 = 135;
       Endif;
       If scPayAdr2 = *blanks;
          topCsz = 135;
       Else;
          topCsz = 150;
       Endif;

       // Address 2
       If scPayAdr12 = *blanks;
          topAdr22 = 135;
       Else;
          topAdr22 = 150;
       Endif;
       If scPayAdr22 = *blanks;
          topCsz2 = 150;
       Else;
          topCsz2 = 165;
       Endif;

       // Address 3
       If scPayAdr13 = *blanks;
          topAdr23 = 135;
       Else;
          topAdr23 = 150;
       Endif;
       If scPayAdr23 = *blanks;
          topCsz3 = 150;
       Else;
          topCsz3 = 165;
       Endif;

       // Address 4
       If scPayAdr14 = *blanks;
          topAdr24 = 135;
       Else;
          topAdr24 = 150;
       Endif;
       If scPayAdr24 = *blanks;
          topCsz4 = 150;
       Else;
          topCsz4 = 165;
       Endif;
       //*** Beg Add *** 12/04/23 **************************************
       // Pay claimant has changed.  On screen re-load set focus to this field
       If scPayClm2 <> *Blanks and scPayClm2 <> payClm2Old;
          focPayClm2 = *on;
          payClm2Old = scPayClm2;
       Else;
          focPayClm2 = *off;
       Endif;
       If scPayClm3 <> *Blanks and scPayClm3 <> payClm3Old;
          focPayClm3 = *on;
          payClm3Old = scPayClm3;
       Else;
          focPayClm3 = *off;
       Endif;
       If scPayClm4 <> *Blanks and scPayClm4 <> payClm4Old;
          focPayClm4 = *on;
          payClm4Old = scPayClm4;
       Else;
          focPayClm4 = *off;
       Endif;

       // PV1 has changed.  On screen re-load set focus to this field
       If scPv12 <> *Blanks and scPv12 <> pv12Old;
          focPv12 = *on;
          pv12Old = scPv12;
       Else;
          focPv12 = *off;
       Endif;
       If scPv13 <> *Blanks and scPv13 <> pv13Old;
          focPv13 = *on;
          pv13Old = scPv13;
       Else;
          focPv13 = *off;
       Endif;
       If scPv14 <> *Blanks and scPv14 <> pv14Old;
          focPv14 = *on;
          pv14Old = scPv14;
       Else;
          focPv14 = *off;
       Endif;

       // Additional Payee has changed.  On screen re-load set focus to this fie
       If scAdPayee2 <> *Blanks and scAdPayee2 <> adPay2Old;
          focAdPay2 = *on;
          adPay2Old = scAdPayee2;
       Else;
          focAdPay2 = *off;
       Endif;
       If scAdPayee3 <> *Blanks and scAdPayee3 <> adPay3Old;
          focAdPay3 = *on;
          adPay3Old = scAdPayee3;
       Else;
          focAdPay3 = *off;
       Endif;
       If scAdPayee4 <> *Blanks and scAdPayee4 <> adPay4Old;
          focAdPay4 = *on;
          adPay4Old = scAdPayee4;
       Else;
          focAdPay4 = *off;
       Endif;

       // Payee has changed.  On screen re-load set focus to this field
       If scPayee2 <> *zeros and scPayee2 <> Payee2Old;
          focPayee2 = *on;
          Payee2Old = scPayee2;
       Else;
          focPayee2 = *off;
       Endif;
       If scPayee3 <> *zeros and scPayee3 <> Payee3Old;
          focPayee3 = *on;
          Payee3Old = scPayee3;
       Else;
          focPayee3 = *off;
       Endif;
       If scPayee4 <> *zeros and scPayee4 <> Payee4Old;
          focPayee4 = *on;
          Payee4Old = scPayee4;
       Else;
          focPayee4 = *off;
       Endif;

       // Claim has changed.  On screen re-load set focus to this field
       If scClaim2 <> *zeros and scClaim2 <> Claim2Old;
          focClm2 = *on;
          Claim2Old = scClaim2;
       Else;
          focClm2 = *off;
       Endif;
       If scClaim3 <> *zeros and scClaim3 <> Claim3Old;
          focClm3 = *on;
          Claim3Old = scClaim3;
       Else;
          focClm3 = *off;
       Endif;
       If scClaim4 <> *zeros and scClaim4 <> Claim4Old;
          focClm4 = *on;
          Claim4Old = scClaim4;
       Else;
          focClm4 = *off;
       Endif;
       //*** End Add *** 12/04/23 **************************************

       Begsr PayeeAddress;
        Clear scPayNamx;
        Clear scPayAdr1x;
        Clear scPayAdr2x;
        Clear scPayCszx;
        Chain (keyFein:keyMod) ppd01;
        If %Found(WMPDL);
           If PDfsn <> *Blanks;
              scPayNamx  = %Trim(PDfsn) + ' ' + %Trim(PDname);
           Else;
              scPayNamx = PDname;
           EndIf;

           If PDdba1 = 'Y';
              scPayAdr1x = PDadr2;
           Else;
              scPayAdr1x = PDadr1;
           EndIf;
           scPayAdr2x= PDadr2;
           strZip    = %Editc(PDzip:'X');
           strZip    = %Subst(strZip:1:5) + '-' + %Subst(strZip:6:4);
           If PDctry <> *blank and PDctry <> 'USA';
              strTbId = %Trim(PDctry);
              Chain (intCo#:'CTRY':strTbId) pgt01;
              If %found(WTGTL);
                 strCtrd  = GTdesc;
              Else;
                 strCtrd  = strNotFnd;
              Endif;
              If scPayAdr2x = *blanks;
                 scPayAdr2x = PDadr3;
                 scPayCszx     = strCtrd;
              Else;
                 scPayCszx     = PDadr3;
              Endif;
           Else;
                 scPayCszx = %Trim(PDcty) + ', ' + %Trim(PDst) + ' ' +
                  %Trim(strZip);
           Endif;
           scPayCty = PDcty;
           scPaySt  = PDst;
           scPayZip = PDzip;
           scCtry   = PDctry;
           scAdr3   = PDadr3;

        Endif;
       Endsr;

       Begsr AddPayeeAddress;
        Clear scPayNamx;
        Clear scPayAdr1x;
        Clear scPayAdr2x;
        Clear scPayCszx;
        If %Found(WDACL1);

           If ACfNam <> *Blanks;
              scPayNamx  = %Trim(ACfNam) + ' ' + %Trim(ACpNam);
           Else;
              scPayNamx = ACpNam;
           EndIf;

           scPayAdr1x = ACadr1;
           scPayAdr2x = ACadr2;
           strZip    = %Editc(ACzip:'X');
           strZip    = %Subst(strZip:1:5) + '-' + %Subst(strZip:6:4);
           If ACctry <> *blank and ACctry <> 'USA';
              strTbId = %Trim(ACctry);
              Chain (intCo#:'CTRY':strTbId) pgt01;
              If %found(WTGTL);
                 strCtrd  = GTdesc;
              Else;
                 strCtrd  = strNotFnd;
              Endif;
              If scPayAdr2x = *blanks;
                 scPayAdr2x = ACadr3;
                 scPayCszx     = strCtrd;
              Else;
                 scPayCszx     = ACadr3;
              Endif;
           Else;
                 scPayCszx = %Trim(ACcty) + ', ' + %Trim(ACst) + ' ' +
                  %Trim(strZip);
           Endif;

           scPayCty = ACcty;
           scPaySt  = ACst;
           scPayZip = ACzip;
           scCtry   = ACctry;
           scAdr3   = ACadr3;
           intDsq#  = ACseq#;

        Endif;
       Endsr;

       Begsr ClaimantAddress;
        Clear scPayNamx;
        Clear scPayAdr1x;
        Clear scPayAdr2x;
        Clear scPayCszx;
        Chain inClmRrn pcm01;
        If %Found(WMCMP);
           Chain (CMss#) pcd01;
           If %found(WMCDL);
             //*** Beg Add *** 05/02/25 **************************
             If CDmid = *Blanks;
              scPayNamx  = %Trim(CDfsn) + ' ' + %Trim(CDlsn);
             Else;
             //*** End Add *** 05/02/25 **************************
              scPayNamx  = %Trim(CDfsn) + ' ' + %Trim(CDmid) + ' ' +
                           %Trim(CDlsn);
             //*** Beg Add *** 05/02/25 **************************
             Endif;
             //*** End Add *** 05/02/25 **************************
              scPayAdr1x = CDadr1;
              scPayAdr2x = CDadr2;
              strZip    = %Editc(CDzip:'X');
              strZip    = %Subst(strZip:1:5) + '-' + %Subst(strZip:6:4);
              If CDctry <> *blank and CDctry <> 'USA';
                  strTbId = %Trim(CDctry);
                 Chain (intCo#:'CTRY':strTbId) pgt01;
                 If %found(WTGTL);
                    strCtrd  = GTdesc;
                 Else;
                    strCtrd  = strNotFnd;
                 Endif;
                 If scPayAdr2x = *blanks;
                    scPayAdr2x = CDadr3;
                    scPayCszx     = strCtrd;
                 Else;
                    scPayCszx     = CDadr3;
                 Endif;
              Else;
                 scPayCszx = %Trim(CDcty) + ', ' + %Trim(CDst) + ' ' +
                  %Trim(strZip);
              Endif;

              scPayCty = CDcty;
              scPaySt  = CDst;
              scPayZip = CDzip;
              scCtry   = CDctry;
              scAdr3   = CDadr3;
           Endif;
        Endif;
       Endsr;

       Begsr NewClaimantAddress;
        Clear scPayNamx;
        Clear scPayAdr1x;
        Clear scPayAdr2x;
        Clear scPayCszx;
        Chain (CMss#) pcd01;
        If %found(WMCDL);
          //*** Beg Add *** 05/02/25 **************************
          If CDmid = *Blanks;
           scPayNamx  = %Trim(CDfsn) + ' ' + %Trim(CDlsn);
          Else;
          //*** End Add *** 05/02/25 **************************
           scPayNamx  = %Trim(CDfsn) + ' ' + %Trim(CDmid) + ' '+ %Trim(CDlsn);
          //*** Beg Add *** 05/02/25 **************************
          Endif;
          //*** End Add *** 05/02/25 **************************

           scPayAdr1x = CDadr1;
           scPayAdr2x = CDadr2;
           strZip    = %Editc(CDzip:'X');
           strZip    = %Subst(strZip:1:5) + '-' + %Subst(strZip:6:4);
           If CDctry <> *blank and CDctry <> 'USA';
              strTbId = %Trim(CDctry);
              Chain (intCo#:'CTRY':strTbId) pgt01;
              If %found(WTGTL);
                 strCtrd  = GTdesc;
              Else;
                 strCtrd  = strNotFnd;
              Endif;
              If scPayAdr2x = *blanks;
                 scPayAdr2x = CDadr3;
                 scPayCszx     = strCtrd;
              Else;
                 scPayCszx     = CDadr3;
              Endif;
           Else;
                 scPayCszx = %Trim(CDcty) + ', ' + %Trim(CDst) + ' ' +
                  %Trim(strZip);
           Endif;

           scPayCty = CDcty;
           scPaySt  = CDst;
           scPayZip = CDzip;
           scCtry   = CDctry;
           scAdr3   = CDadr3;
        Endif;
       Endsr;

     p LoadMultAdj     e
       //===================================================================
       // LoadCorrAdj - Load Correcting Adjustment Screen Fields
       //===================================================================
     p LoadCorrAdj     b
     d LoadCorrAdj     pi

       // Set field positions based on Adr1 or Adr2 = blank
       If scPayAdr1 = *blanks;
          topAdr2 = 435;
       Else;
          topAdr2 = 450;
       Endif;
       If scPayAdr2 = *blanks;
          topCsz = 450;
       Else;
          topCsz = 465;
       Endif;

       // Selection criteria for Additional Payee
       SelAddPay = 'ACfnd=' + %Editc(CMfnd:'X') + ' and ACfyr=' +
        %Editc(CMfyr:'X') + ' and ACcase=' + %Editc(CMcase:'X') +
        ' and ACstat <> ''I''';

       // Population of Offset and Correcting Payment code/Desc
       // Determine Reserve Type
       Chain (CMco#:scPayCd) ppc01;
       If %Found(WTABL);
          Chain (intCo#:PCreCd) prt01;
          If %Found(WTABL);
             scRsvType = RTtype;
             scReTp    = RTtype;
          Endif;
       Endif;

     p LoadCorrAdj     e
       //===================================================================
       // ClearErrors - Clear error fields
       //===================================================================
     p ClearErrors     b
     d ClearErrors     pi

       isInvalid = *off;
       erReTpReq = *off;
       erReTpInd = *off;
       erRecurrRt = *off;
       erPayCdRt = *off;
       erPayCdAut = *off;
       erPayCdTp = *off;
       erAmtLimit = *off;
       erPayTFein = *off;
       erPoDtReq = *off;
       erPoDtInv = *off;
       erPdOtRsv = *off;
       erFrDtReq = *off;
       erFrDtInv = *off;
       erFrDtAcDt = *off;
       erToDtReq = *off;
       erToDtInv = *off;
       erFrDtToDt = *off;
       erDtRcvRsv = *off;
       erDtRcvInv = *off;
       erDtRcvAdt = *off;
       erPyStCdRq = *off;
       erPyTypReq = *off;
       erPyTypMe = *off;
       erPyTypEX = *off;
       erLtFlReq = *off;
       erPv2LtFl = *off;
       erDaysLt7 = *off;
       erEobExist = *off;
       erDtRcvAdt = *off;
       erLmpSumWd = *off;
       erAmtEob = *off;
       erPayCdReq = *off;
       erPcDscReq = *off;
       erPayCdMed = *off;
       erAmtZero = *off;
       erAmtLimit = *off;
       erAmtLimit = *off;
       erAmtRsvLm = *off;
       erPayCdRc = *off;
       erPayCdRc2 = *off;
       erPayCdRc3 = *off;
       erPayCdRc4 = *off;
       erAmtRrsv = *off;
       erAmtRsvCu = *off;
       erAmtClRc = *off;
       erAmtNgRsv = *off;
       erEstRtw = *off;
       erRtwDt = *off;
       erLwDt = *off;
       erRtwDt = *off;
       erAmtGt0 = *off;
       erPayeeReq = *off;
       erStDtReq = *off;
       erStDtInv = *off;
       erStDtAcDt = *off;
       erWkDyCkRq = *off;
       erNumChkGt = *off;
       erNumChk0 = *off;
       erPayCdAr   = *off;
       erDtRreInv  = *off;
       erPrcMChk = *off;
       erPrcMvch = *off;
       erPrcMxml = *off;
       erAcctVch = *off;
       erAcctChk = *off;
       erChk#Req = *off;
       erDaysGtL = *off;
       erAmtRefEx = *off;
       erAmtRefRs = *off;
       erAmtRsNeg = *off;
       erAsWkReq  = *off;
       erFrDtFsDt = *off;
       erPayCdTp1 = *off;
       erPayCdTp2 = *off;
       erPayCdTp3 = *off;
       erPayCdTp4 = *off;
       erPayeeRq1 = *off;
       erPayeeRq2 = *off;
       erPayeeRq3 = *off;
       erPayeeRq4 = *off;
       erPyTypRq2 = *off;
       erPyTypRq3 = *off;
       erPyTypRq4 = *off;
       erLtFlReq2 = *off;
       erLtFlReq3 = *off;
       erLtFlReq4 = *off;
       erPv2LtFl2 = *off;
       erPv2LtFl3 = *off;
       erPv2LtFl4 = *off;
       erPoDtReq2 = *off;
       erPoDtReq3 = *off;
       erPoDtReq4 = *off;
       erPoDtInv2 = *off;
       erPoDtInv3 = *off;
       erPoDtInv4 = *off;
       erOPoDtReq = *off;
       erOPoDtInv = *off;
       erFrDtReq2 = *off;
       erFrDtReq3 = *off;
       erFrDtReq4 = *off;
       erFrDtInv2 = *off;
       erFrDtInv3 = *off;
       erFrDtInv4 = *off;
       erFrDtAcD2 = *off;
       erFrDtAcD3 = *off;
       erFrDtAcD4 = *off;
       erFrDtToD2 = *off;
       erFrDtToD3 = *off;
       erFrDtToD4 = *off;
       erToDtReq2 = *off;
       erToDtReq3 = *off;
       erToDtReq4 = *off;
       erToDtInv2 = *off;
       erToDtInv3 = *off;
       erToDtInv4 = *off;
       erWeeksRq1 = *off;
       erWeeksRq2 = *off;
       erWeeksRq3 = *off;
       erWeeksRq4 = *off;
       erDaysLt71 = *off;
       erDaysLt72 = *off;
       erDaysLt73 = *off;
       erDaysLt74 = *off;
       erODscReq = *off;
       erDscReq2 = *off;
       erDscReq3 = *off;
       erDscReq4 = *off;
       erAmtZero1 = *off;
       erAmtZero2 = *off;
       erAmtZero3 = *off;
       erAmtZero4 = *off;
       erAmtRrsv1 = *off;
       erAmtRrsv2 = *off;
       erAmtRrsv3 = *off;
       erAmtRrsv4 = *off;
       erAmtRsvC1 = *off;
       erAmtRsvC2 = *off;
       erAmtRsvC3 = *off;
       erAmtRsvC4 = *off;

     p ClearErrors     e

       //===================================================================
       // ValidPmt - Validate Panel Screen Fields
       //===================================================================
     p ValidPmt        b
     d ValidPmt        pi

       mgFldReq  = 'This field is required';
       mgDateInv = 'Invalid Date';

       // initialize the error indicator
       isInvalid = *Off;
       isInvalidWdw = *Off;
       //*** Beg Add *** 10/29/24 ***************************************
       isToClmFnd = *off;
       If scClaim <> 0 and scClaim <> scCase and inMode ='C';
          Exec Sql
            Select CMco#,CMfnd,CMfyr,CMemp#,CMdiv,
            CMcase,CMss# Into :decCo#, :decFnd,
            :decFyr,:decEmp#,:decDiv,:decCase,
            :charSS# from WMCMP
            where CMco# = :xxCo#
            and CMfnd = :scFnd
            and CMcase= :scClaim
            and CMss#= :CMss#;
          Chain (decCo#:decFnd:charSS#:decFyr:decCase) pcm03;
          If %found(wmcml2);
             isToClmFnd = *on;
          Else;
             isToClmFnd = *off;
          Endif;
       Endif;
       //*** End Add *** 10/29/24 ***************************************
       //======================================================================
       // Edits from WA011R - Request Mode, Recurring Mode, Manual Mode
       //======================================================================
       // Check to see if Group Master record exists
       Chain (intCo#:intFnd) pfn01;
       If not %Found(WMFNL);
          strMsg = 'Group Master Record does not exist.';
          isInvalid = *on;
          PopOneBtn(strMsg:'Group Error':'Return');
          Return;
       Endif;

       // Check to see if Group Detail record exists
       Chain (intCo#:intFnd) pf101;
       If not %Found(WDF1L);
          strMsg = 'Group Detail Record does not exist.';
          isInvalid = *on;
          PopOneBtn(strMsg:'Group Error':'Return');
          Return;
       Endif;

       // Check group master expiration date
       If %Found(WMFNL);
          sysDt = SystemDate();
          sysDt7 = DateTo7(sysDt);
          If FNexDt > 0 and FNexDt <= sysDt7;
             strMsg = 'This group is expired.';
             isInvalid = *on;
             PopOneBtn(strMsg:'Group Error':'Return');
             Return;
          Endif;
       Endif;

       // Check to see if Group Auxiliary record exists
       Chain (intCo#:intFnd) pf201;
       If not %Found(WDF2L);
          strMsg = 'Group Auxiliary Record does not exist.';
          isInvalid = *on;
          PopOneBtn(strMsg:'Group Error':'Return');
          Return;
       Endif;

       // Check Group Auxiliary Process claims flag
       If %Found(WDF2L) and F2clCk <> 'Y';
          strMsg = 'Group Auxiliary is not setup to Process Claims Payments';
          isInvalid = *on;
          PopOneBtn(strMsg:'Group Error':'Return');
          Return;
       Endif;

       // No User Parameter record.
       Chain (Q1User) pup01;
       If not %found(SMUPL);
          strMsg = 'User is not configured in the system';
          isInvalid = *on;
          PopOneBtn(strMsg:'User Error':'Return');
          Return;
       Endif;

       // If Claim is denied check user security level
       If CMdny = 'Y';
          inUtyp = UPtype;
          inUlvl = UPlvl;
          inCtyp = CXpdCt;
          inClvl = CXpyOd;
          inExec = '1';
          outres = *on;
          inCtyp2 = CXpdC2;
          inCtyp3 = CXpdC3;
          inCtyp4 = CXpdC4;
          inCtyp5 = CXpdC5;
          inCtyp6 = *blank;

          Callp ChkUserAuth(inUtyp:inUlvl:inCtyp:inClvl:inExec:outRes:inCtyp2:
           inCtyp3:inCtyp4:inCtyp5:inCtyp6);
          If outRes = *on;
             strMsg = 'User does not have adequate authority to make +
                payments on a denied claim';
             isInvalid = *on;
             PopOneBtn(strMsg:'Claim Error':'Return');
             Return;
          Endif;
       Endif;

       // If Claim is delayed check user security level
       If CMdly = 'Y';
          inUtyp = UPtype;
          inUlvl = UPlvl;
          inCtyp = CXDs01;
          inClvl = CXdsLv;
          inExec = '1';
          outRes = *off;
          inCtyp2 = CXpdC2;
          inCtyp3 = CXpdC3;
          inCtyp4 = CXpdC4;
          inCtyp5 = CXpdC5;
          inCtyp6 = *blank;

          Callp ChkUserAuth(inUtyp:inUlvl:inCtyp:inClvl:inExec:outRes:inCtyp2:
           inCtyp3:inCtyp4:inCtyp5:inCtyp6);
          If outRes = *on;
             strMsg = 'User does not have adequate authority to make +
                payments on a delayed claim';
             isInvalid = *on;
             PopOneBtn(strMsg:'Claim Error':'Return');
             Return;
          Endif;
       Endif;

       //======================================================================
       // End WA011R Edits - Request Mode, Recurring Mode
       //======================================================================

       //======================================================================
       // Edits from WA012R - Request Mode, Recurring Mode, Manual Mode
       //======================================================================

       // If record only dont allow payments
       If CMltFl = 'R' and inMode <> 'R';
          strMsg = 'Payments are not allowed on Record Only claims';
          isInvalid = *on;
          PopOneBtn(strMsg:'Claim Error':'Return');
          Return;
       Endif;

       // No Claims/DARTS control record.
       Chain (intCo#:intFnd) pcx01;
       If not %found(WMCXL);
          strMsg = 'Claims/DARTS control record does not exist.';
          isInvalid = *on;
          PopOneBtn(strMsg:'Claim/DARTS Error':'Return');
          Return;
       Endif;

       // Payee is required
       //**** Beg Chg **** 05/22/25 ******************************************
       //***If scPayClm = *blank and scAdPayee = *blanks and scPayee = *zeros;
       If scPayClm = *blank and scAdPayee = *blanks and scPayee = *zeros and
        inMode <> 'A';
       //**** End Chg **** 05/22/25 ******************************************
          mgPayeeReq = 'A Payee must be selected';
          isInvalid = *on;
          erPayeeReq = *on;
       Endif;

       // Reserve type is required
       If scReTp = *blank and inMode <> 'A' and inMode <> 'C';
          isInvalid = *on;
          erReTpReq = *on;
       Endif;

       // Reserve type 'C' is only valid for Indemnity claims
       Chain (intCo#:scPayCd) ppc01;
       If %Found(WTABL) and PCtrTp <> 'Z' and PCtrTp <> 'S' and PCtrTp <> 'V';
          Chain (intCo#:PCreCd) prt01;
          If %Found(WTABL) and RTtype = 'C' and CMltFl <> 'I';
             mgReTpInd = 'Reserve type ''C'' is only valid for Indemnity +
              Claims';
             isInvalid = *on;
             erReTpInd = *on;
          Endif;
       Endif;

       // Reserve type must be 'C' for recurring payments
       Chain (intCo#:scPayCd) ppc01;
       If %Found(WTABL);
          Chain (intCo#:PCreCd) prt01;
          If %Found(WTABL) and scRecurr = 'Y' and RTtype <> 'C';
             mgRecurrRt = 'Reserve type must be ''C'' for recurring +
              payments';
             isInvalid = *on;
             erRecurrRt = *on;
          Endif;
       Endif;

       // If claim is Contrverted only allow payments to be released if the
       // reserve type is equal to one of the values defined in the Claims/DARTS
       // Control file values
       Chain (intCo#:scPayCd) ppc01;
       If %Found(WTABL);
          If (PCtrTp = 'M' or PCtrTp = 'P') and CMcvFl = 'Y';
             If %Lookup(scReTp:arrRt) = *zero;
                mgPayCdRt = 'Controverted Claim Reserve type is not defined +
                 in Claim/DARTS Control record.';
                isInvalid = *on;
                erPayCdRt = *on;
             Endif;
          Endif;
       Endif;

       // User must have adequate authority to use the payment type code.
       inUtyp = UPtype;
       inUlvl = UPlvl;
       inCtyp = PCty01;
       inClvl = PClvl;
       inExec = '1';
       outres = *off;
       inCtyp2 = PCty02;
       inCtyp3 = PCty03;
       inCtyp4 = PCty04;
       inCtyp5 = PCty05;
       inCtyp6 = *blank;

       Callp ChkUserAuth(inUtyp:inUlvl:inCtyp:inClvl:inExec:outRes:inCtyp2:
        inCtyp3:inCtyp4:inCtyp5:inCtyp6);
       If outRes = *on;
          mgPayCdAut = 'User does not have adequate authority for the payment +
           type code being used.';
          isInvalid = *on;
          erPayCdAut = *on;
       Endif;

       // FEIN is required when reserve type is not 'C'.
       //*** Beg Add ***** 11/17/25 ***************************
       scRsvType = scReTp;
       //*** End Add ***** 11/17/25 ***************************
       If scReTp <> 'C' and scPayee <= 0 and inMode <> 'R' and inMode <> 'S';
          strMsg = 'FEIN/mod is required when reserve type is not ''C''.  +
           Press Override to continue or Return to make corrections.';
          isOvr = PopTwoBtn(strMsg:'FEIN Required':'Override':'Return');
          If Not isOvr and isInvalidWdw = *off;
             isInvalidWdw = *on;
             Return;
           Else;
             isInvalidWdw = *off;
          Endif;
       Endif;

       //======================================================================
       // End Edits from WA012R - Request Mode, Recurring Mode
       //======================================================================

       // ORM and TPOC date must be accurate for Medicare Beneficiary
       // Request and Recurring
       If cltId = '133' and CDmdBn = 'Y' and CMstat = 'C';
          strMsg = 'You have processed a payment on a closed claim that has a +
           Medicare Benefit flag of yes; you must verify that the ORM +
           and/or TPOC dates are accurate.';
          isVer = PopTwoBtn(strMsg:'Medicare Benefit':'Dates Verified':
             'Return');
          If Not isVer and isInvalidWdw = *off;
             isInvalidWdw = *on;
             Return;
          Else;
             isInvalidWdw = *off;
          Endif;
       Endif;

       // Transaction Type must be payment type.
       // Request only
       If scType = 'P' and inMode <> 'A' and inMode <> 'C'  and inMode <> 'R'
          and inMode <> 'T' and inMode <> 'S';
          Chain (intCo#:scPayCd) ppc01;
          If %Found(WTABL) and PCtrTp <> 'P';
             mgPayCdTp = 'Transaction Type must be payment type.';
             isInvalid = *on;
             erPayCdTp = *on;
          Endif;
       Endif;

       // If case is dismissed or settle so no payments are allowed
       // Common
       If CMstat = 'C';
          Chain (CMco#:CMfnd:CMfyr:CMcase) pll01;
          If %Found(WMLLL);
            keyTbId = CMjrSt + 'MS';
            Chain (CMco#:keyTbid:llmtSt) pgt01;
            If Not %found(WTGTL);
               keyTbId = '  MS';
               Chain (CMco#:keyTbId:llmtSt) pgt01;
            Endif;
            If %Found(WTGTL) and GTfg01 = 'Y';
               strMsg = 'Case is dismissed or settled, payments are not +
                allowed.';
               isInvalid = *on;
               PopOneBtn(strMsg:'Legal Error':'Return');
               Return;
            Endif;
          Endif;
       Endif;

       // Validation Check for weeks and days
       // Comp Request NOT Recurring, Manual Comp
       If scRecurr <> 'Y';
       // Days must be less than 7
          If scDays >= 7;
             isInvalid = *on;
             mgDaysLt7 = 'Number of days must be less than 7';
             erDaysLt7 = *on;
          Endif;

          Chain (intCo#:scPv1) pb201;
          If %Found(WTABL);
             If B2exWdC = 'Y';
                scWeeks = 0;
                scDays  = 0;
             Else;
                If RTtype = 'C';
                   svWeeks = scWeeks;
                   svDays  = scDays;
                   inFrDt8 = scFrDt;
                   inToDt8 = scToDt;
                   isVldDays = *on;
                   Monitor;
                   intDays = dtDiff(inFrDt8:inToDt8);
                   On-error;
                      isVldDays = *off;
                   Endmon;
                   If not isDupl;
                      scWeeks = 0;
                      scDays  = 0;
                   Endif;
                   If isVldDays;
                      intDays = intDays + 1;
                      scWeeks = %Div(intDays:7);
                      scDays = %Rem(intDays:7);
                   Endif;
                   If (svWeeks <> scWeeks) or (svDays <> scdays);
                      strMsg = 'The weeks and days entered do not correspond +
                       to the dates of service. Do you want to override?';
                      isOvr = PopTwoBtn(strMsg:'Weeks/Days':'Override':
                       'Return');
                      If isOvr;
                         scWeeks = svWeeks;
                         scDays  = svDays;
                         isInvalidWdw = *off;
                      Else;
                         isInvalidWdw = *on;
                         Return;
                      Endif;
                   Endif;
                Endif;
             Endif;
          Endif;
       Endif;

       // If the PV1 from the table is a "U" and the FEIN is not zero
       // then it is an error.
       // Common
       Chain (intCo#:scPv1) pb201;
       If %Found(WTABL);
          If B2plCd = 'U' and Fein > 0;
             If PDtype <> '5';
                isInvalid = *on;
                mgPayTFein = 'Payment Value 1 must have provider type of ''5''';
                erPayTFein = *on;
             Endif;
          Endif;
       Endif;

       // Check to see if negative fees are allowed
       allowNegFees = *Off;
       If IsModEnabled('Negative Fee Checks');
          allowNegFees = *On;
       EndIf;

       // Offset Posting date is required and must be valid (For Multi and Corre
       If inMode = 'A' or inMode = 'C';
          If scPoDtOff = *zeros;
             isInvalid = *on;
             erOPoDtReq = *on;
          Else;
             If dtCheck(scPoDtOff) = 0;
                isInvalid = *on;
                erOPoDtInv = *on;
             Endif;
          Endif;
       Endif;

       // Posting date is required and must be valid
       // Request NOT Recurring
       If scRecurr <> 'Y';
          If scPoDt = *zeros;
             isInvalid = *on;
             erPoDtReq = *on;
          Else;
             If dtCheck(scPoDt) = 0;
                isInvalid = *on;
                erPoDtInv = *on;
             ENDIF;
          Endif;

          // Validate Paid on Time code (late code)
          // Request NOT Recurring.  Not Recovery. Not adjustments.
          If scReTp = 'C' and inMode <> 'R' and inMode <> 'T' and
           inMode <> 'S' and inMode <> 'A' and inMode <> 'C' and
           scPdOnTm = *blank;
                isInvalid = *on;
                mgPdOtRsv = 'Paid on Time must be checked when Reserve +
                 type is Claims';
                erPdOtRsv = *on;
          Endif;

          // From Service Date is required and must be valid
          // Request NOT Recurring
          If scFrDt = *zeros;
             isInvalid = *on;
             erFrDtReq = *on;
          Else;
             If dtCheck(scFrDt) = 0;
                isInvalid = *on;
                erFrDtInv = *on;
             ENDIF;
          Endif;

          // From Service date must be greater than or equal Claim Accident date
          // Request NOT Recurring
          If dtCYMD(scFrDt) < CMacDt;
             isInvalid = *on;
             mgFrDtAcDt = 'From Service Date must be greater than or equal to +
              the Accident date';
             erFrDtAcDt = *on;
          Endif;

          // To Service Date is required and must be valid
          // Request NOT Recurring
          If scToDt = *zeros;
             isInvalid = *on;
             erToDtReq = *on;
          Else;
             If dtCheck(scToDt) = 0;
                isInvalid = *on;
                erToDtInv = *on;
             Endif;
          Endif;

          // From Service may not be greater than To Service Date.
          // Request NOT Recurring
          If dtCYMD(scFrDt) > dtCYMD(scToDt) and scToDt <> *zeros;
             isInvalid = *on;
             mgFrDtToDt = 'From Service Date may not be greater than To +
              Service Date';
             erFrDtToDt = *on;
          Endif;

          // Received Date must be a valid date.
          // Request Not Recur not single adjustment and not multiple adjustment
          If inMode <> 'S' and inMode <> 'A' and inMode <> 'C';
             If scReTp = 'M' and scDtRcv = *zeros;
                isInvalid = *on;
                mgDtRcvRsv = 'Received Date is required when Reserve Type is +
                 Medical';
                erDtRcvRsv = *on;
             Else;
                If dtCheck(scDtRcv) = 0 and inMode <> 'A' and inMode <> 'C';
                   isInvalid = *on;
                   erDtRcvInv = *on;
                Endif;
             Endif;

             // Received Date must be greater than or equal Claim Accident date.
             // Request NOT Recurring
             If dtCYMD(scDtRcv) < CMacDt and inMode <> 'A' and inMode <> 'C';
                isInvalid = *on;
                mgDtRcvAdt = 'Received Date must be greater than or equal to +
                 the Accident date';
                erDtRcvAdt = *on;
             Endif;
          Endif;
       Endif;

       // Payment Status Code is required
       // Request Not Manual
       If scType <> 'M' and scPayStCd = *blanks and inMode <> 'A' and
        inMode <> 'C' and inMode <> 'R' and inMode <> 'T' and inMode <> 'S';
          isInvalid = *on;
          erPyStCdRq = *on;
       Endif;

       // Payment Type (PV1) is required
       // Common
       If scPv1 = *blanks;
          isInvalid = *on;
          erPyTypReq = *on;
       Endif;

       // If CA and Payment Type (PV1) has Cost Containment flag set
       // Common
       Chain (intCo#:scPv1) pb201;
       If %Found(WTABL) and B2ccw = 'Y' and CMjrSt = 'CA';
          Select;
          When ELpfDt < 1100701 and PCreCd <> 'ME';
             isInvalid = *on;
             erPyTypMe = *on;
             mgPyTypMe = 'Cost Containment flag is set for this CA claim +
              and the reserve code is not ME';
          When ELpfDt >= 1100701 and PCreCd <> 'EX';
             isInvalid = *on;
             erPyTypEX = *on;
             mgPyTypEX = 'Cost Containment flag is set for this CA claim +
              and the reserve code is not EX';
          Endsl;
       Endif;

       // Payment Value 2 is required
       // Common
       If scPv2 = *blanks;
          isInvalid = *on;
          erLtFlReq = *on;
       Endif;

       // Payment Value 2 must be compatible with Claim Lost Time Status
       // Common
       If scPv2 <> *blanks;
          Chain (intCo#:scPv2) pb701;
          If %Found(WTABL);
             //**** Beg Chg **** 05/22/25 *********************************
             //***If (CMltFl = 'I' and B7LtLf <> 'Y') or
             //****(CMltFl = 'M' and B7ltLf <> 'N') or
             //****(CMltFl = 'R' and B7ltLf <> 'N');
             If (L3_CMltFl = 'I' and B7LtLf <> 'Y') or
              (L3_CMltFl = 'M' and B7ltLf <> 'N') or
              (L3_CMltFl = 'R' and B7ltLf <> 'N');
             //**** End Chg **** 05/22/25 *********************************
                isInvalid = *on;
                mgPv2LtFl = 'Payment Value 2 is incompatible with Claim +
                 Lost Time Flag';
                erPv2LtFl = *on;
             Endif;
          Endif;
       Endif;

       // Validate EOB Number if entered
       // Request Medical Payment Only
       // Not recurring
       If scRecurr <> 'Y';
          If scEob# > 0 and isWCMC = *on and isWCMCintFc = *on;
             inRcn#    = scRcn#;
             inEob#Int = scEob#;
             Callp EobExists(inCo#int:inFndInt:inFyrInt:inCaseInt:inEob#Int:
              inRcn#:outRtCd);
             If outRtCd <> *blanks;
                isInvalid = *on;
                mgEobExist = 'EOMB does not exist or the recon entered is not +
                 at the current recon level.';
                erEobExist = *on;
             Endif;
          Endif;

          // From Service Date must be greater than or equal Claim Full Salary
          // Paid Through Date.
          // Request NOT recurring and not recovery
          If (CMjrSt <> 'TX' and CMfSal <> 'Y') or inMode <> 'R';
             If dtCYMD(scfrDt) < CMfsDt;
                isInvalid = *on;
                mgFrDtFsDt = ' From Service date must be greater than or equal +
                 Claim "Full Salary Paid Through Date".';
                erFrDtFsDt = *on;
             Endif;
          Endif;


          // Payment Value 1 does not have exclude from weeks/days checked
          If scLmpSum = 'Y' and isIDC = *on and (scPayCd = 'CM' or
           scPayCd = 'CP');
             Chain (intCo#:scPv1) pb201;
             If %Found(WTABL) and B2exwdc <> 'Y';
                isInvalid = *on;
                MgLmpSumWd = 'Payment Value 1 code being used does not have +
                 the Exclude from weeks/days checked';
                erLmpSumWd = *On;
             Endif;
          Endif;

          // EOB paid amount does not match paid amount on payment screen
          // Not Recovery
          If isWCMC and inMode <> 'R';
             Callp getSub#Fnd(outRtCd:inCo#Chr:inFndChr:outSub#Chr:outFndChr);
             inSub#Int = %Dec(outSub#Chr:5:0);
             Chain (CMco#:inSub#Int:CMfnd:CMfyr:CMcase:scEob#:scRcn#) pea01;
             If %Found(FAEAL1) and scAmount <> EApaid;
                isInvalid = *on;
                mgAmtEob = 'Payment does not equal EOB paid amount. Contact +
                 software vendor';
                erAmtEob = *On;
             Endif;
          Endif;
       Endif;

       // Check for Auto Calculate of Comp Amount
       // Request and recurring
       If (cltId <> '114' and not allowNegFees and scAmount <= 0) or
        scAmount = 0;
          If scRecurr = 'Y';
             scWeeks = scWeeksChk;
             scDays  = scDaysChk;
          Endif;
          If scReTp = 'C' and (scWeeks > 0 or scDays > 0);
             inWeeks = %Editc(scweeks:'X');
             inDays  = %Editc(scDays:'X');
             outAmount = scAmount;
             Callp AutoCalcComp(inCo#Chr:inFndChr:inFyrChr:inCaseChr:
              inWeeks:indays:inPv1:outAmount);
             If outAmount > 0;
                scAmount = outAmount;
                decAmount = scAmount;
             Else;
                outAmount = 0;
                decAmount = 0;
             Endif;
          Endif;
       Endif;
       // Amount must be greater than 0
       // Common
       If scAmount = 0;
          isInvalid = *on;
          mgAmtGt0 = 'This field cannot equal 0';
          erAmtGt0 = *on;
       Endif;

       // Check for duplicate payment/missing invoice number (114 and 152 only)
       // Get the next available sequence number
       Setgt (CMco#:CMfnd:CMfyr:CMcase) prp01;
       Readpe(n) (CMco#:CMfnd:CMfyr:CMcase) prp01;
       If %Eof(WARPL);
          intPaySeq# = 1;
       Else;
          intPaySeq# = RPseq# + 1;
       Endif;
       inSeq# = intPaySeq#;

       // Get PV1 Reserve Code and reserve type
       Chain (intCo#:scPayCd) ppc01;
       If %Found(WTABL);
          Chain (intCo#:PCreCd) prt01;
       Endif;

       If cltId = '114' or cltId = '152';
          If (RTtype = 'E' and fein <> 0) or (RTtype = 'L' and fein <> 0) or
           RTtype = 'M' and PCreCd = 'RE' and fein <> 0;
             inFein = fein;
             inMod  = modifier;
             inInv# = scInv#;
             Callp chkDupPmntInv#(inCo#Int:inFndInt:inFyrInt:inCaseInt:
              inSeq#:inFein:inMod:inInv#:inFkey);
          Endif;
       Endif;

       // Payment Code and description are required
       // Common
       If scPayCd = *blanks;
          isInvalid = *on;
          mgPayCdReq = 'This field cannot be blank';
          erPayCdReq = *on;
       Endif;
       If scPayCdDsc = *blanks;
          isInvalid = *on;
          mgPcDscReq = 'This field cannot be blank';
          erPcDscReq = *on;
       Endif;

       // Check the "Exclude Medical" flag
       Chain (intCo#:scPayStCd) pps01;
       If scReTp = 'M' and ELexMf = 'Y' and PStype = 'R';
          isInvalid = *on;
          mgPayCdMed = 'Medical information cannot be entered for the +
             associated Employer/Policy year';
          erPayCdMed = *on;
       Endif;

       If scRecurr = 'Y';
          // If the Auto Release flag in the Payment Control file is 'Y',
          // the Reserves must be adequate for ALL Future Payments.
          // Recurring only
          If PCarFl = 'Y';
             decAmount = scAmount * scNumFChk;
          Else;
             decAmount = scAmount;
          Endif;

          // Assign Start Date to other date fields not requiring edits
          scPoDt = scStartDt;
          scFrDt = scStartDt;
          scToDt = scStartDt;

          // Cannot release payment if Auto Release flag = 'N;
          Chain (CMco#:scPayStCd) pps01;
          If %Found(WTABL) and PSstat <> 'I';
             If PCarFl = 'N' and (PStype = 'R' or PStype = ' ');
             mgPayCdAr = 'Cannot release this payment because the Auto +
              Release flag is "N"';
             isInvalid = *on;
             erPayCdAr = *on;

             Endif;
          Endif;
       Endif;

       //  Claim/Payment/Reserves Error Checking  - WA013R

       //  If the Payment Status "Status" is blank ("Released"), & we have
       //  passed previous edits, execute Claim/Payment/Reserve Edits

       // Payment status is released..lets do some editing. Not Recovery.
       If (PStype = *blank or PStype = 'R' or
        (inMode = 'S' and scAsAmtWd = 'A')) and inMode <> 'R';
          If scRecurr <> 'Y';
             decAmount = scAmount;
          Endif;

          // Payment amount must be > 0 unless allowing negative fees
          If decAmount <= 0 and allowNegFees = *off;
             Select;
             When cltID <> '114';
                isInvalid = *on;
                mgAmtZero = 'Amount must be > 0';
                erAmtZero = *on;
             Endsl;
          Endif;

          // Check users payment limit
          If decAmount > UPpaL;
             isInvalid = *on;
             mgAmtLimit = 'Payment amount exceeds users payment authority +
              limit';
             erAmtLimit = *on;
          Endif;

          // Accumulate users previous reserve amount and compare to users limit
          If CMstat = 'C';
             inReCd = PCreCd;
             Callp ReserveLimit(inCo#Int:inFndInt:inFyrInt:inCaseInt:
              inReCd:outRsvAutL);
             If decAmount > outRsvAutL;
                isInvalid = *on;
                mgAmtRsvLm = 'Reserve change exceeds reserve authority limit. +
                 The total of reserve changes entered is greater than the +
                 reserve authority limit for the current user as specified in +
                 User Configuration';
                erAmtRsvLm = *on;
             Endif;
          Endif;

          // First Load an array with the reserve code and amt of the pmt.  If
          // an EOMB is attached to the pmt, load the rsv codes and amt
          // of all associated pmts.
          LoadRsvArr();

          // Reserve Code for the Payment Code must be valid for claim
          For x = 1 to 10;
             If arrRsvCode(x) <> *blanks;
                If %Lookup(arrRsvCode(x):dsClaimRsvTyp:1) = 0;
                   isInvalid = *on;
                   mgPayCdRc = 'Reserve code for the payment code is invalid +
                    for the claim';
                   erPayCdRc = *on;
                   Leave;
                Endif;
             Endif;
          Endfor;

          // If the Claim is Open, the Released Amount cannot be greater
          // than Remaining Reserve.  Not Recovery or Multiple Adjustments.
          //*** Beg Chg *** 08/08/25 *********************************
          // If CMstat <> 'C' and inMode <> 'R' and inMode <> 'A' and
          // inMode <> 'C';
          If CMstat <> 'C' and inMode <> 'R' and inMode <> 'A' and
           inMode <> 'C' and inMode <> 'T';
          //*** End Chg *** 08/08/25 *********************************
             ClaimRemRsv = 0;
             x = 1;
             For x = 1 to 10;
                If arrRsvCode(x) <> *blanks;
                   y = %Lookup(arrRsvCode(x):dsClaimRsvTyp:1);
                   if y > 0;
                      claimRemRsv = dsClaimRemRsv(y);
                   //*** Beg Add *** 09/10/25 ****************************
                      claimRsvTyp = dsClaimRsvTyp(y);
                   //*** End Add *** 09/10/25 ****************************
                   Else;
                      claimRemRsv = 0;
                   Endif;
                   //*** Beg Chg *** 02/28/25 ****************************
                   //*If  decAmount > claimRemRsv;
                   If  decAmount > claimRemRsv and inMode <> 'R' and
                    inMode <> 'T';
                   //*** End Chg *** 02/28/25 ****************************
                      isInvalid = *on;
                      mgAmtRrsv = 'The released payment amount is greater than +
                       the remaining reserve for the claim';
                      erAmtRrsv = *on;
                      Leave;
                   Endif;
                   //*** Beg Add *** 09/10/25 ****************************
                   //*** Need check for pending payments total here
                   xxPPmt = 0;
                   Chain(N) (CMco#:CMfnd:CMfyr:CMcase) prp01;
                   If %found();
                      Dow Not %Eof();
                         If RPstat = *blanks and RPrecd = ClaimRsvTyp;
                               xxPPmt += RPamt;
                         Endif;
                      ReadE(N) (CMco#:CMfnd:CMfyr:CMcase) prp01;
                      Enddo;
                      claimRemRsv = claimRemRsv - xxPPmt;
                     If  decAmount > claimRemRsv and inMode <> 'R' and
                      inMode <> 'T';
                      isInvalid = *on;
                      mgPndRsv = 'The released payment amount is greater than +
                       the remaining reserve and pending payments for the +
                       claim';
                      erPndRsv = *on;
                      Leave;
                     Endif;
                   Endif;
                   //*** End Add *** 09/10/25 ****************************
                Endif;
             Endfor;
          Endif;

          // If the claim is closed, don't allow check release if
          // there is no entry in the reserve cushion file for the reserve
          // code and claim type (indemnity vs medical only). Not Recovery.
       If CMstat = 'C';
             decRsvCushion = 0;
             Setgt (CMco#:CMfnd:CMfyr:CMcase) pch01;
             Readpe (CMco#:CMfnd:CMfyr:CMcase) pch01;
             If not %Eof(WMCHL) and CHocSt = 'C';
                Setgt (CMco#:CMfnd:scReCd:CHefDt) prc02;
                Readpe (CMco#:CMfnd:scReCd) prc02;
                If not %Eof(WDRCL);
                   Select;
                   When CMltFl = 'I';
                      decRsvCushion = RClAmt;
                   When CMltFl = 'M';
                      decRsvCushion = RCmAmt;
                   Endsl;
                Endif;
                If decRsvCushion = 0;
                   isInvalid = *on;
                   mgAmtRsvCu = 'Reserve Cushion for Reserve Code & Claim +
                      Type is not valid';
                   erAmtRsvCu = *on;
                Endif;
             Endif;

          Endif;

          // If the claim is closed:
          // Accumulate all WDPAP transactions with the same reserve code
          // and a transaction date > the latest claim closing date.
          // Add any request for payments (WARPP) which are currently
          // released for the same reserve code including the request you
          // are attempting to release.
          // If this amount exceeds the reserve cushion, send an error msg.
          If CMstat = 'C';

          // Accumulate Payments made into Summed  Released Payments and
          // EOB # Pended Payments.
             Setgt (CMco#:CMfnd:CMfyr:CMcase:CHefDt) ppa04;
             //*** Beg Chg *** 11/22/24 ************************************
             //***Readpe(n) (CMco#:CMfnd:CMfyr:CMcase) ppa04;
             Reade(n) (CMco#:CMfnd:CMfyr:CMcase) ppa04;
             //*** End Chg *** 11/22/24 ************************************
             Dow not %Eof(WDPAL5);
                x = %Lookup(PAreCd:arrRsvCode:1);
                If x > 0;
                   If PAtype = 'P' or PAtype = 'M' or PAtype = 'A' or
                    PAtype = 'Z';
                      arrRemRsv(x) = arrRemRsv(x) + PAamt;
                   Else;
                      arrRemRsv(x) = arrRemRsv(x) - PAamt;
                   Endif;
                Endif;
                //*** Beg Chg *** 11/22/24 *********************************
                //***Readpe(n) (CMco#:CMfnd:CMfyr:CMcase) ppa04;
                Reade(n) (CMco#:CMfnd:CMfyr:CMcase) ppa04;
                //*** End Chg *** 11/22/24 *********************************
             Enddo;

             // Loop through the amounts and if the amount is greater than
             // the reserve cushion, return with error code.
             For x = 1 to 10;
                //*** Beg Add *** 11/22/24 *********************************
                If arrRsvCode(x) = scReCd;
                //*** End Add *** 11/22/24 *********************************

                   If arrRemRsv(x) > 0;
                      decRsvCushion = 0;
                      strReCd = arrRsvCode(x);
                      Setgt (CMco#:CMfnd:strReCd:CHefDt) prc02;
                      Readpe (CMco#:CMfnd:strReCd) prc02;
                      If not %Eof(WDRCL);
                         Select;
                         When CMltFl = 'I';
                            decRsvCushion = RClAmt;
                         When CMltFl = 'M';
                            decRsvCushion = RCmAmt;
                         Endsl;
                      Endif;
                      If arrRemRsv(x) > decRsvCushion;
                         isInvalid = *on;
                         mgAmtClRc ='Amount exceeds the reserve cushion amount +
                          set up for the payment type. Amount requested plus +
                           prior payments on this closed claim exceeds the +
                           allowable amount set up for the reserve code for +
                           this payment type.';
                         erAmtClRc = *on;
                         Leave;
                      Endif;
                   Endif;
                //*** Beg Add *** 11/22/24 *****************************
                Endif;
                //*** End Add *** 11/22/24 *****************************
             Endfor;
          Endif;

          //*** Beg Add *** 10/29/24 ***************************************
          If scClaim <> 0 and scClaim <> scCase and inMode ='C'
             and isToClmFnd = *on;
             // If To claim is Opened then check remaining reserve of To Claim
             If L3_CMstat <> 'C';
                ClaimRemRsv = 0;
                x = 1;
                For x = 1 to 10;
                   If arrRsvCode(x) <> *blanks;
                      y = %Lookup(arrRsvCode(x):dsToClmRsvTyp:1);
                      if y > 0;
                         claimRemRsv = dsToClmRemRsv(y);
                      Else;
                         claimRemRsv = 0;
                      Endif;
                      If decAmount > claimRemRsv;
                         isInvalid = *on;
                         mgAmtRrsv = 'The released payment amount is greater +
                          than the remaining reserve for the To Claim';
                         erAmtRrsv1= *on;
                         Leave;
                      Endif;
                   Endif;
                Endfor;
             Endif;

             // If To claim is Closed then check for reserve cushion
             If L3_CMstat = 'C';
                decRsvCushion = 0;
                Setgt (decCo#:decFnd:decFyr:decCase) pch01;
                Readpe (decCo#:decFnd:decFyr:decCase) pch01;
                If not %Eof(WMCHL) and CHocSt = 'C';
                   Setgt (decCo#:decFnd:scReCd:CHefDt) prc02;
                   Readpe (decCo#:decFnd:scReCd) prc02;
                   If not %Eof(WDRCL);
                      If decAmount > RClAmt or decAmount > RCmAmt;
                         isInvalid = *on;
                         mgAmtRsvCu = 'Reserve Cushion for Reserve Code & +
                            Claim Type is not valid';
                         erAmtRsvC1 = *on;
                      Endif;
                   Endif;
                Endif;
             Endif;
          Endif;
          //*** End Add *** 10/29/24 ***************************************

          // This edit is only for client 114
          // Allow payments to be released if the total paid, for that
          // reserve, is adequate to prevent a negative total paid amount
          x = %Lookup(scReCd:dsClaimRsvTyp:1);
          If x > 0;
             decRsvBal = dsClaimRemRsv(x);
             decRsvBal = decRsvBal + decAmount;
          Endif;
          If  (allowNegFees = *on or cltId = '114') and decAmount < 0 and
             decRsvBal < 0;
             isInvalid = *on;
             mgAmtNgRsv = 'Negative total amount being paid on the reserve';
             erAmtNgRsv = *on;
          Endif;

       Endif;

       // Return to Work Edits

       // Compare Claimant Auxiliary Return to Work DTs against Service From/To
       // Not recurring
       If scRecurr <> 'Y';
          If scReTp = 'C';
             decFrDt7 = DTcyMd(scFrDt);
             decToDt7 = DTcyMd(scToDt);
             scFrDt10 = AlphaUsa(cvtToDate(scFrDt));
             scToDt10 = AlphaUsa(cvtToDate(scToDt));
             If CMeRtw > 0 and CMeRtw < decToDt7;
                erEstRtw = *on;
                scErtwDt10 = AlphaUsa(cvtToDate(DtMdyy(CMeRtw)));
             Endif;

             decRtwDt = CMrtw;
             // Check date in work history
             Setgt (CMco#:CMfnd:CMfyr:CMcase:decToDt7) pwh01;
             Readpe (CMco#:CMfnd:CMfyr:CMcase) pwh01;
             If  %Eof(WMWHL);
                decRtwDt = CMrtw;
             Else;
                Dow Not %Eof(WMWHL);
                   If WHwkSt = 'R';
                      decRtwDt = WHdate;
                      erRtwDt = *on;
                      Leave;
                   Else;
                      If WHwkSt = 'L';
                         decRtwDt = 0;
                         If WHdate >= decFrDt7;
                            erLwDt = *on;
                         Endif;
                      Endif;
                      Leave;
                   Endif;
                   Readpe (CMco#:CMfnd:CMfyr:CMcase) pwh01;
                Enddo;
             Endif;

             // Return to Work Error
             If decRtwDt > 0 and decRtwDt < decToDt7;
                erRtwDt = *on;
                scRtwDt10 = AlphaUsa(cvtToDate(DtMdyy(decRtwDt)));
             Endif;

             // Loss Wages Error
             If erLwDt = *on;
                scLwDt10 = AlphaUsa(cvtToDate(DtMdyy(WHdate)));
             Endif;

             // Estimated Return to work Error
             If erEstRtw = *on;
                scErtwDt10 = AlphaUsa(cvtToDate(DtMdyy(CMeRtw)));
             Endif;
             strSrvDtMsg = 'Press Override to continue or Return to make +
              corrections.';
             If erEstRtw = *on and erRtwDt = *off and erlwDt = *off;
                strMsg = 'Estimated RTW Date ' +
                  %Trim(scErtwDt10) + ' is less than To Service Date ' +
                  %Trim(scToDt10) + '.  ' + %Trim(strSrvDtMsg);
             Endif;

             If erEstRtw = *on and erRtwDt = *on and erlwDt = *off;
                strMsg = 'Estimated RTW Date ' +
                  %Trim(scErtwDt10) + ' is less than To Service Date ' +
                  %Trim(scToDt10) + ' and Actual RTW Date ' + %Trim(scRtwDt10) +
                  ' is less than To Service Date ' + %Trim(scToDt10) +  '.  ' +
                  %Trim(strSrvDtMsg);
             Endif;

             If erEstRtw = *on and erRtwDt = *on and erlwDt = *on;
                strMsg = 'Estimated RTW Date ' +
                  %Trim(scErtwDt10) + ' is less than To Service Date ' +
                  %Trim(scToDt10) + ' and Actual RTW Date ' + %Trim(scRtwDt10) +
                  ' is less than To Service Date ' + %Trim(scToDt10) +
                  ' and Actual LW Date ' + %Trim(scLwDt10) + ' is greater than +
                  From Service Date ' + %Trim(scFrDt10) + '.  ' +
                  %Trim(strSrvDtMsg);
             Endif;

             If erEstRtw = *off and erRtwDt = *off and erlwDt = *on;
                strMsg = 'Actual LW Date ' + %Trim(scLwDt10) + ' is greater +
                  than From Service Date ' + %Trim(scFrDt10) + '.  ' +
                  %Trim(strSrvDtMsg);
             Endif;

             If erEstRtw = *off and erRtwDt = *on and erlwDt = *off;
                strMsg = 'Actual RTW Date ' + %Trim(scRtwDt10) +
                  ' is less than To Service Date ' + %Trim(scToDt10) +
                  '.  ' + %Trim(strSrvDtMsg);
             Endif;

             If erEstRtw = *off and erRtwDt = *on and erlwDt = *on;
                strMsg = 'Actual RTW Date ' + %Trim(scRtwDt10) +
                  ' is less than To Service Date ' + %Trim(scToDt10) +
                  ' and Actual LW Date ' + %Trim(scLwDt10) + ' is greater than +
                  From Service Date ' + %Trim(scFrDt10) +
                  '.  ' + %Trim(strSrvDtMsg);
             Endif;

             If erEstRtw = *on or erRtwDt = *on or erLwDt = *on;
                isOvr = PopTwoBtn(strMsg:'Service Dates':'Override':'Return');
                If Not isOvr and isInvalidWdw = *off;
                   isInvalidWdw = *on;
                   Return;
                Else;
                   isInvalidWdw = *off;
                Endif;
             Endif;
          Endif;
       Endif;

       //  Determine if "Paid on Time" is not 'Y' and Detailed Reason
       //  required is 'Y'
       If scPdOnTm <> *blank;
          Chain (CMco#:scPdOnTm) plr01;
          If %Found(WTABL) and LRstat <> 'I' and LRreas = 'Y';
 sau         LatePaymentPrc(inClmRrn:scPdOnTm);
          Endif;
       Endif;

       // Payment Date Included in Full Salary Date
       // Request and Recurring
       If CMfSal = 'Y' and decFrDt7 <= CMfsDt and scReCd = 'CO'
        and scRsvType = 'C' and CMjrSt = 'TX';
          strMsg = 'Payment date is included in Full Salary dates.  +
           Do you want to override';
          isOvr = PopTwoBtn(strMsg:'Full Salary':'Override':'Return');
          If Not isOvr and isInvalidWdw = *off;
             isInvalidWdw = *on;
             Return;
          Else;
             isInvalidWdw = *off;
          Endif;
       Endif;

       // Edits specific to Recurring Claim Payments
       If scRecurr = 'Y';

          // Validate the Start Date
          // Only Recurring
          If scStartDt = *zeros;
             isInvalid = *on;
             erStDtReq = *on;
          Else;
             If dtCheck(scStartDt) = 0;
                isInvalid = *on;
                erStDtInv = *on;
             Endif;
          Endif;

          // Start Date cannot be < Accident Date
          // Only Recurring
          decStartDt = DTcyMd(scStartDt);
          If decStartDt < CMacDt;
             mgStDtAcDt = 'Start Date cannot be less than Accident Date';
             isInvalid = *on;
             erStDtAcDt = *on;
          Endif;

          // Weeks and days cannot be <= 0
          If scWeeksChk <= 0 and scDaysChk <= 0;
             mgWkDyCkRq = 'Weeks and Days cannot be less than or equal to 0';
             isInvalid = *on;
             erWkDyCkRq = *on;
          Endif;

          // Number of future checks cannot be > control Maximum
          If PCmax# <> 0 and scNumFChk > PCmax#;
             mgNumChkGt = 'Number of future checks cannot be greater than +
              control maximum';
             isInvalid = *on;
             erNumChkGt = *on;
          Endif;

          // Number of future checks cannot be <= 0
          If scNumFChk <= 0;
             mgNumChk0 = 'Number of future checks cannot be less than or +
              equal to zero';
             isInvalid = *on;
             erNumChk0 = *on;
          Endif;

          // Date Received must be valid if entered
          If scDtRcvRec <> 0;
             If dtCheck(scDtRcvRec) = 0;
                isInvalid = *on;
                erDtRreInv = *on;
             Endif;
          Endif;
       // Payment Date Included in Full Salary Date
       // Recurring
       If CMfSal = 'Y' and decFrDt7 <= CMfsDt;
          strMsg = 'Payment date is included in Full Salary dates';
          isOvr = PopOneBtn(strMsg:'Full Salary':'Return');
          If Not isOvr and isInvalidWdw = *off;
             isInvalidWdw = *on;
             Return;
          Else;
             isInvalidWdw = *off;
          Endif;
       Endif;
       Endif;

       // End Edits specific to Recurring Claim Payments

       // For Manual payments we have to determine if the bank account exists
       // for the type of processing (Check, Voucher, XML, etc)
       If scType = 'M';

          // Call the Procedure to retrieve the Bank Account
          If is114 = *on;
             Select;
             When scVoucher = 'V';
                inCall = 'V';
             When scVoucher = 'P';
                inCall = 'C';
             When scVoucher = 'X';
                inCall = 'X';
             Endsl;
          Else;
             If scVoucher = 'Y';
                inCall = 'V';
             Else;
                inCall = 'C';
             Endif;
          Endif;
          inSrDt = %Editc(DateTo8(SystemDate()):'X');
          outHdCy  = *blanks;
          outChkQ  = *blanks;
          outFtyp  = *blanks;
          outEfDt  = *zeros;
          outCpyQ  = *blanks;
          inCkTp   = outCkTp;
          inAppl   = 'WC';
          outEfTc  = *blanks;

          outCkTp = GetBankCode(inCo#Chr:inFndChr:inFyrChr:
           inCaseChr:inAppl:inData:outRtCd:outEfTc);

          inCkTp  = outCkTp;

          outAcct = GetBankAcc(inCo#Chr:inFndChr:inAppl:
           inSrDt:inCall:outHdCy:outChkQ:outFtyp:outEfDt:outCpyQ:
           inCkTp:outEfTc:inCoId:outRtCd);

          If outRtCd = '1';
             If is114 = *on;
                Select;
                When inCall = 'C';
                   mgPrcMchk = '"Check" Bank Account Not Found in Bank +
                    Information';
                   erPrcMChk = *on;
                   isInvalid = *on;
                When inCall = 'V';
                   mgPrcMvch = '"Voucher" Bank Account Not Found in Bank +
                    information file';
                   erPrcMvch = *on;
                   isInvalid = *on;
                When inCall = 'X';
                   mgPrcMxml = '"XML" file type not found in Bank Information';
                   erPrcMxml = *on;
                   isInvalid = *on;
                Endsl;
             Else;
                If inCall = 'V';
                   mgAcctVch = '"Voucher" Bank Account Not Found in Bank +
                    information file';
                   erAcctVch = *on;
                   isInvalid = *on;
                Else;
                   mgAcctChk = '"Check" Bank Account Not Found in Bank +
                    Information';
                   erAcctChk = *on;
                   isInvalid = *on;
                Endif;
             Endif;
          Endif;
       Endif;

       // Edits specific to Recoveries
       If inMode = 'R' or inMode = 'T';

          // Recovery Refund amount exceeds Requested amount
          If inMode = 'R' and scAmount > PAAmt and PAAmt > 0;
             strMsg = 'Recovery Amount more than Payment Amount.  Do you want +
              to override? ';
             isOvr = PopTwoBtn(strMsg:'Recovery Amount':'Override':'Return');
             If Not isOvr and isInvalidWdw = *off;
                isInvalidWdw = *on;
                Return;
              Else;
                isInvalidWdw = *off;
             Endif;
          Endif;

          // Recovery Tracking amount exceeds Requested amount
          If inMode = 'T' and scAmount > RRrAmt and RRrAmt > 0;
             strMsg = 'Recovery Amount more than requested.  Do you want +
              to override? ';
             isOvr = PopTwoBtn(strMsg:'Recovery Amount':'Override':'Return');
             If Not isOvr and isInvalidWdw = *off;
                isInvalidWdw = *on;
                Return;
              Else;
                isInvalidWdw = *off;
             Endif;
          Endif;

          // Check Number is required
          If scChk# = *zeros;
             msChk#Req = 'Check number is required';
             erChk#Req = *on;
             isInvalid = *on;
          Endif;

          // Days/Weeks cant exceed the number of Lost Days
          If CMdays <> 0;
             intDays = scWeeks * CMdays;
          Else;
             intDays = scWeeks * 7;
          Endif;
          intDays = intDays + scDays;
          If intDays <> 0 and intDays > CMlost;
             msDaysGtL = 'Weeks/Days cannot exceed the number of +
              lost days';
             erDaysGtL = *on;
             isInvalid = *on;
          Endif;
          //*** Beg Del *** 10/03/24 *************************
          //*** If scPayCd <> FNmeTr and scPayCd <> FNcoTr and scPayCd
          //***  <> FNexTr and scPayCd <> FNleTr;
          //***    mgPayCdInv = 'The Recovery payment code does not match +
          //***     one of the Net Deductible Recovery table values set up +
          //***     for this group';
          //***    erPayCdInv = *on;
          //***    isInvalid = *on;
          //*** Endif;
          //*** Beg Del *** 10/03/24 *************************

          If scPv1 = FNmeP1 or scPv1 = FNcoP1 or scPv1 = FNexP1 or
           scPv1 = FNlep1;
             Chain(N) (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) pel01;
             If %Found(WDELL);
                Chain (ELco#:ELrtCd) prs02;
                If %Found(WMRSL);
                   intDcEx = ELdcEx;
                   int2Dex = EL2Dex;
                   If ELdCex = 0;
                      intDcEx = *hival;
                   Endif;
                   If EL2Dex = 0;
                      int2Dex = *hival;
                   Endif;

                   Select;
                   When ELdcD2 = *blank and EL2Def <= CMacDt and int2Dex >=
                    CMacDt;
                      chrDdCd = ELdcD2;
                      intDdEf = EL2Def;
                   When ELdcDe = *blank and ELdcEf <= CMacDt and intDcEx >=
                    CMacDt;
                      chrDdCd = ELdcDe;
                      intDdEf = ELdcEf;
                   Endsl;

                   // Check cancellation date for all divisions
                   inFndChr = %Editc(ELfnd:'X');
                   inFyrChr = %Editc(ELfyr:'X');
                   inEmp#Chr = %Editc(ELemp#:'X');
                   inDivChr  = %Editc(ELdiv:'X');
                   outRtDtChr = %Editc(ELdcEf:'X');

                   GetRateDate(inCo#chr:inFndChr:inFyrChr:inEmp#Chr:inDivChr:
                    outRtDtChr);

                   intDdEf = %Dec(outRtDtChr:7:0);

                   Setll (ELco#:ELfnd:RSjrSt:chrDdCd:intDdEf) pdd01;
                   Reade (ELco#:ELfnd:RSjrSt:chrDdCd) pdd01;
                   If %Found(WTDDL3) and DDstat = 'A';
                      Select;
                      When DDtype = 'G';
                         strMsg = 'The employer for this claim is on a Gross +
                          Deductible plan and the Payment Value 1 code which +
                          you entered is used for Net Deductible Recoveries. +
                          Entering this transaction will affect the +
                          deductibles.';
                         isOvr = PopTwoBtn(strMsg:'Deductible':'Override':
                          'Return');
                      Other;
                         strMsg = 'The Payment Value 1 Code you entered is +
                          used for Net Deductible Recoveries.  Entering this +
                          transaction will affect the deductible billing +
                          amount for this employer.';
                         isOvr = PopTwoBtn(strMsg:'Deductible':'Override':
                          'Return');
                      Endsl;
                      If Not isOvr and isInvalidWdw = *off;
                         isInvalidWdw = *on;
                         Return;
                      Else;
                         isInvalidWdw = *off;
                      Endif;
                   Endif;
                Endif;
             Endif;
          Endif;

          // Amount must not exceed total payment amount for the reserve code
          y = %Lookup(scReCd:dsClaimRsvTyp:1);
          If y > 0;
             If scAmount > dsClaimPmtRsv(y);
                mgAmtRefEx = 'Refund amount cannot exceed the total payment +
                 amount made against this reserve code';
                erAmtRefEx = *on;
                isInvalid = *on;
             Endif;
          Endif;

          // If entering a negative refund, the amount will be subtracted
          // from the reserves, therefor reserves must be adequate.
          If scAmount < 0;
             If (scAmount * -1) > dsClaimRemRsv(y);
                mgAmtRefRs = 'Refund Amount cannot reduce reserves below 0.  +
                 Reserves must be increased in order to cover the negative +
                 refund amount entered';
                erAmtRefRs = *on;
                isInvalid = *on;
             Endif;
          Endif;

       Endif;

       // Edits specific to Single Adjustments
       If inMode = 'S';

          // Populate dsData data structure parm for Bank Code procedure
          dsTrDt = dtCymd(scPoDt);
          dsFrDt = dtCymd(scfrDt);
          dsToDt = dtCymd(scToDt);
          dsTrCd = scPayCd;
          dsReCd = scReCd;
          dsPv1  = scPv1;
          dsPv2  = scPv2;
          dsSeq# = *zeros;
          dsDsq# = intDsq#;
          dsEob# = *blanks;
          dsFein = fein;
          dsMod  = modifier;
          dsDsq# = 0;

          //Call the Procedure to retrieve Bank Code.
          inData = dsData;
          inAppl = 'WC';
          outEftC = ' ';
          outCkTp = GetBankCode(inCo#Chr:inFndChr:inFyrChr:
           inCaseChr:inAppl:inData:outRtCd:outEfTc);

          // If bank account not entered then retrieve check/voucher flag
          If scAcct = *blanks;
             Chain (xxCo#:scFnd) pf201;
             Select;
             When is114 = *on;
                If Not %Found(WDF2L);
                   inCall = *blanks;
                Endif;
                If %Found(WDF2L);
                   If F2cTyp <> *blanks;
                      inCall = F2cTyp;
                   Else;
                      inCall = 'C';
                   Endif;
                Endif;
             Other;
                If Not %Found(WDF2L);
                   inCall = *blanks;
                Endif;
                If %Found(WDF2L);
                   If F2cTyp = 'V';
                      inCall = F2cTyp;
                   Else;
                      inCall = 'C';
                   Endif;
                Endif;
             Endsl;
             inSrDt = %Editc(scPoDt:'X');
             outHdCy  = *blanks;
             outChkQ  = *blanks;
             outFtyp  = *blanks;
             outEfDt  = *zeros;
             outCpyQ  = *blanks;
             inCkTp   = outCkTp;
             inAppl   = 'WC';
             outEfTc  = *blanks;

             outAcct = GetBankAcc(inCo#Chr:inFndChr:inAppl:
              inSrDt:inCall:outHdCy:outChkQ:outFtyp:outEfDt:outCpyQ:
              inCkTp:outEfTc:inCoId:outRtCd);
          Endif;

          // Add/Subtract Weeks/Days cannot be blanks
          If scAsAmtWd = ' ';
             erAsWkReq = *on;
             isInvalid = *on;
          Endif;

          // Validate the reserve amount remaining vs the check amount if subtra
          // subtracting but NOT for closed claims                          btra
          //*** Beg Chg *** 11/22/24 ***************************************
          //***If scAsAmtWd = 'S';
          If scAsAmtWd = 'S' and CMstat <> 'C';
          //*** End Chg *** 11/22/24 ***************************************
             Chain (inCo#int:inFndInt:inFyrInt:inCaseInt) pcm02;
             If %Found(WMCML1);
                x = %Lookup(PCreCd:dsClaimRsvTyp:1);
                If x > 0;
                   If scAmount > dsClaimRemRsv(x);
                      erAmtRsNeg = *on;
                      mgAmtRsNeg = 'Remaining reserve may not be made +
                       negative';
                      isInvalid = *on;
                   Endif;
                Endif;
             Endif;
          Endif;
       Endif;
       //======================================================================
       // Edits specific to Adjustments
       //======================================================================
 sau   If inMode = 'C' or inMode = 'A';
 sau      AdjustmentValid();
 sau   Endif;

     p ValidPmt        e

       // AdjustmentValid - Adjustment edits
     pAdjustmentValid  b
     dAdjustmentValid  pi

       mgPv2LtFl = 'Payment Value 2 is incompatible with Claim Lost Time Flag';
       mgFrDtAcDt = 'From Service Date must be greater than or equal to +
           the Accident date';
       mgFrDtToDt = 'From Service Date may not be greater than To +
           Service Date';
       mgWkDyRq = 'Weeks cannot be less than 0';
       mgDaysLt7 = 'Number of days must be between 0-6';
       mgDscReq = 'This field can not be blank';
       mgAmtZero = 'Amount must be greater than zero';
       mgAmtRrsv = 'Amount exceeds reserve amount';
       mgAmtRsvCu= 'Reserve Cushion for Reserve Code & Claim Type is not valid';

       xxAM = 0;

       Chain (intCo#:scPayCd) ppc01;
       If not %Found(WTABL);
          mgPayCdTp1 = 'Invalid code.';
          isInvalid = *on;
          erPayCdTp1 = *on;
       Endif;
       // Offset Pay Description is required
       If scDescOff = *blanks;
          isInvalid = *on;
          erODscReq = *on;
       Endif;
       // Payee is required
       If scPayClm = *blank and scAdPayee = *blanks and scPayee = *zeros
        and scClaim = 0;
          mgPayeeRq1 = 'A Payee must be selected';
          isInvalid = *on;
          erPayeeRq1 = *on;
       Endif;
       // Weeks cannot be < 0
       If scWeeks < 0 ;
          isInvalid = *on;
          erWeeksRq1= *on;
       Endif;

       // Days must be between 0-6
       If scDays >= 7 or scDays < 0;
          isInvalid = *on;
          erDaysLt71 = *on;
       Endif;
       // Amount must be greater than zero
       If AM(1) <= *zeros;
          isInvalid = *on;
          erAmtZero1 = *on;
       Endif;
       Sv1rcd = PCrecd;
       Svtype = PAtype;

       If scNoPayee >= 2;
          Chain (intCo#:scPayCd2) ppc01;
          If not %Found(WTABL);
             mgPayCdTp2 = 'Invalid code.';
             isInvalid = *on;
             erPayCdTp2 = *on;
          Endif;
          // Pay Description is required
          If scPayCdDs2 = *blanks;
             isInvalid = *on;
             erDscReq2 = *on;
          Endif;
          // Payee is required
          If scPayClm2 = *blank and scAdPayee2 = *blanks and scPayee2 = *zeros
           and scClaim2 = 0;
             mgPayeeRq2 = 'A Payee must be selected';
             isInvalid = *on;
             erPayeeRq2 = *on;
          Endif;
          // Payment Value 1 is required
          If scPv12 = *blanks;
             isInvalid = *on;
             erPyTypRq2 = *on;
          Endif;
          // Payment Value 2 is required
          If scPv22 = *blanks;
             isInvalid = *on;
             erLtFlReq2 = *on;
          Else;
          // Payment Value 2 must be compatible with Claim Lost Time Status
             Chain (intCo#:scPv22) pb701;
             If %Found(WTABL);
                //**** Beg Chg **** 05/22/25 **************************
                //***If (CMltFl = 'I' and B7LtLf <> 'Y') or
                //****(CMltFl = 'M' and B7ltLf <> 'N') or
                //****(CMltFl = 'R' and B7ltLf <> 'N');
                If (L3_CMltFl = 'I' and B7LtLf <> 'Y') or
                 (L3_CMltFl = 'M' and B7ltLf <> 'N') or
                 (L3_CMltFl = 'R' and B7ltLf <> 'N');
                //**** End Chg **** 05/22/25 **************************
                   isInvalid = *on;
                   erPv2LtFl2 = *on;
                Endif;
             Endif;
          Endif;
          // Posting date is required and must be valid
          If scPoDt2 = *zeros;
             isInvalid = *on;
             erPoDtReq2 = *on;
          Else;
             If dtCheck(scPoDt2) = 0;
                isInvalid = *on;
                erPoDtInv2 = *on;
             Endif;
          Endif;
          // From Service Date is required and must be valid
          If scFrDt2 = *zeros;
             isInvalid = *on;
             erFrDtReq2 = *on;
          Else;
             If dtCheck(scFrDt2) = 0;
                isInvalid = *on;
                erFrDtInv2 = *on;
             Endif;
          Endif;
          // From Service date must be greater than or equal Claim Accident date
          //**** Beg Chg **** 05/22/25 *************************************
          //***If dtCYMD(scFrDt2) < CMacDt;
          If dtCYMD(scFrDt2) < L3_CMacDt;
          //**** Beg Chg **** 05/22/25 *************************************
             isInvalid = *on;
             erFrDtAcD2 = *on;
          Endif;
          // From Service may not be greater than To Service Date.
          If dtCYMD(scFrDt2) > dtCYMD(scToDt2) and scToDt2 <> *zeros;
             isInvalid = *on;
             erFrDtToD2 = *on;
          Endif;
          // To Service Date is required and must be valid
          If scToDt2 = *zeros;
             isInvalid = *on;
             erToDtReq2 = *on;
          Else;
             If dtCheck(scToDt2) = 0;
                isInvalid = *on;
                erToDtInv2 = *on;
             Endif;
          Endif;

          // Weeks cannot be < 0
          If scWeeks2 < 0 ;
             isInvalid = *on;
             erWeeksRq2= *on;
          Endif;

          // Days must be between 0-6
          If scDays2 >= 7 or scDays2 < 0;
             isInvalid = *on;
             erDaysLt72 = *on;
          Endif;
          // Amount must be greater than zero
          If AM(2) <= *zeros;
             isInvalid = *on;
             erAmtZero2 = *on;
          Endif;

          If scNoPayee >= 3;
             Chain (intCo#:scPayCd3) ppc01;
             If not %Found(WTABL);
                mgPayCdTp3 = 'Invalid code.';
                isInvalid = *on;
                erPayCdTp3 = *on;
             Endif;
             // Pay Description is required
             If scPayCdDs3 = *blanks;
                isInvalid = *on;
                erDscReq3 = *on;
             Endif;
             // Payee is required
             If scPayClm3=*blank and scAdPayee3 = *blanks and scPayee3 = *zeros
              and scClaim3 = 0;
                mgPayeeRq3 = 'A Payee must be selected';
                isInvalid = *on;
                erPayeeRq3 = *on;
             Endif;
             // Payment Value 1 is required
             If scPv13 = *blanks;
                isInvalid = *on;
                erPyTypRq3 = *on;
             Endif;
             // Payment Value 2 is required
             If scPv23 = *blanks;
                isInvalid = *on;
                erLtFlReq3 = *on;
             Else;
             // Payment Value 2 must be compatible with Claim Lost Time Status
                Chain (intCo#:scPv23) pb701;
                If %Found(WTABL);
                   //**** Beg Chg **** 05/22/25 **************************
                   //***If (CMltFl = 'I' and B7LtLf <> 'Y') or
                   //****(CMltFl = 'M' and B7ltLf <> 'N') or
                   //****(CMltFl = 'R' and B7ltLf <> 'N');
                   If (L3_CMltFl = 'I' and B7LtLf <> 'Y') or
                    (L3_CMltFl = 'M' and B7ltLf <> 'N') or
                    (L3_CMltFl = 'R' and B7ltLf <> 'N');
                   //**** End Chg **** 05/22/25 **************************
                      isInvalid = *on;
                      erPv2LtFl3 = *on;
                   Endif;
                Endif;
             Endif;
             // Posting date is required and must be valid
             If scPoDt3 = *zeros;
                isInvalid = *on;
                erPoDtReq3 = *on;
             Else;
                If dtCheck(scPoDt3) = 0;
                   isInvalid = *on;
                   erPoDtInv3 = *on;
                Endif;
             Endif;
             // From Service Date is required and must be valid
             If scFrDt3 = *zeros;
                isInvalid = *on;
                erFrDtReq3 = *on;
             Else;
                If dtCheck(scFrDt3) = 0;
                   isInvalid = *on;
                   erFrDtInv3 = *on;
                Endif;
             Endif;
             // From Service date must be greater than or equal Claim Accident d
             //**** Beg Chg **** 05/22/25 **********************************
             //***If dtCYMD(scFrDt3) < CMacDt;
             If dtCYMD(scFrDt3) < L3_CMacDt;
             //**** Beg Chg **** 05/22/25 **********************************
                isInvalid = *on;
                erFrDtAcD3 = *on;
             Endif;
             // From Service may not be greater than To Service Date.
             If dtCYMD(scFrDt3) > dtCYMD(scToDt3) and scToDt3 <> *zeros;
                isInvalid = *on;
                erFrDtToD3 = *on;
             Endif;
             // To Service Date is required and must be valid
             If scToDt3 = *zeros;
                isInvalid = *on;
                erToDtReq3 = *on;
             Else;
                If dtCheck(scToDt3) = 0;
                   isInvalid = *on;
                   erToDtInv3 = *on;
                Endif;
             Endif;

             // Weeks cannot be < 0
             If scWeeks3 < 0 ;
                isInvalid = *on;
                erWeeksRq3= *on;
             Endif;

             // Days must be between 0-6
                If scDays3 >= 7 or scDays3 < 0;
                   isInvalid = *on;
                   erDaysLt73 = *on;
                Endif;
             // Amount must be greater than zero
             If AM(3) <= *zeros;
                isInvalid = *on;
                erAmtZero3 = *on;
             Endif;

             If scNoPayee = 4;
                Chain (intCo#:scPayCd4) ppc01;
                If not %Found(WTABL);
                   mgPayCdTp4 = 'Invalid code.';
                   isInvalid = *on;
                   erPayCdTp4 = *on;
                Endif;
                // Pay Description is required
                If scPayCdDs4 = *blanks;
                   isInvalid = *on;
                   erDscReq4 = *on;
                Endif;
                // Payee is required
                If scPayClm4 = *blank and scAdPayee4=*blanks and scPayee4=*zeros
                 and scClaim4 = 0;
                   mgPayeeRq4 = 'A Payee must be selected';
                   isInvalid = *on;
                   erPayeeRq4 = *on;
                Endif;
                // Payment Value 1 is required
                If scPv14 = *blanks;
                   isInvalid = *on;
                   erPyTypRq4 = *on;
                Endif;
                // Payment Value 2 is required
                If scPv24 = *blanks;
                   isInvalid = *on;
                   erLtFlReq4 = *on;
                Else;
                // Payment Value 2 must be compatible with Claim Lost Time Statu
                   Chain (intCo#:scPv24) pb701;
                   If %Found(WTABL);
                      //**** Beg Chg **** 05/22/25 ***********************
                      //***If (CMltFl = 'I' and B7LtLf <> 'Y') or
                      //****(CMltFl = 'M' and B7ltLf <> 'N') or
                      //****(CMltFl = 'R' and B7ltLf <> 'N');
                      If (L3_CMltFl = 'I' and B7LtLf <> 'Y') or
                       (L3_CMltFl = 'M' and B7ltLf <> 'N') or
                       (L3_CMltFl = 'R' and B7ltLf <> 'N');
                      //**** End Chg **** 05/22/25 ***********************
                         isInvalid = *on;
                         erPv2LtFl4 = *on;
                      Endif;
                   Endif;
                Endif;
                // Posting date is required and must be valid
                If scPoDt4 = *zeros;
                   isInvalid = *on;
                   erPoDtReq4 = *on;
                Else;
                   If dtCheck(scPoDt4) = 0;
                      isInvalid = *on;
                      erPoDtInv4 = *on;
                   Endif;
                Endif;
                // From Service Date is required and must be valid
                If scFrDt4 = *zeros;
                   isInvalid = *on;
                   erFrDtReq4 = *on;
                Else;
                   If dtCheck(scFrDt4) = 0;
                      isInvalid = *on;
                      erFrDtInv4 = *on;
                   Endif;
                Endif;
                // From Service date must be greater than or equal Claim Acciden
                //**** Beg Chg **** 05/22/25 *******************************
                //***If dtCYMD(scFrDt4) < CMacDt;
                If dtCYMD(scFrDt4) < L3_CMacDt;
                //**** Beg Chg **** 05/22/25 *******************************
                   isInvalid = *on;
                   erFrDtAcD4 = *on;
                Endif;
                // From Service may not be greater than To Service Date.
                If dtCYMD(scFrDt4) > dtCYMD(scToDt4) and scToDt4 <> *zeros;
                   isInvalid = *on;
                   erFrDtToD4 = *on;
                Endif;
                // To Service Date is required and must be valid
                If scToDt4 = *zeros;
                   isInvalid = *on;
                   erToDtReq4 = *on;
                Else;
                   If dtCheck(scToDt4) = 0;
                      isInvalid = *on;
                      erToDtInv4 = *on;
                   Endif;
                Endif;

                // Weeks cannot be < 0
                If scWeeks4 < 0 ;
                   isInvalid = *on;
                   erWeeksRq4= *on;
                Endif;

                // Days must be between 0-6
                   If scDays4 >= 7 or scDays4 < 0;
                      isInvalid = *on;
                      erDaysLt74 = *on;
                   Endif;
                // Amount must be greater than zero
                If AM(4) <= *zeros;
                   isInvalid = *on;
                   erAmtZero4 = *on;
                Endif;

             Endif;
          Endif;
       Endif;

       // VALIDATE THE RESERVE AMOUNT REMAINING VS THE CORRECTING AMOUNT
       For I1 = 1 to scNoPayee;
          Chain (intCo#:CD(I1)) ppc01;
          If %found(WTABL);
             Chain (intCo#:PCrecd) prt01;
             If %found(WTABL);
                RTP(I1) = RTtype;
                RCD(I1) = PCrecd;
                J1 = %Lookup(PCreCd:dsClaimRsvTyp:1);
                If J1 = 0;
                   mgPayCdRc = 'Reserve code for the payment code is +
                    invalid for the claim';
                   Select;
                   When I1 = 1;
                      isInvalid = *on;
                      erPayCdRc = *on;
                   When I1 = 2;
                      isInvalid = *on;
                      erPayCdRc2 = *on;
                   When I1 = 3;
                      isInvalid = *on;
                      erPayCdRc3 = *on;
                   When I1 = 4;
                      isInvalid = *on;
                      erPayCdRc4 = *on;
                   Endsl;
                   Return;
                Endif;
             Endif;
          Endif;

          xxRem = AM(I1);
          // CALCULATE AMOUNT TO COMPARE AGAINST RESERVES
          Exsr SRAM1;
          If CMstat = 'O';
             Chain(N) (CMco#:CMfnd:CMfyr:CMcase) prp01;
             If %found();
                Dow Not %Eof();
                   If RPstat = *blanks and RPrecd = RCD(I1);
                      If svType <> 'R';
                         xxRem += RPamt;
                      Endif;
                   Endif;
                   ReadE(N) (CMco#:CMfnd:CMfyr:CMcase) prp01;
                Enddo;
                If svType <> 'R';
                   If xxRem > dsClaimRemRsv(J1);
                      isInvalid = *on;
                      Select;
                      When I1 = 1;
                         erAmtRrsv1 = *on;
                      When I1 = 2;
                         erAmtRrsv2 = *on;
                      When I1 = 3;
                         erAmtRrsv3 = *on;
                      When I1 = 4;
                         erAmtRrsv4 = *on;
                      Endsl;
                   Endif;
                Else;
                   If xxRem > dsClaimPmtRsv(J1);
                      isInvalid = *on;
                      Select;
                      When I1 = 1;
                         erAmtRrsv1 = *on;
                      When I1 = 2;
                         erAmtRrsv2 = *on;
                      When I1 = 3;
                         erAmtRrsv3 = *on;
                      When I1 = 4;
                         erAmtRrsv4 = *on;
                      Endsl;
                   Endif;
                Endif;
             Endif;
          Else;
          //IF CLAIM IS CLOSED CHECK THE RESERVE CUSHION
             chkRemCush(CMco#:CMfnd:CMfyr:CMcase:PCrecd:CMltfl:xxcush);
             If xxRem > xxCush;
                isInvalid = *on;
                //*** Beg Chg *** 10/16/23 *************************************
                Select;
                When I1 = 1;
                   erAmtRsvC1 = *on;
                When I1 = 2;
                   erAmtRsvC2 = *on;
                When I1 = 3;
                   erAmtRsvC3 = *on;
                When I1 = 4;
                   erAmtRsvC4 = *on;
                Endsl;
             Endif;
          Endif;
          xxAM += AM(I1);
       Endfor;
       If xxAM <> scOfAmount and isInValid = *Off and (inMode = 'A' or
        inMode = 'C');
          strMsg = 'Total amount is not equal to offsetting amount.';
          PopOneBtn(strMsg:'Amount Error');
          isInvalidWdw = *on;
       Endif;

       Begsr SRAM1;
        For B = 1 to scNoPayee;
           If I1 <> B;
              If RCD(I1) = RCD(B);
                 xxRem += AM(B);
              Endif;
           Endif;
        Endfor;

        If sv1Rcd = RCD(I1);
           xxRem -= scOfAmount;
        Endif;
       Endsr;

e-saupAdjustmentValid  e

       //===================================================================
       // ChkDupPmnt - Check for Duplicate Non-Recurring payments
       //===================================================================
     p DupPmt          b
     d DupPmt          pi

       // Check to see if Payment Request is a Possible Duplicate.
       // Not recurring
       inFrDt8Pck  = DtCymd(scFrDt);
       inToDt8Pck  = DtCymd(scToDt);
       inAmt    = scAmount;
       inReCd   = scReCd;
       inFein   = Fein;
       inMod    =  modifier;
       inDfund  = *blanks;
       inDcName = *blanks;
       inDpName = scPayNam;
       in90     = *off;
       inReTp   = scReTp;
       inSeq#2  = 0;
       outDupComment = *blanks;
       If scType = 'M';
          outDupRtCd = 'M';
       Else;
          outDupRtCd  = *blanks;
       Endif;
       Callp chkDupPmnt(inCo#Int:inFndInt:inFyrInt:inCaseInt:inSeq#2:
        inFrDt8Pck:inToDt8Pck:inAmt:inReCd:inFein:inDfund:inDcName:
        inDpName:in90:inReTp:inMod:outDupRtCd:outDupComment);

       // Payment Request
       If scType = 'P';
          isPendPmt = 'N';
          Select;
       // Pend
          When outDupRtCd = '1';
             isPendPmt = 'Y';
       // Cancel
          When outDupRtCd = '4';
             Return;
          Endsl;
       Endif;

       // Manual payments
       If scType = 'M';
          Select;
          When outDupRtCd = '4';
             Return;
          Other;
             // Check user security level
             inUtyp = UPtype;
             inUlvl = UPlvl;
             inCtyp = CXpdCt;
             inClvl = CXpyOd;
             inExec = '1';
             outres = *on;
             inCtyp2 = CXpdC2;
             inCtyp3 = CXpdC3;
             inCtyp4 = CXpdC4;
             inCtyp5 = CXpdC5;
             inCtyp6 = *blank;

             Callp ChkUserAuth(inUtyp:inUlvl:inCtyp:inClvl:inExec:outRes:
                inCtyp2:inCtyp3:inCtyp4:inCtyp5:inCtyp6);
             If outRes = *on;
                strMsg = 'User does not have adequate authority to enter +
                   a duplicate payment';
                PopOneBtn(strMsg:'Claim Error':'Return');
                outDupRtCd = '4';
             Endif;
             Return;
          Endsl;

       Endif;

     p DupPmt          e
       //===================================================================
       // ProcessRecur - Process Recurring Payments
       //===================================================================
     p ProcessRecur    b
     d ProcessRecur    pi

       // Check to see if Payment Request is a Possible Duplicate and write
       // the request
       // Recurring
       x = 1;
       Dow x <= scNumFChk;

          GetDates();
          inFrDt8Pck  = decFrDt7;
          inToDt8Pck  = decToDt7;
          inAmt    = scAmount;
          inReCd   = scReCd;
          inFein   = Fein;
          inMod    =  modifier;
          inDfund  = *blanks;
          inDcName = *blanks;
          inDpName = *blanks;
          in90     = *off;
          inReTp   = scReTp;
          inSeq#2  = 0;
          outDupComment = *blanks;
          outRtCd  = *blanks;
          Callp chkDupPmnt(inCo#Int:inFndInt:inFyrInt:inCaseInt:inSeq#2:
           inFrDt8Pck:inToDt8Pck:inAmt:inReCd:inFein:inDfund:inDcName:
           inDpName:in90:inReTp:inMod:outDupRtCd:outDupComment);

          //  Write the payment request
          isPendPmt = 'N';
          Select;
          // Pend
          When outDupRtCd = '1';
             isPendPmt = 'Y';
             WriteRqstPmt();
             ProcAdditional();
             intPaySeq# = RPseq# + 1;
          // Cancel
          When outDupRtCd = '4';
          Other;
             WriteRqstPmt();
             ProcAdditional();
             intPaySeq# = RPseq# + 1;
          Endsl;
          x = x + 1;
       Enddo;

     p ProcessRecur    e

       //===================================================================
       // GetDates - Get dates for future payments
       //===================================================================
     p GetDates        b
     d GetDates        pi

       If x = 1;
          decFrDt7 = DtCymd(scStartDt);
       Else;
          decDate7 = decToDt7;
          decDays = 1;
          decFrDt7 = DtCymd(dtFuture(decDate7:decDays));
       Endif;

       // Calculate the To Date - Add Weeks and Days to the From Date
       // to get the To Date
       decDate7 = decFrDt7;
       decDays = scWeeksChk * 7;
       decDays = decDays + scDaysChk - 1;
       decToDt7 = DtCymd(dtFuture(decDate7:decDays));

       // The Posting Date should = the To Date unless it falls on a
       // Saturday or Sunday.  In that Case, the Posting Date will need
       // to be the Friday before the To Date
       If decToDt7 <> 0;
          decDate8 = decToDt7;

          // Subtract # of Days in Advance to print the checks from the
          // To Date.  Posting Date logic.
          If PCprtD > 0;
             decDays = PCprtD;
             decPoDt7 = DtCymd(dtPast(decDate8:decDays));
          Endif;
          chrSday = DowShort(decPoDt7);
       Endif;

       If chrSday <> 'Sat.' and chrSday <> 'Sun.';
          //*** Beg Add *** 10/02/25 ******************************************
          decPoDt7 = decDate8;
          //*** End Add *** 10/02/25 ******************************************
       Else;
          If chrSday = 'Sun.';
             decDays = 2;
          Else;
             decDays = 1;
          Endif;
          decDate8 = decPoDt7;
          decPoDt7 = DtCymd(dtPast(decDate8:decDays));
       Endif;

     p GetDates        e

       //===================================================================
       // LoadRsvArr - Load Reserve Code arrays
       //===================================================================
     p LoadRsvArr      b
     d LoadRsvArr      pi

       Clear arrRemRsv;
       Clear arrRemRsvGroup;
       Clear arrRsvCode;
       isGrpChk = *blank;
       x = 1;
       arrRemRsv(x) = scAmount;
       arrRsvCode(x) = PCreCd;
       isGrpChk = *blanks;

       If scEob# <> 0 and cltId <> '152';
          Setll (inCo#Int:inFndInt:inFyrInt:inCaseInt:scEob#:scRcn#) prp03;
          Reade (inCo#Int:inFndInt:inFyrInt:inCaseInt:scEob#:scRcn#) prp03;
          Dow Not %Eof(WARPLN);
             If RPstat = 'P';
                x = %Lookup(RPreCd:arrRsvCode:1);
                If x > 0;
                   x = %Lookup(' ':arrRsvCode:1);
                   arrRsvCode(x) = RPreCd;
                Endif;
                arrRemRsv(x) = arrRemRsv(x) + RPamt;
             Endif;
             If RPpgTp = 'G';
                isGrpChk = 'Y';
                intFein = RPfein;
                intMod  = RPmod;
             Endif;
             Reade (inCo#Int:inFndInt:inFyrInt:inCaseInt:scEob#:scRcn#)
              prp03;
          Enddo;
       Endif;
       If isGrpChk = 'Y' and scAmount < 0;
          Setll (inCo#Int:inFndInt:inFyrInt:inCaseInt:scEob#:scRcn#) prp03;
          Reade (inCo#Int:inFndInt:inFyrInt:inCaseInt:scEob#:scRcn#) prp03;
          Dow Not %Eof(WARPLN);
             If RPstat = 'P' and RPfein = intFein and RPmod = intMod and
              RPpgTp = 'Y';
                arrRemRsvGroup(x) = arrRemRsvGroup(x) + RPamt;
             Endif;
             Reade (inCo#Int:inFndInt:inFyrInt:inCaseInt:scEob#:scRcn#) prp03;
          Enddo;
       Endif;

       // Accumulate all released requests amounts
          Setll (inCo#Int:inFndInt:inFyrInt:inCaseInt) prp01;
          Reade(N) (inCo#Int:inFndInt:inFyrInt:inCaseInt) prp01;
          Dow Not %Eof(WARPL);
             If RPstat = ' ';
                x = %Lookup(RPreCd:arrRsvCode:1);
                If x > 0;
                   isFoundRsvCd = 'Y';
                   arrRemRsv(x) = arrRemRsv(x) + RPamt;
                Else;
                   isFoundRsvCd = 'N';
                Endif;
                If RPfein = intFein and RPmod = intMod and RPpgTp = isGrpChk
                 and scAmount < 0 and isFoundRsvCd = 'Y';
                   arrRemRsvGroup(x) = arrRemRsvGroup(x) + RPamt;
                Endif;
             Endif;
             Reade(N) (inCo#Int:inFndInt:inFyrInt:inCaseInt) prp01;
          Enddo;

     p LoadRsvArr      e
       //===================================================================
       // PreProcessRec - Preprocessing for recoverys
       //===================================================================
     p PreProcessRec   b
     d PreProcessRec   pi

       Select;
       When inMode = 'T';
          Chain(N) (CMco#:CMfnd:CMfyr:CMcase) prr01;
       Endsl;

     p PreProcessRec   e
       //===================================================================
       // OtherProcesses - EDI, Etc.
       //===================================================================
     p OtherProcesses  b
     d OtherProcesses  pi

       //  Call the FL EDI State Maint screen if this is the first comp
       //  pmt and the claim's jurisdiction state is FL.
       //  Request and Recurring and Manual
       If scReTp = 'C' and CMjrSt = 'FL';
          inJrst = CMjrSt;
          inTrid = 'A49';
          Callp ProcEdiA49(intCo#:inJrSt:inTrId:intFnd:outRtCd:inEdiFmt:inElt:
           inElTd:inFroi:insDue:inRls:inCsDt:inEaDc:inHcPr:inFnRc:inHcEd);
          If ouTrtCd = 'Y';
             isFirstCmpPmt = 'Y';
             Chain(n) (CMCo#:CMfnd:CMfyr:CMcase) prp01;
             If %Found(WARPL);
                Dow not %Eof(WARPL);
                   If RPreTp = 'C' and RPstat <> 'D';
                      isFirstCmpPmt = 'N';
                      Leave;
                   Else;
                      Reade(n) (CMCo#:CMfnd:CMfyr:CMcase) prp01;
                   Endif;
                Enddo;
             Endif;
             If isFirstCmpPmt = 'Y';
                Chain(n) (CMCo#:CMfnd:CMfyr:CMcase) ppa00;
                If %Found(WDPAL);
                   Dow not %Eof(WDPAL);
                      If PAreTp = 'C';
                         isFirstCmpPmt = 'N';
                         Leave;
                      Else;
                         Reade(n) (CMCo#:CMfnd:CMfyr:CMcase) ppa00;
                      Endif;
                   Enddo;
                Endif;
             Endif;

             If  isFirstCmpPmt = 'Y';
                inCall  = 'C';
                outRtCd = *blank;
                Callp ProcEdiFl(inCall:inCo#Chr:inFndChr:inFyrChr:inCaseChr:
                 outRtCd);
                If outRtCd = 'A';
                   leavePgm = *on;
                   Return;
                Endif;
             Endif;

          Endif;

       Endif;

     p OtherProcesses  e

       //===================================================================
       // WriteRqstPmt - Write Payment Request Record
       //===================================================================
     p WriteRqstPmt    B
     d WriteRqstPmt    pi

       Clear prp01;


       RPco#  = CMco#;
       RPfnd  = CMfnd;
       RPfyr  = CMfyr;
       RPcase = CMcase;
       RPseq# = intPaySeq#;

       sysDt7 = DateTo7(SystemDate());
       sysTm6 = TimeTo6(SystemTime());


       Select;
       When scPayee <> 0;
          RPfein = Fein;
          RPmod  = Modifier;
          RPpgTp = scPgTp;
       Endsl;

       RPreTp = scRsvType;
       //*** Beg Add *** 04/14/25 ******************************************
       If RpreTp = ' ';
          RPreTp = scReTp;
       Endif;
       //*** End Add *** 04/14/25 ******************************************
       //*** Beg Add *** 04/03/25 ******************************************
       RPbr2  = scPv1;
       //*** End Add *** 04/03/25 ******************************************

       // See if we should process prevent signature logic
       // Request and Recurring
       strpSig = *blanks;
       Chain (intCo#:RPbr2) pb201;
       If %Found(WTABL);
          strPsig = B2psig;
       EndIf;

       If CltId = '129' And strPsig = 'Y';
          inAmt      = scAmount;
          strBr2     = scPv1;
          //*** Beg Chg *** 04/03/25 ***************************************
          //***inFrDt8    = scFRDT;
          inFrDt8Pck = scFRDT;
          //*** End Chg *** 04/03/25 ***************************************
          inAamt     = *Zeros;
          outRtCd    = *Blank;

          //*** Beg Chg *** 04/03/25 ***************************************
          //***ApprActPgm(inCo#Int:inFNDInt:inFyrInt:inCaseInt:
          //***           inAmt:strBr2:inFrdt8:inAamt:outRtCd);
          ApprActPgm(inCo#Int:inFNDInt:inFyrInt:inCaseInt:
                     inAmt:strBr2:inFrdt8Pck:inAamt:outRtCd);
          //*** End Chg *** 04/03/25 ***************************************
          If outRtcd = 'Y';
             RPamt = inAamt;
          Else;
             RPamt = inAmt;
          EndIf;
       Else;
          RPamt = scAmount;
       EndIf;

       //*** Beg Del *** 04/03/25 ******************************************
       //***RPbr2  = scPv1;
       //*** End Del *** 04/03/25 ******************************************
       RPbr7  = scPv2;
       RPncci = %Trim(CMncci);
       RPtrCd = scPayCd;

       Chain (intCo#:RPtrCd) ppc01;
       If %Found(WTABL);
          RPtype = PCtrTp;
          RPreCd = PCreCd;
       Endif;

       RPdesc = scPayCdDsc;

       RPrmk1 = scComm1;
       RPrmk2 = scComm2;
       RPrmk3 = scComm3;
       RPrmDs = scComm4;

       RPpNam = scPayNam;
       RPadr1 = scPayAdr1;
       RPadr2 = scPayAdr2;
       RPCty  = scPayCty;
       RPst   = scPaySt;
       RPzip  = scPayZip;
       RPctry = scCtry;
       RPadr3 = scAdr3;
       RPbul1 = *zeros;
       If dsEob# <> *zeros and dsEob# <> *blanks;
          RPeob# = %Dec(dsEob#:9:0);
       Else;
          RPeob# = scEOB#;
       Endif;
       //
       RPoff# = *blanks;

       // Populate dsData data structure parm for Bank Code procedure
       dsTrDt = RPtrDt;
       dsFrDt = RPfrDt;
       dsToDt = RPtoDt;
       dsTrCd = RPtrCd;
       dsReCd = RPReCd;
       dsPv1  = RPbr2;
       dsPv2  = RPbr7;
       dsSeq# = *zeros;
       Monitor;
       dsEob# = %Editc(RPeob#:'Z');
       On-error;
          dsEob# = *blanks;
       Endmon;
       dsFein = RPfein;
       dsMod  = RPmod;
       dsDsq# = intDsq#;

       If dsDsq# <> 0;
          dsFein = *zeros;
          dsMod  = *zeros;
       Endif;

       //Call the Procedure to retrieve Bank Code.
       inData = dsData;
       inAppl = 'WC';
       outEftC = ' ';
       If scRecurr = 'Y';
          outRtCd = 'R';
       Else;
          outRtCd = ' ';
       Endif;
       outCkTp = GetBankCode(inCo#Chr:inFndChr:inFyrChr:
        inCaseChr:inAppl:inData:outRtCd:outEfTc);
       RPeftC = outEftc;
       RPckTp = outCkTp;

       // If EFT payee, then determine if pre-notification record is required
       isEft = *off;
       isPre = *off;
       If RPeftc = 'E';
          If RPfein = 0;
             // Claimant/dependent
             // Build Key to access API for EFT record
             %Subst(strWrk10:1:3) = %Editc(RPfyr:'X');
             %Subst(strWrk10:4:7) = %Editc(RPCase:'X');

             If intDsq# = 0;
                // Claimant
                inPtyp = 'C';
                inEkey = strWrk10;
             Else;
                // dependent
                inPtyp = 'D';
                %subst(strWrk15:1:10) = strWrk10;
                %subst(strWrk15:11:5) = %Editc(intDsq#:'X');
                inEkey = strwrk15;
             Endif;
             inFndChr = %Editc(RPfnd:'X');
          Else;
             // Provider
             %Subst(strWrk12:1:9)  = %Editc(RPfein:'X');
             %Subst(strWrk12:10:3) = %Editc(RPMod:'X');
             inEkey = strWrk12;
             inPtyp = 'P';
             inFndChr = %Editc(RPfnd:'X');
          Endif;
          inDateChr = %Editc(DateTo7(SystemDate()):'X');

          //Call the Procedure to retrieve EFT status.
          isEft = GetEFTStatus(inCo#Chr:inPtyp:inFndChr:inEkey:inDateChr:
           outPre:outRrnChr:outEobType:outCkTp);

          isPre = outPre;
          If isEft = *Off;
             RPeftc = *Blanks;
          EndIf;
       Endif;
       If cltId = '114';
          If RPckTp = 'CPARX';
             RPCKTP = 'CPAMED';
          Endif;
          // If request amount is >= 5000 put in pend status.
          If RPfnd >= 100 and RPfnd <= 115 And RPamt >= 5000;
             RPsts = 'PE';
             RPstat = 'P';
          Endif;
       Endif;

       RPacct = *blanks;

       RPsts  = scPayStCd;

       // If this was a duplicate payment then Pend the Request
       If isPendPmt = 'Y';
          If CXdpPc <> *blanks;
             RPsts = CXdpPc;
          Else;
             RPsts = 'PE';
          Endif;
          RPstat = 'P';
       Endif;

       Chain (intCo#:scPayStCd) pps01;
       If %Found(WTABL);
          RPsort = PSsrt;
          If PStype = 'R';
             RPstat = *blank;
          Else;
             RPstat = PStype;
          Endif;
       Endif;

       RPsent = 'N';
       RPdSeq = *zeros;
       RPrqst = Q1user;
       RPntwk = *blanks;
       If Q1prms >= 16;
          RPeobs = inRcn#;
       Else;
          RPeobs = scRcn#;
       Endif;
       RPrDt  = 0;
       RPrTm  = 0;
       RPrUsr = *blanks;
       RPrPgm = *blanks;
       If RPstat = ' ';
          RPrDt  = sysDt7;
          RPrTm  = sysTm6;
          RPrUsr = Q1user;
          RPrPgm = Q1pgm;
       Endif;
       If inMode = 'I';
          RPiPgm = 'MC';
       Endif;
       If CXdppc <> *Blank and outRtCd = '1';
          RPsts  = CXdppc;
          RPStat = 'P';
       Endif;

       RPlmpS = scLmpSum;
       RPdSq# = intDsq#;
       RPinv# = scInv#;
       RPdupCom = outDupComment;

       // Populate fields based on Recurring on Non-Recurring
       Select;
       // Not Recurring
       When scRecurr <> 'Y';
          RPfrDt = dtCymd(scfrDt);
          RPtoDt = dtCymd(sctoDt);
          RPrcvD = dtCymd(scDtRcv);
          RPltCd = scPdOnTm;
          RPtrDt = dtCymd(scPoDt);
          RPwks  = scWeeks;
          RPdays = scDays;
          RPfnal  = scFnCmpPmt;
          // Access Payment Value 1 table to obtain # days to due date
          RPdue = RPrcvD;
          Chain (intCo#:RPbr2) pb201;
          If %Found(WTABL);
             Monitor;
                numDate8 = DtCymd(dtFuture(scDtRcv:B2ndDt));
                If numDate8 > 0;
                   RPdue = dtCymd(numdate8);
                Endif;
             On-error;
             Endmon;
          Endif;
       // Recurring
       When scRecurr = 'Y';
          RPfrDt = decFrDt7;
          RPtoDt = decToDt7;
          RPdue = 0;
          RPrcvD =  dtCymd(scDtRcvRec);
          RPltCd = 'Y ';
          RPtrDt = decPoDt7;
          RPfnal  = 'N';
          If B2exWdC = 'Y';
             RPwks  = 0;
             RPdays = 0;
          Else;
             RPwks  = scWeeksChk;
             RPdays = scDaysChk;
          Endif;
       Endsl;

       RPiDt   = DateTo7(SystemDate());
       RPiTm   = TimeTo6(SystemTime());
       RPiUsr  = Q1user;
       RPipgm  = Q1pgm;
       RPDt    = DateTo7(SystemDate());
       RPTime  = TimeTo6(SystemTime());
       RPuser  = Q1user;
       RPpgm   = Q1pgm;

       // Finally we write the record based on EFT and pre-note
       Select;
       // Not EFT payee OR
       // EFT payee but pre-notification is not required, write transaction OR
       // E-Pay
       When isEft = *Off or (isEft = *on and isPre = *off) or RPeftC = 'V';
          Write prp01;
       // EFT payee and Pre-notification is required
       When isEft = *on and isPre = *on;
          // Create Pre-notification with RPAMT = 0
          svAmt = RPamt;
          Clear RPamt;
          Write prp01;
          // Create check request
          RPamt = svAmt;
          RPeftc = *Blanks;

          Write prp01;

       Endsl;

     p WriteRqstPmt    e

       //===================================================================
       // WriteRecPmt - Write Recovery Payment Record to WDPAP
       //===================================================================
     p WriteRecPmt     b
     d WriteRecPmt     pi

       Setgt (CMco#:CMfnd:CMfyr:CMcase) ppa00;
       Readpe (CMco#:CMfnd:CMfyr:CMcase) ppa00;
       If Not %Found(WDPAL);
          PAseq# = 0;
       Endif;
       intPaySeq# = PAseq# + 1;

       Clear ppa00;

       PAco#  = CMco#;
       PAfnd  = CMfnd;
       PAfyr  = CMfyr;
       PAcase = CMcase;
       PAseq# = intPaySeq#;
       PAchk# = scChk#;
       PAbaSq = hldSeq#;
       sysDt7 = DateTo7(SystemDate());
       sysTm6 = TimeTo6(SystemTime());
       PAtrDt = sysDt7;
       PApoDt = DtCyMd(scPoDt);
       PAfein = fein;
       PAmod  = modifier;
       PAamt  = scAmount;
       PAbr2  = scPv1;
       PAbr7  = scPv2;
       PAfrDt = DtcYmd(scFrdt);
       PAtoDt = DtcYmd(scToDt);
       PAwks   = scWeeks;
       PAdays  = scDays;
       PAtrCd  = scPayCd;
       PAtype  = 'R';
       PAreCd  = scReCd;
       PAreTp  = RTtype;
       //*** Beg Add *** 04/14/25 ******************************************
       If PAreTp = ' ';
          PAreTp = scReTp;
       Endif;
       If PAreTp = ' ';
          PAreTp = scRsvType;
       Endif;
       //*** End Add *** 04/14/25 ******************************************
       PAdesc  = scPayCdDsc;
       PAstat  = *blanks;
       PAadSt  = *blanks;
       PAeob#  = scEob#;
       PAeobs  = scRcn#;
       PAoff#  = CMoff#;
       // Check Type and Bank account was determined during screen validation
       PAckTp = outCkTp;
       PAacct = outAcct;
       Clear PApgTp;
       Clear PArcvd;
       Clear PAdue;
       Clear PAltCd;
       Clear PAdpCd;
       Clear PAdpCu;
       Clear PAfnal;
       Clear PArcDt;
       Clear PArAdj;
       PArqst  = Q1user;
       PAuser  = Q1user;
       PApgm   = Q1pgm;
       Padt    = sysDt7;
       PAtime  = sysTm6;
       Write ppa00;

     p WriteRecPmt     e

       //===================================================================
       // WriteSngPmt - Write Single Adjustment to WDPAP
       //===================================================================
     p WriteSngPmt     b
     d WriteSngPmt     pi

       Setgt (CMco#:CMfnd:CMfyr:CMcase) ppa00;
       Readpe (CMco#:CMfnd:CMfyr:CMcase) ppa00;
       If Not %Found(WDPAL);
          PAseq# = 0;
       Endif;
       intPaySeq# = PAseq# + 1;

       Clear ppa00;

       PAco#  = CMco#;
       PAfnd  = CMfnd;
       PAfyr  = CMfyr;
       PAcase = CMcase;
       PAseq# = intPaySeq#;
       PAchk# = scChk#;
       PAbaSq = hldSeq#;
       sysDt7 = DateTo7(SystemDate());
       sysTm6 = TimeTo6(SystemTime());
       PAtrDt = sysDt7;
       PApoDt = DtCyMd(scPoDt);
       PAfein = fein;
       PAmod  = modifier;
       PAamt  = scAmount;
       PAbr2  = scPv1;
       PAbr7  = scPv2;
       PAfrDt = DtcYmd(scFrdt);
       PAtoDt = DtcYmd(scToDt);
       PAwks   = scWeeks;
       PAdays  = scDays;
       PAtrCd  = scPayCd;
       PAtype  = 'A';
       PAreCd  = scReCd;
       PAreTp  = RTtype;
       PAdesc  = scPayCdDsc;
       PAstat  = *blanks;
       PAadSt  = *blanks;
       PAeob#  = scEob#;
       PAeobs  = scRcn#;
       PAoff#  = CMoff#;
       // Check Type and Bank account was determined during screen validation
       PAckTp = outCkTp;
       PAacct = outAcct;
       Clear PApgTp;
       Clear PArcvd;
       Clear PAdue;
       Clear PAltCd;
       Clear PAdpCd;
       Clear PAdpCu;
       Clear PAfnal;
       Clear PArcDt;
       Clear PArAdj;
       PArqst  = Q1user;
       PAuser  = Q1user;
       PApgm   = Q1pgm;
       Padt    = sysDt7;
       PAtime  = sysTm6;
       Write ppa00;

     p WriteSngPmt     e

       //===================================================================
       // WriteManualPmt - Write Payment Record to WDPAP
       //===================================================================
     p WriteManualPmt  b
     d WriteManualPmt  pi

       Setgt (CMco#:CMfnd:CMfyr:CMcase) ppa00;
       Readpe (CMco#:CMfnd:CMfyr:CMcase) ppa00;
       If Not %Found(WDPAL);
          PAseq# = 0;
       Endif;
       intPaySeq# = PAseq# + 1;

       Clear ppa00;

       PAco#  = CMco#;
       PAfnd  = CMfnd;
       PAfyr  = CMfyr;
       PAcase = CMcase;
       PAseq# = intPaySeq#;

       sysDt7 = DateTo7(SystemDate());
       sysTm6 = TimeTo6(SystemTime());


       PAchk# = 0;
       Chain (CMco#:outAcct) pck01;
       If %Found(WSCKL);
          PAchk# = CKlChk;
          CKlChk = CKlChk + 1;
          CKuser = Q1user;
          CKpgm  = Q1pgm;
          CKtime = sysTm6;
          CKdt   = sysDt7;
          Update pck01;
       Endif;


       Select;
       When scPayee <> 0;
          PAfein = Fein;
          PAmod  = Modifier;
          PApgTp = scPgTp;
       Endsl;

       PAreTp = scRsvType;
       //*** Beg Add *** 04/03/25 ******************************************
       PAbr2  = scPv1;
       //*** End Add *** 04/03/25 ******************************************

       // See if we should process prevent signature logic
       // Request and Recurring
       strPSig = *blanks;
       Chain (intCo#:PAbr2) pb201;
       If %Found(WTABL);
          strPsig = B2psig;
       EndIf;

       If CltId = '129' And strPsig = 'Y';
          inAmt      = scAmount;
          strBr2     = scPv1;
          //*** Beg Chg *** 04/03/25 ***************************************
          //***inFrDt8    = scFRDT;
          inFrDt8Pck = scFRDT;
          //*** End Chg *** 04/03/25 ***************************************
          inAamt     = *Zeros;
          outRtCd    = *Blank;

          //*** Beg Chg *** 04/03/25 ***************************************
          //***ApprActPgm(inCo#Int:inFNDInt:inFyrInt:inCaseInt:
          //***           inAmt:strBr2:inFrdt8:inAamt:outRtCd);
          ApprActPgm(inCo#Int:inFNDInt:inFyrInt:inCaseInt:
                     inAmt:strBr2:inFrdt8Pck:inAamt:outRtCd);
          //*** End Chg *** 04/03/25 ***************************************
          If outRtcd = 'Y';
             PAamt = inAamt;
          Else;
             PAamt = inAmt;
          EndIf;
       Else;
          PAamt = scAmount;
       EndIf;

       //*** Beg Del *** 04/03/25 ******************************************
       //***PAbr2  = scPv1;
       //*** End Del *** 04/03/25 ******************************************
       PAbr7  = scPv2;
       PAtrCd = scPayCd;

       Chain (intCo#:PAtrCd) ppc01;
       If %Found(WTABL);
          PAtype = PCtrTp;
          PAreCd = PCreCd;
       Endif;

       PAdesc = scPayCdDsc;

       PAbul1 = *zeros;
       If dsEob# <> *zeros and dsEob# <> *blanks;
          PAeob# = %Dec(dsEob#:9:0);
       Else;
          PAeob# = scEOB#;
       Endif;
       //
       PAoff# = CMoff#;

       // Populate dsData data structure parm for Bank Code procedure
       dsTrDt = PAtrDt;
       dsFrDt = PAfrDt;
       dsToDt = PAtoDt;
       dsTrCd = PAtrCd;
       dsReCd = PAReCd;
       dsPv1  = PAbr2;
       dsPv2  = PAbr7;
       dsSeq# = *zeros;
       Monitor;
       dsEob# = %Editc(PAeob#:'Z');
       On-error;
          dsEob# = *blanks;
       Endmon;
       dsFein = PAfein;
       dsMod  = PAmod;
       dsDsq# = intDsq#;

       If dsDsq# <> 0;
          dsFein = *zeros;
          dsMod  = *zeros;
       Endif;

       //Call the Procedure to retrieve Bank Code.
       inData = dsData;
       inAppl = 'WC';
       outEftC = ' ';
       outRtCd = ' ';

       // Check Type and Bank account was determined during screen validation
       PAckTp = outCkTp;
       PAacct = outAcct;

       // If EFT payee, then determine if pre-notification record is required
       isEft = *off;
       isPre = *off;
       If PAeftc = 'E';
          If PAfein = 0;
             // Claimant/dependent
             // Build Key to access API for EFT record
             %Subst(strWrk10:1:3) = %Editc(PAfyr:'X');
             %Subst(strWrk10:4:7) = %Editc(PACase:'X');

             If intDsq# = 0;
                // Claimant
                inPtyp = 'C';
                inEkey = strWrk10;
             Else;
                // dependent
                inPtyp = 'D';
                %subst(strWrk15:1:10) = strWrk10;
                %subst(strWrk15:11:5) = %Editc(intDsq#:'X');
                inEkey = strwrk15;
             Endif;
             inFndChr = %Editc(PAfnd:'X');
          Else;
             // Provider
             %Subst(strWrk12:1:9)  = %Editc(PAfein:'X');
             %Subst(strWrk12:10:3) = %Editc(PAMod:'X');
             inEkey = strWrk12;
             inPtyp = 'P';
             inFndChr = %Editc(PAfnd:'X');
          Endif;
          inDateChr = %Editc(PAdt:'X');

          //Call the Procedure to retrieve EFT status.
          isEft = GetEFTStatus(inCo#Chr:inPtyp:inFndChr:inEkey:inDateChr:
           outPre:outRrnChr:outEobType:outCkTp);

          isPre = outPre;
          If isEft = *Off;
             PAeftc = *Blanks;
          EndIf;
       Endif;

       If cltId = '114';
          If PAckTp = 'CPARX';
             PACKTP = 'CPAMED';
          Endif;
       Endif;

       PArqst = Q1user;
       If Q1prms >= 16;
          PAeobs = inRcn#;
       Else;
          PAeobs = scRcn#;
       Endif;

       PAlmpS = scLmpSum;
       PAdSq# = intDsq#;
       PAinv# = scInv#;
       PAdupCom = outDupComment;

       PAfrDt = dtCymd(scfrDt);
       PAtoDt = dtCymd(sctoDt);
       PArcvD = dtCymd(scDtRcv);
       PAltCd = scPdOnTm;
       PApoDt = dtCyMd(scPoDt);
       PAtrDt = sysDt7;
       PAwks  = scWeeks;
       PAdays = scDays;
       PAfnal  = scFnCmpPmt;
       PAdue = PArcvD;
       // Access Payment Value 1 table to obtain # days to due date
       Chain (intCo#:PAbr2) pb201;
       If %Found(WTABL);
          Monitor;
             numDate8 = DtCymd(dtFuture(scDtRcv:B2ndDt));
             If numDate8 > 0;
                PAdue = dtCymd(numdate8);
             Endif;
          On-error;
          Endmon;
       Endif;
       If is114 = *on and scVoucher = 'X';
          PAstat = 'X';
       Endif;

       PAiDt   = DateTo7(SystemDate());
       PAiTim  = TimeTo6(SystemTime());
       PAiUsr  = Q1user;
       PAipgm  = Q1pgm;
       PADt    = DateTo7(SystemDate());
       PATime  = TimeTo6(SystemTime());
       PAuser  = Q1user;
       PApgm   = Q1pgm;

       Write ppa00;

     p WriteManualPmt  e

       //===================================================================
       // WriteSinglePmt - Write Single Adjustment to payment File
       //===================================================================
     p WriteSinglePmt  b

       Setgt (CMco#:CMfnd:CMfyr:CMcase) ppa00;
       Readpe (CMco#:CMfnd:CMfyr:CMcase) ppa00;
       If Not %Found(WDPAL);
          PAseq# = 0;
       Endif;
       intPaySeq# = PAseq# + 1;

       Clear ppa00;

       PAseq# = intPaySeq#;

       PAwks  = scWeeks;
       PAdays = scDays;

       sysDt7 = DateTo7(SystemDate());
       sysTm6 = TimeTo6(SystemTime());


       If scAsAmtWd = 'A';
          PAamt = scAmount;
       Else;
          PAamt = -scAmount;
       Endif;

       PAco#  = CMco#;
       PAfnd  = CMfnd;
       PAfyr  = CMfyr;
       PAcase = CMcase;
       PAchk# = scChk#;
       PAbasQ = *zeros;
       PAadSt = *blanks;


       PAtrDt = sysDt7;
       PApoDt = dtCyMd(scPoDt);

       Select;
       When scPayee <> 0;
          PAfein = Fein;
          PAmod  = Modifier;
       Endsl;
       PAbr2  = scPv1;
       PAbr7  = scPv2;

       PAfrDt = dtCymd(scfrDt);
       PAtoDt = dtCymd(sctoDt);
       PAtrCd = scPayCd;
       PAtype = 'A';
       Chain (intCo#:PAtrCd) ppc01;
       If %Found(WTABL);
          PAtype = PCtrTp;
          PAreCd = PCreCd;
       Endif;
       PArcvD = dtCymd(scDtRcv);
       PAreTp = RTtype;
       PAdesc = scPayCdDsc;
       PAstat = *blanks;
       If F2cTyp = 'X';
          PAstat = 'X';
       Endif;

       PArqst = Q1user;
       PAoff# = CMoff#;

       // Check Type and Bank account was determined during screen validation
       // Bank account might have been entered on the screen.

       PAckTp = outCkTp;
       PAdsq# = intDsq#;
       If scAcct <> *blanks;
          PAacct = scAcct;
       Else;
          PAacct = outAcct;
       Endif;

       Clear PAbul1;
       Clear PAeob#;
       Clear PApgTp;
       Clear PArcvD;
       Clear PAdue;
       Clear PALtCd;
       Clear PADpCd;
       Clear PADpCu;
       Clear PArcDt;
       Clear PArAdj;

       If F2cTyp = 'X';
          PAstat = 'X';
       Endif;

       PAdupCom = outDupComment;

       PAiDt   = DateTo7(SystemDate());
       PAiTim  = TimeTo6(SystemTime());
       PAiUsr  = Q1user;
       PAipgm  = Q1pgm;
       PADt    = DateTo7(SystemDate());
       PATime  = TimeTo6(SystemTime());
       PAuser  = Q1user;
       PApgm   = Q1pgm;

       Write ppa00;

     p WriteSinglePmt  e

       //===================================================================
       // WriteRefund - Write to Refund File
       //===================================================================
     p WriteRefund     b

       Chain (PAco#:PAfnd:PAfyr:PAcase:intReq#Rec) prr01;
       RRco#  = PAco#;
       RRfnd  = PAFnd;
       RRfyr  = PAfyr;
       RRreq# = intReq#Rec;
       RRcase = PAcase;
       RRfein = fein;
       RRmod  = modifier;
       RRrAmt = RRrAmt - scAmount;
       RRrcvD = RRrcvD + scAmount;
       RRdesc = scPayCdDsc;
       sysDt7 = DateTo7(SystemDate());
       sysTm6 = TimeTo6(SystemTime());
       RRdtRc = sysDt7;
       RRuser = Q1user;
       RRpgm  = Q1pgm;
       RRdt   = sysDt7;
       RRtime = sysTm6;
       If %Found(WMRRL);
          Update prr01;
       Else;
          RRRcvD = scAmount;
          RRrAmt = 0;
          Write prr01;
       Endif;

     p WriteRefund     e

       //===================================================================
       // WriteViewCheck - Write to View Check File
       //===================================================================
     p WriteViewCheck  b

       Clear prp06;
       RPco# = PAco#;
       RPfnd = PAfnd;
       RPfyr = PAfyr;
       RPcase = PAcase;
       RPchk# = PAchk#;
       RPTrDt = DateTo7(SystemDate());
       If inMode <> 'S';
          RPrmk1 = scComm1;
          RPrmk2 = scComm2;
          RPrmk3 = scComm3;
          RPrMDs = scComm4;
       Endif;
       Select;
       When inMode <> 'A';
          RPpNam = scPayNam;
          RPadr1 = scPayAdr1;
          RPadr2 = scPayAdr2;
          RPcty  = scPayCty;
          RPst   = scPaySt;
          RPzip  = scPayZip;
          RPctry = scCtry;
          RPadr3 = scAdr3;
       When inMode = 'A';
          Select;
          When I1 = 1;
             RPpNam = scPayNam;
             RPadr1 = scPayAdr1;
             RPadr2 = scPayAdr2;
             RPcty  = scPayCty;
             RPst   = scPaySt;
             RPzip  = scPayZip;
             RPctry = scCtry;
             RPadr3 = scAdr3;
          When I1 = 2;
             RPpNam = scPayNam2;
             RPadr1 = scPayAdr12;
             RPadr2 = scPayAdr22;
             RPcty  = scPayCty2;
             RPst   = scPaySt2;
             RPzip  = scPayZip2;
             RPctry = scCtry2;
             RPadr3 = scAdr32;
          When I1 = 3;
             RPpNam = scPayNam3;
             RPadr1 = scPayAdr13;
             RPadr2 = scPayAdr23;
             RPcty  = scPayCty3;
             RPst   = scPaySt3;
             RPzip  = scPayZip3;
             RPctry = scCtry3;
             RPadr3 = scAdr33;
          When I1 = 4;
             RPpNam = scPayNam4;
             RPadr1 = scPayAdr14;
             RPadr2 = scPayAdr24;
             RPcty  = scPayCty4;
             RPst   = scPaySt4;
             RPzip  = scPayZip4;
             RPctry = scCtry4;
             RPadr3 = scAdr34;
          Endsl;
       Endsl;
       If RPctry <> *blanks and RPctry <> 'USA';
          RPadr2 = *blanks;
       Endif;
       RPbul1 = *zeros;
       RPseq# = PAseq#;
       RPstat = *blanks;
       RPuser = Q1user;
       RPpgm  = Q1pgm;
       RPdt   = DateTo7(SystemDate());
       RPtime = TimeTo6(SystemTime());
       Write prp06;

     p WriteViewCheck  e

       //===================================================================
       // PrintHardCopy- Write HardCopy Check
       //===================================================================
     p PrintHardCopy   b
     d PrintHardCopy   pi

       inTrDtChr = %Editc(PAtrDt:'X');
       inSeq#Chr = %Editc(PAseq#:'X');
       inFtyp    = outFtyp;
       inChkQ    = outChkQ;
       PrcChkVoucher(inCo#Chr:inFndChr:inFyrChr:inCaseChr:inTrDtChr:inSeq#Chr:
        inFtyp:inChkQ);

     p PrintHardCopy   e

       //===================================================================
       // WriteDARTS - Write DARTS for Recurring Payments
       //===================================================================
     p WriteDARTSRec   b
     d WriteDARTSRec   pi
       // If Recurring then write the DARTS Remark if specified
       If scDrtEaPmt = 'Y' or (scDrtLsPmt = 'Y'  and x = scNumFchk);
          // Initialize
          inKey = *blanks;
          inRmCd = *blanks;
          incomm = *blanks;
          inUser = *blanks;
          strStaf = *blanks;

          // Get Remark Code from Comp Payment Control File
          If scDrtLsPmt = 'Y' and x = scNumFchk;
             inRmCd = PCrClP;
          Else;
             inRmCd = PCrCeP;
          Endif;
          Chain (inRmCd) pct01;
          If %Found(STCTL1);
             decDays = PCdiDy;
             decDate8 = decpodt7;
             decRmkDt7 = DtCymd(dtPast(decDate8:decDays));

             inCdat = %Editc(dtMDYY(decRmkDt7):'X');

             // If Calculated DARTS remark date falls on weekend then
             // make that date fall on week days.
             chrSday = DowShort(decRmkDt7);
             If chrSday = 'Sat.' or chrSday = 'Sun.';
                If chrSday = 'Sat.';
                   decDays = 1;
                Else;
                   decDays = 2;
                Endif;

                // Recalculate the DARTS diary date
                decDate8 = decRmkDt7;
                //*** Beg Chg *** 04/07/25 *********************************
                //***decToDt7 = DtCymd(dtPast(decDate8:decDays));
                decRmkDt7 = DtCymd(dtPast(decDate8:decDays));
                //*** End Chg *** 04/07/25 *********************************

                inCdat = %Editc(dtMDYY(decRmkDt7):'X');
             Endif;

             Chain (CMclEx) pup02;
             If %Found(SMUPL);
                %Subst(inKey:1:3) = %Editc(CMCo#:'X');
                %Subst(inKey:4:3) = %Editc(CMfnd:'X');
                %Subst(inKey:7:3) = %Editc(CMfyr:'X');
                %Subst(inKey:10:7) = %Editc(CMcase:'X');
                inPnam = 'W0070R';
                inSecL = '000';
                inSbyL = 'N';
                inUser = UpUsr;
                inPgm  = Q1pgm;
                inIpgm  = Q1pgm;
                inseq#Chr = '00000';
                inApId  = *blanks;
                inIdoc  = *blanks;
                inIque  = *blanks;
                inImgF  = *blanks;
                inIpth  = *blanks;
                inPidx  = *zeros;
                inQpty  = *blanks;
                inDcTp  = *blanks;
                inCsId  = *blanks;
                inCode  = *blanks;
                inDcId  = CTdePg;
                inFold  = *blanks;
                inDiFl  = 'Y';
                inFl01  = *blanks;
                inFl02  = *blanks;
                %Subst(inComm:1:40) = CTdesc;
                %Subst(inComm:44:40) = %Trim(scPayNam);
                If CTdePg <> *blanks;
                   inFold = inKey;
                   inFl01 = 'Y';
                Else;
                   inFold = *blanks;
                   inFl01 = *blanks;
                Endif;
                If CTdpRg > *zero;
                   inFl02 = 'Y';
                Else;
                   inFl02 = *blanks;
                Endif;

                WriteDARTS(inKey:inComm:inCdat:inDcId:inPnam:inSecL:inUser:
                 inPgm:inFold:inDiFl:inIpgm:inFl01:inFl02:inRmCd:inseq#Chr:
                 insByL:inCom2:inApId:inIdoc:inIque:inImgF:inIpth:inPidx:
                 inQpty:inDcTp:inCsId:inCode);
                Chain (RPco#:RPfnd:RPfyr:RPcase:RPseq#) prp01;
                If %Found(WARPL);
                   RPsent = 'Y';
                   RPdSeq = %Dec(inseq#Chr:5:0);
                   Update prp01;
                Endif;
             Endif;
          Endif;

       Endif;
     p WriteDARTSRec   e

       //===================================================================
       // Check Invoice# and Duplicate Payment
       //===================================================================
     p chkDupPmntInv#  b
     d chkDupPmntInv#  pi
     d  inCo#Int                      3  0
     d  inFndInt                      3  0
     d  inFyrInt                      3  0
     d  inCaseInt                     7  0
     d  inSeq#                        5s 0
     d  inFein                        9  0
     d  inMod                         3  0
     d  inInv#                       10
     d  inFkey                        2
       If inInv# = *blanks;
          strMsg = 'The invoice # has not been entered.  To continue without +
           entering an invoice #, click "Submit". Otherwise, click +
           "Cancel" to return to the Payment Request screen';
          isInvoice# = PopTwoBtn(strMsg:'Invoice Number':
           'Submit':'Cancel');
          If isInvoice# = *on;
             BtnCancel = *off;
          Else;
             mgInvBlk = 'Invoice number cannot be blank';
             isInvalid = *on;
             erInvBlk = *on;
          Endif;
       Else;
          strMsg = 'An exact duplicate (Fein # & Invoice #) check +
           or check request has been found and you can not continue +
           with this Payment.  Press "Cancel" to return to the check +
           request.';
          Chain (CMco#:CMfnd:CMfyr:CMcase:fein:scInv#) prp02;
          If %Found(WARPLT);
             PopOneBtn(strMsg:'Duplicate Payment':'Cancel');
             isInvalid = *on;
          Else;
             Chain (CMco#:CMfnd:CMfyr:CMcase:fein:scInv#) ppa02;
             If %Found(WDPALT);
                PopOneBtn(strMsg:'Duplicate Payment':'Cancel');
                isInvalid = *on;
             Endif;
          Endif;
       Endif;

     p chkDupPmntInv#  e

       //===================================================================
       // ProcAddtSingle - Additional processing Single adjustment
       //===================================================================
     p ProcAddtSingle  b
     d ProcAddtSingle  pi

       // Update claim reserves
       Chain (inCo#int:inFndInt:inFyrInt:inCaseInt) pcm02;
       If %Found(WMCML1);
          x = %Lookup(PCreCd:dsClaimRsvTyp:1);
          If x > 0;
             svRemRsv = dsClaimRemRsv(X);
             Select;
             When scAsAmtWd = 'A';
                dsClaimRemRsv(X) = dsClaimRemRsv(X) - scAmount;
                dsClaimPmtRsv(X) = dsClaimPmtRsv(X) + scAmount;
             When scAsAmtWd = 'S';
                dsClaimRemRsv(X) = dsClaimRemRsv(X) + scAmount;
                dsClaimPmtRsv(X) = dsClaimPmtRsv(X) - scAmount;
             Endsl;
             If CMstat = 'C';
                dsClaimRemRsv(X) = svRemRsv;
             Endif;
          Endif;

          CMuser = Q1user;
          CMpgm  = Q1pgm;
          CMdt   = DateTo7(SystemDate());
          CMtime = TimeTo6(SystemTime());
          Update pcm02;
       Endif;

       // Adjust Employer file Reserves
       Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) pel01;
       If %Found(WDELL);
          svELout = ELouts;
          Select;
          When scAsAmtWd = 'A';
             ELouts = ELouts - scAmount;
             ELtClP = ELtClP + scAmount;
          When scAsAmtWd = 'S';
             ELouts = ELouts + scAmount;
             ELtClP = ELtClP - scAmount;
          Endsl;

          If CMstat = 'C';
             ELouts = svELout;
          Endif;

          ELuser = Q1user;
          ELpgm  = Q1pgm;
          ELdt   = DateTo7(SystemDate());
          ELtime = TimeTo6(SystemTime());
          Update pel01;
       Endif;

       // A reserve history record will be written in the amount of the
       // adjustment to keep claim MSTR (WMCMP) & RSV HIST (WMRHP) in balance.

       If CMstat = 'C';
          Select;
          When scAsAmtWd = 'S';
             xxAmt = scAmount * -1;
             xxExpl= sFlag;
          When scAsAmtWd = 'A';
             xxAmt = scAmount;
             xxExpl= aFlag;
          Endsl;
          RsvHst();
       Endif;

     p ProcAddtSingle  e

       //===================================================================
       // ProcAdditional - Additional processing
       //===================================================================
     p ProcAdditional  b
     d ProcAdditional  pi

       // If the claim is closed allocate reserves
       If CMstat = 'C';
          Select;
             When inMode = 'R' or inMode = 'T';
                inType = 'F';
                inReCd = PAreCd;
                inAmt  = PAamt;
                Callp AllocResClsClm(inCo#Int:inFndInt:inFyrInt:inCaseInt:
                 inReCd:inType:inAmt);
             Other;
                If scType = 'P' and RPstat = *blank;
                   inType = 'R';
                   inReCd = RPreCd;
                   inAmt  = RPamt;
                   Callp AllocResClsClm(inCo#Int:inFndInt:inFyrInt:inCaseInt:
                    inReCd:inType:inAmt);
                Endif;
                If scType = 'M' and RPstat = *blank;
                   inType = 'R';
                   inReCd = PAreCd;
                   inAmt  = PAamt;
                   Callp AllocResClsClm(inCo#Int:inFndInt:inFyrInt:inCaseInt:
                    inReCd:inType:inAmt);
                Endif;
          Endsl;
       Endif;

       // Manual Payments and Recovery
       If scType = 'M' or inMode = 'R' or inMode = 'T';
          // Adjust Claim Reserves
          Chain (inCo#int:inFndInt:inFyrInt:inCaseInt) pcm02;
          If %Found(WMCML1);
             x = %Lookup(PCreCd:dsClaimRsvTyp:1);
             If x > 0;
                Select;
                When inMode = 'R' or inMode = 'T';
                   dsClaimRemRsv(X) = dsClaimRemRsv(x) + PAamt;
                   dsClaimPmtRsv(X) = dsClaimPmtRsv(x) - PAamt;
                Other;
                   dsClaimRemRsv(X) = dsClaimRemRsv(x) - PAamt;
                   dsClaimPmtRsv(X) = dsClaimPmtRsv(x) + PAamt;
                   CMpcTr = CMpcTr + 1;
                Endsl;
                CMpgm = Q1pgm;
                CMuser = Q1user;
                CMdt   = DateTo7(systemDate());
                CMtime = Timeto6(systemTime());
                Update pcm02;
             Endif;
          Endif;

          // Adjust Employer file Reserves
          Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) pel01;
          If %Found(WDELL);
             Select;
             When inMode = 'R' or inMode = 'T';
                ELtClP = ELtClP - PAamt;
                ELouts = ELouts + PAamt;
             Other;
                ELtClP = ELtClP + PAamt;
                ELouts = ELouts - PAamt;
             Endsl;
             ELuser = Q1user;
             ELpgm  = Q1pgm;
             ELdt   = DateTo7(SystemDate());
             ELtime = TimeTo6(SystemTime());
             Update pel01;
          Endif;

          // Texas log record
          If isIdc and PAreTp = 'C';
             inRrn1 = DbRrn2;
             WriteTxLog(inRrn1);
          Endif;
       Endif;

       // Duplicate payment Auto-Diary if payment was pended and not Manual
       If CXdpPc <> *blank and outDupRtCd = '1' and scType <> 'M';
          inRrn1 = DbRrn1;
          Callp DupPmntAutoDry(inRrn1:outRtCd);
       Endif;

       // Requests and Manual not Recovery
       If inMode <> 'R';
          Select;
          When scRecurr = 'Y' and scType = 'P';
             WriteDARTSRec();
          When (scRecurr <> 'Y' or scType = 'M');
             //  If the LAST PMT FLAG = 'Y', call the EDI Generic Log API to
             //  create an A49 'FN' log record.
             inJrst = CMjrSt;
             inTrid = 'A49';
             Callp ProcEdiA49(intCo#:inJrSt:inTrId:intFnd:outRtCd:inEdiFmt:
              inElt:inElTd:inFroi:insDue:inRls:inCsDt:inEaDc:inHcPr:inFnRc:
              inHcEd);

             If ouTrtCd = 'Y';
                If scFnCmpPmt = 'Y';
                   inJrst  = CMjrSt;
                   outRtCd = *blank;
                   inTset  = 'A49';
                   inMtc   = 'S1';
                   Callp GenLogApi(inCo#Chr:inFndChr:inFyrChr:inCaseChr:inTset:
                    inMtc:inJrSt:outRtCd);
                Endif;
             Endif;
          Endsl;
       Endif;
     p ProcAdditional  e

       //===================================================================
       // UpdWdpap - Update the selected transaction.
       //===================================================================
     p UpdWdpap        b
     d UpdWdpap        pi

       clear svAccd;
       clear svAcct;
       clear svPgtp;
       clear svEob#;
       clear svEobs;
       clear svDpcd;
       clear svDpcu;
       Chain (xxCo#:scFnd:scFyr:scCase:xxSeq#) ppa00;
       If %found(WDPAL);
          svDsq# = PAdsq#;
          svAccd = PAcktp;
          svAcct = PAacct;
          svLtcd = PAltcd;
          svFnal = PAfnal;
          svPgtp = PApgtp;
          svEob# = PAeob#;
          svEobs = PAeobs;
          svDpcd = PAdpcd;
          svDpcu = PAdpcu;

          PAxfyr = scFyr;
          PAxcas = scCase;
          PAadst = 'A';
          PAuser = Q1user;
          PApgm  = Q1pgm;
          PAdt   = DateTo7(SystemDate());
          PAtime = TimeTo6(SystemTime());

          Update ppa00;

       Endif;
     p UpdWdpap        e

       //===================================================================
       // WrtWdpap - Write the Offsetting Adjustment Record
       //===================================================================
     p WrtWdpap        b
     d WrtWdpap        pi

       Setgt (CMco#:CMfnd:CMfyr:CMcase:*HIVAL) ppa00;
       Readpe (CMco#:CMfnd:CMfyr:CMcase) ppa00;
       //If not %found(WDPAL);
       If %Eof(WDPAL);
          PAseq# = 1;
       Endif;

          PAseq# += 1;
          PAchk# = scChk#;
          PAbasq = xxSeq#;
          If svType = 'R';
             PAamt = scOfAmount;
             PAadst = *Blanks;
             PAradj = 'Y';
          Else;
             PAamt = scOfAmount * -1;
             PAadst = 'A';
             PAradj = *Blanks;
          Endif;

          PAcktp = svAccd;
          PAacct = svAcct;

          clear PAoff#;
          Chain(N) (xxCo#:scFnd:scFyr:scCase) pcm02;
          If %Found(WMCML1);
             PAoff# = CMoff#;
          Endif;

          PAtrdt = DateTo7(SystemDate());
          PApodt = DateTo7(SystemDate());
          PAfein = scOfFe;
          PAmod = scOfMo;
          PAbr2 = scOfB2;
          PAbr7 = scOfB7;
          PAfrdt = scOfFr;
          PAtodt = scOfTo;
          PAwks = scOfWk;
          PAdays = scOfDy;
          PAtrcd = scPayCode;
          PAtype = 'A';
          PArecd = scReCd;
          PAretp = scRsvType;
          PAdesc = scOfDs;
          PAstat = *Blanks;
          PAltcd = svLtcd;
          PAfnal = svFnal;
          PAxfyr = scFyr;
          PAxcas = scCase;
          PArqst = Q1user;
          PAuser = Q1user;
          PApgm  = Q1pgm;
          PAdt   = DateTo7(SystemDate());
          PAtime = TimeTo6(SystemTime());
          clear PAbul1;
          clear PAeob#;
          clear PApgtp;
          clear PArcvd;
          clear PAdue;
          clear PAdpcd;
          clear PAdpcu;
          clear PArcdt;
          PApgtp = SVpgtp;
          PAeob# = SVeob#;
          PAeobs = SVeobs;
          PAdpcd = SVdpcd;
          PAdpcu = SVdpcu;
          PAdsq# = svDsq#;
          Chain (xxCo#:scFnd) pf201;
          If %Found(WDF2L) and F2ctyp = 'X';
             PAstat = 'X';
          Endif;
          PAiusr = Q1user;
          PAipgm = Q1pgm;
          PAidt  = DateTo7(SystemDate());
          PAitim = TimeTo6(SystemTime());

          Write ppa00;

          // For inMode = 'C'
          If inMode = 'C';
             //IDC processing offsetting adjustment
             If isModEnabled('IDC') and PAretp = 'C';
                inRrn1 = DBrrn2;
                WriteTxLog(inRrn1);
             Endif;

             //If the claim is closed, and the payment was transferred to
             // another claim or reserve, write a Reserve History record.
             If CMstat = 'C' and scReCd <> RCD(1);
                inType = 'O';
                inReCd = PAreCd;
                inAmt  = PAamt;
                Callp AllocResClsClm(xxCo#:scFnd:scFyr:scCase:
                 inReCd:inType:inAmt);
             Endif;
          Endif;

     p WrtWdpap        e

       //===================================================================
       // UpdWmcmp - Process the Offsetting Claim master record
       //===================================================================
     p UpdWmcmp        b
     d UpdWmcmp        pi

       Chain (xxCo#:scFnd:scFyr:scCase) pcm02;
       If %Found(WMCML1);
          y = %Lookup(scReCd:dsClaimRsvTyp:1);
          If y > 0;
             svRR = dsclaimRemRsv(y);
             If svType = 'R';
                dsClaimRemRsv(y) -= scOfAmount;
                dsClaimPmtRsv(y) += scOfAmount;
             Else;
                dsClaimRemRsv(y) += scOfAmount;
                dsClaimPmtRsv(y) -= scOfAmount;
                CMpctr -= 1;
             Endif;
          Endif;

          // Do not update remaining reserve if claim is closed
          If CMstat = 'C';
             dsClaimRemRsv(1) = svRR;
          Endif;

          CMuser = Q1user;
          CMpgm  = Q1pgm;
          CMdt   = DateTo7(SystemDate());
          CMtime = TimeTo6(SystemTime());

          Update pcm02;

          // Update the employer detail file with the outstanding
          //  reserve & total claims paid
          Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) pel01;
          If %Found(WDELL);
             svOuts =  Elouts;
             If svType = 'R';
                ELouts -= scOfAmount;
                ELtclp += scOfAmount;
             Else;
                ELouts += scOfAmount;
                ELtclp -= scOfAmount;
             Endif;
             // Do not update ELOUTS if cliam is closed
             If CMstat = 'C';
                ELouts = svOuts;
             Endif;

             ELuser = Q1user;
             ELpgm  = Q1pgm;
             ELdt   = DateTo7(SystemDate());
             ELtime = TimeTo6(SystemTime());

             Update pel01;
          Endif;
          If inMode = 'A' or inMode = 'C';

             // A reserve history record will be written in the amount of the
             // offset ADJ to keeo claim MSTR (WMCMP) & RSV HIST (WMRHP) in bala

             If CMstat = 'C';
                If svType = 'R';
                   xxAmt = scOfAmount;
                   xxExpl= incAdj;
                Else;
                   xxAmt = scOfAmount * -1;
                   xxExpl= decAdj;
                Endif;
             RsvHst();
             Endif;
sau       Endif;

       Endif;

     p UpdWmcmp        e

       //=================================================
       //  RsvHst - Write a reserve history record
       //=================================================
     p RsvHst          b
     d RsvHst          pi
        clear prh01;
        RHco#  = CMco#;
        RHfnd  = CMfnd;
        RHfyr = CMfyr;
        RHcase = CMcase;
        RRT(1) = PArecd;
        RSU(1) = xxAmt;
        RHexpl = xxExpl;
        RHuser = Q1user;
        RHpgm  = Q1pgm;
        RHdt   = DateTo7(SystemDate());
        RHtime = TimeTo6(SystemTime());
        RHidt  = DateTo7(SystemDate());
        RHiusr = Q1user;
        RHipgm = Q1pgm;
        RHitim = TimeTo6(SystemTime());
        RHinitRsv = GetInitRsvFlag(RHco#:RHfnd:RHfyr:RHcase);
        //*** Beg Add *** 10/29/24 ***************************************
        If inMode = 'C';
           RHefdt = DateTo7(SystemDate());
        Endif;
        //*** End Add *** 10/29/24 ***************************************

        Write prh01;

     p RsvHst          e

       //===================================================================
       // WrtWdrpp - Write into WDRPP (for correcting adjustment)
       //===================================================================
     p WrtWdrpp        b
     d WrtWdrpp        pi


       Chain(N) (PAco#:PAfnd:PAfyr:PAcase:xxSeq#) prp05;
       If not %Found();
          xxTrdt = dtcymd(scPoDtOff);
          Chain(N) (PAco#:PAfnd:PAfyr:PAcase:PAchk#:xxTrdt) prp06;
       Endif;
       If not %Found();
          RPco# = PAco#;
          RPfnd = PAfnd;
          RPfyr = PAfyr;
          RPcase = PAcase;
          RPchk# = PAchk#;

          // Load the Payee Name and Address
          If scOfFe <> 0 ;
          // Access Payee Demographic record
             xxFein = PAfein;
             xxMod = PAmod;
             Chain(N) (PAfein:PAmod) ppd01;
             If %Found(WMPDL);

                If PDdba1 = 'Y';
                   RPpnam = PDadr1;
                   RPadr1 = PDadr2;
                   RPadr2 = *Blanks;
                Else;
                   RPpnam = PDname;
                   If PDfsn <> *Blanks;
                      RPpNam = %trim(PDfsn) + ' ' + RPpNam;
                   Endif;
                   RPadr1 = PDadr1;
                   RPadr2 = PDadr2;
                Endif;

                RPcty = PDcty;
                RPst = PDst;
                RPzip = PDzip;

                If PDctry <> *Blank and PDctry <> 'USA';
                   RPctry = PDctry;
                   RPadr3 = PDadr3;
                Endif;
             Endif;
          Else;
             // Access Claimant Demographic record
             Chain (CMSS#) pcd01;
             If %found(WMCDL);
                If CDmid <> *blanks;
                   RPpnam = %Trim(CDfsn) + ' ' + %Trim(CDmid) + ' ' +
                    %Trim(CDlsn);
                Else;
                   RPpnam = %Trim(CDfsn) + ' ' + %Trim(CDlsn);
                Endif;
                RPadr1 = CDadr1;
                RPadr2 = CDadr2;
                RPcty  = CDcty;
                RPst   = CDst;
                RPzip  = CDzip;
                If CDctry <> *blank and CDctry <> 'USA';
                   RPctry = CDctry;
                   RPadr3 = CDadr3;
                Endif;
             Endif;
          Endif;

       Endif;

       // Remarks
       RPrmk1 = scComm1;
       RPrmk2 = scComm2;
       RPrmk3 = scComm3;
       RPrmDs = scComm4;

       RPtrdt = PAtrdt;
       RPseq# = PAseq#;
       RPuser = Q1user;
       RPdt = PAdt;
       RPtime = PAtime;

       Write prp06;

     p WrtWdrpp        e

       //===================================================================
       // WrtCorrAdjRcd - Write the CORRECTING ADJUSTMENT record(s)
       //===================================================================
     p WrtCorrAdjRcd   b
     d WrtCorrAdjRcd   pi

       For I1 = 1 to scNoPayee;
          If CD(I1) <> *Blanks;
             //*** Beg Chg *** 10/29/24 ***************************************
             //*** SetGT (xxCo#:scFnd:scFyr:scCase:*HIVAL) ppa00;
             //*** Readpe(N) (xxCo#:scFnd:scFyr:scCase) ppa00;
             If scClaim <> 0 and scClaim <> scCase and isToClmFnd = *on;
                SetGT (xxCo#:scFnd:decFyr:scClaim:*HIVAL) ppa00;
                Readpe(N) (xxCo#:scFnd:decFyr:scClaim) ppa00;
             Else;
                SetGT (xxCo#:scFnd:scFyr:scCase:*HIVAL) ppa00;
                Readpe(N) (xxCo#:scFnd:scFyr:scCase) ppa00;
             Endif;
             //*** End Chg *** 10/29/24 ***************************************
             If not %found(WDPAL);
                intPaySeq# = 0;
             Else;
                intPaySeq# = PAseq#;
                Clear ppa00;
             Endif;
             PAseq# = intPaySeq# + 1;
             PAco# = xxco#;
             PAfnd = scFnd;
             //*** Beg Chg *** 10/29/24 ***************************************
             //*** PAfyr = scFyr;
             //*** PAcase = scCase;
             //*** Beg Chg *** 05/06/25 ***************************************
             //*** PAfyr = decFyr;
             //*** PAcase = decCase;
             If scClaim <> 0 and scClaim <> scCase and inMode ='C';
                PAfyr = decFyr;
                PAcase = decCase;
             Else;
                PAfyr = scFyr;
                PAcase = scCase;
             Endif;
             //*** End Chg *** 05/06/25 ***************************************
             //*** End Chg *** 10/29/24 ***************************************
             PAchk# = scChk#;
             PAxfyr = scFyr;
             PAxcas = scCase;
             PAbasq = xxseq#;
             PAtrdt = DateTo7(SystemDate());
             PApodt = dtCyMd(PO(I1));
             PAamt = AM(I1);
             If svType <> 'R';
                PAamt = AM(I1);
                PAadst = *Blanks;
                PAradj = *Blanks;
             Else;
                PAamt = AM(I1) * -1;
                PAadst = 'A';
                PAradj = 'Y';
             Endif;
             Clear PAoff#;
             //*** Beg Chg *** 10/29/24 ***************************************
             //*** Chain(N) (xxCo#:scFnd:scFyr:scCase) pcm02;
             If scClaim <> 0 and scClaim <> scCase and isToClmFnd = *on;
                Chain(N) (xxCo#:scFnd:decFyr:scClaim) pcm02;
             Else;
                Chain(N) (xxCo#:scFnd:scFyr:scCase) pcm02;
             Endif;
             //*** End Chg *** 10/29/24 ***************************************
             If %Found(WMCML1);
                PAoff# = CMoff#;
             Endif;

             Select;
             When I1 = 1;
                If scPayee <> *zeros;
                   taxID#=scPayee;
                   PAfein=fein;
                   PAmod=modifier;
                Endif;
             When I1 = 2;
                If scPayee2 <> *zeros;
                   taxID#=scPayee2;
                   PAfein=fein;
                   PAmod=modifier;
                Endif;
             When I1 = 3;
                If scPayee3 <> *zeros;
                   taxID#=scPayee3;
                   PAfein=fein;
                   PAmod=modifier;
                Endif;
             When I1 = 4;
                If scPayee4 <> *zeros;
                   taxID#=scPayee4;
                   PAfein=fein;
                   PAmod=modifier;
                Endif;
             Endsl;

             PAbr2 = B2(I1);
             PAbr7 = B7(I1);
             PAfrdt = dtCyMd(FR(I1));
             PAtodt = dtCyMd(TO(I1));
             PAwks = WK(I1);
             PAdays = DY(I1);
             PAtrcd = CD(I1);
             PAtype = 'A';
             PArecd = RCD(I1);
             PAretp = RTP(I1);
             PAdesc = DS(I1);
             PAstat = *Blanks;
             PACKTP = SVACCD;
             PAACCT = SVACCT;
             PALTCD = SVLTCD;
             PAFNAL = SVFNAL;
             PArqst = Q1user;
             PAuser = Q1user;
             PApgm  = Q1pgm;
             PAdt  =  DateTo7(SystemDate());
             PAtime = TimeTo6(SystemTime());
             clear PAbul1;
             clear PAeob#;
             clear PApgtp;
             clear PArcvd;
             clear PAdue;
             clear PAdpcd;
             clear PAdpcu;
             clear PArcdt;
             PApgtp = SVpgtp;
             PAeob# = SVeob#;
             PAeobs = SVeobs;
             PAdpcd = SVdpcd;
             PAdpcu = SVdpcu;
             PAdsq# = svDsq#;
             Chain (xxCo#:scFnd) pf201;
             If %Found(WDF2L) and F2ctyp = 'X';
                PAstat = 'X';
             Endif;
             PAiusr = Q1user;
             PAipgm = Q1pgm;
             PAidt  = DateTo7(SystemDate());
             PAitim = TimeTo6(SystemTime());

             Write ppa00;

             If inMode = 'C';
                //IDC Processing for correcting adjustment.
                If isModEnabled('IDC') and PAretp = 'C';
                   inRrn1 =DBrrn2;
                   WriteTxLog(inRrn1);
                Endif;

                //If the claim is closed, and the payment was transferred to
                //another claim or reserve, write a Reserve History record.
                If CMstat = 'C' and scReCd <> RCD(1);   // Need to verify this f
                   inType = 'C';
                   inReCd = PAreCd;
                   inAmt  = PAamt;
                   //*** Beg Chg *** 10/29/24 **********************************
                   //*** Callp AllocResClsClm(xxCo#:scFnd:scFyr:scCase:
                   //***  inReCd:inType:inAmt);
                   If scClaim <> 0 and scClaim <> scCase and isToClmFnd = *on;
                      Callp AllocResClsClm(xxCo#:scFnd:decFyr:scClaim:
                       inReCd:inType:inAmt);
                   Else;
                      Callp AllocResClsClm(xxCo#:scFnd:scFyr:scCase:
                       inReCd:inType:inAmt);
                   Endif;
                   //*** End Chg *** 10/29/24 **********************************
                Endif;

             Endif;

             // Process the claim Master records effected
             //*** Beg Chg *** 10/29/24 ***************************************
             //*** Chain (xxCo#:scFnd:scFyr:scCase) pcm02;
             If scClaim <> 0 and scClaim <> scCase and isToClmFnd = *on;
                Chain (xxCo#:scFnd:decFyr:scClaim) pcm02;
             Else;
                Chain (xxCo#:scFnd:scFyr:scCase) pcm02;
             Endif;
             //*** End Chg *** 10/29/24 ***************************************
             If %Found(WMCML1);
                Select;
                When I1 = 1;
                   strPayCd = scPayCd;
                When I1 = 2;
                   strPayCd = scPayCd2;
                When I1 = 3;
                   strPayCd = scPayCd3;
                When I1 = 4;
                   strPayCd = scPayCd4;
                Endsl;
                Chain (CMco#:strPayCd) ppc01;
                If %Found(WTABL);
                   strReCd = PCreCd;
                   y = %Lookup(strReCd:dsClaimRsvTyp:1);
                   If y > 0;
                      svRR = dsclaimRemRsv(y);
                      If svType = 'R';
                         dsClaimRemRsv(y) += AM(I1);
                         dsClaimPmtRsv(y) -= AM(I1);
                      Else;
                         dsClaimRemRsv(y) -= AM(I1);
                         dsClaimPmtRsv(y) += AM(I1);
                         CMpctr -= 1;
                      Endif;
                   Endif;
                Endif;

                // Do not update remaining reserve if claim is closed
                If CMstat = 'C';
                   dsClaimRemRsv(1) = svRR;
                Endif;

                CMuser = Q1user;
                CMpgm  = Q1pgm;
                CMdt   = DateTo7(SystemDate());
                CMtime = TimeTo6(SystemTime());

                Update pcm02;

                // Update the employer detail file with the outstanding
                //  reserve & total claims paid
                Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) pel01;
                If %Found(WDELL);
                   svOuts =  Elouts;
                   If svType = 'R';
                      ELouts += AM(I1);
                      ELtclp -= AM(I1);
                   Else;
                      ELouts -= AM(I1);
                      ELtclp += AM(I1);
                   Endif;
                   // Do not update ELOUTS if cliam is closed
                   If CMstat = 'C';
                      ELouts = svOuts;
                   Endif;

                   ELuser = Q1user;
                   ELpgm  = Q1pgm;
                   ELdt   = DateTo7(SystemDate());
                   ELtime = TimeTo6(SystemTime());

                   Update pel01;
                Endif;
                // Write the View Check record to WDRPP
                If inMode = 'A' or inMode = 'C';
                   WriteViewCheck();

                   // A reserve history record will be written in the amount of
                   // offset ADJ to keeo claim MSTR (WMCMP) & RSV HIST (WMRHP) i
                   If CMstat = 'C';
                      If svType = 'R';
                         xxAmt = AM(I1) * -1;
                         xxExpl= decAdj;
                      Else;
                         xxAmt = AM(I1);
                         xxExpl= incAdj;
                      Endif;
                      RsvHst();
                   Endif;
                Endif;
             Endif;
          Endif;
       Endfor;

e-saup WrtCorrAdjRcd   e
