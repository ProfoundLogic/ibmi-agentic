     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      /title WA119R - API to Process Dividends
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H BNDDIR('WTBNDDIR')
      *  Created 05/28/13   C:136   Project: 147WAI
      *=========================================================================
      * DESCRIPTON
      *
      * Mode 1 - Returns *on in outResult if division
      *          qualifies for dividend processing, else *off
      *          Returns current amount of dividend premium stored
      *          in WDELP for Print Only bill. To qualify:
      *        -- CltID must be valid
      *        -- Division must be billed
      *        -- A/R Codes set up in Group Detail
      *        -- Dividend Plan must be active as of policy eff date
      *        -- Can't have invoked minimum premium
      *        -- Can't be cancelled
      *        -- Total of normal premium across policy must be in range
      * Mode 2 - Returns calculated dividend premium in outDivBill
      *          if division qualifies for dividend processing on final
      *          call of W8010R or W7720R or revised bill call of W8010R.
      * Mode 3 - Retrieves stored value of dividend premium in WDELP and
      *          writes A/R for dividend transactions in Billing Calc pgms
      *          (W7720R/W8010R/W8011R)
      * Mode 4 - Returns current amount of dividend premium stored
      *          in WDELP for Calc, Update, and Print Bill if division
      *          qualifies; else, it writes a DARTS Remark informing user
      *          that dividend setup must be reviewed.
      *=========================================================================
      * REVISIONS
      *
      * 04/30/19 C:062 Project: 152WAB
      *                Apply Optacomp Mods.
      *
      * 11/06/13 C:136 Support: WA114 84036
      *                Change div calc per client
      *
      *=========================================================================
     fsmcol     if   e           k disk    rename(pco01 : pco0l)
     fwddvl     if   e           k disk    rename(pdv01 : pdv0l)
     fwdell1    uf   e           k disk    rename(pel01 : pel0l1)
     fwdmpl     if   e           k disk    rename(pmp01 : pmp0l)
     fwdf1l     if   e           k disk    rename(pf101 : pf10l1)
     fwmeal14   if a e           k disk    rename(pea01 : pea0l14)
     fwmeml     if   e           k disk    rename(pem01 : pem0l)
     fwtdvl11   if   e           k disk    rename(pdv01 : pdv0l11)
      *=========================================================================
      * Entry parms
      *=========================================================================
     d*entry
     d WA119R          pr
     d  inCo#                              like(ELco#)
     d  inFnd                              like(ELfnd)
     d  inEmp#                             like(ELemp#)
     d  inDiv                              like(ELdiv)
     d  inFyr                              like(ELfyr)
     d  inPlanID                           like(ELdvcd)
     d  inMode                        1    const
     d  inEnv                         1    const
     d  outResult                      n
     d  outDivBill                         like(EAamt)
     d WA119R          pi
     d  inCo#                              like(ELco#)
     d  inFnd                              like(ELfnd)
     d  inEmp#                             like(ELemp#)
     d  inDiv                              like(ELdiv)
     d  inFyr                              like(ELfyr)
     d  inPlanID                           like(ELdvcd)
     d  inMode                        1    const
     d  inEnv                         1    const
     d  outResult                      n
     d  outDivBill                         like(EAamt)
      *========================================================================
      * Prototypes
      *========================================================================
     d/copy sPrDate
     d/copy wPrAr
     d/copy wPrEmplr
     d CalcARAdj       pr
     d  inDivPrem                    11  0
     d CalcDivAdj      pr            11  0
     d CalcDivPrem     pr            11  0
     d CalcPodt        pr             7  0
     d ChkCallStack    pr                  extpgm('SRCHKPGM')
     d  inPgm                        10
     d  outExistFlag                  1
     d ClearDivInfo    pr
     d  inCo#                              like(ELco#)
     d  inFnd                              like(ELfnd)
     d  inEmp#                             like(ELemp#)
     d  inDiv                              like(ELdiv)
     d  inFyr                              like(ELfyr)
     d GetCxProRate    pr             5  4
     d  inPolFrDt                     7  0
     d  inPolToDt                     7  0
     d  cxDate                        7  0
     d Dissolve        pr
     d  inMode                        1    const
     d  inAmount                           like(EAamt)
     d GetClientID     pr                  extpgm('RTVCLTID')
     d  outCltID                      3
     d GetCXStatus     pr              n
     d GetDivARCodes   pr
     d  outDivBilCode                      like(EAarcd)
     d  outDivPayCode                      like(EAarcd)
     d GetDivPlanInfo  pr
     d  inPlanID                           like(ELdvcd)
     d  inPolEfDt                          like(ELpfdt)
     d  outPlanType                        like(DVptyp)
     d  outGrpDivFlg                       like(DVgdiv)
     d  outErrFlg                      n
     d GetTotDivBill   pr            11  0
     d  inDiv                              like(ELdiv)
     d GetTotDivPaid   pr            11  0
     d  inDiv                              like(ELdiv)
     d GetTotNormPrm   pr            11  0
     d LoadFutArrays   pr
     d  inMode                        1    const
     d LeapCheck       pr             1  0
     d  frDate                        7  0
     d  toDate                        7  0
     d NegateDivAR     pr
     d  inDiv                              like(ELdiv)
     d PrepWMEAP       pr
     d  inMode                        1    const
     d  inAmount                           like(EAamt)
     d ValidCltID      pr              n
     d VerifyPrmRange  pr              n
     d  inPlanID                           like(ELdvcd)
     d WriteDarts      pr                  ExtPgm('DA700R')
     d  outKey                       40
     d  outComm                     256
     d  outCDat                       8
     d  outDcId                      12
     d  outPNam                      10
     d  outSecl                       3
     d  outUser                      10
     d  outPgm                       10
     d  outFold                      80
     d  outDiFl                       1
     d  outIPgm                      10
     d  outFl01                       1
     d  outFl02                       1
     d  outRmCd                       5
     d  outSeq#                       5
     d  outSbyl                       1
     d  outCom2                      19
     d  outApId                       2
     d  outIDoc                      12
     d  outIQue                      10
     d  outImgF                       1
     d  outIPth                      10
     d  outPIdx                       5
     d  outQPty                       1
     d  outDcTp                       5
     d  outCsId                       2
     d  outCode                       2
     d WriteDivPRmk    pr
     d WriteWMEAP      pr
     d  inMode                        1    const
     d  inAmount                           like(EAamt)
     d  inPodt                             like(EApodt)
      *========================================================================
      * Data Structures and Arrays
      *========================================================================
     d psds          esds                  extname(SZQ1P)
     d arDts           s                   like (EApodt) dim(99) inz(0)         future A/R dates
     d arAmts          s                   like (EAamt)  dim(99) inz(0)         future A/R amts
      *========================================================================
      * Stand Alone Fields
      *========================================================================
     d audPgmInStack   s              1
     d clientID        s              3    inz('000')
     d flgSavSummry    s              1
     d inProgram       s             10    inz('W7720R')
     d intSavCo#       s                   like(ELco#)
     d intSavFnd       s                   like(ELfnd)
     d intSavEmp#      s                   like(ELemp#)
     d intSavDiv       s                   like(ELdiv)
     d intSavFyr       s                   like(ELfyr)
     d intSavDivPrm    s                   like(ELdvpr)
     d intTotNormPrm   s             +2    like(ELenmp)
     d isCancelled     s               n
     d isQualified     s               n   inz(*off)
     d outDivBilCode   s                   like(EAarcd)
     d outDivPayCode   s                   like(EAarcd)
     d outErrFlg       s               n
     d outGrpDivFlg    s                   like(DVgdiv)
     d outPlanType     s                   like(DVptyp)
     d revPgmInStack   s              1
     d strBillDesc     s                   like(EAdesc)
     d strPaidDesc     s                   like(EAdesc)
     d sysDate         s                   like(EAdt)
     d sysTime         s                   like(EAtime)
      *========================================================================
      * Klists
      *========================================================================
     c     keyWDELL1     klist
     c                   kfld                    intSavCo#
     c                   kfld                    intSavFnd
     c                   kfld                    intSavFyr
     c                   kfld                    intSavEmp#
     c                   kfld                    intSavDiv
      *========================================================================
      * Main
      *========================================================================
      /free
       // Check Stack for Audit Calc (ELfaf may not be updated to 'A' yet)
       ChkCallStack (inProgram : audPgmInStack);
       If audPgmInStack = *on;
          ELfaf = 'A';
       Endif;
       // Check Stack for Rev Bill Calc to pull div prem from file
       inProgram = 'W8011R';
       ChkCallStack (inProgram : revPgmInStack);
       // Return if client is not eligile to process dividends
       If ValidCltID = *off;
         outResult = *off;
         Return;
       Endif;
       intSavCo#  = inCo#;
       intSavFnd  = inFnd;
       intSavEmp# = inEmp#;
       intSavDiv  = inDiv;
       intSavFyr  = inFyr;
       Chain(N) keyWDELL1 pel0l1;
       // Ensure that API returns 'N' if bill not run
       If ELfaf = *blank;
          outResult = *off;
          *inlr = *on;
          Return;
       Else;
          outResult = *on;
          intSavDivPrm = ELdvpr;
          flgSavSummry = ELarsm;
       Endif;
       If inPlanID = *blank;
          inPlanID = ELdvcd;
       Endif;
       If inPlanID = *blank;
          outResult = *off;
       Endif;
       // Ensure dividend A/R codes are set up in Group Detail
       GetDivARCodes(outDivBilCode : outDivPayCode);
       If outDivBilCode = *blank or outDivPayCode = *blank;
          outResult = *off;
       Endif;
       // Disqualify if Min Prem or State Rpt Div is Cx'd
       isCancelled = GetCxStatus();
       If ELemnp = 'Y' and ELfaf <> 'A' or
        ELamnp = 'Y' and ELfaf =  'A' or isCancelled;
          outResult = *off;
       Endif;
       // Dividend Plan must be valid and total normal
       //  premium across policy within min/max range
       GetDivPlanInfo(inPlanID : ELpfdt : outPlanType : outGrpDivFlg :
        outErrFlg);
       If outResult <> *off;
          Select;
          When revPgmInStack = *on;
             If ELdvpr <> 0;
                isQualified = *on;
             Endif;
          When outErrFlg = *off;
             intTotNormPrm = GetTotNormPrm();
             isQualified = VerifyPrmRange(inPlanID);
          Other;
             isQualified = *off;
             outResult = *off;
          Endsl;
       Endif;
       Select;
       When inMode = '1';
          outDivBill = ELdvpr;
       When inMode = '2';
          outDivBill = CalcDivPrem();
       When inMode = '3';
          sysDate = DateTo7(SystemDate);
          sysTime = TimeTo6(SystemTime);
          strBillDesc = GetARDesc(outDivBilCode);
          strPaidDesc = GetARDesc(outDivPayCode);
          outDivBill = CalcDivAdj();
       When inMode = '4';
          Select;
          When outResult = *off or isQualified = *off;
             outDivBill = 0;
             If inPlanID <> *blank;
                WriteDivPRmk();
             Endif;
             ClearDivInfo(inCo#:inFnd:inEmp#:inDiv:inFyr);
          Other;
             outDivBill = ELdvpr;
          Endsl;
       Endsl;
       *inlr = *on;
      /end-free
      *==============================================================
      * CalcARAdj  - Calculates the A/R Adjustments
      *==============================================================
     p CalcARAdj       b
     d CalcARAdj       pi
     d  inDivPrem                    11  0
     d  totDivBilled   s             11  0
     d  totDivPaid     s             11  0
     d  billedAdj      s                   like(EAamt)
     d  paidAdj        s                   like(EAamt)
      /free
       // Accumulate total of billed and paid dividend
       totDivBilled = GetTotDivBill(intSavDiv);
       totDivPaid   = GetTotDivPaid(intSavDiv);
       billedAdj = inDivPrem - totDivBilled;
       // Adjust payments off of division if not audited
       If ELfaf = 'A';
          paidAdj   = inDivPrem - totDivPaid;
       Else;
          paidAdj   = 0 - totDivPaid;
       Endif;
       PrepWMEAP('B' : billedAdj);
       PrepWMEAP('P' : paidAdj);
      /end-free
     p CalcARAdj       e
      *==============================================================
      * CalcDivAdj  - Calculates Dividend A/R Adjustment on each division
      *==============================================================
     p CalcDivAdj      b
     d CalcDivAdj      pi            11  0
     d intCalcDivPrm   s             11  0
     d intTotDivPrm    s             11  0
     d isQualified     s               n
     c     keyWDELL1a    klist
     c                   kfld                    intSavCo#
     c                   kfld                    intSavFnd
     c                   kfld                    intSavFyr
     c                   kfld                    intSavEmp#
      /free
       If flgSavSummry = 'Y';
          Setll keyWDELL1a pel0l1;
          Reade(N) keyWDELL1a pel0l1;
          Dow not(%eof);
             Select;
             When ELfaf <> 'A' and ELdvdf <> 'Y';
             When ELarsm = 'Y';
                intTotDivPrm = intTotDivPrm + ELdvpr;
                NegateDivAR(ELdiv);
             Endsl;
          Reade(N) keyWDELL1a pel0l1;
          Enddo;
          Chain(N) keyWDELL1 pel0l1;
          CalcARAdj(intTotDivPrm);
          intCalcDivPrm = intTotDivPrm;
       Else;
          Select;
          When ELfaf <> 'A' and ELdvdf <> 'Y';
             intCalcDivPrm = 0;
          Other;
             intCalcDivPrm = ELdvpr;
          Endsl;
          CalcARAdj(intCalcDivPrm);
       Endif;
       Return intCalcDivPrm;
      /end-free
     p CalcDivAdj      e
      *==============================================================
      * CalcDivPrem - Returns result of Div Prem Calculation
      *==============================================================
     p CalcDivPrem     b
     d CalcDivPrem     pi            11  0
     d cxDate          s              7  0
     d cxProRate       s              5  4 inz(1)
      /free
       If audPgmInStack = *on;
         ELfaf = 'A';
       Endif;
       // Retrieve normal premium from min prem file (WDMPP)
       //  because WDELP hasn't been fully updated yet at this
       //  point within the premium calculation.
       If inMode = '2';
          Chain (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) pmp0l;
          If %found;
       //*** beg chg ***** 11/06/13 *************************
       //*** ELanmp = MPanmp;
       //*** ELenmp = MPenmp;
             ELanpr = MPanpr;
             ELaapr = MPaapr;
       //*** end chg ***** 11/06/13 *************************
          Endif;
       Endif;
       Select;
       When outPlanType = 'F';
          Select;
          When outResult = *off or isQualified = *off;
             Return 0;
          When ELfaf = 'A';
       //*** beg chg ***** 11/06/13 *************************
       //*** Return %inth(ELanmp * ELdvpc * .01);
             Return %inth((ELaapr - ELprtd + ELater + ELadtr)
              * ELdvpc * .01);
       //*** end chg ***** 11/06/13 *************************
          When ELfaf <> 'A';
             cxDate = GetCxDate(Elco#:ELfnd:ELemp#:ELdiv);
             If cxDate <> 0;
                cxProRate = GetCxProRate(ELpfdt:ELptdt:cxDate);
             Endif;
       //*** beg chg ***** 11/06/13 *************************
       //*** Return %inth(ELenmp * ELdvpc * ELpror * cxProRate * .01);
             Return %inth((ELanpr - ELprtd + ELeter + ELedtr)
              * ELdvpc * cxProRate * .01);
       //*** end chg ***** 11/06/13 *************************
          Other;
             Return 0;
          Endsl;
       Other;
          Return 0;
       Endsl;
      /end-free
     p CalcDivPrem     e
      *==============================================================
      * CalcPodt - Calculates post date for writing A/R
      *==============================================================
     p CalcPodt        b
     d CalcPodt        pi                  like(EAdt)
      /free
       Select;
       When ELfaf = 'A';
          Return sysDate;
       When ELfaf <> 'A';
          If ELptdt > sysDate;
             Return ELptdt;
          Else;
             Return sysDate;
          Endif;
       Endsl;
      /end-free
     p CalcPodt        e
      *==============================================================
      * ClearDivInfo - Clear Dividend Info if policy doesn't
      *  qualify for dividend processing under the assigned plan.
      *==============================================================
     p ClearDivInfo    b
     d ClearDivInfo    pi
     d  inCo#                              like(ELco#)
     d  inFnd                              like(ELfnd)
     d  inEmp#                             like(ELemp#)
     d  inDiv                              like(ELdiv)
     d  inFyr                              like(ELfyr)
      /free
       Chain (inCo#:inFnd:inFyr:inEmp#:inDiv) pel0l1;
       If %found;
          Clear ELdvcd;
          Clear ELdvpr;
          Clear ELdvpc;
          Clear ELdvdf;
          Update pel0l1;
       Endif;
      /end-free
     p ClearDivInfo    e
      *==============================================================
      * CxProRate - returns Pro Rata factor if cx date exists on policy
      *==============================================================
     p GetCxProRate    b
     d GetCxProRate    pi             5  4
     d  inPolFrDt                     7  0
     d  inPolToDt                     7  0
     d  cxDate                        7  0
     d #ofActDays      s              3  0
     d #ofPolDays      s              3  0
      /free
       If inPolFrDt = inPolToDt;
          Return 0;
       Endif;
       If cxDate <= inPolFrDt;
          Return 0;
       Endif;
       if cxDate >= inPolToDt;
          Return 1;
       Endif;
       #ofActDays = dtDiff(DtMDYY(inPolFrDt):DtMDYY(cxDate))
        - leapCheck(inPolFrDt:cxDate);
       #ofPolDays = dtDiff(DtMDYY(inPolFrDt):DtMDYY(inPolToDt))
        - leapCheck(cxDate:inPolToDt);
        Return(h) #ofActDays/#ofPolDays;
      /end-free
     p GetCxProRate    e
      *==============================================================
      * Dissolve - Dissolve negative amounts over future A/R
      *==============================================================
     p Dissolve        b
     d Dissolve        pi
     d  inMode                        1    const
     d  inAmount                           like(EAamt)
     d  intIdx         s              2  0 inz(1)
      /Free
       For intIdx to 99;
          If arDts(intIdx) = 0 or inAmount = 0;
             Leave;
          Endif;
          If arAmts(intIdx) > 0;
             Select;
             When %Abs(inAmount) <= arAmts(intIdx);
                WriteWMEAP(inMode : inAmount : arDts(intIdx));
                inAmount = 0;
             When %Abs(inAmount) >  arAmts(intIdx);
                inAmount = inAmount + arAmts(intIdx);
                arAmts(intIdx) = arAmts(intIdx) * -1;
                WriteWMEAP(inMode : arAmts(intIdx) : arDts(intIdx));
             Endsl;
          Endif;
       EndFor;
       If inAmount < 0;
          WriteWMEAP(inMode : inAmount : sysDate);
       Endif;
      /End-free
     p Dissolve        e
      *==============================================================
      * GetCXStatus - Retrieves CX Status
      *==============================================================
     p GetCXStatus     b
     d GetCXStatus     pi              n
     d  cxDate         s                   like(EMcad1)
     d  stRptDiv       s                   like(EMdiv)
      /free
       stRptDiv = GetStRptDv(intSavCo# : intSavFnd : intSavEmp#);
       cxDate = GetCxDate(intSavCo# : intSavFnd : intSavEmp# : stRptDiv);
       If cxDate <> 0;
          Return *on;
       Else;
          Return *off;
       Endif;
      /end-free
     p GetCXStatus     e
      *==============================================================
      * GetDivARCode - Retrieves Dividend Codes from Group Detail
      *==============================================================
     p GetDivARCodes   b
     d GetDivARCodes   pi
     d  outDivBilCode                      like(EAarcd)
     d  outDivPayCode                      like(EAarcd)
      /free
       Chain (intSavCO# : intSavFnd : intSavFyr) pf10l1;
       outDivBilCode = F1dvbl;
       outDivPayCode = F1dvpd;
       Return;
      /end-free
     p GetDivARCodes   e
      *==============================================================
      * GetDivPlanInfo - Retrieves Dividend Plan Info
      *==============================================================
     p GetDivPlanInfo  b
     d GetDivPlanInfo  pi
     d  inPlanID                           like(ELdvcd)
     d  inPolEfDt                          like(ELpfdt)
     d  outPlanType                        like(DVptyp)
     d  outGrpDivFlg                       like(DVgdiv)
     d  outErrFlg                      n
      /free
       Setgt (ELco# : inPlanID : ELpfdt) pdv0l11;
       Readpe (ELco# : inPlanID) pdv0l11;
       Dow not(%eof);
          If DVefdt > ELpfdt or (DVexdt <> 0 and DVexdt <= ELpfdt);
             Readpe (ELco# : inPlanID) pdv0l11;
             Iter;
          Endif;
          outPlanType  = DVptyp;
          outGrpDivFlg = DVgdiv;
          Leave;
       Readpe (ELco# : inPlanID) pdv0l11;
       Enddo;
       If %eof;
          outErrFlg = *on;
       Else;
          outErrFlg = *off;
       Endif;
       Return;
      /end-free
     p GetDivPlanInfo  e
      *==============================================================
      * GetTotDivBill - Totals Dividend Billings on Division
      *==============================================================
     p GetTotDivBill   b
     d GetTotDivBill   pi            11  0
     d  inDiv                              like(ELdiv)
     d  totDivBilled   s             11  0
     c     keyWMEAL14    klist
     c                   kfld                    intSavCo#
     c                   kfld                    intSavFnd
     c                   kfld                    intSavEmp#
     c                   kfld                    inDiv
     c                   kfld                    intSavFyr
     c                   kfld                    outDivBilCode
      /free
       Setll keyWmeal14 pea0l14;
       Reade keyWmeal14 pea0l14;
       Dow not(%eof);
          totDivBilled = totDivBilled + EAamt;
       Reade keyWmeal14 pea0l14;
       Enddo;
       Return totDivBilled;
      /end-free
     p GetTotDivBill   e
      *==============================================================
      * GetTotDivPaid - Totals Dividend Payments on Division
      *==============================================================
     p GetTotDivPaid   b
     d GetTotDivPaid   pi            11  0
     d  inDiv                              like(ELdiv)
     d  totDivPaid     s             11  0
     c     keyWMEAL14    klist
     c                   kfld                    intSavCo#
     c                   kfld                    intSavFnd
     c                   kfld                    intSavEmp#
     c                   kfld                    inDiv
     c                   kfld                    intSavFyr
     c                   kfld                    outDivPayCode
      /free
       Setll keyWmeal14 pea0l14;
       Reade keyWmeal14 pea0l14;
       Dow not(%eof);
          totDivPaid = totDivPaid + EAamt;
       Reade keyWmeal14 pea0l14;
       Enddo;
       Return totDivPaid;
      /end-free
     p GetTotDivPaid   e
      *==============================================================
      * GetTotNormPrm - Totals Normal Premium Across Policy
      *==============================================================
     p GetTotNormPrm   b
     d GetTotNormPrm   pi            11  0
     d intNormPrm      s                   like(ELENMP)
     d cxDate          s              7  0
     d cxProRate       s              5  4 inz(1)
     c     keyWDELL1a    klist
     c                   kfld                    intSavCo#
     c                   kfld                    intSavFnd
     c                   kfld                    intSavFyr
     c                   kfld                    intSavEmp#
      /free
       Setll keyWDELL1a pel0l1;
       Reade(N) keyWDELL1a pel0l1;
       Dow not(%eof);
          If audPgmInStack = *on;
            ELfaf = 'A';
          Endif;
       // Retrieve normal premium from min prem file (WDMPP)
       //  because WDELP hasn't been fully updated yet at this
       //  point within the premium calculation.
          If inMode = '2';
             Chain (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) pmp0l;
             If %found;
                ELanmp = MPanmp;
                ELenmp = MPenmp;
             Endif;
          Endif;
          Select;
          When ELfaf = 'A';
             intNormPrm = ELanmp;
          Other;
             cxDate = GetCxDate(Elco#:ELfnd:ELemp#:ELdiv);
             If cxDate <> 0;
                cxProRate = GetCxProRate(ELpfdt:ELptdt:cxDate);
             Endif;
             intNormPrm = %inth(ELenmp * ELpror * cxProRate);
          Endsl;
          intTotNormPrm = intTotNormPrm + intNormPrm;
       Reade(N) keyWDELL1a pel0l1;
       Enddo;
       Chain(N) keyWDELL1 pel0l1;
       Return intTotNormPrm;
      /end-free
     p GetTotNormPrm   e
      *==============================================================
      * LeapCheck - returns 1 if the date range has a leap date in it
      *==============================================================
     p LeapCheck       b
     d LeapCheck       pi             1  0
     d  frDate                        7  0
     d  toDate                        7  0
     d alphaLeapDate   s              8
     d numLeapDate     s              8  0
     d validDate       s              8  0
      /free
       alphaLeapDate = %subst(%editc(frDate:'X'):1:3) + '0229';
       numLeapDate   = %int(alphaLeapDate);
       If numLeapDate <= frDate or numLeapDate >= toDate;
          Return 0;
       Endif;
       validDate = dtCheck(DtMDYY(numLeapDate));
       If validDate = 0;
          Return 0;
       Endif;
       If numLeapDate > frDate and numLeapDate < toDate;
          Return 1;
       Endif;
       alphaLeapDate = %subst(%editc(toDate:'X'):1:3) + '0229';
       numLeapDate   = %int(alphaLeapDate);
       If numLeapDate >= toDate;
          Return 0;
       Endif;
       validDate = dtCheck(DtMDYY(numLeapDate));
       If validDate = 0;
          Return 0;
       Endif;
       If numLeapDate > frDate and numLeapDate < toDate;
          Return 1;
       Endif;
          Return 0;
      /end-free
     p LeapCheck       e
      *==============================================================
      * LoadFutArrays - Loads A/R Dates of Dividend Installments
      *==============================================================
     p LoadFutArrays   b
     d LoadFutArrays   pi
     d  inMode                        1    const
     d  intIdx         s              2  0
     d  postDate       s                   like(EApodt)
     d  divArCode      s                   like(EAarcd)
     c     keyWMEAL14    klist
     c                   kfld                    intSavCo#
     c                   kfld                    intSavFnd
     c                   kfld                    intSavEmp#
     c                   kfld                    intSavDiv
     c                   kfld                    intSavFyr
     c                   kfld                    divARCode
     c     keyWMEAL14A   klist
     c                   kfld                    intSavCo#
     c                   kfld                    intSavFnd
     c                   kfld                    intSavEmp#
     c                   kfld                    intSavDiv
     c                   kfld                    intSavFyr
     c                   kfld                    divARCode
     c                   kfld                    postDate
      /free
       Clear arDts;
       Clear arAmts;
       postDate = sysDate;
       Select;
       When inMode = 'B';
          divARCode = outDivBilCode;
       When inMode = 'P';
          divARCode = outDivPayCode;
       Endsl;
       // Load Dates
       Setll keyWmeal14a pea0l14;
       Reade keyWmeal14  pea0l14;
       Dow not(%eof);
          intIdx = %Lookup(EApodt : arDts);
          If intIdx = 0;
             intIdx = %Lookup(0 : arDts);
             arDts(intIdx) = EApodt;
             arAmts(intIdx) = EAamt;
          Else;
             arAmts(intIdx) = arAmts(intIdx) + EAamt;
          Endif;
       Reade keyWmeal14  pea0l14;
       Enddo;
      /end-free
     p LoadFutArrays   e
      *==============================================================
      * NegateDivAR - Negates Div A/R off summarized divisions not
      *               flagged as state reporting.
      *==============================================================
     p NegateDivAR     b
     d NegateDivAR     pi
     d  inDiv                              like(ELdiv)
     d  totDivBilled   s                   like(EAamt)
     d  totDivPaid     s                   like(EAamt)
      /free
       // Return if state reporting division
       If inDiv = intSavDiv;
          Return;
       Endif;
       // Accumulate total of billed and paid dividend
       totDivBilled = GetTotDivBill(inDiv) * -1;
       totDivPaid   = GetTotDivPaid(inDiv) * -1;
       PrepWMEAP('B' : totDivBilled);
       PrepWMEAP('P' : totDivPaid);
      /end-free
     p NegateDivAR     e
      *==============================================================
      * PrepWMEAP - Prep for writing A/R records
      *==============================================================
     p PrepWMEAP       b
     d PrepWMEAP       pi
     d  inMode                        1    const
     d  inAmount                           like(EAamt)
     d  inPodt         s                   like(EApodt)
     d  inCount        s              1  0
      /free
       Select;
       When inAmount = 0;
          Return;
       When inAmount > 0;
          inPodt = CalcPodt();
          WriteWMEAP(inMode : inAmount : inPodt);
       When inAmount < 0;
       // do for billings and payments
       //  load future dates and amounts
       //  dissolve negative amount against future billing amounts until
       //  exhausted. If surplus exists, post to current date.
          LoadFutArrays(inMode);
          Dissolve(inMode : inAmount);
       Endsl;
      /end-free
     p PrepWMEAP       e
      *==============================================================
      * ValidCltID - Returns *on if Client is authorized for Dividend
      *==============================================================
     p ValidCltID      b
     d ValidCltID      pi              n
      /free
       GetClientID(clientID);
       Select;
       //*** Begin Chg *** 04/30/19 *********************************
       //*** When clientID  = '114';
       When clientID  = '114' Or clientID = '152';
       //*** End   Chg *** 04/30/19 *********************************
          Return *on;
       Other;
          Return *off;
       Endsl;
      /end-free
     p ValidCltID      e
      *==============================================================
      * VerifyPrmRange - Returns *on if policy premium is within range
      *==============================================================
     p VerifyPrmRange  b
     d VerifyPrmRange  pi              n
     d  inPlanID                           like(ELdvcd)
      /free
       Setgt  (ELco# : inPlanID : intTotNormPrm) pdv0l;
       Readpe (ELco# : inPlanID) pdv0l;
       Dow not(%eof);
          If intTotNormPrm >= DVmnlv and
           intTotNormPrm <= DVmxlv;
             Return *on;
          Endif;
       Readpe (ELco# : inPlanID) pdv0l;
       Enddo;
       Return *off;
      /end-free
     p VerifyPrmRange  e
      *==============================================================
      * WriteDivPRmk - Write DARTS Remark if division doesn't qualify
      *                for diviend
      *==============================================================
     p WriteDivPRmk    b
     d WriteDivPRmk    pi
     d  outKey         s             40
     d  outComm        s            256
     d  outCDat        s              8
     d  outDcId        s             12
     d  outPNam        s             10
     d  outSecl        s              3
     d  outUser        s             10
     d  outPgm         s             10
     d  outFold        s             80
     d  outDiFl        s              1
     d  outIPgm        s             10
     d  outFl01        s              1
     d  outFl02        s              1
     d  outRmCd        s              5
     d  outSeq#        s              5
     d  outSbyl        s              1
     d  outCom2        s             19
     d  outApId        s              2
     d  outIDoc        s             12
     d  outIQue        s             10
     d  outImgF        s              1
     d  outIPth        s             10
     d  outPIdx        s              5
     d  outQPty        s              1
     d  outDcTp        s              5
     d  outCsId        s              2
     d  outCode        s              2
     d  strCo#         s              3
     d  strFnd         s              3
     d  strEmp#        s              9
     d  strDiv         s              5
     d  strFyr         s              3
     d  strDefFlatVbg  s              3

      /free
       Chain (intSavCo#) pco0l;
       If COprth = *blank;
          Return;
       Else;
          outRmcd = COprth;
       Endif;
       strCo#  = %Editc(inCo#  : 'X');
       strFnd  = %Editc(inFnd  : 'X');
       strEmp# = %Editc(inEmp# : 'X');
       strDiv  = %Editc(inDiv  : 'X');
       strFyr  = %Editc(inFyr  : 'X');
       If ELdvdf = 'Y';
          strDefFlatVbg = 'Yes';
       Else;
          strDefFlatVbg = 'No';
       Endif;
       outKey  = strCo# + strFnd + strEmp# + strDiv;
       outKey  = %subst(outKey:1:39) + inEnv;
       outComm = 'Policy year ' + strFyr + ' does not meet ' +
        'criteria for dividend plan ' + %trim(ELdvcd) + '.  ' +
        'Percentage: ' + %editc(ELdvpc:'3') + '%' +
        '  Deferred Flat - ' + strDefFlatVbg;
       outCDat = %Editc(DateTo8(systemDate()):'X');
       outPNam = 'W0050R';
       outUser = Q1user;
       outPgm  = Q1pgm;
       outIPgm = Q1pgm;
       WriteDARTS (outKey  : outComm : outCDat : outDcId :
                   outPNam : outSecl : outUser : outPgm  :
                   outFold : outDiFl : outIPgm : outFl01 :
                   outFl02 : outRmCd : outSeq# : outSbyl :
                   outCom2 : outApId : outIDoc : outIQue :
                   outImgF : outIPth : outPIdx : outQPty :
                   outDcTp : outCsId : outCode );
      /end-free
     p WriteDivPRmk    e
      *====================================================================
      * WriteWMEAP - Write Records in WMEAP
      *====================================================================
     p WriteWMEAP      b
     d WriteWMEAP      pi
     d  inMode                        1    const
     d  inAmount                           like(EAamt)
     d  inPodt                             like(EApodt)
      /free
       EAco#  = intSavCo#;
       EAfnd  = intSavFnd;
       EAfyr  = intSavFyr;
       EAemp# = intSavEmp#;
       EAdiv  = ELdiv;
       EAarty = inMode;
       Select;
       When inMode = 'B';
          EAarcd = outDivBilCode;
          EAdesc = strBillDesc;
       When inMode = 'P';
          EAarcd = outDivPayCode;
          EAdesc = strPaidDesc;
       Endsl;
       If EAarcd = *blank;
          Return;
       Endif;
       EAamt  = inAmount;
       EAtrdt = sysDate;
       EApodt = inPodt;
       EAedit = *blank;
       EAstat = *blank;
       EAuser = Q1user;
       EApgm  = Q1pgm;
       EAtime = sysTime;
       EAdt   = sysDate;
       EAbul1 = 0;
       EAchk# = 0;
       EAninv = 0;
       EAlate = *blank;
       EAfl01 = *blank;
       EAfl02 = *blank;
       EAdt01 = 0;
       EAdt02 = 0;
       EAcctp = *blank;
       EAcnum = *blank;
       EAcexp = 0;
       EAcnam = *blank;
       EActyp = 'P';
       EArdiv = ELdiv;
       Write pea0l14;
      /end-free
     p WriteWMEAP      e
