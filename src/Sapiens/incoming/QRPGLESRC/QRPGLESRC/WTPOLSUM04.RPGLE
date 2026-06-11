     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Premium information, All Billing division
     h*               detail box - TAB 1.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLSUM
     h*  Calls to:
     h*
     h*  Created on 04/20/2012 by C:190 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*                                                               *
     h* 12/23/21 C:65  Support : 99801 (Luba)                         *
     h*                When accumulating annual premium do not add    *
   h*                in premium amount twice when no exp cnst.      *
     h*
     h*    07/02/21 C:318 Support: 96295
     h*                  Increaed length of variable to accomodate    *
     h*                  outstanding resreves of all divisions        *
     h*
     h*    01/02/14 C:295 Support: WA133/84219                        *
     h*                  The number of claims in the policy are not   *
     h*                  displayed with origianl value. The number of *
     h*                  open Claim and closed claims and total claims*
     h*                  The number Claims should be current values.  *
     h*                  Added procedure :LoadOthClmInf               *
     h*
   h*    10/07/13 C:190 Project 25WSS                               *
   h*                   Load the total of the "Accumulated -        *
   h*                   Claims Liability" for TASB.                 *
     h*
     h*
     h*
     h**********************************************************************
     fwdelp     if   e             disk    rename(pel01:pel0p)
     fwmldl1    if   e           k disk    rename(pld01:pld01)
     fwdell     if   e           k disk    rename(pel01:pel01)
     fwdell8    if   e           k disk    rename(pel01:pel08)
     fwtabl     if   e           k disk    include(pbs01)
     f****** Beg Add ******* 10/07/13 ************************
     fwddal1    if   e           k disk    rename(pda01:pda01)
     f****** End Add ******* 10/07/13 ************************
     f*** Begin Add *** 01/02/14 **************************************
     fWmcmld    if   e           k disk    rename(pcm01:pcm01)
     f*** End Add *** 01/02/14 ****************************************

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy wPrEmpLr

      * *ENTRY
     DPolicySumm04     pr                  ExtPgm('WTPOLSUM04')
     D                                9  0
     D                            32767
     D                                1
      *
     DPolicySumm04     pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
     d* Load Premium Information(Manual, Standard, Normal, Expense & Annual Premium).
     d  LoadOthPrmInf  pr

     d*** Begin Add *** 01/02/14 **************************************
     d* Load Claim Information (No of open Claims,Closed Claims and paid claims).
     d  LoadOthClmInf  pr
     d*** End Add *** 01/02/14 ****************************************

     d numEdtCde       c                   const('N')
     d intCo#          s                   like(ELco#)
     d intFnd          s                   like(ELfnd)
     d intFyr          s                   like(ELfyr)
     d intdiv          s                   like(ELdiv)
     d decManPrm       s                   like(ELampr)
     d decStdPrm       s                   like(ELaspr)
     d decNrmPrm       s                   like(ELanmp)
     d decExpConst     s                   like(ELprtd)
     d decAunPrm       s                   like(ELaapr)
     d xxExCo          s                   like(ELprtd)
     d intClsClms      s                   like(ELtclc)
     d intOpnClms      s                   like(ELtclc)
     d intTotClms      s                   like(ELt#cc)
     d intTotClmsPmt   s                   like(ELt#cl)
     d****** Beg Chg ******* 07/02/21 ************************
     d****intOutRsvrs     s                   like(ELouts)
     d intOutRsvrs     s             15  2
     d****** End Chg ******* 07/02/21 ************************
     d statRptDiv      s                   like(ELdiv)
     d****** Beg Add ******* 10/07/13 ************************
     d decAccClmAmt    s                   like(DAamtb)
     d****** End Add ******* 10/07/13 ************************
     d xxKey           s             10
     d paymentPlan     s             60

     d*** Begin Add *** 01/02/14 **************************************
     dXXtotP           s              7  0 Inz
     dXXtotb           s              7  0 Inz
     dXXtotc           s              7  0 Inz
     dXXTotO           s              7  0 Inz
     d*** End Add *** 01/02/14 *****************************************

     c* Large Ded Discount file
     c     keyWMLDL1     Klist
     c                   Kfld                    ELCo#
     c                   Kfld                    ELFnd
     c                   Kfld                    ELEmp#
     c                   Kfld                    statRptDiv
     c                   Kfld                    ELfyr

     c* Detail file
     c     keyWDELL      Klist
     c                   Kfld                    ELCo#
     c                   Kfld                    ELFnd
     c                   Kfld                    ELEmp#
     c                   Kfld                    statRptDiv
     c                   Kfld                    ELfyr

     c     keyWDELL8     Klist
     c                   Kfld                    ELco#
     c                   Kfld                    Elfnd
     c                   Kfld                    ELemp#
     c                   Kfld                    Elfyr

     c     keyTable      klist
     c                   kfld                    ELco#
     c                   kfld                    xxKey
     C*** Begin Add *** 01/02/14 **************************************
     c     Cmkey         Klist
     c                   Kfld                    ELCo#
     c                   Kfld                    ELFnd
     c                   Kfld                    ElFyr
     c                   Kfld                    ELEmp#
     c*                  Kfld                    statRptDiv
     C*** End Add *** 01/02/14 ****************************************
      *
      /Free

       Chain(n) inRRN pel0p;


       statRptDiv = GetStRptDv(ELco#:ELfnd:ELemp#);
       Chain(n) keyWDELL pel01;

        // set key values
       intCo# = ELco#;
       intFnd = ELfnd;
       intFyr = ELfyr;
       intdiv = statRptDiv;
       Clear xxExCo;
       Clear decManPrm;
       Clear decStdPrm;
       Clear decNrmPrm;
       Clear decExpConst;
       Clear decAunPrm;
       Clear intClsClms;
       Clear intOpnClms;
       Clear intTotClms;
       Clear intTotClmsPmt;
       Clear intOutRsvrs;

       outJSON = ' ';

       // Populate Retrospective information.
       BuildJSON(outJSON :'OutPrRetMinF' :%editc(ELmnfc:numEdtcde));
       BuildJSON(outJSON :'OutPrRetMaxF' :%editc(ELmxfc:numEdtcde));
       BuildJSON(outJSON :'OutPrLosCvrF' :%editc(ELlcvf:numEdtcde));

       // Populate Large Ded information.
       Chain(n) keyWMLDL1 PLD01;
       If %found;
       BuildJSON(outJSON :'OutPrLDedAmt' :%editc(LDamp1:numEdtcde));
       BuildJSON(outJSON :'OutPrLDedAgg' :%editc(LDamg1:numEdtcde));
       Endif;

       // Populate Payment Plan and Desc.
       Clear xxKey;
       xxKey = %editc(ELbscd : 'X');
       Chain keyTable PBS01;
       paymentPlan = %trimr(xxKey) + ' ' + BSdesc;
       BuildJSON(outJSON :'OutPrLPmtPln' :paymentPlan);

       //* Load Premium Information(Manual, Standard, Normal, Expense & Annual Premium). TAB 1.
       LoadOthPrmInf();
       // Begin Add *** 01/02/14 **************************************
       //* Load Claim Information (No of open Claims,Closed Claims and paid claims).
       LoadOthClmInf();
       // End Add *** 01/02/14 ****************************************
       BuildJSON(outJSON :'OutManPrm1' :%editc(decManPrm:numEdtcde));
       BuildJSON(outJSON :'OutStdPrm1' :%editc(decStdPrm:numEdtcde));
       BuildJSON(outJSON :'OutNrmPrm1' :%editc(decNrmPrm:numEdtcde));
       BuildJSON(outJSON :'OutExpConst1':%editc(decExpConst:numEdtcde));
       BuildJSON(outJSON :'OutAunPrm1' :%editc(decAunPrm:numEdtcde));
       BuildJSON(outJSON :'OutClsClms1' :%editc(intClsClms:numEdtcde));
       BuildJSON(outJSON :'OutTotClms1' :%editc(intTotClms:numEdtcde));
       BuildJSON(outJSON :'OutTotClmsPmt1' :%editc(intTotClmsPmt:numEdtcde));
       BuildJSON(outJSON :'OutOutRsvrs1' :%editc(intOutRsvrs:numEdtcde));
       BuildJSON(outJSON :'OutOpnClms1' :%editc(intOpnClms:numEdtcde));

       //** Begin Add *** 10/07/13 ****************************************
       //* Load the total of the Accumulated Claims Liability  TAB 1.
       decAccClmAmt = 0;
       Chain keyWMLDL1 PDA01;
       DoW not %eof(WDDAL1);
          decAccClmAmt = decAccClmAmt + DAamtb;
          ReadE keyWMLDL1 PDA01;
       Enddo;
       BuildJSON(outJSON :'OutAccumClmLib' :%editc(decAccClmAmt:numEdtcde));
       //** End   Add *** 10/07/13 ****************************************

       rtnCode = ' ';
       Return;
      /End-Free

       //===================================================================
       // Load Premium Information(Manual, Standard, Normal, Expense & Annual Premium).
       //===================================================================
     P LoadOthPrmInf   B
     D LoadOthPrmInf   PI
      /free

       Setll  keyWDELL8 PEL08;
       Reade  keyWDELL8 PEL08;
       DoW not %eof;

       //* Manual Premium
          If ELfaf =  'A';
             decManPrm = decManPrm + ELampr;
             If ELamnp = 'Y';
                decManPrm = decManPrm - (ELain1 + Elain2 + Elawav);
             Endif;
          Else;
             decManPrm = decManPrm + ELmapr;
             If ELemnp = 'Y';
                decManPrm = decManPrm - (ELein1 + ELein2 + ELewav);
             Endif;
          Endif;

       //* Standard Premium
          If  ELfaf = 'A';
              decStdPrm = decStdPrm + ELaspr;
              If ELamnp = 'Y' and ELwvfl <> 'M' and
                 ELwvfl <> 'J' and ELwvfl <> 'E';
                 decStdPrm = decStdPrm - ELawav;
              Endif;
          Else;
              decStdPrm = decStdPrm + ELstpr;
              If ELemnp = 'Y' and  ELwvfl <> 'M' and
                 ELwvfl <> 'J' and ELwvfl <> 'E';
                 decStdPrm = decStdPrm - ELewav;
              Endif;
          Endif;

       //* Normal Premium
          If Elfaf = 'A';
             decNrmPrm = decNrmPrm + ELanmp;
          Else;
             decNrmPrm = decNrmPrm + ELenmp;
          Endif;

       //* Expense Constant
          If  ELfaf = 'A' or ELffl9 = 'Y';
              decExpConst = decExpConst + ELprtd;
              xxExCo =    ELprtd;
          Else;
              If  ELemnp = 'Y' or Elprtd = *Zero;
                  decExpConst = decExpConst +  ELprtd;
                  xxExCo =  ELprtd;
              Else;
                  xxExCo = ELprtf;
                 decExpConst = decExpConst + xxExCo;
              Endif;
          Endif;

       //* Annual Premium
          If  ELfaf= 'A';
              decAunPrm = decAunPrm + ELaapr;
          Else;
          //*** begin chg ***  12/23/21  ****************************************
          //*****    If ELemnp   =  'Y' and ELprtd = *zero;
          //*****       decAunPrm =  decAunPrm + (ELenmp + ELeter + ELedtr + ELearc);
          //*****       If ELemnp = 'Y';
          //*****          decAunPrm = decAunPrm + ELanpr;
          //*****       Endif;
          //*****    Else;
          //*****       decAunPrm = decAunPrm + (ELenmp + xxExCo + ELeter + ELedtr +
          //*****        ELearc);
          //*****    Endif;
              If ELemnp   =  'Y';
                 decAunPrm = decAunPrm + ELanpr;
              Else;
                 If elprtd = 0;
                    decAunPrm =  decAunPrm + ELenmp + ELeter + ELedtr + ELearc;
                 Else;
                    decAunPrm = decAunPrm + (ELenmp + xxExCo + ELeter + ELedtr +
                     ELearc);
                 Endif;
              Endif;
          //*** end chg *****  12/23/21  ****************************************
          Endif;

       // Begin Del *** 01/02/14 **************************************
       //* Claims Info
       //*** intClsClms    = ELtclc + intClsClms;
       //*** intTotClms    = ELt#cc + intTotClms;
       //*** intTotClmsPmt = ELt#cl + intTotClmsPmt;
       // End Del *** 01/02/14 ****************************************
          intOutRsvrs   = ELouts + intOutRsvrs;
          Reade  keyWDELL8 PEL08;
       Enddo;

       // Begin Del *** 01/02/14 **************************************
       //*** intOpnClms    = intTotClms  - intClsClms;
       // End Del *** 01/02/14 ****************************************

      /end-free
     P LoadOthPrmInf   E

       //===================================================================
       // Load Claim Information (No of open Claims,Closed Claims and paid claims).
       //===================================================================
     P LoadOthClmInf   B
     D LoadOthClmInf   PI
      /free
       //* Claims Info

           Setll Cmkey Pcm01;
           Reade Cmkey Pcm01;
           DoW not %eof;
           if CMpctr >= 1;
           XXtotP = XXtotP+1;                    // # of Claims
           Endif;
           XXtotc = XXtotc+1;                    // Total Claims Count

           if Cmstat = 'C' ;
           XXTotb = XXTotb + 1;                  // Total number of closed claims
           Endif;

           if Cmstat = 'O' ;
           XXTotO = XXTotO + 1;                  // Total number of Open claims
           Endif;


           //intOutRsvrs   = ELouts + intOutRsvrs;
           Reade Cmkey Pcm01;
           Enddo;

           intClsClms    = XXtotb ;
           intTotClms    = XXtotc ;
           intTotClmsPmt = XXtotP ;
           intOpnClms    = XXtotO ;

      /end-free
     P LoadOthClmInf   E

