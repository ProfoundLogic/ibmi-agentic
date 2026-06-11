     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Premium information, Billing division
     h*               detail box - TAB 2.
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
   h*    10/07/13 C:190 Project 25WSS                               *
   h*                   Load the total of the "Accumulated -        *
   h*                   Claims Liability" for TASB.                 *
     h*
     h**********************************************************************
     fwdelp     if   e             disk    rename(pel01:pel0p)
     fwmldl1    if   e           k disk    rename(pld01:pld01)
     fwdell8    if   e           k disk    rename(pel01:pel08)
     fwtabl     if   e           k disk    include(pbs01)
     f****** Beg Add ******* 10/07/13 ************************
     fwddal1    if   e           k disk    rename(pda01:pda01)
     f****** End Add ******* 10/07/13 ************************

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

      * *ENTRY
     DPolicySumm01     pr                  ExtPgm('WTPOLSUM01')
     D                                9  0
     D                            32767
     D                                1
      *
     DPolicySumm01     pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
     d  LoadOthPrmInf  pr

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
     d intOutRsvrs     s                   like(ELouts)
     d****** Beg Add ******* 10/07/13 ************************
     d decAccClmAmt    s                   like(DAamtb)
     d****** End Add ******* 10/07/13 ************************
     d xxKey           s             10
     d paymentPlan     s             60

     c* Large Ded Discount file
     c     keyWMLDL1     Klist
     c                   Kfld                    ELco#
     c                   Kfld                    ELfnd
     c                   Kfld                    ELemp#
     c                   Kfld                    ELdiv
     c                   Kfld                    ELfyr

     c     keyTable      klist
     c                   kfld                    ELco#
     c                   kfld                    xxKey
      *
      /Free

       Chain(n) inRRN pel0p;
        // set key values
       intCo# = ELco#;
       intFnd = ELfnd;
       intFyr = ELfyr;
       intdiv = ELdiv;
       Clear xxExCo;

       Chain(n) keyWMLDL1 PLD01;

       outJSON = ' ';

       // Populate Retrospective information.
       BuildJSON(outJSON :'OutRetMinF' :%editc(ELmnfc:numEdtcde));
       BuildJSON(outJSON :'OutRetMaxF' :%editc(ELmxfc:numEdtcde));
       BuildJSON(outJSON :'OutLosCvrF' :%editc(ELlcvf:numEdtcde));

       // Populate Large Ded information.
       Chain(n) keyWMLDL1 PLD01;
       If %found;
       BuildJSON(outJSON :'OutLDedAmt' :%editc(LDamp1:numEdtcde));
       BuildJSON(outJSON :'OutLDedAgg' :%editc(LDamg1:numEdtcde));
       Endif;

       // Populate Payment Plan and Desc.
       Clear xxKey;
       xxKey = %editc(ELbscd : 'X');
       Chain keyTable PBS01;
       paymentPlan = %trimr(xxKey) + ' ' + BSdesc;
       BuildJSON(outJSON :'OutLPmtPln' :paymentPlan);

       //* Load Premium Information(Manual, Standard, Normal, Expense & Annual Premium). TAB 1.
       LoadOthPrmInf();
       BuildJSON(outJSON :'OutManPrm' :%editc(decManPrm:numEdtcde));
       BuildJSON(outJSON :'OutStdPrm' :%editc(decStdPrm:numEdtcde));
       BuildJSON(outJSON :'OutNrmPrm' :%editc(decNrmPrm:numEdtcde));
       BuildJSON(outJSON :'OutExpCon' :%editc(decExpConst:numEdtcde));
       BuildJSON(outJSON :'OutAunPrm' :%editc(decAunPrm:numEdtcde));
       BuildJSON(outJSON :'OutClsClms' :%editc(intClsClms:numEdtcde));
       BuildJSON(outJSON :'OutTotClms' :%editc(intTotClms:numEdtcde));
       BuildJSON(outJSON :'OutTotClmsPmt' :%editc(intTotClmsPmt:numEdtcde));
       BuildJSON(outJSON :'OutOutRsvrs' :%editc(intOutRsvrs:numEdtcde));
       BuildJSON(outJSON :'OutOpnClms' :%editc(intOpnClms:numEdtcde));

       //** Begin Add *** 10/07/13 ****************************************
       //* Load the total of the Accumulated Claims Liability  TAB 1.
       decAccClmAmt = 0;
       Chain keyWMLDL1 PDA01;
       DoW not %eof(WDDAL1);
          decAccClmAmt = decAccClmAmt + DAamtb;
          ReadE keyWMLDL1 PDA01;
       Enddo;
       BuildJSON(outJSON :'OutT2AccumClmLib' :%editc(decAccClmAmt:numEdtcde));
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

       //* Manual Premium
          If ELfaf =  'A';
             decManPrm = ELampr;
             If ELamnp = 'Y';
                decManPrm = decManPrm - (ELain1 + Elain2 + Elawav);
             Endif;
          Else;
             decManPrm = ELmapr;
             If ELemnp = 'Y';
                decManPrm = decManPrm - (ELein1 + ELein2 + ELewav);
             Endif;
          Endif;

       //* Standard Premium
          If  ELfaf = 'A';
              decStdPrm = ELaspr;
              If ELamnp = 'Y' and ELwvfl <> 'M' and
                 ELwvfl <> 'J' and ELwvfl <> 'E';
                 decStdPrm = decStdPrm - ELawav;
              Endif;
          Else;
              decStdPrm = ELstpr;
              If ELemnp = 'Y' and  ELwvfl <> 'M' and
                 ELwvfl <> 'J' and ELwvfl <> 'E';
                 decStdPrm = decStdPrm - ELewav;
              Endif;
          Endif;

       //* Normal Premium
          If Elfaf = 'A';
             decNrmPrm = ELanmp;
          Else;
             decNrmPrm = ELenmp;
          Endif;

       //* Expense Constant
          If  ELfaf = 'A' or ELffl9 = 'Y';
              decExpConst = ELprtd;
              xxExCo =    ELprtd;
          Else;
              If  ELemnp = 'Y' or Elprtd = *Zero;
                  decExpConst = ELprtd;
                  xxExCo =  ELprtd;
              Else;
                  xxExCo = ELprtf;
                 decExpConst = xxExCo;
              Endif;
          Endif;

       //* Annual Premium
          If  ELfaf= 'A';
              decAunPrm = ELaapr;
          Else;
              If ELemnp   =  'Y' or ELprtd = *zero;
                 decAunPrm =  (ELenmp + ELeter + ELedtr + ELearc);
                 If ELemnp = 'Y';
                    decAunPrm = ELanpr;
                 Endif;
              Else;
                 decAunPrm = (ELenmp + xxExCo + ELeter + ELedtr + ELearc);
              Endif;
          Endif;

       //* Claims Info
          intClsClms    = ELtclc;
          intTotClms    = ELt#cc;
          intTotClmsPmt = ELt#cl;
          intOutRsvrs   = ELouts;
          intOpnClms    = ELt#cc - ELtclc;

      /end-free
     P LoadOthPrmInf   E


