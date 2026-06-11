     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve A/R Summary - All Years detail box - TAB 1.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLARINQ
     h*  Calls to:
     h*
     h*  Created on 04/20/2012 by C:190 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwmeap     if   e             disk    rename(pea01:pea0p)
     fwdell1    if   e           k disk    rename(pel01:pel01)
     fwtavl     if   e           k disk    rename(pav01:pav01)
     fwtabl     if   e           k disk    include(pbs01)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

      * *ENTRY
     DPolicyInq01      pr                  ExtPgm('WTPOLARI01')
     D                                9  0
     D                            32767
     D                                1
      *
     DPolicyInq01      pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
     d numEdtCde       c                   const('N')
     d xxKey           s             10
     d paymentPlan     s             60
     d actstatus       s             60

     c* Employer Detail File
     c     keyWDELL1     Klist
     c                   Kfld                    EAco#
     c                   Kfld                    EAfnd
     c                   Kfld                    EAfyr
     c                   Kfld                    EAemp#
     c                   Kfld                    kkDiv             5 0

     c     keyTable      klist
     c                   kfld                    EAco#
     c                   kfld                    xxKey
      *
      /Free

       Chain(n) inRRN pea0p;
       kkDiv = 0;
       Chain(n) keyWDELL1 pel01;

       outJSON = ' ';

       // Populate Policy Period.
       BuildJSON(outJSON :'OutFromDt' :%EditW(dtMDYY(ELpfdt):'  /  /    '));
       BuildJSON(outJSON :'OutToDt' :%EditW(dtMDYY(ELptdt):'  /  /    '));

       // Populate Payment Plan and Desc.
       Clear xxKey;
       xxKey = %editc(ELbscd : 'X');
       Chain keyTable PBS01;
       paymentPlan = %trimr(xxKey) + ' ' + BSdesc;
       BuildJSON(outJSON :'OutLPmtPln' :%trimR(paymentPlan));

       // Populate Current Activity Status.
       Clear AVdesc;
       Clear xxKey;
       xxKey = ELascd;
       Chain keyTable PAV01;
       actstatus  = %trimr(xxKey) + ' ' + AVdesc;
       BuildJSON(outJSON :'OutCurActivity' :%trimR(actstatus));

       rtnCode = ' ';
       Return;
      /End-Free

