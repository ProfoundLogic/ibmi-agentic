     hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for State Comp Benefits Info
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTGRPCLMCG
     h*  Calls to:
     h*
     h*  Created on 03/11/2020 by C:318 for Project WC2196
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     fwdrbp     if   e             disk    rename(prb01:prb0s)

     d/copy sPrDate
     d/copy sPrAtrium

      * *ENTRY
     dwtGrpClm05       pr                  extpgm('wtGrpClm05')
     d                                9  0
     d                            32767
     d                                1

     dwtGrpClm05       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1


      /Free

       Chain inRRN prb0s;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlStateSCB':%trim(RBst));
       BuildJSON(outJSON :'OutDtlEffDateSCB' :
                         alphaUSA(cvtToDate(dtMDYY(RBefdt))));
       BuildJSON(outJSON :'OutDtlCompReimbPct':%editc(RBcmpr:'N'));
       BuildJSON(outJSON :'OutDtlMinCompRate':%editc(RBmnrt:'N'));
       BuildJSON(outJSON :'OutDtlMaxCompRate':%editc(RBpacp:'N'));
       BuildJSON(outJSON :'OutDtlWaitingPeriodDays':%editc(RBwks:'N'));
       BuildJSON(outJSON :'OutDtlUSL&HMinCompRate':%editc(RBlhmn:'N'));
       BuildJSON(outJSON :'OutDtlUSL&HMaxCompRate':%editc(RBlhmx:'N'));
       BuildJSON(outJSON :'OutDtlUSL&HWaitingPeriodDays':%editc(RBlhwp:'N'));
       BuildJSON(outJSON :'OutDtlFullSalCont#Days':%editc(RBdys:'N'));
       BuildJSON(outJSON :'OutDtl#DaysInitialCompPmtDue':%editc(RBicpd:'N'));

       rtnCode = ' ';
       Return;

      /End-Free
