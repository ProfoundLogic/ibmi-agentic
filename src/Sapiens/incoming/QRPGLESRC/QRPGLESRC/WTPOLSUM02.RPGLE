     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Cancellation History, Billing Division
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
     h*
     h*
     h**********************************************************************
     fwdcxp     if   e             disk    rename(pcx01:pcx0p)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

      * *ENTRY
     DPolicySumm02     pr                  ExtPgm('WTPOLSUM02')
     D                                9  0
     D                            32767
     D                                1
      *
     DPolicySumm02     pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
     d numEdtCde       c                   const('N')

      *
      /Free

       Chain(n) inRRN pcx0p;

       outJSON = ' ';

       // Populate Retrospective information.
       BuildJSON(outJSON :'OutCxCom1' :%TrimR(CXcom1));
       BuildJSON(outJSON :'OutCxCom2' :%TrimR(CXcom2));
       BuildJSON(outJSON :'OutRenFee' :%editc(CXrfee:numEdtcde));
       BuildJSON(outJSON :'OutCertFlg' :%TrimR(CXccrt));


       rtnCode = ' ';
       Return;
      /End-Free

