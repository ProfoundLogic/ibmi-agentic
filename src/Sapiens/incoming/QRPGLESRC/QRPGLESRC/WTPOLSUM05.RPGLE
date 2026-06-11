     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Cancellation History, State Reporting
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
     h*
     h*
     h**********************************************************************
     fwdcxp     if   e             disk    rename(pcx01:pcx0p)
     fwdcxl2    if   e           k disk    rename(pcx01:pcx02)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy wPrEmpLr

      * *ENTRY
     DPolicySumm05     pr                  ExtPgm('WTPOLSUM05')
     D                                9  0
     D                            32767
     D                                1
      *
     DPolicySumm05     pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
     d numEdtCde       c                   const('N')
     d StatRptDiv      s                   like(CXdiv)

     c* Detail file
     c     keyWDCXL2     Klist
     c                   Kfld                    CXco#
     c                   Kfld                    Cxfnd
     c                   Kfld                    CxEmp#
     c                   Kfld                    StatRptDiv
     c                   Kfld                    Cxfyr
      *
      /Free

       Chain(n) inRRN pcx0p;
       StatRptDiv = GetStRptDv(Cxco#:Cxfnd:CxEmp#);
       Chain(n) keyWDCXL2 pcx02;

       outJSON = ' ';

       // Populate Retrospective information.
       BuildJSON(outJSON :'OutCxStRpComment1' :%TrimR(CXcom1));
       BuildJSON(outJSON :'OutCxStRpComment2' :%TrimR(CXcom2));
       BuildJSON(outJSON :'OutCxStRpReinsFee' :%editc(CXrfee:numEdtcde));
       If cxccrt = 'Y';
          BuildJSON(outJSON :'OutCxStRpCert' : 'Yes');
       Else;
          BuildJSON(outJSON :'OutCxStRpCert' : 'No');
       Endif;

       rtnCode = ' ';
       Return;
      /End-Free

