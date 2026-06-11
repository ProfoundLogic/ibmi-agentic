     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve RenewalBusiness Tracking detail box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLTRACK
     h*  Calls to:
     h*
     h*  Created on 09/21/2012 by C:279 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h* 09/30/19 C:295 Support: 82550 (CAREWEST)                      *
     h*                Detail box is not getting populated in renewal *
     h*                buisness grid                                  *
     h*
     h*
     h**********************************************************************
      * file
     fwdrtp     if   e             Disk    reName(prt01:prt0p)

      /copy sPrDate
      /copy sPrString
      /copy sPrAtrium
      /copy wPrUsrPrms
      /copy wPrTable
      /copy wPrEmpLr

      * *Entry
     dRenewTracking01  pr                  ExtPgm('WTPOLTRA02')
     d                                9  0
     d                            32767
     d                                1

     dRenewTracking01  pi
     d inRRN                          9  0
     d outJSON                    32767
     d RtnCode                        1

      /free

       // Main process
       Chain inRRN prt0p;

       // set key values
       outJSON = ' ';
       BuildJSON(outJSON :'DtlOutPlCrtDt' :%EditW(dtMDYY(RTycdt):'  /  /    '));
       BuildJSON(outJSON :'DtlOutRvw'     :%EditW(dtMDYY(RTuwdt):'  /  /    '));
       BuildJSON(outJSON :'DtlOutQTADt'   :%EditW(dtMDYY(RTqadt):'  /  /    '));
       BuildJSON(outJSON :'DtlOutAprDt'   :%EditW(dtMDYY(RTaadt):'  /  /    '));
       BuildJSON(outJSON :'DtlOutPolGenDt':%EditW(dtMDYY(RTpgdt):'  /  /    '));
       BuildJSON(outJSON :'DtlOutPolMlDt' :%EditW(dtMDYY(RTpold):'  /  /    '));
       BuildJSON(outJSON :'DtlOutPnlCrtDt':%EditW(dtMDYY(RTpcdt):'  /  /    '));
       BuildJSON(outJSON :'DtlOutPnlMldDt':%EditW(dtMDYY(RTpand):'  /  /    '));
       BuildJSON(outJSON :'DtlOutPnlPstDt':%EditW(dtMDYY(RTppdt):'  /  /    '));

       RtnCode = ' ';
       Return;
      /end-free
