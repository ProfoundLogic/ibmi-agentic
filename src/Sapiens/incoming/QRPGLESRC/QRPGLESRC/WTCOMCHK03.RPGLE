     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Check Schedule Maintenance detail box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTCOMCHK
     h*  Calls to:
     h*
     h*  Created on 10/14/2014 by C:279 for Project: 114WNC
     h**********************************************************************
     h* Revisions:
    h* 07/20/20 C: 079 Project: WC2749
    h*                 Recompile
     h*
     h**********************************************************************
      * Customer Master File
     fwac3p     if   e             disk    rename(pcp01:pcp0p)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

      * *Entry
     dCheckSchd03      pr                  ExtPgm('WTCOMCHK03')
     d                                9  0
     d                            32767
     d                                1

     dCheckSchd03      pi
     d inRRN                          9  0
     d outJSON                    32767
     d RtnCode                        1


      /free

       Chain inRRN pcp0p;

       outJSON = ' ';


       BuildJSON(outJSON :'OutGrpDtlFrmDt':%EditW(dtMDYY(CPFrDt):'  /  /    '));
       BuildJSON(outJSON :'OutGrpDtlToDt' :%EditW(dtMDYY(CPToDt):'  /  /    '));


       RtnCode = ' ';
       Return;

      /End-Free
