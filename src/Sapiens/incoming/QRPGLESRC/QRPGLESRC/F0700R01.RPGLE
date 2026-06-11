     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Rightcoding maintenance screen
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: F0700R
     h*  Calls to:
     h*
     h*  Created on 08/01/2014 by C:212 for Project 114MNV
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     ffarcp     if   e             disk    rename(prc01:prc0p)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

      * *ENTRY
     DF0700R01         pr                  ExtPgm('F0700R01')
     D                                9  0
     D                            32767
     D                                1
      *
     DF0700R01         pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
      /Free

       Chain(n) inRRN prc0p;

       outJSON = ' ';

       // Populate Retrospective information.
       BuildJSON(outJSON :'DtlOutFldEffDat' :%char(CvttoDate(RCefdt)));
       BuildJSON(outJSON :'DtlOutFldState'  :%TrimR(RCst));
       BuildJSON(outJSON :'DtlOutFldCdTyp'  :%TrimR(RCtype));
       BuildJSON(outJSON :'DtlOutFldFrmCd'  :%TrimR(RCfcod));
       BuildJSON(outJSON :'DtlOutFldToCd'   :%TrimR(RCtcod));
       BuildJSON(outJSON :'DtlOutFldPmnt'   :%char(RCpmtt));
       BuildJSON(outJSON :'DtlOutFldAnesHrs':%char(RCaneh));
       BuildJSON(outJSON :'DtlOutFldStatus' :%TrimR(RCstat));

       rtnCode = ' ';

       Return;
      /End-Free

