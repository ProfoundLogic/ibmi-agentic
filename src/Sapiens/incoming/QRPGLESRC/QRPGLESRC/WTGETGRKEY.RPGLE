     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2020 Sapiens, Inc. All rights reserved.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to get the Group darts key.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTACTPNL
     h*  Calls to:
     h*
     h*  Created on 08/12/2020 by C:300 for Group Configuration project WC2196
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
      * Group Master File
     Fwmfnp     if   e             disk    rename(pfn01:pfn0p)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      * *ENTRY
     DGetWtGrKey       pr                  ExtPgm('GetWtGrKey')
     D                                9  0
     D                            32767
     D                                1
      *
     DGetWtGrKey       pi
     D inRRN                          9  0
     D outJSON                    32767
     D RtnCode                        1

     D                 ds
     D  dartsKey               1     40
     D  company#               1      3  0
     D  group#                 4      6  0
      /Free

       Chain(n) inRRN pfn0p;
        // set key values
       company#=FNco#;
       group#=FNfnd;

       outJSON = ' ';
       BuildJSON(outJSON :'DartsKey' :dartsKey);
       RtnCode = ' ';
       Return;

      /End-Free
