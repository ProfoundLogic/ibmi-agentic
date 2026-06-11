     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to get the Group darts key.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: STACTPNL
     h*  Calls to:
     h*
     h*  Created on 10/01/2024 by C:342 for Managed care Group Maintenance UX Guidelines MC447
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
      * Group Master File
     Ffmfnp     if   e             disk    rename(pfn01:pfn0p)

      /copy sPrDate
      /copy sPrUsrPrms
      /copy sPrAtrium
      /copy sPrTable
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
