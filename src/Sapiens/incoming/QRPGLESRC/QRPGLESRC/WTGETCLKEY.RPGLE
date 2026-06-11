     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to get the claim darts key.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTACTPNL
     h*  Calls to:
     h*
     h*  Created on 10/08/2012 by C:062 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
      * Customer Master File
     Fwmcmp     if   e             disk    rename(pcm01:pcm0p)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      * *ENTRY
     DGetWtClKey       pr                  ExtPgm('GetWtClKey')
     D                                9  0
     D                            32767
     D                                1
      *
     DGetWtClKey       pi
     D inRRN                          9  0
     D outJSON                    32767
     D RtnCode                        1

     D                 ds
     D  dartsKey               1     40
     D  company#               1      3  0
     D  group#                 4      6  0
     D  year                   7      9  0
     D  case#                 10     16  0
      /Free

       Chain(n) inRRN pcm0p;
        // set key values
       company#=CMco#;
       group#=CMfnd;
       year=CMfYr;
       case#=CMcase;

       outJSON = ' ';
       BuildJSON(outJSON :'DartsKey' :dartsKey);
       RtnCode = ' ';
       Return;

      /End-Free
