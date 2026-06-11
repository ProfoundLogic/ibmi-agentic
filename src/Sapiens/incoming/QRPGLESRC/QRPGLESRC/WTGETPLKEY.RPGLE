     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to get the policy darts key.
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
     h* 08/12/20  C:300  Project: WC2196
     h*                  Correct ExtPgm Name
     h*
     h**********************************************************************
      * Customer Master File
     Fwdelp     if   e             disk    rename(pel01:pel0p)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      * *ENTRY
      *** Begin Chg *** 08/12/20 *******************************
     D*****  GetWtPlKey       pr                  ExtPgm('GetWtClKey')
     DGetWtPlKey       pr                  ExtPgm('GetWtPlKey')
      *** End   Chg *** 08/12/20 *******************************
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DGetWtPlKey       pi
     D inRRN                          9  0
     D inQuote                      512
     D outJSON                    32767
     D RtnCode                        1

     D                 ds
     D  dartsKey               1     40
     D  company#               1      3  0
     D  group#                 4      6  0
     D  employer#              7     15  0
     D  division              16     20  0
     D  quote                 40     40
      /Free

       Chain(n) inRRN pel0p;
        // set key values
       company#=ELco#;
       group#=ELfnd;
       employer#=ELemp#;
       division=ELdiv;
       If inQuote='Y';
          quote='Q';
       Else;
          quote=' ';
       EndIf;

       outJSON = ' ';
       BuildJSON(outJSON :'DartsKey' :dartsKey);
       RtnCode = ' ';
       Return;

      /End-Free
