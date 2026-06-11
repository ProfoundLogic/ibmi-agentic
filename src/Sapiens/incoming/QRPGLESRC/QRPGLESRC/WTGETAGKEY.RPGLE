     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to get the Agency darts key.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTACTPNL
     h*  Calls to:
     h*
     h*  Created on 06/20/2022 by C:300 for Agency Maintenance Project WC2395
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
      * Agent Master File
     Fwmagp     if   e             disk    rename(pag01:pag0p)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      * *ENTRY
     DGetWtAgKey       pr                  ExtPgm('GetWtAgKey')
     D                                9  0
     D                            32767
     D                                1
      *
     DGetWtAgKey       pi
     D inRRN                          9  0
     D outJSON                    32767
     D RtnCode                        1

     D                 ds
     D  dartsKey               1     40
     D  fein                   1      9  0
     D  mod                   10     12  0
      /Free

       Chain(n) inRRN pag0p;
        // set key values
       fein = AGfein;
       mod = AGmod;

       outJSON = ' ';
       BuildJSON(outJSON :'DartsKey' :dartsKey);
       RtnCode = ' ';
       Return;

      /End-Free
