     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to get the WCMC Flags.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTACTPNL
     h*  Calls to:
     h*
     h*  Created on 10/12/2012 by C:062 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h* 10/19/17 C: 079 Support: 50451/090
     h*                 Change IsModEnabled to use correct parm for WCMC
     h*                 Interface
     h*
     h* 05/15/17 C: 079 Project: MC410
     h*                 Change to use IsModEnabled and
     h*                 Add NonPDSinter to the JSON return parm
     h*
     h**********************************************************************

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
       //*** Beg Add *** 05/15/17 *************************************
      /copy sPrModule
       //*** End Add *** 05/15/17 *************************************

      * Retrieve System Flags
     DSa001R           pr                  ExtPgm('SA001R')
     D wcmsInstalled                  1
     D wcmcInstalled                  1
     D wcmcInterface                  1

      * *ENTRY
     DSa001RW          pr                  ExtPgm('SA001RW')
     D                                9  0
     D                            32767
     D                                1
      *
     DSa001RW          pi
     D inRRN                          9  0
     D outJSON                    32767
     D RtnCode                        1

     D wcmsInstalled   s              1    Inz(' ')
     D wcmcInstalled   s              1    Inz(' ')
     D wcmcInterface   s              1    Inz(' ')
     D*** Beg Add *** 05/15/17 *****************************
     D NonPDSinterf    s              1    Inz(' ')
     D*** End Add *** 05/15/17 *****************************

      /Free

       //*** Beg Chg *** 05/15/17 *******************************
       //****Sa001R(wcmsInstalled:wcmcInstalled:wcmcInterface);
       WCMSInstalled    = IsModEnabled('WorkComp');
       WCMCInstalled    = IsModEnabled('Managed Care');
       //*** Beg Chg *** 12/21/17 ***********************************
       //***** WCMCInterface    = IsModEnabled('WCMCInterace');
       WCMCInterface    = IsModEnabled('WCMCInterface');
       //*** End Chg *** 12/21/17 ***********************************
       NonPDSinterf     = IsModEnabled('NonPDSWC');
       //*** End Chg *** 05/15/17 *******************************
       outJSON = ' ';
       BuildJSON(outJSON :'WCMSInst' :wcmsInstalled);
       BuildJSON(outJSON :'WCMCInst' :wcmcInstalled);
       BuildJSON(outJSON :'WCMCInter' :wcmcInterface);
       //*** Beg Add *** 05/15/17 *******************************
       BuildJSON(outJSON :'NonPDSinter' :NonPDSinterf);
       //*** End Add *** 05/15/17 *******************************
       RtnCode = ' ';
       Return;

      /End-Free
