     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for ICD-10 code
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTCLMICD
     h*  Calls to:
     h*
     h*  Created on 04/20/2015 by C:279 for Project WC2043
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwmicd10p  if   e             disk    rename(pic01:pic0p)

      /copy sPrAtrium

      * *ENTRY
     dwtClmICD01       pr                  extpgm('WTCLMICD01')
     d                                9  0
     d                            32767
     d                                1

     dwtClmICD01       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strSts          s             10

      /free

       Chain inRRN pic0p;

       Exsr BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutStatus'    :%trim(strSts));
       BuildJSON(outJSON :'OutICDLongDesc' :%trim(ICdsc2));

       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // Status
       If ICstat = 'A';
          strSts = 'Active';
       Else;
          strSts = 'Inactive';
       Endif;

       Endsr;
      /end-free
