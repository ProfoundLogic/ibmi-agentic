     hcopyright('(c) 2021 Sapiens, Inc. All rights reserved.')

     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate detail box properites
     h*               for Group Configuration
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTGRPCFG
     h*  Calls to:
     h*
     h*  Created on 11/17/21  by C:33  for Project 146WJV
     h**********************************************************************
     h* Revisions:
     h*
     h*   07/22/25 C:300 Support: 119421                               *
     h*                  In Detail box of Group Configuration Grid     *
     h*                  rcdGrpCfg record format - "Default and Exclude*
     h*                  from Comparative Rating" checkboxes are       *
     h*                  visible, when Connect Portal Block is Hidden  *
     h*                                                                *
     h**********************************************************************
     fwmfnp     if   e             disk    rename(pfn01:pfn0p)
     fwmfip     if   e           k disk    rename(pfi01:pfi0p)

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrModule

      *entry
     dWtGrpCfg02       pr                  extpgm('WTGRPCFG02')
     d                                9  0
     d                              512
     d                            32767
     d                                1
     dWtGrpCfg02       pi
     d inRRN                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1

     dConQuoteItems_visibility...
     d                 s             25    varying inz('hidden')
     dOutDtlConPortal_visibility...
     d                 s             25    varying inz('hidden')

      /Free
       Clear outJSON;
       Clear FNfnd;
       Chain inRRN pfn0p;
       Chain (FNco#:FNfnd) pfi0p;

       Exsr BuildData;

       BuildJSON(outJSON :'ConQuoteItems_visibility':ConQuoteItems_visibility);

       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;
        If IsModEnabled('Portal Module');
           OutDtlConPortal_visibility = 'visible';
        Endif;
        //*** Begin Chg *** 07/22/25 ***************************************
        // If FIquote = 'Y';
        If FIquote = 'Y' and OutDtlConPortal_visibility = 'visible';
        //*** End   Chg *** 07/22/25 ***************************************
           ConQuoteItems_visibility = 'visible';
        Endif;
       Endsr;
      /End-Free
