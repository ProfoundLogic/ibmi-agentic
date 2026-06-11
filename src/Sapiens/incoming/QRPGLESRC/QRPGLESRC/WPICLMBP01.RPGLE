     hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Body part codes
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WPICLMBP
     h*  Calls to:
     h*
     h*  Created on 05/01/2018 by C:053 for Project WC1917
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     fwtpap     if   e             disk    rename(ppa01:ppa0p)
     f********wtabl     if   e           k disk
     fwtgtl     if   e           k disk

      /copy sPrAtrium

      * *ENTRY
     dwpiClmBP01       pr                  extpgm('WPICLMBP01')
     d                                9  0
     d                            32767
     d                                1

     dwpiClmBP01       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strPart         s             10
     d strDesc         s             50
     d strSts          s             10
     d strEdi          s             10
     d strEDesc        s             50

     d ##resvcd        s             10
     d ##tbid          s              4
     d ##key           s             10


     c*****keyWTRTP      Klist
     c************       Kfld                    B2co#
     c***********        Kfld                    ##resvcd

     c     keyWTGTL      Klist
     c                   Kfld                    PAco#
     c                   Kfld                    ##tbid
     c                   Kfld                    ##key

      /Free

       Chain inRRN ppa0p;

       Exsr BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlParCode':%trim(strPart));
       BuildJSON(outJSON :'OutDtlparDesp':%trim(strDesc));
       BuildJSON(outJSON :'OutDtlparStatus' :%trim(strSts));
       BuildJSON(outJSON :'OutDtlParEDI' :%trim(strEdi));
       BuildJSON(outJSON :'OutDtlEDIDesp':%trim(strEDesc));

       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // Part Code
       strPart = PAkey;

       // Description
       strDesc = PAdesc;

       // Status
       If PAstat = 'A';
          strSts = 'Active';
       Else;
          strSts = 'Inactive';
       Endif;

       // EDI Code
       strEdi = PAedip;

       // EDI Desription
       If PAedip <> *blanks;
          ##tbid = 'PART';
          ##key = PAedip;
          Chain keyWTGTL pgt01;
          If %Found(WTGTL);
             strEDesc = %trim(GTdesc);
          Else;
             strEDesc = *blanks;
          Endif;
       Endif;

       endsr;
      /End-Free
