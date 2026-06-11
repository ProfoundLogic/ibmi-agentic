     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')

     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Return Fund Batch Detail grid.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTRFBTCH
     h*  Calls to:
     h*
     h*  Created on 01/05/2024 by C:300 for Project WC2676
     h**********************************************************************
     h* Revisions:
     h*                                                                *
     h*
     h**********************************************************************
     fwxrtp     if   e             disk    rename(prt01:prt0s)
     fwtabl     if   e           k disk    include(ptt01)

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrModule

      * *ENTRY
     dwtRfBtch03       pr                  extpgm('WTRFBTCH03')
     d                                9  0
     d                            32767
     d                                1

     dwtRfBtch03       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strAgtNam       s             50
     d strARcode       s             50
     d xxKey           s             10


     c     keyTable      klist
     c                   kfld                    RTco#
     c                   kfld                    xxKey
      *
      /Free

       Chain inRRN prt0s;
       Exsr BuildData;

       outJSON = ' ';
       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlEmpName' :%trim(RTempn));
       BuildJSON(outJSON :'OutDtlCbxSelected' :%trim(RTselc));
       BuildJSON(outJSON :'OutDtlCbxContAud' :%trim(RTcafl));
       BuildJSON(outJSON :'OutDtlCustID' :%trim(RTcust));
       BuildJSON(outJSON :'OutDtlAgency' :%trim(strAgtNam));
       BuildJSON(outJSON :'OutDtlFein' :%trim(%Char(RTfein)));
       BuildJSON(outJSON :'OutDtlMod' :%trim(%Char(RTmod)));
       BuildJSON(outJSON :'OutDtlARCode' :%trim(strARcode));
       BuildJSON(outJSON :'OutDtlComm1' :%trim(RTcom1));
       BuildJSON(outJSON :'OutDtlComm2' :%trim(RTcom2));
       BuildJSON(outJSON :'OutDtlComm3' :%trim(RTcom3));

       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // Agency Name
       strAgtNam = *Blanks;
       If RTagt# <> 0;
          strAgtNam = %trim(%Char(RTagt#));
       Endif;
       If RTagtn <> *blanks;
          strAgtNam = %trim(strAgtNam) + ' - ' +%trim(RTagtn);
       Endif;

       // AR Code
       strARcode = *Blanks;
       If RTarcd <> '  ';
          strARcode = RTarcd;
          xxKey = RTarcd;
          Chain keyTable PTT01;
          If %found();
             strARcode = %trim(strARcode) + ' - ' + TTdesc;
          Endif;
       Endif;

       endsr;
      /End-Free
