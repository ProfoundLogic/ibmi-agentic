     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Import Errors Screen (FTBILLPRXE)
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: FTBILLPRXE
     h*  Calls to:
     h*
     h*  Created on 04/12/2016 by C:309 for Project 114MXO
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     ffak1p     if   e             disk

      /copy sPrAtrium

      * *ENTRY
     dFTBILLPRE1       pr                  extpgm('FTBILLPRE1')
     d                                9  0
     d                            32767
     d                                1

     dFTBILLPRE1       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strClmntName    s             50
     d strClmStatus    s             50
      /Free

         Chain inRRN pk101;
         Exsr BuildData;
         outJSON = ' ';

        // Populate Detail Box information.
         BuildJSON(outJSON :'DtlOutFldClmntNm':%trim(strClmntName));
         BuildJSON(outJSON :'DtlOutFldClmSts':%trim(strClmStatus));

         rtnCode = ' ';
         Return;

        //===================================================================
        // Buld Data that will be passed on to AJAX
        //===================================================================
         Begsr BuildData;

          strClmntName   =  %Trim(K1MFSN) + ' ' + %Trim(K1MLSN);

          Select;
          When K1PRFL='E';
          strClmStatus = 'This is not a Valid Claim# to Transfer ';

          When K1PRFL='D';
          strClmStatus ='Duplicate Payment';

          When K1prfl = 'Y';
          strClmStatus= 'This Claim# has already been Transferred';

          When K1PRFL='X';
          strClmStatus ='This is a Deleted Claim#';

          Other;
          strClmStatus ='This is a Valid Claim# to Transfer';
          EndSl;

         endsr;
      /End-Free
