     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')

     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Return Funds Batches
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTRFBTCH
     h*  Calls to:
     h*
     h*  Created on 10/26/2023 by C:300 for Project WC2676
     h**********************************************************************
     h* Revisions:
     h*                                                                *
     h*
     h**********************************************************************
     fwxrnp     if   e             disk    rename(prn01:prn0s)
     fwmfnl     if   e           k disk

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrModule

      * *ENTRY
     dwtRFBtch01       pr                  extpgm('WTRFBTCH01')
     d                                9  0
     d                            32767
     d                                1

     dwtRFBtch01       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strFromGroup    s             70
     d strToGroup      s             70
     d strPayeeType    s             20


      /Free

       Chain inRRN prn0s;

       Exsr BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlFromGroup' :%trim(strFromGroup));
       BuildJSON(outJSON :'OutDtlToGroup' :%trim(strToGroup));
       BuildJSON(outJSON :'OutDtlPayeeType' :%trim(strPayeeType));
       BuildJSON(outJSON :'OutDtlCbxExlContAud' :%trim(RNcafl));
       BuildJSON(outJSON :'OutDtlCbxBatchProc' :%trim(RNbpf));
       BuildJSON(outJSON :'OutDtlComment1' :%trim(RNcom1));
       BuildJSON(outJSON :'OutDtlComment2' :%trim(RNcom2));
       BuildJSON(outJSON :'OutDtlComment3' :%trim(RNcom3));

       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // From Group Number
       strFromGroup = *Blanks;
       If RNfgrp <> 0;
          Chain (RNco#:RNfgrp) pfn01;
          IF %found(wmfnl);
             strFromGroup = %char(RNfgrp) + ' - '  + %trim(FNname);
          EndIf;
       EndIf;

       // To Group Number
       strToGroup = *Blanks;
       If RNtgrp <> 0;
          Chain (RNco#:RNtgrp) pfn01;
          IF %found(wmfnl);
             strToGroup = %char(RNtgrp) + ' - '  + %trim(FNname);
          EndIf;
       EndIf;

       // Payee Type
       strPayeeType = *Blanks;
       Select;
       When RNptyp = 'A';
          strPayeeType = 'Agent';
       When RNptyp = 'E';
          strPayeeType = 'Employer';
       Endsl;



       endsr;
      /End-Free
