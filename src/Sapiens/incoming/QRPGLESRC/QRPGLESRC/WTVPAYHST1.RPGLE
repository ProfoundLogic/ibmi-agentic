     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to serve Claim reserve history detail box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTVPAYHIST
     h*  Calls to:
     h*
     h**********************************************************************
     h*  Created on 09/07/2020 by C:062 for Project WC2749orm
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
      * Customer Master File
     Fwcvpayhst if   e             disk    rename(pvh01:pvh0p)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

      * *ENTRY
     DVPayHistory      pr                  ExtPgm('WTVPAYHST1')
     D                                9  0
     D                            32767
     D                                1

     DVPayHistory      pi
     D inRRN                          9  0
     D outJSON                    32767
     D RtnCode                        1
      *
     D numEdtCde       c                   const('N')
      /Free

       Chain(n) inRRN pvh0p;

       outJSON = ' ';
       BuildJSON(outJSON :'DtlOutDocumentID':%Trim(VHdocName));
       BuildJSON(outJSON :'DtlOutExtractDate' :
                    %EditW(dtMDYY(VHExtDate):'  /  /    '));
       BuildJSON(outJSON :'DtlOutExtractTime' :
                    %EditW(VHExtTime:'  :  :  '));
       BuildJSON(outJSON :'DtlOutHD001Count' :%EditC(VHhd001Cnt:numEdtcde));
       BuildJSON(outJSON :'DtlOutVR001Count' :%EditC(VHvr001Cnt:numEdtcde));
       BuildJSON(outJSON :'DtlOutVR002Count' :%EditC(VHvr002Cnt:numEdtcde));
       BuildJSON(outJSON :'DtlOutVR003Count' :%EditC(VHvr003Cnt:numEdtcde));
       BuildJSON(outJSON :'DtlOutTR999Count' :%EditC(VHtr999Cnt:numEdtcde));
       BuildJSON(outJSON :'DtlOutTotalCount' :%EditC(VHtotalCnt:numEdtcde));

       RtnCode = ' ';
       Return;
      /End-Free
