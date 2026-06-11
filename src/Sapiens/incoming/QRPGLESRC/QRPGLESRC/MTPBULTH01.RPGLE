     h*copyright('(c) 2020 Sapiens, Inc. All rights reserved. -
     h**********************************************************************
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     H bnddir('STBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*                     for Uploaded Provider Bills Transfer History
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: MTPBULTRHS
     h*  Calls to:
     h*
     h*  Created on 05/11/2020 by C:212 for Project MC437
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     ffau9p     if   e             disk    rename(pu901:pu90p)

     d/copy sPrAtrium
     d/copy sPrDate

      * *entry
     dMTPBULTH01       pr                  extpgm('MTPBULTH01')
     d                                9  0
     d                            32767
     d                                1

     dMTPBULTH01       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strUlTm         s              8    inz
     d strPrTm         s              8    inz
     C*

      /Free
       Chain(n) inRrn pu90p;
       Exsr builddata;

       outJSON = ' ';

       // populate detail box information.
       BuildJSON(outJSON :'OutDtlMbrNm' :%Trim(U9name));
       BuildJSON(outJSON :'OutDtlBatchNm' :%Trim(U9mbr#));
       BuildJSON(outJSON :'OutDtlUplDt' :
                         alphaUSA(cvtToDate(dtMDYY(U9uldt))));
       BuildJSON(outJSON :'OutDtlUplTm' :%Trim(strUlTm));
       BuildJSON(outJSON :'OutDtlProcDt' :
                         alphaUSA(cvtToDate(dtMDYY(U9prdt))));
       BuildJSON(outJSON :'OutDtlProcTm' :%Trim(strPrTm));
       BuildJSON(outJSON :'OutDtlTotCnt' :%Trim(%Char(U9tcnt)));
       BuildJSON(outJSON :'OutDtlDupVnd' :%Trim(%Char(U9dcnt)));
       BuildJSON(outJSON :'OutDtlClmPrvEr':%Trim(%Char(U9ccnt)));
       BuildJSON(outJSON :'OutDtlReconEr':%Trim(%Char(U9rcnt)));
       BuildJSON(outJSON :'OutDtlNbrBilTfr':%Trim(%Char(U9ucnt)));

       rtnCode = ' ';
       Return;

       //===================================================================
       // Build Data That will be passed on to AJAX
       //===================================================================
       Begsr builddata;

        If U9ultm = *zeros;
           strUlTm = *blanks;
        Else;
           strUlTm = %EditC(U9ultm:'X');
           strUlTm = %Subst(strUltm:1:2) + ':' + %Subst(strUltm:3:2) + ':' +
                     %Subst(strUltm:5:2);
        EndIf;
        If U9prtm = *zeros;
           strPrTm = *blanks;
        Else;
           strPrTm = %EditC(U9prtm:'X');
           strPrTm = %Subst(strPrtm:1:2) + ':' + %Subst(strPrtm:3:2) + ':' +
                     %Subst(strPrtm:5:2);
        EndIf;

       Endsr;

      /end-free
