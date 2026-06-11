     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to serve Provider Bills Header Pgms
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: MTPBPHHD, MTPBHOHD, MTPBPAHD, MTPBDEHD
     h*  Calls to:
     h*
     h*  Created on 07/10/2025 by C:079 for MC448
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     ffaeop     if   e             disk
     ffaehl     if   e           k disk
     ffaeup     if   e             disk
     ffaepp     if   e             disk
     ffaedp     if   e             disk
     ffmicd10l1 if   e           k disk
     ffmdil     if   e           k disk

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrString

      * *Entry
     dHeaderInfo       pr                  ExtPgm('MTPBHD01')
     d                                9  0
     d                              512
     d                            32767
     d                                1

     dHeaderInfo       pi
     d inRRN                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d RtnCode                        1

     d strDiag         s             35a
     d startPos        s              5  0 inz
     d endPos          s              5  0 inz
     d len             s              5  0 inz
     d value           s            100    inz
     d errMsg          s            500    inz
     d msg             s            100    inz

       Exsr ProcessGenParm;

       Chain inRrn peo01;
       If %Found(FAEOP);
          Select;
          When EObTyp = 'N';
             Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) peh01;
             If %Found(FAEHL);
                Select;
                When EHicdInd = 'Y';
                   Chain (strDiag) pic01;
                   If %Found(FMICD10L1);
                      strDiag = IcCode;
                   Else;
                      strDiag = *blanks;
                   Endif;
                Other;
                Endsl;
             Endif;
          When EObTyp = 'H';
          When EObTyp = 'X';
          When EObTyp = 'D';
          Endsl;
       Endif;

       outJSON = ' ';
       BuildJSON(outJSON :'OutDiagCode':%trim(strDiag));
       BuildJSON(outJSON :'rtnCode' :'Successful');

       RtnCode = ' ';
       Return;
       //=================================================================
       // Process genparm
       //=================================================================
       BegSr ProcessGenParm;

         startPos = 0;
         endPos = 0;

         Monitor;
         // extract Diag Code
           startPos = %Scan('|':inGenParm)+1;
           If startPos <= 1;
             LeaveSr;
           Endif;
           Exsr ExtractValue;
           strDiag = Upper(%trim(value));

         Rtncode = ' ';
         On-Error;
           Rtncode = 'X';
         EndMon;

       EndSr;
       //=================================================================
       // extract values from genparm
       //=================================================================
       BegSr ExtractValue;

         value = *blanks;

         If startPos > 0;
            endPos = %Scan('|':inGenParm:startPos);
            If endPos > 0;
               len = endPos-startPos;
               If len > 0;
                  value = %Subst(inGenParm:startPos:len);
               Endif;
            Endif;
         Endif;

       EndSr;

