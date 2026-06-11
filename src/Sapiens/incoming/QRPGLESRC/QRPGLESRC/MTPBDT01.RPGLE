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
     h*  Called from: MPIPBDTPOP
     h*  Calls to:
     h*
     h*  Created on 07/10/2025 by C:079 for MC448
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     ffaeop     if   e             disk
     ffddhl     if   e           k disk
     ffddup     if   e             disk
     ffddpp     if   e             disk
     ffdddp     if   e             disk

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrString

      * *Entry
     dDetailInfo       pr                  ExtPgm('MTPBDT01')
     d                                9  0
     d                              512
     d                            32767
     d                                1

     dDetailInfo       pi
     d inRRN                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d RtnCode                        1

     d strCode         s             35a
     d strAdjSt        s              2a
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
             strCode = strCode;
          When EObTyp = 'H';
          When EObTyp = 'X';
          When EObTyp = 'D';
          Endsl;
       Endif;

       outJSON = ' ';
       BuildJSON(outJSON :'OutValue':%trim(strCode));
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
         // extract Value
           startPos = %Scan('|':inGenParm)+1;
           If startPos <= 1;
             LeaveSr;
           Endif;
           Exsr ExtractValue;
           strCode = Upper(%trim(value));

         // Extract Adjudication State
           startPos = endPos+1;
           If startPos <= 1;
             LeaveSr;
           Endif;
           Exsr ExtractValue;
           strAdjSt = %trim(value);

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

