     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2020 Sapiens, Inc. All rights reserved.')
     h**********************************************************************
     h*
     h*  Description: Response program to update State Comp Rate By Benefits Status (A/I)
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from: WTGRPCLMCG
     h*  Calls to:
     h*
     h*  Created on 10/06/2022 by C:342 for Support 101193
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     Fwdr1p     uf   e             disk    rename(pr101:pr10p)
      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

      *entry
     DWTGRPCLM03       pr                  ExtPgm('WTGRPCLM03')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DWTGRPCLM03       pi
     D inRRN                          9  0
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1

     D StCmRtBnstat    s                   like(R1stat)
     D startPos        s              5  0 inz
     D endPos          s              5  0 inz
     D len             s              5  0 inz
     D value           s             10    inz

     d psds          esds                  extname(szq1p)


       Exsr ProcessGenParm;
       If RtnCode = 'X';
         RtnCode = ' ';
         BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
         Return;
       Endif;
       Chain inRRN pr10p;
       If not %found;
          BuildJSON(outJSON :'RtnCode' :'No record found for RRN passed: ' +
           %char(inRRN));
          Return;
       Endif;

       If StCmRtBnstat= 'A';
          R1stat = 'I';
       Else;
          R1stat = 'A';
       Endif;
       R1user = q1User;
       R1pgm  = q1Pgm;
       R1dt   = DateTo7(SystemDate);
       R1time = TimeTo6(SystemTime);

       Update pr10p;

       BuildJSON(outJSON :'RtnCode' : R1st + ' updated.');

       RtnCode = ' ';
       Return;

       //=================================================================
       // extract values from genparm
       //=================================================================
       BegSr ExtractValue;
       value = *blanks;
       If startPos > 0;
         endPos = %Scan(',':inGenParm:startPos);
         If endPos > 0;
           len = endPos-startPos;
           If len > 0;
             value = %Subst(inGenParm:startPos:len);
           Endif;
         Endif;
       Endif;
       EndSr;
       //=================================================================
       // Process genparm
       //=================================================================
       BegSr ProcessGenParm;
       startPos = 0;
       endPos = 0;
       Monitor;
       // extract Region Status
         startPos = %Scan(',':inGenParm)+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;
         StCmRtBnstat = %trim(value);
         Rtncode = ' ';
       On-Error;
         Rtncode = 'X';
       EndMon;
       EndSr;
      /End-Free
