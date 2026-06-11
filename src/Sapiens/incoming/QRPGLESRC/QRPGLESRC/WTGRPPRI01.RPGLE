     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2020 Sapiens, Inc. All rights reserved.')
     h**********************************************************************
     h*
     h*  Description: Response program to update Region Status (A/I)
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from: WTGRPPRI
     h*  Calls to:
     h*
     h*  Created on 03/18/2020 by C:300 for WC2196
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     Fwmrep     uf   e             disk    rename(pre01:pre0p)
      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

      *entry
     DOutGrpCfg        pr                  ExtPgm('WTGRPPRI01')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DOutGrpCfg        pi
     D inRRN                          9  0
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1

     D regstat         s                   like(REstat)
     D startPos        s              5  0 inz
     D endPos          s              5  0 inz
     D len             s              5  0 inz
     D value           s             10    inz

     d psds          esds                  extname(szq1p)

      /Free
       //                          1   5  8  9
       // input data is in format: ,A,
        // A = Active / I = Inactive

       Exsr ProcessGenParm;
       If RtnCode = 'X';
         RtnCode = ' ';
         BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
         Return;
       Endif;
       Chain inRRN pre0p;
       If not %found;
          BuildJSON(outJSON :'RtnCode' :'No record found for RRN passed: ' +
           %char(inRRN));
          Return;
       Endif;

       If regstat = 'A';
          REstat = 'I';
       Else;
          REstat = 'A';
       Endif;
       REuser = q1User;
       REpgm  = q1Pgm;
       REdt   = DateTo7(SystemDate);
       REtime = TimeTo6(SystemTime);

       Update pre0p;

       BuildJSON(outJSON :'RtnCode' : %char(RErgn#) + ' updated.');

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
         regstat = %trim(value);
         Rtncode = ' ';
       On-Error;
         Rtncode = 'X';
       EndMon;
       EndSr;
      /End-Free
