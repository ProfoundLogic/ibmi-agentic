     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2020 Sapiens, Inc. All rights reserved.')
     h**********************************************************************
     h*
     h*  Description: Response program to update Rate set code status (A/I)
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from: WTGRPPOLCG
     h*  Calls to:
     h*
     h*  Created on 05/01/2020 by C:318 for WC2196
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     Fwmgrp     uf   e             disk    rename(pgr01:pgr0p)
      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

      *entry
     DwtGrpPol01       pr                  ExtPgm('WTGRPPOL01')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DwtGrpPol01       pi
     D inRRN                          9  0
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1

     D rtststat        s                   like(GRstat)
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
       Chain inRRN pgr0p;
       If not %found;
          BuildJSON(outJSON :'RtnCode' :'No record found for RRN passed: ' +
           %char(inRRN));
          Return;
       Endif;

       If rtststat= 'A';
          GRstat = 'I';
       Else;
          GRstat = 'A';
       Endif;
       GRuser = q1User;
       GRpgm  = q1Pgm;
       GRdt   = DateTo7(SystemDate);
       GRtime = TimeTo6(SystemTime);

       Update pgr0p;

       BuildJSON(outJSON :'RtnCode' : GRrtcd + ' updated.');

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
         rtststat = %trim(value);
         Rtncode = ' ';
       On-Error;
         Rtncode = 'X';
       EndMon;
       EndSr;
      /End-Free
