     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2023 Sapiens, Inc. All rights reserved.')
     h**********************************************************************
     h*
     h*  Description: Response program to update Agency Producers status (A/I)
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from: WTAGTPROD
     h*  Calls to:
     h*
     h*  Created on 08/17/2023 by C:300 for WC2395, Support: 113196
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     Fwmapp     uf   e             disk    rename(pap01:pap0p)
      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

      *entry
     DOutAgtPro06      pr                  ExtPgm('WTAGTPRO06')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DOutAgtPro06      pi
     D inRRN                          9  0
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1

     D AgProdstat      s                   like(APstat)
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
       Chain inRRN pap0p;
       If not %found;
          BuildJSON(outJSON :'RtnCode' :'No record found for RRN passed: ' +
           %char(inRRN));
          Return;
       Endif;

       If AgProdstat= 'A';
          APstat = 'I';
       Else;
          APstat = 'A';
       Endif;
       APuser = q1User;
       APpgm  = q1Pgm;
       APdt   = DateTo7(SystemDate);
       APtime = TimeTo6(SystemTime);

       Update pap0p;

       BuildJSON(outJSON :'RtnCode' : %char(APprod) + ' updated.');

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
       // extract Producer Status
         startPos = %Scan(',':inGenParm)+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;
         AgProdstat = %trim(value);
         Rtncode = ' ';
       On-Error;
         Rtncode = 'X';
       EndMon;
       EndSr;
      /End-Free
