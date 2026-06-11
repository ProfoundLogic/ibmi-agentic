     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')
     h**********************************************************************
     h*
     h*  Description: Response program to update Office Info status (A/I)
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from: WTOFFICE
     h*  Calls to:
     h*
     h*  Created on 10/01/2024 by C:342 for WC2940
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     Fwmofp     uf   e             disk    rename(pof01:pof0p)
      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

      *entry
     DOutOffice        pr                  ExtPgm('WTOFFICE02')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DOutOffice        pi
     D inRRN                          9  0
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1

     D Ofcstat         s                   like(OFstat)
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
       Chain inRRN pof0p;
       If not %found;
          BuildJSON(outJSON :'RtnCode' :'No record found for RRN passed: ' +
           %char(inRRN));
          Return;
       Endif;

       If Ofcstat = 'A';
          OFstat = 'I';
       Else;
          OFstat = 'A';
       Endif;
       OFuser = q1User;
       OFpgm  = q1Pgm;
       OFdt   = DateTo7(SystemDate);
       OFtime = TimeTo6(SystemTime);

       Update pof0p;

       BuildJSON(outJSON :'RtnCode' : OFofc# + ' updated.');

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
         Ofcstat = %trim(value);
         Rtncode = ' ';
       On-Error;
         Rtncode = 'X';
       EndMon;
       EndSr;
      /End-Free
