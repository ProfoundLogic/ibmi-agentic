     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h*
     h*  Description: Response program to update Output Control Configuration detail
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from: STAPPCFG
     h*  Calls to:
     h*
     h*  Created on 11/05/2015 by C:33  for SY371
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     Fstspp     uf   e             disk    rename(psp01:psp0p)
      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

      *entry
     DOutCtlCfg        pr                  ExtPgm('STAPPCFG04')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DOutCtlCfg        pi
     D inRRN                          9  0
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1

     DflgSplO          s                   like(SPsplO)
     DflgScrO          s                   like(SPscrO)
     Dflg3Xml          s                   like(SP3Xml)
     D startPos        s              5  0 inz
     D endPos          s              5  0 inz
     D len             s              5  0 inz
     D value           s             10    inz

     d psds          esds                  extname(szq1p)

      /Free
       //                          1   5  8  9
       // input data is in format: ,H,S,3,
        // H = Hardcopy flag
        // S = Scriptura flag
        // 3 = 3rd Party XML flag

       Exsr ProcessGenParm;
       If RtnCode = 'X';
         RtnCode = ' ';
         BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
         Return;
       Endif;
       Chain inRRN psp0p;
       If not %found;
          BuildJSON(outJSON :'RtnCode' :'No record found for RRN passed: ' +
           %char(inRRN));
          Return;
       Endif;
       Exsr ErrorCheck;

       SPsplO = flgSplO;
       SPscrO = flgScrO;
       SP3Xml = flg3Xml;
       SPuser = q1User;
       SPpgm  = q1Pgm;
       SPdt   = DateTo7(SystemDate);
       SPtime = TimeTo6(SystemTime);

       Update psp0p;

       BuildJSON(outJSON :'RtnCode' : %trim(SPtype) + ' updated.');

       RtnCode = ' ';
       Return;

       //=================================================================
       // Check for errors
       //=================================================================
       BegSr ErrorCheck;

         Select;
         When flgSplO = 'N' and flgScrO = 'N' and flg3Xml = 'N';
            BuildJSON(outJSON :'rtnError' :'At least one output type must be' +
             ' selected.');
            Return;
         Endsl;

       EndSr;
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
       // extract hardcopy
         startPos = %Scan(',':inGenParm)+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;
         flgSplO = %trim(value);
       // extract scriptura
         startPos = endPos+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;
         flgScrO = %trim(value);
       // extract 3rd party
         startPos = endPos+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;
         flg3Xml = %trim(value);
         Rtncode = ' ';
       On-Error;
         Rtncode = 'X';
       EndMon;
       EndSr;
      /End-Free
