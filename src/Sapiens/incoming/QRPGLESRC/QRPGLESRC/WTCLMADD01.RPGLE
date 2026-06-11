     h**********************************************************************
     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to get Claim Handling Indicator
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTCLMADD
     h*  Calls to:
     h*
     h*  Created on 09/04/2014 by C:279 for Project: 114WOE
     h**********************************************************************
     h* Revisions:
     h*
     h* 06/16/2015 c:62 Project 114WOE Priss 25
     h*                 Expand the rrn field from 3.0 to 9.0
     h*
     h**********************************************************************
     fwmcmp     if   e             disk    rename(pcm01:pcm0p)
     fwmcxl     if   e           k disk    rename(pcx01:pcx00)

      /copy sPrAtrium
      * Entry
     dDefaultPoints    pr                  ExtPgm('WTCLMADD01')
     d                                9  0
     d                              512
     d                            32767
     d                                1

     dDefaultPoints    pi
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d RtnCode                        1

     d charPoints      s             40    inz
     d intDashPos      s              5  0 inz
     d intEndPos       s              5  0 inz
     d*** Begin Chg *** 06/16/15 ************************
     d*intRrn#**       s              3  0 inz
     d intRrn#         s              9  0 inz
     d*** End   Chg *** 06/16/15 ************************
     d intLen          s              5  0 inz
     d intStartPos     s              5  0 inz
     d charValue       s             41    inz
     d charInst        s              1    inz

      /copy sCpyClmHdI

      /free
       //                          1   5
       // input data is in format: ,xxxxxxxx
       // xx - Rrn#

       intRrn#  = 0;
       outJSON  = ' ';
       charInst = ' ';

       Exsr ProcessGenParm;
       If RtnCode = 'X';
          RtnCode = ' ';
          BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
          Return;
       Endif;

       Chain(n) intRrn# pcm0p;
       If %found(wmcmp);
          Chain (CMco#:CMfnd) pcx00;
          If %found(wmcxl);
             charInst = CxclmInst;
             If isInstExist(CMco#:CMfnd:CMemp#:CMdiv) <> *on;
                charInst = ' ';
             Endif;
          Endif;
       Endif;
       BuildJSON(outJSON :'Points' :charInst);

       RtnCode = ' ';
       Return;
       //**************************************************************
       // Process genparm
       //**************************************************************
       BegSr ProcessGenParm;

       intStartPos = 0;
       intEndPos = 0;
       Rtncode = 'X';
       Monitor;

          // Extract RRN#
          intStartPos = %Scan(',':inGenParm)+1;
          If intStartPos <= 1;
             LeaveSr;
          Endif;
          Exsr ExtractValue;

          Monitor;
             intRrn# = %Int(%trim(charValue));
          On-Error;
             intRrn# = 0;
          EndMon;

          Rtncode = ' ';
       On-Error;
          Rtncode = 'X';
       EndMon;
       EndSr;
       //*********************************************************************
       // extract values from genparm
       //*********************************************************************
       BegSr ExtractValue;
       charValue = *blanks;
       If intStartPos > 0;
         intEndPos = %Scan(',':inGenParm:intStartPos);
         If intEndPos > 0;
           intLen = intEndPos - intStartPos;
           If intLen > 0;
             charValue = %Subst(inGenParm:intStartPos:intLen);
           Endif;
         Endif;
       Endif;
       EndSr;
      /end-free
