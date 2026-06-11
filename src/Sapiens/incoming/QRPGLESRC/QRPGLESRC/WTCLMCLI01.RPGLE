     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to Points
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTCLMDTL
     h*  Calls to:
     h*
     h*  Created on 05/28/2013 by C:282 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     Fwtabl     if   e           k disk    Include(pca01)
      *
      /copy sPrAtrium
      * *ENTRY
     dDefaultPoints    pr                  ExtPgm('WTCLMCLI01')
     d                                9  0
     d                              512
     d                            32767
     d                                1
      *
     dDefaultPoints    pi
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d RtnCode                        1
      *
     D intCo#          s              3  0 inz
     D charKey         s             10    inz
     D charDesc        s             30    inz
     D charPoints      s              7    inz
     D*
     D startPos        s              5  0 inz
     D endPos          s              5  0 inz
     D dashPos         s              5  0 inz
     D len             s              5  0 inz
     D value           s             41    inz
      /Free

       //                          1   5
       // input data is in format: ,ccc,xxxxxxxxxx-xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
       // ccc - Co#; xx - (table file key - decription)

       intCo#   = 0;
       charKey  = ' ';
       charDesc = ' ';
       outJSON  = ' ';

       Exsr ProcessGenParm;
       If RtnCode = 'X';
         RtnCode = ' ';
         BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
         Return;
       Endif;

       Chain(n) (intCo#:charKey) pca01;
       If %found(wtabl);
         charPoints = %char(CApnts);
         BuildJSON(outJSON :'Points' :charPoints);
       Endif;

       RtnCode = ' ';
       Return;
       //**************************************************************
       // Process genparm
       //**************************************************************
       BegSr ProcessGenParm;
       startPos = 0;
       endPos = 0;
       Rtncode = 'X';
       Monitor;

       // Extract company number
         startPos = %Scan(',':inGenParm)+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;

         Monitor;
          intCo# = %Int(%trim(value));
         On-Error;
          intCo# = 0;
         EndMon;

       // Extract Table File Key and Decription
         startPos = endPos+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;

         Monitor;
         dashPos = %scan('-':value);
           charKey  = %subst(value:1:dashPos-1);
           charDesc = %subst(value:dashPos+1:len-dashPos);
         On-Error;
          charKey = ' ';
          charDesc = ' ';
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
      /End-Free
