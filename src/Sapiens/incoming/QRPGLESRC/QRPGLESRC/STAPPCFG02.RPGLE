     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Auto Diary Trigger Setup: Ajax call to build Injury codes
     h*               multi-select-dropdown box based on Jurisdiction Sates
     h*               selection, on-blur event.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: STAPPCFG
     h*  Calls to:
     h*
     h*  Created on 07/08/2015 by C:242 for Project C114WKA
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************

     fwtgtl     if   e           k Disk    ReName(pgt01:pgt00)

     d psds          esds                  extname(szq1p)
      /copy sPrAtrium
     d/copy sprUsrPrms

      * *Entry
     dstAppCfg02       pr                  extpgm('STAPPCFG02')
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1

     dstAppCfg02       pi
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1

     d startPos        s              5  0 inz
     d endPos          s              5  0 inz
     d len             s              5  0 inz
     d State           s              2    inz
     d StateNCCI       s              4    inz
     d outValue        s             50
     d intCo#          s              3  0

      /free

       outJSON  = ' ';

       Exsr ProcessGenParm;

       Select;
       When RtnCode = 'X';
          RtnCode = ' ';
          BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
          Return;
       When RtnCode = 'P';
          RtnCode = ' ';
          BuildJSON(outJSON :'RtnCode' :'Trigger Type is P');
          Return;
       EndSL;

       OutValue = 'GTTBID = ''' + StateNCCI + '''';
       BuildJSON(outJSON :'ChoicesSelectionCriteria':OutValue);

       RtnCode = ' ';
       Return;
       //**************************************************************
       // Process genparm
       //**************************************************************
       BegSr ProcessGenParm;
       startPos = 0;
       endPos = 0;
       Rtncode = 'X';
       StateNCCI = '  NC';
       Monitor;

       // Extract length of Jurisdiction States parameter
       len = %len(%trim(inGenParm));
       If (len < 4);
          Rtncode = 'X';
       Else;

          If %Trim(%Subst(inGenParm:2:1)) <> 'C';
             Rtncode = 'P'; //Policy Type Trigger
          Else;

             // Extract Jurisdiction States starting position
             startPos = %Scan('|':inGenParm:2)+1;
             If startPos <= 1;
                LeaveSr;
             Endif;

             //Extract Jurisdiction States ending position
             endPos = %Scan('|':inGenParm:startPos);
             If endPos <= 1;
                LeaveSr;
             Endif;

             //Specifically Only 1 state selected.
             If (endPos - startPos = 2);
                State = %Trim(%Subst(inGenParm:StartPos:2));

                //Check if StateNCCI available in Generic Table.
                intCo# = GetUserInfo(q1user);
                StateNCCI = State + 'NC';
                Chain (intCo#:StateNCCI) pgt00;
                If not %found;
                   StateNCCI = '  NC';
                Endif;
             Endif;

             Rtncode = ' ';
          Endif;
       Endif;
       On-Error;
          Rtncode = 'X';
       EndMon;
       EndSr;
      /End-Free
