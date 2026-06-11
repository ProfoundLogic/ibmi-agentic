     h**********************************************************************
     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Retrieve system user information
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Created: 07/06/16  C:33  Project: WC2405
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
      /copy sPrAtrium
      * Entry
     dStUsrPrm02       pr                  extpgm('STUSRPRM02')
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1
     dStUsrPrm02       pi
     d inRrn                          9  0
     d inGenParm                    512
     d outJSON                    32767
     d rtnCode                        1

     dRtvUserProfile   pr                  extpgm('SRRTVUSR')
     d inUser                        10    const
     d outName                       50
     d outOutQ                       10
     d outError                       1n

     dlen              s              5  0 inz
     dintEnd           s              5  0 inz(1)
     dintStart         s              5  0 inz
     disError          s              1n
     dstrName          s             50
     dstrOutQ          s             10
     dstrUser          s             10

      /free
       // input data is in format: |xxxxxxxxxx|
       // xxxxxxxxxx - User profile

       outJSON  = ' ';

       Exsr ProcessGenParm;

       Select;
       When rtnCode = 'X';
          rtnCode = ' ';
          BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
          Return;
       Endsl;

       Monitor;
          RtvUserProfile(strUser:strName:strOutQ:isError);
          If isError;
             rtnCode = 'X';
          Else;
             If %subst(strOutQ:1:1) = '*';
                Clear strOutQ;
             Endif;
             BuildJSON(outJSON :'UserName' :strName);
             BuildJSON(outJSON :'UserOutQ' :strOutQ);
             Return;
          Endif;
       On-error;
          rtnCode = 'X';
       Endmon;

       If rtnCode = 'X';
          rtnCode = ' ';
          BuildJSON(outJSON:'RtnCode':'User information not retrieved');
          Return;
       Endif;

       rtnCode = ' ';
       Return;
       //==============================================================
       // Process genparm
       //==============================================================
       BegSr ProcessGenParm;
        rtnCode = ' ';

        Monitor;
           // Extract length
           len = %len(%trim(inGenParm));
           If len < 2;
              rtnCode = 'X';
              Leavesr;
           Endif;
           // Extract user starting position
           intStart = %scan('|':inGenParm:intEnd)+1;
           If intStart <= 1;
              rtnCode = 'X';
              Leavesr;
           Endif;
           //Extract user ending position
           intEnd = %scan('|':inGenParm:intStart);
           If intEnd <= 1;
              rtnCode = 'X';
              LeaveSr;
           Endif;
           strUser = %trim(%subst(inGenParm:intStart:intEnd-intStart));
        On-Error;
           rtnCode = 'X';
        EndMon;

       EndSr;
      /End-Free
