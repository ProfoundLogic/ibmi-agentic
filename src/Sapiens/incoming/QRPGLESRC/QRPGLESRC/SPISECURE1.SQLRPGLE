     h**********************************************************************
     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h*
     h*  Description: Response program to update Application Configuration
     h*               User Access Records
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from: SPISECURE - Javascript
     h*  Calls to:
     h*
     h*  Created on 05/09/2022 by C:79  for 129WWV
     h**********************************************************************
     h* Revisions:                                                         *
     h*                                                                    *
     h**********************************************************************
     Fsdtbl1    uf a e           k disk
     Fsmupl     if   e           k disk
      /copy sPrDate
      /copy sPrString
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

      *entry
     DSpiSecure1       pr                  ExtPgm('SPISECURE1')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DSpiSecure1       pi
     D inRRN                          9  0
     D inGenParm                    512
     D outJSON                    32767
     D rtnCode                        1

     DstartPos         s              3  0
     DendPos           s              3  0
     Dlen              s              3  0
     Dvalue            s            120
     DintCo#           s                   like(TBco#)
     DstrFile          s                   like(TBfile)
     DstrPgmName       s                   like(TBpgmName)
     DstrTabName       s                   like(TBtabName)
     DstrUserPrf       s                   like(TBUserPrf)
     DstrUsrName       s                   like(TBusrName)
     DsysDt            s                   like(TBdt)
     DsysTm            s                   like(TBTime)
     D errMsg          s            500    inz
     D msg             s            100    inz

     d psds          esds                  extname(szq1p)
        //=====================================================================
        // input data is in format: Company|File|Program|Tab|User
        //  Company - Company Number - 3A
        //  File    - Display File Name - 10A
        //  Program - Program Name - 50A
        //  Tab     - Tab Name - 50A
        //  User    - User Profile - User Name - 70A
        //=====================================================================
       Exsr ProcessGenParm;
       If rtnCode = 'X';
         rtnCode = ' ';
         BuildJSON(outJSON :'rtnCode' :'GenParm scan error');
         Return;
       Endif;

       Chain (intCo#:strFile:strPgmName:strTabName:strUserPrf) ptb01;
       If %Found(SDTBL1);
          BuildJSON(outJSON :'rtnCode' :'Duplicate Record Found');
          Return;
       Else;
          sysDt = DateTo7(SystemDate);
          sysTm = TimeTo6(SystemTime);
          TBco# = intCo#;
          TBFile = strFile;
          TBpgmName = strPgmName;
          TBtabName = strTabName;
          TBuserPrf = strUserPrf;
          TBusrName = strUsrName;
          TBuSER    = Q1user;
          TBpgm     = Q1pgm;
          TBdt      = sysDt;
          TBtime    = sysTm;
          TBiUsr    = Q1user;
          TBiPgm    = Q1pgm;
          TBiDt     = sysDt;
          TBiTim    = sysTm;
          write ptb01;
       Endif;

       Exsr ErrorCheck;

       rtnCode = ' ';
       Return;

       //=================================================================
       // Check for errors
       //=================================================================
       BegSr ErrorCheck;

         errMsg = ' ';

         If errMsg <> ' ';
            BuildJSON(outJSON :'rtnError' :%trim(errMsg));
            Return;
         Endif;

       EndSr;
       //=================================================================
       // add message to error message string
       //=================================================================
       BegSr AddError;

         If errMsg = ' ';
            errMsg = msg;
         Else;
            errMsg = %trim(errMsg) + '  ' + %trim(msg);
         Endif;

       EndSr;
       //=================================================================
       // extract values from genparm
       //=================================================================
       BegSr ExtractValue;

         value = *blanks;

         If startPos > 0;
            endPos = %Scan('|':inGenParm:startPos);
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
         // Company Number
           startPos = %Scan('|':inGenParm)+1;
           If startPos <= 1;
             LeaveSr;
           Endif;
           Exsr ExtractValue;
           intCo# = %Dec(value:3:0);

         // Extract File Name
           startPos = endPos+1;
           If startPos <= 1;
             LeaveSr;
           Endif;
           Exsr ExtractValue;
           strFile = %trim(value);

         // Extract Program Name
           startPos = endPos+1;
           If startPos <= 1;
             LeaveSr;
           Endif;
           Exsr ExtractValue;
           strPgmName = %trim(value);

         // Extract Tab Name
           startPos = endPos+1;
           If startPos <= 1;
             LeaveSr;
           Endif;
           Exsr ExtractValue;
           strTabName = %Trim(value);

         // Extract User Profile and User Name
           startPos = endPos + 1;
           If startPos <= 1;
             LeaveSr;
           Endif;
           Exsr ExtractValue;
           strUsrName = %Trim(ParseString(value:'-':1));
           strUserPrf = %Trim(ParseString(value:'-':2));

           rtncode = ' ';

           // Validate the User Profile
           Chain strUserPrf pup01;
           If not %Found(SMUPL);
             rtncode = 'X';
           Endif;

         On-Error;
           rtncode = 'X';
         EndMon;

       EndSr;
      /End-Free
