     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h*
     h*  Description: Response program to update Dashboard User Item Configuration
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from: STAPPCFG
     h*  Calls to:
     h*
     h*  Created on 01/13/2017 by C:33  for SY388
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     Fsmdrp     uf   e             disk    rename(pdr01:pdr0p)
      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

      *entry
     DOutUserItem      pr                  ExtPgm('STAPPCFG05')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DOutUserItem      pi
     D inRRN                          9  0
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1

     DstrItem          s                   like(DRitem)
     DstrRole          s                   like(DRrole)

     dstrMode          s             25
     dstrSql           s           1000
     dstrSqlWhere      s           1000

     D startPos        s              5  0 inz
     D endPos          s              5  0 inz
     D len             s              5  0 inz
     D value           s            100    inz

     ddsSMDRP        e ds                  extname(SMDRP)
     d psds          esds                  extname(szq1p)

      /Free
       //                          1                        27                         52
       // input data is in format: ,role/user                ,item                      ,

       Exsr ProcessGenParm;
       If RtnCode = 'X';
         RtnCode = ' ';
         BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
         Return;
       Endif;

       Exsr ErrorCheck;
       Exsr SaveDashUsr;

       RtnCode = ' ';
       Return;

       //=================================================================
       // Check for errors
       //=================================================================
       BegSr ErrorCheck;


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
       BuildJSON(outJSON :'inParms' : %trim(inGenParm));
       startPos = 0;
       endPos = 0;
       Monitor;
       // extract role
         startPos = %Scan(',':inGenParm)+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;
         strRole = %trim(value);
       // extract item
         startPos = endPos+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;
         strItem = %trim(value);
       On-Error;
         Rtncode = 'X';
       EndMon;
       EndSr;

       //=====================================================================
       Begsr SaveDashUsr;
        Clear dsSMDRP;
        strSqlWhere =
         'where DRposX = ' + %trim(%editc(inRRN:'Z')) +
         ' and DRtype = ''Tab''' +
         ' and DRrole = ''' + %trim(strRole) + '''';
        strSql = 'select * from SMDRP ' + %trim(strSqlWhere);
        Exec SQL Prepare sqlDR From :strSql;
        Exec SQL Declare cursorDR Cursor For sqlDR;
        Exec SQL Open cursorDR;
        Exec SQL Fetch next From cursorDR into :dsSMDRP;
        BuildJSON(outJSON :'SQLCheck' : %trim(strSql));

        DRrole = strRole;
        DRitem = strItem;
        DRposX = inRRN;
        DRposY = 0;
        DRtype = 'Tab';
        DRstat = 'A';
        DRpgm  = q1Pgm;
        DRuser = q1User;
        DRdt   = DateTo7(SystemDate);
        DRtime = TimeTo6(SystemTime);
        Select;
        When SQLSTT <> '00000' and (strItem <> *blanks);
           // Add
           strMode = ' add';
           strSql =  'insert into SMDRP values(' +
            '''' + DRrole      + ''', ' +
            '''' + DRitem      + ''', ' +
            %editc(DRposX:'X') + ', ' +
            %editc(DRposY:'X') + ', ' +
            '''' + DRtype      + ''', ' +
            '''' + DRstat      + ''', ' +
            '''' + DRuser      + ''', ' +
            '''' + DRpgm       + ''', ' +
            %editc(DRdt:'X')   + ', ' +
            %editc(DRtime:'X') + ', ' +
            '''' + DRuser      + ''', ' +
            '''' + DRpgm       + ''', ' +
            %editc(DRdt:'X')   + ', ' +
            %editc(DRtime:'X') +
            ') with NC ';
        When SQLSTT = '00000' and (strItem = *blanks);
           // Delete
           strMode = ' delete';
           strSql = 'delete from SMDRP ' + %trim(strSqlWhere) + ' with NC';
        When SQLSTT = '00000';
           // Update
           strMode = ' update';
           strSql =  'update SMDRP set ' +
            'DRITEM = ''' + DRitem             + ''', ' +
            'DRSTAT = ''' + DRstat             + ''', ' +
            'DRUSER = ''' + DRuser             + ''', ' +
            'DRPGM  = ''' + DRpgm              + ''', ' +
            'DRDT   = '   + %editc(DRdt:'X')   + ', ' +
            'DRTIME = '   + %editc(DRtime:'X') + ' ' +
            %trim(strSqlWhere) + ' with NC';
        Endsl;
        Exec SQL Close cursorDR;
        BuildJSON(outJSON :'SQLUpdate' : %trim(strSql));

        // Process update
        Exec SQL execute immediate :strSql;
        Select;
        When SQLSTT <> '00000';
           BuildJSON(outJSON :'RtnCode' : 'SQL update failed. -' +
            %trimr(strMode) + ' - SQL code: ' + SQLSTT +
            ' - ' + %trim(strSql));
        Other;
           BuildJSON(outJSON :'RtnCode' : 'Position ' +
            %trim(%editc(inRRN:'X')) + ' updated. -' + strMode);
        Endsl;

       Endsr;
      /End-Free
