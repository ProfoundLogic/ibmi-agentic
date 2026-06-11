     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h* * * NOT currently used in javascript * * *
     h**********************************************************************
     h*
     h*  Description: Response program to update Generic File Field Configuration
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from: STAPPCFG
     h*  Calls to:
     h*
     h*  Created on 06/22/2017 by C:33  for WC2405
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     F*mtrpfld  uf   e             disk    rename(ptr01:ptr0pfld)
      /copy sPrAtrium
      /copy sPrDate
      /copy sPrString
      /copy sPrSql
      /copy sPrMessage

      *entry
     DOutGenField      pr                  ExtPgm('STAPPCFG08')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DOutGenField      pi
     D inRRN                          9  0
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1

     DinSqlAct         s                   like(TRsqlAct) inz
     dinMode           s             25                     inz

     D startPos        s              5  0 inz
     D endPos          s              5  0 inz
     D len             s              5  0 inz
     D value           s            100    inz

     ddsSMTRPSQL     e ds                  extname(SMTRPSQL)
     ddsMsgInfo        ds                  likeds(dsRtnMsgInfo)
     d psds          esds                  extname(szq1p)

      /Free
       // input data is in format: |runMode|SQL value|

       Exsr ProcessGenParm;
       If RtnCode = 'X';
         RtnCode = ' ';
         BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
         Return;
       Endif;

       Exec SQL set option COMMIT=*NONE;

       Select;
       When inMode = '*EDIT';
          Exsr ErrorCheck;
       When inMode = '*GET';
          Exsr GetSQL      ;
       When inMode = '*UPDATE';
          Exsr ErrorCheck;
          Exsr SaveSQL;
       Endsl;

       RtnCode = ' ';
       Return;

       //=================================================================
       // Check for errors
       //=================================================================
       BegSr ErrorCheck;
        If inSqlAct <> *blanks;
           If not ValidSqlSyntax(%trim(inSqlAct):'*SQL');
              BuildJSON(outJSON:'RtnError':'Invalid SQL');
              BuildJSON(outJSON:'errorField':'TxaTrigFileSQLAction');
              BuildJSON(outJSON:'errorMsg':dsMsgInfo.msgText);
              Return;
           Endif;
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
        value = ScanReplace('%6C':'%':value);
       EndSr;
       //=====================================================================
       Begsr GetSQL;
        Clear dsSMTRPSQL;
        Exec SQL
         select *
          into :dsSMTRPSQL
          from SMTRPSQL
          where rrn(SMTRPSQL) = :inRRN;
        BuildJSON(outJSON:'TxaTrigFileSQLAction':TRsqlAct);
       Endsr;
       //=================================================================
       // Process genparm
       //=================================================================
       BegSr ProcessGenParm;
        BuildJSON(outJSON :'inParms' : %trim(inGenParm));
        Monitor;
          endPos = 0;
        // extract run mode
          startPos = %Scan('|':inGenParm)+1;
          If startPos <= 1;
            LeaveSr;
          Endif;
          Exsr ExtractValue;
          inMode = %trim(value);
        // extract value
          startPos = endPos+1;
          If startPos <= 1;
            LeaveSr;
          Endif;
          Exsr ExtractValue;
          inSqlAct = %trim(value);
        On-Error;
          RtnCode = 'X';
        EndMon;
       EndSr;

       //=====================================================================
       Begsr SaveSQL;
        // Process update
        Exec SQL
         update SMTRPSQL
          set TRsqlAct = :inSqlAct
          where rrn(SMTRPSQL) = :inRRN;
        Select;
        When SQLSTT <> '00000';
           BuildJSON(outJSON :'RtnCode' : 'SQL update failed. - SQL code: ' +
            SQLSTT);
        Other;
           BuildJSON(outJSON :'RtnSaved' : 'Update successful.');
        Endsl;

       Endsr;
      /End-Free
