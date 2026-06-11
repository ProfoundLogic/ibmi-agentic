     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
      *===========================================================
     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
      *===========================================================
     h*  Description: Response program to serve Add/Delete Remark Code records
     h*
     h*  Created on 09/11/20   C:33  Project: SY409
      *===========================================================
      * Revisions:
      *
      *
      *===========================================================
      /copy sPrAtrium
      /copy sPrDate
      /copy sPrJson
      /copy sPrSql
      /copy sPrString
      /copy sPrTable
      /include yajl_h
      *entry
     dRemarkCodeAddDelete...
     d                 pr                  ExtPgm('WICONFIG07')
     d                              512
     D                            32767
     D                                1
     dRemarkCodeAddDelete...
     d                 pi
     d inGenericParm                512
     D outJSON                    32767
     D rtnCode                        1

     d psds          esds                  extname(szq1p)

     ddsSIRLP        e ds                  inz extname('SIRLP')    qualified
     ddsSIRLPPRD     e ds                  inz extname('SIRLPPRD') qualified
     ddsSIRLPRMK     e ds                  inz extname('SIRLPRMK') qualified

     dintCo#           s              3  0 inz
     dstrBeginsW       s              1    inz
     dstrCo#           s              3    inz
     dstrKey           s             40    inz varying
     dstrParms         s            512    inz
     dstrRmkCode       s              5    inz
     dstrRtnCode       s             25    varying inz('success')
     dstrSql           s           1000    inz varying
     dstrType          s             15    inz varying
      /Free
       Exec Sql Set Option Commit = *None;

       strParms   = inGenericParm;
       strType    = ParseString(strParms:',':1:'1');
       strCo#     = ParseString(strParms:',':1:'1');
       strKey     = ParseString(strParms:',':1:'1');
       strRmkCode = ParseString(strParms:',':1:'1');
       strBeginsW = ParseString(strParms:',':1:'1');
       Monitor;
          intCo#     = %int(strCo#);
       On-error;
       Endmon;
       strRmkCode = Upper(strRmkCode);

       Select;
       When Upper(strType) = 'ADD';
          Exsr RecordAdd;
       When Upper(strType) = 'DELETE';
          Exsr RecordDelete;
       Endsl;

       BuildJSON(outJSON :'returnCode':strRtnCode);
       Return;

       //==============================================
       Begsr RecordAdd;
        Exec sql
         select * into :dsSIRLPRMK
          from SIRLPRMK
          where RLco# = :intCo# and RLkey = :strKey
           and RLrmCd = :strRmkCode;
        Select;
        When IsValidFetch(sqlstt);
           strRtnCode = 'duplicate';
           Leavesr;
        Endsl;
        Reset dsSIRLPRMK;
        dsSIRLPRMK.RLco# = intCo#;
        dsSIRLPRMK.RLkey = strKey;
        dsSIRLPRMK.RLrmCd = strRmkCode;
        dsSIRLPRMK.RLbeginW = strBeginsW;
        dsSIRLPRMK.RLstat = 'A';
        dsSIRLPRMK.RLpgm = q1Pgm;
        dsSIRLPRMK.RLuser = q1User;
        dsSIRLPRMK.RLdt = DateTo7(SystemDate());
        dsSIRLPRMK.RLtime = TimeTo6(SystemTime());
        dsSIRLPRMK.RLiPgm = dsSIRLPRMK.RLpgm;
        dsSIRLPRMK.RLiUsr = dsSIRLPRMK.RLuser;
        dsSIRLPRMK.RLiDt = dsSIRLPRMK.RLdt;
        dsSIRLPRMK.RLiTim = dsSIRLPRMK.RLtime;
        Exec sql
         insert into SIRLPRMK values(:dsSIRLPRMK);
       Endsr;

       //==============================================
       Begsr RecordDelete;
        Exec sql
         delete from SIRLPRMK
          where RLco# = :intCo# and RLkey = :strKey
           and RLrmCd = :strRmkCode;
        Select;
        When not IsValidFetch(sqlstt);
        Endsl;
       Endsr;
      /End-Free
