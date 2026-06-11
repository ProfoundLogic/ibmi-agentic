     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
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
     h*  Created on 05/22/2017 by C:33  for WC2405
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     F*mtrpfld  uf   e             disk    rename(ptr01:ptr0pfld)
      /copy sPrAtrium
      /copy sPrDate
      /copy sPrString

      *entry
     DOutGenField      pr                  ExtPgm('STAPPCFG07')
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

     DinFieldType      s                   like(TRfieldTyp) inz
     dinMode           s             25                     inz
     DinOperatorCur    s                   like(TRoperCur)  inz
     DinOperatorPrv    s                   like(TRoperPrv)  inz
     DinTypeCur        s                   like(TRcmpTypC)  inz
     DinTypePrv        s                   like(TRcmpTypP)  inz
     DinValueCur       s                   like(TRcmpValC)  inz
     DinValuePrv       s                   like(TRcmpValP)  inz
     DisError          s              1n
     DstrFieldType     s                   like(TRfieldTyp) inz
     DstrField         s            100
     dstrSql           s           1000
     dstrSqlWhere      s           1000

     D startPos        s              5  0 inz
     D endPos          s              5  0 inz
     D len             s              5  0 inz
     D value           s            100    inz

     ddsSMTRPFLD     e ds                  extname(SMTRPFLD)
     d psds          esds                  extname(szq1p)

      /Free
       //                                                Current|Prior
       // input data is in format: |runMode|fieldType|operator|type|value

       Exsr ProcessGenParm;
       If RtnCode = 'X';
         RtnCode = ' ';
         BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
         Return;
       Endif;

       Exec SQL set option COMMIT=*NONE;

       Select;
       When inMode = '*GET';
          Exsr GetFieldComp;
       When inMode = '*UPDATE';
          Exsr ErrorCheck;
          Exsr SaveFieldComp;
       Endsl;

       RtnCode = ' ';
       Return;

       //=================================================================
       Begsr CheckAlpha;
        isError = *off;
        Clear strField;
        Select;
        When inOperatorCur = *blanks and
         (inValueCur  <> *blanks or inTypeCur <> *blanks);
           isError = *on;
           strField = 'SlbFieldRelationalOperatorCurrent';
        When inValueCur  = *blanks and
         (inOperatorCur <> *blanks or inTypeCur <> *blanks);
           isError = *on;
           strField = 'TxbFieldComparisonValueCurrent';
        When inOperatorPrv = *blanks and
         (inValuePrv  <> *blanks or inTypePrv <> *blanks);
           isError = *on;
           strField = 'SlbFieldRelationalOperatorPrevious';
        When inValuePrv  = *blanks and
         (inOperatorPrv <> *blanks or inTypePrv <> *blanks);
           isError = *on;
           strField = 'TxbFieldComparisonValuePrevious';
        Endsl;
        If isError;
           BuildJSON(outJSON:'RtnError':'Mutual inclusion');
           BuildJSON(outJSON:'errorField':strField);
           BuildJSON(outJSON:'errorMsg':
            'All fields must have a value if any have a value.');
           Return;
        Endif;
       Endsr;
       //=================================================================
       Begsr CheckDate;
        isError = *off;
        Clear strField;
        Select;
        When inOperatorCur = *blanks and
         (inValueCur  <> *blanks or inTypeCur  <> *blanks);
           isError = *on;
           strField = 'SlbFieldRelationalOperatorCurrent';
        When inValueCur  = *blanks and
         (inOperatorCur <> *blanks or inTypeCur  <> *blanks);
           isError = *on;
           strField = 'TxbFieldComparisonValueCurrent';
        When inTypeCur  = *blanks and
         (inOperatorCur <> *blanks or inValueCur  <> *blanks);
           isError = *on;
           strField = 'SlbFieldComparisonTypeCurrent';
        When inOperatorPrv = *blanks and
         (inValuePrv  <> *blanks or inTypePrv  <> *blanks);
           isError = *on;
           strField = 'SlbFieldRelationalOperatorPrevious';
        When inValuePrv  = *blanks and
         (inOperatorPrv <> *blanks or inTypePrv  <> *blanks);
           isError = *on;
           strField = 'TxbFieldComparisonValuePrevious';
        When inTypePrv  = *blanks and
         (inOperatorPrv <> *blanks or inValuePrv  <> *blanks);
           isError = *on;
           strField = 'SlbFieldComparisonTypePrevious';
        Endsl;
        If isError;
           BuildJSON(outJSON:'RtnError':'Mutual inclusion');
           BuildJSON(outJSON:'errorField':strField);
           BuildJSON(outJSON:'errorMsg':
            'All fields must have a value if any have a value.');
           Return;
        Endif;
       Endsr;
       //=================================================================
       Begsr CheckNumeric;
        isError = *off;
        Clear strField;
        Select;
        When inOperatorCur = *blanks and
         (inValueCur  <> *blanks or inTypeCur  <> *blanks);
           isError = *on;
           strField = 'SlbFieldRelationalOperatorCurrent';
        When inValueCur  = *blanks and
         (inOperatorCur <> *blanks or inTypeCur  <> *blanks);
           isError = *on;
           strField = 'TxbFieldComparisonValueCurrent';
        When inOperatorPrv = *blanks and
         (inValuePrv  <> *blanks or inTypePrv  <> *blanks);
           isError = *on;
           strField = 'SlbFieldRelationalOperatorPrevious';
        When inValuePrv  = *blanks and
         (inOperatorPrv <> *blanks or inTypePrv  <> *blanks);
           isError = *on;
           strField = 'TxbFieldComparisonValuePrevious';
        Endsl;
        If isError;
           BuildJSON(outJSON:'RtnError':'Mutual inclusion');
           BuildJSON(outJSON:'errorField':strField);
           BuildJSON(outJSON:'errorMsg':
            'All fields must have a value if any have a value.');
           Return;
        Endif;
       Endsr;
       //=================================================================
       // Check for errors
       //=================================================================
       BegSr ErrorCheck;
        strFieldType = Upper(inFieldType);
        Select;
        When strFieldType = 'DATE'
         or  strFieldType = 'TIMESTAMP';
           Exsr CheckDate;
        When strFieldType = 'CHAR'
         or  strFieldType = 'VARCHAR';
           Exsr CheckAlpha;
        When strFieldType = 'DECIMAL'
         or  strFieldType = 'NUMERIC'
         or  strFieldType = 'INTEGER'
         or  strFieldType = '7.0';
           Exsr CheckNumeric;
        Endsl;
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
       Begsr GetFieldComp;
        Clear dsSMTRPFLD;
        Exec SQL
         select *
          into :dsSMTRPFLD
          from SMTRPFLD
          where rrn(SMTRPFLD) = :inRRN;
        BuildJSON(outJSON:'SlbFieldRelationalOperatorCurrent':TRoperCur);
        BuildJSON(outJSON:'TxbFieldComparisonValueCurrent':TRcmpValC);
        BuildJSON(outJSON:'SlbFieldComparisonTypeCurrent':TRcmpTypC);
        BuildJSON(outJSON:'SlbFieldRelationalOperatorPrevious':TRoperPrv);
        BuildJSON(outJSON:'TxbFieldComparisonValuePrevious':TRcmpValP);
        BuildJSON(outJSON:'SlbFieldComparisonTypePrevious':TRcmpTypP);
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
        // extract field type
          startPos = endPos+1;
          If startPos <= 1;
            LeaveSr;
          Endif;
          Exsr ExtractValue;
          inFieldType = %trim(value);
        // extract operator - Current
          startPos = endPos+1;
          If startPos <= 1;
            LeaveSr;
          Endif;
          Exsr ExtractValue;
          inOperatorCur = %trim(value);
        // extract compare type - Current
          startPos = endPos+1;
          If startPos <= 1;
            LeaveSr;
          Endif;
          Exsr ExtractValue;
          inTypeCur  = %trim(value);
        // extract compare value - Current
          startPos = endPos+1;
          If startPos <= 1;
            LeaveSr;
          Endif;
          Exsr ExtractValue;
          inValueCur  = %trim(value);
        // extract operator - Previous
          startPos = endPos+1;
          If startPos <= 1;
            LeaveSr;
          Endif;
          Exsr ExtractValue;
          inOperatorPrv = %trim(value);
        // extract compare type - Previous
          startPos = endPos+1;
          If startPos <= 1;
            LeaveSr;
          Endif;
          Exsr ExtractValue;
          inTypePrv = %trim(value);
        // extract compare value - Previous
          startPos = endPos+1;
          If startPos <= 1;
            LeaveSr;
          Endif;
          Exsr ExtractValue;
          inValuePrv  = %trim(value);
        On-Error;
          RtnCode = 'X';
        EndMon;
       EndSr;

       //=====================================================================
       Begsr SaveFieldComp;
        // Process update
        Exec SQL
         update SMTRPFLD
          set TRfieldTyp = :inFieldType,
              TRoperCur  = :inOperatorCur,
              TRcmpTypC  = :inTypeCur ,
              TRcmpValC  = :inValueCur,
              TRoperPrv  = :inOperatorPrv,
              TRcmpTypP  = :inTypePrv ,
              TRcmpValP  = :inValuePrv
          where rrn(SMTRPFLD) = :inRRN;
        Select;
        When SQLSTT <> '00000';
           BuildJSON(outJSON :'RtnCode' : 'SQL update failed. - SQL code: ' +
            SQLSTT);
        Other;
           BuildJSON(outJSON :'RtnSaved' : 'Update successful.');
        Endsl;

       Endsr;
      /End-Free
