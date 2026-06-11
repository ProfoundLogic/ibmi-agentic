     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H nomain
     H/TITLE STUSRASGN - User assignment subfile procedures
     H*****************************************************************
     H*                                                               *
     H* MODULE DESCRIPTION:  User assignment subfile procedures       *
     H*                                                               *
     H* CALLED BY: STUSRDRY                                           *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CREATED: 05/17/12  C:191 PROJECT: Transform                   *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H*   06/24/15 C:33  35210
     H*                  Only open WCMS files if WCMS is installed.
     H*
     H*   02/12/15 C:062 Project: 114WGU
     H*                  Add visit tracking count
     H*
     H*   05/13/13 C:279 Redmine: 2832 (LUBA QA)
     H*                  Modified to populate assignment count from
     H*                  Quote env as well.
     H*
     H*   05/06/13 C:191 Redmine 2846 (LCTA QA)
     H*                  Correct SQL for image counts
     H*
     H*****************************************************************
     F***  Begin Chg  ***  06/24/15  **************************************
     f**********wtgtl     if   e           k disk    rename(pgt01:pgt00)
     fwtgtl     if   e           k disk    rename(pgt01:pgt00) usropn

     d* Retrieve system values
     d RtvSysVal       pr                  ExtPgm('SA001R')
     d  ##Wcif                        1
     d  ##Mcif                        1
     d  ##Wcin                        1
     d  ##Ar                          1
     d  ##Ntwk                        1
     d  ##Ur                          1
     d  ##Med                         1
     d  ##Edi                         1
     d  ##Wcdk                        1
     d  ##Rhab                        1
     d  ##Mss                         1
     d  ##Fsch                        1
     d  ##Prvb                        1

     d ##Wcif          s              1
     d ##Mcif          s              1
     d ##Wcin          s              1
     d ##Ar            s              1
     d ##Ntwk          s              1
     d ##Ur            s              1
     d ##Med           s              1
     d ##Edi           s              1
     d ##Wcdk          s              1
     d ##Rhab          s              1
     d ##Mss           s              1
     d ##Fsch          s              1
     d ##Prvb          s              1
     D***  End   Chg  ***  06/24/15  **************************************
     f*
     d arrClmFlds      s             10    dim(conSize) inz
     d arrClmDesc      s             40    dim(conSize) inz
     d arrEmpFlds      s             10    dim(conSize) inz
     d arrEmpDesc      s             40    dim(conSize) inz
     d strClmSQL       s            512    varying inz
     d strEmpSQL       s            512    varying inz
     d***** beg add ****** 05/13/13 *************************
     d strQteSQL       s            512    varying inz
     d***** end add ****** 05/13/13 *************************
      *
     d dsEmplrFile   e ds                  ExtName(WMEMP)
     d dsClaimFile   e ds                  ExtName(WMCMP)
     D***** beg add ****** 05/13/13 ***************************************
     D dsQuoteFile   e ds                  ExtName(WMEMPQ) prefix(EQ:2)
     D***** end add ****** 05/13/13 ***************************************
     D*
      /copy scpyusrdry
       //*******************************************************************************************
       // Open SQL Cursor
       //*******************************************************************************************
     pOpenCurSor       b                   export
     dOpenCurSor       pi              n
     d inType                         1    const
     d*
     d strSQL          s            512    inz varying
      /Free
       Select;
       When inType = 'A';
       When inType = 'C';
         strSQL = strClmSQL;
       When inType = 'E';
         strSQL = strEmpSQL;
       //****** beg add ***** 05/13/13 *********************
       When inType = 'Q';
         strSQL = strQteSQL;
       //****** end add ***** 05/13/13 *********************
       EndSl;

       strSQL += ' FOR READ ONLY';

       Exec SQL Prepare sql1 From :strSQL;
       Exec SQL Declare cursor1 Cursor For sql1;
       Exec SQL Open cursor1;
       If SQLSTT <> '00000';
         Return *off;
       Else;
         Return *on;
       EndIf;
      /End-Free
     pOpenCurSor       e
       //*******************************************************************************************
       // Fetch Next claim
       //*******************************************************************************************
     pFetchNextClm     b                   export
     dFetchNextClm     pi                  like(SQLSTT)
     d ptrToDS                         *

     d sqlState        s                   like(SQLSTT)
      /Free
        Exec SQL Fetch next From cursor1 into :dsClaimFile;
        If SQLSTT < '02000';
          ptrToDS = %Addr(dsClaimFile);
          sqlState = '00000';
        EndIf;
        Return sqlState;
      /End-Free
     PFetchNextClm     e
       //*******************************************************************************************
       // Fetch Next employer
       //*******************************************************************************************
     pFetchNextEmp     b                   export
     dFetchNextEmp     pi                  like(SQLSTT)
     d ptrToDS                         *
     d sqlState        s                   like(SQLSTT)
      /Free
        Exec SQL Fetch next From cursor1 into :dsEmplrFile;
        If SQLSTT < '02000';
          ptrToDS = %Addr(dsEmplrFile);
          sqlState = '00000';
        EndIf;
        Return sqlState;
      /End-Free
     PFetchNextEmp     e
       //******* Beg Add ******* 05/13/13 **********************************************************
       //*******************************************************************************************
       // Fetch Next quote employer
       //*******************************************************************************************
     pFetchNextQte     b                   export
     dFetchNextQte     pi                  like(SQLSTT)
     d ptrToDS                         *
     d sqlState        s                   like(SQLSTT)
      /Free
        Exec SQL Fetch next From cursor1 into :dsQuoteFile;
        If SQLSTT < '02000';
          ptrToDS = %Addr(dsQuoteFile);
          sqlState = '00000';
        EndIf;
        Return sqlState;
      /End-Free
     PFetchNextQte     e
       //******* End Add ******* 05/13/13 **********************************************************
       //*******************************************************************************************
       // Close SQL Cursor
       //*******************************************************************************************
     PCloseCurSor      b                   export
     DCloseCurSor      pi
      /Free
       Exec SQL Close cursor1;
      /End-Free
     PCloseCurSor      e
       //*******************************************************************************************
       // Build SQL Statements
       //*******************************************************************************************
     PBuildSQLs        b                   export
     DBuildSQLs        pi
     D inStaffCd                      3    const
     D inEmpFld                      10    const
     D inClmFld                      10    const
     D*
     D strWhereC       s            128    inz varying
     D strWhereE       s            128    inz varying
      /Free
        strClmSQL = 'SELECT * FROM WMCMLI WHERE CMSTAT = ''O'' AND ';
        strEmpSQL = 'SELECT * FROM WMEML15 WHERE ';
        //***** beg add ***** 05/13/13 *************************
        strQteSQL = 'SELECT * FROM WMEML9 WHERE ';
        //***** end add ***** 05/13/13 *************************

        If inEmpFld <> *blanks;
          strWhereE += %Trim(inEmpFld) + ' = ''' + %Trim(inStaffCd)
           + '''';
        EndIf;

        If inClmFld <> *blanks;
          strWhereC += %Trim(inClmFld) + ' = ''' + %Trim(inStaffCd)
           + '''';
        EndIf;

        strClmSQL += strWhereC;
        strEmpSQL += strWhereE;
        //***** beg add ***** 05/13/13 *************************
        strQteSQL += strWhereE;
        //***** end add ***** 05/13/13 *************************

      /End-Free
     PBuildSQLs        e
       //*******************************************************************************************
       // Load field arrays
       //*******************************************************************************************
     PLoadFieldArray   b                   export
     DLoadFieldArray   pi
     D inCo#                          3  0 const
     D ptrToEmpFld                     *
     D ptrToEmpDsc                     *
     D ptrToClmFld                     *
     D ptrToClmDsc                     *
     D*
     D intI            s              3  0 inz(0)
     D strClmID        s              4    inz('CLSF')
     D strEmpId        s              4    inz('EMSF')
      /Free
        //Load claim arrays;
       //***  Begin Add  ***  06/24/15  ***********************************
        If not %open(wtgtl);
           RtvSysVal(##Wcif:##Mcif:##Wcin:##Ar:##Ntwk:##Ur:##Med:##Edi:
            ##Wcdk:##Rhab:##Mss:##Fsch:##Prvb);
           If ##Wcif = 'Y';
              Open wtgtl;
           Endif;
        Endif;

        If %open(wtgtl);
       //***  End   Add  ***  06/24/15  ***********************************

           Setll (inCo#:strClmId) pgt00;
           DoU %eof(wtgtl);
              Reade (inCo#:strClmId) pgt00;
              If %eof(wtgtl);
                 Leave;
              EndIf;
              If GTkey <> *blanks;
                 intI += 1;
                 arrClmFlds(intI) = GTkey;
                 arrClmDesc(intI) = GTdesc;
              EndIf;
              If intI = conSize;
                 Leave;
              EndIf;
           EndDo;

           //Load employer arrays;
           intI = 0;
           Setll (inCo#:strEmpId) pgt00;
           DoU %eof(wtgtl);
              Reade (inCo#:strEmpId) pgt00;
              If %eof(wtgtl);
                 Leave;
              EndIf;
              If GTkey <> *blanks;
                 intI = intI + 1;
                 arrEmpFlds(intI) = GTkey;
                 arrEmpDesc(intI) = GTdesc;
              EndIf;
              If intI = conSize;
                 Leave;
              EndIf;
           EndDo;

       //***  Begin Add  ***  06/24/15  ***********************************
        Endif;
       //***  End   Add  ***  06/24/15  ***********************************

        ptrToEmpFld = %Addr(arrEmpFlds);
        ptrToEmpDsc = %Addr(arrEmpDesc);
        ptrToClmFld = %Addr(arrClmFlds);
        ptrToClmDsc = %Addr(arrClmDesc);
      /End-Free
     PLoadFieldArray   e
       //*******************************************************************************************
       // Get SQL Count
       //*******************************************************************************************
     pGetSQLcount      b                   export
     dgetSQLcount      pi             9  0
     d inType                         1    const
     d*
     d strSQL          s            512    inz varying
     d rcount          s              9  0 inz
      /Free
       Select;
       When inType = 'A';
       When inType = 'C';
         strSQL = strClmSQL;
       When inType = 'E';
         strSQL = strEmpSQL;
       //****** beg add ***** 05/13/13 *********************
       When inType = 'Q';
         strSQL = strQteSQL;
       //****** end add ***** 05/13/13 *********************
       EndSl;

       strSQL += ' FOR READ ONLY';
       Exec SQL Prepare sqlCount From :strSQL;
       Exec SQL Declare curCnt Insensitive Cursor For sqlCount;
       Exec SQL Open curCnt;
       Exec SQL Get Diagnostics :rcount = DB2_NUMBER_ROWS;
       Exec SQL Close curCnt;
       Return rcount;
      /End-Free
     pGetSQLCount      e
       //*******************************************************************************************
       // Get Darts SQL Count
       //*******************************************************************************************
     pGetDartCount     b                   export
     dGetDartCount     pi             9  0
     d inUser                        10    const
     d inFromDate                     7  0 const
     d inOperator                     2    const
     d inToDate                       7  0 const options(*nopass)
     d*
     d strSQL          s            512    inz varying
     d rcount          s              9  0 inz
      /Free

       strSQL = 'SELECT Count(*) FROM SDCML4 WHERE CMUSER =''';
       strSQL += %Trim(inUser) + ''' AND CMCDAT';
       If %Parms >3 And inToDate > 0;
         strSQL += ' >= ' + %Trim(%Char(inFromDate));
         strSQL += ' AND CMCDAT < ' + %Trim(%Char(inToDate));
       Else;
         strSQL += inOperator +  %Trim(%Char(inFromDate));
       EndIf;

       strSQL += ' FOR READ ONLY';
       Exec SQL Prepare sqlDart From :strSQL;
       Exec SQL Declare curDart Cursor For sqlDart;
       Exec SQL Open curDart;
       Exec SQL Fetch next From curDart into :rcount;
       Exec SQL Close curDart;
       Return rcount;
      /End-Free
     pGetDartCount     e
       //*******************************************************************************************
       // Get Image Counts
       //*******************************************************************************************
     pGetImageCounts   b                   export
     dGetImageCounts   pi             9  0
     d inUser                        10    const
     d inType                         1    const
     d*
     d strSQL          s            512    inz varying
     d rcount          s              7  0 inz
     d totalCount      s              9  0 inz
      /Free
       totalCount = 0;
       If inType = 'U';
         strSQL = 'SELECT COUNT(*) FROM IMIELE WHERE (IESTAT =''A'' OR';
         strSQL += ' IESTAT = '' '') AND IEDSTS = ''RL'' AND IEQID =''';
         strSQL += %Trim(inUser) + '''';
       Else;
       //**** BEG CHANGE **** 05/06/13 ***********************
         strSQL = 'SELECT ' +
          '(SELECT COUNT(*) FROM IMIELE' +
          ' WHERE IEQID=IMIQP.IQID AND IESTAT<>''I''' +
          ' AND IEDSTS=''RL'') AS ROWCOUNT FROM IMIQP WHERE IQFL03 = ''Y''' +
          ' AND IQSTAT<>''I''';
       //****'  WHERE IEQID=IMIQP.IQID AND (IESTAT=''A'' OR IESTAT='' '')'+
       //****' AND IEDSTS=''RL'') AS ROWCOUNT FROM IMIQP WHERE IQFL03 = ''Y'')';
       //**** END CHANGE **** 05/06/13 ***********************
       Endif;
       strSQL += ' FOR READ ONLY';

       Exec SQL Prepare sqlImage From :strSQL;
       Exec SQL Declare curImage Cursor For sqlImage;
       Exec SQL Open curImage;

       Dou 1=0;
         Exec SQL Fetch Next From curImage into :rcount;
         If SQLSTT >= '02000';
           Leave;
         EndIf;
         totalCount += rcount;
       Enddo;

       Exec SQL Close curImage;
       Return totalCount;
      /End-Free
     pGetImageCounts   e
       // *** Begin Add *** 02/12/15 *************************************************
       //*******************************************************************************************
       // Get Visit SQL Count
       //*******************************************************************************************
     pGetVisitCount    b                   export
     dGetVisitCount    pi             9  0
     d inUser                        10    const
     d inFromDate                     7  0 const
     d inOperator                     2    const
     d inToDate                       7  0 const options(*nopass)
     d*
     d strSQL          s            512    inz varying
     d rcount          s              9  0 inz
      /Free

       strSQL = 'SELECT Count(*) FROM WMVIP WHERE VISTAT = ' + '''A''';
       strSQL += ' AND VIASSIGN =''';
       strSQL += %Trim(inUser) + ''' AND VIVSDT';
       If %Parms >3 And inToDate > 0;
         strSQL += ' >= ' + %Trim(%Char(inFromDate));
         strSQL += ' AND VIVSDT < ' + %Trim(%Char(inToDate));
       Else;
         strSQL += inOperator +  %Trim(%Char(inFromDate));
       EndIf;

       strSQL += ' FOR READ ONLY';
       Exec SQL Prepare sqlVisit From :strSQL;
       Exec SQL Declare curVisit Cursor For sqlVisit;
       Exec SQL Open curVisit;
       Exec SQL Fetch next From curVisit into :rcount;
       Exec SQL Close curVisit;
       Return rcount;

      /End-Free

     pGetVisitCount    e
       // *** End   Add *** 02/12/15 *************************************************
