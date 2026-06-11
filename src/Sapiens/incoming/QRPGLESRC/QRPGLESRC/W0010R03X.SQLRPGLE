     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
      *=========================================================================
     H/TITLE -  Run SQL statements program                                     *
      *=========================================================================
      *                                                                        *
      *  DESCRIPTION: Run SQL statements to process the group copy             *
      *                                                                        *
      *                                                                        *
      *  CREATED: 01/24/2008  C:189 PROJECT WC1621                             *
      *  Rewrite: 04/10/2018  C:33  Project: 150WCF                            *
      *=========================================================================
      * Revisions:
      *
      * 04/29/24 C:340 Project: WC2965
      *                Clear the email field when copying groups.
      *
      * 12/28/22  C:342 Support: 105585
      *                 Update flag AAPORTDFT to blank when copying group to
      *                 WMAAP file.
      *
      * 09/04/19  C:291 Support: 82588
      *                 File WMDSP, WMDXSP, and WTDDP are not year level files.
      *                 Removed XSFYR, DSFYR, and DDFYR from compile time array
      *                 from YrField so these files get copied with other files.
      *                 Added special logic for WDXSP to only copy Group level
      *                 records.
      *
      * 07/31/19  C:212 Support:81174 / 80870
      *                 delete files from QTEMP once processing done
      *
      * 07/22/19  C:298 Support:80462
      *                 Include WDF4P
      *
      * 05/24/19  C:33  Project:SY406
      *                 Include WMFIP
      *=========================================================================
      *
     D decCount        s              3  0
     D tm6Current      s              6  0
     D dt7Current      s              7  0
     D i1              s              3  0
     D isDelete        s              1n   inz('0')
     D isDrop          s              1n   inz('1')
     D isFinalCopy     s              1n   inz('1')
     D strGroup        s              3
     D strLib          s             10
     D strLibFr        s             10    inz
     D strLibTo        s             10    inz
     D strQtemp        s             10    inz('QTEMP')
     D strSqlCopy      s            500
     D strSqlCreate    s            500
     D strSqlDelete    s            500
     D strSqlDrop      s            500
     D strSqlUpdate    s            500
     D strSqlWhere     s            500
      *
     D #ofFiles        c                   55
     D arrFileInfo     s                   like(dsFileInfo)                     OPTIONS,FUNCTIONS
     D                                     dim(#ofFiles) ctdata perrcd(1)
     D dsFileInfo      ds                  qualified
     D  file                         10
     D  grpField                     10
     D  yrField                      10
     D  initDtFld                    10
     D  initTmFld                    10
     D  initUsFld                    10
     D  initPgFld                    10

     D w0010r03x      uds                  dtaara(W0010R03X)
     D  inLibFr                      10
     D  inLibTo                      10
     D  inDelete                      1n
     D  inDrop                        1n

     D psds          esds                  extname(szq1p)

     DGetFileLib       pr            10
     D inFile                        10

     D/copy sPrDate
     D/copy sPrString

      *entry
     DGrpCpySQL        pr                  extpgm('W0010R03X')
     D inMode                        10
     D inGrpTo                        3
     D inGrpFrom                      3
     D inCopyYear                     1n
     D outError                       1
     D inNxtEmp#                      9    options(*nopass)
     D inNxtQte#                      9    options(*nopass)
     D inNxtCase#                     9    options(*nopass)
     DGrpCpySQL        pi
     D inMode                        10
     D inGrpTo                        3
     D inGrpFrom                      3
     D inCopyYear                     1n
     D outError                       1
     D inNxtEmp#                      9    options(*nopass)
     D inNxtQte#                      9    options(*nopass)
     D inNxtCase#                     9    options(*nopass)
      /Free
       Exec sql set option COMMIT = *NONE;
       Monitor;
          In w0010r03x;
          strLibFr = inLibFr;
          strLibTo = inLibTo;
          isDelete = inDelete;
          isDrop   = inDrop;
       On-error;
          Reset strLibFr;
          Reset strLibTo;
          Reset isDelete;
          Reset isDrop;
       Endmon;
       dt7Current = DateTo7(SystemDate);
       tm6Current = TimeTo6(SystemTime);
       outError = *off;
       Select;
       When Upper(inMode) = 'VERIFY';
          strGroup = inGrpTo;
       When Upper(inMode) = 'COPY';
          strGroup = inGrpFrom;
       Endsl;
       For i1 = 1 to #ofFiles;
          dsFileInfo = arrFileInfo(i1);
          Select;
          When dsFileInfo.file = *blank;
             Iter;
          When inCopyYear;
          When dsFileInfo.yrField <> *blank;
             Iter;
          Endsl;
          strLib = GetFileLib(dsFileInfo.file);
          If strLibFr <> *blanks;
             strLib = strLibFr;
          Endif;
          If strLibTo = *blanks;
             strLibTo = strLib;
          Endif;
          If strLibTo = strLib;
             isDelete = *off;
          Endif;
          strSqlWhere = 'where ' + %trim(dsFileInfo.grpField) + ' = ' +
           strGroup;
          If dsFileInfo.yrField <> *blank;
          Endif;
          Select;
          When %trim(dsFileInfo.file) = 'WMLDP';
             strSqlWhere = %trim(strSqlWhere) + ' and LDEMP# = 0';
          //*** Beg Add ***** 09/04/19 ******************************
          When %trim(dsFileInfo.file) = 'WDXSP';
             strSqlWhere = %trim(strSqlWhere) + ' and XSEMP# = 0';
          //*** End Add ***** 09/04/19 ******************************
          Endsl;
          strSqlCreate = 'create table ' + %trim(strQtemp) + '/' +
           %trim(dsFileInfo.file) +
           ' as (select * from ' + %trim(strLib) + '/ ' +
           %trim(dsFileInfo.file) + ' ' + %trim(strSqlWhere) + ') ' +
           'with data';
          strSqlDrop = 'drop table ' + %trim(strQtemp) + '/' +
           %trim(dsFileInfo.file);
          Exec sql execute immediate :strSqlDrop;
          Exec sql execute immediate :strSqlCreate;
          Select;
          When Upper(inMode) = 'VERIFY';
             Exec sql get diagnostics :decCount = ROW_COUNT;
             If decCount > 0;
                outError = *on;
             Endif;
          //***** Beg Add *** 07/31/19 *************************//
          // delete the QTEMP copy of file object
          Exec sql execute immediate :strSqlDrop;
          //***** End Add *** 07/31/19 *************************//
          When Upper(inMode) = 'COPY';
             Exsr CopyFile;
          Endsl;
          If isDrop;
             Exec sql execute immediate :strSqlDrop;
          Endif;
          Select;
          When outError = *on;
             Leave;
          Endsl;
       Endfor;
       *inlr = *on;
       //==================================================================
       // Proecss the file copy
       //==================================================================
       Begsr CopyFile;
        strSqlDelete = 'delete from ' + %trim(strLibTo) + '/' +
         %trim(dsFileInfo.file) + ' ' +
         'where ' + %trim(dsFileInfo.grpField) + ' = ' + inGrpTo;
        strSqlUpdate = 'update ' + %trim(strQtemp) + '/' +
         %trim(dsFileInfo.file) +
         ' set ' + %trim(dsFileInfo.grpField) + ' = ' + inGrpTo;
        Select;
       //*** Beg Chg ***  04/29/24  *** WC2965 *************************
        //When %trim(dsFileInfo.file) = 'WMFNP';
        //   strSqlUpdate = %trim(strSqlUpdate) + ', FNNAEN = ' + inNxtEmp# +
        //    ', FNNAQN = ' + inNxtQte# + ', FNNACS = ' + inNxtCase#;
        When %trim(dsFileInfo.file) = 'WMFNP';
           strSqlUpdate = %trim(strSqlUpdate) + ', FNNAEN = ' + inNxtEmp# +
            ', FNNAQN = ' + inNxtQte# + ', FNNACS = ' + inNxtCase# +
            ', FNEMAIL = '' ''';
       //*** End Chg ***  04/29/24  *** WC2965 *************************
        When %trim(dsFileInfo.file) = 'WDF2P';
           strSqlUpdate = %trim(strSqlUpdate) +
            ', F2JOBQ = '' '', F2SBS = '' ''';
        //***** Beg Add *** 12/28/22 *************************//
        When %trim(dsFileInfo.file) = 'WMAAP';
           strSqlUpdate = %trim(strSqlUpdate) +
            ', AAPORTDFT = '' ''';
        //***** End Add *** 12/28/22 *************************//
        Endsl;
        Exsr MaintFields;

        strSqlCopy = 'insert into ' + %trim(strLibTo) + '/' +
         %trim(dsFileInfo.file) + ' select * ' +
         ' from ' + %trim(strQtemp) + '/' + %trim(dsFileInfo.file);
        Exec sql execute immediate :strSqlUpdate;
        If isFinalCopy;
           If isDelete;
              Exec sql execute immediate :strSqlDelete;
           Endif;
           Exec sql execute immediate :strSqlCopy;
        Endif;
       Endsr;
       //==================================================================
       // Maintenance Fields
       //==================================================================
       Begsr MaintFields;
        If dsFileInfo.initDtFld <> *blank;
           strSqlUpdate = %trim(strSqlUpdate) +
            ', ' + %trim(dsFileInfo.initDtFld) + ' = ' +
            %editc(dt7Current:'X');
        Endif;
        If dsFileInfo.initTmFld <> *blank;
           strSqlUpdate = %trim(strSqlUpdate) +
            ', ' + %trim(dsFileInfo.initTmFld) + ' = ' +
            %editc(tm6Current:'X');
        Endif;
        If dsFileInfo.initUsFld <> *blank;
           strSqlUpdate = %trim(strSqlUpdate) +
            ', ' + %trim(dsFileInfo.initUsFld) + ' = ' +
            '''' + q1user + '''';
        Endif;
        If dsFileInfo.initPgFld <> *blank;
           strSqlUpdate = %trim(strSqlUpdate) +
            ', ' + %trim(dsFileInfo.initPgFld) + ' = ' +
            '''COPY_' + inGrpFrom + '''';
        Endif;
       Endsr;
      /End-free
      //==================================================================
     PGetFileLib       b
     DGetFileLib       pi            10
     D inFile                        10

     DRtvObjLib        pr                  extpgm('RTVOBJLIB')
     D outFile                       10    const
     D outType                        7    const
     D inLib                         10

     DstrLib           s             10
      /Free
       RtvObjLib(inFile:'*FILE':strLib);
       Return strLib;
      /End-free
     PGetFileLib       e
** File   GrpField  YrField   InitDtFld  InitTmFld  InitUsFld  InitPgFld
WMFNP     FNFND               FNIDT      FNITIM     FNIUSR     FNIPGM
WMFIP     FIFND               FIIDT      FIITIM     FIIUSR     FIIPGM
WACLP     CLFND               CLIDT      CLITM      CLIUSR     CLIPGM
WAMLP     MLFND               MLIDT      MLITM      MLIUSR     MLIPGM
WDCDP     CDFND               CDIDT                 CDIUSR     CDIPGM
WDFHP     FHFND               FHIDT      FHITIM     FHIUSR     FHIPGM
WDF1P     F1FND     F1FYR     F1IDT      F1ITM      F1IUSR     F1IPGM
WDF2P     F2FND               F2IDT      F2ITIM     F2IUSR     F2IPGM
WDF3P     F3FND
WDRCP     RCFND
WDECP     ECFND               ECIDT      ECITIM     ECIUSR     ECIPGM
WDHEP     HEFND               HEIDT      HEITIM     HEIUSR     HEIPGM
WDRBP     RBFND               RBIDT      RBITIM     RBIUSR     RBIPGM
WDRRP     RRFND               RRIDT      RRITIM     RRIUSR     RRIPGM
WDR1P     R1FND               R1IDT      R1ITIM     R1IUSR     R1IPGM
WDSCP     SCFND               SCIDT                 SCIUSR
WDSTP     STFND               STIDT      STITIM     STIUSR     STIPGM
WDXSP     XSFND               XSIDT      XSITIM     XSIUSR     XSIPGM             ** 09/04/19
WMAAP     AAFND               AAIDT      AAITM      AAIUSR     AAIPGM
WMADP     ADFND               ADIDT      ADITIM     ADIUSR     ADIPGM
WMARP     ARFND
WMAXP     AXFND               AXIDT                 AXIUSR
WMBIP     BIFND
WMBXP     BXFND               BXIDT      BXITM      BXIUSR     BXIPGM
WMCFP     CFFND               CFIDT      CFITIM     CFIUSR     CFIPGM
WMCIP     CIFND               CIIDT      CIITIM     CIIUSR     CIIPGM
WMCPP     CPFND
WMCXP     CXFND               CXIDT                 CXIUSR
WMDCP     DCGRP               DCIDT      DCITIM     DCIUSR     DCIPGM
WMDSP     DSFND               DSIDT      DSITIM     DSIUSR     DSIPGM             ** 09/04/19
WMDXP     DXFND               DXIDT      DXITM      DXIUSR     DXIPGM
WMEXP     EXFND               EXIDT      EXITM      EXIUSR     EXIPGM
WMFCP     FCFND               FCIDT      FCITIM     FCIUSR     FCIPGM
WMGCP     GCFND               GCIDT                 GCIUSR     GCIPGM
WMGRP     GRFND               GRIDT      GRITIM     GRIUSR     GRIPGM
WMIFP     IFFND               IFIDT      IFITIM     IFIUSR
WMLDP     LDFND               LDIDT      LDITIM     LDIUSR     LDIPGM
WMMGP     MGFND               MGIDT      MGITIM     MGIUSR     MGIPGM
WMNXP     NXFND               NXIDT      NXITIM     NXIUSR     NXIPGM
WMPCP     PCFND
WMPEP     PEFND               PEIDT                 PEIUSR     PEIPGM
WMPFP     PFFND               PFIDT      PFITM      PFIUSR     PFIPGM
WMREP     REFND               REIDT      REITM      REIUSR     REIPGM
WMRTP     RTFND               RTIDT      RTITIM     RTIUSR     RTIPGM
WMSTP     STFND               STIDT      STITIM     STIUSR     STIPGM
WTC3P     C3FND               C3IDT                 C3IUSR     C3IPGM
WTDDP     DDFND               DDIDT      DDITIM     DDIUSR     DDIPGM             ** 09/04/19
WTENP     ENFND
WTFIP     FIFND               FIIDT      FIITM      FIIUSR     FIIPGM
WTLMP     LMFND               LMIDT      LMITIM     LMIUSR     LMIPGM
WTMYP     MYFND
WTXCP     XCFND               XCIDT      XCITIM     XCIUSR     XCIPGM
WDF4P     F4FND
