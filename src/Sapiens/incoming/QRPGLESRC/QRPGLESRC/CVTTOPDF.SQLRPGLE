    Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
    H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) bnddir('STBNDDIR')
    H*
    H/TITLE CVTTOPDF - Convert Documents to PDF
    H**************************************************************************************
    H*                                                                                    *
    H*  Description:  This program will convert documents to PDF.                         *
    H*                 It does not account for opitcal documents.                         *
    H*                                                                                    *
    H*  Created: 07/27/17   Programmer C:33    Project: 146WGV
    H*                                                                                    *
    H**************************************************************************************
    H*  Revisions:                                                                        *
    H*                                                                                    *
     H*  03/20/19 C:KXK PHW#77543
     H*                 All items not being processed because of checking
     H*                 field CMIMGF for non-blank value.
    H*  06/22/18 C:33  Project: 129WUU
     H*                 Log information about document not able to be
    H*                  converted.                                                        *
    H*                                                                                    *
    H*  06/05/18 C:KXK Project: 129WUU
     H*  Changed DLTSPLF to use the *LAST copy created.
    H**************************************************************************************
    F*
     FSDCMP     uf   e             disk    infds(sdcmpFDS)  rename(pcm01:pcm0p)
     FSMMDP     uf   e             disk    infds(smmdpFDS)  rename(pmd01:pmd0p)
     FSMMDL3    if   e           k disk    infds(smmdl3FDS) rename(pmd01:pmd03)
     FSMOLL1    if   e           k disk
    D*
     D dbfds           ds                  qualified
     D  dbrrn                397    400B 0
     D sdcmpFDS        ds                  likeds(dbfds)
     D smmdpFDS        ds                  likeds(dbfds)
     D smmdl3FDS       ds                  likeds(dbfds)
    D*
     DintRrn           s              9  0
     DsysTime          s               t
     DsysDate          s               d
     Drrn              s              9  0
     D##rrn            s              9
     Droot             s             10
     DrootDir          s             10
     Dname             s             40
     Dfolder           s             80
     DstopDate         s               d
     DstopTime         s               t
     DintLoc           s              3  0
     DisUpdate         s              1n   inz('1')
     DstrMode          s              1    inz
     DstrStatus        s              5
     DstrFile          s             10
     DtotalRecord      s             10  0 inz
    D*
     D cvtToPdf        ds            60    dtaara
     D  fileName                     10
     D  recordNumber                  9  0
     D  lastRunDate                   8  0                                      mmddyyyy
     D  lastUser                     10
     D  totalRecords                 10  0
     D  status                       13
    D*
    D* Prototypes
    D*
     DConvertDocument  pr             1n
     D inName                              like(CMdcId) const
     D inFold                              like(CMfold) const
     D inUncN                              like(CMuncN) const

     DMoveDocument     pr                  ExtPgm('CVTDLSIFS')
     D  mode                          1    const

     D ProcessSDCMP    pr             5
     D  inRRN                         9  0 const
     D  inMode                        1    const options(*nopass)
     D ProcessSMMDP    pr             5
     D  inRRN                         9  0 const
     D  inMode                        1    const options(*nopass)
     DValidFolder      pr             1n
     D inFolder                            const like(CMfold)
     DValidFolderFmt   pr             1n
     D inFolder                            const like(CMfold)
    D*
     D CvtToIFS        pr                  ExtPgm('CVTTOIFS')
     D  ##file                       10    const
     D  ##rrn                         9    const
     D  ##stopDate                    8    const
     D  ##stopTime                    6    const
     D  ##mode                        1    const options(*nopass)
    D*
     D RtvDocRoot      pr                  ExtPgm('RTVDOCROOT')
     D  rootDir                      10
    D*
    D*entry
     D CvtToPdfParm    pr                  ExtPgm('CVTTOPDF')
     D  @@file                       10
     D  @@rrn                         9
     D  @@stopDate                    8
     D  @@stopTime                    6
     D  @@mode                        1    options(*nopass)
     D CvtToPdfParm    pi
     D  @@file                       10
     D  @@rrn                         9
     D  @@stopDate                    8
     D  @@stopTime                    6
     D  @@mode                        1    options(*nopass)
     D*
     D psds          esds                  extname(szq1p)
    D*
     D/copy sPrDate
     D/copy sPrRunCmd
     D/copy sPrString
      /free
       Exec SQL
        set option commit = *none;
       strFile = @@file;
       intRrn = %int(@@rrn);
       stopDate = CvtToDate(%int(@@stopDate));
       stopTime = %time(%int(@@stopTime));
       Clear strMode;
       If %parms >= 5;
          strMode = @@mode;
       Endif;

      // QDLS Root ?
       RtvDocRoot(rootDir);
       If rootDir = 'QDLS';
          *inlr = *on;
          Return;
       Endif;

       Select;
       When strFile = 'SDCMP';
          strStatus = ProcessSDCMP(intRRN:strMode);
       When strFile = 'SMMDP';
          strStatus = ProcessSMMDP(intRRN:strMode);
       Endsl;

       In *lock cvtToPdf;
       lastRunDate  = %dec(%date():*USA);
       lastUser     = Q1USER;
       totalRecords = totalRecords;
       Select;
       When strStatus = '*LAST';
          fileName     = '*NONE';
          recordNumber = *zeros;
          status       = 'Completed';
       When strStatus = '*STOP';
          fileName     = fileName;
          recordNumber = recordNumber;
          status       = 'Stopped';
       When strStatus = '*SNGL';
       Endsl;
       Out cvtToPdf;

       // Close program
       MoveDocument('E');

       *inlr = *on;

      /End-free
     P*========================================================================
     PConvertDocument  b
     DConvertDocument  pi             1n
     D inName                              like(CMdcId) const
     D inFold                              like(CMfold) const
     D inUncN                              like(CMuncN) const

     DisArchived       s              1n   inz(*off)
     DisConverted      s              1n   inz(*off)
     DisPrinted        s              1n   inz(*off)
     DstrCmd           s            500
     DstrToDir         s            200
      /Free
       Monitor;
          strCmd = 'SYPRTDOC ' +
           'DOC(' + %trim(inName) + ') ' +
           'FLR(' + %trim(inFold) + ') ' +
           'OUTQ(QPRINT) SPLFILE(CVTTOPDF) ' +
           'UNCN(''' + %trim(inUncN) + ''') ' +
           'FSF(I) JOBQ(*NO)';
          RunCmd(%trim(strCmd):%len(%trim(strCmd)));
          isPrinted = *on;
       On-error;
          isPrinted = *off;
       Endmon;
       If isPrinted;
          Monitor;
             strCmd = 'SPLAMATIC/SPLCONVERT FILE(*LASTCRT) ' +
              'TOOBJ(''' + %trim(inUncn) + '.PDF'') ' +
              'TRANSFORM(*PDF811)';
             RunCmd(%trim(strCmd):%len(%trim(strCmd)));
             isConverted = *on;
          On-error;
             isConverted = *off;
          Endmon;
     /*   RunCmd('DLTSPLF FILE(CVTTOPDF)':22); */
          Monitor;
             RunCmd('DLTSPLF FILE(CVTTOPDF) SPLNBR(*LAST)':36);
          On-error;
          Endmon;
       Endif;
       If isConverted and isUpdate;
          Monitor;
             strToDir = '/' + %trim(rootDir) + '/' + %trim(inFold) + '/Archive';
             strCmd = 'CRTDIR  ' +
              'DIR(''' + %trim(strToDir) + ''') ';
             RunCmd(%trim(strCmd):%len(%trim(strCmd)));
          On-error;
          Endmon;
          Monitor;
             strCmd = 'MOV ' +
              'OBJ(''' + %trim(inUncN) + ''') ' +
              'TODIR(''' + %trim(strToDir) + ''') ';
             RunCmd(%trim(strCmd):%len(%trim(strCmd)));
             isArchived = *on;
          On-error;
             isArchived = *off;
          Endmon;
       Endif;
       Return isConverted;
      /End-free
     PConvertDocument  e
     P*========================================================================
     PLogNonConvert    b
     DLogNonConvert    pi
     D inFile                        10                 const
     D inRRN                          9  0              const
     D inName                              like(CMdcId) const
     D inFolder                            like(CMfold) const
     D inUncN                              like(CMuncN) const

     D strSQL          s           1000
      /Free
        strSQL = 'create table ' + %trim(Q1pLib) + '/CVTTOPDF ' +
         '(FILE_NAME CHAR   (10 )   NOT NULL WITH DEFAULT,' +
         ' FILE_RRN NUMERIC (9 , 0) NOT NULL WITH DEFAULT,' +
         ' DOC_ID CHAR      (12 )   NOT NULL WITH DEFAULT,' +
         ' FOLDER CHAR      (80 )   NOT NULL WITH DEFAULT,' +
         ' UNC CHAR         (255 )  NOT NULL WITH DEFAULT,' +
         ' UNC_RENAME CHAR  (255 )  NOT NULL WITH DEFAULT)';
        Exec SQL execute immediate :strSQL;
        strSQL = 'insert into ' + %trim(Q1pLib) + '/CVTTOPDF ' +
         'values(''' + %trim(inFile) + ''', ' + %editc(inRRN:'X') + ', ' +
         '''' + %trim(inName) + ''', ''' + %trim(inFolder) + ''', ' +
         '''' + %trim(inUncN) + ''', '''')';
        Exec SQL execute immediate :strSQL;
      /End-free
     PLogNonConvert    e

     P*========================================================================
     PProcessSDCMP     b
     DProcessSDCMP     pi             5
     D inRRN                          9  0 const
     D inMode                         1    const options(*nopass)

      /Free
      // Read Data from SDCMP
       Setll inRRN pcm0p;

       Dou %eof(sdcmp);
          Read(n) pcm0p;
          Select;
          When %eof;
             Iter;
          // Only process records that are not PDFs
          // **Start 03/20/19
          // When CMimgF <> ' ' or CMstat = 'I';
          When CMstat = 'I';
          // **End 03/20/19
             Iter;
          Endsl;

          // Get Current Date and Time
          sysTime = SystemTime;
          sysdate = SystemDate;

          // End Process ?
          Select;
          When %parms >= 2 and inMode = 'S';
          When sysDate > stopDate;
            // Update dataarea
             Exsr UpdateDataArea;
             Return '*STOP';
          When sysDate = stopDate and sysTime > stopTime;
            // Update dataarea
             Exsr UpdateDataArea;
             Return '*STOP';
          Endsl;

          // If Object is not on IFS, convert
          If CMifsP <> 'I' and CMfold <> *blanks;
             If isUpdate;
                CvtToIFS('SDCMP':%editc(sdcmpFDS.dbrrn:'X')
                 :@@stopDate:@@stopTime:'S');
                Chain(n) sdcmpFDS.dbrrn pcm0p;
             Endif;
          Endif;

          // Count Records Processed
          totalRecord = totalRecord + 1;

          // Only Process if folder defined
          If CMfold <> *blanks;
             // Replace '\' with '/'
             intLoc = %scan('\':CMfold);
             If intLoc <> 0;
                CMfold = %replace('/':CMfold:intLoc);
             Endif;
             If ValidFolder(CMfold);
                If CMfl05 <> 'O';
                   If CMimgF = 'Y' or CMimgF = 'I';
                      name = CMiDoc;
                   Else;
                      name = CMdcId;
                   Endif;
                   If name <> ' ' and (%scan('\':name) = 0) and
                    (%scan('/':name) = 0);
                      If ConvertDocument(name:CMfold:CMuncN) and isUpdate;
                         // Update CMUNCN & CMIMGF
                         Chain sdcmpFDS.dbrrn pcm0p;
                         CMuncN = %trim(CMuncN) + '.PDF';
                         CMimgF = 'O';
                         Update pcm0p;
                      Else;
                         LogNonConvert('SDCMP':sdcmpFDS.dbrrn:name:
                          CMfold:CMuncN);
                      Endif;
                      Exsr LikeSMMDP;
                   Endif; // Name exists
                Endif; // CMfl05 <> 'O'
             Endif; // ValidFolder
          Endif; // CMfold <> *blank
       Enddo;

      // Update dataarea
       Exsr UpdateDataArea;

       Return ProcessSMMDP(1);

       Begsr LikeSMMDP;
        Setll (CMfold:name) smmdl3;
        Dou %eof(smmdl3);
           Reade (CMfold:name) smmdl3;
           If not %eof;
              ProcessSMMDP(smmdl3FDS.dbrrn:'S');
           Endif;
        Enddo;
       Endsr;

       Begsr UpdateDataArea;
        In *lock cvtToPDF;
        fileName     = 'SDCMP';
        recordNumber = sdcmpFDS.dbrrn;
        lastRunDate  = %dec(%date():*USA);
        lastUser     = Q1USER;
        totalRecords = totalRecords + totalRecord;
        status       = 'Process';
        Out cvtToPdf;
        Clear totalRecord;
       Endsr;

      /End-free
     PProcessSDCMP     e

     P*========================================================================
     PProcessSMMDP     b
     DProcessSMMDP     pi             5
     D inRRN                          9  0 const
     D inMode                         1    const options(*nopass)

     DstrCleanOId      s                   like(MDoId)
     Dindex            s              5  0

      /Free
      // Read Data from SMMDP
       Setll inRRN pmd0p;

       Dou %eof(smmdp);
          Read(n) pmd0p;
          Select;
          When %eof;
             Iter;
          // Only process records that are not PDFs
          When MDoTyp <> ' ' or MDstat = 'I';
             Iter;
          Endsl;

          // Get Current Date and Time
          sysTime = SystemTime;
          sysdate = SystemDate;

          // End Process ?
          Select;
          When %parms >= 2 and inMode = 'S';
          When sysDate > stopDate;
            // Update dataarea
             Exsr UpdateDataArea;
             Return '*STOP';
          When sysDate = stopDate and sysTime > stopTime;
            // Update dataarea
             Exsr UpdateDataArea;
             Return '*STOP';
          Endsl;

          // If Object is not on IFS, convert
          If MDuncF <> 'I' and MDoFlr <> *blanks;
             If isUpdate;
                CvtToIFS('SMMDP':%editc(smmdpFDS.dbrrn:'X')
                 :@@stopDate:@@stopTime:'S');
                Chain(n) smmdpFDS.dbrrn pmd0p;
             Endif;
          Endif;

          // Count Records Processed
          totalRecord = totalRecord + 1;

          // Only Process if folder defined
          If MDoFlr <> *blanks;
             // Remove *null characters
             strCleanOId = MDoId;
             For index = 1 to %size(strCleanOId);
                If %subst(strCleanOId:index:1) = x'00';
                   %subst(strCleanOId:index:1) = x'40';
                Endif;
             Endfor;
             // Determine if optical
             Chain (MDkey:MDseq#:MDoFlr:MDoId) pol01;
             MDoId = strCleanOId;
             If not %found(smoll1);
                Chain (MDkey:MDseq#:MDoFlr:MDoId) pol01;
             Endif;
             // Not Optical
             If not %found(smoll1);
                // Replace '\' with '/'
                intLoc = %scan('\':MDoFlr);
                If intLoc <> 0;
                   MDoFlr = %replace('/':MDoFlr:intLoc);
                Endif;
                If ConvertDocument(MDoId:MDoFlr:MDuncP) and isUpdate;
                   // Update MDuncP & MDoTyp
                   Chain smmdpFDS.dbrrn pmd0p;
                   MDuncP = %trim(MDuncP) + '.PDF';
                   MDoTyp = 'O';
                   Update pmd0p;
                Else;
                   LogNonConvert('SMMDP':smmdpFDS.dbrrn:MDoId:
                    MDoFlr:MDuncP);
                Endif;
             Endif; // Not in SMOLL1
          Endif; // MDoFlr <> *blank

          If %parms >= 2 and inMode = 'S';
             Return '*SNGL';
          Endif;

          // Read next record
          Read pmd0p;

       Enddo;

      // Update dataarea
       Exsr UpdateDataArea;

       Return '*LAST';

       Begsr UpdateDataArea;
        In *lock cvtToPdf;
        fileName     = 'SMMDP';
        recordNumber = smmdpFDS.dbrrn;
        lastRunDate  = %dec(%date():*USA);
        lastUser     = Q1USER;
        totalRecords = totalRecords + totalRecord;
        status       = 'Process';
        Out cvtToPdf;
        Clear totalRecord;
       Endsr;

      /End-free

     PProcessSMMDP     e

     P*========================================================================
     P* Validate folder
     P*========================================================================
     PValidFolder      b
     DValidFolder      pi             1n
     D inFolder                                  const like(CMfold)

     disError          s              1n         inz
     DintLoc1          s              3  0       inz
     DintLoc2          s              3  0       inz
     DstrFold1         s                         like(CMfold) inz
     DstrFold2         s                         like(CMfold) inz
     DisValidSubFlr    s              1n         inz('1')

      /Free
       intLoc1 = %scan('/':inFolder);

       strFold1 = inFolder;
       If intLoc1 <> 0;
          strFold1 = %subst(inFolder:1:intLoc1-1);
          intLoc2  = intLoc1 + 1;
          intLoc1  = %scan(' ':inFolder);
          strFold2 = %subst(inFolder:intLoc2);
          isValidSubFlr = ValidFolder(strFold2);
       Endif;

       // strFold = strFold1;

       Select;
       When not isValidSubFlr;
          Return *off;
       When not ValidFolderFmt(strFold1);
          Return *off;
       Other;
          Return *on;
       Endsl;

      /End-free

     PValidFolder      e

     P*========================================================================
     P* Validate folder format
     P*========================================================================
     PValidFolderFmt   b
     DValidFolderFmt   pi             1n
     D inFolder                                  const like(CMfold)

     DintFirstLoc      s              3  0 inz(1)
     DintCount         s              3  0
     DaryLoc           s              3  0 dim(12)

      /Free

       For intCount = 1 to 10;
          aryLoc(intCount) = %scan(' ':inFolder:intFirstLoc);
          intFirstLoc = aryLoc(intCount) + 1;
          If intCount > 1 and (aryLoc(intCount) > aryLoc(intCount - 1) + 1);
             Return *off;
          Endif;
       Endfor;

       Select;
       When %scan(' ':inFolder) > 13;
          Return *off;
       When %scan('.':inFolder) > 9;
          Return *off;
       When %scan('.':inFolder) = 0 and %scan(' ':inFolder) > 9;
          Return *off;
       Endsl;

       Return *on;

      /end-free

     PValidFolderFmt   e
