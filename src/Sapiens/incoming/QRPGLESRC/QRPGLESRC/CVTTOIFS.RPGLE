    Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
    H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*
    H/TITLE CVTTOIFS - Convert documents from QDLS to IFS
    H**************************************************************************************
    H*                                                                                    *
    H*  Description:  This is a re-write of CVTSDCMP2, CVTSMMDP & CVTIMDOP                *
    H*                                                                                    *
    H*  Created: 06/24/14   Programmer C:33    Project: SY195
    H*                                                                                    *
    H**************************************************************************************
    H*  Revisions:                                                                        *
     H*
     H* 07/31/17  C:33  Project: 146WGV
     H*                 Allow for a single run as a direct call
    H*                                                                                    *
     H*  02/10/16 C:33  39476                                                              *
     H*                 Check folder validity recursively for multiple                     *
     H*                  sub-folders.
    H*                                                                                    *
     H*  01/26/16 C:33  39476                                                              *
     H*                 Remove *null characters from MDoId                                 *
    H*                                                                                    *
     H*  09/11/15 C:308 Project: 129WUA                                                    *
     H*                 Added bypassing of inactive record in SDCMP  and SMMDP.            *
     H*                 Added bypassing non optical and removed record in IMDOP.           *
    H*                                                                                    *
     H*  08/20/15 C:279 Project: 129WTJ                                                    *
     H*                 Added filter to not pick records which do not reside in QDLS.      *
     H*                                                                                    *
    H**************************************************************************************
    F*
     FSDCMP     uf   e             disk    infds(sdcmpFDS) rename(pcm01:pcm0p)
     FSMMDP     uf   e             disk    infds(smmdpFDS) rename(pmd01:pmd0p)
     FSMMDL3    if   e           k disk    infds(smmdl3FDS) rename(pmd01:pmd03)
     FIMDOP     uf   e             disk    infds(imdopFDS) rename(pdo01:pdo0p)
     FIMDOLb    if   e           k disk    infds(imdolbFDS) rename(pdo01:pdo0b)
     FSMOLL1    if   e           k disk
    D*
     D dbfds           ds                  qualified
     D  dbrrn                397    400B 0
     D sdcmpFDS        ds                  likeds(dbfds)
     D smmdpFDS        ds                  likeds(dbfds)
     D smmdl3FDS       ds                  likeds(dbfds)
     D imdopFDS        ds                  likeds(dbfds)
     D imdolbFDS       ds                  likeds(dbfds)
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
     D***  Begin Add  ***  07/31/17  ****************************************
     DstrMode          s              1    inz
     D***  End   Add  ***  07/31/17  ****************************************
     DstrStatus        s              5
     DstrFile          s             10
     DtotalRecord      s             10  0 inz
    D*
     D cvtDlsIfs       ds            60    dtaara
     D  fileName                     10
     D  recordNumber                  9  0
     D  lastRunDate                   8  0                                      mmddyyyy
     D  lastUser                     10
     D  totalRecords                 10  0
     D  status                       13
    D*
    D* Prototypes
    D*
     D ProcessSDCMP    pr             5
     D  inRRN                         9  0 const
     D  inMode                        1    const options(*nopass)
     D ProcessSMMDP    pr             5
     D  inRRN                         9  0 const
     D  inMode                        1    const options(*nopass)
     D ProcessIMDOP    pr             5
     D  inRRN                         9  0 const
     D  inMode                        1    const options(*nopass)
     DValidFolder      pr             1n
     D inFolder                            const like(CMfold)
     DValidFolderFmt   pr             1n
     D inFolder                            const like(CMfold)
    D*
     D RtvDocRoot      pr                  ExtPgm('RTVDOCROOT')
     D  rootDir                      10
    D*
     D MoveDocument    pr                  ExtPgm('CVTDLSIFS')
     D  mode                          1    const
     D  fileName                     10    const options(*nopass)
     D  rrn                           9  0 const options(*nopass)
     D  root                         10    const options(*nopass)
     D  name                         40    const options(*nopass)
     D  folder                       80    const options(*nopass)
    D*
    D*entry
     D CvtToIFS        pr                  ExtPgm('CVTTOIFS')
     D  @@file                       10
     D  @@rrn                         9
     D  @@stopDate                    8
     D  @@stopTime                    6
     D***  Begin Add  ***  07/31/17  ****************************************
     D  @@mode                        1    options(*nopass)
     D***  End   Add  ***  07/31/17  ****************************************
     D CvtToIFS        pi
     D  @@file                       10
     D  @@rrn                         9
     D  @@stopDate                    8
     D  @@stopTime                    6
     D***  Begin Add  ***  07/31/17  ****************************************
     D  @@mode                        1    options(*nopass)
     D***  End   Add  ***  07/31/17  ****************************************
     D*
     D psds          esds                  extname(szq1p)
    D*
     D/copy sPrDate
     D/copy sPrString
    D*
    D*================================================================
    D*  User defined fields
    D*================================================================
    C*
    C*
    C*================================================================
    C*  Key List
    C*================================================================
    C*
    C*
    C*================================================================
     C*
      /free

       intRrn = %int(@@rrn);
       stopDate = CvtToDate(%int(@@stopDate));
       stopTime = %time(%int(@@stopTime));
       //***  Begin Add  ***  07/31/17  ******************************
       Clear strMode;
       If %parms >= 5;
          strMode = @@mode;
       Endif;
       //***  End   Add  ***  07/31/17  ******************************

       // Check for program passed instead of file
       Select;
       When @@file = 'CVTSDCMP2';
          strFile = 'SDCMP';
       When @@file = 'CVTSMMDP';
          strFile = 'SMMDP';
       When @@file = 'CVTIMDOP';
          strFile = 'IMDOP';
       Other;
          strFile = @@file;
       Endsl;

      // QDLS Root ?
       RtvDocRoot(rootDir);
       If rootDir = 'QDLS';
          *inlr = *on;
          Return;
       Endif;

       //***  Begin Chg  ***  07/31/17  ******************************
       //Select;
       //When strFile = 'SDCMP';
       //   strStatus = ProcessSDCMP(intRRN);
       //When strFile = 'SMMDP';
       //   strStatus = ProcessSMMDP(intRRN);
       //When strFile = 'IMDOP';
       //   strStatus = ProcessIMDOP(intRRN);
       //Endsl;
       Select;
       When strFile = 'SDCMP';
          strStatus = ProcessSDCMP(intRRN:strMode);
       When strFile = 'SMMDP';
          strStatus = ProcessSMMDP(intRRN:strMode);
       When strFile = 'IMDOP';
          strStatus = ProcessIMDOP(intRRN:strMode);
       Endsl;
       //***  End   Chg  ***  07/31/17  ******************************

       In *lock *dtaara;
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
       Out *dtaara;

       //***  Begin Add  ***  07/31/17  ******************************
       Select;
       When strMode = 'S';
          Return;
       Endsl;
       //***  End   Add  ***  07/31/17  ******************************

       // Close program
       MoveDocument('E');

       *inlr = *on;

      /End-free
     P*========================================================================
     PProcessSDCMP     b
     DProcessSDCMP     pi             5
     D inRRN                          9  0 const
     D inMode                         1    const options(*nopass)

     D strFlr          s                   like(DOflr)
     D strSFlr         s                   like(DOsFlr)

      /Free
      // Read Data from SDCMP
       Chain inRRN pcm0p;

       Dow Not %EOF(sdcmp);

          // Get Current Date and Time
          sysTime = SystemTime;
          sysdate = SystemDate;

          // End Process ?
          Select;
          When %parms >= 2 and inMode = 'S';
             //***  Begin Add  ***  07/31/17  ******************************
             If inRRN <> sdcmpfds.dbrrn;
                Return '*SNGL';
             Endif;
             //***  End   Add  ***  07/31/17  ******************************
          When sysDate > stopDate;
            // Update dataarea
             Exsr UpdateDataArea;
             Return '*STOP';
          When sysDate = stopDate and sysTime > stopTime;
            // Update dataarea
             Exsr UpdateDataArea;
             Return '*STOP';
          Endsl;

          // Do not process records that have already been processed
        //***** Beg Chg *** 09/11/15 ***********************************************
         // If CMifsP = 'I';
          If CMifsP = 'I' or CMstat = 'I';
      //***** Chg End *** 09/11/15 ***********************************************
             // Read Next Record
             Read pcm0p;
             Iter;
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

                      MoveDocument('P':'SDCMP':sdcmpFDS.dbrrn
                       :rootDir:name:CMfold);

                      // Update CMUNCN & CMIFSP
                      CMuncN = '/' + %trim(rootDir) + '/' +
                       %trim(CMfold) + '/' + %trim(name);
                      CMifsP = 'I';
                      Update pcm0p;

                      Exsr LikeSMMDP;
                      Exsr LikeIMDOP;

                   Endif; // Name exists

                Endif; // CMfl05 <> 'O'

             Endif; // ValidFolder

          Endif; // CMfold <> *blank

       //***  Begin Add  ***  07/31/17  ******************************
          If %parms >= 2 and inMode = 'S';
             Return '*SNGL';
          Endif;
       //***  End   Add  ***  07/31/17  ******************************

          // Read Next Record
          Read pcm0p;

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

       Begsr LikeIMDOP;
        intLoc = %scan('/':CMfold);
        Select;
        When intLoc <> 0;
           strFlr = %subst(CMfold:1:intLoc-1);
           strSFlr = %subst(CMfold:intLoc+1);
        Other;
           strFlr = CMfold;
           strSFlr = *blanks;
        Endsl;

        Setll (name:strFlr:strSFlr) imdolb;
        Dou %eof(imdolb);
           Reade (name:strFlr:strSFlr) imdolb;
           If not %eof;
              ProcessIMDOP(imdolbFDS.dbrrn:'S');
           Endif;
        Enddo;

       Endsr;

       Begsr UpdateDataArea;
        In *lock *dtaara;
        fileName     = 'SDCMP';
        recordNumber = sdcmpFDS.dbrrn;
        lastRunDate = %dec(%date():*USA);
        lastUser     = Q1USER;
        totalRecords = totalRecords + totalRecord;
        status       = 'Process';
        Out *dtaara;
        Clear totalRecord;
       Endsr;

      /End-free
     PProcessSDCMP     e

     P*========================================================================
     PProcessSMMDP     b
     DProcessSMMDP     pi             5
     D inRRN                          9  0 const
     D inMode                         1    const options(*nopass)

      ***  Begin Add  ***  01/26/16  **************************************
     DstrCleanOId      s                   like(MDoId)
     Dindex            s              5  0
      ***  End   Add  ***  01/26/16  **************************************

      /Free
      // Read Data from SMMDP
       Chain inRRN pmd0p;

       Dow Not %EOF(smmdp);

          // Get Current Date and Time
          sysTime = SystemTime;
          sysdate = SystemDate;

          // End Process ?
          Select;
          When %parms >= 2 and inMode = 'S';
             //***  Begin Add  ***  07/31/17  ******************************
             If inRRN <> smmdpfds.dbrrn;
                Return '*SNGL';
             Endif;
             //***  End   Add  ***  07/31/17  ******************************
          When sysDate > stopDate;
            // Update dataarea
             Exsr UpdateDataArea;
             Return '*STOP';
          When sysDate = stopDate and sysTime > stopTime;
            // Update dataarea
             Exsr UpdateDataArea;
             Return '*STOP';
          Endsl;

          // Do not process records that have already been processed
        //***** Beg Chg *** 09/11/15 ***********************************
         // If MDuncF = 'I';
          If MDuncF = 'I' or MDstat = 'I';
         //***** End Chg *** 09/11/15 ***********************************************
             // Read Next Record
             Read pmd0p;
             Iter;
          Endif;

          // Count Records Processed
          totalRecord = totalRecord + 1;

          // Only Process if folder defined
          If MDoFlr <> *blanks;

          //***  Begin Add  ***  01/26/16  ********************************
          // Remove *null characters
             strCleanOId = MDoId;
             For index = 1 to %size(strCleanOId);
                If %subst(strCleanOId:index:1) = x'00';
                   %subst(strCleanOId:index:1) = x'40';
                Endif;
             Endfor;
          //***  End   Add  ***  01/26/16  ********************************

             // Determine if optical
             Chain (MDkey:MDseq#:MDoFlr:MDoId) pol01;
          //***  Begin Add  ***  01/26/16  ********************************
             MDoId = strCleanOId;
             If not %found(smoll1);
                Chain (MDkey:MDseq#:MDoFlr:MDoId) pol01;
             Endif;
          //***  End   Add  ***  01/26/16  ********************************

             // Not Optical
             If not %found(smoll1);

                // Replace '\' with '/'
                intLoc = %scan('\':MDoFlr);
                If intLoc <> 0;
                   MDoFlr = %replace('/':MDoFlr:intLoc);
                Endif;

                MoveDocument('P':'SMMDP':smmdpFDS.dbrrn
                 :rootDir:MDoId:MDoFlr);

                // Update MDuncP & MDuncF
                MDuncP = '/' + %trim(rootDir) + '/' +
                 %trim(MDoFlr) + '/' + %trim(MDoId);
                MDuncF   = 'I';
                Update pmd0p;

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

       Return ProcessIMDOP(1);

       Begsr UpdateDataArea;
        In *lock *dtaara;
        fileName     = 'SMMDP';
        recordNumber = smmdpFDS.dbrrn;
        lastRunDate = %dec(%date():*USA);
        lastUser     = Q1USER;
        totalRecords = totalRecords + totalRecord;
        status       = 'Process';
        Out *dtaara;
        Clear totalRecord;
       Endsr;

      /End-free

     PProcessSMMDP     e

     P*========================================================================
     PProcessIMDOP     b
     DProcessIMDOP     pi             5
     D inRRN                          9  0 const
     D inMode                         1    const options(*nopass)

      /Free
      // Read Data from SDCMP
       Chain inRRN pdo0p;

       Dow Not %EOF(imdop);

          // Get Current Date and Time
          sysTime = SystemTime;
          sysdate = SystemDate;

          // End Process ?
          Select;
          When %parms >= 2 and inMode = 'S';
             //***  Begin Add  ***  07/31/17  ******************************
             If inRRN <> imdopfds.dbrrn;
                Return '*SNGL';
             Endif;
             //***  End   Add  ***  07/31/17  ******************************
          When sysDate > stopDate;
            // Update dataarea
             Exsr UpdateDataArea;
             Return '*STOP';
          When sysDate = stopDate and sysTime > stopTime;
            // Update dataarea
             Exsr UpdateDataArea;
             Return '*STOP';
          Endsl;

          // Do not process records that have already been processed
        //***** Beg Chg *** 09/11/15 ***********************************
         // If DOifsP = 'I';
          If DOifsP = 'I' or DOoptf = 'Y' or DOfl02 = 'D';
        //***** End Chg *** 09/11/15 ***********************************
             // Read Next Record
             Read pdo0p;
             Iter;
          Endif;

          // Count Records Processed
          totalRecord = totalRecord + 1;

          // Only Process if folder defined
          If DOflr <> *blanks;

             // Replace '\' with '/'
             intLoc = %scan('\':DOflr);
             If intLoc <> 0;
                DOflr = %replace('/':DOflr:intLoc);
             Endif;
             intLoc = %scan('\':DOsFlr);
             If intLoc <> 0;
                DOsFlr = %replace('/':DOsFlr:intLoc);
             Endif;

             MoveDocument('P':'IMDOP':imdopFDS.dbrrn
              :rootDir:DOdcId:%trim(DOflr) + '/' + %trim(DOsFlr));

             // Update DOuncN & DOifsP
             DOuncN = '/';
             If rootDir <> ' ';
                DOuncN = %trim(DOuncN) + %trim(rootDir) + '/';
             Endif;
             If DOflr <> ' ';
                DOuncN = %trim(DOuncN) + %trim(DOflr) + '/';
             Endif;
             If DOsFlr <> ' ';
                DOuncN = %trim(DOuncN) + %trim(DOsFlr) + '/';
             Endif;
             If DOdcId <> ' ';
                DOuncN = %trim(DOuncN) + %trim(DOdcId);
             Endif;
             DOifsP   = 'I';
             Update pdo0p;

          Endif;

          If %parms >= 2 and inMode = 'S';
             Return '*SNGL';
          Endif;

          // Read next record
          Read pdo0p;

       Enddo;

      // Update dataarea
       Exsr UpdateDataArea;

       Return '*LAST';

       Begsr UpdateDataArea;
        In *lock *dtaara;
        fileName     = 'IMDOP';
        recordNumber = imdopFDS.dbrrn;
        lastRunDate = %dec(%date():*USA);
        lastUser     = Q1USER;
        totalRecords = totalRecords + totalRecord;
        status       = 'Process';
        Out *dtaara;
        Clear totalRecord;
       Endsr;

      /End-free
     PProcessIMDOP     e

     P*========================================================================
     P* Validate folder
     P*========================================================================
     PValidFolder      b
     DValidFolder      pi             1n
     D inFolder                                  const like(CMfold)

     disError          s              1n         inz
     DintLoc1          s              3  0       inz
     DintLoc2          s              3  0       inz
     D***  Begin Chg  ***  02/10/16  **************************************
     D**********strFold1         s             40          inz
     D**********strFold2         s             40          inz
     DstrFold1         s                         like(CMfold) inz
     DstrFold2         s                         like(CMfold) inz
     DisValidSubFlr    s              1n         inz('1')
     D***  End   Chg  ***  02/10/16  **************************************

      /Free
       intLoc1 = %scan('/':inFolder);

       strFold1 = inFolder;
       If intLoc1 <> 0;
          strFold1 = %subst(inFolder:1:intLoc1-1);
          intLoc2  = intLoc1 + 1;
          intLoc1  = %scan(' ':inFolder);
          strFold2 = %subst(inFolder:intLoc2);
          //***  Begin Add  ***  02/10/16  ********************************
          isValidSubFlr = ValidFolder(strFold2);
          //***  End   Add  ***  02/10/16  ********************************
       Endif;

       // strFold = strFold1;

       Select;
       //***  Begin Add  ***  02/10/16  ********************************
       When not isValidSubFlr;
          Return *off;
       //***  End   Add  ***  02/10/16  ********************************
       When not ValidFolderFmt(strFold1);
          Return *off;
       //***  Begin Del  ***  02/10/16  ********************************
       //**********When strFold2 <> *blank and not ValidFolderFmt(strFold2);
       //**********   Return *off;
       //***  End   Del  ***  02/10/16  ********************************
       //When %scan(' ':strFold1) > 13;
       //   Return *off;
       //When %scan('.':strFold1) > 9;
       //   Return *off;
       //When %scan('.':strFold1) = 0 and %scan(' ':inFolder) > 9;
       //   Return *off;
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
