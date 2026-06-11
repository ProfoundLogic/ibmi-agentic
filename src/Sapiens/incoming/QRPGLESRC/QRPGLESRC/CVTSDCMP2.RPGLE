    Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
    H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*
    H/TITLE CVTSDCMP
    H**************************************************************************************
    H*                                                                                    *
    H*  Description:  Convert SDCMP QDLS to IFS                                           *
    H*                                                                                    *
    H*  Created: 11/01/12   Programmer C:280   Project: c25wsj
    H*                                                                                    *
    H**************************************************************************************
    H*  Revisions:                                                                        *
    H*                                                                                    *
    H**************************************************************************************
    F*
     FSDCMP     UF   E             DISK
     F                                     INFDS(DBFDS)
    F*
    F* Remark Detail File
    F*
    D*
     D dbfds           DS
     D  dbrrn                397    400B 0
    D*
     DintRrn           s              9  0
     DsysTime          s                   like(q1time)
     DsysDate          s              8  0
     Dmode             s              1
     Drrn              s              9  0
     D##rrn            s              9
     Droot             s             10
     Drootdir          s             10
     Dname             s             40
     Dfolder           s             80
     Dstopdate         s              8  0
     Dstoptime         s              6  0
     Dxxloc            s              3  0
     DintLoc1          s              3  0
     DintLoc2          s              3  0
     DstrFold          s             40
     DstrFold1         s             40
     DstrFold2         s             40
     D isError         s               n
     D isEnd           s               n
     D intLoc          s              3  0 dim(12)
     D fstLoc          s              3  0
     D intCount        s              3  0
     D totalrecord     s             10  0 inz
    D*
     D cvtdlsifs       ds            60    dtaara
     D  filename                     10
     D  recordnumber                  9  0
     D  lastrundate                   8  0                                      mmddyyyy
     D  lastuser                     10
     D  totalrecords                 10  0
     D  status                       13
    D*
    D* Prototypes
    D*
    D*
     D RtvDocRoot      pr                  ExtPgm('RTVDOCROOT')
     D  RootDir                      10
    D*
     D CVTDLSIFSP      pr                  ExtPgm('CVTDLSIFS')
     D  mode                          1
     D  filename                     10
     D  rrn                           9  0
     D  root                         10
     D  name                         40
     D  folder                       80
    D*
     D CVTSMMDP        pr                  ExtPgm('CVTSMMDP')
     D  ##rrn                         9
     D  stopdate                      8
     D  stoptime                      6
    D*
     DTimeTo6          PR             6  0
     D time                            t   Const
    D*
     DsystemDate       PR              d
    D*
     DsystemTime       PR              t
     D psds          esds                  extname(szq1p)
    D*
     D/copy sprstring
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
    C*  PLIST definitions
    C*================================================================
    C*
     C     *entry        plist
     C                   parm                    @@rrn             9
     C                   parm                    @@stopDate        8
     C                   parm                    @@stopTime        6
    C*
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *blanks       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
    C*
    C*================================================================
     C*
    C*----------------------------------------------------------------------
    C*  Process Request
    C*----------------------------------------------------------------------
    C*
    C*** DO WHILE NOT END OF JOB
    C*
      /free

       isEnd = *off;
       intRrn = %int(@@rrn);
       stopDate = %int(@@stopDate);
       stopTime = %int(@@stopTime);

      // Get Current Date and Time

         RTVDOCROOT(rootdir);

      // QDLS Root ?

         If rootdir <> 'QDLS';

      // Read Data from SDCMP

            Chain intRrn pcm01;

            Dow Not %EOF(sdcmp);

      // Get Current Date and Time

           sysTime = TimeTo6(SystemTime);

           sysdate = %dec(%date():*iso);

      // End Process ?

         If sysDate > stopDate or (sysDate = stopDate and sysTime > stopTime);
            Exsr ProcessEnd;
            isEnd = *on;
            Leave;
         Endif;

      // Count Records Processed

               TotalRecord = TotalRecord + 1;

               If CMifsp <> 'I';
      // Only Process if folder defined

               isError = *off;
               If cmfold <> *blanks;

      // Replace '\' with '/'

             xxloc = 0;
             xxloc = %scan('\':cmfold);

             If xxloc <> 0;
               cmfold = %replace('/':cmfold:xxloc);
             Endif;

             Exsr validFold;

                If isError = *off;

                 If cmfl05 <> 'O';

                   If cmimgf = 'Y' or cmimgf = 'I';
                      name = cmidoc;
                   Else;
                      name = cmdcid;
                   Endif;

                  If name <> ' ' and (%scan('\':name) = 0) and
                   (%scan('/':name) = 0);
                   mode     = 'P';
                   filename = 'SDCMP';
                   rrn      = dbrrn;
                   root     = rootdir;
                   folder   = cmfold;

                   CVTDLSIFSP(mode:filename:rrn:root:name:folder);

      // Update CMUNCN & CMIFSP

                   cmuncn = '/' + %trim(rootdir) + '/' +
                                  %trim(cmfold) + '/' +
                                  %trim(name);
                   cmifsp   = 'I';

      // Update file

                   update pcm01;

                  Endif;
                 Endif;
                Endif;

               Endif;
              Endif;

      // Read Next Record

            Read pcm01;

            Enddo;

      // Update dataarea

            In *lock *dtaara;

              FileName     = 'SDCMP';
              Recordnumber = dbrrn;
              Lastrundate = %dec(%date():*USA);
              LastUser     = Q1USER;
              TotalRecords = TotalRecords + TotalRecord;
              Status       = 'Process';

            Out *dtaara;

           If isEnd = *off;
      // run cvtsmmdp

            ##rrn = '000000001';

            CVTSMMDP (##rrn:@@stopdate:@@stoptime);

            exsr ProcessEnd;

           Endif;
         Endif;

         *inlr = *on;

      /end-free

    C*----------------------------------------------------------------------
    C*  Process End Subroutine
    C*----------------------------------------------------------------------

      /free

       BegSr ProcessEnd;

         mode     = 'E';
         filename = 'SDCMP';
         rrn      = *zeros;
         root     = rootdir;
         folder   = cmfold;

         CVTDLSIFSP(mode:filename:rrn:root:name:folder);

         // In *lock *dtaara;

         //      FileName     = 'SDCMP';
         //      Recordnumber = *zeros;
         //      Lastrundate = %dec(%date():*USA);
         //      LastUser     = Q1USER;
         //      TotalRecords = TotalRecords;
         //      Status       = 'Process';

         // Out *dtaara;

       EndSr;

       //--------------------------------------------------------------------
       //  Validate Folder
       //--------------------------------------------------------------------

       BegSr ValidFold;

          intLoc1 = 0;
          intLoc2 = 0;
          strFold1 = ' ';
          strFold2 = ' ';
          intLoc1 = %scan('/':CMfold);

          If intLoc1 <> 0;
             intLoc1  = intLoc1 - 1;
             strFold1 = %subst(CMfold:1:intLoc1);
             intLoc2  = intLoc1 + 2;
             intLoc1  = 0;
             intLoc1  = %scan(' ':CMfold);
             strFold2 = %subst(CMfold:intLoc2:intLoc1);
          Else;
             strFold1 = CMfold;
          Endif;

          strFold = strFold1;
          Exsr vldFldFmt;

          If isError = *off and strFold2 <> ' ';
             strFold = strFold2;
             Exsr vldFldFmt;
          Endif;

       EndSr;

       //--------------------------------------------------------------------
       //  Validate Folder Format
       //--------------------------------------------------------------------

       BegSr vldFldFmt;

          intLoc1 = 0;
          intLoc2 = 0;
          fstLoc  = 1;
          intCount = 1;
          intLoc   = 0;
          intLoc1 = %scan(' ':strFold);

          Dow intCount <= 10 and isError = *off;
             intLoc(intCount) = %scan(' ':strFold:fstLoc);
             fstLoc = intLoc(intCount) + 1;
             If intCount > 1 and (intLoc(intCount) > intLoc(intCount - 1) + 1);
                isError = *on;
             Endif;
             intCount = intCount + 1;
          Enddo;

          If intLoc1 > 13;
             isError = *on;
          Else;
             intLoc2 = %scan('.':strFold);
             If intLoc2 > 9 or (intLoc2 = 0 and intLoc1 > 9);
                isError = *on;
             Endif;
          Endif;

       EndSr;
      /end-free
