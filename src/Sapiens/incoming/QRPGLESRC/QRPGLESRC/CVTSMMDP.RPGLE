    Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
    H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*
    H/TITLE CVTSMMDP
    H**************************************************************************************
    H*                                                                                    *
    H*  Description:  Convert SMMDP QDLS to IFS                                           *
    H*                                                                                    *
    H*  Created: 11/01/12   Programmer C:280   Project: c25wsj
    H*                                                                                    *
    H**************************************************************************************
    H*  Revisions:                                                                        *
    H*                                                                                    *
    H**************************************************************************************
    F*
     FSMMDP     UF   E             DISK
     F                                     INFDS(DBFDS)
    F*
    F* Multi-Object (doc) File
    F*
     FSMOLL1    IF   E           K DISK
    F*
    F* Object   Location File (Optical Support)-Visdart
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
     D isEnd           s               n
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
     D CVTIMDOP        pr                  ExtPgm('CVTIMDOP')
     D  ##rrn                         9
     D  stopdate                      8
     D  stoptime                      6
    D*
     DDateTo8          PR             8  0
     D date                            d   Const
     D
     DTimeTo6          Pr             6  0
     D time                            t   Const
    D*
     DsystemTime       PR              t
    D*
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

      // Read Data from SMMDP

            Chain intRrn pmd01;

            Dow Not %EOF(smmdp);

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

      // Only Process records currently not in IFS
               If MDuncf <> 'I';

      // Only Process if folder defined

               If mdoflr <> *blanks;
                  mdoid  = %subst(mdoid:1:12);
      // Determine if optical

                 Chain (mdkey:mdseq#:mdoflr:mdoid) pol01;

      // Not Optical

                   If not %found(smoll1);

      // Replace '\' with '/'

                     xxloc = 0;
                     xxloc = %scan('\':mdoflr);

                   If xxloc <> 0;
                     mdoflr = %replace('/':mdoflr:xxloc);
                   Endif;

                   mode     = 'P';
                   filename = 'SMMDP';
                   name     = mdoid;
                   folder   = mdoflr;
                   rrn      = dbrrn;
                   root     = rootdir;

                   CVTDLSIFSP(mode:filename:rrn:root:name:folder);

      // Update MDUNCP & MDUNCF

                   mduncp = '/' + %trim(rootdir) + '/' +
                                  %trim(mdoflr) + '/' +
                                  %trim(mdoid);
                   mduncf   = 'I';

      // Update file

                   update pmd01;

                   Endif;

               Endif;
              Endif;

      // Read Next Record

         Read pmd01;

         Enddo;

      // Update dataarea

            In *lock *dtaara;

              FileName     = 'SMMDP';
              Recordnumber = dbrrn;
              Lastrundate = %dec(%date():*USA);
              LastUser     = Q1USER;
              TotalRecords = TotalRecords + TotalRecord;
              Status       = 'Process';

            Out *dtaara;

           If isEnd = *off;
      // run cvtimdop

            ##rrn = '000000001';

            CVTIMDOP (##rrn:@@stopdate:@@stoptime);

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
         filename = 'SMMDP';
         rrn      = *zeros;
         root     = rootdir;
         folder   = mdoflr;

         CVTDLSIFSP(mode:filename:rrn:root:name:folder);

         // In *lock *dtaara;

         //      FileName     = 'SMMDP';
         //      Recordnumber = *zeros;
         //      Lastrundate = %dec(%date():*USA);
         //      LastUser     = Q1USER;
         //      TotalRecords = TotalRecords;
         //      Status       = 'Process';

         // Out *dtaara;

       EndSr;

      /end-free
