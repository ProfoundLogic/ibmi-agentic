    Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
    H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*
    H/TITLE CVTIMODP
    H**************************************************************************************
    H*                                                                                    *
    H*  Description:  Convert IMODP QDLS to IFS                                           *
    H*                                                                                    *
    H*  Created: 11/01/12   Programmer C:280   Project: c25wsj
    H*                                                                                    *
    H**************************************************************************************
    H*  Revisions:                                                                        *
    H*                                                                                    *
    H**************************************************************************************
    F*
     FIMDOP     UF   E             DISK
     F                                     INFDS(DBFDS)
    F*
    F* Document Object Transaction File
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
     DstopDate         s              8  0
     DstopTime         s              6  0
     Droot             s             10
     Drootdir          s             10
     Dname             s             40
     Dfolder           s             80
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
     DTimeTo6          PR             6  0
     D time                            t   Const
    D*
     DsystemDate       PR              d
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

      // Read Data from IMDOP


         Chain intRrn pdo01;

         Dow Not %EOF(imdop);

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

         If DOifsp <> 'I';
      // Only Process if folder defined

               If doflr <> *blanks;

      // Replace '\' with '/'

             xxloc = 0;
             xxloc = %scan('\':doflr);

             If xxloc <> 0;
                doflr = %replace('/':doflr:xxloc);
             Endif;

             xxloc = 0;
             xxloc = %scan('/':dosflr);

             If xxloc <> 0;
                dosflr = %replace('\':dosflr:xxloc);
             Endif;

                 mode     = 'P';
                 filename = 'IMDOP';
                 name     = dodcid;
                 folder   = %trim(doflr) + '/' + %trim(dosflr);
                 rrn      = dbrrn;
                 root     = rootdir;

                 CVTDLSIFSP(mode:filename:rrn:root:name:folder);

      // Update DOUNCN & DOIFSP

                 DOuncn = '/';
                 If rootdir <> ' ';
                    DOuncn = %trim(DOuncn) + %trim(rootdir) + '/';
                 Endif;
                 If DOflr <> ' ';
                    DOuncn = %trim(DOuncn) + %trim(DOflr) + '/';
                 Endif;
                 If DOsflr <> ' ';
                    DOuncn = %trim(DOuncn) + %trim(DOsflr) + '/';
                 Endif;
                 If DOdcid <> ' ';
                    DOuncn = %trim(DOuncn) + %trim(DOdcid);
                 Endif;
                 DOifsp   = 'I';

      // Update file

                 update pdo01;

               Endif;
              Endif;

      // Read Next Record

         Read pdo01;

         Enddo;

          If isEnd = *on;

      // Update dataarea
              FileName     = 'IMDOP';
              Recordnumber = dbrrn;
              Lastrundate = %dec(%date():*USA);
              LastUser     = Q1USER;
              TotalRecords = TotalRecords + TotalRecord;
              Status       = 'Process';

            Out *dtaara;
          Else;

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

         In *lock *dtaara;

             FileName     = 'IMDOP';
             Recordnumber = *Zeros;
             Lastrundate  = %dec(%date():*USA);
             LastUser     = Q1USER;
             TotalRecords = TotalRecords;
             Status       = 'Completed';

         Out *dtaara;

             mode     = 'E';
             filename = *blanks;
             rrn      = *zeros;
             root     = *blanks;
             folder   = *blanks;

             CVTDLSIFSP(mode:filename:rrn:root:name:folder);

       EndSr;

      /end-free
