     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h bnddir('STBNDDIR')
     fsmcol     if   e           k disk    rename(pco01:pco00)
     fsmtrl     if   e           k disk    rename(ptr01:ptr00)
     f                                     prefix(m)

     di1               s              5  0
     ddecDate          s              7  0 inz
     ddecTime          s              6  0 inz
     dstrEvent         s              8    inz('*UPDATE')
     dstrField         s             10    inz
     dstrFieldType     s             10    inz
     dstrFile          s             10    inz('WMPDP')
     dstrLib           s             10    inz('PDSFIL')
     dstrTrigName      s            128    inz

     D#ofFields        c                   14
     D aryField        s             10    dim(#ofFields) ctdata perrcd(1)
     D aryFieldType    s             10    dim(#ofFields) alt(aryField)

     d/copy sPrDate
     d/copy sPrTrigger

     dGetSuffix        pr             3

     d psds          esds                  extname(szq1p)
      /Free
       strLib = %trim(strLib) + GetSuffix;
       decDate = DateTo7(SystemDate);
       decTime = TimeTo6(Systemtime);
       Setll *loval smcol;
       Dou %eof(smcol);
          Read smcol;
          If %eof;
             Iter;
          Endif;
          mTRseq# = -1;
          Setgt (COco#:'FILE RULES') ptr00;
          Readpe (COco#:'FILE RULES') ptr00;
          mTRseq# += 1;
          Exec sql INSERT INTO SMTRP    (TRCO#, TRNAME, TRSEQ#,
           TRDESC, TRTYPE, TRSTAT,
           TRIUSR, TRIPGM, TRITIM, TRIDT)
           VALUES(:COco#, 'FILE RULES', :mTRseq#,
           'WMPDP - Name/Address Change', 'File', 'I',
           :q1user, :q1pgm, :decTime, :decDate);
          Exec sql INSERT INTO SMTRPACT (TRCO#, TRNAME, TRSEQ#,
           TRACTION, TREXTERNAL,
           TRIUSR, TRIPGM, TRITIM, TRIDT)
           VALUES(:COco#, 'FILE RULES', :mTRseq#,
           'Send Email', 'E',
           :q1user, :q1pgm, :decTime, :decDate);
          Exec sql INSERT INTO SMTRPFIL (TRCO#, TRNAME, TRSEQ#,
           TRLIBRARY, TRFILE,
           TRIUSR, TRIPGM, TRITIM, TRIDT)
           VALUES(:COco#, 'FILE RULES', :mTRseq#,
           :strLib, :strFile,
           :q1user, :q1pgm, :decTime, :decDate);
          For i1 = 1 to #ofFields;
             strFieldType = aryFieldType(i1);
             strField = aryField(i1);
             Exec sql INSERT INTO SMTRPFLD (TRCO#, TRNAME, TRSEQ#,
              TRTYPE, TRFIELD,
              TRIUSR, TRIPGM, TRITIM, TRIDT)
              VALUES(:COco#, 'FILE RULES', :mTRseq#,
              :strFieldType, :strField,
              :q1user, :q1pgm, :decTime, :decDate);
          Endfor;
          strTrigName = GetTriggerName(strLib:strFile:strEvent);
          Exec sql INSERT INTO SMTRPFRQ (TRCO#, TRNAME, TRSEQ#,
           TRTRGNAME, TRLIBRARY, TRFILE, TRTRGTIME, TRTRGEVENT, TRTRGPGM,
           TRIUSR, TRIPGM, TRITIM, TRIDT)
           VALUES(:COco#, 'FILE RULES', :mTRseq#,
           :strTrigName, :strLib, :strFile, '*AFTER', '*UPDATE', 'STTRIGGER',
           :q1user, :q1pgm, :decTime, :decDate);
          Exec sql INSERT INTO SMTRPLOG (TRCO#, TRNAME, TRSEQ#,
           TRACTION,
           TRIUSR, TRIPGM, TRITIM, TRIDT)
           VALUES(:COco#, 'FILE RULES', :mTRseq#,
           'Inactivate',
           :q1user, :q1pgm, :decTime, :decDate);
          Exec sql INSERT INTO SMTRPREC (TRCO#, TRNAME, TRSEQ#,
           TRTYPE, TREMAIL,
           TRIUSR, TRIPGM, TRITIM, TRIDT)
           VALUES(:COco#, 'FILE RULES', :mTRseq#,
           'Email', 'example@rules.sr',
           :q1user, :q1pgm, :decTime, :decDate);
       Enddo;
       *inlr = *on;
      /End-free
     P*==================================================================
     PGetSuffix        b
     dGetSuffix        pi             3

     dSA030R           pr                  extpgm('SA030R')
     d outSuffix                      3

     d strSuffix       s              3
      /Free
       SA030R(strSuffix);
       Return %trim(strSuffix);
      /End-free
     PGetSuffix        e
**        FieldType
PDNAME    Change
PDFSN     Change
PDADR1    Change
PDCTY     Change
PDST      Change
PDZIP     Change
PDFEIN    Report
PDMOD     Report
PDNAME    Report
PDFSN     Report
PDADR1    Report
PDCTY     Report
PDST      Report
PDZIP     Report
