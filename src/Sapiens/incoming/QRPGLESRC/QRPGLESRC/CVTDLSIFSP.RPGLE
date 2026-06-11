    Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
    H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H/TITLE CVTDLSIFSR
    H**************************************************************************************
    H*                                                                                    *
    H*  Description:  Convert QDLS to IFS Prompt                                          *
    H*                                                                                    *
    H*  Created: 11/01/12   Programmer C:280   Project: c25wsj
    H*                                                                                    *
    H**************************************************************************************
    H*  Revisions:                                                                        *
    H*                                                                                    *
    H**************************************************************************************
    F*
     FCVTDLSIFSFCF   E             WORKSTN
    F*
    F* Prompt Screen
    F*
    D*
    D* Headings
    D*
     D name            C                   CONST('StoneRiver')
     D option          C                   CONST('Convert QDLS to IFS')
    D*
     D curDow          S              1  0
     D xxDays          S              3  0
     D xxTodte         S              8  0
    D*
     D program         s             10
     D rrn             s              9  0
     D todate          s              8  0
     D totime          s              6  0
    D*
    D* Prototypes
    D*
     D SrCenter        pr                  ExtPgm('SRCENTER')
    D*
     D CVTDLSSBM       pr                  ExtPgm('CVTDLSSBM')
     D  program                      10
     D  rrn                           9  0
     D  todate                        8  0
     D  totime                        6  0
    D*
     D cvtdlsifs       ds            60    dtaara
     D  filename                     10
     D  recordnumber                  9  0
     D  lastrundate                   8  0                                      mmddyyyy
     D  lastuser                     10
     D  totalrecords                 10  0
     D  status                       13
    D*
     D/copy spRdate
     D/copy scopypr01d
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
    C*----------------------------------------------------------------------
    C*  Process Request
    C*----------------------------------------------------------------------
    C*
    C*** DO WHILE NOT END OF JOB
    C*
      /free
          DOW *INLR = *OFF;

             EXFMT SCR01;

             *in10 = *off;
             *in11 = *off;
             *in12 = *off;
             *in13 = *off;
             *in99 = *off;

             // Exit
             IF *INKC;
                *INLR = *ON;
                LEAVE;
             ENDIF;

       // Reset
          If *inkh;
             Exsr Reset;
             *in99 = *on;
          Endif;
          If *in99 = *off;

       // Invalid day of the week

          If scdow < 1 or scdow > 7;
             *in10 = *on;
             *in99 = *on;
          Endif;

       // Invalid time of the day

          If sctmod = *zeros;
             *in11 = *on;
             *in99 = *on;
          Endif;

       // Invalid date

          If sclrdt <> *zeros;
             sclrdt= dtcheck(sclrdt);
               If sclrdt = 0;
                  *In12 = *On;
                  *In99 = *On;
                   Leave;
               EndIf;
          Else;
             *in12 = *on;
             *in99 = *on;
          Endif;

       // Invalid file

          If scfile <> *blanks;
               If scfile <> 'SDCMP' and scfile <> 'SMMDP' and
                  scfile <> 'IMDOP';
                  *In13 = *On;
                  *In99 = *On;
                   Leave;
               EndIf;
          Else;
             *in13 = *on;
             *in99 = *on;
          Endif;
          Endif;

       // No Errors Update Data Area and end pgm

          If not *in99;

             CurDow = dow(DateTo8(SystemDate()));

             xxDays = scDow - curDow;

             If xxDays < *zero;
                xxDays = 7 + xxDays;
             Endif;

             xxTodte = dtfuture(dateto8(SystemDate()):xxDays);

             Todate = %dec(%char(%date(xxtodte:*usa):*iso0):8:0);

             LastRunDate  = ScLrdt;
             LastUser     = ScLrus;
             FileName     = ScFile;
             RecordNumber = ScRrn;
             TotalRecords = ScTotr;
             Status       = ScSts;

             Out *dtaara;

       // No Errors Submit Job

             Rrn    = ScRrn;
             ToTime = Sctmod;

             Select;

             When scfile = 'SDCMP';
                  program = 'CVTSDCMP2';
             When scfile = 'SMMDP';
                  program = 'CVTSMMDP';
             When scfile = 'IMDOP';
                  program = 'CVTIMDOP';

             Endsl;

             CVTDLSSBM(program:rrn:todate:totime);

             *INLR = *ON;

           Endif;

          ENDDO;

      /END-FREE
    C*
    C**********************************************************************
    C* *inzsr - Initialization
    C**********************************************************************
    C*
      /FREE

       BegSr *Inzsr;

         ScDow = dow(DateTo8(SystemDate()));
         ScTmod = TimeTo6(SystemTime);

         In *lock *dtaara;

         If FileName = *blanks;

            Exsr Reset;

         Else;

            ScLRDT = LastRunDate;
            ScLRUS = LastUser;
            ScFile = FileName;
            ScRrn  = RecordNumber;
            ScTotr = TotalRecords;
            ScSts  = Status;

         Endif;
    
      // Center Company name in display field
    
         scconm = centerfld(name:40);
    
      // Center Option Name
    
         scprds = centerfld(option:40);

       Endsr;

      //*******************************************************************
      // Reset
      //*******************************************************************

       BegSr Reset;

            ScLRDT = DateTo8(systemDate());
            ScLRUS = q1user;
            ScFile = 'SDCMP';
            ScRrn  = 1;
            ScTotr = *Zeros;
            ScSts  = *Blanks;
            *in90 = *on;

       Endsr;


      /END-FREE
