    Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
    H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) bnddir('STBNDDIR')
    H/TITLE CVTDLSIFSR
    H**************************************************************************************
    H*                                                                                    *
    H*  Description:  Convert Documents to PDF                                            *
    H*                                                                                    *
    H*  Created: 07/27/17   Programmer C:33    Project: 146WGV
    H*                                                                                    *
    H**************************************************************************************
    H*  Revisions:                                                                        *
    H*                                                                                    *
    H**************************************************************************************
     FCVTTOPDFF CF   E             WORKSTN
     D*
     D name            C                   CONST('StoneRiver')
     D option          C                   CONST('Convert Documents to PDF')
    D*
     D curDow          S              1  0
     D xxDays          S              3  0
     D xxTodte         S              8  0
    D*
     D file            s             10
     D rrn             s              9  0
     D todate          s              8  0
     D totime          s              6  0
     D isError         s              1n
    D*
    D* Prototypes
    D*
     D SrCenter        pr                  ExtPgm('SRCENTER')
    D*
     D CVTTOPDFS       pr                  ExtPgm('CVTTOPDFS')
     D  file                         10
     D  rrn                           9  0
     D  todate                        8  0
     D  totime                        6  0
    D*
     D cvtToPdf        ds            60    dtaara qualified
     D  fileName                     10
     D  recordNumber                  9  0
     D  lastRunDate                   8  0                                      mmddyyyy
     D  lastUser                     10
     D  totalRecords                 10  0
     D  status                       13
    D*
     D/copy spRdate
     D/copy scopypr01d
     D/copy sprstring
    C*
    C*----------------------------------------------------------------------
    C*  Process Request
    C*----------------------------------------------------------------------
      /Free
       Dou *inlr;
          Exfmt scr01;
          *in10 = *off;
          *in11 = *off;
          *in12 = *off;
          *in13 = *off;
          isError = *off;

          Select;
          //Exit
          When *inkc;
             *inlr = *on;
             Iter;
          // Reset
          When  *inkh;
             Exsr Reset;
             isError = *on;
          Other;
             // Edit screen
             Exsr EditScreen;
             // No Errors Update Data Area and end pgm
             If not isError;
                Exsr Process;
                *inlr = *on;
             Endif;
          Endsl;

       Enddo;

       //==================================================================
       BegSr *Inzsr;

         ScDow = dow(DateTo8(SystemDate()));
         scTmOD = TimeTo6(SystemTime);

         In *lock *dtaara;

         If cvtToPdf.fileName = *blanks;
            Exsr Reset;
         Else;
            scLrDt = cvtToPdf.lastRunDate;
            scLrUs = cvtToPdf.lastUser;
            scFile = cvtToPdf.fileName;
            scRRN  = cvtToPdf.recordNumber;
            scTotR = cvtToPdf.totalRecords;
            scSts  = cvtToPdf.status;
         Endif;
    
        // Center Company name in display field
         scconm = CenterFld(name:40);
    
        // Center Option Name
         scprds = CenterFld(option:40);

       Endsr;

       //==================================================================
       BegSr EditScreen;

         // Invalid day of the week
         If scdow < 1 or scdow > 7;
            *in10 = *on;
            isError = *on;
         Endif;
         // Invalid time of the day
         If scTmOD = *zeros;
            *in11 = *on;
            isError = *on;
         Endif;
         // Invalid date
         If DtCheck(scLrDt) = *zero;
            *In12 = *On;
            isError = *On;
         Endif;
         // Invalid file
         If scFile <> 'SDCMP' and scFile <> 'SMMDP';
            *In13 = *On;
            isError = *On;
         EndIf;

       Endsr;

       //==================================================================
       BegSr Process;

         CurDow = dow(DateTo8(SystemDate()));
         xxDays = scDow - curDow;
         If xxDays < *zero;
            xxDays = 7 + xxDays;
         Endif;
         xxTodte = DtFuture(dateto8(SystemDate()):xxDays);
         Todate = %dec(%char(%date(xxtodte:*usa):*iso0):8:0);
         cvtToPdf.lastRunDate  = scLrDt;
         cvtToPdf.lastUser     = scLrUs;
         cvtToPdf.fileName     = scFile;
         cvtToPdf.recordNumber = scRRN;
         cvtToPdf.totalRecords = scTotR;
         cvtToPdf.status       = scSts;
         Out *dtaara;
         Rrn    = scRRN;
         ToTime = scTmOD;
         CVTTOPDFS(scFile:rrn:todate:totime);

       Endsr;

       //==================================================================
       BegSr Reset;

         scLrDt = DateTo8(systemDate());
         scLrUs = q1user;
         scFile = 'SDCMP';
         scRRN  = 1;
         scTotR = *Zeros;
         scSts  = *Blanks;
         *in90 = *on;

       Endsr;

      /End-free
