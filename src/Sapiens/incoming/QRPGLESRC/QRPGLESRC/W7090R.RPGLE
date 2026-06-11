     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*
     H****************************************************************
     H*  W7090R - Fein/Mod Transfer Program
     H*
     H*  Description: This pgm transfers an old mod to new one
     H*
     H*  Created    : 10/27/2014  Programmer: 212  Project: 25MTT
     H*
     H*  Called By  : W7090C1
     H*  Calls To   :
     H*
     H****************************************************************
     H*  Revisions:
     H****************************************************************
    F* File definitions
    f*
    f* Provider Modifier From/To File
     fwaptp     if   e             disk
    f*
    f* Provider Modifier Transfer File
     fwat3p     if   e             disk
    f*
    f* Provider Mod Transfer History Detail file
     fwat1p     o    e             disk
    f*
    f* Payee demographics
     fwmpdl     uf   e           k disk
    f*
    f* Payee demographics
     ffmpdl     uf   e           k disk    rename(ppd01:ppd01f)
    f*
    f* Provider auxiliary file
     ffmp1l     uf   e           k disk
    f*
    f* Provider auxiliary file
     ffmp2l     uf   e           k disk
    f*
     D** Procedure definitions
     d*
     d GetSystem       pr                  ExtPgm('SA001R')
     d ##wcif                              like(##rtcd)                         WC INST
     d ##mcif                              like(##rtcd)                         MC INST
     d ##wcin                              like(##rtcd)                         WC INTERFACE
     d ##ar                                like(##rtcd)                         MC AR INST
     d ##ntwk                              like(##rtcd)                         NETWORK INST
     d ##ur                                like(##rtcd)                         PRECERT INST
     d ##med                               like(##rtcd)                         MEDMGMT INST
     d ##edi                               like(##rtcd)                         EDI INST
     d ##wcdk                              like(##rtcd)                         WC DARTS KEY
     d ##rhab                              like(##rtcd)                         REHAB INST
     d ##mss                               like(##rtcd)                         MS SUPPORT
     d ##fsch                              like(##rtcd)                         FEE SCHEDULE
     d ##prvb                              like(##rtcd)                         PROVIDER BILL

     d RunSQLStmt      pr                  ExtPgm('SYRUNSQL')
     d inSqlStmt                    550

     d** constant definitions
     d*
     d qot             C                   const('''')

     d** data structure definitions
     d*
     d psds          esds                  extname(szq1p)

     d** standalone definitions
     d*
     d @@delmod        s              1
     d @@rrn           s              9
     d @@rtncde        s              1
     d*
     d ##Rtcd          s              1    inz
     d*
     d ##Wcif          s                   like(##rtcd)
     d ##Mcif          s                   like(##rtcd)
     d ##Wcin          s                   like(##rtcd)
     d ##Ar            s                   like(##rtcd)
     d ##Ntwk          s                   like(##rtcd)
     d ##Ur            s                   like(##rtcd)
     d ##Med           s                   like(##rtcd)
     d ##Edi           s                   like(##rtcd)
     d ##Wcdk          s                   like(##rtcd)
     d ##Rhab          s                   like(##rtcd)
     d ##Mss           s                   like(##rtcd)
     d ##Fsch          s                   like(##rtcd)
     d ##Prvb          s                   like(##rtcd)
     d*
     d strSQLStmt      s            550    inz
     d updSQLStmt      s            550    inz
     d dltSQLStmt      s            550    inz
     d isSqlError      s               n   inz(*off)
     d*
     d @rrn#           s              9  0 inz
     d curdat7         s              7    inz
     d curtim6         s              6    inz
     d isBldUpdQry     s               n   inz(*off)
     d isBldDltQry     s               n   inz(*off)
     d delfilenm       s             10    inz
     d delfeinfld      s             10    inz
     d delmodfld       s             10    inz
     d dltcntr         s              2  0 inz
     d isDltInd        s               n   inz(*off)
     d*
    C*
     c     *entry        plist
     C                   parm                    @@delmod
     C                   parm                    @@rrn
     C                   parm                    @@rtncde
      /free
        //================================================================
        //mainline
        //================================================================

        // initialisation routine
         exsr srinit;

         Chain(n) @rrn# waptp;
         if %found(waptp);

           // verify if WCMC is installed
           getsystem(##wcif:##mcif:##wcin:##ar:##ntwk:##ur:##med:
                     ##edi:##wcdk:##rhab:##mss:##fsch:##prvb);

           // loop through to get the details of files that needs update
           Setll *start wat3p;
           Read wat3p;
           DoW not %eof(wat3p);

             // WC files
             if T3Product = 'W';
               isBldUpdQry = *on;
               exsr BuildSqlStmt;
               isBldUpdQry = *off;
               strSQLstmt = updSqlStmt;
             endif;

             // MC files
             if T3Product = 'M' and ##mcif = 'Y';
               isBldUpdQry = *on;
               exsr BuildSqlStmt;
               isBldUpdQry = *off;
               strSQLstmt = updSqlStmt;
             endif;

             // DARTS files
             if T3Product = 'D' and ##mcif = 'Y';
               isBldUpdQry = *on;
               exsr BuildSqlStmt;
               isBldUpdQry = *off;
               strSQLstmt = updSqlStmt;
             endif;

             // run the sql
             Monitor;
              Callp RunSqlStmt (strSQLstmt);
              isSqlError = *off;
             On-error;
              isSqlError = *on;
             EndMon;

             // write history record
             T1key = PTkey;
             T1File = T3FileNm;
             if isSqlError = *on;
               T1Note = 'Error processing SQL statement';
               T1Stat = 'E';
             else;
               T1Note = 'Processed';
               T1Stat = 'P';
             endif;
             T1Pgm = q1pgm;
             T1User = q1user;
             T1Dt = %int(curdat7);
             T1Time = %int(curtim6);
             T1Ipgm = q1pgm;
             T1IUsr = q1user;
             T1IDt = %int(curdat7);
             T1ITim = %int(curtim6);

             Write pt101;

             // read next record
             Read wat3p;
           EndDo;

           // verify if the fein/mod needs to be deleted
           if @@delmod = 'Y';
             isBldDltQry = *on;
             dltcntr = 0;
             isDltInd = *off;

             Dow *inlr = *off;

               dltcntr += 1;
               Select;

               When dltcntr = 1;
                DelFileNm = 'WMPDP';
                DelFeinFld = 'PDFEIN';
                DelModFld = 'PDMOD';
                isDltInd = *on;

               When dltcntr = 2;
                if ##mcif = 'Y';
                  DelFileNm = 'FMPDP';
                  DelFeinFld = 'PDFEIN';
                  DelModFld = 'PDMOD';
                  isDltInd = *on;
                endif;

               When dltcntr = 3;
                if ##mcif = 'Y';
                  DelFileNm = 'FMP1P';
                  DelFeinFld = 'P1FEIN';
                  DelModFld = 'P1MOD';
                  isDltInd = *on;
                endif;

               When dltcntr = 4;
                if ##mcif = 'Y';
                  DelFileNm = 'FMP2P';
                  DelFeinFld = 'P2FEIN';
                  DelModFld = 'P2MOD';
                  isDltInd = *on;
                endif;

               Other;
                isDltInd = *off;
                leave;

               Endsl;

               if isDltInd = *on;

                 exsr BuildSqlStmt;
                 strSQLstmt = dltSqlStmt;
                 // run the sql
                 Monitor;
                  Callp RunSqlStmt (strSQLstmt);
                  isSqlError = *off;
                 On-error;
                  isSqlError = *on;
                 EndMon;

                 // write history record
                 T1key = PTkey;
                 T1File = DelFileNm;
                 if isSqlError = *on;
                   T1Note = 'Error processing SQL statement for DELETE';
                   T1Stat = 'E';
                 else;
                   T1Note = 'Processed for DELETE';
                   T1Stat = 'P';
                 endif;
                 T1Pgm = q1pgm;
                 T1User = q1user;
                 T1Dt = %int(curdat7);
                 T1Time = %int(curtim6);
                 T1Ipgm = q1pgm;
                 T1IUsr = q1user;
                 T1IDt = %int(curdat7);
                 T1ITim = %int(curtim6);

                 Write pt101;
               endif;

             EndDo;

             isBldDltQry = *off;
           endif;

         endif;

         *inlr = *on;

        //================================================================
        // BuildSqlStmt - builds SQL statement
        //================================================================
         begsr buildsqlstmt;

           if isBldUpdQry = *on;

             // update statement
             updsqlstmt =

              'UPDATE ' +
                %trim(T3FileNm) + ' ' +
              'SET ' +
                %trim(T3ModFld) + ' = ' + %editc(PTToMod:'X');

             if T3PgmFld <> *blanks;
               updsqlstmt = %trim(updsqlstmt) +
                ' , ' + %trim(T3PgmFld) + ' = ' + qot + %trim(q1pgm) + qot;
             endif;

             if T3UserFld <> *blanks;
               updsqlstmt = %trim(updsqlstmt) +
                ' , ' + %trim(T3UserFld) + ' = ' + qot + %trim(q1user) + qot;
             endif;

             if T3DtFld <> *blanks;
               updsqlstmt = %trim(updsqlstmt) +
                ' , ' + %trim(T3DtFld) + ' = ' +  %trim(curdat7);
             endif;

             if T3TimeFld <> *blanks;
               updsqlstmt = %trim(updsqlstmt) +
                ' , ' + %trim(T3TimeFld) + ' = ' +  %trim(curtim6);
             endif;

             // add where clause
             updsqlstmt = %trim(updsqlstmt) +
              ' WHERE ' +
                 %trim(T3FeinFld) + ' = ' + %editc(PTFrmFein:'X') +
                 ' AND ' + %trim(T3ModFld) + ' = ' + %editc(PTFrmMod:'X')
                 ;

             if %subst(updsqlstmt:55:1) = *blanks;
               updsqlstmt = %subst(updsqlstmt:1:55) + ' ' +
                            %trim(%subst(updsqlstmt:56:495));
             endif;

             if %subst(updsqlstmt:110:1) = *blanks;
               updsqlstmt = %subst(updsqlstmt:1:110) + ' ' +
                            %trim(%subst(updsqlstmt:111:439));
             endif;

             if %subst(updsqlstmt:165:1) = *blanks;
               updsqlstmt = %subst(updsqlstmt:1:165) + ' ' +
                            %trim(%subst(updsqlstmt:166:385));
             endif;

           endif;

           if isBldDltQry = *on;

             // delete statement
             dltsqlstmt =

              'DELETE FROM ' +
                %trim(DelFileNm) +
              ' WHERE ' +
                 %trim(DelFeinFld) + ' = ' + %editc(PTFrmFein:'X') +
                 ' AND ' + %trim(DelModFld) + ' = ' + %editc(PTFrmMod:'X')
                 ;

           endif;

         endsr;

        //================================================================
        // SrInit - initialization subroutine
        //================================================================
         begsr srinit;

              @rrn# = %int(@@rrn);
              curdat7 = %char(%date():*cymd0);
              curtim6 = %char(%time():*iso0);

         endsr;
        //===============================================================
      /end-free

