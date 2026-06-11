     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H BNDDIR('STBNDDIR')
      //====================================================================
     H/title Quarterly PBILL Report
      //  Description:  Provider Bills by State Report
      //====================================================================
      //  Created: 01/24/11   C:251  Project: SY246
      //====================================================================
     //  Revisions:
      //
      //  07/12/2021 C: 079 Project: 99/96442
      //                    Sort by Adjudication State
      //                    Convert to Free Form
      //====================================================================
     // System Company
     FSMCOL     if   e           k disk
     FSMUPL     IF   E           K DISK
     F*** Beg Del *** 07/12/21 *********************************************
     F***FAEOL1    IF   E           K DISK
     F*** End Del *** 07/12/21 *********************************************
     FQSYSPRT   O    F  120        PRINTER OFLIND(*INOA)
       // ----------------------------------------------------------------
       // Standalone top -------------------------------------------------
       // ----------------------------------------------------------------
     D xxfrdt          s             10
     D xxtodt          s             10
     D xxco#           s              3
     D ##data          s             80
     D ##leng          s              3  0
     D*** Beg Chg *** 07/12/21 ********************************************
     D****** companynm       s             40
     D companynm       s             70
     D*** End Chg *** 07/12/21 ********************************************
     D xxcnt           s              7  0
     D xxtot           s              7  0
     D prvast          s                   like(eoast)
     D xxfdat          s                   like(eotrdt)
     D xxtdat          s                   like(eotrdt)
     D kkusr           s                   like(upusr)
     D kkco#           s                   like(eoco#)
      //** Beg Add *** 07/12/21 ********************************************
     D sqlStmtStr      s            700a   Inz(*blanks)
     D intFrmDt7       s                   Like(EOtrDt)
     D intToDt7        s                   Like(EOtrDt)
     D intDate8        s              8S 0
     D moreRows        s               n   Inz('1')
      //** End Add *** 07/12/21 ********************************************
       // ----------------------------------------------------------------
       // Standalone bottom ----------------------------------------------
       // ----------------------------------------------------------------
       // ----------------------------------------------------------------
       // Procedure Prototypes -------------------------------------------
       // ----------------------------------------------------------------
       /Copy sprString
       /Copy sprSQL
       // ----------------------------------------------------------------
       // Procedure Prototypes bottom ------------------------------------  --
       // ----------------------------------------------------------------
       // ----------------------------------------------------------------
       // Data structures top -------------------------------------------
       // ----------------------------------------------------------------
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0
     d
      //** Beg Add *** 07/12/21 ********************************************
     d  dsFAEOP      e ds                  ExtName(FAEOP)
      //** End Add *** 07/12/21 ********************************************
     DPSDS           ESDS                  Extname(SZQ1P)
       // ----------------------------------------------------------------
       // Data structures bottom ----------------------------------------
       // ----------------------------------------------------------------

     C     upky          klist
     C                   kfld                    kkusr
      //
     C     *entry        plist
     C                   parm                    qqmode            1
     C                   parm                    qqfdat            8
     C                   parm                    qqtdat            8
      //
      //----------------------------------------------------------------------
      //  Main Program
      //----------------------------------------------------------------------
      // Set initial values, convert from/to dates, etc.
      //
       Exsr init;
       Except head;
       //*** Beg Del *** 07/12/21 ********************************************
       //***Setll *loval peo01;
       //***Read peo01;
       //***Dow not %Eof(FAEOL1);
       //*** End Del *** 07/12/21 ********************************************
       //*** Beg Add *** 07/12/21 ********************************************
       //
       // SQL setup. Build SQL statement, declare cursor, etc.
       Exec Sql Set Option Commit = *None;
       Exsr SrBldSqlStmt;
       Exec Sql prepare stmt from :sqlStmtStr;
       Exec Sql declare eobcursor cursor for stmt;
       Exec Sql Close eobCursor;
       Exec Sql Open eobcursor;
       //
       // Fetch first records from FAEOP
       Exec Sql Fetch Next from EobCursor into :DSfaeop;
       If SqlStt <> '00000';
          moreRows = *off;
       Endif;

       // Process until there are no more rows
       Dow moreRows = *on;
       //*** End Add *** 07/12/21 ********************************************
         if eoast <> prvast and prvast <> *blanks;
           if *inoa = *on;
             except head;
             *inoa = *off;
           endif;
           except detail;
           xxtot = xxtot + xxcnt;
           xxcnt = *zeros;
         endif;
         xxcnt = xxcnt + 1;
         prvast = eoast;
       //*** Beg Del *** 07/12/21 ********************************************
       //***  read peo01;
       //***  *IN99 = %EOF;
       //***enddo;
       //*** End Del *** 07/12/21 ********************************************
       //*** Beg Add *** 07/12/21 ********************************************
          Exec Sql Fetch Next from EobCursor into :DSfaeop;
          If SqlStt <> '00000';
             moreRows = *off;
          Endif;
       Enddo;
       //*** End Add *** 07/12/21 ********************************************

       // provision for last state printing
       if *inoa = *on;
         except head;
         *inoa = *off;
       endif;
       except detail;
       xxtot = xxtot + xxcnt;
       xxcnt = *zeros;
       except totals;

       Exec Sql Close eobCursor;
       *inlr = *on;
       Return;
       //=====================================================================
       //    Subroutine SrBldSqlStmt;
       //=====================================================================
       Begsr SrBldSqlStmt;
         sqlStmtStr = 'Select * ' +
          'from FAEOP where EOco# = ' +
          %trim(xxCo#) +
          ' and EOblSt = ''C'' and EOtrDt >= '
          + %EditC(intFrmDt7:'X') +
          ' and EOtrDt <= ' +
          %EditC(intToDt7:'X') + ' and EOsys in(''P'', ''H'')'  +
          ' and EOblSt not in(''D'', ''X'')' +
          ' Order by EOast';
       Endsr;
       //=====================================================================
       //    Subroutine Init
       //=====================================================================
       begsr init;
         *inoa = *on;

         // Convert from and to dates
         xxfrdt = %subst(qqfdat:5:2) + '/' +
             %subst(qqfdat:7:2) + '/'+
             %subst(qqfdat:1:4);
         xxtodt = %subst(qqtdat:5:2) + '/' +
             %subst(qqtdat:7:2) + '/'+
             %subst(qqtdat:1:4);

     C                   time                    dsdttm
       //*** Beg Chg *** 07/12/21 ********************************************
     C****************   move      qqfdat        xxfdat
     C****************   move      qqtdat        xxtdat
         intDate8 = %Dec(qqFdat:8:0);
         intFrmDt7 = %Dec(%Date(intDate8:*ISO):*CYMD);
         intDate8 = %Dec(qqTdat:8:0);
         intToDt7 = %Dec(%Date(intDate8:*ISO):*CYMD);
       //*** End Chg *** 07/12/21 ********************************************

      // Get the company number
         kkUsr = Q1user;
         chain upky pup01;
         If %Found(SMUPL);
            kkCo# = UPco#;
         Endif;
         xxcnt = *zeros;
         xxtot = *zeros;
         prvast = *blanks;
         exsr getco;
       endsr;
       //=====================================================================
       //    Subroutine GetCo
       //=====================================================================
       Begsr GetCo;
         chain kkco# pco01;
         if %Found(SMCOL);
         //*** Beg Add *** 07/12/21 ********************************************
            xxCo# = %EditC(CoCo#:'X');
            ##data = %Trim(COname) + '    # ' + xxCo#;
            CompanyNm = CenterFld(##Data);
         //*** End Add *** 07/12/21 ********************************************
         //*** Beg Del *** 07/12/21 ********************************************
     C**** coname        cat(p)    '#':4         ##data
     C****               cat       xxco#:1       ##data
     C****               call      'SRCENTER'                           99
     C****               parm                    ##data
     C****               parm      40            ##leng
     C****               movel(p)  ##data        companyNm
       //*** End Del *** 07/12/21 ********************************************
         endif;
       Endsr;
       //===============================================================*
     OQsysprt   e            Head           1 05
     O                       dstime              09 '  :  :  '
     O                       CompanyNm           85
     O                       dsdate             120 '  /  /    '
      //
     O          e            head           1 06
     O                                           61 'Number of EOBs processed'
     O                                           70 'by State'
     O                                          115 'Page:'
     O                       page          Z    120
      //
     O          e            head           4
     O                                           43 'From:'
     O                       xxfrdt              54
     O                                           59 'To:'
     O                       xxtodt              70
     O          e            head           1
     O                                           12 'Adj. State'
     O                                           +2 'EOBs processed'
     O          e            head           1
     O                                           12 '----------'
     O                                           +2 '--------------'
      //
     O          e            detail      1  1
     O                       prvast              04
     O                       xxcnt         J     28
      //
     O          e            totals         1
     O                                           28 '--------------------------'
     O          e            totals         1
     O                                            7 'Total'
     O                       xxtot         J     28
