     H Copyright('(c) 2025 Sapiens. All Rights Reserved.')
     H dftactgrp(*no) actgrp(*caller) bnddir('WTBNDDIR')
     H/TITLE WTIDCQTR1 - Indemnity Data Call Quarterly Build
     H DFTNAME(WTIDCQTR1)
     H/DEFINE   PROFOUNDUI
      *---------------------------------------------------------------*
      *  Created   : 04/11/25
      *  Programmer: C:053
      *  Support   : 118865 - WTIDCQTR running retroactively not
      *                       picking up transactions due to the
      *                       transaction run creating initial date
      *                       stamp outside the quarter.
      *---------------------------------------------------------------*
      *  Description:  IDC quarterly build.  Retro run.
      *                Based on WTICDQTR.
      *
      *---------------------------------------------------------------*
      * Revisions:                                                    *
      *                                                               *
      * 04/11/25    C:053  Support 118865
      *                    Retro run not picking up transactions
      *
      * 01/14/25    C:340  Project WC2995
      *                    Recompile Only;
      *
      *---------------------------------------------------------------*
      * 05/13/2022 C: 079 Support: 146/102029                         *
      *                   Fix logic that checks for Medical and Comp  *
      *                   changes                                     *
      *---------------------------------------------------------------*
     FWMCML3    IF   E           K DISK    Infds(fdsCML3) Rename(PCM01:PCM03)
     FWMCML1    IF   E           K DISK    Infds(fdsCML)
     FWMRHL2    IF   E           K DISK    rename(PRH01:PRH02)
     FWMIDCTRL2 IF   E           K DISK    rename(PTR01:PTR02)
     d*** Beg Add *** 04/11/25 *****************************
     FWMIDCTR   IF   E             DISK
     d*** End Add *** 04/11/25 *****************************
     FWMIDCQTL  IF   E           K DISK
     FWMIDCQTL5 UF   E           K DISK    rename(PQT01:PQT05)
     f*** Beg Add *** 05/13/22 ************************************************
     Fwmthl     if   e           k Disk
     f*** End Add *** 05/13/22 ************************************************
      * *Entry
      *
     d*** Beg Chg *** 04/11/25 *****************************
     D*WTIDCQTR ****** PR
     D WTIDCQTR1       PR
     d*** End Chg *** 04/11/25 *****************************
     D inCo#                          3A
     D inQtrDate                      8

     d*** Beg Chg *** 04/11/25 *****************************
     D*WTIDCQTR ****** PI
     D WTIDCQTR1       PI
     d*** End Chg *** 04/11/25 *****************************
     D inCo#                          3A
     D inQtrDate                      8

     D/copy WPRIDC
     d*** Beg Add *** 05/13/22 ************************************************
     d/copy sprSql
     d*** Beg Add *** 05/13/22 ************************************************

     D fdsCML          DS
     D  dbCMrrn              397    400B 0

     D fdsCML3         DS
     D  dbCMrrn3             397    400B 0

     D kkEntDt         S                   LIKE(CMentD) INZ
     D xxFromDt        S              8A   INZ
     D xxToDt          S                   LIKE(CMentD)INZ
     D xxFromDt7       S              7  0 inz
     d*** Beg Add *** 04/11/25 *****************************
     D xxtrndat7       S              7  0 inz
     d*** End Add *** 04/11/25 *****************************
     D xxInitDate      S              7  0 inz
     D intPyr          S                   LIKE(QTpYr) INZ
     D intPqtr         S                   LIKE(QTpQtr)INZ
     D intIdx          S              5  0 INZ
     D intRrn          S              9P 0 INZ
     D intIincur       S             11P 2 INZ
     D intMincur       S             11P 2 INZ
     D intALAEpdt      S             11P 2 INZ
     D intPrvCo#       S                   LIKE(CMco#) INZ
     D intPrvFnd       S                   LIKE(CMfnd) INZ
     D intPrvFyr       S                   LIKE(CMfyr) INZ
     D intPrvCase      S                   LIKE(CMcase) INZ
     D xxCoAmt         S              9  2 INZ
     D xxMeAmt         S              9  2 INZ
     D intCo#          S              3  0 INZ
     d*** Beg Add *** 05/13/22 ************************************************
     D xxCoAmtPrv      S              9  2 INZ
     D xxMeAmtPrv      S              9  2 INZ
     D xxMthYear       S              4A   INZ
     D xxFromDtPrv     S              8A   INZ
     D xxFromDt7Prv    S              7  0 INZ
     D xxToDtPrv       S              8A   INZ
     D xxToDt7Prv      S              7  0 INZ
     D sqlStmtStr      s            700a
     d RHco#Hld        s                   Like(RHco#)
     d RHfndHld        s                   Like(RHfnd)
     d RHfyrHld        s                   Like(RHfyr)
     d RHcaseHld       s                   Like(RHcase)
     D intPyrPrv       s                   LIKE(QTpYr) INZ
     D flgLtStatus     s              1
     d*** End Add *** 05/13/22 ************************************************
      *
     D                 DS
     D RT                      1     20    DIM(10)
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20

     D                 DS
     D AR                      1     90  2 DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2

     D                 DS
     D RR                      1     90  2 DIM(10)
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D RC                      1     20    DIM(10)
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS
     D SU                      1     90  2 DIM(10)
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
      /Free
       //*** Beg Add *** 05/13/22 *********************************************
     d dsReserveHist e ds                  extname(WMRHP) qualified inz
       //=====================================================================
       // Main Program
       //=====================================================================
       Exec Sql
         set Option commit = *none;
       Exec SQL close cursorReserveHist;
       //*** End Add *** 05/13/22 *********************************************
       If inQtrDate > *blanks;
          intCo#  = %int(inCo#);
          intPyr  = %int(%subst(inQtrDate:5:4));
          intPqtr = %int(%subst(GetPrcQtr(inQtrDate):1:1));

          If intPqtr > *Zero;
             xxFromDt  = %subst(GetPrcQtr(inQtrDate):2:4) +
                         %subst(inQtrDate:5:4);
             xxFromDt7 = %dec(%char(%date(xxFromDt:*usa0):*cymd0):7:0);
             xxToDt    = %dec(%char(%date(inQtrDate:*usa0):*cymd0):7:0);
             kkEntDt   = %dec(%char(%date(xxFromDt:*usa0):*cymd0):7:0);

             Setll (intCo#:kkEntDt) PCM03;
             Read PCM03;

             // 1. Process new claims
             Dow not %eof(WMCML3) and CMentD <= xxToDt;
                If CMentD >= xxFromDt7 and CMentD <= xxToDt;
                   intRrn = dbCMrrn3;
                   intIdx =  %Lookup('CO':RT);
                   If intIdx > *zero;
                      IntIincur = AR(intIdx) + RR(intIdx);
                   Else;
                      IntIincur = *zeros;
                   Endif;

                   If IsIdcState(CMco#:CMjrst)= *On and CMLTFL = 'I' and
                      isQtLogFnd(CMco#:CMfnd:CMfyr:CMcase:intPyr:intPqtr) = *on
                      and IntIincur <> *zero;
                      WriteQtLog(intRrn:inQtrDate);
                   Endif;
                Endif;
                Read PCM03;
             Enddo;

             //2. Process existing claims from Transactional within Quarter
        //*** Beg Chg *** 04/11/25 *****************************//
        //***Setll xxFromDt7 PTR02;
        //***Read PTR02;
        //***Dow not %eof(WMIDCTRL2) and TRidt <= xxToDt;
        //***   If TRidt >= xxFromDt7 and TRidt <= xxToDt;
        //***Setll *loval PTR01;
             Read PTR01;
             Dow not %eof(WMIDCTR);
                xxtrndat7 = %dec(%char(%date(TRtrndate:*iso0):*cymd0):7:0);
                If xxtrndat7 >= xxFromDt7 and xxtrndat7 <= xxToDt;
        //*** End Chg *** 04/11/25 *****************************//
                   Chain (TRco#:TRfnd:TRfyr:TRcase) PCM01;
                   If %found(WMCML1) and isQtLogFnd(CMco#:CMfnd:CMfyr:CMcase:
                                                    intPyr:intPqtr) = *on and
                      isIdcState(CMco#:CMjrst) = *on and CMltfl = 'I';
                      intRrn = dbCMrrn;
                      WriteQtLog(intRrn:inQtrDate);

                      //Check for  offsetting transfer and update its tran date
                      chain (CMco#:CMfnd:CMfyr:CMcase:intPyr:intPqtr) PQT05;
                      If %found(WMIDCQTL5) ;
                         QTtrnDate = %Subst(inQtrDate:5:4) +
                                     %Subst(inQtrDate:1:2) +
                                     %Subst(inQtrDate:3:2);
                         Update PQT05;
                      Endif;

                   Endif;
                Endif;
        //*** Beg Chg *** 04/11/25 *****************************//
        //***** Read PTR02;
                Read PTR01;
        //*** End Chg *** 04/11/25 *****************************//
             Enddo;

             //3. Process reserve changes current quarter compared to previous

             //*** Beg Add *** 05/13/22 ****************************************
             Exsr GetPrevQtrDates;
             //*** End Add *** 05/13/22 ****************************************

             // Current Quarter
             xxCoAmt = *zeros;
             xxMeAmt = *zeros;
             Setll xxFromDt7 PRH02;
             Read PRH02;
             intPrvCo# = RHco#;
             intPrvFnd = RHfnd;
             intPrvFyr = RHfyr;
             intPrvCase = RHcase;

             Dow not %eof(WMRHL2) and RHefdt <= xxToDt;

                If RHco# = intPrvCo# and RHfnd = intPrvFnd and
                   RHfyr = intPrvFyr and RHcase = intPrvCase;

                   If RHefdt >= xxFromDt7 and RHefdt <= xxToDt;

                      //Indemnity changes
                      intIdx = %lookup('CO':RC);
                      If intIdx > *zero;
                         xxCoAmt = xxCoAmt + SU(intIdx); //Indemnity Incurred
                      Endif;

                      //Medical changes
                      intIdx = %lookup('ME':RC);
                      If intIdx > *zero;
                         xxMeAmt = xxMeAmt + SU(intIdx); //Medical Incurred
                      Endif;

                   Endif;

                Else;
                   //*** Beg Add *** 05/13/22 **********************************
                   Exsr PrevReserveHist;
                   Exsr GetLtStatus;
                   //*** End Add *** 05/13/22 **********************************
                   Exsr ClmBreak;
                Endif;

                Read PRH02;
             Enddo;

             Exsr ClmBreak;

          Endif;
       Endif;
       *inLR = *ON;
       //-----------------------------------------------------------------------
       // Claim number break
       //-----------------------------------------------------------------------
       Begsr ClmBreak;
       //*** Beg Chg *** 05/13/22 *********************************************
       //***If xxCoAmt <> *Zero or xxMeAmt <> *Zero;
       If xxCoAmt <> xxCoAmtPrv or xxMeAmt <> xxMeAmtPrv;
       //*** End Chg *** 05/13/22 *********************************************
          Chain (intPrvCo#:intPrvFnd:intPrvFyr:intPrvCase) PCM01;
          If %found(WMCML1) and isQtLogFnd(CMco#:CMfnd:CMfyr:CMcase:
                                           intPyr:intPqtr)=*on and
              isIdcState(CMco#:CMjrst) = *on and CMltfl = 'I';
             intRrn = dbCMrrn;
             WriteQtLog(intRrn:inQtrDate);
          Endif;
       Endif;

       //*** Beg Chg *** 05/13/22 ****************************************
       //***intPrvCo# = RHco#;
       //***intPrvFnd = RHfnd;
       //***intPrvFyr = RHfyr;
       //***intPrvCase = RHcase;
       intPrvCo# = RHco#Hld;
       intPrvFnd = RHfndHld;
       intPrvFyr = RHfyrHld;
       intPrvCase = RHcaseHld;
       //*** End Chg *** 05/13/22 ****************************************
       xxCoAmt = *Zeros;
       xxMeAmt = *Zeros;
       Endsr;
       //*** Beg Add *** 05/13/22 ****************************************
       //-----------------------------------------------------------------------
       // Previous Quarter Reserve History - PrevReserveHist
       //-----------------------------------------------------------------------
       Begsr PrevReserveHist;

         RHco#Hld  = RHco#;
         RHfndHld  = RHfnd;
         RHfyrHld  = RHfyr;
         RHcaseHld = RHcase;

         xxCoAmtPrv = *zeros;
         xxMeAmtPrv = *zeros;
         sqlStmtStr = 'Select * from WMRHP +
          where RHco# = ' + %editc(intPrvCo#:'X') + ' and RHfnd = ' +
          %Editc(intPrvFnd:'X') + ' and RHfyr = ' +
          %Editc(intPrvFyr:'X') + ' and RHcase = ' +
          %Editc(intPrvCase:'X') + ' and RHefDt >= ' +
          %Editc(xxFromDt7Prv:'X') + ' and RHefDt <= ' +
          %Editc(xxToDt7Prv:'X');

          //Prepare the SQL statement
          Exec SQL prepare sqlReserveHist from :sqlStmtStr;

          //Declare the SQL cursor
          Exec SQL declare cursorReserveHist cursor for sqlReserveHist;

          //Run SQL
          Exec SQL open cursorReserveHist;

          //Get first record if there is one
          Dou not IsValidFetch(sqlstt);
             Exec SQL fetch next from cursorReserveHist into :dsReserveHist;
              If dsReserveHist.RHRT01 = 'CO';
                 xxCoAmtPrv = xxCoAmtPrv + dsReserveHist.RHsu01;
              Endif;
              If dsReserveHist.RHRT01 = 'ME';
                 xxMeAmtPrv = xxMeAmtPrv + dsReserveHist.RHsu01;
              Endif;
              If dsReserveHist.RHRT02 = 'CO';
                 xxCoAmtPrv = xxCoAmtPrv + dsReserveHist.RHsu02;
              Endif;
              If dsReserveHist.RHRT02 = 'ME';
                 xxMeAmtPrv = xxMeAmtPrv + dsReserveHist.RHsu02;
              Endif;
              If dsReserveHist.RHRT03 = 'CO';
                 xxCoAmtPrv = xxCoAmtPrv + dsReserveHist.RHsu03;
              Endif;
              If dsReserveHist.RHRT03 = 'ME';
                 xxMeAmtPrv = xxMeAmtPrv + dsReserveHist.RHsu03;
              Endif;
              If dsReserveHist.RHRT04 = 'CO';
                 xxCoAmtPrv = xxCoAmtPrv + dsReserveHist.RHsu04;
              Endif;
              If dsReserveHist.RHRT04 = 'ME';
                 xxMeAmtPrv = xxMeAmtPrv + dsReserveHist.RHsu04;
              Endif;
              If dsReserveHist.RHRT05 = 'CO';
                 xxCoAmtPrv = xxCoAmtPrv + dsReserveHist.RHsu05;
              Endif;
              If dsReserveHist.RHRT05 = 'ME';
                 xxMeAmtPrv = xxMeAmtPrv + dsReserveHist.RHsu05;
              Endif;
              If dsReserveHist.RHRT06 = 'CO';
                 xxCoAmtPrv = xxCoAmtPrv + dsReserveHist.RHsu06;
              Endif;
              If dsReserveHist.RHRT06 = 'ME';
                 xxMeAmtPrv = xxMeAmtPrv + dsReserveHist.RHsu06;
              Endif;
              If dsReserveHist.RHRT07 = 'CO';
                 xxCoAmtPrv = xxCoAmtPrv + dsReserveHist.RHsu07;
              Endif;
              If dsReserveHist.RHRT07 = 'ME';
                 xxMeAmtPrv = xxMeAmtPrv + dsReserveHist.RHsu07;
              Endif;
              If dsReserveHist.RHRT08 = 'CO';
                 xxCoAmtPrv = xxCoAmtPrv + dsReserveHist.RHsu08;
              Endif;
              If dsReserveHist.RHRT08 = 'ME';
                 xxMeAmtPrv = xxMeAmtPrv + dsReserveHist.RHsu08;
              Endif;
              If dsReserveHist.RHRT09 = 'CO';
                 xxCoAmtPrv = xxCoAmtPrv + dsReserveHist.RHsu09;
              Endif;
              If dsReserveHist.RHRT09 = 'ME';
                 xxMeAmtPrv = xxMeAmtPrv + dsReserveHist.RHsu09;
              Endif;
              If dsReserveHist.RHRT10 = 'CO';
                 xxCoAmtPrv = xxCoAmtPrv + dsReserveHist.RHsu10;
              Endif;
              If dsReserveHist.RHRT10 = 'ME';
                 xxMeAmtPrv = xxMeAmtPrv + dsReserveHist.RHsu10;
              Endif;
          Enddo;
          Exec SQL close cursorReserveHist;

       Endsr;
       //-----------------------------------------------------------------------
       // Get the Lost Time Status for the reporting period
       //-----------------------------------------------------------------------
       Begsr GetLtStatus;
        // If the Lost Time Status was I anytime during the reporting period
        // then set to I
         Chain (intPrvCo#:intPrvFnd:intPrvFyr:intPrvCase) pcm01;
         If %Found(WMCML1);
            flgLtStatus = CMltFl;
         Endif;

         // Find Lost Time History records within the quarter and if the
         // From or To is Indemnity then the claim was indenmnity during
         // some time of this quarter.  If no records are found look for the mos
         // recent record prior to this quarter.
         Setgt (intPrvCo#:intPrvFnd:intPrvFyr:intPrvCase:xxFromDt7) pth01;
         Readpe (CMco#:CMfnd:CMfyr:CMcase) pth01;
         If not %Eof(WMTHL) and THtrDt >= xxFromDt7;
            Dow not %Eof(WMTHL) and ThtrDt <= xxToDt;
               If THto = 'I' or Thfrom = 'I';
                  flgLtStatus = THto;
                  Leave;
               Endif;
               Reade (CMco#:CMfnd:CMfyr:CMcase) pth01;
            Enddo;
         Else;
            If Not %Eof(WMTHL) and THto = 'I';
               flgLtStatus = THto;
            Endif;
         Endif;

       Endsr;
       //-----------------------------------------------------------------------
       // Previous Quarter From/To Dates - GetPrevQtrDates
       //-----------------------------------------------------------------------
       Begsr GetPrevQtrDates;
        xxMthYear = %subst(GetPrcQtr(inQtrDate):2:4);
        xxFromDt7Prv = 0;
        xxToDt7Prv  = 0;
        intPyrPrv = intPyr - 1;

        Select;
          When xxMthYear = '0101';
             xxFromDtPrv  = '1001' + %Editc((intPyrPrv):'Z');
             xxFromDt7Prv = %dec(%char(%date(xxFromDtPrv:*usa0):
               *cymd0):7:0);
             xxToDtPrv = '1231' + %Editc((intPyrPrv):'Z');
             xxToDt7Prv = %dec(%char(%date(xxToDtPrv:*usa0):
               *cymd0):7:0);
          When xxMthYear = '0401';
             xxFromDtPrv = '0101' + %Editc(intPyr:'Z');
             xxFromDt7Prv = %dec(%char(%date(xxFromDtPrv:*usa0):
               *cymd0):7:0);
             xxToDtPrv = '0331' + %Editc(intPyr:'Z');
             xxToDt7Prv = %dec(%char(%date(xxToDtPrv:*usa0):
               *cymd0):7:0);
          When xxMthYear = '0701';
             xxFromDtPrv = '0401' + %Editc(intPyr:'Z');
             xxFromDt7Prv = %dec(%char(%date(xxFromDtPrv:*usa0):
               *cymd0):7:0);
             xxToDtPrv = '0630' + %Editc(intPyr:'Z');
             xxToDt7Prv = %dec(%char(%date(xxToDtPrv:*usa0):
               *cymd0):7:0);
          When xxMthYear = '1001';
             xxFromDtPrv = '0701' + %Editc(intPyr:'Z');
             xxFromDt7Prv = %dec(%char(%date(xxFromDtPrv:*usa0):
               *cymd0):7:0);
             xxToDtPrv = '0930' + %Editc(intPyr:'Z');
             xxToDt7Prv = %dec(%char(%date(xxToDtPrv:*usa0):
               *cymd0):7:0);
        Endsl;
       Endsr;
       //*** End Add *** 05/13/22 ****************************************
      /End-Free
