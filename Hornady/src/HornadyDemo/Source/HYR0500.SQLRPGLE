     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2016')
     H DFTACTGRP(*NO) ACTGRP('HDSOPT')
     H BNDDIR('HYR0500')
      **************************************************************************
      * APPLICATION: Shipping                                                  *
      *       TITLE: UCC-128 SSCC and Tracking Number Association              *
      *          BY: William Earl (MCA)                                        *
      *        DATE: December 2016                                             *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------  *
      *  01/25/2017  MCA  Handle Varsity secondary order shipments.            *
      *  10/31/2017  MCA  Varsity FedEx changes.                               *
      *  07/17/2019  MCA  Handle zero-qty labels that occur from BoxLogix      *
      *                   back-end unship.                                     *
      *  06/03/2021  MCA  Add SWOG mode.                                       *
      *  11/18/2021  MCA  Handle shipping master-pack scan.                    *
      *  06/08/2022  MCA  Handle USPS complex tracking numbers.                *
      *  07/19/2022  MCA  $ByOrder - handle shipping master-pack scan.         *
      *  12/14/2022  MCA  Fix $ByOrder if screen data is passed back field-by- *
      *                   field.  This might be caused by scanner config or    *
      *                   by keyboard emulator config (not sure).              *
      *  04/26/2023  MCA  In SWOG mode, ensure all UCC-128 records have been   *
      *                   associated with tracking numbers before allowing     *
      *                   F11 to complete and then exit this program.          *
      *  02/22/2024  MCA  In SWOG mode, if no HYPSSCC records exist for the    *
      *                   customer# and dropship#, exit the program.           *
      *  09/16/2024  MCA  For SWOG mode, check for and handle UCC-128 records  *
      *                   still in the work file (HYPSCWK).  Now needed        *
      *                   because we're implementing SWOG into the BoxLogix/   *
      *                   DigiPick shipping program.                           *
      *  09/20/2024  MCA  Fix bug.  In SWOG mode, when retrieving the 8-digit  *
      *                   order number from the varsity order number, when the *
      *                   varsity order number is for a subsequent shipment    *
      *                   (and the order number has a hyphen), the code wasn't *
      *                   substringing the varsity order, and needed to.       *
      *                   Note: This code was added on 6/3/21 as part of the   *
      *                   original SWOG programming.  It was never encountered *
      *                   before now, because HYR0140 was not checking for     *
      *                   and hyphenating subsequent shipments until now.      *
      *  09/24/2024  MCA  In SWOG mode, check for UCC-128 records in both the  *
      *                   work file and the main file.  Determine whether to   *
      *                   use work or main file based on calling program and   *
      *                   which file has UCC-128 records for any active        *
      *                   turnarounds for the current customer.                *
      *  11/21/2024  MCA  Handle wk_MDORDR SQL null retrieval (no SQL0305).    *
      **************************************************************************
      *
     FHYD0500   CF   E             WORKSTN
     F                                     SFILE(SFL01:wk_Sfl01RRN)
     F                                     SFILE(SFL01A:wk_Sfl01ARRN)
     F                                     SFILE(SFL02:wk_Sfl02RRN)
     F                                     SFILE(SFL02A:wk_Sfl02ARRN)
     FHYW0500   UF A E           K DISK
     FHYPMPCD   IF   E           K DISK
      *
     D/EJECT
      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
     D HYR0500         PR                  EXTPGM('HYR0500')
     D  PgmMode                       1A   CONST                                1=By order, 2=SWOG
     D  Caller                       10A   CONST                                Calling program
     D  Order#                        8S 0 CONST
     D  Turn#                         9S 0 CONST
     D  Cust#                         7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  ShipVia                       2A   CONST
     D  Ord#Ary                       8P 0 DIM(600) CONST                       SWOG order array
     D  TurnAry                       9P 0 DIM(600) CONST                       SWOG turn array
     D  RtnCode                       2A

      /COPY *LIBL/QPRPSRC,CHKDIGIT

      **************************************************************************
      * Define *Entry parameter list.                                          *
      **************************************************************************
     D HYR0500         PI
     D  in_PgmMode                    1A   CONST                                1=By order, 2=SWOG
     D  in_Caller                    10A   CONST                                Calling program
     D  in_Order#                     8S 0 CONST
     D  in_Turn#                      9S 0 CONST
     D  in_Cust#                      7S 0 CONST
     D  in_DropShip#                  7S 0 CONST
     D  in_ShipVia                    2A   CONST
     D  in_Ord#Ary                    8P 0 DIM(600) CONST                       SWOG order array
     D  in_TurnAry                    9P 0 DIM(600) CONST                       SWOG turn array
     D  out_RtnCode                   2A

      **************************************************************************
      * Define program status data structure.                                  *
      **************************************************************************
     D                SDS
     D ps_Pgnm                 1     10
     D ps_JobN               244    253
     D ps_Job#               264    269

      **************************************************************************
      * Arrays                                                                 *
      **************************************************************************
     D ary_Ord#        S              8  0 DIM(600)                             SWOG order array
     D ary_Turn        S              9  0 DIM(600)                             SWOG turn array
     D idx1            S              3P 0
     D maxidx1         S                   LIKE(idx1) INZ(%elem(ary_Ord#))

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_PgmMode      S              1A
     D wk_Caller       S             10A
     D wk_Order#       S              8S 0
     D wk_Turn#        S              9S 0
     D wk_Cust#        S              7S 0
     D wk_DropShip#    S              7S 0
     D wk_ShipVia      S              2A
     D wk_RtnCode      S              2A
     D wk_Shto#        S              7S 0
     D wk_ShtoName     S             30A
     D wk_AlwF9        S              1N   INZ(*OFF)
     D wk_RmvPrior     S              1N   INZ(*OFF)
     D wk_Data         S              1A                                        'W'=Work, 'M'=Main
     D wk_ScanType     S              1A
     D wk_Serial#      S              9P 0
     D wk_SSCC         S             20A
     D wk_SSCCNbr      S             19P 0
     D wk_ChkDigit     S              1A
     D wk_SCORD#       S              8S 0
     D wk_SCTURN       S              9S 0
     D wk_MDORDR       S             20A
     D wk_OESHTO       S              7S 0
     D wk_OEDSHP       S              7S 0
     D wk_TDVORD       S             10A
     D wk_MHSHIN       S             15A
     D wk_TDFF07       S              1A
     D wk_LongTrk#     S              1N
     D wk_Track#       S             35A
     D wk_HypPos       S              3P 0
     D wk_Sfl01RRN     S              5P 0
     D wk_Sfl01ARRN    S              5P 0
     D wk_Sfl02RRN     S              5P 0
     D wk_Sfl02ARRN    S              5P 0
     D wk_TimeStamp    S               Z
     D wk_DltWk        S              1N   INZ(*OFF)
     D wk_LPNeedTrk    S              7P 0
     D wk_MPScanned    S              1N   INZ(*OFF)
     D wk_MP#          S                   LIKE(MDMP#)
     D wk_MDMP#        S                   LIKE(MDMP#)
     D wk_Len          S              3P 0
     D wk_StrPos       S              3P 0
     D wk_EndPos       S              3P 0
     D wk_RcdCnt       S              7P 0
     D wk_RcdCnt1      S              7P 0
     D wk_RcdCnt2      S              7P 0
     D wk_Error        S              1N   INZ(*OFF)
     D wk_MsgID        S              7A
     D wk_MsgFile      S             10A   INZ('GUPMSGF')
     D wk_MsgData      S            512A
     D sv_C1SSCC       S                   LIKE(C1SSCC)
     D sv_C1SSCC2      S                   LIKE(C1SSCC)
     D sv_C2SCAN       S                   LIKE(C2SCAN)

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D digits          C                   '0123456789'

     C/EJECT
      **************************************************************************
      * Main routine.                                                          *
      **************************************************************************

     C                   EVAL      wk_PgmMode = in_PgmMode
     C                   EVAL      wk_Caller = in_Caller
     C                   EVAL      wk_Order# = in_Order#
     C                   EVAL      wk_Turn# = in_Turn#
     C                   EVAL      wk_Cust# = in_Cust#
     C                   EVAL      wk_DropShip# = in_DropShip#
     C                   EVAL      wk_ShipVia = in_ShipVia
     C                   EVAL      ary_Ord# = in_Ord#Ary
     C                   EVAL      ary_Turn = in_TurnAry
     C                   EVAL      wk_RtnCode = *Zeros
     C                   EVAL      out_RtnCode = *Zeros

      /Free
       IF wk_ShipVia = *Blanks;
         wk_TDVORD = *Blanks;
         wk_MHSHIN = *Blanks;
         EXEC SQL select TDVORD into :wk_TDVORD
                  from HYPTDTA
                  where TDTURN=:wk_Turn#;
         IF wk_TDVORD <> *Blanks;
           EXEC SQL select MHSHIN into :wk_MHSHIN
                    from MFH1MH
                    where MHCMNO='01' and MHOTYP=' ' and
                          MHORDR=:wk_TDVORD;
           IF %len(%trimr(wk_MHSHIN)) = 2;
             wk_ShipVia = wk_MHSHIN;
           ENDIF;                                                            // %len(%trimr(wk_MHSHI
         ENDIF;                                                              // wk_TDVORD<>*Blanks
       ENDIF;                                                                // wk_ShipVia=*Blanks

       wk_TDFF07 = *Blank;
       EXEC SQL select TDFF07 into :wk_TDFF07
                from GUPTDAT
                where TDTABL='SHIPVIA' and TDKEY1=:wk_ShipVia;
       IF wk_TDFF07 = 'Y';
         wk_LongTrk# = *ON;
       ELSE;
         wk_LongTrk# = *OFF;
       ENDIF;                                                                // wk_TDFF07='Y'
      /End-Free

     C                   SELECT
     C                   WHEN      wk_PgmMode = '1'                             By order
     C                   EXSR      $ByOrder
     C                   WHEN      wk_PgmMode = '2'                             SWOG
     C                   EXSR      $SWOG
     C                   ENDSL

     C                   EVAL      out_RtnCode = wk_RtnCode

     C                   EVAL      *INLR = *ON
     C                   RETURN

     C/EJECT

      **************************************************************************
      *****               S  U  B  R  O  U  T  I  N  E  S                  *****
      **************************************************************************

      **************************************************************************
      * Process for a given order.                                             *
      **************************************************************************
     CSR   $ByOrder      BEGSR

     C                   EXSR      $Reset1
     C                   EVAL      *IN93 = *OFF

     C                   EXSR      $Reship

      /Free
       wk_Data = *Blank;
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSCWK a where a.S1ORD#=:wk_Order# and
                a.S1TURN=:wk_Turn#;
       IF wk_RcdCnt > *Zero;
         wk_Data = 'W';
       ELSE;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from HYPSSCC a where a.SCORD#=:wk_Order# and
                  a.SCTURN=:wk_Turn#;
         IF wk_RcdCnt > *Zero;
           wk_Data = 'M';
         ELSE;
           wk_RtnCode = '99';
           *IN12 = *ON;
         ENDIF;
       ENDIF;
      /End-Free

     C                   DOW       *IN12 = *OFF

      *
      * Display Subfile
      *
     C                   IF        wk_Sfl01RRN > *Zero
     C                   EVAL      *IN27 = *ON
     C                   ELSE
     C                   EVAL      *IN27 = *OFF
     C                   ENDIF                                                  wk_SFL01RRN>*Zero
     C                   EVAL      *IN26 = *OFF

     C                   WRITE     MSFCTL01
     C                   IF        not wk_LongTrk#
     C                   WRITE     SFLCMD01
     C                   EXFMT     SFLCTL01                             LR
     C                   ELSE
     C                   WRITE     SFLCMD01A
     C                   EXFMT     SFLCTL01A                            LR
     C                   ENDIF                                                  not wk_LongTrk#

     C                   EVAL      *IN27 = *OFF
     C                   EVAL      *IN93 = *ON
     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN42 = *OFF
     C                   EXSR      $RMVPM

      *
      * F12=Cancel
      *
     C                   IF        *IN12
      /Free
       EXEC SQL delete from HYW0500 where W1JOB#=:ps_Job#;
      /End-Free
     C                   EVAL      wk_RtnCode = '12'
     C                   LEAVE
     C                   ENDIF                                                  *IN12

      *
      * F5=Refresh
      *
     C                   IF        *IN05
     C                   EXSR      $Init1
     C                   ITER
     C                   ENDIF                                                  *IN05

      *
      * F14=Reset
      *
     C                   IF        *IN14
     C                   EXSR      $Reset1
     C                   EXSR      $Reship
     C                   IF        wk_Sfl01RRN = *Zero
     C                   EVAL      *IN93 = *OFF
     C                   ENDIF                                                  wk_Sfl01RRN=*Zero
     C                   ITER
     C                   ENDIF                                                  *IN14

      *
      * Validate entries.
      *
     C                   EXSR      $Edit1


      *
      * If any errors detected, display message to user.
      *
     C                   IF        wk_Error
     C                   ITER
     C                   ENDIF                                                  wk_Error

     C                   IF        wk_Sfl01RRN > *Zero
     C                   EXSR      $PrcSFL01
     C                   ENDIF                                                  wk_Sfl01RRN>*Zero

      *
      * Write work file record and write subfile record.
      *
     C                   IF        C1SSCC <> *Blanks  and
     C                              C1TRK# <> *Blanks
     C                   IF        wk_TimeStamp = *Loval
     C                   EVAL      wk_TimeStamp = %timestamp
     C                   ENDIF                                                  wk_TimeStamp=*Loval
     C                   IF        not wk_MPScanned
     C                   EVAL      W1JOB# = ps_Job#
     C                   EVAL      W1SSCC = C1SSCC
     C                   EVAL      W1TRK# = C1TRK#
     C                   EVAL      W1TSTP = wk_TimeStamp
     C                   WRITE     HYW0500R
     C                   EVAL      S1OPT = *Blank
     C                   EVAL      S1SSCC = C1SSCC
     C                   EVAL      S1TRK# = C1TRK#
     C                   IF        not wk_LongTrk#
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1
     C                   WRITE     SFL01
     C                   ELSE
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1
     C                   EVAL      wk_Sfl01ARRN = wk_Sfl01ARRN + 1
     C                   WRITE     SFL01A
     C                   ENDIF                                                  not wk_LongTrk#
     C                   ELSE
     C     wk_MP#        SETLL     HYPMPCD
     C                   DOU       %eof(HYPMPCD)
     C     wk_MP#        READE     HYPMPCD
     C                   IF        %eof(HYPMPCD)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPMPCD)
     C                   EVAL      wk_SSCC = '0000090255' +
     C                               %editc(MDSRL#:'X')
     C                   EVAL      wk_SSCCNbr =
     C                               %dec(%subst(wk_SSCC:1:19):19:0)
     C                   EVAL      wk_ChkDigit =
     C                               RtvM10ChkDigit(wk_SSCCNbr:19)
     C                   EVAL      %subst(wk_SSCC:20:1) = wk_ChkDigit
     C                   EVAL      W1JOB# = ps_Job#
     C                   EVAL      W1SSCC = wk_SSCC
     C                   EVAL      W1TRK# = C1TRK#
     C                   EVAL      W1TSTP = wk_TimeStamp
     C                   WRITE     HYW0500R
     C                   EVAL      S1OPT = *Blank
     C                   EVAL      S1SSCC = wk_SSCC
     C                   EVAL      S1TRK# = C1TRK#
     C                   IF        not wk_LongTrk#
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1
     C                   WRITE     SFL01
     C                   ELSE
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1
     C                   EVAL      wk_Sfl01ARRN = wk_Sfl01ARRN + 1
     C                   WRITE     SFL01A
     C                   ENDIF                                                  not wk_LongTrk#
     C                   ENDDO                                                  %eof(HYPMPCD)
     C                   ENDIF                                                  not wk_MPScanned
     C                   EVAL      C1SSCC = *Blanks
     C                   EVAL      C1TRK# = *Blanks
     C                   EVAL      C1TRK#A = *Blanks
     C                   EVAL      wk_MPScanned = *OFF
     C                   EVAL      wk_MP# = *Blanks
     C                   EVAL      sv_C1SSCC2 = *Blanks
     C                   ENDIF                                                  C1SSCC<>*Blanks and

      *
      * Determine whether all license plates have been associated
      * with tracking numbers.
      *
      /Free
       wk_RcdCnt = *Zero;
       SELECT;
       WHEN wk_Data = 'W';
         EXEC SQL select count(*) into :wk_RcdCnt
                  from HYPSCWK a where a.S1ORD#=:wk_Order# and
                  a.S1TURN=:wk_Turn# and
                  a.S1UOMQ>0 and
                  not exists (select * from HYW0500 b where
                   b.W1JOB#=:ps_Job# and
                   substr(b.W1SSCC,11,9)=digits(a.S1SRL#));
       WHEN wk_Data = 'M';
         EXEC SQL select count(*) into :wk_RcdCnt
                  from HYPSSCC a where a.SCORD#=:wk_Order# and
                  a.SCTURN=:wk_Turn# and
                  a.SCUOMQ>0 and
                  not exists (select * from HYW0500 b where
                   b.W1JOB#=:ps_Job# and
                   substr(b.W1SSCC,11,9)=digits(a.SCSRL#));
       ENDSL;
      /End-Free

      *
      * If F11 was not pressed, send message to user to press F11.
      *
     C                   IF        *IN11 = *OFF
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_MsgID = 'GUM0031'
     C                   EVAL      wk_MsgData = 'F11'
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   ITER
     C                   ELSE
     C                   IF        wk_RcdCnt > *Zero
     C                   EVAL      wk_MsgID = 'ERR1159'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   ITER
     C                   ENDIF                                                  wk_RcdCnt>*Zero
     C                   ENDIF                                                  *IN11=*OFF

      /Free
      //
      // Delete any prior associated records from HYPLPTK.  These records
      // were retrieved in $Reship.
      //
       SELECT;
       WHEN wk_Data = 'W';
         EXEC SQL delete from
                    HYPLPTK a where a.LTORD#=:wk_Order# and
                    exists (select * from HYPSCWK b where b.S1ORD#=a.LTORD# and
                     b.S1TURN=:wk_Turn# and
                     digits(b.S1SRL#)=substr(a.LTSSCC,11,9));
       WHEN wk_Data = 'M';
         EXEC SQL delete from
                    HYPLPTK a where a.LTORD#=:wk_Order# and
                    exists (select * from HYPSSCC b where b.SCORD#=a.LTORD# and
                     b.SCTURN=:wk_Turn# and
                     digits(b.SCSRL#)=substr(a.LTSSCC,11,9));
       ENDSL;

      //
      // Process the work file - write the HYPLPTK records.
      //
       EXEC SQL insert into HYPLPTK
                       (LTORD#, LTSSCC, LTTRK#, LTTSTP)
                select :wk_Order#, W1SSCC, W1TRK#, :wk_TimeStamp from
                  HYW0500 where W1JOB#=:ps_Job#;
      /End-Free
     C                   EXSR      $Reset1
     C                   EVAL      *IN93 = *OFF
     C                   LEAVE

     C                   ENDDO                                                  *IN12=*OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Process for a given customer - SWOG.                                   *
      *   SWOG mode is called from two places -                                *
      *   1) HYR0140 Combine Screen. Lbl data should be in HYPSSCC.            *
      *   2) HYR0614 BoxLogix/DigiPick shipping. Lbl data should be in HYPSCWK.*
      **************************************************************************
     CSR   $SWOG         BEGSR

      /Free
       wk_Data = *Blank;
       wk_RcdCnt1 = *Zero;
       wk_RcdCnt2 = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt1
                from OEORHP a
                where a.IHSHTO=:wk_Cust# and a.IHDSHP=:wk_DropShip# and
                exists (select * from HYPSCWK b where b.S1ORD#=a.IHORD# and
                b.S1TURN=a.IHTURN);
       EXEC SQL select count(*) into :wk_RcdCnt2
                from OEORHP a
                where a.IHSHTO=:wk_Cust# and a.IHDSHP=:wk_DropShip# and
                exists (select * from HYPSSCC b where b.SCORD#=a.IHORD# and
                b.SCTURN=a.IHTURN);
       SELECT;
       WHEN wk_Caller = 'HYR0614'  and  wk_RcdCnt1 > *Zero;
         wk_Data = 'W';
       WHEN wk_Caller = 'HYR0140'  and  wk_RcdCnt2 > *Zero;
         wk_Data = 'M';
       WHEN wk_RcdCnt1 > *Zero;
         wk_Data = 'W';
       WHEN wk_RcdCnt2 > *Zero;
         wk_Data = 'M';
       OTHER;
         wk_RtnCode = '99';
         *IN12 = *ON;
         LEAVESR;
       ENDSL;
      /End-Free

     C                   EXSR      $Reset2
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_rcdCnt
                  from HYPLPTK a join OEORHD b on a.LTORD#=b.OEORD#
                  where b.OESHTO=:wk_Cust# and b.OEDSHP=:wk_DropShip# and
                  exists (select * from HYPSSCC c where c.SCORD#=a.LTORD# and
                   digits(c.SCSRL#)=substr(a.LTSSCC,11,9) and
                   c.SCIVDT='0001-01-01');
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_AlwF9 = *OFF
     C                   ELSE
     C                   EVAL      wk_AlwF9 = *ON
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   EVAL      wk_RmvPrior = *OFF
     C                   EVAL      *IN93 = *OFF

      *
      * SWOG mode is only available to use the main UCC-128 files.  If we
      * change HYR0110 to call this program in SWOG mode (probably so that
      * this program processes the same regardless of from where it is
      * called), wk_Data needs to be set like it is early in $ByOrder. Also,
      * other changes will need to be made - compare $SWOG to $ByOrder
      * wk_Data usage.
      *  The above was dealt with on 9/16/24, due to now calling SWOG mode
      *  from the BoxLogix/DigiPick shipping program.
      *
     C****               EVAL      wk_Data = 'M'

     C                   DOW       *IN12 = *OFF

      *
      * Display Subfile
      *
     C                   IF        wk_Sfl02RRN > *Zero
     C                   EVAL      *IN27 = *ON
     C                   ELSE
     C                   EVAL      *IN27 = *OFF
     C                   ENDIF                                                  wk_SFL02RRN>*Zero
     C                   EVAL      *IN26 = *OFF

     C                   EVAL      *IN09 = wk_AlwF9

     C                   WRITE     MSFCTL01
     C                   IF        not wk_LongTrk#
     C                   WRITE     SFLCMD02
     C                   EXFMT     SFLCTL02                             LR
     C                   ELSE
     C                   WRITE     SFLCMD02A
     C                   EXFMT     SFLCTL02A                            LR
     C                   ENDIF                                                  not wk_LongTrk#

     C                   EVAL      *IN27 = *OFF
     C                   EVAL      *IN93 = *ON
     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EXSR      $RMVPM

      *
      * F12=Cancel
      *
     C                   IF        *IN12
      /Free
       EXEC SQL delete from HYW0500 where W1JOB#=:ps_Job#;
      /End-Free
     C                   EVAL      wk_RtnCode = '12'
     C                   LEAVE
     C                   ENDIF                                                  *IN12

      *
      * F5=Refresh
      *
     C                   IF        *IN05
     C                   EXSR      $Init2
     C                   ITER
     C                   ENDIF                                                  *IN05

      *
      * F9=Reship
      *
     C                   IF        *IN09
     C                   EXSR      $Reship2
     C                   EVAL      wk_AlwF9 = *OFF
     C                   EVAL      wk_RmvPrior = *ON
     C                   EVAL      *IN93 = *OFF
     C                   ITER
     C                   ENDIF                                                  *IN09

      *
      * F14=Reset
      *
     C                   IF        *IN14
     C                   EXSR      $Reset2
     C                   IF        wk_Sfl02RRN = *Zero
     C                   EVAL      *IN93 = *OFF
     C                   ENDIF                                                  wk_Sfl02RRN=*Zero
     C                   ITER
     C                   ENDIF                                                  *IN14

      *
      * Validate entries.
      *
     C                   EXSR      $Edit2


      *
      * If any errors detected, display message to user.
      *
     C                   IF        wk_Error
     C                   ITER
     C                   ENDIF                                                  wk_Error

     C                   IF        wk_Sfl02RRN > *Zero
     C                   EXSR      $PrcSFL02
     C                   IF        not wk_DltWk  and  C2SCAN = *Blanks  and
     C                               not *IN11
     C                   EVAL      wk_MsgID = 'ERR1365'
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ITER
     C                   ENDIF                                                  not wk_DltWk
     C                   ELSE
     C                   IF        C2SCAN = *Blanks
     C                   EVAL      wk_MsgID = 'ERR1365'
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ITER
     C                   ENDIF                                                  C2SCAN=*Blanks
     C                   ENDIF                                                  wk_Sfl02RRN>*Zero

      *
      * Write work file record and write subfile record.
      *
     C                   IF        C2SCAN <> *Blanks
     C                   SELECT
     C                   WHEN      wk_ScanType = 'L'
     C                   IF        wk_TimeStamp = *Loval
     C                   EVAL      wk_TimeStamp = %timestamp
     C                   ENDIF                                                  wk_TimeStamp=*Loval
     C                   IF        not wk_MPScanned
     C                   EVAL      W1JOB# = ps_Job#
     C                   EVAL      W1SSCC = C2SCAN
     C                   EVAL      W1TRK# = *Blanks
     C                   EVAL      W1TSTP = wk_TimeStamp
     C                   WRITE     HYW0500R
     C                   EVAL      S2OPT = *Blank
     C                   EVAL      S2SSCC = C2SCAN
     C                   EVAL      S2TRK# = *Blanks
     C                   IF        not wk_LongTrk#
     C                   EVAL      wk_Sfl02RRN = wk_Sfl02RRN + 1
     C                   WRITE     SFL02
     C                   ELSE
     C                   EVAL      wk_Sfl02RRN = wk_Sfl02RRN + 1
     C                   EVAL      wk_Sfl02ARRN = wk_Sfl02ARRN + 1
     C                   WRITE     SFL02A
     C                   ENDIF                                                  not wk_LongTrk#
     C                   ELSE
     C     wk_MP#        SETLL     HYPMPCD
     C                   DOU       %eof(HYPMPCD)
     C     wk_MP#        READE     HYPMPCD
     C                   IF        %eof(HYPMPCD)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPMPCD)
     C                   EVAL      wk_SSCC = '0000090255' +
     C                               %editc(MDSRL#:'X')
     C                   EVAL      wk_SSCCNbr =
     C                               %dec(%subst(wk_SSCC:1:19):19:0)
     C                   EVAL      wk_ChkDigit =
     C                               RtvM10ChkDigit(wk_SSCCNbr:19)
     C                   EVAL      %subst(wk_SSCC:20:1) = wk_ChkDigit
     C                   EVAL      W1JOB# = ps_Job#
     C                   EVAL      W1SSCC = wk_SSCC
     C                   EVAL      W1TRK# = *Blanks
     C                   EVAL      W1TSTP = wk_TimeStamp
     C                   WRITE     HYW0500R
     C                   EVAL      S2OPT = *Blank
     C                   EVAL      S2SSCC = wk_SSCC
     C                   EVAL      S2TRK# = *Blanks
     C                   IF        not wk_LongTrk#
     C                   EVAL      wk_Sfl02RRN = wk_Sfl02RRN + 1
     C                   WRITE     SFL02
     C                   ELSE
     C                   EVAL      wk_Sfl02RRN = wk_Sfl02RRN + 1
     C                   EVAL      wk_Sfl02ARRN = wk_Sfl02ARRN + 1
     C                   WRITE     SFL02A
     C                   ENDIF                                                  not wk_LongTrk#
     C                   ENDDO                                                  %eof(HYPMPCD)
     C                   ENDIF                                                  not wk_MPScanned
     C                   WHEN      wk_ScanType = 'T'
      /Free
       EXEC SQL update HYW0500
                 set W1TRK#=:C2SCAN
                where W1JOB#=:ps_Job# and W1TRK#=' ';
      /End-Free
     C                   EXSR      $BldSfl02
     C                   ENDSL
     C                   EVAL      C2SCAN = *Blanks
     C                   EVAL      C2SCANA = *Blanks
     C                   ITER
     C                   ENDIF                                                  C2SCAN<>*Blanks

      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYW0500
                where W1JOB#=:ps_Job# and W1SSCC<>' ' and W1TRK#<>' ';
      /End-Free

      *
      * If F11 was not pressed, send message to user to press F11.
      *
     C                   IF        not *IN11  and  wk_LPNeedTrk = *Zero  and
     C                               not wk_DltWk
     C                   IF        wk_RcdCnt > *Zero
     C                   EVAL      wk_MsgID = 'GUM0031'
     C                   EVAL      wk_MsgData = 'F11'
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   ENDIF                                                  wk_RcdCnt>*Zero
     C                   ITER
     C                   ENDIF                                                  not *IN11 and

      *
      * If F11 was not pressed (and not already handled), re-display prompt
      * screen (without any associated message).
      *
     C                   IF        not *IN11
     C                   ITER
     C                   ENDIF                                                  not *IN11

      *
      * If F11 was pressed, but there are scanned UCC-128 license plate
      * numbers that have not yet been associated with a tracking number,
      * send error message.
      *
     C                   IF        *IN11  and  wk_LPNeedTrk > *Zero
     C                   EVAL      wk_MsgID = 'ERR1371'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   ITER
     C                   ENDIF                                                  *IN11 and

      *
      * If F11 was pressed, but there have been no UCC-128 license plate
      * numbers scanned, send error message.
      *
     C                   IF        *IN11  and  wk_RcdCnt = *Zero
     C                   EVAL      wk_MsgID = 'ERR1372'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   ITER
     C                   ENDIF                                                  *IN11 and

      *
      * Determine whether all license plates have been associated
      * with tracking numbers.
      *
     C                   IF        *IN11

      /Free
       wk_RcdCnt = *Zero;
       FOR idx1 = 1 to maxidx1;
         IF ary_Ord#(idx1) = *Zero;
           ITER;
         ENDIF;                                                              // ary_Ord#(idx1)=*Zero
         wk_Order# = ary_Ord#(idx1);
         wk_Turn# = ary_Turn(idx1);
         SELECT;
         WHEN wk_Data = 'W';
           EXEC SQL select count(*) into :wk_RcdCnt
                    from HYPSCWK a where a.S1ORD#=:wk_Order# and
                    a.S1TURN=:wk_Turn# and
                    a.S1UOMQ>0 and
                    not exists (select * from HYW0500 b where
                     b.W1JOB#=:ps_Job# and
                     substr(b.W1SSCC,11,9)=digits(a.S1SRL#));
         WHEN wk_Data = 'M';
           EXEC SQL select count(*) into :wk_RcdCnt
                    from HYPSSCC a where a.SCORD#=:wk_Order# and
                    a.SCTURN=:wk_Turn# and
                    a.SCUOMQ>0 and
                    not exists (select * from HYW0500 b where
                     b.W1JOB#=:ps_Job# and
                     substr(b.W1SSCC,11,9)=digits(a.SCSRL#));
         ENDSL;
         IF wk_RcdCnt > *Zero;
           LEAVE;
         ENDIF;                                                              // wk_RcdCnt>*Zero
       ENDFOR;
      /End-Free

     C                   IF        wk_RcdCnt > *Zero
     C                   EVAL      wk_MsgID = 'ERR1159'
     C                   EVAL      wk_MsgData = 'SWOG'
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   ITER
     C                   ENDIF                                                  wk_RcdCnt>*Zero

     C                   ENDIF                                                  *IN11

      /Free
      //
      // Delete any prior associated records from HYPLPTK.  These records
      // were retrieved in $Reship2.
      //
       IF wk_RmvPrior;
         EXEC SQL delete from
                    HYPLPTK a where exists (select * from OEORHD b where
                    b.OEORD#=a.LTORD# and b.OESHTO=:wk_Cust# and
                    b.OEDSHP=:wk_DropShip#) and
                    exists (select * from HYPSSCC c where c.SCORD#=a.LTORD# and
                     digits(c.SCSRL#)=substr(a.LTSSCC,11,9) and
                     c.SCIVDT='0001-01-01');
         EXEC SQL delete from
                    HYPLPTK a where exists (select * from OEORHD b where
                    b.OEORD#=a.LTORD# and b.OESHTO=:wk_Cust# and
                    b.OEDSHP=:wk_DropShip#) and
                    exists (select * from HYPSCWK c where c.S1ORD#=a.LTORD# and
                     digits(c.S1SRL#)=substr(a.LTSSCC,11,9));
       ENDIF;                                                                // wk_RmvPrior

      //
      // Process the work file - write the HYPLPTK records.
      //
       EXEC SQL insert into HYPLPTK
                       (LTORD#, LTSSCC, LTTRK#, LTTSTP)
                select :wk_Order#, W1SSCC, W1TRK#, :wk_TimeStamp from
                  HYW0500 where W1JOB#=:ps_Job#;
      /End-Free
     C                   EXSR      $Reset2
     C                   EVAL      *IN93 = *OFF
     C                   LEAVE

     C                   ENDDO                                                  *IN12=*OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Send message to program message queue.                                 *
      **************************************************************************
     CSR   $SndPM        BEGSR

     C                   CALL      'GUCSNDPM'
     C                   PARM                    wk_MsgFile
     C                   PARM                    wk_MsgID
     C                   PARM                    wk_MsgData

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Remove messages from program message queue.                            *
      **************************************************************************
     CSR   $RmvPM        BEGSR

     C                   CALL      'GUCRMVPM'

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Initialize entry fields.                                               *
      **************************************************************************
     CSR   $Init1        BEGSR

     C                   EXSR      $BldSFL01

     C                   EVAL      C1SSCC = *Blanks
     C                   EVAL      C1TRK# = *Blanks
     C                   EVAL      C1TRK#A = *Blanks

     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN42 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Reset.                                                                 *
      **************************************************************************
     CSR   $Reset1       BEGSR

      *
      * Clear work file.
      *
      /Free
       EXEC SQL delete from HYW0500 where W1JOB#=:ps_Job#;
      /End-Free

      *
      * Clear subfile.
      *
     C                   IF        not wk_LongTrk#
     C                   EVAL      *IN27 = *OFF
     C                   EVAL      *IN26 = *ON
     C                   WRITE     SFLCTL01
     C                   EVAL      *IN26 = *OFF
     C                   EVAL      wk_Sfl01RRN = *Zero
     C                   ELSE
     C                   EVAL      *IN27 = *OFF
     C                   EVAL      *IN26 = *ON
     C                   WRITE     SFLCTL01A
     C                   EVAL      *IN26 = *OFF
     C                   EVAL      wk_Sfl01RRN = *Zero
     C                   EVAL      wk_Sfl01ARRN = *Zero
     C                   ENDIF                                                  not wk_LongTrk#

      *
      * Initialize screen fields.
      *
      /Free
       wk_Shto# = *Zero;
       wk_ShtoName = *Blanks;
       EXEC SQL select OESHTO into :wk_Shto#
                from OEORHD where OEORD#=:wk_Order#;
       EXEC SQL select CMCNA1 into :wk_ShtoName
                from HDCUST where CMCUST=:wk_Shto#;
      /End-Free
     C                   EVAL      C1ORD# = %triml(%editc(wk_Order#:'Z'))
     C                   EVAL      C1SHTO = %triml(%editc(wk_Shto#:'Z')) +
     C                               ' - ' + wk_ShtoName
     C                   EVAL      C1SSCC = *Blanks
     C                   EVAL      C1TRK# = *Blanks
     C                   EVAL      C1TRK#A = *Blanks
     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN42 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Validate entries.                                                      *
      **************************************************************************
     CSR   $Edit1        BEGSR

     C                   EVAL      wk_Error = *OFF

      *
      * Ensure UCC-128 license plate number is left-justified in the field.
      *
     C                   EVAL      C1SSCC = %triml(C1SSCC)

      *
      * If long tracking number is allowed, attempt to parse it.
      *
     C                   IF        wk_LongTrk#
     C                   EVAL      C1TRK# = *Blanks
     C                   EVAL      C1TRK#A = %triml(C1TRK#A)
     C                   IF        %len(%trimr(C1TRK#A)) <= 35
     C                   EVAL      C1TRK# = C1TRK#A
     C                   ELSE
     C                   EVAL      wk_Len = %scan(' ':C1TRK#A)
     C                   IF        wk_Len = *Zero  or wk_Len > 36
     C                   EVAL      C1TRK# = C1TRK#A
     C                   ELSE
     C                   EVAL      wk_Track# = %subst(C1TRK#A:1:wk_Len)
      /Free
       wk_RcdCnt = *Zero;
       wk_MDORDR = *Blanks;
       EXEC SQL select count(*), coalesce(max(MDORDR),' ')
                 into :wk_RcdCnt, :wk_MDORDR
                from MFD1MD where MDFBRC=:wk_Track#;
       IF wk_RcdCnt = *Zero;
         EXEC SQL select count(*), coalesce(max(MDORDR),' ')
                   into :wk_RcdCnt, :wk_MDORDR
                  from MFDHMD where MDFBRC=:wk_Track#;
       ENDIF;
      /End-Free
     C                   IF        wk_RcdCnt > *Zero
     C                   EVAL      C1TRK# = wk_Track#
     C                   ELSE
     C                   EVAL      wk_StrPos = %check(' ':C1TRK#A:wk_Len+1)
     C                   IF        wk_StrPos = *Zero
     C                   EVAL      C1TRK# = C1TRK#A
     C                   ELSE
     C                   EVAL      wk_EndPos = %scan(' ':C1TRK#A:wk_StrPos)
     C                   IF        wk_EndPos = *Zero
     C                   EVAL      wk_EndPos = 271
     C                   ENDIF                                                  wk_EndPos=*Zero
     C                   EVAL      wk_Len = wk_EndPos - wk_StrPos
     C                   EVAL      C1TRK# = %subst(C1TRK#A:wk_StrPos:wk_Len)
     C                   ENDIF                                                  wk_StrPos=*Zero
     C                   ENDIF                                                  wk_RcdCnt>*Zero
     C                   ENDIF                                                  wk_Len=*Zero or
     C                   ENDIF                                                  %len(%trimr(C1TRK#))
     C                   ENDIF                                                  wk_LongTrk#

      *
      * UCC-128 license plate number is required and must be valid.
      *
     C                   IF        C1SSCC = *Blanks
     C                   IF        C1TRK# <> *Blanks
     C                   EVAL      wk_MsgID = 'ERR0666'
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  C1TRK#<>*Blanks
     C                   ELSE

     C                   IF        C1SSCC <> sv_C1SSCC2
     C                   EVAL      wk_MPScanned = *OFF
     C                   EVAL      wk_MP# = *Blanks
     C                   EVAL      sv_C1SSCC = *Blanks
     C                   IF        %len(%trim(C1SSCC)) = 12  and                Shipping master
     C                                %subst(C1SSCC:1:3) = 'HMP'                  pack??
     C                   EVAL      wk_MP# = %trim(C1SSCC)
     C     wk_MP#        CHAIN     HYPMPCD
     C                   IF        %found(HYPMPCD)
     C                   EVAL      wk_SSCC = '0000090255' +
     C                               %editc(MDSRL#:'X')
     C                   EVAL      wk_SSCCNbr =
     C                               %dec(%subst(wk_SSCC:1:19):19:0)
     C                   EVAL      wk_ChkDigit =
     C                               RtvM10ChkDigit(wk_SSCCNbr:19)
     C                   EVAL      %subst(wk_SSCC:20:1) = wk_ChkDigit
     C                   EVAL      sv_C1SSCC = C1SSCC
     C                   EVAL      C1SSCC = wk_SSCC
     C                   EVAL      sv_C1SSCC2 = C1SSCC
     C                   EVAL      wk_Serial# = MDSRL#
     C                   EVAL      wk_MPScanned = *ON
     C                   ENDIF                                                  %found(HYPMPCD)
     C                   ENDIF                                                  %len(%trim(C1SSCC))=
     C                   ENDIF                                                  C1SSCC<>sv_C1SSCC2

     C                   IF        %subst(C1SSCC:21:1) <> *Blank
     C                   EVAL      wk_MsgID = 'ERR0667'
     C                   EVAL      wk_MsgData = C1SSCC
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
      /Free
       IF %check(digits:%subst(C1SSCC:11:9)) = *Zero;
         wk_Serial# = %dec(%subst(C1SSCC:11:9):9:0);
       ELSE;
         wk_Serial# = *Zero;
       ENDIF;                                                                 // %check(digits:%subs
       wk_RcdCnt = *Zero;
       wk_SCORD# = *Zero;
       wk_SCTURN = *Zero;
       SELECT;
       WHEN wk_Data = 'W';
         EXEC SQL select count(*), max(S1ORD#), max(S1TURN)
                    into :wk_RcdCnt, :wk_SCORD#, :wk_SCTURN
                  from HYPSCWK where S1SRL#=:wk_Serial#;
       WHEN wk_Data = 'M';
         EXEC SQL select count(*), max(SCORD#), max(SCTURN)
                    into :wk_RcdCnt, :wk_SCORD#, :wk_SCTURN
                  from HYPSSCC where SCSRL#=:wk_Serial#;
       ENDSL;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_MsgID = 'ERR0667'
     C                   EVAL      wk_MsgData = C1SSCC
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
     C                   IF        wk_SCORD# <> wk_Order#
     C                   EVAL      wk_MsgID = 'ERR0668'
     C                   EVAL      wk_MsgData = C1SSCC
     C                   EVAL      %subst(wk_MsgData:21:8) =
     C                               %triml(%editc(wk_Order#:'Z'))
     C                   EVAL      %subst(wk_MsgData:29:8) =
     C                               %triml(%editc(wk_SCORD#:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
     C                   IF        wk_SCTURN <> wk_Turn#
     C                   EVAL      wk_MsgID = 'ERR1158'
     C                   EVAL      wk_MsgData = C1SSCC
     C                   EVAL      %subst(wk_MsgData:21:9) =
     C                               %triml(%editc(wk_Turn#:'Z'))
     C                   EVAL      %subst(wk_MsgData:30:9) =
     C                               %triml(%editc(wk_SCTURN:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
      /Free
       wk_RcdCnt=*Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYW0500
                where W1JOB#=:ps_Job# and W1SSCC=:C1SSCC;
      /End-Free
     C                   IF        wk_RcdCnt > *Zero
     C                   EVAL      wk_MsgID = 'ERR1160'
     C                   EVAL      wk_MsgData = C1SSCC
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_RcdCnt>*Zero
     C                   ENDIF                                                  wk_SCTURN<>wk_Turn#
     C                   ENDIF                                                  wk_SCORD#<>wk_Order#
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   ENDIF                                                  %subst(C1SSCC:21:1)
     C                   IF        wk_Error  and  wk_MPScanned
     C                   EVAL      C1SSCC = sv_C1SSCC
     C                   ENDIF                                                  wk_Error
     C                   ENDIF                                                  C1SSCC=*Blanks

      *
      * Tracking number is required and must be valid.
      *
     C                   IF        C1TRK# = *Blanks
     C                   IF        C1SSCC <> *Blanks
     C                   EVAL      wk_MsgID = 'ERR1144'
     C                   EXSR      $SndPM
     C                   EVAL      *IN42 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  C1SSCC<>*Blanks
     C                   ELSE
      /Free
       wk_RcdCnt = *Zero;
       wk_MDORDR = *Blanks;
       EXEC SQL select count(*), coalesce(max(MDORDR),' ')
                 into :wk_RcdCnt, :wk_MDORDR
                from MFD1MD where MDFBRC=:C1TRK#;
       IF wk_RcdCnt = *Zero;
         EXEC SQL select count(*), coalesce(max(MDORDR),' ')
                   into :wk_RcdCnt, :wk_MDORDR
                  from MFDHMD where MDFBRC=:C1TRK#;
       ENDIF;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_MsgID = 'ERR1145'
     C                   EVAL      wk_MsgData = C1TRK#
     C                   EXSR      $SndPM
     C                   EVAL      *IN42 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
     C                   IF        %len(%trimr(C1TRK#)) = 34                    Assume FedEx
     C                   EVAL      C1TRK# = %subst(C1TRK#:23:12)
     C                   ENDIF                                                  %len(%trimr(C1TRK#))
     C****               IF        wk_MDORDR <> %char(wk_Order#)
     C                   EVAL      wk_HypPos = %scan('-':wk_MDORDR)
     C                   IF        (wk_HypPos = *Zero  and
     C                               wk_MDORDR <> %char(wk_Order#))  or
     C                             (wk_HypPos = 1)  or
     C                             (wk_HypPos > 1  and
     C                               %subst(wk_MDORDR:1:wk_HypPos-1) <>
     C                                 %char(wk_Order#))
     C                   EVAL      wk_MsgID = 'ERR1157'
     C                   EVAL      wk_MsgData = C1TRK#
     C                   EVAL      %subst(wk_MsgData:31:8) =
     C                               %triml(%editc(wk_Order#:'Z'))
     C                   EVAL      %subst(wk_MsgData:39:10) = wk_MDORDR
     C                   EXSR      $SndPM
     C                   EVAL      *IN42 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
      /Free
       wk_RcdCnt=*Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYW0500
                where W1JOB#=:ps_Job# and W1TRK#=:C1TRK#;
      /End-Free
     C                   IF        wk_RcdCnt > *Zero
     C                   IF        not wk_Error
     C                   DOU       *IN11  or  *IN12
     C                   EXFMT     WINDOW01
     C                   ENDDO                                                  *IN11 or *IN12
     C                   ELSE
     C                   EVAL      *IN12 = *ON
     C                   ENDIF                                                  not wk_Error
     C                   IF        *IN12
     C                   EVAL      wk_MsgID = 'ERR1161'
     C                   EVAL      wk_MsgData = C1TRK#
     C                   EXSR      $SndPM
     C                   EVAL      *IN42 = *ON
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN12 = *OFF
     C                   ELSE
     C                   EVAL      *IN11 = *OFF
     C                   ENDIF                                                  *IN12
     C                   ENDIF                                                  wk_RcdCnt>*Zero
     C                   ENDIF                                                  wk_MDORDR<>%char(wk_
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   ENDIF                                                  C1TRK#=*Blanks

      *
      * If errors were detected, sound alarm.
      *
     C                   IF        wk_Error
     C                   EVAL      *IN40 = *ON
     C                   ENDIF                                                  wk_Error

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Build subfile from work file.                                          *
      **************************************************************************
     CSR   $BldSFL01     BEGSR

      *
      * Clear subfile.
      *
     C                   IF        not wk_LongTrk#
     C                   EVAL      *IN27 = *OFF
     C                   EVAL      *IN26 = *ON
     C                   WRITE     SFLCTL01
     C                   EVAL      *IN26 = *OFF
     C                   EVAL      wk_Sfl01RRN = *Zero
     C                   ELSE
     C                   EVAL      *IN27 = *OFF
     C                   EVAL      *IN26 = *ON
     C                   WRITE     SFLCTL01A
     C                   EVAL      *IN26 = *OFF
     C                   EVAL      wk_Sfl01RRN = *Zero
     C                   EVAL      wk_Sfl01ARRN = *Zero
     C                   ENDIF                                                  not wk_LongTrk#

     C     ps_Job#       SETLL     HYW0500
     C                   DOU       %eof(HYW0500)
     C     ps_Job#       READE     HYW0500
     C                   IF        %eof(HYW0500)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYW0500)
     C                   EVAL      S1OPT = *Blank
     C                   EVAL      S1SSCC = W1SSCC
     C                   EVAL      S1TRK# = W1TRK#
     C                   IF        not wk_LongTrk#
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1
     C                   WRITE     SFL01
     C                   ELSE
     C                   EVAL      wk_Sfl01ARRN = wk_Sfl01ARRN + 1
     C                   WRITE     SFL01A
     C                   ENDIF                                                  not wk_LongTrk#
     C                   ENDDO                                                  %eof(HYW0500)

     C                   IF        wk_LongTrk#
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01ARRN
     C                   ENDIF                                                  wk_LongTrk#

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Read subfile records and process deletes.                              *
      **************************************************************************
     CSR   $PrcSFL01     BEGSR

     C                   EVAL      wk_DltWk = *OFF

     C                   DOU       %eof(HYD0500)

     C                   IF        not wk_LongTrk#
     C                   READC     SFL01
     C                   ELSE
     C                   READC     SFL01A
     C                   ENDIF                                                  not wk_LongTrk#

     C                   IF        %eof(HYD0500)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYD0500)

     C                   IF        S1OPT = *Blank
     C                   ITER
     C                   ENDIF                                                  S1OPT=*Blank

     C                   IF        S1OPT = 'D'
     C                   EVAL      wk_Serial# = %dec(%subst(S1SSCC:11:9):9:0)
      /Free
       wk_MDMP# = *Blanks;
       EXEC SQL select coalesce(max(MDMP#),' ') into :wk_MDMP#
                from HYPMPCD
                where MDSRL#=:wk_Serial#;
      /End-Free
     C                   IF        wk_MDMP# = *Blanks
      /Free
       EXEC SQL delete from HYW0500 where W1JOB#=:ps_Job# and
                  W1SSCC=:S1SSCC and W1TRK#=:S1TRK#;
      /End-Free
     C                   EVAL      wk_MsgID = 'MSG0439'
     C                   EVAL      wk_MsgData = S1SSCC
     C                   EXSR      $SndPM
     C                   EVAL      wk_DltWk = *ON
     C                   ITER
     C                   ELSE
     C     wk_MDMP#      SETLL     HYPMPCD
     C                   DOU       %eof(HYPMPCD)
     C     wk_MDMP#      READE     HYPMPCD
     C                   IF        %eof(HYPMPCD)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPMPCD)
     C                   EVAL      wk_SSCC = '0000090255' +
     C                               %editc(MDSRL#:'X')
     C                   EVAL      wk_SSCCNbr =
     C                               %dec(%subst(wk_SSCC:1:19):19:0)
     C                   EVAL      wk_ChkDigit =
     C                               RtvM10ChkDigit(wk_SSCCNbr:19)
     C                   EVAL      %subst(wk_SSCC:20:1) = wk_ChkDigit
      /Free
       EXEC SQL delete from HYW0500 where W1JOB#=:ps_Job# and
                  W1SSCC=:wk_SSCC and W1TRK#=:S1TRK#;
      /End-Free
     C                   EVAL      wk_DltWk = *ON
     C                   ENDDO                                                  %eof(HYPMPCD)
     C                   EVAL      wk_MsgID = 'MSG0440'
     C                   EVAL      wk_MsgData = wk_MDMP#
     C                   EXSR      $SndPM
     C                   ITER
     C                   ENDIF                                                  wk_MDMP#=*Blanks
     C                   ENDIF

     C                   EVAL      *IN28 = *ON
     C                   EVAL      *IN61 = *ON
     C                   IF        not wk_LongTrk#
     C                   UPDATE    SFL01
     C                   ELSE
     C                   UPDATE    SFL01A
     C                   ENDIF                                                  not wk_LongTrk#

     C                   EVAL      wk_MsgID = 'GUM0001'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM

     C                   ENDDO                                                  %eof(HYD0500)

     C                   EVAL      *IN28 = *OFF
     C                   EVAL      *IN61 = *OFF

     C                   IF        wk_DltWk
     C                   EXSR      $BldSFL01
     C                   IF        wk_Sfl01RRN = *Zero
     C                   EVAL      *IN93 = *OFF
     C                   ENDIF                                                  wk_Sfl01RRN=*Zero
     C                   ENDIF                                                  wk_DltWk

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Check for re-ship, and if found, load work file.                       *
      **************************************************************************
     CSR   $Reship       BEGSR

      /Free
       SELECT;
       WHEN wk_Data = 'W';
         EXEC SQL insert into HYW0500
                         (W1JOB#, W1SSCC, W1TRK#, W1TSTP)
                  select :ps_Job#, LTSSCC, LTTRK#, LTTSTP from
                    HYPLPTK a where a.LTORD#=:wk_Order# and
                    exists (select * from HYPSCWK b where b.S1ORD#=a.LTORD# and
                     b.S1TURN=:wk_Turn# and
                     digits(b.S1SRL#)=substr(a.LTSSCC,11,9));
       WHEN wk_Data = 'M';
         EXEC SQL insert into HYW0500
                         (W1JOB#, W1SSCC, W1TRK#, W1TSTP)
                  select :ps_Job#, LTSSCC, LTTRK#, LTTSTP from
                    HYPLPTK a where a.LTORD#=:wk_Order# and
                    exists (select * from HYPSSCC b where b.SCORD#=a.LTORD# and
                     b.SCTURN=:wk_Turn# and
                     digits(b.SCSRL#)=substr(a.LTSSCC,11,9));
       ENDSL;
      /End-Free

     C                   EXSR      $BldSFL01

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Initialize entry fields (for SWOG).                                    *
      **************************************************************************
     CSR   $Init2        BEGSR

     C                   EXSR      $BldSFL02

     C                   EVAL      C2SCAN = *Blanks
     C                   EVAL      C2SCANA = *Blanks

     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN41 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Reset (for SWOG).                                                      *
      **************************************************************************
     CSR   $Reset2       BEGSR

      *
      * Clear work file.
      *
      /Free
       EXEC SQL delete from HYW0500 where W1JOB#=:ps_Job#;
      /End-Free

      *
      * Clear subfile.
      *
     C                   IF        not wk_LongTrk#
     C                   EVAL      *IN27 = *OFF
     C                   EVAL      *IN26 = *ON
     C                   WRITE     SFLCTL02
     C                   EVAL      *IN26 = *OFF
     C                   EVAL      wk_Sfl02RRN = *Zero
     C                   ELSE
     C                   EVAL      *IN27 = *OFF
     C                   EVAL      *IN26 = *ON
     C                   WRITE     SFLCTL02A
     C                   EVAL      *IN26 = *OFF
     C                   EVAL      wk_Sfl02RRN = *Zero
     C                   EVAL      wk_Sfl02ARRN = *Zero
     C                   ENDIF                                                  not wk_LongTrk#

      *
      * Initialize screen fields.
      *
      /Free
       wk_Shto# = wk_Cust#;
       wk_ShtoName = *Blanks;
       IF wk_DropShip# = *Zero;
         EXEC SQL select CMCNA1 into :wk_ShtoName
                  from HDCUST where CMCUST=:wk_Shto#;
       ELSE;
         EXEC SQL select DSNAME into :wk_ShtoName
                  from HDDSHP where DSVCF='C' and
                   DSVNCS=:wk_Shto# and DSNMBR=:wk_DropShip#;
       ENDIF;                                                                // wk_DropShip#=*Zero
      /End-Free
     C                   EVAL      C2SHTO = %triml(%editc(wk_Shto#:'Z')) +
     C                               ' - ' + wk_ShtoName
     C                   EVAL      C2SCAN = *Blanks
     C                   EVAL      C2SCANA = *Blanks
     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN41 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Validate entries (for SWOG).                                           *
      **************************************************************************
     CSR   $Edit2        BEGSR

     C                   EVAL      wk_Error = *OFF

      /Free
       wk_LPNeedTrk = *Zero;
       EXEC SQL select count(*) into :wk_LPNeedTrk
                from HYW0500
                where W1JOB#=:ps_Job# and W1TRK#=' ';
      /End-Free

      *
      * Ensure scanned license plate number or tracking number is
      * left-justified in the field.
      *
     C                   IF        not wk_LongTrk#
     C                   EVAL      C2SCAN = %triml(C2SCAN)
     C                   ELSE
     C                   EVAL      C2SCANA = %triml(C2SCANA)
     C                   EVAL      C2SCAN = C2SCANA
     C                   ENDIF                                                  wk_LongTrk#

      *
      * If scan fields is blank, leave subroutine.
      *
     C                   IF        C2SCAN = *Blanks
     C                   LEAVESR
     C                   ENDIF                                                  C2SCAN=*Blanks

      *
      * Determine scan type.  L=UCC-128 license plate number  T=Tracking number
      *
     C                   EVAL      wk_Serial# = *Zero
     C                   EVAL      wk_ScanType = 'T'
     C                   IF        %len(%trimr(C2SCAN)) = 20  and
     C                               %subst(C2SCAN:1:10) = '0000090255'
     C                   EVAL      wk_Serial# = %dec(%subst(C2SCAN:11:9):9:0)
     C                   EVAL      wk_ScanType = 'L'
     C                   ENDIF                                                  %len(%trimr(C2SCAN))
     C                   IF        %len(%trimr(C2SCAN)) <= 9  and
     C                                %check(digits:%trimr(C2SCAN)) = *Zero
     C                   EVAL      wk_Serial# = %dec(%trimr(C2SCAN):9:0)
      /Free
       wk_RcdCnt = *Zero;
       SELECT;
       WHEN wk_Data = 'M';
       EXEC SQL select count(*)
                  into :wk_RcdCnt
                from HYPSSCC where SCSRL#=:wk_Serial#;
       WHEN wk_Data = 'W';
       EXEC SQL select count(*)
                  into :wk_RcdCnt
                from HYPSCWK where S1SRL#=:wk_Serial#;
       ENDSL;
      /End-Free
     C                   IF        wk_Rcdcnt > *Zero
     C****               EVAL      C2SCAN = '0000090255' +
     C****                           %editc(wk_Serial#:'X') + '-'
     C                   EVAL      wk_SSCC = '0000090255' +
     C                               %editc(wk_Serial#:'X')
     C                   EVAL      wk_SSCCNbr =
     C                               %dec(%subst(wk_SSCC:1:19):19:0)
     C                   EVAL      wk_ChkDigit =
     C                               RtvM10ChkDigit(wk_SSCCNbr:19)
     C                   EVAL      %subst(wk_SSCC:20:1) = wk_ChkDigit
     C                   EVAL      C2SCAN = wk_SSCC
     C                   EVAL      wk_ScanType = 'L'
     C                   ELSE
     C                   EVAL      wk_Serial# = *Zero
     C                   ENDIF                                                  wk_RcdCnt>*Zero
     C                   ENDIF                                                  %len(%trimr(C2SCAN))
     C                   EVAL      wk_MPScanned = *OFF
     C                   EVAL      wk_MP# = *Blanks
     C                   EVAL      sv_C2SCAN = *Blanks
     C                   IF        %len(%trim(C2SCAN)) = 12  and                Shipping master
     C                                %subst(C2SCAN:1:3) = 'HMP'                  pack??
     C                   EVAL      wk_MP# = %trim(C2SCAN)
     C     wk_MP#        CHAIN     HYPMPCD
     C                   IF        %found(HYPMPCD)
     C                   EVAL      wk_SSCC = '0000090255' +
     C                               %editc(MDSRL#:'X')
     C                   EVAL      wk_SSCCNbr =
     C                               %dec(%subst(wk_SSCC:1:19):19:0)
     C                   EVAL      wk_ChkDigit =
     C                               RtvM10ChkDigit(wk_SSCCNbr:19)
     C                   EVAL      %subst(wk_SSCC:20:1) = wk_ChkDigit
     C                   EVAL      sv_C2SCAN = C2SCAN
     C                   EVAL      C2SCAN = wk_SSCC
     C                   EVAL      wk_Serial# = MDSRL#
     C                   EVAL      wk_MPScanned = *ON
     C                   EVAL      wk_ScanType = 'L'
     C                   ENDIF                                                  %found(HYPMPCD)
     C                   ENDIF                                                  %len(%trim(C2SCAN))=

      *
      * UCC-128 license plate number must be valid.
      *
     C                   IF        wk_ScanType = 'L'
     C                   IF        wk_LongTrk#  and  not wk_MPScanned
     C                   EVAL      C2SCAN = C2SCANA
     C                   ENDIF                                                  wk_LongTrk#
      /Free
         wk_RcdCnt = *Zero;
         wk_SCORD# = *Zero;
         wk_OESHTO = *Zero;
         wk_OEDSHP = *Zero;
       SELECT;
       WHEN wk_Data = 'M';
         EXEC SQL select count(*), max(SCORD#)
                    into :wk_RcdCnt, :wk_SCORD#
                  from HYPSSCC where SCSRL#=:wk_Serial#;
       WHEN wk_Data = 'W';
         EXEC SQL select count(*), max(S1ORD#)
                    into :wk_RcdCnt, :wk_SCORD#
                  from HYPSCWK where S1SRL#=:wk_Serial#;
       ENDSL;
         EXEC SQL select OESHTO, OEDSHP
                    into :wk_OESHTO, :wk_OEDSHP
                  from OEORHD where OEORD#=:wk_SCORD#;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_MsgID = 'ERR0667'
     C                   EVAL      wk_MsgData = C2SCAN
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
     C                   IF        wk_OESHTO <> wk_Cust#
     C                   EVAL      wk_MsgID = 'ERR1366'
     C                   EVAL      wk_MsgData = C2SCAN
     C                   EVAL      %subst(wk_MsgData:21:7) =
     C                               %triml(%editc(wk_Cust#:'Z'))
     C                   EVAL      %subst(wk_MsgData:28:7) =
     C                               %triml(%editc(wk_OESHTO:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
     C                   IF        wk_OEDSHP <> wk_DropShip#
     C                   EVAL      wk_MsgID = 'ERR1367'
     C                   EVAL      wk_MsgData = C2SCAN
     C                   IF        wk_DropShip# = *Zero
     C                   EVAL      %subst(wk_MsgData:21:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:21:7) =
     C                               %triml(%editc(wk_DropShip#:'Z'))
     C                   ENDIF                                                  wk_DropShip#=*Zero
     C                   IF        wk_OEDSHP = *Zero
     C                   EVAL      %subst(wk_MsgData:28:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:28:7) =
     C                               %triml(%editc(wk_OEDSHP:'Z'))
     C                   ENDIF                                                  wk_OEDSHP=*Zero
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
      /Free
       wk_RcdCnt=*Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYW0500
                where W1JOB#=:ps_Job# and W1SSCC=:C2SCAN;
      /End-Free
     C                   IF        wk_RcdCnt > *Zero
     C                   EVAL      wk_MsgID = 'ERR1160'
     C                   EVAL      wk_MsgData = C2SCAN
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_RcdCnt>*Zero
     C                   ENDIF                                                  wk_OEDSHP<>wk_DropSh
     C                   ENDIF                                                  wk_OESHTO<>wk_Cust#
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   IF        wk_Error  and  wk_MPScanned
     C                   EVAL      C2SCAN = sv_C2SCAN
     C                   ENDIF                                                  wk_Error  and
     C                   ENDIF                                                  wk_ScanType='L'

      *
      * Tracking number must be valid.
      *
     C                   IF        wk_ScanType = 'T'

      *
      * If long tracking number is allowed, attempt to parse it.
      *
     C                   IF        wk_LongTrk#
     C                   EVAL      C2SCAN = *Blanks
     C                   EVAL      C2SCANA = %triml(C2SCANA)
     C                   IF        %len(%trimr(C2SCANA)) <= 35
     C                   EVAL      C2SCAN = C2SCANA
     C                   ELSE
     C                   EVAL      wk_Len = %scan(' ':C2SCANA)
     C                   IF        wk_Len = *Zero  or wk_Len > 36
     C                   EVAL      C2SCAN = C2SCANA
     C                   ELSE
     C                   EVAL      wk_Track# = %subst(C2SCANA:1:wk_Len)
      /Free
       wk_RcdCnt = *Zero;
       wk_MDORDR = *Blanks;
       EXEC SQL select count(*), coalesce(max(MDORDR),' ')
                 into :wk_RcdCnt, :wk_MDORDR
                from MFD1MD where MDFBRC=:wk_Track#;
       IF wk_RcdCnt = *Zero;
         EXEC SQL select count(*), coalesce(max(MDORDR),' ')
                   into :wk_RcdCnt, :wk_MDORDR
                  from MFDHMD where MDFBRC=:wk_Track#;
       ENDIF;
      /End-Free
     C                   IF        wk_RcdCnt > *Zero
     C                   EVAL      C2SCAN = wk_Track#
     C                   ELSE
     C                   EVAL      wk_StrPos = %check(' ':C2SCANA:wk_Len+1)
     C                   IF        wk_StrPos = *Zero
     C                   EVAL      C2SCAN = C2SCANA
     C                   ELSE
     C                   EVAL      wk_EndPos = %scan(' ':C2SCANA:wk_StrPos)
     C                   IF        wk_EndPos = *Zero
     C                   EVAL      wk_EndPos = 271
     C                   ENDIF                                                  wk_EndPos=*Zero
     C                   EVAL      wk_Len = wk_EndPos - wk_StrPos
     C                   EVAL      C2SCAN = %subst(C2SCANA:wk_StrPos:wk_Len)
     C                   ENDIF                                                  wk_StrPos=*Zero
     C                   ENDIF                                                  wk_RcdCnt>*Zero
     C                   ENDIF                                                  wk_Len=*Zero or
     C                   ENDIF                                                  %len(%trimr(C2SCAN))
     C                   ENDIF                                                  wk_LongTrk#

      /Free
       wk_RcdCnt = *Zero;
       wk_MDORDR = *Blanks;
       EXEC SQL select count(*), coalesce(max(MDORDR),' ')
                 into :wk_RcdCnt, :wk_MDORDR
                from MFD1MD where MDFBRC=:C2SCAN;
       IF wk_RcdCnt = *Zero;
         EXEC SQL select count(*), coalesce(max(MDORDR),' ')
                   into :wk_RcdCnt, :wk_MDORDR
                  from MFDHMD where MDFBRC=:C2SCAN;
       ENDIF;
       EVAL wk_MDORDR = %triml(wk_MDORDR);
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_MsgID = 'ERR1145'
     C                   EVAL      wk_MsgData = C2SCAN
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
     C                   IF        %len(%trimr(C2SCAN)) = 34                    Assume FedEx
     C                   EVAL      C2SCAN = %subst(C2SCAN:23:12)
     C                   ENDIF                                                  %len(%trimr(C2SCAN))
     C                   EVAL      wk_HypPos = %scan('-':wk_MDORDR)
     C                   IF        wk_HypPos = *Zero  and
     C                               %len(%trimr(wk_MDORDR)) <= 8  and
     C                               %check(digits:%trimr(wk_MDORDR)) = *Zero
     C                   EVAL      wk_Order# = %dec(%trimr(wk_MDORDR):8:0)
     C                   ELSE
     C****               IF        wk_HypPos <= 8
     C****               EVAL      wk_Order# = %dec(%trimr(wk_MDORDR):8:0)
     C                   IF        wk_HypPos > 1  and  wk_HypPos <= 8
     C                   EVAL      wk_Order# = %dec(%trimr(
     C                               %subst(wk_MDORDR:1:wk_HypPos-1)):8:0)
     C                   ELSE
     C                   EVAL      wk_Order# = *Zero
     C****               ENDIF                                                  wk_HypPos<=8
     C                   ENDIF                                                  wk_HypPos>1 and
     C                   ENDIF                                                  wk_HypPos=*Zero and
     C                   IF        wk_Order# <> *Zero
      /Free
       wk_OESHTO = *Zero;
       wk_OEDSHP = *Zero;
       EXEC SQL select OESHTO, OEDSHP
                 into :wk_OESHTO, :wk_OEDSHP
                from OEORHD
                where OEORD#=:wk_Order#;
      /End-Free
     C                   ELSE
     C                   EVAL      wk_OESHTO = *Zero
     C                   EVAL      wk_OEDSHP = *Zero
     C                   ENDIF                                                  wk_Order#<>*Zero
     C                   IF        wk_OESHTO <> wk_Cust#
     C                   EVAL      wk_MsgID = 'ERR1368'
     C                   EVAL      wk_MsgData = C2SCAN
     C                   EVAL      %subst(wk_MsgData:31:7) =
     C                               %triml(%editc(wk_Cust#:'Z'))
     C                   EVAL      %subst(wk_MsgData:38:7) =
     C                               %triml(%editc(wk_OESHTO:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
     C                   IF        wk_OEDSHP <> wk_DropShip#
     C                   EVAL      wk_MsgID = 'ERR1369'
     C                   EVAL      wk_MsgData = C2SCAN
     C                   EVAL      %subst(wk_MsgData:31:7) =
     C                               %triml(%editc(wk_DropShip#:'Z'))
     C                   EVAL      %subst(wk_MsgData:38:7) =
     C                               %triml(%editc(wk_OEDSHP:'Z'))
     C                   EVAL      %subst(wk_MsgData:45:7) =
     C                               %triml(%editc(wk_Cust#:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
      /Free
       wk_RcdCnt=*Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYW0500
                where W1JOB#=:ps_Job# and W1TRK#=:C2SCAN;
      /End-Free
     C                   IF        wk_RcdCnt > *Zero
     C                   IF        not wk_Error
     C                   EVAL      W1TRK# = C2SCAN
     C                   DOU       *IN11  or  *IN12
     C                   EXFMT     WINDOW01
     C                   ENDDO                                                  *IN11 or *IN12
     C                   ELSE
     C                   EVAL      *IN12 = *ON
     C                   ENDIF                                                  not wk_Error
     C                   IF        *IN12
     C                   EVAL      wk_MsgID = 'ERR1161'
     C                   EVAL      wk_MsgData = C2SCAN
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN12 = *OFF
     C                   ELSE
     C                   EVAL      *IN11 = *OFF
     C                   ENDIF                                                  *IN12
     C                   ENDIF                                                  wk_RcdCnt>*Zero
     C                   ENDIF                                                  wk_OEDSHP<>wk_DropSh
     C                   ENDIF                                                  wk_OESHTO<>wk_Cust#
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   IF        not wk_Error
     C                   IF        wk_LPNeedTrk = *Zero
     C                   EVAL      wk_MsgID = 'ERR1370'
     C                   EVAL      wk_MsgData = C2SCAN
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_LPNeedTrk=*Zero
     C                   ENDIF                                                  not wk_Error
     C                   ENDIF                                                  wk_ScanType='T'

      *
      * If errors were detected, sound alarm.
      *
     C                   IF        wk_Error
     C                   EVAL      *IN40 = *ON
     C                   ENDIF                                                  wk_Error

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Build subfile from work file (for SWOG).                               *
      **************************************************************************
     CSR   $BldSFL02     BEGSR

      *
      * Clear subfile.
      *
     C                   IF        not wk_LongTrk#
     C                   EVAL      *IN27 = *OFF
     C                   EVAL      *IN26 = *ON
     C                   WRITE     SFLCTL02
     C                   EVAL      *IN26 = *OFF
     C                   EVAL      wk_Sfl02RRN = *Zero
     C                   ELSE
     C                   EVAL      *IN27 = *OFF
     C                   EVAL      *IN26 = *ON
     C                   WRITE     SFLCTL02A
     C                   EVAL      *IN26 = *OFF
     C                   EVAL      wk_Sfl02RRN = *Zero
     C                   EVAL      wk_Sfl02ARRN = *Zero
     C                   ENDIF                                                  not wk_LongTrk#

     C     ps_Job#       SETLL     HYW0500
     C                   DOU       %eof(HYW0500)
     C     ps_Job#       READE     HYW0500
     C                   IF        %eof(HYW0500)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYW0500)
     C                   EVAL      S2OPT = *Blank
     C                   EVAL      S2SSCC = W1SSCC
     C                   EVAL      S2TRK# = W1TRK#
     C                   IF        not wk_LongTrk#
     C                   EVAL      wk_Sfl02RRN = wk_Sfl02RRN + 1
     C                   WRITE     SFL02
     C                   ELSE
     C                   EVAL      wk_Sfl02ARRN = wk_Sfl02ARRN + 1
     C                   WRITE     SFL02A
     C                   ENDIF                                                  not wk_LongTrk#
     C                   ENDDO                                                  %eof(HYW0500)

     C                   IF        wk_LongTrk#
     C                   EVAL      wk_Sfl02RRN = wk_Sfl02ARRN
     C                   ENDIF                                                  wk_LongTrk#

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Read subfile records and process deletes (for SWOG).                   *
      **************************************************************************
     CSR   $PrcSFL02     BEGSR

     C                   EVAL      wk_DltWk = *OFF

     C                   DOU       %eof(HYD0500)

     C                   IF        not wk_LongTrk#
     C                   READC     SFL02
     C                   ELSE
     C                   READC     SFL02A
     C                   ENDIF                                                  not wk_LongTrk#

     C                   IF        %eof(HYD0500)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYD0500)

     C                   IF        S2OPT = *Blank
     C                   ITER
     C                   ENDIF                                                  S2OPT=*Blank

     C                   IF        S2OPT = 'D'
     C                   EVAL      wk_Serial# = %dec(%subst(S2SSCC:11:9):9:0)
      /Free
       wk_MDMP# = *Blanks;
       EXEC SQL select coalesce(max(MDMP#),' ') into :wk_MDMP#
                from HYPMPCD
                where MDSRL#=:wk_Serial#;
      /End-Free
     C                   IF        wk_MDMP# = *Blanks
      /Free
       EXEC SQL delete from HYW0500 where W1JOB#=:ps_Job# and
                  W1SSCC=:S2SSCC and W1TRK#=:S2TRK#;
      /End-Free
     C                   EVAL      wk_MsgID = 'MSG0439'
     C                   EVAL      wk_MsgData = S2SSCC
     C                   EXSR      $SndPM
     C                   EVAL      wk_DltWk = *ON
     C                   ITER
     C                   ELSE
     C     wk_MDMP#      SETLL     HYPMPCD
     C                   DOU       %eof(HYPMPCD)
     C     wk_MDMP#      READE     HYPMPCD
     C                   IF        %eof(HYPMPCD)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPMPCD)
     C                   EVAL      wk_SSCC = '0000090255' +
     C                               %editc(MDSRL#:'X')
     C                   EVAL      wk_SSCCNbr =
     C                               %dec(%subst(wk_SSCC:1:19):19:0)
     C                   EVAL      wk_ChkDigit =
     C                               RtvM10ChkDigit(wk_SSCCNbr:19)
     C                   EVAL      %subst(wk_SSCC:20:1) = wk_ChkDigit
      /Free
       EXEC SQL delete from HYW0500 where W1JOB#=:ps_Job# and
                  W1SSCC=:wk_SSCC and W1TRK#=:S2TRK#;
      /End-Free
     C                   EVAL      wk_DltWk = *ON
     C                   ENDDO                                                  %eof(HYPMPCD)
     C                   EVAL      wk_MsgID = 'MSG0440'
     C                   EVAL      wk_MsgData = wk_MDMP#
     C                   EXSR      $SndPM
     C                   ITER
     C                   ENDIF                                                  wk_MDMP#=*Blanks
     C                   ENDIF

     C                   EVAL      *IN28 = *ON
     C                   EVAL      *IN61 = *ON
     C                   IF        not wk_LongTrk#
     C                   UPDATE    SFL02
     C                   ELSE
     C                   UPDATE    SFL02A
     C                   ENDIF                                                  wk_LongTrk#

     C                   EVAL      wk_MsgID = 'GUM0001'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM

     C                   ENDDO                                                  %eof(HYD0500)

     C                   EVAL      *IN28 = *OFF
     C                   EVAL      *IN61 = *OFF

     C                   IF        wk_DltWk
     C                   EXSR      $BldSFL02
     C                   IF        wk_Sfl02RRN = *Zero
     C                   EVAL      *IN93 = *OFF
     C                   ENDIF                                                  wk_Sfl02RRN=*Zero
     C                   ENDIF                                                  wk_DltWk

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Check for re-ship, and if found, load work file (for SWOG).            *
      **************************************************************************
     CSR   $Reship2      BEGSR

      /Free
       SELECT;
       WHEN wk_Data = 'M';
       EXEC SQL insert into HYW0500
                       (W1JOB#, W1SSCC, W1TRK#, W1TSTP)
                select :ps_Job#, LTSSCC, LTTRK#, LTTSTP from
                  HYPLPTK a join OEORHD b on a.LTORD#=b.OEORD#
                  where b.OESHTO=:wk_Cust# and b.OEDSHP=:wk_DropShip# and
                  exists (select * from HYPSSCC c where c.SCORD#=a.LTORD# and
                   digits(c.SCSRL#)=substr(a.LTSSCC,11,9) and
                   c.SCIVDT='0001-01-01');
       WHEN wk_Data = 'W';
       EXEC SQL insert into HYW0500
                       (W1JOB#, W1SSCC, W1TRK#, W1TSTP)
                select :ps_Job#, LTSSCC, LTTRK#, LTTSTP from
                  HYPLPTK a join OEORHD b on a.LTORD#=b.OEORD#
                  where b.OESHTO=:wk_Cust# and b.OEDSHP=:wk_DropShip# and
                  exists (select * from HYPSCWK c where c.S1ORD#=a.LTORD# and
                   digits(c.S1SRL#)=substr(a.LTSSCC,11,9));
       ENDSL;
      /End-Free

     C                   EXSR      $BldSFL02

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Program initialization.                                                *
      **************************************************************************
     CSR   *INZSR        BEGSR

      *
      * Initialize work fields.
      *
     C                   EVAL      S@PGMQ = '*'

      *
      * Remove any program messages resulting from initialization.
      *
     C                   EXSR      $RmvPM

     C                   EVAL      P@PGNM = ps_Pgnm
     C                   EVAL      P@JOBN = ps_JobN

     CSR                 ENDSR

     C/EJECT
