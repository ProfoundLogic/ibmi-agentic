     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W9606R - REPORT - Loss Control Tracking Report
     H*****************************************************************
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     W9606C - OVERRIDE                                         *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*     NONE                                                      *
     H*                                                               *
     H*  04/19/99        PROGRAMMER C:61       PROJECT 102AK          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 09/29/09 C:62  PROJECT OP135 67535                            *
     H*                Change out Client table with Generic.          *
     H*                                                               *
     H* 10/17/01 C:61  PROJECT 102BZ                                  *
     H*                Add parm for secondary sort.                   *
     H*                                                               *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  71       Print Inception Date                                *
     H*  72       Print Cancel Date                                   *
     H*  73       Print Visit Date                                    *
     H*  74       Print Summary line                                  *
     H*  75       Secondary Sort (Visit date / Employer Number)       *
     H*  89       *ALL Loss reps                                      *
     H*  91       Read main File                                      *
     H*  92       Last record processing                              *
     H*  93       Next Employer / Visit Date                          *
     H*  XX - XX  XXX............................XXX                  *
     H*                                                               *
     H*****************************************************************
      *
      * System Company
     FSMCOL     IF   E           K DISK
      *
      * System Output Routing
     FSMORL     IF   E           K DISK
      *
      * User Parameter
     FWMUPL     IF   E           K DISK
      *
      * Loss Control Task Work File
     FWXVSL     IF   E           K DISK
      ***** BEG ADD ******* 10/17/01 **********************************
     FWXVSL1    IF   E           K DISK
     F                                     RENAME(PVS01:PVS02)
      ***** END ADD ******* 10/17/01 **********************************
      *
      * Employer Master
     FWMEML     IF   E           K DISK
      *
      * Employer Detail
     FWDELL     IF   E           K DISK
      *
      * Fund Master File
     FWMFNL     IF   E           K DISK
      *
      *  System User Parm
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
      *
      *
     F*** Begin Chg *** 09/29/09 **************************************
     F**********WTVLL   IF  E           K        DISK
     FWTGTL     IF   E           K DISK
     F*** End   Chg *** 09/29/09 **************************************
      *
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
      *
      *
      *  OCCURANCE OF TYP OF VISIT TOTALS
      *
      *
      *  OCCURANCE OF HRS TOTALS
      *
      *
      *  TYPE VISIT CODES - COINCIDE WITH OCUR'S OF TOVT IN TOVDS
      *

     D TV              S              2    DIM(200)                             TOV CODES
     D TX              S              2    DIM(200)                             SORTED TOV CODES
      *
      *  TYPE OF VISIT TOTALS
      *
     D TOVDS           DS                  OCCURS(200)
     D  TOVT                   1     11  0
     D                                     DIM(1)                               TYP VISIT TOTALS
      *
      *  HOURS TOTALS
      *
     D THRDS           DS                  OCCURS(200)
     D  THRT                   1     13  2
     D                                     DIM(1)                               HRS TOTALS
      *
      *
      *
     D XTITL           C                   CONST('*** LOSS CONTROL TR-
     D                                     ACKING REPORT ***')
      *
      *
      *
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *   User Parameter
      *

     C     UPKY1         KLIST
     C                   KFLD                    Q1USER
      *
      *   Output Routing
      *
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKPRT             1
      *
      *   Company
      *
     C     COKY          KLIST
     C                   KFLD                    KKCO#
      *
      *   Loss Control Task Work File
      *
     C     VSKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      *
      *   Employer Master
      *
     C     EMKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    VSEMP#
     C                   KFLD                    VSDIV
      *
      *   Employer Detail
      *
     C     ELKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    VSEMP#
     C                   KFLD                    VSDIV
      *
      *   Fund
      *
     C     FNKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      *
      *  System user key
      *
     C     SMUPKY        KLIST
     C                   KFLD                    KKSTAF
      *
      *
      *
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      KLIST
     C**********           KFLD           KKCO#
     C**********           KFLD           KKTBID
     C**********           KFLD           KKKEY
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*** End   Chg *** 09/29/09 **************************************
      *
      *   *LIKE Define Variables
      *
     C     *LIKE         DEFINE    VSCO#         KKCO#
     C     *LIKE         DEFINE    VSCO#         KKFND
     C     *LIKE         DEFINE    UPUSNM        X1USNM
     C     *LIKE         DEFINE    UPSTAF        KKSTAF
      *
     C     *LIKE         DEFINE    VSEMP#        SVEMP#
     C     *LIKE         DEFINE    VSDIV         SVDIV
      *
      ***** BEG ADD ******* 10/17/01 **********************************
     C     *LIKE         DEFINE    VSVSDT        SVVSDT
      ***** END ADD ******* 10/17/01 **********************************
      *
     C     *LIKE         DEFINE    VSSTF         SVSTAF
     C     *LIKE         DEFINE    UPUSNM        SVUSNM
      *
     C     *LIKE         DEFINE    FNNAME        X1FNNM
     C     *LIKE         DEFINE    EMEFD1        XXEFDT
     C     *LIKE         DEFINE    EMNAME        X1EMNM
     C     *LIKE         DEFINE    EMCAD1        XXCNDT
     C     *LIKE         DEFINE    ELANPR        X1ANPR
     C*** Begin Chg *** 09/29/09 **************************************
     C********** *LIKE     DEFN VLTBID    KKTBID
     C********** *LIKE     DEFN VLKEY     KKKEY
     C********** *LIKE     DEFN VLDESC    XXTPDS
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    GTDESC        XXTPDS
     C*** End   Chg *** 09/29/09 **************************************
      *
      *
      *   ENTRY PARAMETERS
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FRDT            8
     C                   PARM                    @@TODT            8
     C                   PARM                    @@STAF            3
      ***** BEG ADD ******* 10/17/01 **********************************
     C                   PARM                    @@VE              1
      ***** END ADD ******* 10/17/01 **********************************
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *
      *   Initialize
      *
     C                   TIME                    DSDTTM
     C                   MOVE      *ALL'_'       LINE            132
     C                   MOVE      *ON           MEETPR            1            PRINTED
      *
      *  PGMSTR executes first
      *
     C                   EXSR      PGMSTR
      *
      *   Print Routing Page
      *
     C                   EXSR      SRRTE
      *
      *  Read 1st record
      *
     C                   EXSR      READ1
      *
      *  Initialize break criteria
      *
     C                   EXSR      IZL1BK
     C                   EXSR      IZL2BK
      *
     C     *IN91         DOWEQ     '0'
      *
      *  If subset criteria met
      *
     C                   MOVE      *ON           MEETSS            1
     C                   EXSR      SUBSET
      *
     C     MEETSS        IFEQ      *ON
      *
      *  Test for break in report
      *
     C                   MOVE      *ON           MEETL1            1
     C                   MOVE      *ON           MEETL2            1
     C                   EXSR      L1TST
     C                   EXSR      L2TST
      *
      *  L1 Break
      *
     C     MEETL1        IFEQ      *OFF
     C     MEETL2        OREQ      *OFF
      *
     C     L1CNT         IFNE      *ZERO
     C                   EXSR      L1BK
     C                   ENDIF                                                  L1CNT
      *
     C                   EXSR      IZL1BK
     C                   ENDIF                                                  MEETL1
      *
      *  L2 Break
      *
     C     MEETL2        IFEQ      *OFF
      *
     C     L2CNT         IFNE      *ZERO
     C                   EXSR      L2BK
     C                   ENDIF                                                  L2CNT
      *
     C                   EXSR      IZL2BK
     C                   ENDIF                                                  MEETL1
      *
      *  Write detail record
      *
     C                   EXSR      SRDET
     C                   MOVE      *OFF          MEETPR                         PRINTED
      *
      *  Accumulate Totals
      *
     C                   ADD       1             L1CNT             7 0
     C                   ADD       1             L2CNT             7 0
     C                   ADD       1             LRCNT             9 0
     C                   EXSR      ACCTOT
      *
     C                   ENDIF                                                  MEETSS
      *
      *  Read next record
      *
     C                   EXSR      READN
      *
     C                   ENDDO                                                  WRK RECORD
      *
      *  Last Record processing
      *
     C     MEETPR        IFEQ      *ON
     C     LRCNT         OREQ      *ZERO
     C                   SETON                                        OA92
     C                   EXCEPT    HEAD
     C                   EXCEPT    NOREC
      *
     C                   ELSE
      *
     C     L1CNT         IFNE      *ZERO
     C                   EXSR      L1BK
     C                   ENDIF
      *
     C     L2CNT         IFNE      *ZERO
     C                   EXSR      L2BK
     C                   ENDIF
      *
     C                   EXSR      LRBK
      *
     C                   ENDIF                                                  MEETPR
      *
      *  End of Processing
      *
     C     END           TAG
     C                   MOVE      *ON           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       kkPrt = '0';
       Chain orKy POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
      *
      *
      *****************************************************************
      *
      *
      *
      *****************************************************************
      * PGMSTR - Set-up and Initialize
      *****************************************************************
      *
     CSR   PGMSTR        BEGSR
      *
      *  Entry parm processing
      *
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FRDT        X1FRDT            8 0
     C                   MOVE      @@TODT        X1TODT            8 0
      ***** BEG ADD ******* 10/17/01 **********************************
     C     @@VE          COMP      'V'                                    75
      ***** END ADD ******* 10/17/01 **********************************
      *
      * Get User Name, if passed in
      *
     C     @@STAF        IFNE      *BLANKS
     C                   MOVEL(P)  @@STAF        KKSTAF
     C     SMUPKY        CHAIN     PUP02                              60
     C  N60              MOVEL(P)  UPUSNM        X1USNM
     C   60              MOVEL(P)  *BLANKS       X1USNM
      *
     C                   ELSE
      *
     C                   SETON                                        89
     C                   ENDIF                                                  @@STAF
      *
      * Get Fund Name
      *
     C     FNKY1         CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        X1FNNM
     C   60              MOVEL(P)  *BLANKS       X1FNNM
      *
      *  INITIALIZE TYPE VISIT RELATED FIELDS
      *
     C                   MOVE      *BLANKS       TV
     C                   DO        200           X                 3 0
     C     X             OCCUR     TOVDS
     C     X             OCCUR     THRDS
     C                   MOVE      *ZEROS        TOVT
     C                   MOVE      *ZEROS        THRT
     C                   END
      *
      *  FIRST RECORD OF NEXT EMPLOYER OR VISIT DATE
      *
     C                   SETOFF                                       93
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      *****************************************************************
      * READ1 - Read first record from file
      *****************************************************************
      *
     CSR   READ1         BEGSR
      *
      ***** BEG ADD ******* 10/17/01 **********************************
     C     @@VE          IFEQ      'E'
      ***** END ADD ******* 10/17/01 **********************************
     C     VSKY1         CHAIN     PVS01                              91
      ***** BEG ADD ******* 10/17/01 **********************************
      *
     C                   ELSE
      *
     C     VSKY1         CHAIN     PVS02                              91
     C                   ENDIF                                                  @@VE
      ***** END ADD ******* 10/17/01 **********************************
      *
     C                   ENDSR                                                  READ1
      *
      *
      *
      *****************************************************************
      * READN - Read next record from file
      *****************************************************************
      *
     CSR   READN         BEGSR
      *
      ***** BEG ADD ******* 10/17/01 **********************************
     C     @@VE          IFEQ      'E'
      ***** END ADD ******* 10/17/01 **********************************
     C                   CLEAR                   PVS01
     C     VSKY1         READE     PVS01                                  91
      ***** BEG ADD ******* 10/17/01 **********************************
      *
     C                   ELSE
      *
     C                   CLEAR                   PVS02
     C     VSKY1         READE     PVS02                                  91
     C                   ENDIF                                                  @@VE
      ***** END ADD ******* 10/17/01 **********************************
      *
     C                   ENDSR                                                  READN
      *
      *
      *
      *****************************************************************
      * SUBSET - Check if member of records processed subset
      *****************************************************************
     CSR   SUBSET        BEGSR
      *
      *  MEETSS = *OFF, if not to be processed
      *****************************************************************
      *
      *
     C                   ENDSR                                                  SUBSET
      *
      *
      *
      *****************************************************************
      * L1TST - First Level Break TEST criteria
      *****************************************************************
      *
     CSR   L1TST         BEGSR
      *
      *  MEETL1 = *OFF if L1BK processing to proceed
      *****************************************************************
      ***** BEG ADD ******* 10/17/01 **********************************
      *
     C     @@VE          IFEQ      'E'
      ***** END ADD ******* 10/17/01 **********************************
      *
      *  Employer / Division change
      *
     C     VSEMP#        IFNE      SVEMP#
     C     VSDIV         ORNE      SVDIV
     C                   MOVE      *OFF          MEETL1
     C                   ENDIF
      ***** BEG ADD ******* 10/17/01 **********************************
      *
     C                   ELSE
      *
      *  Visit Date change
      *
     C     VSVSDT        IFNE      SVVSDT
     C                   MOVE      *OFF          MEETL1
     C                   ENDIF
      *
     C                   ENDIF                                                  @@VE
      ***** END ADD ******* 10/17/01 **********************************
      *
     C                   ENDSR                                                  L1TST
      *
      *
      *
      *****************************************************************
      * L1BK  - First Level Break action
      *****************************************************************
      *
     CSR   L1BK          BEGSR
      *
      *  Output L1 totals
      *
      *  No Totals for Employers generated
      *
      *
      *  First Record of Next employer OR Visit Date
      *
     C                   SETOFF                                       93
      *
     C                   ENDSR                                                  L1BK
      *
      *
      *
      *****************************************************************
      * IZL1BK - Initialize Level 1 break criteria
      *****************************************************************
      *
     CSR   IZL1BK        BEGSR
      *
      *  Employer / Division
      *
      ***** BEG ADD ******* 10/17/01 **********************************
     C     @@VE          IFEQ      'E'
      ***** END ADD ******* 10/17/01 **********************************
     C                   MOVE      VSEMP#        SVEMP#
     C                   MOVE      VSDIV         SVDIV
      ***** BEG ADD ******* 10/17/01 **********************************
      *
     C                   ELSE
      *
      *  Visit Date
      *
     C                   Z-ADD     VSVSDT        SVVSDT
     C                   ENDIF                                                  @@VE
      ***** END ADD ******* 10/17/01 **********************************
      *
     C                   ENDSR                                                  IZL1BK
      *
      *
      *
      *****************************************************************
      * L2TST - Second Level Break TEST criteria
      *****************************************************************
      *
     CSR   L2TST         BEGSR
      *
      *  MEETL2 = *OFF if L2BK processing to proceed
      *****************************************************************
      *
      *  Loss Control Rep
      *
     C     VSSTF         IFNE      SVSTAF
     C                   MOVE      *OFF          MEETL2
     C                   ENDIF
      *
     C                   ENDSR                                                  L2TST
      *
      *
      *
      *****************************************************************
      * L2BK  - Second Level Break Action
      *****************************************************************
      *
     CSR   L2BK          BEGSR
      *
      *  Output L2 totals
      *
      *  Loss Rep Totals
      *
     C                   EXCEPT    L2TOT
      *
      *  PAGE BREAK AFTER
      *
     C                   SETON                                        OA
      *
      *  Re initialize L2 totals = 0, if 'B' not used in output spec
      *
     C                   ENDSR                                                  L2BK
      *
      *
      *
      *****************************************************************
      * IZL2BK - Initialize Level 2 break criteria
      *****************************************************************
      *
     CSR   IZL2BK        BEGSR
      *
      *  Loss Control Representative
      *
     C                   MOVEL(P)  VSSTF         SVSTAF
     C                   MOVEL(P)  VSSTF         KKSTAF
     C     SMUPKY        CHAIN     PUP02                              60
     C  N60              MOVEL(P)  UPUSNM        SVUSNM
     C   60              MOVEL(P)  *BLANKS       SVUSNM
      *
     C                   ENDSR                                                  IZL2BK
      *
      *
      *
      *****************************************************************
      * SRDET - Write Detail Record
      *****************************************************************
      *
     CSR   SRDET         BEGSR
      *
     C                   SETOFF                                       717273
      *
     C                   Z-ADD     *ZERO         X1INDT            8 0
     C                   Z-ADD     *ZERO         X1CNDT            8 0
     C                   Z-ADD     *ZERO         X1ANPR
     C                   Z-ADD     *ZERO         X1VSDT            8 0
     C                   MOVEL(P)  *BLANKS       X1EMNM
      *
      * Employer Name
      *
     C     EMKY1         CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
      *
      * Employer Name
      *
     C                   MOVEL(P)  EMNAME        X1EMNM
      *
      * Inception Date
      *
     C                   SELECT
      *
     C     EMEFD5        WHENNE    *ZERO
     C                   Z-ADD     5             X                 3 0
     C                   MOVE      EMEFD5        XXEFDT
      *
     C     EMEFD4        WHENNE    *ZERO
     C                   Z-ADD     4             X                 3 0
     C                   MOVE      EMEFD4        XXEFDT
      *
     C     EMEFD3        WHENNE    *ZERO
     C                   Z-ADD     3             X                 3 0
     C                   MOVE      EMEFD3        XXEFDT
      *
     C     EMEFD2        WHENNE    *ZERO
     C                   Z-ADD     2             X                 3 0
     C                   MOVE      EMEFD2        XXEFDT
      *
     C     EMEFD1        WHENNE    *ZERO
     C                   Z-ADD     1             X                 3 0
     C                   MOVE      EMEFD1        XXEFDT
      *
     C                   ENDSL
      *
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1INDT            8 0
      *
     C     SRERR         IFEQ      *OFF
     C     XXEFDT        ANDNE     *ZERO
     C                   SETOFF                                       71
     C                   ELSE
     C                   SETON                                        71
     C                   ENDIF                                                  X1INDT
      *
      * Cancel Date
      *
     C                   SELECT
      *
     C     X             WHENEQ    1
     C     EMCAD1        ANDNE     *ZERO
     C                   MOVE      EMCAD1        XXCNDT
      *
     C     X             WHENEQ    2
     C     EMCAD2        ANDNE     *ZERO
     C                   MOVE      EMCAD2        XXCNDT
      *
     C     X             WHENEQ    3
     C     EMCAD3        ANDNE     *ZERO
     C                   MOVE      EMCAD3        XXCNDT
      *
     C     X             WHENEQ    4
     C     EMCAD4        ANDNE     *ZERO
     C                   MOVE      EMCAD4        XXCNDT
      *
     C     X             WHENEQ    5
     C     EMCAD5        ANDNE     *ZERO
     C                   MOVE      EMCAD5        XXCNDT
      *
     C                   OTHER
     C                   MOVE      *ZERO         XXCNDT
      *
     C                   ENDSL
      *
     C                   Z-ADD     XXCNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1CNDT            8 0
      *
     C     SRERR         IFEQ      *OFF
     C     XXCNDT        ANDNE     *ZERO
     C                   SETOFF                                       72
     C                   ELSE
     C                   SETON                                        72
     C                   ENDIF                                                  X1CNDT
      *
     C                   ENDIF                                                  *IN60
      *
      *  Annual Premium
      *
     C     ELKY1         SETGT     PEL01
     C     ELKY1         READPE    PEL01                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAAPR        X1ANPR
      *
     C                   ELSE
      *
     C                   Z-ADD     ELANPR        X1ANPR
     C                   ENDIF                                                  ELFAF
      *
     C                   ENDIF                                                  *IN60
      *
      * Visit Date
      *
     C                   Z-ADD     VSVSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1VSDT            8 0
      *
     C     SRERR         IFEQ      *OFF
     C     X1VSDT        ANDNE     *ZERO
     C                   SETOFF                                       73
     C                   ELSE
     C                   SETON                                        73
     C                   ENDIF                                                  X1VSDT
      *
      *  Detail record print
      *
      *    NOTE: any exception/elimination criteria should be in SUBSET
      *
      ***** BEG ADD ******* 10/17/01 **********************************
      *
     C     @@VE          IFEQ      'E'
      ***** END ADD ******* 10/17/01 **********************************
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DETAIL
      ***** BEG ADD ******* 10/17/01 **********************************
     C   OA              SETOFF                                       OA
      *
     C                   ELSE
      *
     C   OA              EXCEPT    HEAD1
     C   OA              SETOFF                                       OA
     C                   EXCEPT    DET1
     C                   ENDIF                                                  @@VE
      ***** END ADD ******* 10/17/01 **********************************
      *
      *  Accumulate Type of Visit summary values
      *
      *
      *                    *** Get Occurance Number for Type Code
     C                   EXSR      SROCNM
      *
     C                   ADD       1             TOVT(1)
     C                   ADD       VSHRS         THRT(1)
      *
     C                   ENDSR                                                  SRDET
      *
      *
      *
      *****************************************************************
      * ACCTOT - Accumulate Totals
      *****************************************************************
      *
     CSR   ACCTOT        BEGSR
      *
      *  Accumulate L1 Totals (Employer)
      *
      *
      *  Accumulate L2 Totals (Loss Rep)
      *
     C  N93              ADD       X1ANPR        L2ANPR           13 2          ONCE EACH EMPL
     C                   ADD       VSHRS         L2HRS             7 2
      *
      *  Accumulate LR Totals (Group)
      *
     C  N93              ADD       X1ANPR        LRANPR           13 2          ONCE EACH EMPL
     C                   ADD       VSHRS         LRHRS             7 2
      *
      *  Following records of employer to print
      *
     C                   SETON                                        93
      *
     C                   ENDSR                                                  ACCTOT
      *
      *
      *
      *****************************************************************
      * LRBK - Last Record Processing
      *****************************************************************
      *
     CSR   LRBK          BEGSR
      *
     C                   SETON                                        92
     C                   EXCEPT    HEAD
     C                   EXCEPT    LRTOT
      *
      * Print summary by Type of Visit
      *
     C                   SETOFF                                       74
     C                   EXCEPT    LSHEAD
     C                   EXSR      LSBK
      *
     C                   ENDSR                                                  LRBK
      *
      *
      *
      *****************************************************************
      * SRRTE - Print routing page
      *****************************************************************
      *
     CSR   SRRTE         BEGSR
      *
      *   GET COMPANY, USER, INFO.
      *
     C     COKY          CHAIN     PCO01                              60
     C     UPKY1         CHAIN     PUP01                              60
      *
      *   Output Routing - detail report 0
      *
     C                   MOVE      '0'           KKPRT
     C     ORKY          CHAIN     POR01                              60
      *
     C     *IN60         IFEQ      '0'
      *
      * Left Justify Title in Field
      *
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF
     C                   ENDIF
      *
      *  User Name if no Distribution specified in routing
      *
     C     ORDIS1        IFEQ      *BLANKS
     C     ORDIS2        ANDEQ     *BLANKS
     C     ORDIS3        ANDEQ     *BLANKS
     C     ORDIS4        ANDEQ     *BLANKS
     C                   MOVEL(P)  UPUSNM        ORDIS1
     C                   ENDIF
      *
      ***** Beg Del ***** 08/26/14 *************************************************
     C************       EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *************************************************
      *
     C                   ELSE
      *
     C                   MOVEL(P)  XTITL         ORTITL
      *
     C                   ENDIF
      *
      *  Center Title in Field
      *
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        ORTITL
      *
      * Center Company name in display field
      *
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        CONAME
      *
     C                   SETON                                        OA
      *
     C                   ENDSR                                                  SRRTE
      *
      *
      *
      *****************************************************************
      * SRONNM - GET NUMBER OF OCCURANCE OF DATA STRUCTURES
      *****************************************************************
     C     SROCNM        BEGSR
      *
      * DETERMINE OCCURANCE NUMBER OF TOVT (TYPE VISIT TOTALS)
      *
     C                   MOVEL     *BLANKS       XXBLNK            2
     C                   MOVEL     VSVSTP        XXTOV             2
     C                   Z-ADD     1             I                 9 0
     C     XXTOV         LOOKUP    TV(I)                                  60
      *
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             I
     C     XXBLNK        LOOKUP    TV(I)                                  61
      *
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     200           I
      *
     C                   ELSE
      *
     C                   MOVEL     XXTOV         TV(I)
     C                   END
      *
     C                   END
      *
     C     I             OCCUR     TOVDS
     C     I             OCCUR     THRDS
      *
     C                   ENDSR                                                  SROCNM
      *
      *
      *
      *****************************************************************
      * LSBK   -  Print Totals by Type Code
      *================================================================
      *
     C     LSBK          BEGSR
      *
      * CHECK EACH OCCURANCE FOR OUTPUT
      *
      *                    *** OUTPUT TOV CODES SORTED
     C                   MOVE      TV            TX
     C                   SORTA     TX
      *
      *                    *** FOR EACH TOVT
     C                   Z-ADD     1             J
     C     J             DOUGT     200
      *
      *                    *** IF NOT BLANK ENTRY
     C     TX(J)         IFNE      *BLANKS
      *
      *                    *** FIND INDEX FOR TOTALS
     C                   MOVEL     TX(J)         XXXLK             2
     C                   Z-ADD     1             I
     C     XXXLK         LOOKUP    TV(I)                                  60
     C     I             OCCUR     TOVDS
     C     I             OCCUR     THRDS
      *
      *                    *** IF HRS OR TOVT NOT ZERO
     C     THRT(1)       IFNE      *ZERO
     C     TOVT(1)       ORNE      *ZERO
      *
      *                    *** GET DESCRIPTION FOR TOV REPORT OUTPUT
     C                   MOVEL     'LTRK'        KKTBID
     C                   MOVEL     XXXLK         KKKEY
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      CHAINPVL01                60
     C**N60*****           MOVELVLDESC    XXTPDS
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        XXTPDS
     C*** End   Chg *** 09/29/09 **************************************
     C   60              MOVEL     *BLANKS       XXTPDS
      *
      *                    *** OUTPUT TOV CODE TOTALS TO REPORT
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    LSTOT
     C                   SETON                                        74
     C                   ENDIF
     C                   ENDIF
      *
     C                   ADD       1             J                 9 0
      *
     C                   ENDDO
      *
      *
     C                   ENDSR                                                  LSBK
      *
      *
      *
      *
      *****************************************************************
      *
      *
      *
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
      **********
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              56
      **********
     O**********E            ROUTE          1
     O**********             ORDIS2              56
      **********
     O**********E            ROUTE          1
     O**********             ORDIS3              56
      *
     O**********E            ROUTE          1
     O**********             ORDIS4              56
      ***** End Del ***** 08/26/14 *************************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              56
      **********
     O          E            ROUTE          1
     O                       ORDIS2              56
      **********
     O          E            ROUTE          1
     O                       ORDIS3              56
      *
     O          E            ROUTE          1
     O                       ORDIS4              56
      ***** End Add ***** 08/05/15 ************************************
      *
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      *
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
      **********
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *************************************************
      *
     O          E            ROUTE       2  2
     O                                           24 'REPORT SELECTIONS:'
      *
     O          E            ROUTE          2
     O                                           26 'COMPANY . . . . :'
     O                       KKCO#         Z   +001
      *
     O          E            ROUTE          2
     O                                           26 'GROUP . . . . . :'
     O                       KKFND         Z   +001
      *
     O          E            ROUTE          2
     O                                           26 'FROM DATE . . . :'
     O                       X1FRDT            +001 '  /  /    '
      *
     O          E            ROUTE          2
     O                                           26 'TO DATE . . . . :'
     O                       X1TODT            +001 '  /  /    '
      *
     O          E            ROUTE          2
     O                                           26 'LOSS CONTROL REP:'
     O               89                          31 '*ALL'
     O              N89      @@STAF              30
     O              N89      X1USNM              81
      ***** BEG ADD ******* 10/17/01 **********************************
      *
     O          E            ROUTE          2
     O                                           26 'SECONDARY SORT  :'
     O                       @@VE              +001
     O               75                          46 'VISIT DATE     '
     O              N75                          46 'EMPLOYER NUMBER'
      ***** BEG ADD ******* 10/17/01 **********************************
      *
     O          E            ROUTE          1 60
     O                                           25 '(C) COPYRIGHT PROGRESSI'
     O                                         +000 'VE DATA SOLUTIONS. INC.,'
     O                       *YEAR             +001
      *
      *
     O          E            HEAD           1 03
     O                       DSTIME               8 '  :  :  '
     O                       CONAME              86
     O                       DSDATE        Y    132
      *
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
      *
      *
     O          E            HEAD        1  1
     O                                         +000 'Group:'
     O                       KKFND             +001
     O                       X1FNNM            +001
      *
     O          E            HEAD           1
     O                                         +000 'Visit date range:'
     O                       X1FRDT            +001 '  /  /    '
     O                                         +001 '-'
     O                       X1TODT            +001 '  /  /    '
     O                                         +010 'Loss rep:'
     O              N89      @@STAF              64
     O               89                          65 '*ALL'
      ***** BEG ADD ******* 10/17/01 **********************************
     O                                         +010 'Secondary sort:'
     O                                         +001 'Employer number'
      ***** END ADD ******* 10/17/01 **********************************
      *
     O          E   N92      HEAD        1  1
     O                                         +000 'Loss rep:'
     O                       SVSTAF            +001
     O                       SVUSNM            +002
      *
     O          E   N92      HEAD           1
     O                                         +059 'Inception'
     O                                         +004 'Cancel'
     O                                         +010 'Annual'
     O                                         +007 'Visit'
     O                                         +005 'Visit'
      *
     O          E            HEAD           0
     O                       LINE              +000
      *
     O          E   N92      HEAD           2
     O                                         +000 'Employer#'
     O                                         +001 'Div  '
     O                                         +001 'Name'
     O                                         +041 'Date'
     O                                         +008 'Date'
     O                                         +011 'Premium'
     O                                         +006 'Date'
     O                                         +006 'Type'
     O                                         +003 'Hours'
      *
      *
     O          E   N93      DETAIL         1
     O                       VSEMP#        Z      9
     O                       VSDIV         Z   +001
     O                       X1EMNM            +001
     O              N71      X1INDT            +002 '  /  /    '
     O              N72      X1CNDT            +002 '  /  /    '
     O                       X1ANPR        J   +001
      *
     O          E            DETAIL         1
     O              N73      X1VSDT            +098 '  /  /    '
     O                       VSVSTP            +004
     O                       VSHRS         J   +003
      *
      ***** BEG ADD ******* 10/17/01 **********************************
      *
     O          E            HEAD1          1 03
     O                       DSTIME               8 '  :  :  '
     O                       CONAME              86
     O                       DSDATE        Y    132
      *
     O          E            HEAD1          1
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
      *
      *
     O          E            HEAD1       1  1
     O                                         +000 'Group:'
     O                       KKFND             +001
     O                       X1FNNM            +001
      *
     O          E            HEAD1          1
     O                                         +000 'Visit date range:'
     O                       X1FRDT            +001 '  /  /    '
     O                                         +001 '-'
     O                       X1TODT            +001 '  /  /    '
     O                                         +010 'Loss rep:'
     O              N89      @@STAF              64
     O               89                          65 '*ALL'
     O                                         +010 'Secondary sort:'
     O                                         +001 'Visit date'
      *
     O          E   N92      HEAD1       1  2
     O                                         +000 'Loss rep:'
     O                       SVSTAF            +001
     O                       SVUSNM            +002
      *
     O          E   N92      HEAD1          1
     O                                         +002 'Visit'
     O                                         +063 'Inception'
     O                                         +005 'Cancel'
     O                                         +010 'Annual'
     O                                         +005 'Visit'
      *
     O          E            HEAD1          0
     O                       LINE              +000
      *
     O          E   N92      HEAD1          2
     O                                         +003 'Date'
     O                                         +005 'Employer#'
     O                                         +001 'Div  '
     O                                         +001 'Name'
     O                                         +041 'Date'
     O                                         +008 'Date'
     O                                         +011 'Premium'
     O                                         +004 'Type'
     O                                         +003 'Hours'
      *
      *
     O          E            DET1           1
     O              N73N93   X1VSDT            +000 '  /  /    '
     O                       VSEMP#        Z   +002
     O                       VSDIV         Z   +001
     O                       X1EMNM            +001
     O              N71      X1INDT            +002 '  /  /    '
     O              N72      X1CNDT            +002 '  /  /    '
     O                       X1ANPR        J   +001
     O                       VSVSTP            +004
     O                       VSHRS         J   +003
      ***** END ADD ******* 10/17/01 **********************************
      *
      *
      ***** BEG ADD ******* 10/17/01 **********************************
     O          E    75      L2TOT          1
      *
      ***** END ADD ******* 10/17/01 **********************************
     O          E            L2TOT          1
     O                                         +000 'Totals for Loss Rep:'
     O                       SVUSNM            +001
      *
     O          E            L2TOT          1
     O                                         +030 '# visits:'
     O                       L2CNT         AB  +001
      ***** BEG CHG ******* 10/17/01 **********************************
     O*********                L2ANPRAB+027
     O              N75      L2ANPR        AB  +027
      ***** END CHG ******* 10/17/01 **********************************
     O                       L2HRS         AB  +017
      *
      *
     O          E            LRTOT       2  1
     O                                         +000 'Grand totals, all reps:'
      *
     O          E            LRTOT          3
     O                                         +005 '# visits:'
     O                       LRCNT         AB  +001
      ***** BEG CHG ******* 10/17/01 **********************************
     O*********                        +005 'Annual Premium:'
     O*********                LRANPRAB+001
     O              N75                        +005 'Annual Premium:'
     O              N75      LRANPR        AB  +001
      ***** END CHG ******* 10/17/01 **********************************
     O                                         +005 'Hours:'
     O                       LRHRS         AB  +001
      *
      *
     O          E            LSHEAD      2  0
     O                                         +028 '____'
     O                                         +002 '___________________'
     O                                         +028 '________'
     O                                         +011 '___________'
      *
     O          E            LSHEAD      0  1
     O                                         +028 'Code'
     O                                         +002 'Description        '
     O                                         +028 '# visits'
     O                                         +011 'Total hours'
      *
      *
     O          E            LSTOT          1
     O              N74                        +005 'Summary by visit code:'
     O                       TV(I)             +002
     O                       XXTPDS            +003
     O                       TOVT(1)       B   +001
     O                       THRT(1)       B   +003
      *
      *
     O          E            NOREC       2
     O                                           27 '*** NO RECORDS MEET THE'
     O                                           50 'SELECTED CRITERIA ***'
      *
      *
