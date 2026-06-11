     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE W7432R - LOSS RATIO REPORT IN VARIOUS SEQUENCES
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 04/10/00 C:118 PROJECT: WC987                                 *
     H*                Load alternate company name from fund master   *
     H*                                                               *
     H* 12/15/99 C:15  PROJECT: WC577                                 *
     H*                Recompiled due to chg in WXPLP.                *
     H*                                                               *
     H* 05/03/99 C:118 PROJECT: WC577                                 *
     H*                ADD 'REPORT SELECTIONS' TO ROUTING PAGE.       *
     H*                CHANGED 'FYR' TO 'YR'. MODIFIED REPORTING      *
     H*                FORMAT FOR SELECTIONS MADE.  CHANGED 'RUN DATE'*
     H*                FROM 6 TO 8 DIGITS.
     H*
     H* 07/17/98 C:108 PROJECT:WC577                                  *
     H*                EXPAND LOSS RATIO FROM 6,2 TO 7,2; ADJUST      *
     H*                THAT AND OTHER AFFECTED FIELDS.                *
     H*                                                               *
     H* 08/23/95 C:24 PROJECT: CM447                                  *
     H*               COMMERCIAL ENHANCEMENTS - ADD SELECTION BY      *
     H*               ACCIDENT DATE RANGE FOR NONCOMMON EXPIRATION    *
     H*               DATE FUNDS.  ADD SELECTION OF AN EMPLOYER RANGE *
     H*               FOR REPORTS SORTED BY EMPLOYER NUMBER.          *
     H*****************************************************************
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWXPLP     IF   E           K DISK
     FQSYSPRT   O    F  138        PRINTER OFLIND(*INOF)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATES
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D*
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECT DATES
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D*
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    PLEMP#
     C                   KFLD                    PLDIV
     C*
     C*  FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    EMDT          XXCADT
     C     *LIKE         DEFINE    EMEMP#        SVEMP#
     C     *LIKE         DEFINE    EMDIV         SVDIV
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQDTE             8
     C                   PARM                    QQSORT            1
     C                   PARM                    QQFRPR           11
     C                   PARM                    QQTOPR           11
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQFDAT            8
     C                   PARM                    QQTDAT            8
     C                   PARM                    QQFEMP            9
     C                   PARM                    QQFDIV            5
     C                   PARM                    QQTEMP            9
     C                   PARM                    QQTDIV            5
     C                   PARM                    QQCMFL            1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQDTE         X1DATE            8 0
     C                   MOVE      QQFRPR        XXFRPR           11 2
     C                   MOVE      QQTOPR        XXTOPR           11 2
     C                   MOVE      QQMODE        XXMODE            1
     C                   MOVE      QQFDAT        X1FDAT            8 0
     C                   MOVE      QQTDAT        X1TDAT            8 0
     C                   MOVE      QQFEMP        XXFEMP            9 0
     C                   MOVE      QQFDIV        XXFDIV            5 0
     C                   MOVE      QQTEMP        XXTEMP            9 0
     C                   MOVE      QQTDIV        XXTDIV            5 0
     C*
     C***** BEGIN CHG **** 05/03/99 ***********************************
     C**********           Z-ADDX1DATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXDATE  60
     C*
     C**********           Z-ADDX1FDAT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXFDAT  60
     C*
     C**********           Z-ADDX1TDAT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXTDAT  60
     C*
     C                   MOVE      X1DATE        XXDATE            8 0
     C                   MOVE      X1FDAT        XXFDAT            8 0
     C                   MOVE      X1TDAT        XXTDAT            8 0
     C***** END CHG ****** 05/03/99 ***********************************
     C*
     C     QQCMFL        IFEQ      *ON
     C                   MOVE      *ON           *IN70
     C                   ENDIF
     C*
     C     QQSORT        IFEQ      'E'
     C                   MOVE      *ON           *IN71
     C                   ENDIF
     C*
     C***** BEGIN ADD **** 05/03/99 ***********************************
     C     XXFEMP        IFEQ      *ZERO
     C     XXFDIV        ANDEQ     *ZERO
     C     XXTEMP        ANDEQ     *HIVAL
     C     XXTDIV        ANDEQ     *HIVAL
     C                   MOVE      *ON           *IN99
     C                   ENDIF
     C***** END ADD ****** 05/03/99 ***********************************
     C*
     C                   Z-ADD     *ZERO         SVEMP#
     C                   Z-ADD     *ZERO         SVDIV
     C*
     C                   Z-ADD     0             HFYR
     C                   Z-ADD     0             L1PREM           11 2
     C                   Z-ADD     0             L1ITOT           11 2
     C                   Z-ADD     0             L1PTOT           11 2
     C                   Z-ADD     0             L1OPEN            5 0
     C                   Z-ADD     0             L1CLOS            5 0
     C                   Z-ADD     0             L1TOTC            5 0
     C*
     C                   READ      WXPLP                                  60
     C*
     C     *IN60         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    NORECS
     C                   GOTO      TERM
     C                   END
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C*   CHAIN TO EMPLOYER MASTER FILE
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     HFYR          IFNE      *ZERO
     C     HFYR          ANDNE     PLFYR
     C     QQCMFL        ANDEQ     *BLANK
     C                   EXSR      SUBYR
     C                   MOVE      '1'           *INOF
     C                   END
     C*
     C                   Z-ADD     PLFYR         HFYR              3 0
     C     PLEMP#        IFNE      SVEMP#
     C     PLDIV         ORNE      SVDIV
     C                   ADD       1             EMPTOT            5 0
     C                   ENDIF
     C*
     C                   Z-ADD     PLEMP#        SVEMP#
     C                   Z-ADD     PLDIV         SVDIV
     C                   MOVEL     EMNAME        ENAME            28
     C*
     C*   FIRST CYCLE PROCESSING
     C*
     C  N11              EXSR      SR1ST
     C*
     C* Check to find out if employer was cancelled on or before the
     C* run date.
     C*
     C                   MOVE      QQDTE         X1CADT            8 0
     C                   Z-ADD     X1CADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCADT
     C*
     C*   FIND THE EFFECTIVE DATE
     C                   Z-ADD     1             X                 1 0
     C     *ZERO         LOOKUP    ED(X)                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     5             X
     C                   ELSE
     C                   SUB       1             X
     C                   END
     C     X             DOWNE     0
     C     XXCADT        COMP      ED(X)                              60  60
     C  N60              SUB       1             X
     C  N60              END
     C*
     C*   CHECK CANCELLED DATE
     C     X             IFEQ      0
     C                   MOVE      *BLANKS       CANFL             2
     C*
     C                   ELSE
     C*
     C     CD(X)         IFLE      XXCADT
     C     CD(X)         ANDNE     0
     C                   MOVE      'CX'          CANFL
     C                   ELSE
     C                   MOVE      *BLANKS       CANFL
     C                   END
     C*
     C                   END
     C*
     C*  ACCUMULATE FUND YEAR TOTALS
     C*
     C                   Z-ADD     0             XXTOTC            5 0
     C     PLCLOS        ADD       PLOPEN        XXTOTC
     C                   ADD       PLPREM        L1PREM           11 2
     C                   ADD       PLOPEN        L1OPEN            5 0
     C                   ADD       PLCLOS        L1CLOS            5 0
     C                   ADD       XXTOTC        L1TOTC            5 0
     C                   ADD       PLINC         L1ITOT           11 2
     C                   ADD       PLPAID        L1PTOT           11 2
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    DET
     C*
     C                   END
     C*
     C                   READ      WXPLP                                  60
     C                   END
     C*
     C                   EXSR      SUBYR
     C*
     C*    CALCULATE LOSS RATIO
     C*
     C     LRPREM        IFNE      *ZERO
     C     LRITOT        MULT      100           XXITOT           13 2
     C***** BEGIN CHG **** 07/17/98 ***********************************
     C********** XXITOT    DIV  LRPREM    LRLOSS  62H
     C     XXITOT        DIV(H)    LRPREM        LRLOSS            7 2
     C***** END   CHG **** 07/17/98 ***********************************
     C                   ELSE
     C                   Z-ADD     *ZERO         LRLOSS
     C                   END
     C*
     C*    CALCULATE FREQUENCY RATIO
     C*
     C     LRPREM        DIV(H)    1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     LRTOTC        DIV(H)    THPREM        LRFREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         LRFREQ
     C                   END
     C*
     C                   MOVE      '1'           *IN12
     C                   EXCEPT    HEAD
     C                   EXCEPT    TOTLR
     C*
     C     TERM          TAG
      /Free
       // ***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       // ***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C*
     C     SR1ST         BEGSR
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C     COKEY         CHAIN     SMCOL                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        XFNAME           60
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C***** END   ADD **** 04/10/00 ***********************************
     C***** BEGIN ADD **** 05/03/99 ***********************************
     C                   MOVE      *BLANKS       XXTITL           60
     C*
     C     XXFRPR        IFEQ      *ZERO
     C     XXTOPR        ANDEQ     *HIVAL
     C                   MOVE      *ON           *IN98
     C                   ENDIF
     C***** END ADD ****** 05/03/99 ***********************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C***** BEGIN ADD **** 05/03/99 ***********************************
     C*   LEFT JUSTIFIY TITLE ON ROUTING PAGE
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXTITL
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      XXTITL
     C                   ENDIF
     C                   ENDIF
     C***** END ADD ****** 05/03/99 ***********************************
      ***** Beg Del ***** 08/26/14 ************************************
     C********           EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C*
     C* * * PRINT 1ST PAGE HEADINGS
     C*
     C                   EXCEPT    HEAD
     C*
     C                   ENDSR
     C*
      *==================================================================*
      *   FUND YR BREAK                    ** Subroutine **   S U B Y R  *
      *==================================================================*
     C*
     C     SUBYR         BEGSR
     C*
     C                   ADD       EMPTOT        EMPL3             5 0
     C                   ADD       L1PREM        LRPREM           11 2
     C                   ADD       L1CLOS        LRCLOS            5 0
     C                   ADD       L1OPEN        LROPEN            5 0
     C                   ADD       L1TOTC        LRTOTC            5 0
     C                   ADD       L1ITOT        LRITOT           11 2
     C                   ADD       L1PTOT        LRPTOT           11 2
     C*
     C*    CALCULATE LOSS RATIO
     C*
     C     L1PREM        IFNE      *ZERO
     C     L1ITOT        MULT      100           XXITOT
     C***** BEGIN CHG **** 07/17/98 ***********************************
     C********** XXITOT    DIV  L1PREM    L1LOSS  62H
     C     XXITOT        DIV(H)    L1PREM        L1LOSS            7 2
     C***** END   CHG **** 07/17/98 ***********************************
     C                   ELSE
     C                   Z-ADD     *ZERO         L1LOSS
     C                   END
     C*
     C*    CALCULATE FREQUENCY RATIO
     C*
     C     L1PREM        DIV(H)    1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     L1TOTC        DIV(H)    THPREM        L1FREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L1FREQ
     C                   END
     C*
     C     QQCMFL        IFEQ      *BLANK
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '1'           *INOF
     C                   END
     C*
     C                   EXCEPT    TOTL1
     C*
     C                   ENDIF                                                  QQCMFL = *BLANK
     C*
     C*   CLEAR TOTAL FIELDS FOR YEAR
     C*
     C                   Z-ADD     0             EMPTOT
     C                   Z-ADD     0             L1PREM
     C                   Z-ADD     0             L1CLOS
     C                   Z-ADD     0             L1OPEN
     C                   Z-ADD     0             L1TOTC
     C                   Z-ADD     0             L1ITOT
     C                   Z-ADD     0             L1PTOT
     C*
     C                   ENDSR
     C**
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ********************************************
     O*********                                   8 'XXXXXXXX'
     O*********                                  20 'XXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXX     XX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXXX    XX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XXXX    XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   8 'XXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   4 'XXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   5 'XXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   6 'XX XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   7 'XX  XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX    XXXX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   8 'XX   XXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  33 'XXXXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX    XXXX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          3
     O*********                                   9 'XX    XXX'
     O*********                                  20 'XXXXXX'
     O*********                                  32 'XXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX     XXX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          1
     O*********                                  15 'ROUTE TO:'
     O*********      08      ORDIS1              60
     O*********         09   UPUSNM              67
     O********* E            ROUTE          1
     O*********      08      ORDIS2              60
     O********* E            ROUTE          1
     O*********      08      ORDIS3              60
     O********* E            ROUTE          2
     O*********      08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O***** BEGIN CHG ******** 05/03/99 *******************************
     O**********               ORTITL    80
     O                       XXTITL              80
     O***** END CHG ********** 05/03/99 *******************************
      ***** Beg Del ***** 08/26/14 ************************************
     O********* E            ROUTE          2
     O*********                                  19 'PROGRAM NAME:'
     O*********              ORPNAM              30
     O*********                                  36 'FILE:'
     O*********              ORFIL#              38
     O*********                                  46 'COPIES:'
     O*********              ORCOPY        Z     50
     O*********                                  56 'HOLD:'
     O*********              ORHOLD              61
     O********* E            ROUTE          2
     O*********                                  19 'OUTPUT QUEUE:'
     O*********              OROUTQ              30
     O*********                                  37 'FORMS:'
     O*********              ORFORM              48
     O*********                                  56 'SAVE:'
     O*********              ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O***** BEGIN ADD ******** 05/03/99 *******************************
     O          E            ROUTE       3  2
     O                                           24 'REPORT SELECTIONS:'
     O          E            ROUTE          2
     O                                           35 'COMPANY                :'
     O                       XXCO#               47
     O          E            ROUTE          2
     O                                           35 'GROUP                  :'
     O                       XXFND               47
     O          E            ROUTE          2
     O                                           35 'LOSSES AS OF           :'
     O                       XXDATE              47 '  /  /    '
     O          E    70      ROUTE          2
     O               70                          35 'ACCIDENT DATE RANGE    :'
     O               70      XXFDAT              47 '  /  /    '
     O               70                          49 '-'
     O               70      XXTDAT              60 '  /  /    '
     O          E    70      ROUTE          2
     O               70                          35 'SORT SELECTION         :'
     O               70      QQSORT              47
     O          E    70      ROUTE          2
     O               70                          35 'PREMIUM RANGE          :'
     O               70N98   XXFRPR        J     52
     O               70N98                       54 '-'
     O               70N98   XXTOPR        J     70
     O               70 98                       47 '*ALL'
     O*
     O          E   N70      ROUTE          2
     O              N70                          35 'POLICY YEAR            :'
     O              N70      HFYR                47
     O          E   N70      ROUTE          2
     O              N70                          35 'SORT SELECTION         :'
     O              N70      QQSORT              47
     O          E            ROUTE          2
     O               71                          35 'EMPLOYER/DIVISION RANGE:'
     O               71N99   XXFEMP        3     47
     O               71N99                       48 '/'
     O               71N99   XXFDIV        3     53
     O               71N99                       55 '-'
     O               71N99   XXTEMP        3     65
     O               71N99                       66 '/'
     O               71N99   XXTDIV        3     71
     O               71 99                       47 '*ALL'
     O          E   N70      ROUTE          2
     O              N70                          35 'PREMIUM RANGE          :'
     O              N70N98   XXFRPR        J     52
     O              N70N98                       53 '-'
     O              N70N98   XXTOPR        J     69
     O              N70 98                       47 '*ALL'
     O***** END ADD ********** 05/03/99 *******************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O****                               23 '(c) Copyright Progressi'
     O****                                  've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    132
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O***** BEGIN ADD ******** 05/03/99 *******************************
     O          E            HEAD           1
     O                                          120 'RUN DATE:'
     O                       XXDATE        Y    132
     O***** END ADD ********** 05/03/99 *******************************
     O          E            HEAD           2
     O                                              'GROUP:'
     O                       XXFND         Z   +  1
     O                       XFNAME            +  1
     O          E            HEAD           1
     O              N12N70                          'POLICY YEAR:'
     O              N12N70   HFYR          Z   +  1
     O              N70                          32 'PREMIUM RANGE:'
     O***** BEGIN CHG ******** 05/03/99 *******************************
     O**********      N70      XXFRPRJ   48
     O**********      N70                50 '-'
     O**********      N70      XXTOPRJ   66
     O              N70N98   XXFRPR        J     48
     O              N70N98                       50 '-'
     O              N70N98   XXTOPR        J     66
     O              N70 98                       37 '*ALL'
     O**********         N70             79 'RUN DATE:'
     O**********         N70   XXDATEY   88
     O                 N70                      105 'SORT SELECTION:'
     O                 N70   QQSORT             107
     O*
     O                  70                       20 'ACCIDENT DATE RANGE:'
     O**********          70   XXFDATY   29
     O**********          70             31 '-'
     O**********          70   XXTDATY   40
     O                  70   XXFDAT        Y     32
     O                  70                       34 '-'
     O                  70   XXTDAT        Y     45
     O**********          70             56 'PREMIUM RANGE:'
     O**********          70   XXFRPRJ   72
     O**********          70             74 '-'
     O**********          70   XXTOPRJ   90
     O                  70                       62 'PREMIUM RANGE:'
     O               70N98   XXFRPR        J     78
     O               70N98                       80 '-'
     O               70N98   XXTOPR        J     96
     O               70 98                       67 '*ALL'
     O**********          70            103 'RUN DATE:'
     O**********          70   XXDATEY  112
     O**********          70            129 'SORT SELECTION:'
     O**********          70   QQSORT   131
     O                  70                      115 'SORT SELECTION:'
     O                  70   QQSORT             118
     O          E            HEAD           1
     O                  71                       19 'EMPLOYER/DIV RANGE:'
     O**********          71   XXFEMP    29 '       0 '
     O**********          71             30 '/'
     O**********          71   XXFDIV    35 '   0 '
     O**********          71             37 '-'
     O**********          71   XXTEMP    47 '       0 '
     O**********          71             48 '/'
     O**********          71   XXTDIV    53 '   0 '
     O*
     O               71N99   XXFEMP        3     29
     O               71N99                       30 '/'
     O               71N99   XXFDIV        3     35
     O               71N99                       37 '-'
     O               71N99   XXTEMP        3     47
     O               71N99                       48 '/'
     O               71N99   XXTDIV        3     53
     O               71 99                       25 '*ALL'
     O***** END CHG ********** 05/03/99 *******************************
     O          E            HEAD           1
     O                                           74 'OPEN  CLOSED'
     O                                           81 'TOTAL'
     O***** BEGIN CHG **** 07/17/98 ***********************************
     O********************************* 128 'LOSS  FREQ'
     O                                          129 'LOSS  FREQ'
     O***** END   CHG **** 07/17/98 ***********************************
     O          E            HEAD           2
     O              N12                           9 'EMPLOYER'
     O              N12                          15 'DIV'
     O              N12N70                       29 'EMPLOYER NAME'
     O***** BEGIN CHG **** 05/03/99 ***********************************
     O**********      N12 70             19 'FYR'
     O              N12 70                       19 'YR'
     O***** END CHG ****** 05/03/99 ***********************************
     O              N12 70                       33 'EMPLOYER NAME'
     O                                           59 'PREM TO DATE'
     O                                           81 'CLAIMS CLAIMS CLAIMS'
     O                                           96 'INCURRED'
     O                                          112 'PAID'
     O***** BEGIN CHG **** 07/17/98 ***********************************
     O********************************* 128 'RATIO RATIO'
     O                                          129 'RATIO RATIO'
     O***** END   CHG **** 07/17/98 ***********************************
     O          E            DET            1
     O                       PLEMP#        Z      9
     O                       PLDIV         Z     15
     O                 N70   ENAME               44
     O                  70   PLFYR         Z     19
     O                  70   ENAME               48
     O                       PLPREM        JB    60
     O                       PLOPEN        3B    67
     O                       PLCLOS        3B    74
     O                       XXTOTC        3B    81
     O                       PLINC         JB    97
     O                       PLPAID        JB   113
     O***** BEGIN CHG **** 07/17/98 **********************************
     O************************ PLLOSSJB 123
     O                       PLLOSS        JB   124
     O************************ PLFREQJB 129
     O                       PLFREQ        JB   130
     O***** END   CHG **** 07/17/98 **********************************
     O                       CANFL              132
     O          E            TOTL1       1  1
     O                                           15 'EMPLOYER COUNT:'
     O                       EMPTOT        3B    21
     O                                           44 'GROUP YEAR TOTALS:'
     O                       L1PREM        JB    60
     O                       L1OPEN        3B    67
     O                       L1CLOS        3B    74
     O                       L1TOTC        3B    81
     O                       L1ITOT        JB    97
     O                       L1PTOT        JB   113
     O***** BEGIN CHG **** 07/17/98 ***********************************
     O************************ L1LOSSJB 123
     O                       L1LOSS        JB   124
     O************************ L1FREQJB 129
     O                       L1FREQ        JB   130
     O***** END   CHG **** 07/17/98 ***********************************
     O          E            TOTLR       1  1
     O                                           15 'EMPLOYER COUNT:'
     O                       EMPL3         3B    21
     O                                           44 'GROUP TOTALS:'
     O                       LRPREM        JB    60
     O                       LROPEN        3B    67
     O                       LRCLOS        3B    74
     O                       LRTOTC        3B    81
     O                       LRITOT        JB    97
     O                       LRPTOT        JB   113
     O***** BEGIN CHG **** 07/17/98 **********************************
     O************************ LRLOSSJB 123
     O                       LRLOSS        JB   124
     O************************ LRFREQJB 129
     O                       LRFREQ        JB   130
     O***** END   CHG **** 07/17/98 **********************************
     O          E            NORECS      2  2
     O                                           23 '*** NO RECORDS MEET THE'
     O                                           46 ' SELECTION CRITERIA ***'
