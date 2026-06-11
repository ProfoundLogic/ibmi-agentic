     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7331R - EMPLOYER LABELS
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/30/09 C:191 SUPPORT: OP071/69859                           *
     H*                ADJUST SECOND COLUMN ON LASER PRINT SO THAT    *
     H*                FIRST CHARACTER IS NOT CUTOFF                  *
     H*                                                               *
     H* 07/24/08 C:61  PROJECT: 46WGC                                 *
     H*                Process labels in order of Mass CX batch.      *
     H*                                                               *
     H* 07/26/04 C:53  PROJECT: WC1235                                *
     H*                Base POL#/Federal name on a flag.              *
     H*                                                               *
     H* 01/15/04 C:53  PROJECT: WC1176                                *
     H*                Rmv POL#/Cancel dt. Add Federal name.          *
     H*                                                               *
     H* 10/06/99 C:87  PROJECT: SUPP #19894                           *
     H*                If called in cancellation process - don't use  *
     H*                WXEBP fields to chain to WMEMP.                *
     H*                                                               *
     H*  02/18/98 C:58 SUPPORT WC091 SEQ# 10327                       *
     H*                Use numeric instructions for dates.
     H*                                                               *
     H*  02/12/98 C:61 PROJECT 81WLI                                  *
     H*                Corrected Print of Cancel Date.                *
     H*                                                               *
     H* 12/10/97  C:35  PROJECT WC700                                 *
     H*                 REMOVED FUND MASTER FILE, SINCE IT IS NOT     *
     H*                 NECESSARY TO PRINT THE POLICY NUMBER IN       *
     H*                 DIFFERENT FORMATS FOR COMMERCIAL/SIG.  ALSO   *
     H*                 FIXED PROBLEM WHERE FYR WAS NOT PRINTING      *
     H*                 ON THE LABELS.                                *
     H*                                                               *
     H*  11/20/96 C:57 PROJECT CM024                                  *
     H*                ADDED BATCH# PARM TO ALLOW FOR CALL FROM ISSUE *
     H*                MASS CANCELLATIONS.  THIS PARM WILL ALLOW FOR  *
     H*                PROCESSING OF LABELS FOR EMPLRS CANCELLED IN   *
     H*                BATCH RUN WITHIN ONE SPOOL FILE.               *
     H*                                                               *
     H*****************************************************************
      *
     FWXEBL     IF   E           K DISK
      ******* beg chg ******* 07/24/08 *****************************
     F*******WXCNL3  IF  E           K        DISK
     FWXCNL7    IF   E           K DISK
      ******* beg chg ******* 07/24/08 *****************************
     FWMEML     IF   E           K DISK
     FWMAHL     IF   E           K DISK
      **** BEGIN DEL **** 12/10/97 ***************************************
     F********** WMFNL   IF  E           K        DISK
      **** END   DEL **** 12/10/97 ***************************************
      *
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOF)
     FQSYSPRT2  O    F  132        PRINTER OFLIND(*INOB)
      *

     D SZ              S              1    DIM(13)                              STATE & ZIP
      *** BEG CHG *** 01/15/04 ****************************
     D***********         XXA1        2 33               ADDR LEFT
     D***********         XXA2        2 33               ADDR RIGHT
     D XXA1            S             33    DIM(4)                               ADDR LEFT
     D XXA2            S             33    DIM(4)                               ADDR RIGHT
      *** END CHG *** 01/15/04 ****************************
     D BRK             S             34    DIM(4) CTDATA PERRCD(1)              BREAK DESCRIPTION
      *
     D                 DS
     D  EMPZIP                 1      9  0
     D  XXZPP1                 1      5  0
     D  XXZPP2                 6      9  0
      *
     D                 DS
     D  EMMZIP                 1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
      *
     D                 DS
     D  XXCNTY                 1      4
     D  EBJRST                 1      2
     D  EBCNTY                 3      4
      *
      * System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      * KEY LIST FOR THE MASS CANCELLATION DETAIL FILE
      *

     C     CNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXBAT#
      *
      * KEY LIST FOR THE EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      * KEY LIST FOR THE AGENT HISTORY MASTER FILE
      *
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      **** BEGIN DEL **** 12/10/97 ************************************
      *
      * KEY LIST FOR THE FUND MASTER FILE
      *
     C********** FNKY      KLIST
     C******************** KFLD           XXCO#
     C******************** KFLD           XXFND
      **** END   DEL **** 12/10/97 ************************************
      *
      *****************************************************************
      *
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMFND         XXFND1
     C     *LIKE         DEFINE    EMFND         XXFND2
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMEMP#        XXEMP1
     C     *LIKE         DEFINE    EMEMP#        XXEMP2
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMDIV         XXDIV1
     C     *LIKE         DEFINE    EMDIV         XXDIV2
     C     *LIKE         DEFINE    CNBAT#        XXBAT#
     C     *LIKE         DEFINE    EBFYR         XXFYR
     C     *LIKE         DEFINE    EBFYR         XXFYR1
     C     *LIKE         DEFINE    EBFYR         XXFYR2
     C     *LIKE         DEFINE    EBAGT#        XXHAGT
     C     *LIKE         DEFINE    EBCHAP        XXHCHP
     C     *LIKE         DEFINE    EBRGN#        XXHRGN
      *
     C                   MOVE      *BLANKS       XXNAME           33
      *** BEG ADD *** 01/15/04 ****************************
     C                   MOVE      *BLANKS       XXENAM           33
     C                   MOVE      *BLANKS       XXENA1           33
     C                   MOVE      *BLANKS       XXENA2           33
      *** END ADD *** 01/15/04 ****************************
     C                   MOVE      *BLANKS       XXNAM1           33
     C                   MOVE      *BLANKS       XXNAM2           33
     C                   MOVE      *BLANKS       XXADR1           33
     C                   MOVE      *BLANKS       XXAD11           33
     C                   MOVE      *BLANKS       XXAD12           33
     C                   MOVE      *BLANKS       XXADR2           33
     C                   MOVE      *BLANKS       XXCSZ            33
     C                   MOVE      *BLANKS       XXSTAT            2
     C                   MOVE      *BLANKS       XXZIP1            5
     C                   MOVE      *BLANKS       XXCITY           18
     C                   MOVE      *BLANKS       XXST             13
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVE      *BLANK        XXTERM            1
     C                   MOVE      *BLANK        XXTYPE            1
     C                   MOVE      *BLANK        XXFORM            1
     C                   MOVE      *BLANK        XXSORT            1
     C                   MOVE      *BLANKS       XXHCNY            4
     C                   MOVE      *BLANKS       XXBRK            34
     C                   MOVE      *BLANKS       XXAGTC            5
     C                   MOVE      *BLANKS       XXCHPC            3
     C                   MOVE      *BLANKS       XXRGNC            3
      *
     C                   Z-ADD     0             XXZIP2            4 0
     C                   Z-ADD     0             XXAGT#            5 0
     C                   Z-ADD     0             XXCXDT            6 0
     C                   Z-ADD     0             XXCXD8            8 0
     C                   Z-ADD     0             XXCXD1            8 0
     C                   Z-ADD     0             XXCXD2            8 0
     C                   Z-ADD     0             XXLBL#            2 0
     C                   Z-ADD     0             X                 1 0
      *
      *****************************************************************
      *
      *
      *   ENTRY PARAMETERS
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQBAT#            9
     C                   PARM                    QQSORT            1
     C                   PARM                    QQFORM            1
      *** BEG ADD *** 07/26/04 ****************************
     C                   PARM                    QQPOL#            1
      *** END ADD *** 07/26/04 ****************************
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
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY '       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQBAT#        XXBAT#
     C                   MOVE      QQTYPE        XXTYPE
     C                   MOVE      QQSORT        XXSORT
     C                   MOVE      QQFORM        XXFORM
     C                   TIME                    DSDTTM
      **** BEGIN DEL **** 12/10/97 ***************************************
      *
      * READ THE FUND MASTER TO DETERMINE IF COMMERCIAL
      *
     C********** FNKY      CHAINPFN01                60
      *
     C********** *IN60     IFEQ *OFF
      *
     C********** FNCGFL    IFEQ 'Y'
     C******************** MOVE *ON       *IN40
     C******************** ELSE
     C******************** MOVE *OFF      *IN40
     C******************** ENDIF                           FNCGFL IFEQ 'Y'
      **** END   DEL **** 12/10/97 ***************************************
      *** BEG ADD *** 07/26/04 ****************************
     C                   SELECT
     C     QQPOL#        WHENEQ    'P'
     C                   MOVE      *ON           *IN40
      *
     C     QQPOL#        WHENEQ    'F'
     C                   MOVE      *OFF          *IN40
      *
      *** DEFAULT TO POL# IF CALLED FROM BATCH
      *
     C                   OTHER
     C                   MOVE      *ON           *IN40
     C                   ENDSL                                                              'Y'
      *** END ADD *** 07/26/04 ****************************
      *
     C                   Z-ADD     10            XXLBL#
     C                   Z-ADD     0             XXHAGT
     C                   Z-ADD     0             XXHCHP
     C                   Z-ADD     10            XXHRGN
      *
      *  NO BATCH #
      *
     C     XXBAT#        IFEQ      0
      *
     C                   READ      PEB01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   EXSR      SRADDR
      *
     C                   MOVE      EBAGT#        XXAGT#
      *
     C     XXSORT        IFEQ      'A'
     C     EBAGT#        ANDNE     XXHAGT
      *
     C     XXSORT        OREQ      'C'
     C     XXCNTY        ANDNE     XXHCNY
      *
     C     XXSORT        OREQ      'H'
     C     EBCHAP        ANDNE     XXHCHP
      *
     C     XXSORT        OREQ      'R'
     C     EBRGN#        ANDNE     XXHRGN
      *
     C                   EXSR      BREAK
      *
     C                   ENDIF                                                  XXSORT IFEQ
      *
      **** BEGIN ADD **** 12/10/97 ****************************************
     C                   MOVE      EBFYR         XXFYR
      **** END   ADD **** 12/10/97 ****************************************
     C     XXFORM        IFEQ      'P'
     C                   EXSR      SRPRNT
     C                   ELSE
     C                   EXSR      LASPRT
     C                   ENDIF                                                  XXFORM IFEQ 'P'
      *
      *** BEG ADD *** 01/15/04 ****************************
     C                   MOVE      *OFF          *IN55
      *** END ADD *** 01/15/04 ****************************
      *
     C                   READ      PEB01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OF
      *
     C                   ELSE
      *
      * BATCH# PASSED
      *
     C     CNKY          CHAIN     PCN01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   Z-ADD     CNEMP#        XXEMP#
     C                   Z-ADD     CNDIV         XXDIV
      *
     C     EMKY          CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   EXSR      SRADDR
      *
     C                   EXSR      SRAGT
      *
     C     XXFORM        IFEQ      'P'
     C                   EXSR      SRPRNT
     C                   ELSE
     C                   EXSR      LASPRT
     C                   ENDIF                                                  XXFORM IFEQ 'P'
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C     CNKY          READE     PCN01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OF
      *
     C                   ENDIF                                                  XXBAT# IFEQ 0
      *
      **** BEGIN DEL **** 12/10/97 ***************************************
     C******************** ENDIF                           *IN60 IFEQ *OFF
      **** END   DEL **** 12/10/97 ***************************************
      *
      * IF PRINTING LASER LABELS AND RIGHT LABEL WAS NOT FILLED,
      * NEED TO CLEAR RIGHT LABEL AND PRINT LAST SET OF LEFT/RIGHT
      * LABELS
      *
     C     XXFORM        IFEQ      'L'
     C     *IN30         ANDEQ     *ON
     C                   EXSR      LASLST
     C                   ENDIF                                                  XXFORM IFEQ 'L'
      *
     C                   MOVE      '1'           *INLR
      *
      *****************************************************
      * SUBROUTINE TO PRINT BREAK LABELS                  *
      *****************************************************
     C     BREAK         BEGSR
      *
     C                   SELECT
      *
     C     XXSORT        WHENEQ    'A'
     C                   MOVEL(P)  BRK(1)        XXBRK
     C                   MOVE      EBAGT#        XXAGTC
     C                   CAT       XXAGTC:1      XXBRK
     C                   MOVE      EBAGT#        XXHAGT
      *
     C     XXSORT        WHENEQ    'C'
     C                   MOVEL(P)  BRK(2)        XXBRK
     C                   CAT       XXCNTY:1      XXBRK
     C                   MOVE      XXCNTY        XXHCNY
      *
     C     XXSORT        WHENEQ    'H'
     C                   MOVEL(P)  BRK(3)        XXBRK
     C                   MOVE      EBCHAP        XXCHPC
     C                   CAT       XXCHPC:1      XXBRK
     C                   MOVE      EBCHAP        XXHCHP
      *
     C     XXSORT        WHENEQ    'R'
     C                   MOVEL(P)  BRK(4)        XXBRK
     C                   MOVE      EBRGN#        XXRGNC
     C                   CAT       XXRGNC:1      XXBRK
     C                   MOVE      EBRGN#        XXHRGN
      *
     C                   ENDSL                                                  XXSORT WHEQ
      *
     C     XXFORM        IFEQ      'L'
      *
     C     *IN30         IFEQ      *ON
      *
     C                   EXSR      LASLST
      *
     C                   ELSE
      *
     C     XXLBL#        IFEQ      10
      *
     C                   EXCEPT    LASTOP
     C                   Z-ADD     0             XXLBL#
      *
     C                   ENDIF                                                  XXLBL# IFEQ 10
      *
     C                   ENDIF                                                  *IN30 IFEQ *ON
      *
     C                   EXCEPT    LASBRK
      *
     C                   ADD       1             XXLBL#
      *
     C                   ELSE
      *
     C     XXTYPE        IFEQ      'R'
      *
     C                   EXCEPT    REGBRK
      *
     C                   ELSE
      *
     C                   EXCEPT    LOGBRK
      *
     C                   ENDIF                                                  XXTYPE IFEQ 'R'
      *
     C                   ENDIF                                                  XXFORM IFEQ 'L'
      *
     C                   ENDSR                                                  BREAK
      *
      *****************************************************
      * SUBROUTINE TO PRINT LAST LABEL                    *
      *****************************************************
     C     LASLST        BEGSR
      *
     C     XXLBL#        IFEQ      10
     C                   EXCEPT    LASTOP
     C                   ENDIF                                                  XXLBL# IFEQ
      *
     C                   Z-ADD     0             XXFND2
     C                   Z-ADD     0             XXEMP2
     C                   Z-ADD     0             XXDIV2
     C                   Z-ADD     0             XXFYR2
      *** BEG ADD *** 01/15/04 ****************************
     C                   MOVE      *BLANKS       XXENA2
      *** END ADD *** 01/15/04 ****************************
     C                   MOVE      *BLANKS       XXNAM2
     C                   MOVE      *BLANKS       XXAD12
     C                   MOVE      *BLANKS       XXA2(1)
     C                   MOVEL     *BLANKS       XXA2(2)
     C                   MOVE      *OFF          *IN54
     C                   Z-ADD     0             XXCXD2
      *** BEG ADD *** 07/26/04 ****************************
     C*** XXA2,1 & XXA2,2 ARE BLANK. SUPPRESS POL# PRINT
     C*
     C                   MOVE      *ON           *IN41
      *** END ADD *** 07/26/04 ****************************
      *
     C                   EXCEPT    LASDET
      *
     C                   ADD       1             XXLBL#
      *
     C                   ENDSR                                                  LASLST
      *
      *****************************************************
      * SUBROUTINE TO PRINT LASER LABELS                  *
      *****************************************************
     C     LASPRT        BEGSR
      *
      *   MOVE NAME TO SMALLER FIELD
      *
      *** BEG CHG *** 01/15/04 ****************************
     C***********          MOVELEMNAME    XXNAME
     C                   MOVEL     EMFDNM        XXNAME
      *** END CHG *** 01/15/04 ****************************
      *** BEG ADD *** 01/15/04 ****************************
     C                   MOVEL(P)  EMNAME        XXENAM
      *** END ADD *** 01/15/04 ****************************
     C                   MOVE      *OFF          *IN52
      *
     C     XXBAT#        IFEQ      0
      *
     C     EBCNDT        IFNE      0
      *
     C*** BEGIN CHG ******* 02/18/98 **********************************
     C**********           MOVE EBCNDT    SRDTI
     C                   Z-ADD     EBCNDT        SRDTI
     C*** END CHANGE ****** 02/18/98 **********************************
      *
     C                   CALL      'SRCVTDAT'    DTMDYY
      *
     C                   MOVEL     SRDTO         XXCXD8
     C                   MOVE      *ON           *IN52
      *
     C                   ENDIF                                                  EBCNDT IFNE 0
      *
     C                   ELSE
      *
     C*** BEGIN CHG ******* 02/18/98 **********************************
     C**********           MOVE CNDATE    SRDTI
     C                   Z-ADD     CNDATE        SRDTI
     C*** END CHANGE ****** 02/18/98 **********************************
      *
     C                   CALL      'SRCVTDAT'    DTMDYY
      *
     C                   MOVEL     SRDTO         XXCXD8
     C                   MOVE      *ON           *IN52
      *
     C                   ENDIF                                                  XXBAT# IFEQ 0
      *
     C     *IN30         IFEQ      *OFF
      *
      * MOVE FIELDS TO LEFT LABEL
      *
     C                   MOVE      *ON           *IN30
     C                   MOVE      EMFND         XXFND1
     C                   MOVE      EMEMP#        XXEMP1
     C                   MOVE      EMDIV         XXDIV1
     C                   MOVE      XXFYR         XXFYR1
      *** BEG CHG *** 01/15/04 ****************************
     C***********          MOVELXXADR1    XXAD11
     C***********          MOVELXXNAME    XXNAM1
     C                   MOVEL     XXENAM        XXENA1
      *** END CHG *** 01/15/04 ****************************
     C                   Z-ADD     1             X
      *** BEG ADD *** 01/15/04 ****************************
      *
     C                   MOVE      *BLANKS       XXA1
      *
      *** BEG CHG *** 07/26/04 ****************************
     C********** XXENAM    IFNE XXNAME
     C  N40XXENAM        IFNE      XXNAME
      *** END CHG *** 07/26/04 ****************************
     C     XXNAME        ANDNE     *BLANKS
     C                   MOVEL     XXNAME        XXA1(X)
     C                   ADD       1             X
     C                   ENDIF
      *
     C                   MOVEL     XXADR1        XXA1(X)
     C                   ADD       1             X
      *** END ADD *** 01/15/04 ****************************
      *
     C     XXADR2        IFNE      *BLANKS
      *
     C                   MOVEL     XXADR2        XXA1(X)
     C                   ADD       1             X
      *
     C                   ENDIF                                                  XXADR2 IFNE
      *
     C                   MOVEL     XXCSZ         XXA1(X)
      *
      *** BEG DEL *** 01/15/04 ****************************
     C***********X         IFLT 2
     C***********          ADD  1         X
     C***********          MOVE *BLANKS   XXA1,X
     C***********          ENDIF                           X LFLT 2
      *** END DEL *** 01/15/04 ****************************
      *
     C                   MOVE      *IN52         *IN53
      *
     C     *IN52         IFEQ      *ON
     C                   MOVE      XXCXD8        XXCXD1
     C                   ENDIF                                                  *IN52 IFEQ
      *
     C                   ELSE
      *
      * MOVE FIELDS TO RIGHT LABEL
      *
     C                   MOVE      *OFF          *IN30
     C                   MOVE      EMFND         XXFND2
     C                   MOVE      EMEMP#        XXEMP2
     C                   MOVE      EMDIV         XXDIV2
     C                   MOVE      XXFYR         XXFYR2
      *** BEG CHG *** 01/15/04 ****************************
     C***********          MOVELXXADR1    XXAD12
     C***********          MOVELXXNAME    XXNAM2
     C                   MOVEL     XXENAM        XXENA2
      *** END CHG *** 01/15/04 ****************************
     C                   Z-ADD     1             X
      *** BEG ADD *** 01/15/04 ****************************
      *
     C                   MOVE      *BLANKS       XXA2
      *
      *** BEG CHG *** 07/26/04 ****************************
     C********** XXENAM    IFNE XXNAME
     C  N40XXENAM        IFNE      XXNAME
      *** END CHG *** 07/26/04 ****************************
     C     XXNAME        ANDNE     *BLANKS
     C                   MOVEL     XXNAME        XXA2(X)
     C                   ADD       1             X
     C                   ENDIF
      *
     C                   MOVEL     XXADR1        XXA2(X)
     C                   ADD       1             X
      *** END ADD *** 01/15/04 ****************************
      *
     C     XXADR2        IFNE      *BLANKS
      *
     C                   MOVEL     XXADR2        XXA2(X)
     C                   ADD       1             X
      *
     C                   ENDIF                                                  XXADR2 IFNE
      *
     C                   MOVEL     XXCSZ         XXA2(X)
      *
      *** BEG DEL *** 01/15/04 ****************************
     C***********X         IFLT 2
     C***********          ADD  1         X
     C***********          MOVE *BLANKS   XXA2,X
     C***********          ENDIF                           X LFLT 2
      *** END DEL *** 01/15/04 ****************************
      *
      *** BEG ADD *** 07/26/04 ****************************
     C     XXA2(1)       IFEQ      *BLANKS
     C     XXA2(2)       ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN41
     C                   ELSE
     C                   MOVE      *OFF          *IN41
     C                   ENDIF
      *** END ADD *** 07/26/04 ****************************
     C                   MOVE      *IN52         *IN54
      *
     C     *IN52         IFEQ      *ON
     C                   MOVE      XXCXD8        XXCXD2
     C                   ENDIF                                                  *IN52 IFEQ
      *
      * IF 10 LEFT/RIGHT LABEL SETS HAVE BEEN PRINTED, SKIP TO
      * FIRST LABEL ON NEXT PAGE
      *
     C     XXLBL#        IFEQ      10
      *
     C                   Z-ADD     0             XXLBL#
      *
     C                   EXCEPT    LASTOP
      *
     C                   ENDIF                                                  XXLBL# IFEQ
      *
      * PRINT ONE SET OF LEFT/RIGHT LABELS
      *
     C                   EXCEPT    LASDET
      *
     C                   ADD       1             XXLBL#
      *
     C                   ENDIF                                                  *IN30 IFEQ *OFF
      *
     C                   ENDSR                                                  LASPRT
      *
      ********************************************************
      * SUBROUTINE TO GET EMPLOYER ADDRESS                   *
      ********************************************************
     C     SRADDR        BEGSR
     C*
     C***  BEGIN ADD  ***  10/06/99  **********************************
     C     XXBAT#        IFEQ      0
     C***  END   ADD  ***  10/06/99  **********************************
      *
     C                   MOVE      EBEMP#        XXEMP#
     C                   MOVE      EBDIV         XXDIV
      *
     C     EMKY          CHAIN     PEM01                              60
     C*
     C***  BEGIN ADD  ***  10/06/99  **********************************
     C                   ENDIF                                                  XXBAT#=0
     C***  END   ADD  ***  10/06/99  **********************************
      *
     C     EMMPFL        IFEQ      'M'
      *
      *   LOAD CITY, STATE, & ZIP FOR THE MAILING ADDRESS
      *
     C                   MOVE      EMMST         XXSTAT
     C                   MOVE      XXZPM1        XXZIP1
     C                   MOVE      XXZPM2        XXZIP2
     C                   MOVEL(P)  EMMCTY        XXCITY
     C                   MOVEL(P)  EMMAD1        XXADR1
     C                   MOVEL(P)  EMMAD2        XXADR2
      *
     C                   ELSE
      *
      *   LOAD CITY, STATE, & ZIP FOR THE PHYSICAL ADDRESS
      *
     C                   MOVE      EMPST         XXSTAT
     C                   MOVE      XXZPP1        XXZIP1
     C                   MOVE      XXZPP2        XXZIP2
     C                   MOVEL(P)  EMPCTY        XXCITY
     C                   MOVEL(P)  EMPAD1        XXADR1
     C                   MOVEL(P)  EMPAD2        XXADR2
      *
     C                   ENDIF                                                  EMMPFL IFEQ 'M'
      *
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     XXSTAT        XXST
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZIP1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
      *
     C     XXZIP2        IFNE      0
      *
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZIP2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
      *
     C                   ENDIF                                                  XXZIP2 IFNE 0
      *
     C                   MOVE      *BLANKS       XXSZ
     C                   MOVEA     SZ            XXSZ
     C                   MOVEL(P)  XXCITY        XXLAST
     C                   MOVEL(P)  XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM
      *
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
      *
     C                   MOVEL(P)  XXLAST        XXCSZ
      *
     C                   ENDSR                                                  SRADDR
      *
      ******************************************************
      *    -SUBROUTINE TO GET THE AGENT NUMBER             *
      ******************************************************
     C     SRAGT         BEGSR
      *
     C                   MOVE      EMEMP#        XXEMP#
     C                   MOVE      EMDIV         XXDIV
     C                   MOVE      CNFYR         XXFYR
      *
     C     AHKY          SETGT     PAH01
     C                   READP     PAH01                                  60
      *
     C     *IN60         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C     *IN60         IFEQ      *OFF
      *
     C     AHCO#         ANDEQ     XXCO#
     C     AHFND         ANDEQ     XXFND
     C     AHEMP#        ANDEQ     EMEMP#
     C     AHDIV         ANDEQ     EMDIV
     C     AHFYR         ANDEQ     XXFYR
      *
     C                   Z-ADD     AHAGT#        XXAGT#
      *
     C                   ELSE
      *
     C                   Z-ADD     0             XXAGT#
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   ENDSR                                                  SRAGT
      *
      *****************************************************
      * SUBROUTINE TO PRINT REQUESTED INFORMATION         *
      *****************************************************
     C     SRPRNT        BEGSR
      *
      *   MOVE NAME TO SMALLER FIELD
      *
      *** BEG CHG *** 01/15/04 ****************************
     C***********          MOVELEMNAME    XXNAME    P
     C                   MOVEL(P)  EMFDNM        XXNAME
      *** END CHG *** 01/15/04 ****************************
      *** BEG ADD *** 01/15/04 ****************************
     C                   MOVEL(P)  EMNAME        XXENAM
     C     XXENAM        IFNE      XXNAME
     C     XXNAME        ANDNE     *BLANKS
     C                   MOVE      *ON           *IN55
     C                   ENDIF
      *** END ADD *** 01/15/04 ****************************
      ***** BEG ADD ******* 02/12/98 **********************************
     C                   SETOFF                                       52
      ***** END ADD ******* 02/12/98 **********************************
      *
     C     XXBAT#        IFEQ      0
      *
     C     EBCNDT        IFNE      0
      *
     C*** BEGIN CHG ******* 02/18/98 **********************************
     C**********           MOVE EBCNDT    SRDTI
     C                   Z-ADD     EBCNDT        SRDTI
     C*** END CHANGE ****** 02/18/98 **********************************
      *
     C                   CALL      'SRCVTDAT'    DTMDYY
      *
     C                   MOVEL     SRDTO         XXCXD8
     C                   MOVE      *ON           *IN52
      *
     C                   ENDIF                                                  EBCNDT IFNE 0
      *
     C                   ELSE
      *
     C*** BEGIN CHG ******* 02/18/98 **********************************
     C**********           MOVE CNDATE    SRDTI
     C                   Z-ADD     CNDATE        SRDTI
     C*** END CHANGE ****** 02/18/98 **********************************
      *
     C                   CALL      'SRCVTDAT'    DTMDYY
      *
     C                   MOVEL     SRDTO         XXCXD8
     C                   MOVE      *ON           *IN52
      *
     C                   ENDIF                                                  XXBAT# IFEQ 0
      *
      * Print on the Correct Label Type
      *
     C     XXTYPE        IFEQ      'R'
      *
     C     *IN52         IFEQ      *ON
      *
     C*** BEGIN CHG ******* 02/18/98 **********************************
     C**********           MOVE XXCXD8    SRDTI
     C                   Z-ADD     XXCXD8        SRDTI
     C*** END CHANGE ****** 02/18/98 **********************************
      *
     C                   CALL      'SRCVTDAT'    DTMDY
      *
     C                   MOVEL     SRDTO         XXCXDT
      *
     C                   ENDIF                                                  *IN52 IFEQ *ON
      *
     C                   EXCEPT    DET
      ***** BEG ADD ******* 02/12/98 **********************************
     C                   SETOFF                                       52
      ***** END ADD ******* 02/12/98 **********************************
      *
     C     XXADR2        IFEQ      *BLANKS
      *
     C                   EXCEPT    DET3
      *
     C                   ELSE
      *
     C                   EXCEPT    DET2
      *
     C                   ENDIF                                                  XXADR2 IFEQ *BL
      *
     C                   ENDIF                                                  XXTYPE IFEQ 'R'
      *
     C     XXTYPE        IFEQ      'L'
      *
     C                   EXCEPT    LDET
      *
     C     XXADR2        IFEQ      *BLANKS
      *
     C                   EXCEPT    LDET3
      *
     C                   ELSE
      *
     C                   EXCEPT    LDET2
      *
     C                   ENDIF                                                  XXADR2 IFEQ *BL
      *
     C                   ENDIF                                                  XXTYPE IFEQ 'L'
      *
     C                   ENDSR                                                  SRPRNT
      *
      *
      *
      *
      * Regular Labels
      *
     OQSYSPRT   E            DET            1 01
      *** BEG DEL *** 01/15/04 ****************************
     O***********              EMFND     04
      **** BEGIN DEL **** 12/10/97 **************************************
     O*************** N40                05 '-'
     O*************** N40      EMEMP#    14
     O*************** N40      EMDIV     20
      **** END   DEL **** 12/10/97 **************************************
      **** BEGIN CHG **** 12/10/97 **************************************
     O**************** 40      EMEMP#    13
     O**************** 40      XXFYR     16
     O**************** 40      EMDIV     22
     O***********              EMEMP#    13
     O***********              XXFYR     16
     O***********              EMDIV     22
      **** END   CHG **** 12/10/97 **************************************
     O***********      52                25 'CX'
     O***********      52      XXCXDT    36 '  /  /  '
      *** END DEL *** 01/15/04 ****************************
      *** BEG ADD *** 07/26/04 ****************************
     O               40      EMFND               04
     O               40      EMEMP#              13
     O               40      XXFYR               16
     O               40      EMDIV               22
      *** END ADD *** 07/26/04 ****************************
      *** BEG ADD *** 01/15/04 ****************************
      *** BEG CHG *** 07/26/04 ****************************
     O***********              XXENAM    34
     O              N40      XXENAM              34
      *** END CHG *** 07/26/04 ****************************
      *** END ADD *** 01/15/04 ****************************
      *** BEG CHG *** 01/15/04 ****************************
     O********E  1             DET
      *** BEG CHG *** 07/26/04 ****************************
     O********E  1     55      DET
     O          E    55N40   DET            1
      *** END CHG *** 07/26/04 ****************************
      *** END CHG *** 01/15/04 ****************************
     O                       XXNAME              34
      *** BEG ADD *** 07/26/04 ****************************
     O          E    40      DET            1
     O                       XXENAM              34
      *** END ADD *** 07/26/04 ****************************
     O          E            DET            1
     O                       XXADR1              34
     O          E            DET2           1
     O                       XXADR2              34
     O          E            DET2           1
     O                       XXCSZ               34
     O          E            DET3           1
     O                       XXCSZ               34
     O          E            REGBRK         1 01
     O                                           11 '**********'
     O                                           35 '************************'
     O          E            REGBRK         1
     O                                           11 '**********'
     O                                           35 '************************'
     O          E            REGBRK         1
     O                       XXBRK               35
     O          E            REGBRK         1
     O                                           11 '**********'
     O                                           35 '************************'
     O          E            REGBRK         1
     O                                           11 '**********'
     O                                           35 '************************'
      *
      * Logo Labels
      *
     OQSYSPRT1  E            LDET           1 11
      *** BEG DEL *** 01/15/04 ****************************
     O***********              EMFND     13
      **** BEGIN DEL **** 12/10/97 *********************************
     O*************** N40                14 '-'
     O*************** N40      EMEMP#    23
     O*************** N40      EMDIV     29
      **** END   DEL **** 12/10/97 *********************************
      **** BEGIN CHG **** 12/10/97 *********************************
     O**************** 40      EMEMP#    22
     O**************** 40      XXFYR     25
     O**************** 40      EMDIV     31
     O***********              EMEMP#    22
     O***********              XXFYR     25
     O***********              EMDIV     31
      **** END   CHG **** 12/10/97 *********************************
     O******* E  1             LDET
     O***********      52                12 'CX'
     O***********      52      XXCXD8    23 '  /  /    '
     O********E  1             LDET
      *** END DEL *** 01/15/04 ****************************
      *** BEG ADD *** 07/26/04 ****************************
     O               40      EMFND               13
     O               40      EMEMP#              22
     O               40      XXFYR               25
     O               40      EMDIV               31
      *** END ADD *** 07/26/04 ****************************
      *** BEG CHG *** 01/15/04 ****************************
     O***********              XXNAME    38
      *** BEG CHG *** 07/26/04 ****************************
     O***********              XXENAM    38
     O              N40      XXENAM              38
      *** END CHG *** 07/26/04 ****************************
      *** END CHG *** 01/15/04 ****************************
      *** BEG ADD *** 01/15/04 ****************************
      *** BEG CHG *** 07/26/04 ****************************
     O******* E  1     55      LDET
     O          E    55N40   LDET           1
      *** END CHG *** 07/26/04 ****************************
     O                       XXNAME              38
      *** END ADD *** 01/15/04 ****************************
      *** BEG ADD *** 07/26/04 ****************************
     O          E    40      LDET           1
     O                       XXENAM              38
      *** END ADD *** 07/26/04 ****************************
     O          E            LDET           1
     O                       XXADR1              38
     O          E            LDET2          1
     O                       XXADR2              38
     O          E            LDET2          1
     O                       XXCSZ               38
     O          E            LDET3          2
     O                       XXCSZ               38
      *
     O          E            LOGBRK         1 11
     O                                           11 '**********'
     O                                           35 '************************'
     O          E            LOGBRK         1
     O                                           11 '**********'
     O                                           35 '************************'
     O          E            LOGBRK         1
     O                       XXBRK               35
     O          E            LOGBRK         1
     O                                           11 '**********'
     O                                           35 '************************'
     O          E            LOGBRK         1
     O                                           11 '**********'
     O                                           35 '************************'
     O          E            LOGBRK         1
     O                                           11 '**********'
     O                                           35 '************************'
      *
      * LASER LABELS
      *
     OQSYSPRT2  E            LASTOP         0  4
     O          E            LASDET         1
      *** BEG DEL *** 01/15/04 ****************************
     O*******
     O*******                  XXFND1    05
     O*****BEGIN DEL **** 12/10/97 *****************************
     O*************** N40                06 '-'
     O*************** N40      XXEMP1    15
     O*************** N40      XXDIV1    21
     O*****END   DEL **** 12/10/97 *****************************
     O*****BEGIN CHG **** 12/10/97 *****************************
     O**************** 40      XXEMP1    14
     O**************** 40      XXFYR1    17
     O**************** 40      XXDIV1    23
     O*******                  XXEMP1    14
     O*******                  XXFYR1    17
     O*******                  XXDIV1    23
     O*****END   CHG **** 12/10/97 *****************************
     O*******          53                26 'CX'
     O*******          53      XXCXD1    37 '  /  /    '
     O*******                  XXFND2    46
     O*****BEGIN DEL **** 12/10/97 *****************************
     O*************** N40                47 '-'
     O*************** N40      XXEMP2    56
     O*************** N40      XXDIV2    62
     O*****END   DEL **** 12/10/97 *****************************
     O*****BEGIN CHG **** 12/10/97 *****************************
     O**************** 40      XXEMP2    55
     O**************** 40      XXFYR2    58
     O**************** 40      XXDIV2    64
     O*******                  XXEMP2    55
     O*******                  XXFYR2    58
     O*******                  XXDIV2    64
     O*****END   CHG **** 12/10/97 *****************************
     O*******          54                67 'CX'
     O*******          54      XXCXD2    78 '  /  /    '
      *** END DEL *** 01/15/04 ****************************
      *** BEG ADD *** 07/26/04 ****************************
     O               40      XXFND1              05
     O               40      XXEMP1              14
     O               40      XXFYR1              17
     O               40      XXDIV1              23
     O*** BEGIN CHG ****** 09/30/09 ************************************
     O*************    40N41   XXFND2    46
     O*************    40N41   XXEMP2    55
     O*************    40N41   XXFYR2    58
     O*************    40N41   XXDIV2    64
     O               40N41   XXFND2              48
     O               40N41   XXEMP2              57
     O               40N41   XXFYR2              60
     O               40N41   XXDIV2              66
     O*** END   CHG ****** 09/30/09 ************************************
      *** END ADD *** 07/26/04 ****************************
      *** BEG ADD *** 01/15/04 ****************************
      *** BEG CHG *** 07/26/04 ****************************
     O***********              XXENA1    35
     O***********              XXENA2    76
     O              N40      XXENA1              35
     O*** BEGIN CHG ****** 09/30/09 ************************************
     O*************   N40      XXENA2    76
     O              N40      XXENA2              78
     O*** END   CHG ****** 09/30/09 ************************************
      *** END CHG *** 07/26/04 ****************************
      *** END ADD *** 01/15/04 ****************************
      *** BEG CHG *** 01/15/04 ****************************
     O******* E  1             LASDET
     O*******                  XXNAM1    35
     O*******                  XXNAM2    76
     O******* E  1             LASDET
     O*******                  XXAD11    35
     O*******                  XXAD12    76
     O******* E  1             LASDET
     O*******                  XXA1,1    35
     O*******                  XXA2,1    76
     O******* E  2             LASDET
     O*******                  XXA1,2    35
     O*******                  XXA2,2    76
     O***
      *** BEG ADD *** 07/26/04 ****************************
     O          E    40      LASDET         1
     O                       XXENA1              35
     O*** BEGIN CHG ****** 09/30/09 ************************************
     O*************            XXENA2    76
     O                       XXENA2              78
     O*** END   CHG ****** 09/30/09 ************************************
      *** END ADD *** 07/26/04 ****************************
     O          E            LASDET         1
     O                       XXA1(1)             35
     O*** BEGIN CHG ****** 09/30/09 ************************************
     O*************            XXA2,1    76
     O                       XXA2(1)             78
     O*** END   CHG ****** 09/30/09 ************************************
     O          E            LASDET         1
     O                       XXA1(2)             35
     O*** BEGIN CHG ****** 09/30/09 ************************************
     O*************            XXA2,2    76
     O                       XXA2(2)             78
     O*** END   CHG ****** 09/30/09 ************************************
     O          E            LASDET         1
     O                       XXA1(3)             35
     O*** BEGIN CHG ****** 09/30/09 ************************************
     O*************            XXA2,3    76
     O                       XXA2(3)             78
     O*** END   CHG ****** 09/30/09 ************************************
      *** BEG CHG *** 07/26/04 ****************************
     O******* E  2             LASDET
     O          E   N40      LASDET         2
      *** END CHG *** 07/26/04 ****************************
     O                       XXA1(4)             35
     O*** BEGIN CHG ****** 09/30/09 ************************************
     O*************            XXA2,4    76
     O                       XXA2(4)             78
     O*** END   CHG ****** 09/30/09 ************************************
      *** END CHG *** 01/15/04 ****************************
      *** BEG ADD *** 07/26/04 ****************************
     O          E    40      LASDET         1
      *** END ADD *** 07/26/04 ****************************
     O          E            LASBRK         1
     O                                           11 '**********'
     O                                           35 '************************'
     O*** BEGIN CHG ****** 09/30/09 ************************************
     O*******************                52 '**********'
     O*******************                76 '************************'
     O                                           54 '**********'
     O                                           78 '************************'
     O*** END   CHG ****** 09/30/09 ************************************
     O          E            LASBRK         1
     O                                           11 '**********'
     O                                           35 '************************'
     O*** BEGIN CHG ****** 09/30/09 ************************************
     O*******************                52 '**********'
     O*******************                76 '************************'
     O                                           54 '**********'
     O                                           78 '************************'
     O*** END   CHG ****** 09/30/09 ************************************
     O          E            LASBRK         1
     O                       XXBRK               35
     O*** BEGIN CHG ****** 09/30/09 ************************************
     O*******************      XXBRK     76
     O                       XXBRK               78
     O*** END   CHG ****** 09/30/09 ************************************
     O          E            LASBRK         1
     O                                           11 '**********'
     O                                           35 '************************'
     O*** BEGIN CHG ****** 09/30/09 ************************************
     O*******************                52 '**********'
     O*******************                76 '************************'
     O                                           54 '**********'
     O                                           78 '************************'
     O*** END   CHG ****** 09/30/09 ************************************
     O          E            LASBRK         2
     O                                           11 '**********'
     O                                           35 '************************'
     O*** BEGIN CHG ****** 09/30/09 ************************************
     O*******************                52 '**********'
     O*******************                76 '************************'
     O                                           54 '**********'
     O                                           78 '************************'
     O*** END   CHG ****** 09/30/09 ************************************
** BRK - BREAK DESCRIPTION
BEGIN LABELS FOR AGENT
BEGIN LABELS FOR COUNTY
BEGIN LABELS FOR CHAPTER
BEGIN LABELS FOR REGION
