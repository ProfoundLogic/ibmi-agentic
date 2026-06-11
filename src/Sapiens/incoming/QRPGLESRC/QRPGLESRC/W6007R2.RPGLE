     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W6007R2 - GET FROM DATE FOR GROUP CHECK RUN
     H*****************************************************************
     H* PROGRAM DESCRIPTIION:                                         *
     H*                                                               *
     H* THIS PROGRAM:                                                 *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W6007CG                                                     *
     H* CALLS  TO:                                                    *
     H*   none                                                        *
     H*                                                               *
     H* CREATED 05/28/04  C:53  PROJECT: WC1198                       *
     H*                                                               *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 05/28/04 C:15  Project: WC1198                                *
     H*                Chgd code to use the date past into the pgm as *
     H*                the batch date.                                *
     H*****************************************************************
     H*  INDICATORS:                                                  *
     H*                                                               *
     H*
     H*  @@RTCD VALUES
     H*                                                               *
     H*****************************************************************
     FWAC3L2    IF   E           K DISK
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  ZZDT                   1      8  0
     D  ZZMM                   1      2  0
     D  ZZDD                   3      4  0
     D  ZZYY                   5      8  0
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  SSDT                   1      8  0
     D  SSMM                   1      2  0
     D  SSDD                   3      4  0
     D  SSYY                   5      8  0

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@FRDT            8            From dt
     C                   PARM                    @@PROC            1            Process flag
     C                   PARM                    @@CO#             3
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     CPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKPRDT                         Process dt
     C*
     C     *LIKE         DEFINE    CPCO#         KKCO#
     C     *LIKE         DEFINE    CPPRDT        KKPRDT
     C*
     C                   TIME                    DSDTTM
     C*** Begin chg *** 05/28/04 **************************************/
     C*******************  Z-ADDDSDATE    SSDT
     C                   MOVE      @@FRDT        SSDT
     C*** End chg ***** 05/28/04 **************************************/
     C*
     C*** REMOVED SUBMISSION SELECTION BY DATE PROCESS ***
     C                   MOVE      'Y'           @@PROC
     C**********
     C****DEFAULT TO PROCESS NO
     C**********
     C**********           MOVE 'N'       @@PROC
     C**********
     C****DO*NOT PROCESS IF NOT THE A VALID SUBMISSION DATE
     C**********
     C**********           SELEC
     C********** SSDD      WHLE 14
     C**********           GOTO EOJ
     C**********
     C********** SSDD      WHEQ 15
     C**********           MOVE 'Y'       @@PROC
     C**********
     C********** SSDD      WHGE 16
     C********** SSDD      ANDLE27
     C**********           GOTO EOJ
     C**********
     C****CHECK*FOR MONTH END DATES
     C**********
     C**********           OTHER
     C**********
     C****MONTH*END DATES
     C**********
     C**********           SELEC
     C**30*DAYS*
     C********** SSDD      WHEQ 30
     C**********
     C********** SSMM      IFEQ 4
     C********** SSMM      OREQ 6
     C********** SSMM      OREQ 9
     C********** SSMM      OREQ 11
     C**********           MOVE 'Y'       @@PROC
     C**********           ENDIF
     C**31*DAYS*
     C********** SSDD      WHEQ 31
     C**********           MOVE 'Y'       @@PROC
     C**********
     C**FEBRUARY / LEAP YEARS
     C**********
     C********** SSMM      WHEQ 2
     C********** SSDD      ANDEQ28
     C**********
     C********** SSYY      IFNE 2004
     C********** SSYY      ANDNE2008
     C********** SSYY      ANDNE2012
     C********** SSYY      ANDNE2016
     C********** SSYY      ANDNE2020
     C********** SSYY      ANDNE2024
     C********** SSYY      ANDNE2028
     C********** SSYY      ANDNE2032
     C********** SSYY      ANDNE2036
     C********** SSYY      ANDNE2040
     C********** SSYY      ANDNE2044
     C********** SSYY      ANDNE2048
     C********** SSYY      ANDNE2052
     C********** SSYY      ANDNE2056
     C**********           MOVE 'Y'       @@PROC
     C**********           ENDIF
     C**********
     C********** SSMM      WHEQ 2
     C********** SSDD      ANDEQ29
     C**********           MOVE 'Y'       @@PROC
     C**********
     C**********           ENDSL
     C**********           ENDSL
     C**********
     C****PROCESS = NO - RETURN
     C**********
     C********** @@PROC    CABEQ'N'       EOJ
     C*
     C*** GET FROM DATE
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      *HIVAL        KKPRDT
     C*
     C     CPKY          SETGT     PCP01
     C     KKCO#         READPE    PCP01                                  99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVE      '03262004'    @@FRDT
     C                   GOTO      EOJ
     C                   ENDIF
     C*
     C                   Z-ADD     CPTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ZZDT
     C*
     C*** YEAR END DATE
     C*
     C     ZZMM          IFEQ      12
     C     ZZDD          ANDEQ     31
     C                   Z-ADD     1             ZZMM
     C                   Z-ADD     1             ZZDD
     C                   ADD       1             ZZYY
     C                   MOVE      ZZDT          @@FRDT
     C                   GOTO      EOJ
     C                   ENDIF
     C*
     C*** MONTH END DATES
     C*
     C                   SELECT
     C     ZZDD          WHENEQ    30
     C*
     C     ZZMM          IFEQ      4
     C     ZZMM          OREQ      6
     C     ZZMM          OREQ      9
     C     ZZMM          OREQ      11
     C                   Z-ADD     1             ZZDD
     C                   ADD       1             ZZMM
     C                   ELSE
     C                   ADD       1             ZZDD
     C                   ENDIF
     C*
     C     ZZDD          WHENEQ    31
     C                   Z-ADD     1             ZZDD
     C                   ADD       1             ZZMM
     C*
     C     ZZMM          WHENEQ    2
     C     ZZDD          ANDEQ     28
     C*
     C     ZZYY          IFNE      2004
     C     ZZYY          ANDNE     2008
     C     ZZYY          ANDNE     2012
     C     ZZYY          ANDNE     2016
     C     ZZYY          ANDNE     2020
     C     ZZYY          ANDNE     2024
     C     ZZYY          ANDNE     2028
     C     ZZYY          ANDNE     2032
     C     ZZYY          ANDNE     2036
     C     ZZYY          ANDNE     2040
     C     ZZYY          ANDNE     2044
     C     ZZYY          ANDNE     2048
     C     ZZYY          ANDNE     2052
     C     ZZYY          ANDNE     2056
     C                   Z-ADD     1             ZZDD
     C                   ADD       1             ZZMM
     C                   ELSE
     C                   ADD       1             ZZDD
     C                   ENDIF
     C*
     C     ZZMM          WHENEQ    2
     C     ZZDD          ANDEQ     29
     C                   Z-ADD     1             ZZDD
     C                   ADD       1             ZZMM
     C*
     C                   OTHER
     C                   ADD       1             ZZDD
     C                   ENDSL
     C*
     C                   MOVE      ZZDT          @@FRDT
     C*
     C     EOJ           TAG
     C                   MOVE      *ON           *INLR
     C                   RETURN
