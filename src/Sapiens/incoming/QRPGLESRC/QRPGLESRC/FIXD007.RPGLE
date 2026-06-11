      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD007 - Check claim payment amounts
      *****************************************************************
      *
      * THIS pgm DOES NOT UPDATE
      *
      * DESCRIPTION:  This pgm identifies payment discrepancies
      *   between the payment file and the payments in the claim master
      *   file.
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
      *
     FBADPV3    IF   E             DISK
     FWMCML1    IF   E           K DISK
     FWDPAL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     D XXCO#           S              3  0 INZ(777)
     D TOTRA           S                   LIKE(CMAR01) INZ
     D TOTRR           S                   LIKE(CMRR01) INZ
     D TOTTA           S                   LIKE(PAAMT)  INZ
     D TOTDIF          S                   LIKE(PAAMT)  INZ
     D TOTCNT          S              5  0 INZ
     D TA              S              9  2 DIM(10)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  RC                     1     20
     D                                     DIM(10)
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
     D  RA                     1     90  2
     D                                     DIM(10)
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
     D  RR                     1     90  2
     D                                     DIM(10)
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
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C**************************************************************************
     C*
     C     XXKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    P3FND
     C                   KFLD                    P3FYR
     C                   KFLD                    P3CASE

     C                   Z-ADD     0             X                 3 0
     C                   Z-ADD     0             Z                 3 0
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C                   EXCEPT    HEAD
     C**************************************************************************
     C*  Start reading the record in error
     C*
     C                   READ      BADPV3

     C                   DOW       NOT %EOF(BADPV3)
     C     CMKY          CHAIN     PCM01                              50
     C     *IN50         IFEQ      '0'
      *
      *  Accumulate actual payments
      *
     C                   CLEAR                   TA
     C     XXKY          CHAIN     PPA01                              60
     C     *IN60         DOWEQ     *OFF
      *
     C                   Z-ADD     1             X
     C     PARECD        LOOKUP    RC(X)                                  61
     C     *IN61         IFEQ      *ON
     C     PATYPE        IFEQ      'A'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'P'
     C     PATYPE        OREQ      'Z'
     C                   ADD       PAAMT         TA(X)
     C                   ELSE
     C                   SUB       PAAMT         TA(X)
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    RC(X)                                  61
     C   61              MOVE      PARECD        RC(X)
     C     *IN61         IFEQ      *ON
     C     PATYPE        IFEQ      'A'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'P'
     C     PATYPE        OREQ      'Z'
     C                   Z-ADD     PAAMT         TA(X)
     C                   ELSE
     C                   Z-SUB     PAAMT         TA(X)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXKY          READE     PPA01                                  60
     C                   ENDDO
     C*
     C*    Calculate difference between actual and released payments.
     C*
      *
     C                   DO        10            IJ                3 0
     C                   MOVE      *ZEROS        DIFF              9 2
     C     TA(IJ)        SUB       RA(IJ)        DIFF
     C*
     C                   ADD       RA(IJ)        TOTRA
     C                   ADD       TA(IJ)        TOTTA
     C                   ADD       RR(IJ)        TOTRR
     C                   EVAL      TOTDIF = TOTDIF + DIFF
     C                   IF        DIFF <> *ZERO
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   EVAL      TOTCNT = TOTCNT + 1
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C*  Get the next claim record.
     C*
     C**                 ENDIF
     C                   ENDIF
     C*
     C                   READ      BADPV3
     C                   ENDDO
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    TOT1
     C                   MOVE      '1'           *INLR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                                           75 'Check for payments out of'
     O                                         +001 'balance'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                           10 'WTFIXD007'
     O                                          125 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           2
     O                                            3 'FND'
     O                                           10 'YEAR'
     O                                           20 'CLAIM #'
     O                                           27 'CODE'
     O                                           40 'RSV PAYMNT'
     O                                           60 'CHK PAYMNT'
     O                                           73 'DIFFERENCE'
     O          E            DET
     O                       CMFND         4      3
     O                       CMFYR         4      9
     O                       CMCASE        4     20
     O                       RC(IJ)              26
     O                       RA(IJ)        K     41
     O                       TA(IJ)        K     61
     O                       DIFF          KB    83
     O          E            TOT1        2
     O                       TOTCNT        Z     26
     O                       TOTRA         K     41
     O                       TOTTA         K     61
     O                       TOTDIF        K     83
