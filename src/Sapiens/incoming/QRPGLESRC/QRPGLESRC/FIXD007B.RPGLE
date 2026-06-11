      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD007B - Check reserve history vs claim incurred amount
      *****************************************************************
      *
      * THIS pgm DOES NOT UPDATE
      *
      * DESCRIPTION:  This pgm identifies  discrepancies
      *   between the claim incurred amount and reserve history.
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
      *
     FBADPV3    IF   E             DISK
     FWMRHL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D @1ST            S               N   INZ(*ON)
     D TOTTC           S                   LIKE(CMAR01) INZ
     D TOTTR           S                   LIKE(RHSU01) INZ
     D TOTCNT          S              5  0 INZ
     D TOTDIF          S              9  2 INZ

     D TC              S              9  2 DIM(10)
     D TR              S              9  2 DIM(10)
     D DF              S              9  2 DIM(10)
     D DS              S             40    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  RT                     1     20
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
     D  RC                     1     20
     D                                     DIM(10)
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
     D                 DS
     D  AR                     1     90  2
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
     D  SU                     1     90  2
     D                                     DIM(10)
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
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C**************************************************************************
     C*
     C     RHKY          KLIST
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
     C                   KFLD                    P3CO
     C                   KFLD                    P3FND
     C                   KFLD                    P3FYR
     C                   KFLD                    P3CASE

     C*
     C                   Z-ADD     0             X                 3 0
     C                   Z-ADD     0             Z                 3 0
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C**************************************************************************
     C*  Start reading the claim records.
     C*
     C                   READ      BADPV3
     C                   DOW       NOT %EOF(BADPV3)
     C     CMKY          CHAIN     PCM01                              50

     C     *IN50         IFEQ      '0'
     C*
     C*  Clear the workfields.
     C*
     C                   CLEAR                   TC
     C                   CLEAR                   TR
     C*
     C*  Load the claim total incurred.
     C*
     C                   DO        10            X
     C                   ADD       RR(X)         TC(X)
     C                   ADD       AR(X)         TC(X)
     C                   ADD       RR(X)         TOTTC
     C                   ADD       AR(X)         TOTTC
     C                   ENDDO
     C*
     C*  Read the reserve history records.
     C*
     C     RHKY          CHAIN     PRH01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*
     C                   DO        10            X
     C*
     C                   Z-ADD     1             Z
     C     RC(X)         LOOKUP    RT(Z)                                  60
     C   60              ADD       SU(X)         TR(Z)
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             Z
     C     *BLANKS       LOOKUP    RT(Z)                                  60
     C                   Z-ADD     SU(X)         TR(Z)
     C                   MOVEL     RC(X)         RT(Z)
     C                   ENDIF
     C*
     C                   ADD       SU(X)         TOTTR
     C                   ENDDO
     C*
     C     RHKY          READE     PRH01                                  60
     C                   ENDDO
     C*
     C*  Determine if the reserves are out of balance.  If so, create
     C*    a resrve history entry.
     C*
     C*  Check total reserves to total incurred.
     C                   XFOOT     TC            CLMTI            11 2
     C                   XFOOT     TR            HISTI            11 2
     C*
     C                   DO        10            X
     C*
     C     TC(X)         SUB       TR(X)         DF(X)
     C                   EVAL      TOTDIF = TOTDIF + DF(X)
     C                   IF        DF(X) <> *ZERO
     C                   IF        @1ST or *INOF
     C                   MOVE      *OFF          @1ST
     C                   EXCEPT    HEAD
     C                   ENDIF
     C                   EXCEPT    DET
     C                   EVAL      TOTCNT = TOTCNT + 1
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C*  Get the next claim record.
     C*
     C                   READ      BADPV3
     C                   ENDDO
     C*
     C                   IF        @1ST or *INOF
     C                   MOVE      *OFF          @1ST
     C                   EXCEPT    HEAD
     C                   ENDIF
     C                   IF        TOTCNT <> *ZERO
     C                   EXCEPT    TOT1
     C                   ENDIF
     C                   MOVE      '1'           *INLR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                                           65 'Expense Reserve History'
     O                                         +001 'vs'
     O                                         +001 'Claim Incurred Expense'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                           10 'WTFIXD007B'
     O                                          125 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           2
     O                                            3 'FND'
     O                                           10 'YEAR'
     O                                           20 'CLAIM #'
     O                                           33 'RSV CODE'
     O                                           49 'RSV HISTORY'
     O                                           65 'CLM TOT INC'
     O                                           81 'DIFFERENCE'
     O          E            DET
     O                       CMFND         4      3
     O                       CMFYR         4      9
     O                       CMCASE        4     20
     O                       RT(X)               30
     O                       TR(X)         K     50
     O                       TC(X)         K     66
     O                       DF(X)         KB    82
     O          E            TOT1        2
     O                       TOTCNT        Z     30
     O                       TOTTR         K     50
     O                       TOTTC         K     66
     O                       TOTDIF        K     82
** RSV CHG DESC
ADJUSTED RESERVES
