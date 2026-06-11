      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD006B - Adjust reserve history.
      *****************************************************************
      *
      * DESCRIPTION:  This pgm identifies  discrepancies
      *   between the claim incurred amount and reserve history, then
      *   writes the difference to reserve history file.
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
      *
     FWMRHL     IF A E           K DISK    USROPN
     FWMCML1    IF   E           K DISK    USROPN
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D XXCO            S              3  0
     D XXFND           S                   LIKE(CMFND)
     D XXFYR           S                   LIKE(CMFYR)
     D XXCASE          S                   LIKE(CMCASE)
     D @1ST            S               N   INZ(*ON)
     D QQPRT           S              1

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
     C                   KFLD                    XXCO
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE

     C     *ENTRY        PLIST
     C                   PARM                    @CO               3
     C                   PARM                    @FND              3
     C                   PARM                    @FYR              3
     C                   PARM                    @CASE             7
     C                   PARM                    QQUPD             1
     C                   PARM                    QQPRT             1
     C                   PARM                    QQEXIT            1

     C                   SELECT
     C                   WHEN      QQEXIT = *ON
     C                   MOVE      *ON           *INLR

     C                   OTHER
     C                   OPEN      WMRHL
     C                   OPEN      WMCML1
     C                   MOVE      @CO           XXCO
     C                   MOVE      @FND          XXFND
     C                   MOVE      @FYR          XXFYR
     C                   MOVE      @CASE         XXCASE

     C                   IF        QQUPD = 'Y'
     C                   MOVE      *ON           *IN75
     C                   ELSE
     C                   MOVE      *OFF          *IN75
     C                   ENDIF

     C                   Z-ADD     0             X                 3 0
     C                   Z-ADD     0             Z                 3 0
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C**************************************************************************
     C*  Start reading the claim records.
     C*
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
     C     RR(X)         ADD       AR(X)         TC(X)
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
     C                   MOVE      'N'           XXUPD             1
     C                   DO        10            X
     C*
     C     TC(X)         SUB       TR(X)         DF(X)
     C                   IF        DF(X) <> *ZERO  and QQPRT ='Y'
     C                   MOVE      'Y'           XXUPD
     C                   IF        @1ST or *INOF
     C                   MOVE      *OFF          @1ST
     C                   EXCEPT    HEAD
     C                   ENDIF
     C                   EXCEPT    DET
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C     XXUPD         IFEQ      'Y'
     C     QQUPD         ANDEQ     'Y'
     C                   EXSR      SRUPD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   CLOSE     WMRHL
     C                   CLOSE     WMCML1
     C*
     C                   RETURN
     C                   ENDSL
     C**************************************************************************
     C*  SRUPD - This subroutine creates a Reserve History record for
     C*    claims which are out of balance.
     C**************************************************************************
     C     SRUPD         BEGSR
     C*
     C                   CLEAR                   PRH01
     C                   Z-ADD     CMCO#         RHCO#
     C                   Z-ADD     CMFND         RHFND
     C                   Z-ADD     CMFYR         RHFYR
     C                   Z-ADD     CMCASE        RHCASE
     C*
     C                   DO        10            X
     C*
     C     TC(X)         SUB       TR(X)         DF(X)
     C     DF(X)         IFNE      0
     C                   MOVE(P)   RT(X)         RC(X)
     C                   Z-ADD     DF(X)         SU(X)
     C                   ENDIF
     C*
     C                   ENDDO

     C                   Z-ADD     XXDATE        RHEFDT

     C                   MOVEL(P)  DS(1)         RHEXPL
     C                   MOVE      *BLANKS       RHSTAT
     C                   MOVE      CMSTAT        RHCLST
     C                   MOVE      Q1USER        RHUSER
     C                   MOVE      Q1PGM         RHPGM
     C                   Z-ADD     XXDATE        RHDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RHTIME
     C*
     C                   WRITE     PRH01
     C*
     C                   ENDSR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                                           65 'Expense Reserve History'
     O                                         +001 'vs'
     O                                         +001 'Claim Incurred Expense'
     O               75                        +001 '- UPDATE'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                           10 'WTFIXD006B'
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
     O                       TR(X)               50 '       .  -'
     O                       TC(X)               66 '       .  -'
     O                       DF(X)               82 '       .  -'
** RSV CHG DESC
ADJUSTED RESERVES
