      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD006C - Fix released payments
      *****************************************************************
      *
      * DESCRIPTION:  This pgm identifies  discrepancies
      *   between the claim released amount and
      *   the actual expense payments , then updates the expense released am
      *   ount to match the actual payments.
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
     FWDPAL     IF   E           K DISK    USROPN
     FWMCML1    UF   E           K DISK    USROPN
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D XXCO            S              3  0
     D XXFND           S                   LIKE(CMFND)
     D XXFYR           S                   LIKE(CMFYR)
     D XXCASE          S                   LIKE(CMCASE)
     D @1ST            S               N   INZ(*ON)

     D TA              S              9  2 DIM(10)
     D TR              S              9  2 DIM(10)
     D DF              S              9  2 DIM(10)
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

     C     XXKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE

     C     *ENTRY        PLIST
     C                   PARM                    @CO               3
     C                   PARM                    @FND              3
     C                   PARM                    @FYR              3
     C                   PARM                    @CASE             7
     C                   PARM                    QQUPD             1
     C                   PARM                    QQPRT             1
     C                   PARM                    QQEXIT            1
     C*
     C                   SELECT
     C                   WHEN      QQEXIT = *ON
     C                   MOVE      '1'           *INLR

     C                   OTHER
     C                   OPEN      WDPAL
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
     C                   CLEAR                   TR
     C*
     C*  Load the claim total released payments.
     C*
     C                   DO        10            X
     C                   ADD       AR(X)         TR(X)
     C                   ENDDO
      *
      *  Accumulate the amount of actual payments
      *
     C                   CLEAR                   TA
     C     XXKY          CHAIN     PPA01                              60
     C     *IN60         DOWEQ     *OFF
      *
     C                   Z-ADD     1             X
     C     PARECD        LOOKUP    RT(X)                                  61
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
     C     *BLANKS       LOOKUP    RT(X)                                  61
     C   61              MOVE      PARECD        RT(X)
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
     C*  Determine if the reserves are out of balance.  If so, create
     C*    a resrve history entry.
     C*
     C*  Check total reserves to total incurred.
     C*
     C                   MOVE      'N'           XXUPD             1
     C                   DO        10            X
     C*
     C     TA(X)         SUB       TR(X)         DF(X)
     C                   IF        DF(X) <> *ZERO  AND QQPRT ='Y'
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
     C*  Get the next claim record.
     C*

     C*
     C                   CLOSE     WDPAL
     C                   CLOSE     WMCML1
     C                   RETURN
     C                   ENDSL
     C**************************************************************************
     C*  SRUPD - This subroutine updates released payments for
     C*    claims that are out of balance.
     C**************************************************************************
     C     SRUPD         BEGSR
     C*
     C*
     C                   DO        10            X
     C*
     C     TA(X)         SUB       TR(X)         DF(X)
     C     DF(X)         IFNE      0
     C                   ADD       DF(X)         AR(X)
     C                   ENDIF
     C                   ENDDO

     C                   UPDATE    PCM01
     C                   ENDSR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                                           65 'Expense Released payments'
     O                                         +001 'vs'
     O                                         +001 'Actual payments'
     O               75                        +001 '- UPDATE'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                           10 'WTFIXD006C'
     O                                          125 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           2
     O                                            3 'FND'
     O                                           10 'YEAR'
     O                                           20 'CLAIM #'
     O                                           33 'RSV CODE'
     O                                           49 'ACTUAL PAYM'
     O                                           65 'RELSED PAYM'
     O                                           81 'DIFFERENCE'
     O          E            DET
     O                       CMFND         4      3
     O                       CMFYR         4      9
     O                       CMCASE        4     20
     O                       RT(X)               30
     O                       TA(X)               50 '       .  -'
     O                       TR(X)               66 '       .  -'
     O                       DF(X)               82 '       .  -'
