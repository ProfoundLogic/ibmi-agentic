      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD006D - Fix reserves for payment requests
      *****************************************************************
      *
      * DESCRIPTION: Fix reserves for payment requests                *
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
     FWARPL     IF   E           K DISK    USROPN
     FWMCML1    UF   E           K DISK    USROPN
     FWMRHL     O    E           K DISK    USROPN
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     D XXCO            S              3  0
     D XXFND           S                   LIKE(CMFND)
     D XXFYR           S                   LIKE(CMFYR)
     D XXCASE          S                   LIKE(CMCASE)
     D @1ST            S               N   INZ(*ON)
     D RQ              S              9  2 DIM(10)                              Request Payment
     D DF              S              9  2 DIM(10)
     D DS              S             40    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)                              Reserve Type
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
     D  RR                     1     90  2
     D                                     DIM(10)                              Remaining Reserve
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
     D  RC                     1     20
     D                                     DIM(10)                              Reserve Type
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
     D  SU                     1     90  2
     D                                     DIM(10)                              Remaining Reserve
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
     C     *ENTRY        PLIST
     C                   PARM                    @CO               3
     C                   PARM                    @FND              3
     C                   PARM                    @FYR              3
     C                   PARM                    @CASE             7
     C                   PARM                    QQUPD             1
     C                   PARM                    QQPRT             1
     C                   PARM                    QQEXIT            1
     C**************************************************************************
     C*  Key list for the Claim Master file
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
     C     RPKY          KLIST
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
     C                   MOVE      @CO           XXCO
     C                   MOVE      @FND          XXFND
     C                   MOVE      @FYR          XXFYR
     C                   MOVE      @CASE         XXCASE
     C                   SELECT
     C                   WHEN      QQEXIT = *ON
     C                   MOVE      '1'           *INLR

     C                   OTHER
     C                   OPEN      WARPL
     C                   OPEN      WMCML1
     C                   OPEN      WMRHL
     C                   Z-ADD     1             X                 3 0
     C                   IF        QQUPD = 'Y'
     C                   MOVE      *ON           *IN75
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C**************************************************************************
     C*  Loop through all Claim records.
     C*

     C     CMKY          CHAIN     WMCML1                             50
     C     *IN50         IFEQ      '0'
     C*
     C*  Accumulate the total of all released rqst pmt records.
     C*
     C                   CLEAR                   RQ
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C     RPSTAT        IFEQ      *BLANK
     C*
     C                   Z-ADD     1             X
     C     RPRECD        LOOKUP    RT(X)                                  60
     C   60              ADD       RPAMT         RQ(X)
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             X
     C     *BLANK        LOOKUP    RT(X)                                  60
     C   60              MOVEL     RPRECD        RT(X)
     C   60              ADD       RPAMT         RQ(X)
     C                   ENDIF
     C*
     C                   ENDIF
     C     RPKY          READE     PRP01                                  60
     C                   ENDDO
     C*
     C*  Determine if the reserves are sufficient.
     C*
     C                   MOVE      'N'           XXUPD             1
     C                   CLEAR                   DF
     C                   DO        10            X
     C*
     C     RR(X)         IFLT      RQ(X)
     C     RQ(X)         SUB       RR(X)         DF(X)
     C                   IF        DF(X) <> *ZERO
     C                   IF        QQPRT = 'Y'
     C                   IF        *INOA or @1ST
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          @1ST
     C                   ENDIF
     C                   EXCEPT    DET
     C                   MOVE      'Y'           XXUPD
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C     XXUPD         IFEQ      'Y'
     C     QQUPD         ANDEQ     'Y'
     C                   EXSR      SRUPD
     C                   ENDIF
     C*
     C*  Determine if the reserves are negative.
     C*
     C                   MOVE      'N'           XXUPD             1
     C                   CLEAR                   DF
     C                   DO        10            X
     C*
     C     RR(X)         IFLT      0
     C     RR(X)         MULT      -1            DF(X)
     C                   IF        QQPRT = 'Y'
     C                   IF        *INOA or @1ST
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          @1ST
     C                   ENDIF
     C                   EXCEPT    DET
     C                   ENDIF
     C                   MOVE      'Y'           XXUPD
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C     XXUPD         IFEQ      'Y'
     C     QQUPD         ANDEQ     'Y'
     C                   EXSR      SRUPD
     C                   ENDIF
     C*
     C*  Get the next claim record.
     C*
     C                   ENDIF
     C*
     C*  End of Pgm
     C*
     C                   CLOSE     WARPL
     C                   CLOSE     WMCML1
     C                   CLOSE     WMRHL
     C                   RETURN
     C                   ENDSL
     C**************************************************************************
     C*  SRUPD - This subroutine creates a Reserve History record for
     C*    claims which are out of balance.
     C**************************************************************************
     C     SRUPD         BEGSR
     C*
     C                   MOVE      'N'           XXUPD
     C                   CLEAR                   PRH01
     C                   Z-ADD     CMCO#         RHCO#
     C                   Z-ADD     CMFND         RHFND
     C                   Z-ADD     CMFYR         RHFYR
     C                   Z-ADD     CMCASE        RHCASE
     C*
     C                   DO        10            X
     C*
     C     DF(X)         IFNE      0
     C     RT(X)         ANDNE     *BLANKS
     C                   MOVE      RT(X)         RC(X)
     C                   Z-ADD     DF(X)         SU(X)
     C                   ADD       DF(X)         RR(X)
     C                   MOVE      'Y'           XXUPD
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   Z-ADD     XXDATE        RHEFDT
     C                   MOVEL(P)  DS(1)         RHEXPL
     C                   MOVE      *BLANKS       RHSTAT
     C                   MOVE      *BLANKS       RHCLST
     C                   MOVE      Q1USER        RHUSER
     C                   MOVE      Q1PGM         RHPGM
     C                   Z-ADD     XXDATE        RHDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RHTIME
     C*
     C     XXUPD         IFEQ      'Y'
     C                   WRITE     PRH01
     C                   UPDATE    PCM01
     C                   ENDIF
     C*
     C                   ENDSR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 02
     O                       DSTIME              08 '  :  :  '
     O                                           51 'This report prints claim'
     O                                           75 's with insufficient rese'
     O                                           99 'rves or negative reserve'
     O               75                        +001 ' - UPDATE'
     O                                          101 's.'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                           10 'WTFIXD006D'
     O                                          125 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           2
     O                                            3 'Grp'
     O                                           19 'FYR   Claim#'
     O                                           30 'Rsv Code'
     O                                           46 'Remaining Rsv'
     O                                           63 'Rqst Pmt Total'
     O          E            DET            1
     O                       CMFND         4      3
     O                       CMFYR         4      9
     O                       CMCASE        4     19
     O                       RT(X)               27
     O                       RR(X)         L     44
     O                       RQ(X)         L     60
