     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXRSVBAL2 - Fix Claim Payment Amounts
     H*****************************************************************
     H* Description:  This pgm identifies closed claims with reserves *
     H*   and determines if the reserves should be reduced to 0.  Only*
     H*   claims with request pmts should have reserves.              *
     H*                                                               *
     H*****************************************************************
     H*
     FWMCML1    UF   E           K DISK
     FWARPL     IF   E           K DISK
     FWMRHL     O    E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D TC              S              2    DIM(10)
     D TR              S              9  2 DIM(10)
     D EXPL            S             40    DIM(1) CTDATA PERRCD(1)
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
     D  SC                     1     20
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
     D  SR                     1     90  2
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
     C     *LIKE         DEFINE    CMRR01        XXRR
     C     *LIKE         DEFINE    CMRR01        XXTR
     C                   Z-ADD     0             X                 3 0
     C                   Z-ADD     0             Z                 3 0
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C                   EXCEPT    HEAD
     C**************************************************************************
     C*  Start reading the claim records.
     C*
     C                   READ      PCM01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C                   XFOOT     RR            XXRR
     C     CMSTAT        IFEQ      'C'
     C     XXRR          ANDGT     0
     C*
     C*  Accumulate the amount of any released payments
     C*
     C                   CLEAR                   TC
     C                   CLEAR                   TR
     C     XXKY          CHAIN     PRP01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     RPSTAT        IFEQ      ' '
     C*
     C                   Z-ADD     1             X
     C     RPRECD        LOOKUP    TC(X)                                  61
     C     *IN61         IFEQ      *ON
     C                   ADD       RPAMT         TR(X)
     C                   ELSE
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    TC(X)                                  61
     C   61              MOVE      RPRECD        TC(X)
     C   61              Z-ADD     RPAMT         TR(X)
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     XXKY          READE     PRP01                                  60
     C                   ENDDO
     C*
     C*  If the remaining reserve amount is not equal to the total of
     C*    released pmts, the reserves on this claim are incorrect.
     C*
     C                   XFOOT     TR            XXTR
     C     XXRR          IFNE      XXTR
     C*
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C                   EXSR      SRUPD
     C*
     C                   ENDIF
     C*
     C*  Get the next claim record.
     C*
     C                   ENDIF
     C                   READ      PCM01                                  50
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
     C**************************************************************************
     C*  SRUPD - This subroutine reduces the reserve to 0 and creates a
     C*    reserve history record.
     C**************************************************************************
     C     SRUPD         BEGSR
     C*
     C                   CLEAR                   PRH01
     C*
     C*  Loop thru reserves and correct as needed.
     C*
     C                   DO        10            X
     C*
     C     RC(X)         IFNE      *BLANKS
     C*
     C                   Z-ADD     1             Z
     C     RC(X)         LOOKUP    TC(Z)                                  61
     C   61              Z-ADD     TR(Z)         XXTR
     C  N61              Z-ADD     0             XXTR
     C     RR(X)         IFNE      XXTR
     C                   MOVE      RC(X)         SC(X)
     C     XXTR          SUB       RR(X)         SR(X)
     C                   Z-ADD     XXTR          RR(X)
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C*  Update the Claim Master file
     C*
     C                   MOVE      Q1USER        CMUSER
     C                   MOVE      Q1PGM         CMPGM
     C                   Z-ADD     XXDATE        CMDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CMTIME
     C                   UPDATE    PCM01
     C*
     C                   Z-ADD     CMCO#         RHCO#
     C                   Z-ADD     CMFND         RHFND
     C                   Z-ADD     CMFYR         RHFYR
     C                   Z-ADD     CMCASE        RHCASE
     C                   Z-ADD     XXDATE        RHEFDT
     C                   MOVEL     EXPL(1)       RHEXPL
     C                   MOVE      *BLANKS       RHSTAT
     C                   MOVE      *BLANKS       RHCLST
     C                   MOVE      Q1USER        RHUSER
     C                   MOVE      Q1PGM         RHPGM
     C                   Z-ADD     XXDATE        RHDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RHTIME
     C                   WRITE     PRH01
     C*
     C                   ENDSR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                                           71 'Reserves on Closed'
     O                                           78 'Claims'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                           10 'FIXRSVBAL3'
     O                                          125 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           2
     O                                            3 'FND'
     O                                           10 'YEAR'
     O                                           20 'CLAIM #'
     O                                           27 'CODE'
     O                                           40 'RSV AMOUNT'
     O                                           47 'CODE'
     O                                           60 'RSV AMOUNT'
     O                                           67 'CODE'
     O                                           80 'RSV AMOUNT'
     O                                           87 'CODE'
     O                                          100 'RSV AMOUNT'
     O          E            DET
     O                       CMFND         4      3
     O                       CMFYR         4      9
     O                       CMCASE        4     20
     O                       CMRT01              26
     O                       CMRR01              41 '       .  -'
     O                       CMRT02              46
     O                       CMRR02              61 '       .  -'
     O                       CMRT03              66
     O                       CMRR03              81 '       .  -'
     O                       CMRT04              86
     O                       CMRR04             101 '       .  -'
** Reserve change description
Reduced reserves for closed claim
