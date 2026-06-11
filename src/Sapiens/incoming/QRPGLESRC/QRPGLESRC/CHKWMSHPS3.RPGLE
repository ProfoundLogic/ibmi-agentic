     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CHKWMSHPST - CHECK IF STATE INFORMATION MATCHES POLICY RCDND   R
     H*****************************************************************
     H*                                                               *
     H*   PROGRAMMER: C:65                                            *
     H*   CREATED:    07/14/04    PROJECT: SUPPORT#42969              *
     H*                                                               *
     H*****************************************************************
     FWMSHP     IF   E             DISK
     FWMSHL1    UF   E           K DISK
     F                                     RENAME(PSH01:PSH02)
     FWMEML     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  SST                    1     30
     D                                     DIM(15)
     D  SHST01                 1      2
     D  SHST02                 3      4
     D  SHST03                 5      6
     D  SHST04                 7      8
     D  SHST05                 9     10
     D  SHST06                11     12
     D  SHST07                13     14
     D  SHST08                15     16
     D  SHST09                17     18
     D  SHST10                19     20
     D  SHST11                21     22
     D  SHST12                23     24
     D  SHST13                25     26
     D  SHST14                27     28
     D  SHST15                29     30
     D                 DS
     D  SSS                    1     15
     D                                     DIM(15)
     D  SHSS01                 1      1
     D  SHSS02                 2      2
     D  SHSS03                 3      3
     D  SHSS04                 4      4
     D  SHSS05                 5      5
     D  SHSS06                 6      6
     D  SHSS07                 7      7
     D  SHSS08                 8      8
     D  SHSS09                 9      9
     D  SHSS10                10     10
     D  SHSS11                11     11
     D  SHSS12                12     12
     D  SHSS13                13     13
     D  SHSS14                14     14
     D  SHSS15                15     15

     C*
     C*  KEY LIST FOR SUBMISSION HISTORY FILE
     C*
     C     SHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C*
     C*  KEY LIST FOR EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     *LIKE         DEFINE    SHCO#         XXCO#
     C     *LIKE         DEFINE    SHFND         XXFND
     C     *LIKE         DEFINE    SHEMP#        XXEMP#
     C     *LIKE         DEFINE    SHFYR         XXFYR
     C     *LIKE         DEFINE    SHDIV         XXDIV
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQUPD             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     QQUPD         IFEQ      'Y'
     C                   MOVE      *ON           *IN66
     C                   ELSE
     C                   MOVE      *OFF          *IN66
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*
     C                   READ      PSH01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C     SHTYPE        IFEQ      'P'
     C     SHSBDT        ANDNE     *ZERO
     C*
     C                   MOVEA     SST           XXSST            30
     C                   MOVEL     XXSST         XXSSTP           15
     C                   Z-ADD     SHCO#         XXCO#
     C                   Z-ADD     SHFND         XXFND
     C                   Z-ADD     SHEMP#        XXEMP#
     C                   Z-ADD     SHFYR         XXFYR
     C                   Z-ADD     SHDIV         XXDIV
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     EMCAD1        ANDNE     *ZERO
     C*
     C                   Z-ADD     SHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C                   Z-ADD     SHSBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXSBDT            8 0
     C*
     C                   MOVE      *BLANK        XXSUB             1
     C*
     C     SHKY          CHAIN     PSH02                              50
     C     *IN50         DOWEQ     *OFF
     C*
     C     SHTYPE        IFEQ      'C'
     C     SHSBDT        ANDNE     0
     C*
     C                   MOVE      'Y'           XXSUB
     C*
     C                   ENDIF                                                  SHSBDT IFNE
     C*
     C     SHKY          READE     PSH02                                  50
     C                   ENDDO
     C*
     C     XXSUB         IFEQ      *BLANK
     C*
     C     SHKY          CHAIN     PSH02                              50
     C     *IN50         DOWEQ     *OFF
     C*
     C     SHTYPE        IFEQ      'C'
     C*
     C     SHSBDT        IFEQ      0
     C     SHSTAT        ANDLT     '4'
     C                   MOVEA     SST           X1SST            30
     C                   MOVEL     X1SST         X1SSTP           15
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C                   Z-ADD     SHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XSTRDT            8 0
     C                   Z-ADD     SHSBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XSSBDT            8 0
     C*
     C                   EXCEPT    DET
     C*
     C     *IN66         IFEQ      *ON
     C*
     C                   MOVEA     XXSST         SST
     C*
     C     1             DO        15            I                 2 0
     C     SST(I)        IFNE      *BLANKS
     C                   MOVE      '3'           SSS(I)
     C                   ENDIF
     C                   ENDDO
     C*
     C                   Z-ADD     0             SHSBDT
     C                   MOVE      '3'           SHSTAT
     C                   MOVEL(P)  Q1PGM         SHPGM
     C                   UPDATE    PSH02
     C                   ENDIF                                                  *IN66 IFEQ
     C*
     C                   ENDIF                                                  SHSBDT IFEQ
     C*
     C                   ENDIF                                                  SHTYPE IFNE
     C*
     C     SHKY          READE     PSH02                                  50
     C                   ENDDO
     C*
     C                   ENDIF                                                  XXSUB IFEQ
     C*
     C                   ENDIF                                                  EMCAD1 IFNE
     C*
     C                   ENDIF                                                  SHTYPE IFEQ
     C*
     C                   READ      PSH01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C*
     C                   MOVE      *ON           *INLR
     C*
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'PROGRAM NAME:'
     O                                           26 'CHKWMSHPST'
     O                                           70 'RUN DATE:'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                                           24 'PGM TO CHECK IF STATE '
     O                                           44 'LIST IS NOT CORRECT'
     O               66                          73 'UPDATE MODE IS ACTIVE'
     O          E            HEAD           2
     O                                            5 'CO#'
     O                                           11 'FUND'
     O                                           16 'FYR'
     O                                           27 'EMPLOYER#'
     O                                           34 'DIV'
     O                                           41 'TYPE'
     O                                           46 'STAT'
     O                                           57 'P_TRDT'
     O                                           68 'X_TRDT'
     O                                           79 'P_SBDT'
     O                                           90 'X_SBDT'
     O                                           98 'P_SST'
     O                                          116 'X_SST'
     O          E            DET            1
     O                       SHCO#         4      5
     O                       SHFND         4     11
     O                       SHFYR         4     16
     O                       SHEMP#        4     27
     O                       SHDIV         4     34
     O                       SHTYPE              39
     O                       SHSTAT              44
     O                       XXTRDT              57 '  /  /    '
     O                       XSTRDT              68 '  /  /    '
     O                       XXSBDT              79 '  /  /    '
     O                       XSSBDT              90 '  /  /    '
     O                       XXSSTP             108
     O                       X1SSTP             126
