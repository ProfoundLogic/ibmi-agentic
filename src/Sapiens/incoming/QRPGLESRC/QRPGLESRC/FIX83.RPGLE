     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIX82 - Check claim payment amounts
     H*
     H* SAME AS FIX79B, BUT THIS ONE DOES NOT UPDATE
     H*
     H*****************************************************************
     H* Description:  This pgm identifies payment discrepancies
     H*   between the payment file and the payments in the claim master
     H*   file.
     H*
     H*****************************************************************
     H*
     FWMCML1    IF   E           K DISK
     FWDPAL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D TR              S              9  2 DIM(10)
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
     D  RA                     1     90  2
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
     C     CMSTAT        IFNE      'C'
     C***        XXRR      ANDGT0
     C*
     C*  Accumulate the amount of any released payments
     C*
     C                   CLEAR                   TR
     C     XXKY          CHAIN     PPA01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     PASTAT        IFNE      '@'
     C*
      *
     C                   Z-ADD     1             X
     C     PARECD        LOOKUP    RC(X)                                  61
     C     *IN61         IFEQ      *ON
     C****                 ADD  PAAMT     TR,X
     C**** 10/03/11 ****
     C     PATYPE        IFEQ      'A'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'P'
     C     PATYPE        OREQ      'Z'
     C                   ADD       PAAMT         TR(X)
     C                   ELSE
     C                   SUB       PAAMT         TR(X)
     C                   ENDIF
     C**** 10/03/11 ****
     C                   ELSE
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    RC(X)                                  61
     C   61              MOVE      PARECD        RC(X)
     C** 61                Z-ADDPAAMT     TR,X
     C**** 10/03/11 ****
     C     *IN61         IFEQ      *ON
     C     PATYPE        IFEQ      'A'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'P'
     C     PATYPE        OREQ      'Z'
     C                   Z-ADD     PAAMT         TR(X)
     C                   ELSE
     C                   Z-SUB     PAAMT         TR(X)
     C                   ENDIF
     C                   ENDIF
     C**** 10/03/11 ****
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     XXKY          READE     PPA01                                  60
     C                   ENDDO
     C*
     C*  If the remaining reserve amount is not equal to the total of
     C*    released pmts, the reserves on this claim are incorrect.
     C*
     C                   XFOOT     TR            XXTR
      *
     C                   DO        10            IJ                3 0
     C     TR(IJ)        IFNE      RR(IJ)
     C                   MOVE      *ZEROS        DIFF              9 2
     C     TR(IJ)        SUB       RR(IJ)        DIFF
     C*
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C*
     C                   ENDIF
     C                   ENDDO
     C*
     C*  Get the next claim record.
     C*
     C                   ENDIF
     C                   READ      PCM01                                  50
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                                           71 'Reserves & Payments'
     O                                         +001 'out of balance'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                           10 'FIX79B'
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
     O                                           85 'REMAIN RSV'
     O          E            DET
     O                       CMFND         4      3
     O                       CMFYR         4      9
     O                       CMCASE        4     20
     O                       RC(IJ)              26
     O                       RR(IJ)              41 '       .  -'
     O                       TR(IJ)              61 '       .  -'
     O                       DIFF           B    73 '       .  -'
     O                       RA(IJ)              85 '       .  -'
