     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CMPRESBAL - COMPARES RESERVE HISTORY TO CLAIM TOT INC.
     H*****************************************************************
     H* Description:  This pgm prints a list of claims which have a   *
     H*   discrepancy between the Reserve History file and the claim's*
     H*   total incurred.                                             *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*   C:58 6/10/00  PROJECT: WC993
     H*                 Use fund master flag to select non-test groups.
     H*****************************************************************
     H*
     FWMRHL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FSMCOL     IF   E           K DISK
     F**** BEGIN ADD *** 06/10/00 ************************************
     FWMFNL     IF   E           K DISK
     F**** END ADD ***** 06/10/00 ************************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

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
     D  PR                     1     90  2
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
     C*  Key list for the Company Master file
     C*
     C     COKY          KLIST
     C                   KFLD                    CMCO#
     C*
     C*  Key list for the Reserve History file
     C*
     C     RHKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C**** BEGIN ADD *** 06/10/00 ************************************
     C*
     C*  Key list for the Fund Master file
     C*
     C     FNKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C*
     C     *LIKE         DEFINE    CMFND         SVFND
     C**** END ADD ***** 06/10/00 ************************************
     C*
     C                   TIME                    DSDTTM
     C**************************************************************************
     C*  Start of Main loop
     C*
     C                   READ      PCM01                                  50
     C*
     C  N50COKY          CHAIN     PCO01                              60
     C                   EXCEPT    HEAD
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C**** BEGIN CHG *** 06/10/00 ************************************
     C********** CMFND     IFNE 999
     C*
     C     SVFND         IFNE      CMFND
     C     FNKY          CHAIN     PFN01                              67
     C     *IN67         IFEQ      *ON
     C                   MOVE      'Y'           FNTEST
     C                   ENDIF
     C                   Z-ADD     CMFND         SVFND
     C                   ENDIF
     C     FNTEST        IFNE      'Y'
     C**** END CHANGE ** 06/10/00 ************************************
     C*
     C                   XFOOT     RR            XXOUTS            9 2
     C                   XFOOT     PR            XXOUT1            9 2
     C                   ADD       XXOUT1        XXOUTS
     C*
     C                   MOVE      *ZERO         SU
     C                   Z-ADD     0             XXOUT2
     C                   Z-ADD     0             XXOUT3
     C*
     C     RHKY          CHAIN     PRH01                              60
     C     *IN60         DOWEQ     '0'
     C                   XFOOT     SU            XXOUT2            9 2
     C     XXOUT2        ADD       XXOUT3        XXOUT3            9 2
     C     RHKY          READE     PRH01                                  60
     C                   END
     C*
     C     XXOUT3        IFNE      XXOUTS
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       XXOUT3        XTOUT3           11 2
     C                   ADD       XXOUTS        XTOUTS           11 2
     C                   ADD       1             XTCASE            9 0
     C                   END
     C*
     C                   END
     C                   READ      PCM01                                  50
     C                   END
     C*
     C                   EXCEPT    TOT
     C                   MOVE      '1'           *INLR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              96
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           76 'COMPARE WMCMP & WMRHP'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            3 'FND'
     O                                            8 'FYR'
     O                                           17 'CASE'
     O                                           33 'RES HST AMT'
     O                                           50 'TOTAL INC'
     O          E            DET            1
     O                       CMFND         4      3
     O                       CMFYR         4      8
     O                       CMCASE        4     17
     O                       XXOUT3        J     34
     O                       XXOUTS        J     51
     O          E            TOT         2
     O                                            7 'TOTALS:'
     O                       XTCASE        4     17
     O                       XTOUT3        J     34
     O                       XTOUTS        J     51
