     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXCLRES1 - DETERNMINE CLAIMS NOT EQUAL TO TOTAL INC.
     H*
     H***********************************************************
     H* REVISIONS:
     H*   C:58 6/10/00  PROJECT: WC993
     H*                 Select only non-test groups.
     H*
     H*   12-28-92   003   CHANGE PROGRAM TO ACCEPT ENTRY PARMS
     H*                    FOR CO#  AND FND#
     H***********************************************************
     H*
     FWMRHL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F**** BEGIN ADD *** 06/10/00 ************************************
     F*
     F* Group Master
     FWMFNL     IF   E           K DISK
     F**** END ADD ***** 06/10/00 ************************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
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

     C*
     C     RHKEY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C**** BEGIN ADD *** 06/10/00 ************************************
     C*
     C*  Fund master
     C     FNKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C*
     C     *LIKE         DEFINE    CMCO#         SVCO#
     C     *LIKE         DEFINE    CMFND         SVFND
     C**** END ADD ***** 06/10/00 ************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C*
     C*
     C                   SETON                                        OF
     C     CMKEY         CHAIN     WMCML1                             50
     C     *IN50         DOWEQ     '0'
     C*
     C**** BEGIN DEL *** 06/10/00 ************************************
     C********** CMFND     IFNE 999
     C**** END DEL ***** 06/10/00 ************************************
     C**** BEGIN ADD *** 06/10/00 ************************************
     C     CMCO#         IFNE      SVCO#
     C     CMFND         ORNE      SVFND
     C     FNKY          CHAIN     PFN01                              63
     C     *IN63         IFEQ      *ON
     C                   MOVEL     'Y'           FNTEST                          FORCE SKIP
     C                   ENDIF                                                  .63 = ON
     C                   Z-ADD     CMCO#         SVCO#
     C                   Z-ADD     CMFND         SVFND
     C                   ENDIF                                                  .CO/FND <>
     C*
     C     FNTEST        IFNE      'Y'
     C**** END ADD ***** 06/10/00 ************************************
     C*
     C                   XFOOT     RR            XXOUTS            9 2
     C                   XFOOT     PR            XXOUT1            9 2
     C     XXOUT1        ADD       XXOUTS        XXOUTS
     C*
     C     XXOUTS        IFNE      0
     C                   MOVE      *ZERO         SU
     C                   Z-ADD     0             XXOUT2
     C                   Z-ADD     0             XXOUT3
     C     RHKEY         CHAIN     PRH01                              60
     C     *IN60         DOWEQ     '0'
     C                   XFOOT     SU            XXOUT2            9 2
     C     XXOUT2        ADD       XXOUT3        XXOUT3            9 2
     C     RHKEY         READE     PRH01                                  60
     C                   END
     C     XXOUT3        IFNE      XXOUTS
     C   OF              EXCEPT    HEAD
     C                   SETOFF                                           OF
     C                   EXCEPT    DET
     C                   END
     C*
     C                   END
     C                   END
     C*
     C     CMKEY         READE     WMCML1                                 50
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     OQSYSPRT   E            HEAD           1 06
     O                                           56 'ALL CLAIMS, ALL FUNDS'
     O          E            HEAD           2
     O                                            8 'FND'
     O                                           18 'YEAR'
     O                                           30 'CASE NUMBER'
     O                                           45 'RES HST'
     O                                           60 'CLAIM MAST'
     O          E            DET
     O                       CMFND         Z      7
     O                       CMFYR         Z     17
     O                       CMCASE              30
     O                       XXOUT3              45 '         .  -'
     O                       XXOUTS              60 '         .  -'
