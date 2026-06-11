     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CHKNEWPAY- CHECK FOR NEGATIVE RESERVES
     H*
     FWMCML1    IF   E           K DISK
     FQSYSPRT   O    F   80        PRINTER OFLIND(*INOF)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSDATE                 7     12  0
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

     C*
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*****************************************************************
     C                   READ      PCM01                                  61
     C     *IN61         DOWEQ     '0'
     C*
     C                   DO        10            X                 2 0
     C     AR(X)         IFLT      0
     C                   MOVE      RT(X)         XXRECD            2
     C                   Z-ADD     AR(X)         XXAR              9 2
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   END
     C                   END
     C*
     C                   READ      PCM01                                  61
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'PROGRAM NAME:'
     O                                           25 'CHKNEGRSV'
     O                                           70 'RUN DATE:'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                                           21 'THIS PROGRAM CHECKS'
     O                                           43 'FOR CLAIMS WHICH HAVE'
     O                                           62 'NEGATIVE RESERVES.'
     O          E            HEAD           2
     O                                            5 'FND'
     O                                           10 'FYR'
     O                                           18 'CASE #'
     O                                           32 'RSV CD'
     O                                           50 'RESERVES'
     O          E            DET            1
     O                       CMFND         4      5
     O                       CMFYR         4     10
     O                       CMCASE        4     18
     O                       XXRECD              30
     O                       XXAR          J     51
