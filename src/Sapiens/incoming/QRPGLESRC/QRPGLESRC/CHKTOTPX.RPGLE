     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CHKFDCAP - PRINT IF CATOTP DOES NOT XFOOT
     H*
     FFDCAL5    IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*****************************************************************
     C                   READ      PCA01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     CAEOB#        IFGT      1000000
     C*
     C                   Z-ADD     0             XXTOTP            9 2
     C     CATOTC        SUB       CAOTHR        XXTOTP
     C                   SUB       CATOTD        XXTOTP
     C                   SUB       CANDIS        XXTOTP
     C*
     C     CATOTP        IFNE      XXTOTP
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   END
     C*
     C                   END
     C*
     C                   READ      PCA01                                  60
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                       DSDATE        Y      9
     O                       DSTIME              19 '  :  :  '
     O                                          142 'PAGE:'
     O                       PAGE          Z    150
     O          E            HEAD           2
     O                                           21 'DETAIL CHARGE   '
     O                                           44 'CATOTP DOES NOT XFOOT '
     O                                           67 'TO CATOTC - CAOTHR -  '
     O                                           85 'CATOTD - CANDIS  '
     O          E            HEAD           2
     O                                            4 'CO#'
     O                                           10 'SUB#'
     O                                           15 'FND'
     O                                           20 'FYR'
     O                                           28 'CASE'
     O                                           39 'EOMB#'
     O                                           51 'TOTAL CHG'
     O                                           65 'OTHER DIS'
     O                                           82 'TTL DISALLOWED'
     O                                           93 'NTWK DIS'
     O                                          108 'CALC TOT PD'
     O                                          122 'CATOTP    '
     O                                          136 'CATOTA  '
     O          E            DET            1
     O                       CACO#         4      4
     O                       CASUB#        4      9
     O                       CAFND         4     15
     O                       CAFYR         4     20
     O                       CACASE        4     28
     O                       CAEOB#        4     39
     O                       CATOTC        J     53
     O                       CAOTHR        J     67
     O                       CATOTD        J     81
     O                       CANDIS        J     95
     O                       XXTOTP        J    109
     O                       CATOTP        J    123
     O                       CATOTA        J    137
