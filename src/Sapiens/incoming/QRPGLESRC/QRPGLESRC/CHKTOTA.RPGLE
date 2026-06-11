     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CHKFAEOP - PRINT EOMB IF EOAMT <> SUM OF CATOTA
     H*
     FFAEOL3    IF   E           K DISK
     FFDCAL5    IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C     CAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*****************************************************************
     C                   READ      PEO01                                  60
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     0             XXEAMT            9 2
     C     EOAMT         SUB       EONDIS        XXEAMT
     C*
     C                   Z-ADD     0             XXTOTC            9 2
     C                   Z-ADD     0             XXTOTA            9 2
     C                   Z-ADD     0             XXTOTD            9 2
     C                   Z-ADD     0             XXOTHR            9 2
     C                   Z-ADD     0             XXNDIS            9 2
     C     CAKY          CHAIN     PCA01                              61
     C     *IN61         DOWEQ     '0'
     C                   ADD       CATOTC        XXTOTC
     C                   ADD       CATOTA        XXTOTA
     C                   ADD       CATOTD        XXTOTD
     C                   ADD       CAOTHR        XXOTHR
     C                   ADD       CANDIS        XXNDIS
     C     CAKY          READE     PCA01                                  61
     C                   END
     C*
     C     EOEOB#        IFGT      1000000
     C     XXEAMT        IFNE      XXTOTA
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   END
     C                   END
     C*
     C                   READ      PEO01                                  60
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
     O                                           21 'THE EOMB PAYMENT'
     O                                           44 'DOES NOT EQUAL THE SUM'
     O                                           67 'OF THE AMOUNT PAID FOR'
     O                                           85 'THE CORRESPONDING'
     O                                           94 'CHARGES.'
     O          E            HEAD           1
     O                                           57 '----------------'
     O                                           62 'EOMB'
     O                                           78 '-----------------'
     O                                          110 '-----------------------'
     O                                          125 'DETAIL CHARGES'
     O                                          150 '------------------------'
     O          E            HEAD           1
     O                                           52 'RECOMMENDED'
     O                                           65 'NETWORK'
     O                                           93 'TOTAL'
     O                                          105 'TOTAL'
     O                                          119 'OTHER'
     O                                          135 'NETWORK'
     O          E            HEAD           2
     O                                            4 'CO#'
     O                                           10 'SUB#'
     O                                           15 'FND'
     O                                           20 'FYR'
     O                                           28 'CASE'
     O                                           39 'EOMB#'
     O                                           51 'ALLOWANCE'
     O                                           66 'DISCOUNT'
     O                                           94 'CHARGES'
     O                                          108 'DIS-ALLOWED'
     O                                          122 'REDUCTIONS'
     O                                          136 'DISCOUNT'
     O                                          150 'TOTAL PAID'
     O          E            DET            1
     O                       EOCO#         4      4
     O                       EOSUB#        4      9
     O                       EOFND         4     15
     O                       EOFYR         4     20
     O                       EOCASE        4     28
     O                       EOEOB#        4     39
     O                       EOAMT         J     53
     O                       EONDIS        J     67
     O                       XXTOTC        J     95
     O                       XXTOTD        J    109
     O                       XXOTHR        J    123
     O                       XXNDIS        J    137
     O                       XXTOTA        J    151
