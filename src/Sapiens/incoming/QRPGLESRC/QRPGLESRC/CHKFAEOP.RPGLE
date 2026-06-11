     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CHKFAEOP - PRINT EOMB IF EOAMT <> SUM OF CATOTP
     H*
     FFAEOL3    IF   E           K DISK
     FFDCAL5    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
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
     C                   Z-ADD     0             XXTOTP            9 2
     C     CAKY          CHAIN     PCA01                              61
     C     *IN61         DOWEQ     '0'
     C                   ADD       CATOTC        XXTOTC
     C                   ADD       CATOTP        XXTOTP
     C     CAKY          READE     PCA01                                  61
     C                   END
     C*
     C     XXEAMT        IFNE      XXTOTP
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
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
     O                                           94 'PAGE:'
     O                       PAGE          Z    102
     O          E            HEAD           2
     O                                           21 'THE EOMB PAYMENT'
     O                                           44 'DOES NOT EQUAL THE SUM'
     O                                           67 'OF THE AMOUNT PAID FOR'
     O                                           85 'THE CORRESPONDING'
     O                                           93 'CHARGES.'
     O          E            HEAD           1
     O                                           56 '----------'
     O                                           60 'EOMB'
     O                                           70 '----------'
     O                                           80 '------'
     O                                           94 'DETAIL CHARGES'
     O                                          100 '------'
     O          E            HEAD           2
     O                                            4 'CO#'
     O                                           10 'SUB#'
     O                                           15 'FND'
     O                                           20 'FYR'
     O                                           28 'CASE'
     O                                           39 'EOMB#'
     O                                           55 'CHARGED'
     O                                           70 'PAID'
     O                                           85 'CHARGED'
     O                                          100 'PAID'
     O          E            DET            1
     O                       EOCO#         4      4
     O                       EOFND         4      9
     O                       EOFYR         4     20
     O                       EOCASE        4     28
     O                       EOEOB#        4     39
     O                       EOAMTC        J     56
     O                       XXEAMT        J     71
     O                       XXTOTC        J     86
     O                       XXTOTP        J    101
