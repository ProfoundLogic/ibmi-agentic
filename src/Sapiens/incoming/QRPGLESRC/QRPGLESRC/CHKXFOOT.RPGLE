     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CHKXFOOT - VERIFY ALL PAYMENT/REDUCT FIELDS EQUAL CHARGED
     FFDCALB    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     *ZEROS        COUNT             9 0
     C*
     C                   READ      PCA01                                  60
     C  N60              EXCEPT    HEAD
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     *ZEROS        LINTOT            9 2
     C*
     C     CATOTA        IFEQ      CATOTP
     C*
     C     CATOTA        ADD       CATOTD        LINTOT
     C     LINTOT        ADD       CANDIS        LINTOT
     C     LINTOT        ADD       CAOTHR        LINTOT
     C*
     C     CATOTC        IFNE      LINTOT
     C                   ADD       1             COUNT
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    DET
     C                   END                                                    END CATOTC NE LINTOT
     C*
     C                   ELSE                                                   ELSE CATOTA EQ CATOT
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    DET
     C*
     C                   END                                                    END CATOTA EQ CATOTP
     C*
     C                   READ      PCA01                                  60
     C                   END
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C     COUNT         IFEQ      *ZEROS
     C                   EXCEPT    MSG
     C                   ELSE
     C                   EXCEPT    TOT
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       DSDATE        Y    127
     O          E            HEAD           2
     O                                           77 'PAYMENT/REDUCTION'
     O                                           89 'FIELDS <> TO CHARGED'
     O                                           96 'AMOUNT'
     O                                          121 'PAGE:'
     O                       PAGE          Z    127
     O          E            HEAD           1
     O                                           03 'CO#'
     O                                           10 'SUB #'
     O                                           15 'FND'
     O                                           20 'FYR'
     O                                           29 'CASE'
     O                                           36 'DESQ #'
     O                                           50 'CATOTC'
     O                                           64 'CATOTD'
     O                                           78 'CANDIS'
     O                                           92 'CAOTHR'
     O                                          106 'CATOTA'
     O                                          120 'CATOTP'
     O          E            DET            1
     O                       CACO#         Z     03
     O                       CASUB#        Z     10
     O                       CAFND         Z     15
     O                       CAFYR         Z     20
     O                       CACASE        Z     29
     O                       CADSEQ        Z     36
     O                       CATOTC        J     50
     O                       CATOTD        J     64
     O                       CANDIS        J     78
     O                       CAOTHR        J     92
     O                       CATOTA        J    106
     O                       CATOTP        J    120
     O          E            TOT            1
     O                                           16 'TOTAL # RECORDS:'
     O                       COUNT         Z     28
     O          E            MSG            1
     O                                           19 'NO RECORDS IN ERROR'
