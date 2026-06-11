     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CHKNEWPAY- CHECK FOR NEGATIVE RESERVES
     H*
     FWDELL1    IF   E           K DISK
     FWMEAL4    IF   E           K DISK
     FWTABL     IF   E           K DISK
     FQSYSPRT   O    F   80        PRINTER OFLIND(*INOF)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSDATE                 7     12  0

     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    XXARTY            1
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C*
     C                   MOVE      'B'           XXARTY
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C*
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*****************************************************************
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     0             XXEB             11 2
     C*
     C     EAKY          CHAIN     PEA01                              50
     C     *IN50         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              40
     C     *IN40         IFEQ      *OFF
     C     TTCTYP        ANDEQ     'E'
     C                   ADD       EAAMT         XXEB
     C                   ENDIF                                                  *IN40 IFEQ
     C*
     C     EAKY          READE     PEA01                                  50
     C                   ENDDO                                                  *IN50 DOWEQ
     C*
     C* PRINT EMPLOYER IF EXPENSE CONSTANT BILLED DOES NOT MATCH A/R.
     C*
     C     ELPRTD        IFNE      XXEB
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ENDIF                                                  ELPRTD IFNE
     C*
     C     ELKY          READE     PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'PROGRAM NAME:'
     O                                           25 'CHKECZERO'
     O                                           70 'RUN DATE:'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                                           24 'PGM TO CHECK IF ELPRTD'
     O                                           44 'DOES NOT MATCH A/R:'
     O                                           50 'CO#:'
     O                       XXCO#         4     54
     O                                           62 'FUND#:'
     O                       XXFND         4     66
     O                                           73 'FYR#:'
     O                       XXFYR         4     77
     O          E            HEAD           2
     O                                            5 'CO#'
     O                                           11 'FUND'
     O                                           16 'FYR'
     O                                           27 'EMPLOYER#'
     O                                           34 'DIV'
     O                                           42 'STATUS'
     O                                           56 'ELPRTD'
     O                                           72 'EXP CNST A/R'
     O          E            DET            1
     O                       ELCO#         4      5
     O                       ELFND         4     11
     O                       ELFYR         4     16
     O                       ELEMP#        4     27
     O                       ELDIV         4     34
     O                       ELFAF               39
     O                       ELPRTD        J     56
     O                       XXEB          J     72
