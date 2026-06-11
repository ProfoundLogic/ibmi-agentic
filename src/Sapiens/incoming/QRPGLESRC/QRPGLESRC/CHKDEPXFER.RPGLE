     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CHKDEPXFER - CHECK DEPOSIT TRANSFER BALANCES
     H*
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMEAL3    IF   E           K DISK
     FQSYSPRT   O    F   80        PRINTER OFLIND(*INOF)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSDATE                 7     12  0

     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXARTY            1
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C*
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*****************************************************************
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     ELKY          SETGT     PEL01
     C     ELKY          READPE    PEL01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     0             XXIB             11 2
     C                   Z-ADD     0             XXPB             11 2
     C                   MOVE      'I'           XXARTY
     C*
     C     EAKY          CHAIN     PEA01                              50
     C     *IN50         DOWEQ     *OFF
     C*
     C     EAPGM         IFEQ      'W7720R'
     C                   SUB       EAAMT         XXIB
     C                   ENDIF
     C*
     C     EAKY          READE     PEA01                                  50
     C                   ENDDO                                                  *IN50 DOWEQ
     C*
     C                   MOVE      'P'           XXARTY
     C*
     C     EAKY          CHAIN     PEA01                              50
     C     *IN50         DOWEQ     *OFF
     C*
     C     EAPGM         IFEQ      'W7720R'
     C                   ADD       EAAMT         XXPB
     C                   ENDIF
     C*
     C     EAKY          READE     PEA01                                  50
     C                   ENDDO                                                  *IN50 DOWEQ
     C*
     C* PRINT EMPLOYER IF 'I' TOTAL DOES NOT MATCH 'P' TOTAL
     C*
     C     XXIB          IFNE      XXPB
     C     XXIB          SUB       XXPB          XXDIF            11 2
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ENDIF                                                  ELPRTD IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EMKY          READE     PEM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'PROGRAM NAME:'
     O                                           26 'CHKDEPXFER'
     O                                           70 'RUN DATE:'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                                           25 'PGM TO CHECK IF DEPOSIT'
     O                                           43 'TRANSFER DOES NOT'
     O                                           51 'BALANCE'
     O                                           62 'CO#:'
     O                       XXCO#         4     66
     O                                           73 'FUND#:'
     O                       XXFND         4     77
     O          E            HEAD           2
     O                                            5 'CO#'
     O                                           11 'FUND'
     O                                           16 'FYR'
     O                                           27 'EMPLOYER#'
     O                                           34 'DIV'
     O                                           49 'DEP PD'
     O                                           64 'PREM PD'
     O                                           79 'DIFF'
     O          E            DET            1
     O                       ELCO#         4      5
     O                       ELFND         4     11
     O                       ELFYR         4     16
     O                       ELEMP#        4     27
     O                       ELDIV         4     34
     O                       XXIB          J     49
     O                       XXPB          J     64
     O                       XXDIF         J     79
