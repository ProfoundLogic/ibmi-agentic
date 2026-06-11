     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE CMPEXPCNST - CHECKS FOR A/R NOT EQUAL TO EMP DETAIL
     H*****************************************************************
     F*
     FWMEAL4    IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXARTY
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  INITIALIZE FIELDS
     C*
     C     *LIKE         DEFINE    EAARTY        XXARTY
     C     *LIKE         DEFINE    TTKEY         XXKEY
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      'B'           XXARTY
     C*
     C*  Start of main loop.
     C*
     C     ELKY          READE     PEL01                                  60
     C*
     C                   MOVE      *ON           *INOF
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      ELEMP#        XXEMP#
     C                   MOVE      ELDIV         XXDIV
     C                   MOVE      ELFYR         XXFYR
     C*
     C                   Z-ADD     *ZEROS        XXAMT            11 2
     C*
     C     EAKY          CHAIN     PEA01                              50
     C     *IN50         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              40
     C     *IN40         IFEQ      *OFF
     C     TTCTYP        ANDEQ     'E'
     C                   ADD       EAAMT         XXAMT
     C                   ENDIF
     C*
     C     EAKY          READE     PEA01                                  50
     C                   ENDDO
     C*
     C     XXAMT         IFNE      ELPRTD
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C                   EXCEPT    DET
     C*
     C                   ENDIF                                                  XXAMT IFNE
     C*
     C*
     C     ELKY          READE     PEL01                                  60
     C                   ENDDO
     C*
     C*  End of program
     C*
     C                   MOVE      *ON           *INLR
     C*
     C********************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                                           65 'CHECK ACCOUNTS WITH A/R'
     O                                           87 'NOT EQUAL TO EXP CNST.'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                            5 'CO#'
     O                                           10 'FND'
     O                                           22 'EMPLOYER #'
     O                                           32 'DIVISION'
     O                                           37 'FYR'
     O                                           52 'EXP CNST'
     O                                           67 'A/R EXP CNST'
     O          E            DET            1
     O                       ELCO#         4      5
     O                       ELFND         4     10
     O                       ELEMP#        4     22
     O                       ELDIV         4     30
     O                       ELFYR         4     37
     O                       ELPRTD        J     52
     O                       XXAMT         J     67
