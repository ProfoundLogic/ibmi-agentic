     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXROLL - DLT ROLLOVER INFO FOR A GIVEN FUND/FYR
     H*
     FWDELL1    UF   E           K DISK
     FWMEPL1    UF   E           K DISK
     FWMAHL1    UF   E           K DISK
     FWABLL     UF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQDEL             1
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C*
     C                   Z-ADD     0             XXCNT1            5 0
     C                   Z-ADD     0             XXCNT2            5 0
     C                   Z-ADD     0             XXCNT3            5 0
     C                   Z-ADD     0             XXCNT4            5 0
     C     QQDEL         IFEQ      '1'
     C                   MOVE      '1'           *IN99
     C                   ELSE
     C                   MOVE      '0'           *IN99
     C                   END
     C                   EXCEPT    HEAD
     C*
     C     XXKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*  DELETE EMPLOYER DETAIL RECORDS
     C*
     C     XXKEY         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     '0'
     C   99              DELETE    PEL01
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C                   ADD       1             XXCNT1
     C                   MOVE      ELEMP#        XXEMP#
     C                   MOVE      ELDIV         XXDIV
     C                   EXCEPT    DETAIL
     C     XXKEY         READE     PEL01                                  60
     C                   END
     C                   EXCEPT    TOTAL1
     C*
     C*  DELETE EMPLOYER PAYROLL RECORDS
     C*
     C     XXKEY         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     '0'
     C   99              DELETE    PEP01
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C                   ADD       1             XXCNT2
     C                   MOVE      EPEMP#        XXEMP#
     C                   MOVE      EPDIV         XXDIV
     C                   EXCEPT    DETAIL
     C     XXKEY         READE     PEP01                                  60
     C                   END
     C                   EXCEPT    TOTAL2
     C*
     C*  DELETE AGENT HISTORY RECORDS
     C*
     C     XXKEY         CHAIN     PAH01                              60
     C     *IN60         DOWEQ     '0'
     C   99              DELETE    PAH01
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C                   ADD       1             XXCNT3
     C                   MOVE      AHEMP#        XXEMP#
     C                   MOVE      AHDIV         XXDIV
     C                   EXCEPT    DETAIL
     C     XXKEY         READE     PAH01                                  60
     C                   END
     C                   EXCEPT    TOTAL3
     C*
     C*  DELETE BILLING REQUEST RECORDS
     C*
     C     XXKEY         CHAIN     PBL01                              60
     C     *IN60         DOWEQ     '0'
     C   99              DELETE    PBL01
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C                   ADD       1             XXCNT4
     C                   MOVE      BLEMP#        XXEMP#
     C                   MOVE      BLDIV         XXDIV
     C                   EXCEPT    DETAIL
     C     XXKEY         READE     PBL01                                  60
     C                   END
     C                   EXCEPT    TOTAL4
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OQSYSPRT   E            HEAD           1 06
     O                                           60 '     ROLLOVER RECORDS   '
     O          E            HEAD           2
     O                                           60 '        DELETED         '
     O          E            HEAD           2
     O                                            4 'CO#'
     O                                           10 'FUND'
     O                                           22 'EMP#'
     O                                           29 'DIV'
     O                                           34 'FYR'
     O          E            DETAIL         1
     O                       XXCO#         Z      4
     O                       XXFND         Z     10
     O                       XXEMP#        Z     22
     O                       XXDIV         Z     29
     O                       XXFYR         Z     34
     O          E            TOTAL1      2  2
     O                                              '# OF EMP DTL RECS DEL: '
     O                       XXCNT1        Z
     O          E            TOTAL2      2  2
     O                                              '# OF EMP PYRL RECS DEL: '
     O                       XXCNT2        Z
     O          E            TOTAL3      2  2
     O                                              '# OF AGT HIST RECS DEL: '
     O                       XXCNT3        Z
     O          E            TOTAL4      2  2
     O                                              '# OF BILL REQ RECS DEL: '
     O                       XXCNT4        Z
