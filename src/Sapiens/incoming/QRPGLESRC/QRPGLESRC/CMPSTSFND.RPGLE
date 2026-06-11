     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CMPSTS: COMPARE CMSTAT TO LAST CHOCST IN WMCHP
     H*
     H***********************************************************
     H* REVISIONS:
     H*   01-27-95   003   CHANGE PROGRAM TO ACCEPT ENTRY PARMS
     H*                    FOR CO#  AND FND#
     H***********************************************************
     H*
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMCHL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     C*
     C*  CLAIM HISTORY MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*  CLAIM HISTORY MASTER FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C*
     C*
     C*  START READING CLAIM MASTER FILE
     C*
     C                   Z-ADD     0             XXCNT             9 0
     C                   Z-ADD     0             XXLINE            2 0
     C                   EXCEPT    HEAD
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     XXLINE        IFGT      55
     C                   EXCEPT    HEAD
     C                   Z-ADD     0             XXLINE
     C                   END
     C*
     C*  GET CLAIM HISTORY RECORD
     C*
     C     CHKEY         SETGT     WMCHL
     C                   READP     WMCHL                                  60
     C     *IN60         IFEQ      '0'
     C     CMCO#         ANDEQ     CHCO#
     C     CMFND         ANDEQ     CHFND
     C     CMFYR         ANDEQ     CHFYR
     C     CMCASE        ANDEQ     CHCASE
     C*
     C     CHOCST        IFNE      CMSTAT
     C                   ADD       1             XXCNT
     C                   ADD       1             XXLINE
     C                   MOVE      *BLANKS       CDFSN
     C                   MOVE      *BLANKS       CDLSN
     C     CDKEY         CHAIN     PCD01                              60
     C                   MOVE      '0'           *IN25
     C                   EXCEPT    DET
     C                   END
     C*
     C                   ELSE
     C*
     C                   ADD       1             XXCNT
     C                   ADD       1             XXLINE
     C                   MOVE      *BLANKS       CDFSN
     C                   MOVE      *BLANKS       CDLSN
     C     CDKEY         CHAIN     PCD01                              60
     C                   MOVE      '1'           *IN25
     C                   EXCEPT    DET
     C*
     C                   END
     C*
     C     CMKEY         READE     PCM01                                  60
     C                   END
     C                   EXCEPT    TOT
     C*
     C                   MOVE      '1'           *INLR
     C*
     OQSYSPRT   E            HEAD           2 03
     O                                           19 'CO# FND FYR   CASE#'
     O                                           29 'LAST NAME'
     O                                           61 'FIRST NAME'
     O                                           95 'CHOCST  CMSTAT'
     O          E            DET            1
     O                       CMCO#                3
     O                       CMFND                7
     O                       CMFYR         Z     11
     O                       CMCASE        Z     19
     O                       CDLSN               50
     O                       CDFSN               66
     O              N25      CHOCST              84
     O              N25      CMSTAT              92
     O               25                          92 'NO WMCHP RECORD'
     O          E            TOT         2  1
     O                                           13 'TOTAL COUNT:'
     O                       XXCNT         J     30
