     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE FIX29681I - COUNT DUPLICATE A/R RECORDS
     H*****************************************************************
     H* DESCRIPTION                                                   *
     H*  THIS REPORT COUNTS A/R RECORDS THAT HAVE DUPLICATE KEYS FOR  *
     H*  WMEAL6 (CO#, FND, EMP#, DIV, FYE, PODT, DT, TIME)            *
     H*****************************************************************
     H* FILES:
     H*    WMEAL6 (PEA02)
     H*
     H****************************************************************
     F*
     FWMEAL6    IF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     C*
     C     EAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C                   KFLD                    KKPODT
     C                   KFLD                    KKDT
     C                   KFLD                    KKTIME
     C*
     C*  INITIALIZE FIELDS
     C*
     C     *LIKE         DEFINE    EACO#         KKCO#
     C     *LIKE         DEFINE    EAFND         KKFND
     C     *LIKE         DEFINE    EAEMP#        KKEMP#
     C     *LIKE         DEFINE    EADIV         KKDIV
     C     *LIKE         DEFINE    EAFYR         KKFYR
     C     *LIKE         DEFINE    EAPODT        KKPODT
     C     *LIKE         DEFINE    EADT          KKDT
     C     *LIKE         DEFINE    EATIME        KKTIME
     C*
     C**************************************************************************
     C*  Start of main loop.
     C*
     C                   Z-ADD     0             CNT               9 0
     C                   READ      PEA02                                  60
     C*
     C                   MOVE      *ON           *INOF
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      EACO#         KKCO#
     C                   MOVE      EAFND         KKFND
     C                   MOVE      EAEMP#        KKEMP#
     C                   MOVE      EADIV         KKDIV
     C                   MOVE      EAFYR         KKFYR
     C                   MOVE      EAPODT        KKPODT
     C                   MOVE      EADT          KKDT
     C                   MOVE      EATIME        KKTIME
      *
     C     EAKY          READE     PEA02                                  61
     C     *IN61         DOWEQ     *OFF
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C                   ADD       1             CNT
     C*
     C     EADT          IFGE      1011001
     C                   EXCEPT    DET1
     C                   ENDIF
     C     EAKY          READE     PEA02                                  61
     C                   ENDDO                                                  *IN61 EQ *OFF
     C*
     C     *IN61         IFEQ      *ON
     C     EAKY          SETGT     PEA02
     C                   READ      PEA02                                  60
     C                   ENDIF
     C*
     C                   ENDDO                                                  *IN60 EQ *OFF
     C*
     C                   EXCEPT    DET2
     C*
     C                   MOVE      *ON           *INLR
     C*
     C********************************************************************
     OQSYSPRT   E            HEAD           3 06
     O                                           73 'COUNT DUPLICATE RECORDS'
     O                                           82 'IN WMEAP'
     O                                          120 'PAGE:'
     O                       PAGE          Z    126
     O          E            DET1           1
     O                       EACO#         4      3
     O                       EAFND         4      8
     O                       EAEMP#        4     20
     O                       EADIV         4     28
     O                       EAFYR         4     34
     O                       EAPODT        4     42
     O                       EADT          4     51
     O                       EATIME        4     62
     O                       EAARCD              70
     O                       EAAMT         4     85
     O                       EAPGM               97
     O          E            DET2           1
     O                                           47 'There are'
     O                       CNT           4     56
     O                                           81 'records with duplicate k'
     O                                           94 'eys in WMEAP.'
