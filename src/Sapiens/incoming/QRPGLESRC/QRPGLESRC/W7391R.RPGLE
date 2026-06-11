     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7391R - AGENT LABELED MAILING ADDRESS BY FUND
     FWMAGL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D                 DS
     D  AGMZIP                 1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C*
     C*   KEY LIST FOR THE AGENT ASSIGNMENT MASTER FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE AGENT MASTER FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*   START READING THE AGENT ASSIGNMENT MASTER FILE
     C*
     C     AAKEY         CHAIN     PAA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*
     C*   GET THE AGENT MASTER FILE
     C*
     C     AGKEY         CHAIN     PAG01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   MOVE NAME AND ADDRESS INTO SMALLER FIELDS
     C*
     C                   MOVE      *BLANKS       XXNAME
     C                   MOVE      *BLANKS       XXADDR
     C                   MOVEL     AGNAME        XXNAME           33
     C                   MOVEL     AGMAD1        XXADDR           33
     C*
     C*   LOAD CITY, STATE, & ZIP FOR THE MAILING ADDRESS
     C*
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     AGMST         XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     AGMCTY        XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXMCSZ           33
     C*
     C*   PRINT THE LINE
     C*
     C                   EXCEPT    DET
     C                   END
     C     AAKEY         READE     PAA01                                  60
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
     OQSYSPRT   E            DET         1  1
     O                       AAAGT#        Z     34
     O          E            DET            1
     O                       XXNAME              34
     O          E            DET            1
     O                       XXADDR              34
     O          E            DET            2
     O                       XXMCSZ              34
