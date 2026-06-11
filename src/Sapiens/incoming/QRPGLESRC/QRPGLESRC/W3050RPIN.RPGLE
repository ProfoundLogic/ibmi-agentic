     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W3050R - COMMISSION CHECKS
     FWAR2L     IF   E           K DISK
     FWAC2L     IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     F*

     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  AGMZIP                 1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR REQUEST COMMISSION PAYMENT FILE
     C*
     C     R2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  CHECK SCHEDULE FILE
     C*
     C     C2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE AGENT ASSIGNMENT MASTER FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    R2CO#
     C                   KFLD                    R2FND
     C                   KFLD                    R2AGT#
     C*
     C*   KEY LIST FOR THE AGENT MASTER FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    R2CO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C     *LIKE         DEFINE    AGDT          XXDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDATE            6
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQDATE        XXDATE            6 0
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C*
     C*  GET THE ENTRY FROM THE CHECK PROCESSING FILE
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C     C2KEY         CHAIN     PC201                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      ' '           XXSTRT
     C*
     C     R2KEY         CHAIN     WAR2L                              50
     C*
     C*  IF THE REPLACEMENT CHECK IS NOT ZERO, WE NEED TO START WITH
     C*  A CERTAIN CHECK
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C     C2RSTR        IFNE      *ZERO
     C     C2RSTR        IFNE      R2CHCK
     C     XXSTRT        IFEQ      ' '
     C                   GOTO      NCHK#
     C                   END
     C                   ELSE
     C                   MOVE      'Y'           XXSTRT            1
     C                   END
     C                   END
     C*
     C*  RETRIEVE NAME AND MAILING ADDRESS - USE EITHER SECRETARY OR
     C*  AGENT NAME.
     C*
     C                   MOVE      *BLANKS       XXNAME           32
     C                   MOVE      *BLANKS       XXADDR           32
     C                   MOVE      *BLANKS       XXMCSZ           32
     C     AAKEY         CHAIN     PAA01                              51
     C     *IN51         IFEQ      '0'
     C*
     C     AGKEY         CHAIN     PAG01                              51
     C     *IN51         IFEQ      '0'
     C*
     C                   MOVEL     AGNAME        XXNAME
     C                   MOVEL     AGMAD1        XXADDR
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
     C                   MOVEL     XXLAST        XXMCSZ           32
     C*
     C                   END
     C                   END
     C*
     C*  LOAD DOLLAR AMOUNT SPELLED OUT
     C*
     C                   MOVE      R2AMT         QQAMT             9
     C                   MOVE      *BLANKS       QQAMT1           60
     C                   MOVE      *BLANKS       QQAMT2           60
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRAMNT'
     C                   PARM                    QQAMT
     C                   PARM                    QQAMT1
     C                   PARM                    QQAMT2
     C                   PARM                    QQTERM
     C                   MOVE      *BLANKS       XXAMT1           60
     C                   MOVE      *BLANKS       XXAMT2           60
     C                   MOVEL     QQAMT1        XXAMT1
     C                   MOVEL     QQAMT2        XXAMT2
     C*
     C*  PRINT CHECK
     C*
     C                   EXCEPT    CHECK
     C     NCHK#         TAG
     C*
     C     R2KEY         READE     WAR2L                                  50
     C*
     C                   END
     C                   END
     C*
     C                   SETON                                        LR
     C*
     OQSYSPRT   E            CHECK            05
     O                       R2CHCK        Z     77
     O          E            CHECK            06
     O                       XXDATE        Y     66
     O          E            CHECK          1 09
     O                       XXAMT1              66
     O                       R2AMT               80 '  ,   , $0.  '
     O          E            CHECK          2
     O                       XXAMT2              66
     O          E            CHECK          1 14
     O                       XXNAME              43
     O          E            CHECK          1
     O                       XXADDR              43
     O          E            CHECK          1
     O                       XXMCSZ              43
     O          E            CHECK            28
     O                                           16 'AGENT #'
     O                       R2AGT#        Z     22
     O                                           46 'COMMISSION PAYMENT'
     O                       R2AMT         J     74
