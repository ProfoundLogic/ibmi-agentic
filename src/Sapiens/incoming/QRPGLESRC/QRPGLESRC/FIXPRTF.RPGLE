     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXPRTF - POPULATE ELPRTF WITH ANNUAL EXPENSE CONSTANT.
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                       *
     H**                                                              *
     H**   POPULATE THE ELPRTF FIELD WITH ANNUAL EXPENSE CONSTANT.    *
     H*                                                               *
     H* PROGRAMMER: C:65                                              *
     H* CREATED:    11/27/01            PROJECT: WC1059               *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H* 61  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FWDELL     UF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWDELL1    IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWDECL     IF   E           K DISK
     FQSYSPRT   O    F   85        PRINTER OFLIND(*INOF)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD            3
     C*
     C*  EXPENSE CONSTANT FILE
     C*
     C     ECKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELPFDT
     C*
     C     ECKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   MOVE      *ON           *INOF
     C                   TIME                    DSDTTM
     C*
     C* READ ALL EMPLOYER DETAIL RECORDS.
     C*
     C                   READ(N)   PEL02                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C* ONLY UPDATE RECORDS WITH EXPENSE CONSTANT.
     C*
     C     ELPRTD        IFNE      *ZERO
     C     ELPRTF        ANDEQ     *ZERO
     C*
     C                   Z-ADD     ELCO#         XXCO#
     C                   Z-ADD     ELFND         XXFND
     C                   Z-ADD     ELFYR         XXFYR
     C                   Z-ADD     ELEMP#        XXEMP#
     C                   Z-ADD     ELDIV         XXDIV
     C                   Z-ADD     0             XXEXCO           11 2
     C*
     C* FIND HIGHEST EXPENSE CONSTANT.
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     ECKY          SETGT     PEC01
     C     ECKY1         READPE    PEC01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     ECEXCO        ANDGT     XXEXCO
     C     ELPFDT        ANDNE     ELPTDT
     C                   Z-ADD     ECEXCO        XXEXCO
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     ELKY          READE     PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* UPDATE RECORD WITH EXPENSE CONSTANT.
     C*
     C     ELKEY         CHAIN     PEL02                              60
     C     *IN60         IFEQ      *OFF
     C*
     C* PULL TABLE AMOUNT.
     C*
     C     XXEXCO        IFNE      *ZERO
     C                   Z-ADD     XXEXCO        ELPRTF
     C                   ELSE
     C*
     C* UNPRORATE IF THERE IS NO TABLE AMOUNT.
     C*
     C     ELPROR        IFNE      *ZERO
     C*
     C     ELPRTD        DIV(H)    ELPROR        XXWK10           11 0
     C                   Z-ADD     XXWK10        ELPRTF
     C*
     C     ELPRTF        IFGT      250
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ENDIF
     C*
     C                   ELSE
     C                   Z-ADD     ELPRTD        ELPRTF
     C                   ENDIF                                                  ELPROR IFNE
     C*
     C                   ENDIF                                                  XXEXCO IFNE
     C*
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C*
     C                   UPDATE    PEL02
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  ELPRTD IFNE
     C*
     C* READ NEXT EMPLOYER DETAIL.
     C*
     C                   READ(N)   PEL02                                  60
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           54 'ANNUAL EXPENSE CONSTANT '
     O                                           74 'PAGE:'
     O                       PAGE          Z     80
     O          E            HEAD           2
     O                                            3 'CO#'
     O                                            8 'FND'
     O                                           13 'FYR'
     O                                           24 'EMPLOYER#'
     O                                           34 'DIVISION'
     O                                           50 'ELPRTD'
     O                                           66 'ELPRTF'
     O                                           75 'ELPROR'
     O          E            DET            1
     O                       ELCO#         3      3
     O                       ELFND         3      8
     O                       ELFYR         3     13
     O                       ELEMP#        3     24
     O                       ELDIV         3     34
     O                       ELPRTD        J     50
     O                       ELPRTF        J     66
     O                       ELPROR        3     75
