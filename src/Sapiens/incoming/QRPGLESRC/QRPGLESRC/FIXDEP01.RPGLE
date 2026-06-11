     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIX PROGRAM - FIXDEP01
     H*****************************************************************
     H* Description:  THIS PROGRAM WILL LIST ANY EMPLOYERS THAT HAVE  *
     H*   HAD DEPOSIT TRANSFERS DONE WITHOUT A MATCHING PREMIUM       *
     H*   PAYMENT.                                                    *
     H*   PGMR: C:65                                                  *
     H*   SUPP: 42021                                                 *
     H*   DATE: 4/20/04                                               *
     H*****************************************************************
     FWDELL     IF   E           K DISK
     FWMEAL4    IF   E           K DISK
     FWMEAL1    IF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    EAUSER        XXUSER
     C     *LIKE         DEFINE    EATRDT        XXTRDT
     C*
     C* EMPLOYER A/R FILE
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXTYPE            1
     C*
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXUSER
     C                   KFLD                    XXTRDT
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *BLANKS       XXNAME           30
     C*
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   Z-ADD     0             XXCNT             4 0
     C*
     C*  Start main loop
     C*
     C                   READ      PEL01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      ELCO#         XXCO#
     C                   MOVE      ELFND         XXFND
     C                   MOVE      ELEMP#        XXEMP#
     C                   MOVE      ELDIV         XXDIV
     C                   MOVE      ELFYR         XXFYR
     C*
     C                   MOVE      'H'           XXTYPE
     C*
     C     EAKY          CHAIN     PEA01                              50
     C     *IN50         DOWEQ     *OFF
     C*
     C                   MOVE      *BLANK        XXARH             1
     C                   MOVE      *BLANK        XXARI             1
     C                   MOVE      *BLANK        XXARP             1
     C                   Z-ADD     0             XXHAMT           11 2
     C                   Z-ADD     0             XXIAMT           11 2
     C                   MOVE      *BLANKS       XXUSER
     C                   Z-ADD     0             XXTRDT
     C                   Z-ADD     0             XXTIME            6 0
     C*
     C     EAPGM         IFEQ      'W8010R'
     C                   MOVE      EAUSER        XXUSER
     C                   MOVE      EATRDT        XXTRDT
     C                   MOVE      EATIME        XXTIME
     C                   Z-ADD     EAAMT         XXHAMT
     C                   MOVE      'Y'           XXARH
     C                   ENDIF
     C*
     C     XXARH         IFEQ      'Y'
     C*
     C     EAKY1         CHAIN     PEA02                              51
     C     *IN51         DOWEQ     *OFF
     C*
     C     EAARTY        IFEQ      'I'
     C     EAPGM         ANDEQ     'W8010R'
     C                   Z-ADD     EAAMT         XXIAMT
     C                   MOVE      'Y'           XXARI
     C                   MOVE      *ON           *IN51
     C                   ENDIF
     C*
     C  N51EAKY1         READE     PEA02                                  51
     C                   ENDDO                                                  *IN51 DOWEQ
     C*
     C     XXARI         IFEQ      'Y'
     C*
     C     EAKY1         CHAIN     PEA02                              51
     C     *IN51         DOWEQ     *OFF
     C*
     C     EAARTY        IFEQ      'P'
     C     EAPGM         ANDEQ     'W8010R'
     C                   MOVE      'Y'           XXARP
     C                   MOVE      *ON           *IN51
     C                   ENDIF
     C*
     C  N51EAKY1         READE     PEA02                                  51
     C                   ENDDO                                                  *IN51 DOWEQ
     C*
     C                   ENDIF                                                  XXARI IFEQ
     C*
     C                   ENDIF                                                  XXARH IFEQ
     C*
     C* REPORT EMPLOYER IF DEPOSIT BILLED/PAID WRITTEN AND NO PAYMENT.
     C*
     C     XXARH         IFEQ      'Y'
     C     XXARI         ANDEQ     'Y'
     C     XXARP         ANDEQ     *BLANK
     C*
     C                   Z-ADD     XXTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE            8 0
     C*
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       1             XXCNT             4 0
     C                   MOVE      *ON           *IN50
     C*
     C                   ENDIF
     C*
     C  N50EAKY          READE     PEA01                                  50
     C                   ENDDO                                                  *IN50 DOWEQ
     C*
     C* READ NEXT POLICY.
     C*
     C                   READ      PEL01                                  60
     C                   ENDDO
     C*
     C   OF              EXCEPT    HEAD
     C*
     C     XXCNT         IFNE      *ZERO
     C                   EXCEPT    TOT
     C                   ELSE
     C                   EXCEPT    NOREC
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                                           67 'DEPOSIT TRANSFER WITH '
     O                                           82 'NO PAYMENT RCD.'
     O                       DSDATE        Y    132
     O          E            HEAD           1
     O                       Q1PGM               10
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            6 'GRP'
     O                                           18 'EMP#'
     O                                           26 'DIV'
     O                                           32 'FYR'
     O                                           52 'DEP BILL AMT'
     O                                           72 'DEP PAID AMT'
     O                                           87 'USER'
     O                                          102 'TRDT'
     O                                          112 'TIME'
     O          E            DET
     O                       XXFND                6
     O                       ELEMP#        4     18
     O                       ELDIV         4     26
     O                       XXFYR         4     32
     O                       XXHAMT        J     52
     O                       XXIAMT        J     72
     O                       XXUSER              87
     O                       XXDATE             102 '  /  /    '
     O                       XXTIME             112 '  :  :  '
     O          E            TOT         2
     O                                           31 'COUNT OF RECORDS WITH'
     O                                           51 'DIFFERENT PREMIUMS:'
     O                       XXCNT         4     56
     O          E            NOREC
     O                                           26 '*** NO RECORDS FOUND ***'
