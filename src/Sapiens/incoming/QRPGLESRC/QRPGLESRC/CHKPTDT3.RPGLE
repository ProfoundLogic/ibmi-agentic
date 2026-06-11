     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CHKPTDT - CHECK FOR POLICIES WHERE WDELP ELPTDT DOES
     H*                NOT MATCH THE LATEST WDEHP EHPTDT.
     H**************************************************************************
     H*  PROJECT: SUPP# 71728         DESC:  GENERATE A REPORT OF ALL POLICIES
     H*  DATE:    03/25/2010                 WHERE THE WDELP POLICY EXPIRATION
     H*  PGMR:    C:65                       DATE DOES NOT MATCH LATEST WDEHP.
     H**************************************************************************
     FWDEHL6    IF   E           K DISK
     FWDMPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWDELL1    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOA)
     FQSYSPRT2  O    F  132        PRINTER OFLIND(*INOB)

     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND
     D  CONAME                 1     40
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSDATE                 7     12  0
     D                 DS
     D  EHTRDT                 1      7  0
     D  YYMM                   1      5  0

     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C     EHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     MPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     *LIKE         DEFINE    EHCO#         XXCO#
     C     *LIKE         DEFINE    EHFND         XXFND
     C     *LIKE         DEFINE    EHFYR         XXFYR
     C     *LIKE         DEFINE    EHEMP#        XXEMP#
     C     *LIKE         DEFINE    EHDIV         XXDIV
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   Z-ADD     0             XXCO#
     C                   Z-ADD     0             XXFND
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C                   Z-ADD     0             XXFYR
     C*
     C                   Z-ADD     0             XXCNT             5 0
     C                   Z-ADD     0             XXCNT2            5 0
     C                   Z-ADD     0             XXCNT3            5 0
     C*
     C                   TIME                    DSDTTM
     C*
     C                   READ      PEL01                                  50
     C*
     C                   Z-ADD     ELCO#         XXCO#
     C*
     C     COKEY         CHAIN     SMCOL                              60
     C*
     C*  REMOVE LEADING BLANKS FROM COMPANY NAME
     C*
     C                   Z-ADD     1             Z                 2 0
     C     *BLANK        LOOKUP    CONM(Z)                            60
     C                   MOVEA     CONM(Z)       XXCONM           50
     C*
     C* PRINT HEADING
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD2
     C                   EXCEPT    HEAD3
     C*
     C* READ EMPLOYER DETAIL
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C* FIND A RECORD THAT FALLS WITHIN CRITERIA
     C*
     C                   Z-ADD     ELCO#         XXCO#
     C                   Z-ADD     ELFND         XXFND
     C                   Z-ADD     ELFYR         XXFYR
     C                   Z-ADD     ELEMP#        XXEMP#
     C                   Z-ADD     ELDIV         XXDIV
     C*
     C     MPKY          CHAIN     PMP01                              60
     C*
     C* USING LATEST WDEHP DATE.
     C*
     C     EHKY          SETGT     PEH01
     C     EHKY          READPE    PEH01                                  70
     C*
     C     *IN70         DOWEQ     *OFF
     C     ELFAF         ANDNE     'A'
     C     EHFAF         ANDEQ     'A'
     C     EHKY          READPE    PEH01                                  70
     C                   ENDDO
     C*
     C     *IN70         IFEQ      *OFF
     C     EHPTDT        ANDNE     ELPTDT
     C*
     C                   EXSR      PRINT
     C*
     C                   ENDIF
     C*
     C     ELFND         IFNE      132
     C     ELFND         ANDNE     178
     C     ELFND         ANDNE     179
     C     ELFND         ANDNE     174
     C     ELFND         ANDNE     175
     C     ELFND         ANDNE     820
     C     ELFND         ANDNE     134
     C     ELFND         ANDNE     135
     C     ELFND         ANDNE     430
     C*
     C* USING FIRST WDEHP DATE.
     C*
     C     ELFAF         IFNE      'A'
     C*
     C     EHKY          SETGT     PEH01
     C     EHKY          READPE    PEH01                                  70
     C*
     C     *IN70         IFEQ      *OFF
     C     EHTRDT        ANDGE     1100301
     C*
     C     EHKY          SETLL     PEH01
     C     EHKY          READE     PEH01                                  70
     C*
     C     *IN70         DOWEQ     *OFF
     C     ELFAF         ANDNE     'A'
     C     EHFAF         ANDEQ     'A'
     C     EHKY          READE     PEH01                                  70
     C                   ENDDO
     C*
     C     *IN70         IFEQ      *OFF
     C     EHPTDT        ANDNE     ELPTDT
     C*
     C                   EXSR      PRINT2
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  EHTRDT IFGT
     C*
     C                   ENDIF                                                  ELFAF IFNE
     C*
     C                   ENDIF                                                  ELFND IFNE
     C*
     C* USING FIRST WDEHP DATE.
     C*
     C     ELFAF         IFEQ      'A'
     C*
     C     EHKY          SETGT     PEH01
     C     EHKY          READPE    PEH01                                  70
     C*
     C     *IN70         IFEQ      *OFF
     C     EHTRDT        ANDGE     1100301
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C     10000         ADD       ELPFDT        XXPFDT            7 0
     C*
     C     EHKY          SETLL     PEH01
     C     EHKY          READE     PEH01                                  70
     C*
     C     *IN70         IFEQ      *OFF
     C     EHPTDT        ANDNE     ELPTDT
     C     EMCAD1        ANDEQ     *ZERO
     C     EMCAD1        ORNE      *ZERO
     C     ELPTDT        ANDNE     EMCAD1
     C     EMCAD1        ANDGE     ELPFDT
     C     EMCAD1        ANDLE     XXPFDT
     C*
     C                   EXSR      PRINT3
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  EHTRDT IFGT
     C*
     C                   ENDIF                                                  ELFAF IFNE
     C*
     C* READ NEXT POLICY
     C*
     C                   READ      PEL01                                  50
     C                   ENDDO                                                  *IN50 DOWEQ
     C*
     C* PRINT TOTAL COUNT
     C*
     C     XXCNT         IFNE      *ZERO
     C                   EXCEPT    COUNT
     C                   ELSE
     C                   EXCEPT    NOCNT
     C                   ENDIF
     C*
     C     XXCNT2        IFNE      *ZERO
     C                   EXCEPT    COUNT2
     C                   ELSE
     C                   EXCEPT    NOCNT2
     C                   ENDIF
     C*
     C     XXCNT3        IFNE      *ZERO
     C                   EXCEPT    COUNT3
     C                   ELSE
     C                   EXCEPT    NOCNT3
     C                   ENDIF
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*****************************************************************
     C*
     C* PRINT AN EMPLOYER TO THE REPORT
     C*
     C     PRINT         BEGSR
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   Z-ADD     EMCAD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XMCAD1            8 0
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XEPTDT            8 0
     C*
     C                   Z-ADD     EHPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XHPTDT            8 0
     C*
     C                   Z-ADD     ELDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XEDT              8 0
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XEPFDT            8 0
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C                   EXCEPT    DET
     C*
     C                   ADD       1             XXCNT
     C*
     C                   ENDSR                                                  PRINT
     C*
     C*****************************************************************
     C*
     C* PRINT AN EMPLOYER TO THE REPORT
     C*
     C     PRINT2        BEGSR
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   Z-ADD     EMCAD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XMCAD1            8 0
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XEPTDT            8 0
     C*
     C                   Z-ADD     EHPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XHPTDT            8 0
     C*
     C                   Z-ADD     ELDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XEDT              8 0
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XEPFDT            8 0
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD2
     C                   MOVE      *OFF          *INOA
     C                   ENDIF
     C*
     C                   EXCEPT    DET2
     C*
     C                   ADD       1             XXCNT2
     C*
     C                   ENDSR                                                  PRINT2
     C*
     C*****************************************************************
     C*
     C* PRINT AN EMPLOYER TO THE REPORT
     C*
     C     PRINT3        BEGSR
     C*
     C                   Z-ADD     EMCAD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XMCAD1            8 0
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XEPTDT            8 0
     C*
     C                   Z-ADD     EHPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XHPTDT            8 0
     C*
     C                   Z-ADD     ELDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XEDT              8 0
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XEPFDT            8 0
     C*
     C     *INOB         IFEQ      *ON
     C                   EXCEPT    HEAD3
     C                   MOVE      *OFF          *INOB
     C                   ENDIF
     C*
     C                   EXCEPT    DET3
     C*
     C                   ADD       1             XXCNT3
     C*
     C                   ENDSR                                                  PRINT3
     C*
     C*****************************************************************
     C*
     C*****************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'CHKPTDT: '
     O                                           36 'PGM TO CHECK IF EMPLR '
     O                                           60 'HISTORY EXP DATE IS DIFF'
     O                                          100 'USING LATEST WDEHP RCD'
     O                                          120 'RUN DATE:'
     O                       DSDATE        Y    130
     O          E            HEAD           2
     O                                           21 'CO#:'
     O                       XXCO#         4     25
     O                       XXCONM             124
     O          E            HEAD           2
     O                                           06 'FUND'
     O                                           12 'FYR'
     O                                           22 'EMPLOYER#'
     O                                           30 'DIV'
     O                                           37 'ELFAF'
     O                                           44 'EHFAF'
     O                                           55 'ELPFDT'
     O                                           68 'ELPTDT'
     O                                           81 'EHPTDT'
     O                                           94 'EMCAD1'
     O                                          107 '  ELDT'
     O                                          114 ' ELPGM'
     O                                          128 'ELUSER'
     O          E            DET            1
     O                       EHFND         4     06
     O                       EHFYR         4     12
     O                       EHEMP#        4     22
     O                       EHDIV         4     30
     O                       ELFAF               35
     O                       EHFAF               42
     O                       XEPFDT              55 '  /  /    '
     O                       XEPTDT              68 '  /  /    '
     O                       XHPTDT              81 '  /  /    '
     O                       XMCAD1              94 '  /  /    '
     O                       XEDT               107 '  /  /    '
     O                       ELPGM              119
     O                       ELUSER             132
     O          E            COUNT       1  1
     O                                           20 'TOTAL RECORDS:'
     O                       XXCNT         1     26
     O          E            NOCNT       1  1
     O                                           25 'NO RECORDS MATCHED'
     C*****************************************************************
     OQSYSPRT1  E            HEAD2          2 06
     O                                           14 'CHKPTDT: '
     O                                           36 'PGM TO CHECK IF EMPLR '
     O                                           60 'HISTORY EXP DATE IS DIFF'
     O                                           72 'NOT AUDITED'
     O                                          100 'USING EARLIEST WDEHP RCD'
     O                                          120 'RUN DATE:'
     O                       DSDATE        Y    130
     O          E            HEAD2          2
     O                                           21 'CO#:'
     O                       XXCO#         4     25
     O                       XXCONM             124
     O          E            HEAD2          2
     O                                           06 'FUND'
     O                                           12 'FYR'
     O                                           22 'EMPLOYER#'
     O                                           30 'DIV'
     O                                           37 'ELFAF'
     O                                           44 'EHFAF'
     O                                           55 'ELPFDT'
     O                                           68 'ELPTDT'
     O                                           81 'EHPTDT'
     O                                           94 'EMCAD1'
     O                                          107 '  ELDT'
     O                                          114 ' ELPGM'
     O                                          128 'ELUSER'
     O          E            DET2           1
     O                       EHFND         4     06
     O                       EHFYR         4     12
     O                       EHEMP#        4     22
     O                       EHDIV         4     30
     O                       ELFAF               35
     O                       EHFAF               42
     O                       XEPFDT              55 '  /  /    '
     O                       XEPTDT              68 '  /  /    '
     O                       XHPTDT              81 '  /  /    '
     O                       XMCAD1              94 '  /  /    '
     O                       XEDT               107 '  /  /    '
     O                       ELPGM              119
     O                       ELUSER             132
     O          E            COUNT2      1  1
     O                                           20 'TOTAL RECORDS:'
     O                       XXCNT2        1     26
     O          E            NOCNT2      1  1
     O                                           25 'NO RECORDS MATCHED'
     C*****************************************************************
     OQSYSPRT2  E            HEAD3          2 06
     O                                           14 'CHKPTDT: '
     O                                           36 'PGM TO CHECK IF EMPLR '
     O                                           60 'HISTORY EXP DATE IS DIFF'
     O                                           72 'AUDITED'
     O                                          100 'USING EARLIEST WDEHP RCD'
     O                                          120 'RUN DATE:'
     O                       DSDATE        Y    130
     O          E            HEAD3          2
     O                                           21 'CO#:'
     O                       XXCO#         4     25
     O                       XXCONM             124
     O          E            HEAD3          2
     O                                           06 'FUND'
     O                                           12 'FYR'
     O                                           22 'EMPLOYER#'
     O                                           30 'DIV'
     O                                           37 'ELFAF'
     O                                           44 'EHFAF'
     O                                           55 'ELPFDT'
     O                                           68 'ELPTDT'
     O                                           81 'EHPTDT'
     O                                           94 'EMCAD1'
     O                                          107 '  ELDT'
     O                                          114 ' ELPGM'
     O                                          128 'ELUSER'
     O          E            DET3           1
     O                       EHFND         4     06
     O                       EHFYR         4     12
     O                       EHEMP#        4     22
     O                       EHDIV         4     30
     O                       ELFAF               35
     O                       EHFAF               42
     O                       XEPFDT              55 '  /  /    '
     O                       XEPTDT              68 '  /  /    '
     O                       XHPTDT              81 '  /  /    '
     O                       XMCAD1              94 '  /  /    '
     O                       XEDT               107 '  /  /    '
     O                       ELPGM              119
     O                       ELUSER             132
     O          E            COUNT3      1  1
     O                                           20 'TOTAL RECORDS:'
     O                       XXCNT3        1     26
     O          E            NOCNT3      1  1
     O                                           25 'NO RECORDS MATCHED'
