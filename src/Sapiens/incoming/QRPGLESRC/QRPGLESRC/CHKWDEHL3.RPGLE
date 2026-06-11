     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CHKWDEHL - CHECK FOR INCORRECT EMPLOYER HISTORY
     H*
     FWDEHL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWDELL1    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

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
     C*
     C     EHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     *LIKE         DEFINE    EHCO#         XXCLT
     C     *LIKE         DEFINE    EHCO#         XXCO#
     C     *LIKE         DEFINE    EHFND         XXFND
     C     *LIKE         DEFINE    EHFYR         XXFYR
     C     *LIKE         DEFINE    EHEMP#        XXEMP#
     C     *LIKE         DEFINE    EHDIV         XXDIV
     C     *LIKE         DEFINE    EHPFDT        XXFDT
     C     *LIKE         DEFINE    EHPFDT        XXTDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCLT             3
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFDT             8
     C                   PARM                    QQTDT             8
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
     C* SET UP KEY INFORMATION
     C*
     C                   MOVE      QQCLT         XXCLT
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFDT         X1FDT             8 0
     C                   MOVE      QQTDT         X1TDT             8 0
     C*
     C                   Z-ADD     X1FDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDT
     C*
     C                   Z-ADD     X1TDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDT
     C*
     C                   Z-ADD     0             XXFND
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C                   Z-ADD     0             XXFYR
     C*
     C                   Z-ADD     0             XXCNT             5 0
     C*
     C                   TIME                    DSDTTM
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
     C*
     C* READ EMPLOYER HISTORY
     C*
     C     EHKY          CHAIN     PEH01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C* FIND A RECORD THAT FALLS WITHIN CRITERIA
     C*
     C     EHTRDT        IFGE      XXFDT
     C     EHTRDT        ANDLE     XXTDT
     C*
     C                   Z-ADD     EHFND         XXFND
     C                   Z-ADD     EHFYR         XXFYR
     C                   Z-ADD     EHEMP#        XXEMP#
     C*
     C                   MOVE      *OFF          *IN80
     C*
     C     ELKY          CHAIN     PEL01                              70
     C*
     C     *IN70         DOWEQ     *OFF
     C     *IN80         ANDEQ     *OFF
     C*
     C     ELDIV         IFNE      EHDIV
     C*
     C     ELASP1        IFNE      EHASP1
     C     ELASP2        ORNE      EHASP2
     C     ELASP3        ORNE      EHASP3
     C     ELANDT        ORNE      EHANDT
     C     ELDISC        ORNE      EHDISC
     C     ELEXM1        ORNE      EHEXM1
     C     ELEXM2        ORNE      EHEXM2
     C     ELEXM3        ORNE      EHEXM3
     C     ELILCD        ORNE      EHILCD
     C     ELINL2        ORNE      EHI2CD
     C     ELDCDE        ORNE      EHDCDE
     C     ELASPM        ORNE      EHASPM
     C     ELASPS        ORNE      EHASPS
     C     ELDBCR        ORNE      EHDBCR
     C*
     C                   MOVE      *ON           *IN80
     C*
     C                   EXSR      PRINT
     C*
     C                   ENDIF                                                  EHASP1 IFNE
     C*
     C                   ENDIF                                                  EHDIV IFNE
     C*
     C  N80ELKY          READE     PEL01                                  70
     C                   ENDDO                                                  *IN70 DOWEQ
     C*
     C                   ENDIF                                                  EATRDT IFGE
     C*
     C* READ NEXT EMPLOYER HISTORY
     C*
     C     EHKY          READE     PEH01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* PRINT TOTAL COUNT
     C*
     C     XXCNT         IFNE      *ZERO
     C                   EXCEPT    COUNT
     C                   ELSE
     C                   EXCEPT    NOCNT
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
     C                   Z-ADD     EHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*
     C                   Z-ADD     EHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8 0
     C*
     C                   Z-ADD     EHIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXIDT             8 0
     C*
     C                   Z-ADD     EHDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT              8 0
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
     C*
     C*
     C*****************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'CHKWDEHL: '
     O                                           36 'PGM TO CHECK IF EMPLR '
     O                                           57 'HISTORY RECORD IS BAD'
     O                                           70 'RUN DATE:'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                                            8 'CLT:'
     O                       XXCLT         4     12
     O                                           21 'CO#:'
     O                       XXCO#         4     25
     O                                           38 'FROM DT:'
     O                       X1FDT               49 '  /  /    '
     O                                           60 'TO DT:'
     O                       X1TDT               71 '  /  /    '
     O                       XXCONM             124
     O          E            HEAD           2
     O                                            5 'CO#'
     O                                           11 'FUND'
     O                                           16 'FYR'
     O                                           27 'EMPLOYER#'
     O                                           34 'DIV'
     O                                           42 'STATUS'
     O                                           55 'EHTRDT'
     O                                           68 'EHEFDT'
     O                                           81 ' EHIDT'
     O                                           94 '  EHDT'
     O          E            DET            1
     O                       EHCO#         4      5
     O                       EHFND         4     11
     O                       EHFYR         4     16
     O                       EHEMP#        4     27
     O                       EHDIV         4     34
     O                       EHFAF               39
     O                       XXTRDT              55 '  /  /    '
     O                       XXEFDT              68 '  /  /    '
     O                       XXIDT               81 '  /  /    '
     O                       XXDT                94 '  /  /    '
     O          E            COUNT       1  1
     O                                           20 'TOTAL RECORDS:'
     O                       XXCNT         1     26
     O          E            NOCNT       1  1
     O                                           25 'NO RECORDS MATCHED'
