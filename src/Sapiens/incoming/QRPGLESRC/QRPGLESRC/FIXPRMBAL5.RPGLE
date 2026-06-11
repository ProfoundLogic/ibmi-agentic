     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIX PROGRAM - FIXPRMBAL5
     H*****************************************************************
     H* Description:  LIST EMPLOYERS WITH A DIFFERENCE BETWEEN BILLED *
     H*               AND CALCULATED PREMIUM.  S/R EMPLOYERS WILL NOT *
     H*               BE LISTED UNLESS THEY ARE AUDITED SINCE BILLED  *
     H*               PREMIUM ON THOSE ACCOUNTS WILL NOT MATCH CALCU- *
     H*               CX A/R HOLDING FILE IS ADDED TO BILLED PREMIUM  *
     H*               AS WELL.                                        *
     H*****************************************************************
     FWDELL1    IF   E           K DISK
     FWMEAL8    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMXAL1    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  CX                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECTIVE DATE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0

     C**************************************************************************
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELFND         SVFND
     C     *LIKE         DEFINE    ELFYR         SVFYR
     C     *LIKE         DEFINE    ELAAPR        XXANPR
     C     *LIKE         DEFINE    ELAAPR        XXPREM
     C     *LIKE         DEFINE    ELAAPR        XXCXAR
     C     *LIKE         DEFINE    ELAAPR        XXDIFF
     C     *LIKE         DEFINE    ELAAPR        XXTOT
     C     *LIKE         DEFINE    ELAAPR        XTTOT
     C     *LIKE         DEFINE    BSKEY         XXKEY
     C     *LIKE         DEFINE    CONAME        XXCONM
     C*
     C*  Key list for the Employer Detail file
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     EAKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C     XAKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C     TBKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    XXKEY
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELFND
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
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
     C                   MOVE      *OFF          *INOF
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   Z-ADD     0             XXTOT
     C                   Z-ADD     0             XXCNT
     C                   Z-ADD     0             XTTOT
     C                   Z-ADD     0             XTCNT
     C                   Z-ADD     0             SVFND
     C                   Z-ADD     0             SVFYR
     C*
     C     XXFND         IFNE      *ZERO
     C                   MOVE      *ON           *IN67
     C                   ENDIF
     C*
     C     XXFND         IFNE      *ZERO
     C     XXFYR         ANDNE     *ZERO
     C                   MOVE      *ON           *IN68
     C                   ENDIF
     C*
     C     XXFND         IFEQ      *ZERO
     C     XXFYR         ANDNE     *ZERO
     C                   MOVE      *ON           *IN69
     C                   ENDIF
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        XXCONM
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        XXFNNM           25
     C*
     C                   EXCEPT    HEAD
     C*
     C*  Start main loop
     C*
     C  N67ELKEY         SETLL     PEL01
     C  N67ELKEY         READE     PEL01                                  60
     C   67ELKY          SETLL     PEL01
     C   67ELKY          READE     PEL01                                  60
     C   68ELKY2         SETLL     PEL01
     C   68ELKY2         READE     PEL01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     ELFND         SVFND
     C                   Z-ADD     ELFYR         SVFYR
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     ELFAF         IFNE      *BLANK
     C*
     C     *IN69         IFEQ      *OFF
     C     *IN69         OREQ      *ON
     C     ELFYR         ANDEQ     XXFYR
     C*
     C                   MOVEL     ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C*
     C     BSSEFL        IFEQ      'E'
     C     ELFAF         OREQ      'A'
     C*
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAAPR        XXANPR
     C                   ELSE
     C                   Z-ADD     ELANPR        XXANPR
     C                   MOVE      *ON           *IN21
     C                   ENDIF
     C*
     C* READ EMPLR A/R FILE.
     C*
     C     EAKY          SETLL     PEA01
     C     EAKY          READE     PEA01                                  60
     C     *IN60         DOWEQ     *OFF
     C     EAARTY        IFEQ      'B'
     C     EAARTY        OREQ      'A'
     C     EAARTY        OREQ      'D'
     C                   ADD       EAAMT         XXPREM
     C                   ENDIF
     C     EAKY          READE     PEA01                                  60
     C                   ENDDO
     C*
     C* ADD IN A/R FROM CX DELETED A/R FILE.
     C*
     C     XAKY          SETLL     PXA01
     C     XAKY          READE     PXA01                                  60
     C     *IN60         DOWEQ     '0'
     C     ELFAF         ANDNE     'A'
     C*
     C     XAARTY        IFEQ      'B'
     C     XAARTY        OREQ      'A'
     C     XAARTY        OREQ      'D'
     C                   ADD       XAAMT         XXCXAR
     C                   ENDIF
     C*
     C     XAKY          READE     PXA01                                  60
     C                   ENDDO
     C*
     C     XXANPR        SUB       XXPREM        XXDIFF
     C                   SUB       XXCXAR        XXDIFF
     C                   MULT      -1            XXDIFF
     C*
     C     XXDIFF        IFNE      *ZERO
     C*
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVEL     EMNAME        XXNAME
     C*
     C                   Z-ADD     1             Y                 1 0
     C     *ZERO         LOOKUP    EF(Y)                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   ENDIF
     C*
     C                   Z-ADD     CX(Y)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCXDT            8 0
     C     XXCXDT        IFEQ      *ZEROS
     C                   MOVE      *ON           *IN20
     C                   ENDIF
     C*
     C   OF              EXCEPT    HEAD
     C*
     C     SVFND         IFNE      ELFND
     C     SVFYR         ORNE      ELFYR
     C*
     C                   MOVE      *OFF          *IN61
     C                   EXCEPT    TOT
     C                   Z-ADD     ELFND         SVFND
     C                   Z-ADD     ELFYR         SVFYR
     C                   Z-ADD     0             XXCNT
     C                   Z-ADD     0             XXTOT
     C*
     C                   ENDIF
     C*
     C   OF              EXCEPT    HEAD
     C*
     C                   EXCEPT    DET
     C*
     C                   ADD       1             XXCNT             4 0
     C                   ADD       XXDIFF        XXTOT
     C                   ADD       1             XTCNT             4 0
     C                   ADD       XXDIFF        XTTOT
     C*
     C                   ENDIF                                                  XXDIFF IFNE
     C*
     C                   ENDIF                                                  BSSEFL IFEQ
     C*
     C                   ENDIF                                                  *IN69 IFEQ
     C*
     C                   ENDIF                                                  ELFAF IFNE
     C*
     C  N67ELKEY         READE     PEL01                                  60
     C   67ELKY          READE     PEL01                                  60
     C   68ELKY2         READE     PEL01                                  60
     C                   Z-ADD     *ZEROS        XXANPR
     C                   Z-ADD     *ZEROS        XXPREM
     C                   Z-ADD     *ZEROS        XXCXAR
     C                   Z-ADD     *ZEROS        XXDIFF
     C                   MOVE      *BLANKS       XXNAME
     C                   MOVE      *OFF          *IN20
     C                   MOVE      *OFF          *IN21
     C*
     C                   ENDDO
     C*
     C                   EXCEPT    TOT
     C*
     C                   Z-ADD     0             SVFND
     C                   Z-ADD     0             SVFYR
     C                   Z-ADD     XTCNT         XXCNT
     C                   Z-ADD     XTTOT         XXTOT
     C                   MOVE      *ON           *IN61
     C                   EXCEPT    TOT
     C*
     C                   ELSE
     C                   EXCEPT    NOREC
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM              86
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           67 'CALCULATED PREMIUM VS.'
     O                                           82 'BILLED PREMIUM'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            6 'GROUP:'
     O                       XXFND               10
     O                       XXFNNM              37
     O                                           54 'POLICY YEAR:'
     O                       XXFYR         4     58
     O          E            HEAD           2
     O                                           23 '* EXCLUDES A/R CODES'
     O                                           29 'WO/DI'
     O          E            HEAD           1
     O                                           45 'WDELP'
     O                                           60 'WMEAP'
     O                                           75 'WMXAP'
     O                                           99 'AUDIT'
     O                                          111 'BILL'
     O                                          126 'CANCEL'
     O          E            HEAD           2
     O                                            5 'FND'
     O                                           10 'FYR'
     O                                           22 'EMP#'
     O                                           29 'DIV'
     O                                           45 'PREM'
     O                                           60 'PREM *'
     O                                           75 'PREM *'
     O                                           90 'DIFF.'
     O                                           99 'STATUS'
     O                                          111 'TYPE'
     O                                          126 'DATE'
     O          E            DET
     O                       ELFND         4      5
     O                       ELFYR         4     10
     O                       ELEMP#        4     22
     O                       ELDIV         4     29
     O                       XXANPR        P     45
     O                       XXPREM        P     60
     O                       XXCXAR        P     75
     O                       XXDIFF        P     90
     O                       ELFAF               99
     O                       BSSEFL             111
     O              N20      XXCXDT        Y    126
     O          E            TOT         2  2
     O              N61                          31 'COUNT OF RECORDS WITH'
     O              N61                          51 'DIFFERENT PREMIUMS:'
     O               61                          31 'COUNT OF RECORDS WITH'
     O               61                          52 'DIFFERENT PREM(ALL):'
     O                       XXCNT         4     57
     O                                           75 'TOTAL DIFF:'
     O                       XXTOT         P     90
     O          E            NOREC
     O                                           26 '*** NO RECORDS FOUND ***'
