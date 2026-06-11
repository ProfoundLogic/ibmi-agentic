     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIX PROGRAM - FIXPRMBAL
     H*****************************************************************
     H* Description:  This pgm prints a list of employers with a      *
     H*   calculated premium different to billed A/R.                 *
     H*                                                               *
     H*****************************************************************
     FWDELL1    IF   E           K DISK
     FWMEAL8    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOA)

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
     C     *LIKE         DEFINE    ELAAPR        XXANPR
     C     *LIKE         DEFINE    ELAAPR        XXPREM
     C     *LIKE         DEFINE    ELAAPR        XXDIFF
     C     *LIKE         DEFINE    BSKEY         XXKEY
     C     *LIKE         DEFINE    CONAME        XXCONM
     C*
     C*  Key list for the Employer Detail file
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     EAKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
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
     C                   KFLD                    XXFND
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
     C                   EXCEPT    HEAD1
     C*
     C*  Start main loop
     C*
     C     ELKY          SETLL     PEL01
     C     ELKY          READE     PEL01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     *IN60         DOWEQ     *OFF
     C*
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAAPR        XXANPR
     C                   ELSE
     C                   Z-ADD     ELANPR        XXANPR
     C                   MOVE      *ON           *IN21
     C                   ENDIF
     C*
     C     EAKY          SETLL     PEA01
     C     EAKY          READE     PEA01                                  60
     C     *IN60         DOWEQ     *OFF
     C     EAARCD        IFNE      'WO'
     C     EAARCD        ANDNE     'DI'
     C     EAARTY        IFEQ      'B'
     C     EAARTY        OREQ      'A'
     C     EAARTY        OREQ      'D'
     C                   ADD       EAAMT         XXPREM
     C                   ENDIF
     C                   ENDIF                                                  ARCD<>'WO'
     C     EAKY          READE     PEA01                                  60
     C                   ENDDO
     C*
     C     XXANPR        SUB       XXPREM        XXDIFF
     C*
     C   OF              EXCEPT    HEAD
     C   OA              EXCEPT    HEAD1
     C     XXANPR        IFNE      XXPREM
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
     C                   MOVEL     ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C*
     C                   EXCEPT    DET
     C   21              EXCEPT    DET1
     C                   ADD       1             XXCNT             4 0
     C   21              ADD       1             XXSRCT            4 0
     C                   ENDIF
     C*
     C     ELKY          READE     PEL01                                  60
     C                   Z-ADD     *ZEROS        XXANPR
     C                   Z-ADD     *ZEROS        XXPREM
     C                   Z-ADD     *ZEROS        XXDIFF
     C                   MOVE      *BLANKS       XXNAME
     C                   MOVE      *OFF          *IN20
     C                   MOVE      *OFF          *IN21
     C*
     C                   ENDDO
     C                   EXCEPT    TOT
     C                   EXCEPT    TOT1
     C                   ELSE
     C                   EXCEPT    NOREC
     C   21              EXCEPT    NOREC1
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
     O                                           65 'CALC'
     O                                           83 'BILLED'
     O                                          108 'AUDIT'
     O                                          117 'BILL'
     O                                          132 'CANCEL'
     O          E            HEAD           2
     O                                            9 'EMP#'
     O                                           15 'DIV'
     O                                           24 'NAME'
     O                                           65 'PREM'
     O                                           85 'PREM *'
     O                                           97 'DIFF.'
     O                                          108 'STAT'
     O                                          117 'TYPE'
     O                                          132 'DATE'
     O          E            DET
     O                       ELEMP#        4      9
     O                       ELDIV         4     15
     O                       XXNAME              50
     O                       XXANPR        3     65
     O                       XXPREM        3     83
     O                       XXDIFF        P     97
     O                       ELFAF              108
     O                       BSSEFL             117
     O              N20      XXCXDT        Y    132
     O          E            TOT         2
     O                                           31 'COUNT OF RECORDS WITH'
     O                                           51 'DIFFERENT PREMIUMS:'
     O                       XXCNT         4     56
     O          E            NOREC
     O                                           26 '*** NO RECORDS FOUND ***'
     OQSYSPRT1  E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM              86
     O                       DSDATE        Y    132
     O          E            HEAD1          2
     O                       Q1PGM               10
     O                                           67 'CALCULATED PREMIUM VS.'
     O                                           82 'BILLED PREMIUM'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD1          2
     O                                            6 'GROUP:'
     O                       XXFND               10
     O                       XXFNNM              37
     O                                           54 'POLICY YEAR:'
     O                       XXFYR         4     58
     O          E            HEAD1          2
     O                                           23 '* EXCLUDES A/R CODES'
     O                                           29 'WO/DI'
     O          E            HEAD1          1
     O                                           65 'CALC'
     O                                           83 'BILLED'
     O                                          108 'AUDIT'
     O                                          117 'BILL'
     O                                          132 'CANCEL'
     O          E            HEAD1          2
     O                                            9 'EMP#'
     O                                           15 'DIV'
     O                                           24 'NAME'
     O                                           65 'PREM'
     O                                           85 'PREM *'
     O                                           97 'DIFF.'
     O                                          108 'STAT'
     O                                          117 'TYPE'
     O                                          132 'DATE'
     O          E            DET1
     O                       ELEMP#        4      9
     O                       ELDIV         4     15
     O                       XXNAME              50
     O                       XXANPR        3     65
     O                       XXPREM        3     83
     O                       XXDIFF        P     97
     O                       ELFAF              108
     O                       BSSEFL             117
     O              N20      XXCXDT        Y    132
     O          E            TOT1        2
     O                                           31 'COUNT OF S/R ACCOUNTS'
     O                                           53 'NOT AUDITED W/ DIFFER'
     O                                           58 'ENCE:'
     O                       XXSRCT        4     63
     O          E            NOREC1
     O                                           26 '*** NO RECORDS FOUND ***'
