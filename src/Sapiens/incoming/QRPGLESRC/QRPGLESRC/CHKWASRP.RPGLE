     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CHKWASRP - CALCULATE S/R PREMIUM FOR GROUP/REPORTING MONTH/YEAR.
     H*
     H**************************************************************************
     H*  PROJECT: SUPP# 72246         DESC:  GENERATE A/R BATCH RECORDS FOR ALL
     H*  DATE:    04/14/2010                 ADJUSTMENTS NEEDED FOR A GROUP AND
     H*  PGMR:    C:65                       REPORTING MONTH/YEAR.
     H**************************************************************************
     FWDELL1    IF   E           K DISK
     F                                     INFDS(DBPEL0)
     FWASRL2    IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWMEPL     IF   E           K DISK
     FWMPRL1    IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D MD              S              2  0 DIM(12) CTDATA PERRCD(12)            # DAYS IN MONTHS
     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND
     D  CONAME                 1     40
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSDATE                 7     12  0
     D*
     D* Reporting month & year
     D                 DS
     D  XXRPMY                 1      6  0
     D  XXRPMN                 1      2  0
     D  XXRPYR                 3      6  0
     D                 DS
     D  XXREP                  1      8  0
     D  XXREPM                 1      2  0
     D  XXREPD                 3      4  0
     D  XXREPY                 5      8  0
     D                 DS
     D  XXRPDT                 1      7  0
     D  XXRPYM                 1      5  0
     D  XXRPDY                 6      7  0
     D DBPEL0          DS
     D  DBRRN                397    400B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     SRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXRPYM
     C*
     C     EPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEFDT
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXSPEC
     C*
     C*  KEY LIST FOR THE PAYROLL CLASS FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    XXCLAS
     C                   KFLD                    X1EFDT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    XXCLAS
     C*
     C*  KEY LIST FOR THE RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C*
     C*  KEY LIST FOR THE STAT FILE
     C*
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    X2EFDT
     C*
     C     RRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C*   KEY LIST FOR THE FUND DETAIL
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C*
     C     *LIKE         DEFINE    EPEFDT        X1EFDT
     C     *LIKE         DEFINE    EPEFDT        X2EFDT
     C     *LIKE         DEFINE    EPEFDT        XXEFDT
     C     *LIKE         DEFINE    EPCLAS        XXCLAS
     C     *LIKE         DEFINE    EPSPEC        XXSPEC
     C*
     C     *LIKE         DEFINE    RRTERR        XXTERR
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C     *LIKE         DEFINE    RSJRST        XXST
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
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQRPMY            6
     C                   PARM                    QQUPD             1
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQRPMY        XXRPMY
     C*
     C                   Z-ADD     XXRPMN        XXREPM
     C                   Z-ADD     XXRPYR        XXREPY
     C                   Z-ADD     1             XXREPD                         XXREP = BEGIN MDYY
     C                   Z-ADD     XXREP         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBGDT            7 0          XXBGDT = 7.0 BEGIN D
     C                   MOVEL     SRDTO         XXRPYM
     C                   MOVE      XXRPMN        MM                2 0
     C                   MOVE      MD(MM)        XXRPDY                         XXRPDT = 7.0 END DT
     C                   Z-ADD     XXRPDT        XXENDT            7 0          XXENDT = 7.0 END DT
     C*
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C                   Z-ADD     0             XXFYR
     C*
     C                   Z-ADD     0             XXCNT             5 0
     C                   Z-ADD     0             SVBAT#            9 0
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
     C* READ EMPLOYER DETAIL
     C*
     C     ELKY          CHAIN     PEL01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C* FIND A RECORD THAT FALLS WITHIN CRITERIA
     C*
     C                   Z-ADD     ELFYR         XXFYR
     C                   Z-ADD     ELEMP#        XXEMP#
     C                   Z-ADD     ELDIV         XXDIV
     C                   MOVE      ELRTCD        XXRTCD
     C*
     C* IF REPORTING PERIOD FALLS WITHIN POLICY PERIOD.
     C*
     C     XXBGDT        IFLT      ELPTDT
     C     XXENDT        ANDGE     ELPFDT
     C*
     C                   Z-ADD     0             XXMPRM           13 2
     C                   Z-ADD     0             XXMAPR           13 2
     C                   Z-ADD     0             XXTPAY           13 2
     C*
     C     SRKY          CHAIN     PSR01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      SRCLAS        XXCLAS
     C                   MOVE      SRSPEC        XXSPEC
     C                   MOVE      SREFDT        XXEFDT
     C*
     C     EPKY          CHAIN     PEP01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     0             X1EFDT
     C                   MOVE(P)   XXCO#         Q1CO#             3
     C                   MOVE(P)   XXFND         Q1FND             3
     C                   MOVE(P)   XXFYR         Q1FYR             3
     C                   MOVE(P)   XXEMP#        Q1EMP#            9
     C                   MOVE(P)   XXDIV         Q1DIV             5
     C                   MOVE(P)   XXRPDT        Q1RPDT            7
     C                   MOVE(P)   'P'           Q1TYPE            1
     C                   MOVE(P)   XXCLAS        Q1CLAS            5
     C                   CALL      'SREFFD'
     C                   PARM                    Q1CO#
     C                   PARM                    Q1FND
     C                   PARM                    Q1FYR
     C                   PARM                    Q1EMP#
     C                   PARM                    Q1DIV
     C                   PARM                    Q1RPDT
     C                   PARM                    Q1TYPE
     C                   PARM                    Q1CLAS
     C*
     C                   MOVE      Q1RPDT        X1EFDT
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C*
     C     PRPCF         IFEQ      'P'
     C                   ADD       SRAMTP        XXTPAY
     C     SRAMTP        DIV       100           X2WK11           13 4
     C                   ELSE
     C                   Z-ADD     SRAMTP        X2WK11
     C                   ENDIF
     C*
     C     EPENTR        MULT(H)   X2WK11        XXWK11           11 0
     C                   ADD       XXWK11        XXMPRM
     C*
     C     PRRT          MULT(H)   X2WK11        XXWK11           11 0
     C                   ADD       XXWK11        XXMAPR
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     SRKY          READE     PSR01                                  60
     C                   ENDDO
     C*
     C* CALCULATE TERRORISM CHARGE ON PAYROLL TOTAL.
     C*
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C*
     C     X1EFDT        IFNE      *ZERO
     C     X1EFDT        ANDNE     X2EFDT
     C                   MOVE      X1EFDT        X2EFDT
     C     XXST          IFEQ      'NJ'
     C                   Z-ADD     ELPFDT        X2EFDT
     C                   ENDIF
     C                   ENDIF
     C*
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C*
     C     RRTERR        IFNE      *ZERO
     C     RRDTRR        ORNE      *ZERO
     C*
     C     XXTPAY        DIV(H)    100           XXWK11
     C                   MOVE(P)   XXCO#         Q1CO#
     C                   MOVE(P)   XXFND         Q1FND
     C                   MOVE(P)   XXRTCD        Q1RTCD
     C                   MOVE(P)   RRTERC        Q1CLAS
     C                   MOVE(P)   X2EFDT        Q1EFDT
     C                   CALL      'SRLCM'
     C                   PARM                    Q1CO#             3
     C                   PARM                    Q1FND             3
     C                   PARM                    Q1RTCD            3
     C                   PARM                    Q1CLAS            5
     C                   PARM                    Q1EFDT            7
     C                   PARM      *BLANKS       Q1LCM             5
      *
     C     Q1LCM         IFNE      *BLANKS
     C                   MOVE      Q1LCM         XXLCM             5 4
     C                   ENDIF
      *
     C     RRTERL        IFNE      'Y'
      *
     C     RRTERR        MULT(H)   XXLCM         RRTERR
     C     RRDTRR        MULT(H)   XXLCM         RRDTRR
     C                   ENDIF                                                  RRTERL IFNE
      *
     C                   Z-ADD(H)  RRTERR        XXTERR
     C                   ADD(H)    RRDTRR        XXTERR
     C                   MULT(H)   XXTERR        XXWK11
     C*
     C                   ADD       XXWK11        XXMPRM
     C                   ENDIF                                                  RRTERR & RRDTRR
     C*
     C                   Z-ADD     DBRRN         @@RRN
     C                   MOVE      QQRPMY        @@RPMY
     C                   Z-ADD     0             @@TODT
     C                   Z-ADD     XXMAPR        @@MAPR
     C                   Z-ADD     0             @@ADLD
     C                   Z-ADD     XXTPAY        @@PAYR
     C                   Z-ADD     0             @@PREM
     C                   Z-ADD     0             @@TAX
     C                   Z-ADD     0             @@PPRM
     C                   Z-ADD     0             @@PTAX
     C                   MOVE      *BLANKS       @@FACT
     C                   Z-ADD     0             @@PREV
     C*
     C                   CALL      'SRPREM'
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RPMY            6 0
     C                   PARM                    @@TODT            8 0
     C                   PARM                    @@MAPR           11 0
     C                   PARM                    @@ADLD           11 0
     C                   PARM                    @@PAYR           11 0
     C                   PARM                    @@PREM           11 0
     C                   PARM                    @@TAX            11 0
     C                   PARM                    @@PPRM           11 0
     C                   PARM                    @@PTAX           11 0
     C                   PARM                    @@FACT          143
     C                   PARM                    @@PREV           11 2
     C*
     C     XXMPRM        SUB       @@PPRM        XXAMT            11 2
     C*
     C                   MOVE      *BLANKS       XXARCD            2
     C     F1KY          CHAIN     PF101                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      F1SRBB        XXARCD
     C                   ENDIF
     C*
     C* CREATE A/R BATCH RECORD FOR ADJUSTMENT AMOUNT.
     C*
     C     XXAMT         IFNE      *ZERO
     C     XXARCD        ANDNE     *BLANKS
     C*
     C                   MOVE(P)   XXCO#         Q1CO#             3
     C                   MOVE(P)   XXFND         Q1FND             3
     C                   MOVE(P)   XXFYR         Q1FYR             3
     C                   MOVE(P)   XXEMP#        Q1EMP#            9
     C                   MOVE(P)   XXDIV         Q1DIV             5
     C*
     C                   Z-ADD     XXRPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         Q1PODT
     C*
     C     XXRPDT        IFGT      ELPTDT
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         Q1PODT
     C                   ENDIF
     C*
     C                   MOVE      XXAMT         Q1AMT
     C                   MOVE      XXARCD        Q1ARCD
     C                   MOVE      SVBAT#        Q1BAT#
     C                   MOVE      *BLANK        Q1MODE
     C*
     C     QQUPD         IFEQ      'Y'
     C*
     C                   CALL      'SRBAT'
     C                   PARM                    Q1CO#             3
     C                   PARM                    Q1FND             3
     C                   PARM                    Q1EMP#            9
     C                   PARM                    Q1DIV             5
     C                   PARM                    Q1FYR             3
     C                   PARM                    Q1PODT            8
     C                   PARM                    Q1ARCD            2
     C                   PARM                    Q1AMT            11
     C                   PARM                    Q1BAT#            9
     C                   PARM                    Q1MODE            1
     C*
     C                   MOVE      Q1BAT#        SVBAT#
     C*
     C                   ENDIF                                                  QQUPD IFEQ
     C*
     C                   EXSR      PRINT
     C*
     C                   ENDIF                                                  XXAMT IFNE
     C*
     C                   ENDIF
     C*
     C* READ NEXT POLICY
     C*
     C     ELKY          READE     PEL01                                  50
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
     C                   MOVE      '1'           *INLR
     C*
     C*****************************************************************
     C*
     C* PRINT AN EMPLOYER TO THE REPORT
     C*
     C     PRINT         BEGSR
     C*
     C                   MOVE      Q1PODT        XXPODT            8 0
     C                   Z-ADD     @@PPRM        XXPPRM           11 2
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
     C*****************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'CHKWASRP:'
     O                                           38 'PGM TO CALCULATE S/R R'
     O                                           62 'EPORT AND POST ADJUSTMEN'
     O                                           77 'TS TO A/R BATCH'
     O                                          120 'RUN DATE:'
     O                       DSDATE        Y    130
     O          E            HEAD           2
     O                                           21 'CO#:'
     O                       XXCO#         4     25
     O                                           33 'FND:'
     O                       XXFND         4     37
     O                                           51 'MONTH/YEAR:'
     O                       XXRPMY              59 '  /    '
     O                       XXCONM             124
     O          E            HEAD           2
     O                                           06 'FUND'
     O                                           12 'FYR'
     O                                           22 'EMPLOYER#'
     O                                           30 'DIV'
     O                                           42 'BATCH'
     O                                           48 'CODE'
     O                                           60 'POST DATE'
     O                                           79 'NEW AMOUNT'
     O                                           95 'PRIOR BILLED'
     O                                          111 'ADJUSTMENT'
     O          E            DET            1
     O                       XXFND         4     06
     O                       XXFYR         4     12
     O                       XXEMP#        4     22
     O                       XXDIV         4     30
     O                       SVBAT#        4     42
     O                       XXARCD              47
     O                       XXPODT              60 '  /  /    '
     O                       XXMPRM        J     79
     O                       XXPPRM        J     95
     O                       XXAMT         J    111
     O          E            COUNT       1  1
     O                                           20 'TOTAL RECORDS:'
     O                       XXCNT         1     26
     O          E            NOCNT       1  1
     O                                           25 'NO RECORDS MATCHED'
     C*****************************************************************
** MD - NUMBER OF DAYS PER MONTH
312831303130313130313031
