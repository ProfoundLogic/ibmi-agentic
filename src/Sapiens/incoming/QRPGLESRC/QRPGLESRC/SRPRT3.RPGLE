     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE SRPRT3 - EARNED PAYROLL/PREMIUM FOR S/R EMPLOYERS
     H*****************************************************************
     H* CREATED:  03/16/00   C:61  PROJECT WC741                      *
     H*****************************************************************
     H*                                                               *
     H* Calculates earned S/R payroll and premium year to date and    *
     H* for month entered.                                            *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT WC1473                                 *
     H*                CHANGE TO RETURN PAYROLL AND PREMIUM VALUES    *
     H*                SPECIFIC TO PAYROLL CLASS CODE EXTENSION.      *
     H*                                                               *
     H* 01/24/06 C:65 PROJECT 114WHO                                  *
     H*               USE THE RETROSPECTIVE PREMIUM IF POPULATED FOR  *
     H*               PREMIUM BASIS.                                  *
     H*                                                               *
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
      *
      * Employer Detail
     FWDELL     IF   E           K DISK
      *
      * Employer Payroll
     F******BEG CHG****04/20/09*************************************
     FWMEPL7E   IF   E           K DISK
     F******* WMEPL7  IF  E           K        DISK
      *
      * Employer Self-Reporting Payroll
     FWASRL4E   IF   E           K DISK
     F******* WASRL4  IF  E           K        DISK
     F******END CHG****04/20/09*************************************
      *
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *===========================================================
      *
      *  KLIST
      *
      *
      *  Employer Detail
      *

     C     ELKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
      *
      *  Employer Self - Reporting Payroll
      *
     C     SRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
      *
     C     SRKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKCLAS
     C                   KFLD                    KKSPEC
      *
      *  Employer Payroll
      *
     C     EPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
      *
     C     EPKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKCLAS
     C                   KFLD                    KKSPEC
     C*******BEG ADD******04/20/09********************************
     C                   KFLD                    KKEXT             5
     C*******END ADD******04/20/09********************************
      *
     C     EPKY2         KLIST
     C                   KFLD                    SRCO#
     C                   KFLD                    SRFND
     C                   KFLD                    SRFYR
     C                   KFLD                    SREMP#
     C                   KFLD                    SRDIV
     C                   KFLD                    SRCLAS
     C                   KFLD                    SRSPEC
     C*******BEG ADD******04/20/09********************************
     C                   KFLD                    SREXT
     C*******END ADD******04/20/09********************************
      *
      *============================================================
      *
      *   *LIKE defines
      *
     C     *LIKE         DEFINE    ELPFDT        XXRNDT
     C     *LIKE         DEFINE    ELPTDT        XXTODT
     C     *LIKE         DEFINE    ELCO#         KKCO#
     C     *LIKE         DEFINE    ELFND         KKFND
     C     *LIKE         DEFINE    ELEMP#        KKEMP#
     C     *LIKE         DEFINE    ELDIV         KKDIV
     C     *LIKE         DEFINE    ELFYR         KKFYR
     C     *LIKE         DEFINE    SRCLAS        KKCLAS
     C     *LIKE         DEFINE    SRSPEC        KKSPEC
      *
     C     *LIKE         DEFINE    SRCO#         SVCO#
     C     *LIKE         DEFINE    SRFND         SVFND
     C     *LIKE         DEFINE    SRFYR         SVFYR
     C     *LIKE         DEFINE    SREMP#        SVEMP#
     C     *LIKE         DEFINE    SRDIV         SVDIV
     C     *LIKE         DEFINE    SRCLAS        SVCLAS
     C     *LIKE         DEFINE    SRSPEC        SVSPEC
     C     *LIKE         DEFINE    SRRPMY        SVRPMY
     C     *LIKE         DEFINE    SRAMTP        SVAMTP
      *
      *=============================================================
      *
      *  PLIST definitions
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@EMP#            9
     C                   PARM                    @@DIV             5
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CLAS            5
     C                   PARM                    @@SPEC            1
     C*********BEG ADD***04/20/09**********************************
     C                   PARM                    @@EXT             5
     C*********END ADD***04/20/09**********************************
     C                   PARM                    @@RNDT            8
     C                   PARM                    @@AMT1           11            S/R YTD PYRL
     C                   PARM                    @@AMT2           11            S/R YTD PRM
     C                   PARM                    @@AMT3           11            S/R MTH PYRL
     C                   PARM                    @@AMT4           11            S/R MTH PRM
      *
      *  Calculations are for CLAS entered.  If CLAS = 0 then calc for
      *  all classes.
      *
      *    - AMT1 = Earned S/R payroll for the year.       11.2
      *    - AMT2 = Earned S/R premium for the year.       11.2
      *    - AMT3 = Earned S/R payroll for the month.      11.2
      *    - AMT4 = Earned S/R premium for the month.      11.2
      *
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *
      *============================================================
      *  Begin
      *============================================================
      *
      *  INITIALIZE
      *
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@EMP#        KKEMP#
     C                   MOVE      @@DIV         KKDIV
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CLAS        KKCLAS
     C                   MOVE      @@SPEC        KKSPEC
      *
     C                   MOVEL     @@RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
      *
     C                   Z-ADD     *ZERO         XXAMT1           11 2
     C                   Z-ADD     *ZERO         XXAMT2           11 2
     C                   Z-ADD     *ZERO         XXAMT3           11 2
     C                   Z-ADD     *ZERO         XXAMT4           11 2
      *
     C     KKCLAS        IFNE      *ZERO
     C                   SETON                                        71
     C                   ENDIF
      *
      *  READ EMPLOYER DETAIL
      *
     C     ELKY          CHAIN     PEL01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
      *   Use Audited amounts if available
      *
     C     ELFAF         IFEQ      'A'
     C                   EXSR      GETAAP
      *
     C                   ELSE
      *
      *   Calc Earned Payroll / Premium
      *
     C                   EXSR      SRAMT
      *
     C                   ENDIF                                                  ELFAF
      *
     C                   ENDIF                                                  *IN60 = *OFF
      *
      *
     C                   MOVE      XXAMT1        @@AMT1
     C                   MOVE      XXAMT2        @@AMT2
     C                   MOVE      XXAMT3        @@AMT3
     C                   MOVE      XXAMT4        @@AMT4
      *
      *
     C                   MOVE      '1'           *INLR
      *
      *
      *
      *================================================================
      * GETAAP - Get Audited Annual Self Reported Payroll
      *================================================================
     C     GETAAP        BEGSR
      *
      * Get audited payroll for all classes
      *
     C                   Z-ADD     *ZERO         XXAPAY           15 2
     C     EPKY          CHAIN     PEP01                              60        ALL CLASSES
      *
     C     *IN60         DOWEQ     *OFF
     C                   ADD       EPAAPR        XXAPAY
     C     EPKY          READE     PEP01                                  60     ALL CLASSES
     C                   ENDDO
      *
     C                   Z-ADD     XXAPAY        XXAMT1
      *
      * Get audited Premium for all classes
      *
     C     ELAAPR        SUB       ELPRTD        XXAPRM           15 2
     C*** BEGIN ADD ***  01/24/06  ************************************
     C     ELSTAM        IFNE      *ZERO
     C     ELSTAM        SUB       ELPRTD        XXAPRM           15 2
     C                   ENDIF
     C*** END ADD *****  01/24/06  ************************************
     C                   Z-ADD     XXAPRM        XXAMT2
      *
      *
     C     *IN71         IFEQ      *ON
      *
      * Get Payroll for single class
      *
     C     EPKY1         CHAIN     PEP01                              60        ONE CLASS
     C                   Z-ADD     EPAAPR        XXAMT1
      *
      * Get Premium for single class
      *
     C     XXAPAY        IFNE      *ZERO
     C     XXAPRM        DIV       XXAPAY        XXRATO           20 8
     C     XXRATO        MULT      EPAAPR        XXAMT2
      *
     C                   ELSE
      *
     C                   Z-ADD     *ZERO         XXAMT2
     C                   ENDIF                                                  XXAPAY
      *
     C                   ENDIF                                                  *IN71
      *
     C                   ENDSR                                                  GETAAP
      *
      *
      *
      *================================================================
      * MTHDIF - Determine number of months difference
      *================================================================
     C     MTHDIF        BEGSR
      *
     C                   MOVE      MTH1          XXM1              2 0
     C                   MOVEL     MTH1          XXCY1             3 0
     C                   MOVE      MTH2          XXM2              2 0
     C                   MOVEL     MTH2          XXCY2             3 0
      *
     C     XXCY1         MULT      12            XXTMP1           15 0
     C     XXCY2         MULT      12            XXTMP2           15 0
      *
     C                   ADD       XXM1          XXTMP1
     C                   ADD       XXM2          XXTMP2
      *
     C     XXTMP1        SUB       XXTMP2        XXMDIF           15 0
      *
     C                   ENDSR                                                  MTHDIF
      *
      *
      *
      *================================================================
      * SRAMT - Calc Earned S/R Payroll and Premium amounts
      *================================================================
     C     SRAMT         BEGSR
      *
     C                   MOVEL     ELPFDT        XXFRMY            5 0
      *
     C                   MOVE      ELPTDT        XXTMP9            4
      *
     C     XXTMP9        IFEQ      '0101'
      *
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##DATE
      *
     C                   CALL      'SRPAST8'
     C                   PARM                    ##DATE            8 0
     C                   PARM      1             ##DAYS            5 0
     C                   PARM                    ##ERR             1
      *
     C                   MOVEL     ##DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
      *
     C                   ELSE
      *
     C                   MOVEL     ELPTDT        XXTODT
     C                   ENDIF                                                  XXTMP9
      *
     C                   MOVEL     XXTODT        XXTOMY            5 0
     C                   MOVEL     XXRNDT        XXRNMY            5 0
      *
     C  N71SRKY          CHAIN     PSR01                              60        ALL CLASSES
     C   71SRKY1         CHAIN     PSR01                              60        ONE CLASS
      *
     C                   EXSR      STRPAY
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     SRCO#         IFNE      SVCO#
     C     SRFND         ORNE      SVFND
     C     SRFYR         ORNE      SVFYR
     C     SREMP#        ORNE      SVEMP#
     C     SRDIV         ORNE      SVDIV
     C     SRCLAS        ORNE      SVCLAS
     C     SRSPEC        ORNE      SVSPEC
      *
     C                   EXSR      CLSBRK
     C                   EXSR      STRPAY
      *
     C                   ENDIF                                                  SRCO#
      *
     C                   EXSR      ADDPAY
      *
     C  N71SRKY          READE     PSR01                                  60     ALL CLASSES
     C   71SRKY1         READE     PSR01                                  60     ONE CLASS
     C                   ENDDO
      *
     C                   EXSR      CLSBRK
      *
     C                   ENDSR                                                  SRAMT
      *
      *
      *
      *================================================================
      * STRPAY - INITIALIZE FOR NEXT CLASS ACCUMULATION
      *================================================================
     C     STRPAY        BEGSR
      *
     C                   MOVE      SRCO#         SVCO#
     C                   MOVE      SRFND         SVFND
     C                   MOVE      SRFYR         SVFYR
     C                   MOVE      SREMP#        SVEMP#
     C                   MOVE      SRDIV         SVDIV
     C                   MOVE      SRCLAS        SVCLAS
     C                   MOVE      SRSPEC        SVSPEC
      *
      *  Get Rate
      *
     C     EPKY2         CHAIN     PEP01                              61
      *
     C                   MOVEL     ELPFDT        MTH2              5 0
      *
     C                   Z-ADD     *ZERO         XXMISS                         MISS MONTH CNT
     C                   Z-ADD     *ZERO         XXPRC            15 2          PYRLL FOR CLASS
     C                   Z-ADD     *ZERO         SVRPMY
     C                   Z-ADD     *ZERO         SVAMTP
     C                   Z-ADD     *ZERO         XXMCNT            5 0          MONTH COUNT
      *
     C                   ENDSR                                                  STRPAY
      *
      *
      *
      *================================================================
      * ADDPAY - Accumulate Earned payroll for class
      *================================================================
     C     ADDPAY        BEGSR
      *
     C     SRRPMY        IFGE      XXFRMY
     C     SRRPMY        ANDLE     XXTOMY
     C     SRRPMY        ANDLE     XXRNMY
      *
      *  Accumulate missing months
      *
     C                   Z-ADD     SRRPMY        MTH1              5 0
     C                   EXSR      MTHDIF
     C                   SUB       1             XXMDIF
      *
     C     XXMDIF        IFGT      *ZERO
     C                   ADD       XXMDIF        XXMISS           10 0
     C                   ENDIF
      *
     C                   Z-ADD     SRRPMY        MTH2
      *
      *  Accumulate earned payroll for class
      *
     C                   ADD       SRAMTP        XXPRC
     C                   ADD       1             XXMCNT            5 0
     C                   Z-ADD     SRRPMY        SVRPMY
     C                   Z-ADD     SRAMTP        SVAMTP
      *
     C                   ENDIF
      *
     C                   ENDSR                                                  ADDPAY
      *
      *
      *
      *================================================================
      * CLSBRK - PAYROLL CLASS BREAK
      *================================================================
     C     CLSBRK        BEGSR
      *
      *  Get average of reported months
      *
     C     XXMCNT        IFNE      *ZERO
     C     XXPRC         DIV       XXMCNT        XXAVE            15 2
      *
     C                   ELSE
      *
     C                   Z-ADD     *ZERO         XXAVE
     C                   ENDIF
      *
      *  Determine any remaining missing months
      *
     C     SVRPMY        IFNE      XXRNMY
     C     SVRPMY        ANDNE     XXTOMY
      *
     C     XXRNMY        IFLT      XXTOMY
     C                   Z-ADD     XXRNMY        MTH1
      *
     C                   ELSE
      *
     C                   Z-ADD     XXTOMY        MTH1
     C                   ENDIF                                                  XXRNMY
      *
     C                   Z-ADD     SVRPMY        MTH2
     C                   EXSR      MTHDIF
     C                   SUB       1             XXMDIF
      *
     C     XXMDIF        IFGT      *ZERO
     C                   ADD       XXMDIF        XXMISS           10 0
     C                   ENDIF
      *
     C                   ENDIF
      *
      *  Prorate last month payroll
      *
     C                   MOVE      XXRNDT        XXTMP7            2 0
     C     XXTMP7        DIV       30            XXTMP3           10 5
      *
     C                   SELECT
      *
     C     SVRPMY        WHENEQ    XXRNMY
     C                   SUB       SVAMTP        XXPRC
     C     SVAMTP        MULT      XXTMP3        XXTMP4           11 2
      *
     C     SVRPMY        WHENLT    XXRNMY
     C     XXRNMY        ANDLE     XXTOMY
     C     XXAVE         MULT      XXTMP3        XXTMP4           11 2
      *
     C     XXRNMY        WHENGT    XXTOMY
     C                   Z-ADD     XXAVE         XXTMP4
      *
     C                   OTHER
     C                   Z-ADD     *ZERO         XXTMP4
      *
     C                   ENDSL
      *
      *  Accumulate Payroll and Premium for month
      *
     C                   ADD       XXTMP4        XXPRC
      *
     C     XXRNMY        IFLE      XXTOMY
     C                   ADD       XXTMP4        XXAMT3
      *
     C     EPARAT        MULT      XXTMP4        XXTMP6           15 5
     C                   ADD       XXTMP6        XXAMT4
     C                   ENDIF                                                  RNMY <= TOMY
      *
      *  Add missing month payroll
      *
     C     XXMISS        MULT      XXAVE         XXTMP8           15 2
     C                   ADD       XXTMP8        XXPRC
      *
      *  Accumulate Payroll / Premium for year
      *
     C                   ADD       XXPRC         XXAMT1
      *
     C     EPARAT        MULT      XXPRC         XXTMP6           15 5
     C                   ADD       XXTMP6        XXAMT2
      *
     C                   ENDSR                                                  CLSBRK
      *
      *
