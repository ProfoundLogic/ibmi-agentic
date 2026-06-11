     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE PRTDED1 - REPORT DEDUCTIBLE RECOVERY DISCREPANCIES
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     FWMFNL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMEAL     IF   E           K DISK
     FWDDDL1    IF   E           K DISK
     FWADQL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C**************************************************************************
     C*  KEY LIST FOR THE DEDUCTIBLE DETAIL FILE
     C*
     C     DDKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    DDFND
     C*
     C*  KEY LIST FOR THE EMPLOYER A/R FILE
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C**************************************************************************
     C*  ENTRY PARM LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C*
     C     *LIKE         DEFINE    FNCO#         XXCO#
     C     *LIKE         DEFINE    DDFND         SVFND
     C     *LIKE         DEFINE    DDEMP#        SVEMP#
     C     *LIKE         DEFINE    DDDIV         SVDIV
     C     *LIKE         DEFINE    DDAMTR        XXAMTR
     C     *LIKE         DEFINE    DDAMTR        XXPNDR
     C     *LIKE         DEFINE    EAAMT         XXAMTP
     C     *LIKE         DEFINE    EAARCD        XXRDPY
     C                   Z-ADD     0             XXWK92            9 2
     C                   Z-ADD     0             XXDIFF            9 2
     C                   Z-ADD     0             XTAMTP           11 2
     C                   Z-ADD     0             XTAMTR           11 2
     C                   Z-ADD     0             XTPNDR           11 2
     C                   Z-ADD     0             XTDIFF           11 2
     C                   Z-ADD     0             XXTOTP           11 2
     C                   Z-ADD     0             XXTOTR           11 2
     C                   Z-ADD     0             XXTOTD           11 2
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*
     C*  Get the deduct tax paid A/R code.
     C*
     C                   READ      PF101                                  60
     C     *IN60         DOWEQ     '0'
     C     F1DTXP        IFNE      *BLANKS
     C                   LEAVE
     C                   ENDIF
     C                   READ      PF101                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  Get the deduct paid A/R code.
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C                   MOVEL     TTKEY         XXDTXP            2
     C     TTCTYP        IFEQ      'R'
     C     TTTYPE        ANDEQ     'Q'
     C     XXDTXP        ANDNE     F1DTXP
     C                   MOVEL     TTKEY         XXRDPY
     C                   LEAVE
     C                   ENDIF                                                  TTCTYP IFEQ
     C                   READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C**************************************************************************
     C*  LOOP THROUGH ALL DEDUCTIBLE DETAIL RECORDS.  THIS PROGRAM
     C*  WILL PROCESS ONE CLAIM AT A TIME.
     C*
     C     DDKY          CHAIN     PDD01                              50
     C                   Z-ADD     DDFND         SVFND
     C                   Z-ADD     DDEMP#        SVEMP#
     C                   Z-ADD     DDDIV         SVDIV
     C     *IN50         DOWEQ     '0'
     C*
     C     DDFND         IFNE      SVFND
     C                   EXSR      EMPBRK
     C                   EXSR      FNDBRK
     C                   ENDIF
     C*
     C     DDEMP#        IFNE      SVEMP#
     C     DDDIV         ORNE      SVDIV
     C                   EXSR      EMPBRK
     C                   ENDIF
     C*
     C*  ACCUMULATE THE TOTAL DEDUCTIBLE RECOVERED.
     C*
     C                   ADD       DDAMTR        XXAMTR
     C*
     C     NEXT          TAG
     C     DDKY          READE     PDD01                                  50
     C                   ENDDO
     C*
     C*  End of Pgm
     C*
     C                   EXCEPT    TOT
     C                   MOVE      '1'           *INLR
     C**************************************************************************
     C*  DEDPD - THIS SUBROUTINE CALCULATES THE TOTAL AMOUNT OF DEDUCT
     C*           PAID BY AN EMPLOYER AND THE AMOUNT PENDING RECOVERY.
     C**************************************************************************
     C     DEDPD         BEGSR
     C*
     C                   Z-ADD     0             XXAMTP
     C     EAKY          CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     EAARCD        IFEQ      XXRDPY
     C                   ADD       EAAMT         XXAMTP
     C                   ENDIF
     C*
     C     EAKY          READE     PEA01                                  60
     C                   ENDDO
     C*
     C                   Z-ADD     0             XXPNDR
     C     EAKY          CHAIN     PDQ01                              60
     C     *IN60         DOWEQ     '0'
     C                   ADD       DQAMTR        XXPNDR
     C     EAKY          READE     PDQ01                                  60
     C                   ENDDO
     C*
     C                   ENDSR
     C*****************************************************************
     C* FNDBRK SUBROUTINE              ** SUBROUTINE **   F N D B R K *
     C*****************************************************************
     C     FNDBRK        BEGSR
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C     FNKY          SETGT     PDD01
     C                   GOTO      NEXT
     C                   ENDIF
     C*
     C                   Z-ADD     DDFND         SVFND
     C*
     C                   ENDSR
     C*****************************************************************
     C* EMPBRK SUBROUTINE              ** SUBROUTINE **   E M P B R K *
     C*****************************************************************
     C     EMPBRK        BEGSR
     C*
     C*  Calculate the deduct billed and compare to the amount paid.
     C*    If they are different, print on the report.
     C*
     C                   EXSR      DEDPD
     C     XXAMTR        ADD       XXPNDR        XXWK92
     C     XXAMTP        IFNE      XXWK92
     C     XXAMTP        SUB       XXWK92        XXDIFF
     C                   ADD       XXAMTR        XTAMTR
     C                   ADD       XXPNDR        XTPNDR
     C                   ADD       XXAMTP        XTAMTP
     C                   ADD       XXDIFF        XTDIFF
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ENDIF
     C*
     C                   ADD       XXAMTR        XXTOTR
     C                   ADD       XXPNDR        XXTOTD
     C                   ADD       XXAMTP        XXTOTP
     C                   Z-ADD     0             XXAMTR
     C                   Z-ADD     DDEMP#        SVEMP#
     C                   Z-ADD     DDDIV         SVDIV
     C*
     C                   ENDSR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 02
     O                       DSTIME              08 '  :  :  '
     O                                           45 'THIS REPORT PRINTS EMPLO'
     O                                           69 'YERS WHICH HAVE DISCREPA'
     O                                           93 'NCIES IN THE AMOUNT OF D'
     O                                          109 'EDUCT RECOVERIES'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                           09 'PRTDED1  '
     O                                          125 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           2
     O                                            5 'FUND'
     O                                           22 'EMPLOYER    DIV'
     O                                           36 'AMOUNT PAID'
     O                                           55 'AMT RECOVERED'
     O                                           74 'REC PENDING'
     O                                           93 'DIFFERENCE'
     O          E            DET            1
     O                       SVFND         4      4
     O                       SVEMP#        4     15
     O                       SVDIV         4     22
     O                       XXAMTP        J     37
     O                       XXAMTR        J     56
     O                       XXPNDR        J     75
     O                       XXDIFF        J     94
     O          E            TOT         1  2
     O                                           15 'TOTALS:'
     O                       XTAMTP        J     37
     O                       XTAMTR        J     56
     O                       XTPNDR        J     75
     O                       XTDIFF        J     94
     O          E            TOT         2  1
     O                                           21 'Total amount paid by'
     O                                           39 'all employers . :'
     O                       XXTOTP        J     55
     O          E            TOT         1  1
     O                                           21 'Total deductibles re'
     O                                           39 'covered . . . . :'
     O                       XXTOTR        J     55
     O          E            TOT         1  1
     O                                           21 'Total deductibles pe'
     O                                           39 'nding recovery  :'
     O                       XXTOTD        J     55
