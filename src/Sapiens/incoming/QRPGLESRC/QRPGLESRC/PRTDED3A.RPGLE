     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE PRTDED3 - PRINT DEDUCTIBLE BILLING DISCREPANCIES
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     FWMEML     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMEAL     IF   E           K DISK
     FWDDDL1    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C**************************************************************************
     C*
     C*  KEY LIST FOR THE EMPLOYER A/R FILE
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C**************************************************************************
     C*  ENTRY PARM LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C*
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    DDAMTB        XXDDBL
     C     *LIKE         DEFINE    EAAMT         XXEABL
     C     *LIKE         DEFINE    EAARCD        XXRDBL
     C                   Z-ADD     0             XXDIFF            9 2
     C                   Z-ADD     0             XTDDBL           11 2
     C                   Z-ADD     0             XTEABL           11 2
     C                   Z-ADD     0             XTDIFF           11 2
     C                   Z-ADD     0             XXTOT1           11 2
     C                   Z-ADD     0             XXTOT2           11 2
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*
     C*  Get the deduct billed A/R code.
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      'R'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XXRDBL
     C                   LEAVE
     C                   ENDIF                                                  TTCTYP IFEQ
     C                   READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C**************************************************************************
     C*  LOOP THROUGH ALL EMPLOYERS
     C*
     C                   READ      PEM01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C*  ACCUMULATE THE TOTAL DEDUCTIBLE BILLED IN A/R.
     C*
     C                   Z-ADD     0             XXEABL
     C     EAKY          CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C     EAARCD        IFEQ      XXRDBL
     C                   ADD       EAAMT         XXEABL
     C                   ENDIF
     C     EAKY          READE     PEA01                                  60
     C                   ENDDO
     C*
     C*  ACCUMULATE THE TOTAL DEDUCTIBLE BILLED.
     C*
     C                   Z-ADD     0             XXDDBL
     C     EAKY          CHAIN     PDD01                              60
     C     *IN60         DOWEQ     '0'
     C                   ADD       DDAMTB        XXDDBL
     C     EAKY          READE     PDD01                                  60
     C                   ENDDO
     C*
     C*  IF THE DEDUCT BILLING TOTALS DON'T MATCH, PRINT ON REPORT.
     C*
     C     XXDDBL        IFNE      XXEABL
     C     XXEABL        SUB       XXDDBL        XXDIFF
     C                   ADD       XXEABL        XTEABL
     C                   ADD       XXDDBL        XTDDBL
     C                   ADD       XXDIFF        XTDIFF
     C                   ADD       XXEABL        XXTOT1
     C                   ADD       XXDDBL        XXTOT2
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ENDIF
     C*
     C*
     C                   READ      PEM01                                  50
     C                   ENDDO
     C*
     C*  End of Pgm
     C*
     C                   EXCEPT    TOT
     C                   MOVE      '1'           *INLR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 02
     O                       DSTIME              08 '  :  :  '
     O                                           45 'THIS REPORT PRINTS DEDUC'
     O                                           69 'TIBLE BILLING DISCREPANC'
     O                                           93 'IES BETWEEN WMEAP AND WD'
     O                                          109 'DDP.            '
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                           09 'PRTDED3A '
     O                                          125 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           2
     O                                            5 'FUND'
     O                                           22 'EMPLOYER    DIV'
     O                                           40 'A/R AMT BILLED'
     O                                           61 'WDDDP AMT BILLED'
     O                                           77 'DIFFERENCE'
     O          E            DET            1
     O                       EMFND         4      4
     O                       EMEMP#        4     15
     O                       EMDIV         4     22
     O                       XXEABL        J     41
     O                       XXDDBL        J     62
     O                       XXDIFF        J     78
     O          E            TOT         1  2
     O                                           15 'TOTALS:'
     O                       XTEABL        J     41
     O                       XTDDBL        J     62
     O                       XTDIFF        J     78
     O          E            TOT         2  1
     O                                           21 'Total amount billed '
     O                                           42 'in the A/R file  . :'
     O                       XXTOT1        J     58
     O          E            TOT         1  1
     O                                           21 'Total deductibles bi'
     O                                           42 'lled in detail file:'
     O                       XXTOT2        J     58
