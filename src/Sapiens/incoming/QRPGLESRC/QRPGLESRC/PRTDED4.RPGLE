     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE PRTDED4 - PRINT DEDUCTIBLE RECOVERY DISCREPANCIES
     H*****************************************************************
     H* PROGRAM DESCRIPTION:  This pgm calculates the deduct recovery *
     H*   amt (using the payment detail file) for each employer on a  *
     H*   deduct plan (based on an entered date) and compares it to   *
     H*   the amt recovered in the Deduct Detail file.                *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   NONE                                                        *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H* PROGRAMMER: C:15                                              *
     H* CREATED: 09/25/97                                             *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*                                                               *
     H*  FILES:                                                       *
     H*     WDELL  (PEL01): CO#/FND/EMP#/DIV/FYR                      *
     H*     WMEML (PEM01): CO#/FND/EMP#/DIV                           *
     H*     WMFNL  (PFN01): CO#/FND                                   *
     H*     WTTTL  (PTT01): CO#/KEY                                   *
     H*     WMCML  (PCM01): CO#/FND/EMP#/DIV/FYR/CASE                 *
     H*     WDPAL5 (PCM01): CO#/FND/FYR/CASE/TRDT                     *
     H*     WDDDL1 (PDD01): CO#/FND/EMP#/DIV/FYR/CASE                 *
     H*     WMEAL1 (PDD01): CO#/FND/EMP#/DIV/FYR/CASE                 *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWDELL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)
     FWMCML     IF   E           K DISK
     FWDPAL5    IF   E           K DISK
     FWDDDL1    IF   E           K DISK
     FWMEAL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D YR              S              3  0 DIM(10)
     D PD              S             11  2 DIM(10)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  CD                     1     30  0
     D                                     DIM(5) ASCEND
     D  EMCAD1                 1      6  0
     D  EMCAD2                 7     12  0
     D  EMCAD3                13     18  0
     D  EMCAD4                19     24  0
     D  EMCAD5                25     30  0
     D                 DS
     D  DC                     1     20
     D                                     DIM(10)
     D  FNDC01                 1      2
     D  FNDC02                 3      4
     D  FNDC03                 5      6
     D  FNDC04                 7      8
     D  FNDC05                 9     10
     D  FNDC06                11     12
     D  FNDC07                13     14
     D  FNDC08                15     16
     D  FNDC09                17     18
     D  FNDC10                19     20
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELFND
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELFND
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C*  KEY LIST FOR THE PAYMENT DETAIL FILE
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*  KEY LIST FOR THE DEDUCTIBLE DETAIL FILE
     C*
     C     DDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C*
     C*  ENTRY PARM LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C*
     C     *LIKE         DEFINE    DDCO#         XXCO#
     C     *LIKE         DEFINE    DDFND         SVFND
     C     *LIKE         DEFINE    DDFND         SVFYR
     C     *LIKE         DEFINE    DDEMP#        SVEMP#
     C     *LIKE         DEFINE    DDDIV         SVDIV
     C     *LIKE         DEFINE    DDAMTB        XXRECV
     C     *LIKE         DEFINE    DDAMTB        XXAMTR
     C     *LIKE         DEFINE    DDAMTB        XXAMTP
     C     *LIKE         DEFINE    DDAMTB        XXDIFF
     C     *LIKE         DEFINE    EAARCD        XXRDPY
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   Z-ADD     0             XTRECV           11 2
     C                   Z-ADD     0             XTAMTR           11 2
     C                   Z-ADD     0             XTAMTP           11 2
     C                   Z-ADD     0             XTDIFF           11 2
     C                   Z-ADD     0             XXTOT1           11 2
     C                   Z-ADD     0             XXTOT2           11 2
     C                   Z-ADD     0             XXTOT3           11 2
     C                   CLEAR                   PD
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
     C*
     C*  READ CLAIM FILE FOR ALL EMPLOYERS
     C*
     C     ELKY          CHAIN     PEL01                              60
     C                   Z-ADD     ELEMP#        SVEMP#
     C                   Z-ADD     ELDIV         SVDIV
     C                   EXSR      FNDBRK
     C     *IN60         DOWEQ     '0'
     C     ELDCDE        IFNE      *BLANKS
     C*
     C*  Process Fund or Employer break.
     C*
     C     ELFND         IFNE      SVFND
     C                   EXSR      EMPBRK
     C                   EXSR      FNDBRK
     C                   END
     C*
     C     ELEMP#        IFNE      SVEMP#
     C     ELDIV         ORNE      SVDIV
     C                   EXSR      EMPBRK
     C                   END
     C*
     C*  Read all claims for this employer
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  Accumulate the amt of deduct recoveries using WDPAP.
     C*
     C                   EXSR      CLMPMT
     C*
     C     CMKY          READE     PCM01                                  60
     C                   ENDDO
     C*
     C                   END
     C     NEXT          TAG
     C     ELKY          READE     PEL01                                  60
     C                   ENDDO
     C*
     C*  End of Pgm
     C*
     C                   EXCEPT    TOT
     C                   MOVE      '1'           *INLR
     C**************************************************************************
     C*  CLMPMT - THIS SUBROUTINE CALCULATES THE TOTAL AMOUNT OF DEDUCT
     C*           RECOVERIES MADE ON A CLAIM.
     C**************************************************************************
     C     CLMPMT        BEGSR
     C*
     C     PAKY          CHAIN     PPA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     PABR2         IFEQ      FNMEP1
     C     PABR2         OREQ      FNCOP1
     C     PABR2         OREQ      FNEXP1
     C     PABR2         OREQ      FNLEP1
     C                   ADD       PAAMT         XXRECV
     C                   ENDIF
     C*
     C     PAKY          READE     PPA01                                  60
     C                   ENDDO
     C*
     C                   ENDSR
     C**************************************************************************
     C*  DEDREC - THIS SUBROUTINE CALCULATES THE TOTAL AMOUNT OF DEDUCT
     C*             RECOVERIES FOR AN EMPLOYER.
     C**************************************************************************
     C     DEDREC        BEGSR
     C*
     C                   Z-ADD     0             XXAMTR
     C     DDKY          CHAIN     PDD01                              60
     C     *IN60         DOWEQ     '0'
     C                   ADD       DDAMTR        XXAMTR
     C     DDKY          READE     PDD01                                  60
     C                   ENDDO
     C*
     C                   Z-ADD     0             XXAMTP
     C     DDKY          CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     EAARCD        IFEQ      XXRDPY
     C                   ADD       EAAMT         XXAMTP
     C                   ENDIF
     C*
     C     DDKY          READE     PEA01                                  60
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
     C     ELKY1         SETGT     PEL01
     C                   GOTO      NEXT
     C                   ENDIF
     C*
     C                   Z-ADD     ELFND         SVFND
     C*
     C                   ENDSR
     C*****************************************************************
     C* EMPBRK SUBROUTINE              ** SUBROUTINE **   E M P B R K *
     C*****************************************************************
     C     EMPBRK        BEGSR
     C*
     C*  Calculate the deduct recovered and compare to the amount
     C*    recovered on the claims.  If different, print on report.
     C*
     C                   EXSR      DEDREC
     C     XXRECV        IFNE      XXAMTR
     C     XXRECV        SUB       XXAMTR        XXDIFF
     C                   ADD       XXRECV        XTRECV
     C                   ADD       XXAMTR        XTAMTR
     C                   ADD       XXAMTP        XTAMTP
     C                   ADD       XXDIFF        XTDIFF
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ENDIF
     C*
     C                   ADD       XXRECV        XXTOT1
     C                   ADD       XXAMTR        XXTOT2
     C                   ADD       XXAMTP        XXTOT3
     C                   Z-ADD     0             XXRECV
     C                   Z-ADD     ELEMP#        SVEMP#
     C                   Z-ADD     ELDIV         SVDIV
     C*
     C                   ENDSR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 02
     O                       DSTIME              08 '  :  :  '
     O                                           45 'THIS REPORT LISTS THE DE'
     O                                           69 'DUCTIBLE RECOVERED AMOUN'
     O                                           93 'TS IN WDPAP, WDDDP AND W'
     O                                          109 'MEAP.           '
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                           09 'PRTDED4  '
     O                                          125 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           1
     O                                           35 'CLAIM'
     O                                           56 'DEDUCT'
     O                                          102 'A/R'
     O          E            HEAD           2
     O                                            5 'FUND'
     O                                           22 'EMPLOYER    DIV'
     O                                           38 'RECOVERED AMT'
     O                                           59 'RECOVERED AMT'
     O                                           78 'DIFFERENCE'
     O                                          105 'DEDUCT PAID'
     O          E            DET            1
     O                       SVFND         4      4
     O                       SVEMP#        4     15
     O                       SVDIV         4     22
     O                       XXRECV        J     39
     O                       XXAMTR        J     60
     O                       XXDIFF        J     79
     O                       XXAMTP        J    106
     O          E            TOT         1  2
     O                                           15 'TOTALS:'
     O                       XTRECV        J     39
     O                       XTAMTR        J     60
     O                       XTDIFF        J     79
     O                       XTAMTP        J    106
     O          E            TOT         2  1
     O                                           21 'Total amount of clai'
     O                                           38 'm recoveries  . :'
     O                       XXTOT1        J     54
     O          E            TOT         1  1
     O                                           21 'Total deductible rec'
     O                                           38 'overies . . . . :'
     O                       XXTOT2        J     54
     O          E            TOT         1  1
     O                                           21 'Total A/R deductible'
     O                                           38 's paid  . . . . :'
     O                       XXTOT3        J     54
