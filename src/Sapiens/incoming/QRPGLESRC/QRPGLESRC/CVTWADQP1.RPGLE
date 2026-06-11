     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTWADQP1 - LOAD DEDUCTIBLE PAYMENT SUMMARY RECORDS
     H*****************************************************************
     H*   PROGRAM DESCRIPTION:                                        *
     H*     This program reads all deductible payments and writes a   *
     H*     WADQP record.                                             *
     H*                                                               *
     H*   PROGRAMMER: C:15                                            *
     H*   CREATED:    07/05/05                                        *
     H*                                                               *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*   60  -  GENERAL WORK INDICATOR                               *
     H*   L1  -  LEVEL BREAK ON EMPLOYER                              *
     H*   L2  -  LEVEL BREAK ON FUND DETAIL                           *
     H*****************************************************************
     FWMEALT    IF   E           K DISK
     FWADQL     UF A E           K DISK
     FWDELL     IF   E           K DISK
     FWTDDL2    IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*-----------------------------------------------------------------
     C*   Entry Parm List
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQTRDT            7
     C*
     C*   Key list for the A/R Detail File
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXARTY
     C                   KFLD                    XXTRDT
     C*
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXARTY
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
     C                   KFLD                    EAFYR
     C*
     C*   KEY LIST FOR THE RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    XXCD              3
     C*
     C*   KEY LIST FOR THE DEDUCTIBLE DISCOUNT CODE FILE
     C*
     C     DDKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    XXST              2
     C                   KFLD                    XXDDCD
     C                   KFLD                    XXDDEF
     C*
     C     DDKY1         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    XXST
     C                   KFLD                    XXDDCD
     C*
     C*  KEY LIST FOR THE DEDUCTIBLE PAYMENT RECOVERIES PENDING FILE
     C*
     C     DQKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
     C                   KFLD                    EAFYR
     C                   KFLD                    EACHK#
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*  DEFINE LEVEL BREAK FIELDS
     C*
     C     *LIKE         DEFINE    EACO#         XXCO#
     C     *LIKE         DEFINE    EAARTY        XXARTY
     C     *LIKE         DEFINE    EATRDT        XXTRDT
     C     *LIKE         DEFINE    ELDCDE        XXDDCD
     C     *LIKE         DEFINE    ELDCEF        XXDDEF
     C                   Z-ADD     0             XTAMT            11 2
     C                   Z-ADD     0             X8TRDT            8 0
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      'Q'           XXARTY
     C                   MOVE      QQTRDT        XXTRDT
     C*-----------------------------------------------------------------
     C*  Start reading A/R records.
     C*
     C     EAKY          SETGT     PEA01
     C     EAKY1         READE     PEA01                                  LR
     C     *INLR         DOWEQ     *OFF
     C*
     C*  If the A/R code is a deductible payment type, continue.
     C*
     C     EAARCD        IFEQ      'DQ'
     C*
     C*  Determine if the employer is on a "Net" deductible plan.
     C*
     C     ELKY          CHAIN     PEL01                              60
     C  N60              MOVE      ELRTCD        XXCD
     C  N60RSKY          CHAIN     PRS01                              60
     C  N60              MOVE      RSJRST        XXST
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      ELDCDE        XXDDCD
     C                   MOVE      ELDCEF        XXDDEF
     C*
     C  N60DDKY          SETGT     PDD01
     C  N60DDKY1         READPE    PDD01                                  60
     C   60              MOVE      *BLANKS       DDTYPE
     C*
     C                   ENDIF
     C*
     C     DDTYPE        IFEQ      'N'
     C*
     C*  Only write a WADQP record if one does not exist, and the
     C*  amount is greater than 0.
     C*
     C     DQKY          CHAIN     PDQ01                              60
     C     *IN60         IFEQ      *ON
     C     EAAMT         ANDGT     0
     C*
     C                   CLEAR                   PDQ01
     C*
     C                   MOVE      EACO#         DQCO#
     C                   MOVE      EAFND         DQFND
     C                   MOVE      EAFYR         DQFYR
     C                   MOVE      EAEMP#        DQEMP#
     C                   MOVE      EADIV         DQDIV
     C                   MOVE      EACHK#        DQCHK#
     C                   MOVE      EAAMT         DQAMT
     C                   MOVE      EAAMT         DQAMTR
     C                   MOVE      Q1USER        DQUSER
     C                   MOVE      Q1PGM         DQPGM
     C                   MOVE      DSTIME        DQTIME
     C                   MOVE      EATRDT        DQDT
     C                   MOVE      *BLANKS       DQSTAT
     C                   WRITE     PDQ01
     C*
     C*  Print record on report.
     C*
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL(P)  SRDTO         X8TRDT
     C                   ADD       EAAMT         XTAMT
     C*
     C                   EXCEPT    DET
     C*
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDIF                                                  DDTYPE = 'N'
     C*
     C                   ENDIF                                                  EAARCD = 'DQ'
     C*
     C     EAKY1         READE     PEA01                                  LR
     C                   ENDDO
     C*
     C                   EXCEPT    TOT
     C*
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 02
     O                       DSTIME              08 '  :  :  '
     O                                           45 'DEDUCTIBLE RECOVERY CONV'
     O                                           45 'ERSION REPORT'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                           09 'CVTWADQP '
     O                                          125 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           2
     O                                            5 'GROUP'
     O                                           22 'EMPLOYER    DIV'
     O                                           36 'TRANS DATE'
     O                                           47 'CHECK #'
     O                                           62 'AMOUNT PAID'
     O          E            DET            1
     O                       EAFND         4      4
     O                       EAEMP#        4     15
     O                       EADIV         4     22
     O                       X8TRDT              36 '  /  /    '
     O                       EACHK#        4     47
     O                       EAAMT         J     63
     O          E            TOT         2  1
     O                                           23 'Total amount added:'
     O                       XTAMT         J     63
