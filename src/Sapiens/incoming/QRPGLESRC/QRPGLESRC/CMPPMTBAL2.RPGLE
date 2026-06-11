     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE PROGRAM TO COMPARE PAID AMOUNTS IN CLAIM MASTER FILE
     H*****************************************************************
     H* Description:  This pgm prints a list of claims which have a   *
     H*   discrepancy between the total paid in the Claim file and    *
     H*   the total paid in the Pmt Detail file.                      *
     H*                                                               *
     H*****************************************************************
     H*   1/15/03   C:58  SUPP  WC094 #31404
     H*      1).  ADD TOTAL DIFFERENCE.
     H*      2).  ADD DIFFERENCES/TOTALS BY RESERVE TYPE
     H*      3).  ADD RESERVE TYPE DIFFERENCES EVEN IF CLAIM IS
     H*             BALANCED IN TOTAL (SWAPS) (BYPASS PRINTING OF
     H*             SWAP DETAIL WHICH IS DONE ONLY IN CMPPMTBAL3).
     H*
     H*   7/11/00   C:58  SUPP  WA063 #23309
     H*      Revised version CMPPMTBAL - with reworked report format.
     H*
     H*****************************************************************
     FWMCML1    IF   E           K DISK
     FWDPAL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*** BEG ADD *** 1/15/03 *************************
     D PR              S              9  2 DIM(10)                              PAYMENT ARRAY
     D DR              S              9  2 DIM(10)                              DIFF ARRAY
     D TD              S              9  2 DIM(10)                              TOT DIFF ARRAY
     D*** END ADD *** 1/15/03 *************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  CD                     1     20
     D                                     DIM(10)
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20

     C**************************************************************************
     C*
     C*  Key list for the Pmt Detail file
     C*
     C     PAKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*  Key list for the Company file
     C*
     C     COKY          KLIST
     C                   KFLD                    CMCO#
     C*
     C                   TIME                    DSDTTM
     C*
     C*  Start of main loop
     C*
     C                   READ      PCM01                                  60
     C*
     C  N60COKY          CHAIN     PCO01                              60
     C                   EXCEPT    HEAD
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     0             XXPAY            11 2
     C                   Z-ADD     0             XXNRP             7 0
     C*** BEG ADD *** 1/15/03 *************************
     C                   Z-ADD     0             PR
     C*** END ADD *** 1/15/03 *************************
     C*
     C*  GET ALL THE CLAIM DETAIL RECORDS
     C     PAKY          CHAIN     PPA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     PATYPE        IFNE      'P'
     C     PATYPE        ANDNE     'M'
     C     PATYPE        ANDNE     'Z'
     C     PATYPE        ANDNE     'A'
     C                   MULT      -1            PAAMT
     C                   END
     C*
     C                   ADD       PAAMT         XXPAY
     C                   ADD       1             XXNRP
     C*
     C*** BEG ADD *** 1/15/03 *************************
     C*
     C*      Accumulate payment by reserve code.
     C                   DO        10            P                 3 0
     C     PARECD        IFEQ      CD(P)
     C                   ADD       PAAMT         PR(P)
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*** END ADD *** 1/15/03 *************************
     C*
     C     PAKY          READE     PPA01                                  60
     C                   END
     C*
     C                   XFOOT     AR            XXAR             11 2
     C     XXPAY         COMP      XXAR                               6161
     C     XXAR          SUB       XXPAY         XXDIF            11 2
     C*
     C*** BEG ADD *** 1/15/03 *************************
     C*
     C*      Calculate difference by reserve type.
     C                   SUB       PR            AR
     C                   ADD       AR            TD
     C                   XFOOT     PR            PRT               9 2
     C*
     C* }}}} TEMP }}}}}}}}}}} PRINT SWAPPED $ }}}}}}}}}}
     C                   DO        10            P
     C     AR(P)         IFNE      *ZERO
     C**********           MOVE *ON       *IN61
     C*      PRINT DONE IN CMPPMTBAL3 ONLY
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C* }}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
     C*
     C*** END ADD *** 1/15/03 *************************
     C*
     C     *IN61         IFEQ      *ON
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C*** BEG ADD *** 1/15/03 *************************
     C*      Print line 2 if any data
     C*        found in reserves 5-10.
     C     AR(6)         IFNE      *ZERO
     C     AR(7)         ORNE      *ZERO
     C     AR(8)         ORNE      *ZERO
     C     AR(9)         ORNE      *ZERO
     C     AR(10)        ORNE      *ZERO
     C                   EXCEPT    DET2
     C                   ENDIF
     C*** END ADD *** 1/15/03 *************************
     C                   ADD       XXPAY         XTPAY            11 2
     C                   ADD       XXAR          XTAR             11 2
     C     XXAR          SUB       XXPAY         XTDIF            11 2
     C                   ADD       1             XTCASE            5 0
     C                   END
     C*
     C                   READ      PCM01                                  60
     C                   END
     C*** BEG ADD *** 1/15/03 *************************
     C     XTAR          SUB       XTPAY         XTDIF            11 2
     C*** END ADD *** 1/15/03 *************************
     C*
     C                   EXCEPT    TOT
     C                   SETON                                        LR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              96
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           76 'COMPARE WMCMP & WDPAP'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            3 'FND'
     O                                            7 'FYR'
     O                                           15 'CASE'
     O                                           30 'Reserves'
     O                                           44 'Payments'
     O                                           58 'Difference'
     O                                           68 'by Rsv:'
     O                       CD(1)               71
     O                       CD(2)               84
     O                       CD(3)               97
     O                       CD(4)              110
     O                       CD(5)              123
     O          E            DET
     O                       CMFND         4      3
     O                       CMFYR         4      7
     O                       CMCASE        4     15
     O                       XXAR          J     31
     O                       XXPAY         J     45
     O                       XXDIF         J     59
     O                       AR(1)         J     72
     O                       AR(2)         J     85
     O                       AR(3)         J     98
     O                       AR(4)         J    111
     O                       AR(5)         J    124
     O          E            DET2
     O                       CD(6)                2
     O                                            3 ':'
     O                       AR(6)         J     17
     O                       CD(7)               22
     O                                           23 ':'
     O                       AR(7)         J     37
     O                       CD(8)               42
     O                                           43 ':'
     O                       AR(8)         J     57
     O                       CD(9)               62
     O                                           63 ':'
     O                       AR(9)         J     77
     O                       CD(10)              82
     O                                           83 ':'
     O                       AR(10)        J     97
     O          E            TOT         2
     O                                            7 'TOTALS:'
     O                       XTAR          J     31
     O                       XTPAY         J     45
     O                       XTDIF         J     59
     O                       XTCASE        3     68
     O                                           74 'cases'
     O          E            TOT         2
     O                                           22 '                *** To'
     O                                           44 'tal Differences by Res'
     O                                           66 'erve ***              '
     O          E            TOT         2
     O                       CD(1)                2
     O                                            3 ':'
     O                       TD(1)         J     17
     O                       CD(2)               22
     O                                           23 ':'
     O                       TD(2)         J     37
     O                       CD(3)               42
     O                                           43 ':'
     O                       TD(3)         J     57
     O                       CD(4)               62
     O                                           63 ':'
     O                       TD(4)         J     77
     O                       CD(5)               82
     O                                           83 ':'
     O                       TD(5)         J     97
     O          E            TOT         2
     O                       CD(6)                2
     O                                            3 ':'
     O                       TD(6)         J     17
     O                       CD(7)               22
     O                                           23 ':'
     O                       TD(7)         J     37
     O                       CD(8)               42
     O                                           43 ':'
     O                       TD(8)         J     57
     O                       CD(9)               62
     O                                           63 ':'
     O                       TD(9)         J     77
     O                       CD(10)              82
     O                                           83 ':'
     O                       TD(10)        J     97
