     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE PRTCHKPROV - PRINT CHECKS WITH INCORRECT PAYEE INFO
     H*****************************************************************
     H* Description:  This program prints WARPP records with incorrect*
     H*   payee name or address.                                      *
     H*                                                               *
     H* Created by:  C:15       04/27/99                              *
     H*****************************************************************
     FWARPL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C**************************************************************************
     C*  Key list for the Claim Master file
     C*
     C     CMKY          KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C*
     C*  Key list for the Claim Demographics file
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  Key list for the Provider file
     C*
     C     PDKY          KLIST
     C                   KFLD                    RPFEIN
     C                   KFLD                    RPMOD
     C*
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C                   MOVE      *BLANKS       XXRNAM           15
     C                   MOVE      *BLANKS       XXRADR           20
     C                   MOVE      *BLANKS       XXPNAM           15
     C                   MOVE      *BLANKS       XXPADR           20
     C                   Z-ADD     0             XXCNT             5 0
     C*
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C**************************************************************************
     C*  Process all request payment records
     C*
     C                   READ      PRP01                                  99
     C     *IN99         DOWEQ     '0'
     C*
     C     RPFEIN        IFNE      0
     C*
     C*  If the payment FEIN matches the claimant's SS#, skip this pmt.
     C*
     C                   MOVE      RPFEIN        XXSS#
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '1'
     C     XXSS#         OREQ      CMSS#
     C                   GOTO      NEXT
     C                   ENDIF
     C*
     C     CDKY          CHAIN     PCD01                              60
     C   60              GOTO      NEXT
     C*
     C     RPADR1        IFEQ      CDADR1
     C*
     C*  If the payee name or address does not match the provider file
     C*    name and address, print the payment on the report.
     C*
     C                   MOVE      *BLANKS       PDNAME
     C                   MOVE      *BLANKS       PDADR1
     C     PDKY          CHAIN     PPD01                              60
     C*
     C                   MOVEL     RPPNAM        XXRNAM
     C                   MOVEL     RPADR1        XXRADR
     C                   MOVEL     PDNAME        XXPNAM
     C                   MOVEL     PDADR1        XXPADR
     C                   ADD       1             XXCNT
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     NEXT          TAG
     C                   READ      PRP01                                  99
     C                   ENDDO                                                  *IN99 DOWEQ
     C*
     C*  End of Pgm
     C*
     C                   EXCEPT    TOT
     C                   MOVE      '1'           *INLR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 02
     O                       DSTIME              08 '  :  :  '
     O                                           55 'THIS REPORT PRINTS PAYME'
     O                                           79 'NTS WHICH HAVE INVALID P'
     O                                           95 'AYEE INFORMATION'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                           10 'PRTPROVCHK'
     O                                          125 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           2
     O                                            3 'GRP'
     O                                            8 'FYR'
     O                                           16 'CASE'
     O                                           25 'CHECK#'
     O                                           35 'TRANS DT'
     O                                           42 'PAYEE'
     O                                           50 'MOD'
     O                                           58 'RPNAME'
     O                                           75 'RPADR1'
     O                                           97 'PDNAME'
     O                                          114 'PDADR1'
     O          E            DET            1
     O                       RPFND         4      3
     O                       RPFYR         4      8
     O                       RPCASE        4     16
     O                       RPCHK#        4     25
     O                       RPTRDT              35 '   /  /  '
     O                       RPFEIN        4     46
     O                       RPMOD         4     50
     O                       XXRNAM              67
     O                       XXRADR              89
     O                       XXPNAM             106
     O                       XXPADR             128
     O          E            TOT         1  2
     O                                           17 'TOTAL PAYMENTS:'
     O                       XXCNT         4     24
