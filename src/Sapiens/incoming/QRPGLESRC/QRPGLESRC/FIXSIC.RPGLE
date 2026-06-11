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
     FWTGTL     UF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  XXSIC                  1      4
     D  X1SIC                  1      1
     D  X2SIC                  2      2
     D  X3SIC                  3      3
     D  X4SIC                  4      4

     C**************************************************************************
     C*
     C*  Key list for the GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C*
     C                   Z-ADD     777           XXCO#             3 0
     C                   MOVEL     'SIC '        XXTBID            4
     C*
     C*  Start of main loop
     C*
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVEL     GTKEY         XXSIC
     C     X1SIC         IFNE      'X'
     C     X4SIC         ANDEQ     *BLANK
     C*
     C                   MOVE      X3SIC         X4SIC
     C                   MOVE      X2SIC         X3SIC
     C                   MOVE      X1SIC         X2SIC
     C                   MOVE      '0'           X1SIC
     C                   MOVEL     XXSIC         GTKEY
     C                   UPDATE    PGT01
     C*
     C                   ENDIF
     C*
     C     GTKY          READE     PGT01                                  60
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C**************************************************************************
