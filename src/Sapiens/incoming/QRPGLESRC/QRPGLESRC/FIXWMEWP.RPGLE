     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE FIXWMEWP - CHECK FOR DUPLICATE EXCLUDED WORKPLACES
     H*****************************************************************
     H* DESCRIPTION                                                   *
     H*  THIS REPORT CHECKS FOR DUPLICATE PAYROLL CLASS CODES.        *
     H*****************************************************************
     H* FILES:
     H*    WMEWP  (PEW01)
     H*    WMEWL  (PEW02)
     H*
     H****************************************************************
     F*
     FWMEWP     IF   E             DISK
     FWMEWL     UF   E           K DISK
     F                                     RENAME(PEW01:PEW02)
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     C*
     C     EWKY          KLIST
     C                   KFLD                    SVCO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVFYR
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    SVWPL#
     C*
     C*  INITIALIZE FIELDS
     C*
     C     *LIKE         DEFINE    EWCO#         SVCO#
     C     *LIKE         DEFINE    EWFND         SVFND
     C     *LIKE         DEFINE    EWEMP#        SVEMP#
     C     *LIKE         DEFINE    EWDIV         SVDIV
     C     *LIKE         DEFINE    EWFYR         SVFYR
     C     *LIKE         DEFINE    EWWPL#        SVWPL#
     C*
     C**************************************************************************
     C*  Start of main loop.
     C*
     C                   READ      WMEWP                                  60
     C*
     C                   MOVE      *ON           *INOF
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      EWCO#         SVCO#
     C                   MOVE      EWFND         SVFND
     C                   MOVE      EWFYR         SVFYR
     C                   MOVE      EWEMP#        SVEMP#
     C                   MOVE      EWDIV         SVDIV
     C                   MOVE      EWFYR         SVFYR
     C                   MOVE      EWWPL#        SVWPL#
     C     EWKY          CHAIN     PEW02                              61
     C     EWKY          READE     PEW02                                  61
     C     *IN61         DOWEQ     *OFF
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C                   EXCEPT    DET
     C*
     C                   DELETE    PEW02
     C*
     C     EWKY          READE     PEW02                                  61
     C                   ENDDO                                                  *IN61 EQ *OFF
     C*
     C                   READ      WMEWP                                  60
     C                   ENDDO                                                  *IN60 EQ *OFF
     C*
     C*  End of program
     C*
     C                   MOVE      *ON           *INLR
     C*
     C********************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                                           65 'CHECK FOR DUPLICATE EXC'
     O                                           87 'LUDE RECORDS IN WMEWP.'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                            5 'CO#'
     O                                           10 'FND'
     O                                           22 'EMPLOYER #'
     O                                           32 'DIVISION'
     O                                           37 'FYR'
     O                                           48 'WORKPL #'
     O          E            DET            1
     O                       EWCO#         4      5
     O                       EWFND         4     10
     O                       EWEMP#        4     22
     O                       EWDIV         4     30
     O                       EWFYR         4     37
     O                       EWWPL#              48
