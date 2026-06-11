     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE FIXWMEPL - CHECK FOR DUPLICATE PAYROLL CLASS CODES
     H*****************************************************************
     H* DESCRIPTION                                                   *
     H*  THIS REPORT CHECKS FOR DUPLICATE PAYROLL CLASS CODES.        *
     H*****************************************************************
     H* FILES:
     H*    WMEPL  (PEP01)
     H*
     H****************************************************************
     F*
     FWMEPL     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  INITIALIZE FIELDS
     C*
     C     *LIKE         DEFINE    EPCO#         SVCO#
     C     *LIKE         DEFINE    EPFND         SVFND
     C     *LIKE         DEFINE    EPEMP#        SVEMP#
     C     *LIKE         DEFINE    EPDIV         SVDIV
     C     *LIKE         DEFINE    EPFYR         SVFYR
     C     *LIKE         DEFINE    EPEFDT        SVEFDT
     C     *LIKE         DEFINE    EPCLAS        SVCLAS
     C     *LIKE         DEFINE    EPSPEC        SVSPEC
     C     *LIKE         DEFINE    EPCO#         XXCO#
     C     *LIKE         DEFINE    EPFND         XXFND
     C     *LIKE         DEFINE    EPEMP#        XXEMP#
     C     *LIKE         DEFINE    EPDIV         XXDIV
     C     *LIKE         DEFINE    EPFYR         XXFYR
     C*
     C**************************************************************************
     C*  Start of main loop.
     C*
     C                   READ      PEP01                                  60
     C*
     C                   MOVE      *ON           *INOF
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     EPCO#         IFEQ      SVCO#
     C     EPFND         ANDEQ     SVFND
     C     EPEMP#        ANDEQ     SVEMP#
     C     EPDIV         ANDEQ     SVDIV
     C     EPFYR         ANDEQ     SVFYR
     C     EPEFDT        ANDEQ     SVEFDT
     C     EPCLAS        ANDEQ     SVCLAS
     C     EPSPEC        ANDEQ     SVSPEC
     C                   MOVE      EPCO#         XXCO#
     C                   MOVE      EPFND         XXFND
     C                   MOVE      EPEMP#        XXEMP#
     C                   MOVE      EPDIV         XXDIV
     C                   MOVE      EPFYR         XXFYR
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C                   EXCEPT    DET
     C*
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     C*
     C                   ENDIF                                                  EPCO# IFEQ
     C*
     C                   Z-ADD     EPCO#         SVCO#
     C                   Z-ADD     EPFND         SVFND
     C                   Z-ADD     EPEMP#        SVEMP#
     C                   Z-ADD     EPDIV         SVDIV
     C                   Z-ADD     EPFYR         SVFYR
     C                   Z-ADD     EPEFDT        SVEFDT
     C                   Z-ADD     EPCLAS        SVCLAS
     C                   MOVE      EPSPEC        SVSPEC
     C*
     C                   READ      PEP01                                  60
     C                   ENDDO
     C*
     C*  End of program
     C*
     C                   MOVE      *ON           *INLR
     C*
     C********************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                                           65 'CHECK FOR DUPLICATE PAY'
     O                                           87 'ROLL RECORDS IN WMEPP.'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                            5 'CO#'
     O                                           10 'FND'
     O                                           22 'EMPLOYER #'
     O                                           32 'DIVISION'
     O                                           37 'FYR'
     O                                           48 'EFF DATE'
     O                                           55 'CLASS'
     O                                           61 'SPEC'
     O                                           67 'STAT'
     O          E            DET            1
     O                       EPCO#         4      5
     O                       EPFND         4     10
     O                       EPEMP#        4     22
     O                       EPDIV         4     30
     O                       EPFYR         4     37
     O                       EPEFDT              48 '   /  /  '
     O                       EPCLAS              55
     O                       EPSPEC              60
     O                       EPSTAT              66
