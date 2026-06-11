     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE FIXWMEPL2 - CHECK FOR OUT OF SEQUENCE MAINTENACE INFO
     H*****************************************************************
     H* DESCRIPTION                                                   *
     H*  THIS REPORT READS OVER THE PHYSICAL FORMAT OF THE PAYROLL    *
     H*  FILE AND LISTS RECORDS THAT APPEAR OUT OF SEQUENCE DUE TO    *
     H*  INITIAL DATE FIELD.                                          *
     H*****************************************************************
     H* FILES:
     H*    WMEPP  (PEP01)
     H*
     H****************************************************************
     F*
     FWMEPP     IF   E           K DISK
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
     C     *LIKE         DEFINE    EPIDT         SVIDT
     C     *LIKE         DEFINE    EPCO#         XXCO#
     C     *LIKE         DEFINE    EPFND         XXFND
     C     *LIKE         DEFINE    EPEMP#        XXEMP#
     C     *LIKE         DEFINE    EPDIV         XXDIV
     C     *LIKE         DEFINE    EPFYR         XXFYR
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*
     C**************************************************************************
     C*  Start of main loop.
     C*
     C                   READ      PEP01                                  60
     C*
     C                   MOVE      *ON           *INOF
     C                   Z-ADD     EPIDT         SVIDT
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     EPIDT         IFLT      SVIDT
     C                   MOVE      EPCO#         XXCO#
     C                   MOVE      EPFND         XXFND
     C                   MOVE      EPEMP#        XXEMP#
     C                   MOVE      EPDIV         XXDIV
     C                   MOVE      EPFYR         XXFYR
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8 0
     C*
     C                   Z-ADD     SVIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XSIDT             8 0
     C*
     C                   Z-ADD     EPIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXIDT             8 0
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C                   EXCEPT    DET
     C*
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     C*
     C                   ELSE
     C*
     C                   Z-ADD     EPIDT         SVIDT
     C*
     C                   ENDIF                                                  EPIDT IFLT SVIDT
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
     O                                           65 'CHECK FOR PAYROLL OUT O'
     O                                           87 'F SEQ BY IDT IN WMEPP.'
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
     O                                           80 'SAVED DATE'
     O                                           93 'INITIAL DATE'
     O                                          105 'INITIAL USER'
     O                                          117 'INITIAL PGM'
     O          E            DET            1
     O                       EPCO#         4      5
     O                       EPFND         4     10
     O                       EPEMP#        4     22
     O                       EPDIV         4     30
     O                       EPFYR         4     37
     O                       XXEFDT              48 '  /  /    '
     O                       EPCLAS              55
     O                       EPSPEC              60
     O                       EPSTAT              66
     O                       XSIDT               80 '  /  /    '
     O                       XXIDT               93 '  /  /    '
     O                       EPIUSR             105
     O                       EPIPGM             117
