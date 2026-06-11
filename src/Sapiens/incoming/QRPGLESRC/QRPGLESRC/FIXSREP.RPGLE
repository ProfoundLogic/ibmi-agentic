     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXSREP - Correct WMEPP/EPSAPR using values from WASRP/SRAMTP
     H*****************************************************************
      *
      * Employer Payroll
     FWMEPL1    UF   E           K DISK
      *
      * Self Reporting Payroll Transaction
     FWASRL5    IF   E           K DISK
      *
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
      *
      *
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *

     C     SRKY          KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPFND
     C                   KFLD                    EPEMP#
     C                   KFLD                    EPDIV
     C                   KFLD                    EPFYR
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEFDT
     C                   KFLD                    EPSPEC
      *
      *=============================================================
      *
     C     *LIKE         DEFINE    EPSAPR        SVSAPR
     C     *LIKE         DEFINE    EPSAPR        XXSAPR
      *
      *=============================================================
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *=============================================================
      *
     C                   READ      PEP01                                  60
     C                   MOVE      *ON           *INOF
      *
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     EPSAPR        SVSAPR
     C                   Z-ADD     *ZERO         XXSAPR
      *
      * Accumulate S/R Payroll
      *
     C     SRKY          CHAIN     PSR01                              60
      *
     C     *IN60         DOWEQ     *OFF
     C                   ADD       SRAMTP        XXSAPR
     C     SRKY          READE     PSR01                                  60
     C                   ENDDO                                                  WASRP
      *
      * Update WMEPP if indicated
      *
     C     EPSAPR        IFNE      XXSAPR
     C                   Z-ADD     XXSAPR        EPSAPR
      *
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C                   MOVEL     DSTIME        EPTIME
      *
     C                   UPDATE    PEP01
      *
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
      *
     C                   EXCEPT    DET
     C                   ENDIF                                                  EPSAPR <> XXSAPR
      *
     C                   READ      PEP01                                  60
     C                   ENDDO                                                  WMEPP
      *
      *  End of program
      *
     C                   MOVE      *ON           *INLR
      *
      *
      *
      *===================================================================
      *
     OQSYSPRT   E            HEAD           1 02
     O                       DSTIME               8 '  :  :  '
     O                       DSDATE        Y    132
      *
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           60 'WMEPP/EPSAPR RECORDS'
     O                                         +  1 'UPDATED USING'
     O                                         +  1 'WASRP VALUES.'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
      *
     O          E            HEAD           2
     O                                            5 'CO#'
     O                                           10 'FND'
     O                                           22 'EMPLOYER #'
     O                                           32 'DIVISION'
     O                                           37 'FYR'
     O                                           48 'EFF DATE'
     O                                           55 'CLASS'
     O                                           61 'SPEC'
     O                                           67 'STAT'
     O                                         +009 'WAS'
     O                                         +014 'NOW'
      *
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
     O                       SVSAPR        J   +002
     O                       EPSAPR        J   +002
      *
