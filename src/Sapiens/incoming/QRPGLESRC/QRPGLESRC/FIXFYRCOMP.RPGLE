     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXFYRCOMP - DETERMINE IF FUND YEARS ARE ACCURATE
     H*****************************************************************
     H* Description:  This pgm prints a list of claims which have an  *
     H*   inaccurate FYR assigned, based on the Employer Detail file. *
     H*                                                               *
     H*****************************************************************
     FWMCML1    IF   E           K DISK
     FWDELL2    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     F*

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C**************************************************************************
     C*
     C*  Key list for the Employer Detail file
     C*
     C     ELKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMACDT
     C*
     C     ELKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C                   Z-ADD     0             XTCASE            5 0
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C**************************************************************************
     C*  Start of main loop.
     C*
     C                   READ      PCM01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C*  Calculate the FYR from the employer detail file.
     C*
     C                   Z-ADD     0             ELPFDT
     C                   Z-ADD     0             ELPTDT
     C     ELKY          SETGT     PEL01
     C     ELKY1         READPE    PEL01                                  60
     C  N60CMACDT        COMP      ELPTDT                             60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELFYR         XXFYR
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXFYR
     C                   END
     C*
     C*  If the calculated FYR does not match the claim FYR, print.
     C*
     C                   MOVE      '0'           *IN21
     C     XXFYR         IFNE      CMFYR
     C     XXFYR         COMP      0                                      21
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       1             XTCASE
     C                   END
     C*
     C                   READ      PCM01                                  60
     C                   END
     C*
     C                   EXCEPT    TOT
     C                   MOVE      '1'           *INLR
     C* ******************************************************
     OQSYSPRT   E            HEAD           1  3
     O                       DSTIME               8 '  :  :  '
     O                                          120 'RUN DATE:'
     O                       DSDATE        Y    132
     O          E            HEAD           1
     O                                           10 'FIXFYRCOMP'
     O                                           46 'THIS REPORT SHOWS ALL'
     O                                           68 'CLAIMS WITH INCORRECT'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                           57 'FUND YEARS ASSIGNED.'
     O          E            HEAD           2
     O                                            3 'FND'
     O                                            8 'FYR'
     O                                           17 'CASE'
     O                                           28 'EMPLOYER'
     O                                           34 'DIV'
     O                                           46 'ACC DATE'
     O                                           57 'FYR CALC'
     O                                           71 'POL FROM DT'
     O                                           82 'POL TO DT'
     O                                           94 'INIT DATE'
     O                                          106 'INIT USER'
     O          E            DET            2
     O                       CMFND         4      3
     O                       CMFYR         4      7
     O                       CMCASE        4     17
     O                       CMEMP#        4     28
     O                       CMDIV         4     34
     O                       CMACDT              46 '   /  /  '
     O                       XXFYR               54
     O                       ELPFDT              70 '   /  /  '
     O                       ELPTDT              82 '   /  /  '
     O                       CMIDT               94 '   /  /  '
     O                       CMIUSR             107
     O               21                         123 'Clm not within'
     O               21                         132 'coverage'
     O          E            TOT         2
     O                                            7 'TOTALS:'
     O                       XTCASE        4     17
