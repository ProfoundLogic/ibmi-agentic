     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXHDNPCT - Fix hidden percentages
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION                                          *
     H*                                                               *
     H*    Employer detail records that have entered percentages with *
     H*    no corresponding WDRRP header are written to report output.*
     H*                                                               *
     H*  CREATED:                                                     *
     H*       05/02/00 C:61 PROJECT WC988                             *
     H*****************************************************************
      *
      * System Company
     FSMCOL     IF   E           K DISK
      *
      *  Employer Detail
     FWDELL     IF   E           K DISK
      *
      *  Rate Set Code
     FWMRSL     IF   E           K DISK
      *
      *  Statistical Reporting Rating
     FWDRRL     IF   E           K DISK
      *
      *  Output File
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
      *
      *
      * SYSTEM DATE AND TIME

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *
      *****************************************************************
      *  KEY LISTS
      *****************************************************************
      *
      *
      *    RATE SET CODE FILE
      *

     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      *
      *  STATISTICAL REPORTING/RATING FILE
      *
     C     RRKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
      *
     C     RRKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
     C                   KFLD                    ELPFDT
      *
      *   Company
      *
     C     COKY          KLIST
     C                   KFLD                    ELCO#
      *
      *
      *
      *****************************************************************
      *  *LIKE DEFN
      *****************************************************************
      *
     C     *LIKE         DEFINE    RSJRST        XXST
      *
      *
      *
      ****************************************************************
      *  MAIN LOOP
      ****************************************************************
      *
      *
     C                   TIME                    DSDTTM
     C                   MOVE      *ALL'_'       LINE            132
     C                   Z-ADD     *ZERO         COUNT            11 0
     C                   SETON                                        OA
      *
     C                   READ      PEL01                                  60
      *
     C     *IN60         DOWEQ     '0'
      *
      *   Check for missing headings
      *
     C                   EXSR      CKHEAD
      *
     C     *IN70         IFEQ      *ON
     C                   SETOFF                                       70
      *
      *   WRITE DETAIL TO SPOOL FILE
      *
      *
     C     CONAME        IFEQ      *BLANKS
     C     COKY          CHAIN     PCO01                              71
      *
      * Center Company name in display field
      *
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        CONAME
      *
     C                   ENDIF                                                  CONAME
      *
     C   OA              EXCEPT    HEAD
     C   OA              SETOFF                                       OA
     C                   EXCEPT    DET
     C                   ADD       1             COUNT
     C                   ENDIF                                                  *IN70 = *ON
      *
     C                   READ      PEL01                                  60
     C                   ENDDO                                                  *IN60
      *
     C                   EXCEPT    TOT
      *
     C                   SETON                                        LR
      *
      *
      *
      *******************************************************************
      * CHHEAD -  Error check for missing headings
      *******************************************************************
     C     CKHEAD        BEGSR
      *
      *  Get statistical data elements associated with
      *  rate code and effective date
      *
     C     RSKY          CHAIN     PRS01                              60
     C  N60              MOVEL     RSJRST        XXST
     C   60              MOVEL     *BLANKS       XXST
      *
     C  N60RRKY2         SETGT     PRR01
     C  N60RRKY1         READPE    PRR01                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
      *    Deviation #1
      *
     C     ELASP2        IFNE      *ZERO
     C     RRDV1H        ANDEQ     *BLANKS
     C                   MOVE(P)   'Y'           XXDV1             1
     C                   SETON                                            70
     C                   ENDIF                                                  DV1H
      *
      *    Deviation #2
      *
     C     ELDEV2        IFNE      *ZERO
     C     RRDV2H        ANDEQ     *BLANKS
     C                   MOVE(P)   'Y'           XXDV2             1
     C                   SETON                                            70
     C                   ENDIF                                                  DV2H
      *
      *    Subject Premium Credit 1A
      *
     C     ELASPM        IFNE      *ZERO
     C     RR1AHD        ANDEQ     *BLANKS
     C                   MOVE(P)   'Y'           XXPC1A            1
     C                   SETON                                            70
     C                   ENDIF                                                  1AHD
      *
      *    Subject Premium Credit 1B
      *
     C     ELASPS        IFNE      *ZERO
     C     RR1BHD        ANDEQ     *BLANKS
     C                   MOVE(P)   'Y'           XXPC1B            1
     C                   SETON                                            70
     C                   ENDIF                                                  1BHD
      *
      *    Scheduled Debit Stat
      *
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        ANDGT     1
     C     RRDBHD        ANDEQ     *BLANKS
     C                   MOVE(P)   'Y'           XXDB              1
     C                   SETON                                            70
     C                   ENDIF                                                  DBHD
      *
      *    Scheduled Credit Stat
      *
     C     ELDBCR        IFNE      *ZERO
     C     ELDBCR        ANDLT     1
     C     RRCRHD        ANDEQ     *BLANKS
     C                   MOVE(P)   'Y'           XXCR              1
     C                   SETON                                            70
     C                   ENDIF                                                  CRHD
      *
      *    Waiver Surcharge
      *
     C     ELWVPC        IFNE      *ZERO
     C     RRWVHD        ANDEQ     *BLANKS
     C                   MOVE(P)   'Y'           XXWVPC            1
     C                   SETON                                            70
     C                   ENDIF                                                  WVHD
      *
     C                   ENDIF                                                  *IN60
      *
     C                   ENDSR                                                  CKHEAD
      *
      *
      *******************************************************************
      *
      *
      *
      *
      *
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME               8 '  :  :  '
     O                       DSDATE        Y    132
     O                       CONAME              82
     O                       COCO#             +005
      *
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                         +030 'WDELP records with'
     O                                         +001 'UNmatching WDRRP records'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
      *
     O          E            HEAD        0  0
     O                       LINE              +000
      *
     O          E            HEAD           2
     O                                         +000 'FND'
     O                                         +002 'EMP#     '
     O                                         +002 'DIV  '
     O                                         +002 'PYR '
     O                                         +003 'DV1 '
     O                                         +002 'DV2 '
     O                                         +002 '1A  '
     O                                         +002 '1B  '
     O                                         +002 'DBT '
     O                                         +002 'CR  '
     O                                         +002 'WAIV'
      *
     O          E            DET            1
     O                       ELFND         Z   +000
     O                       ELEMP#        Z   +002
     O                       ELDIV         Z   +002
     O                       ELFYR         Z   +002
     O                       XXDV1          B  +005
     O                       XXDV2          B  +005
     O                       XXPC1A         B  +005
     O                       XXPC1B         B  +005
     O                       XXDB           B  +005
     O                       XXCR           B  +005
     O                       XXWVPC         B  +005
      *
     O          E            TOT         2  1
     O                                           20 'COUNT:'
     O                       COUNT         Z   +005
      *
      *
