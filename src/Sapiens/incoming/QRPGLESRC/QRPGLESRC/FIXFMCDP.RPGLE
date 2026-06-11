     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***** Beg Add *** 06/09/20 ************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H* COPYRIGHT PROGRESSIVE DATA SOLUTIONS, INC. 2000
     H*
     H/TITLE PROGRAM TO COMPARE WMCDP / FMCDP
     H*****************************************************************
     H* Description:  This pgm prints a list of claims which have a
     H*   discrepancy between the demographic records in WC vs MC.
     H*   If mode parm is 'U' then records in MC (FMCDP) are updated
     H*   or written as needed.
     H*****************************************************************
     H*****************************************************************
     H** REVISIONS:
     H*                                                               *
     H* 06/09/20 C:295 Project: SY420                                 *
     H*                -Mask SSN                                      *
     H*                                                               *
     H*****************************************************************
     FWMCDL     IPE  E           K DISK
     FFMCDL     UF A E           K DISK
     F                                     RENAME(PCD01:PCD02)
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D**************************************************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     H*****************************************************************

     IPCD01         51
     IPCD02
     I              CDLSN                       C2LSN
     I              CDFSN                       C2FSN
     I              CDMID                       C2MID
     I              CDADR1                      C2ADR1
     I              CDADR2                      C2ADR2
     I              CDCTY                       C2CTY
     I              CDST                        C2ST
     I              CDZIP                       C2ZIP
     I              CDDTOB                      C2DTOB
     I              CDSEX                       C2SEX
     I              CDSTAT                      C2STAT
     I              CDUSER                      C2USER
     I              CDPGM                       C2PGM
     I              CDTIME                      C2TIME
     I              CDDT                        C2DT
     I              CDAREA                      C2AREA
     I              CDPHN#                      C2PHN#
     I              CDMSTS                      C2MSTS
     I              CDWARA                      C2WARA
     I              CDWPH#                      C2WPH#

     C**************************************************************************
     C*  Start WCMS main loop
     C*
     C     *IN51         IFEQ      *ON
     C                   ADD       1             INREC             7 0
     C*
     C     CDKY          CHAIN     PCD02                              61
     C     *IN61         IFEQ      '0'
     C*
     C     C2LSN         IFNE      CDLSN
     C     C2FSN         ORNE      CDFSN
     C     C2MID         ORNE      CDMID
     C                   ADD       1             UPDTD             7 0
     C*
     C     C2LSN         CAT(P)    ',':0         SVNAME
     C                   CAT       C2FSN:1       SVNAME
     C                   CAT       C2MID:1       SVNAME
     C*
     C     CDLSN         CAT(P)    ',':0         XXNAME
     C                   CAT       CDFSN:1       XXNAME
     C                   CAT       CDMID:1       XXNAME
     C*
     C   OF              EXCEPT    HEAD
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
        CDSS# = MaskData('SSN':'CDSS#':CDSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   EXCEPT    DET
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
        CDSS# = UnMaskData('SSN':'CDSS#':CDSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C     QQMODE        IFEQ      'U'
     C                   EXSR      MOVREC
     C                   UPDATE    PCD02
     C                   ENDIF                                                  .MODE = U
     C                   ENDIF                                                  .CDLSN vs C2
     C*
     C*
     C*     NOT FOUND - WRITE ONE
     C                   ELSE                                                   . 61 OFF
     C*
     C     QQMODE        IFEQ      'U'
     C                   EXSR      MOVREC
     C                   WRITE     PCD02
     C                   ENDIF                                                  .MODE = U
     C                   ADD       1             ADDREC            7 0
     C                   ENDIF                                                  . 61 OFF
     C*
     C                   ENDIF                                                  . 51 ON
     C*
     CLR                 EXCEPT    HEAD
     CLR                 EXCEPT    TOT
     C**************************************************************************
     C**         MOVREC    Move Data
     C**************************************************************************
     C     MOVREC        BEGSR
     C                   MOVEL     CDLSN         C2LSN
     C                   MOVEL     CDFSN         C2FSN
     C                   MOVEL     CDMID         C2MID
     C                   MOVEL     CDADR1        C2ADR1
     C                   MOVEL     CDADR2        C2ADR2
     C                   MOVEL     CDCTY         C2CTY
     C                   MOVEL     CDST          C2ST
     C                   Z-ADD     CDZIP         C2ZIP
     C                   Z-ADD     CDDTOB        C2DTOB
     C                   MOVEL     CDSEX         C2SEX
     C                   MOVEL     CDSTAT        C2STAT
     C                   MOVEL     CDUSER        C2USER
     C                   MOVEL     CDPGM         C2PGM
     C                   Z-ADD     CDTIME        C2TIME
     C                   Z-ADD     CDDT          C2DT
     C                   Z-ADD     CDAREA        C2AREA
     C                   Z-ADD     CDPHN#        C2PHN#
     C                   MOVEL     CDMSTS        C2MSTS
     C                   Z-ADD     CDWARA        C2WARA
     C                   Z-ADD     CDWPH#        C2WPH#
     C*
     C                   ENDSR
     C**************************************************************************
     C     *INZSR        BEGSR
     C**************************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQMODE            1
     C*
     C*  Key list for the Company file
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*  Key list for the Claimant Demographics file
     C*
     C     CDKY          KLIST
     C                   KFLD                    CDSS#
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *BLANKS       SVNAME           40
     C                   MOVE      *BLANKS       XXNAME           40
     C                   EXCEPT    HEAD
     C                   ENDSR
     C*
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              96
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            6 'SS#'
     O                                           26 'Previous name'
     O                                           69 'Correct name'
     O          E            DET
     O                       CDSS#               10
     O                       SVNAME              53
     O                       XXNAME              97
     O          E            TOT         3  3
     O                                           26 '** TOTALS ** '
     O          E            TOT            1
     O                                           22 '  NUMBER READ'
     O                                           44 ' . . . . . . . . . . .'
     O                       INREC         4     52
     O          E            TOT            1
     O                                           22 '  - NOT FOUND'
     O                                           44 '-WRITTEN . . . . . . .'
     O                       ADDREC        4     52
     O          E            TOT            1
     O                                           22 '  - DIFFERENT'
     O                                           44 '--UPDATED  . . . . . .'
     O                       UPDTD         4     52
