     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE CADJAR - CREATE $5 WRITE OFF RECORDS
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program creates adjustment records for policies   **
     H**     with +/- $5 balance.                                   **
     H**   CALLED FROM:                                             **
     H**     ______ - ___________________________                   **
     H**   CALLS TO:                                                **
     H**     ______ - ___________________________                   **
     H**   PROGRAMMER: C:31                                         **
     H****************************************************************
     H*
     H****************************************************************
     H** REVISIONS:                                                 **
     H**  11/18/96 *** C:107 *** V2000 ***                          **
     H**           1. Version 2 format changes.                     **
     H****************************************************************
     H*
     H****************************************************************
     H** INDICATOR USAGE:                                           **
     H**   INDICATOR            FUNCTION DESCRIPTION                **
     H**      50       General Usage                                **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H**   FILES:                                                   **
     H**    CDARL1 (PAR01): Account Receivable File                 **
     H****************************************************************
     H*
     FCDARL1    UP A E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*                                             ARCC  L1
     D*
     D* -----------------------------------------------------
     D* EXTERNAL SYSTEM INFORMATION DATA STRUCTURE
     D* -----------------------------------------------------
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D* -----------------------------------------------------
     D* SYSTEM DATE AND TIME
     D* -----------------------------------------------------
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* -----------------------------------------------------
     D* ADJUSTMENT DESCRIPTION
     D* -----------------------------------------------------
     D                 DS
     D  XXDESC                 1     30    INZ('$5.00 AND UNDER-
     D                                      WRITE OFF     ')

     I*
     I* -----------------------------------------------------
     I* CDARP LEVEL BREAK
     I* -----------------------------------------------------
     IPAR01
     I                                          ARCO#         L1
     I                                          ARPOL#        L1
      /EJECT

     C*---------------------------------------------------------------
     C*---------------------------------------------------------------
     C*          M A I N L I N E
     C*---------------------------------------------------------------
     C*---------------------------------------------------------------
     C*
     C*          GET TIME AND PRINT HEADING
     C*          --------------------------------------------
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
     C     *INL1         IFEQ      *ON
     C                   SETOFF                                       80
     C                   Z-ADD     0             XXTOT
     C                   END
     C     *IN99         IFEQ      *OFF
     C                   TIME                    DSDTTM
     C                   END
     C                   SETON                                        99
     C*
     C*
     C*          UPDATE TOTAL FOR POLICY / SECTION
     C*          --------------------------------------------
     C     ARPYTY        IFEQ      'I'
     C     ARPYTY        OREQ      'P'
     C                   ADD       ARAMT         XXTOT             9 2
     C                   ELSE
     C                   SUB       ARAMT         XXTOT
     C                   END
     C*
     C*          NEW POLICY / SECTION
     C*          CREATE ADJUSTMENT IF +/- $5.00
     C*          --------------------------------------------
     CL1   XXTOT         IFLE      5.00
     CL1   XXTOT         ANDGE     -5.00
     CL1   XXTOT         ANDNE     0
     CL1                 ADD       XXTOT         LRTOT             9 2
     CL1                 EXSR      SR02
     CL1                 END
     C*
     C*---------------------------------------------------------------
     C*---------------------------------------------------------------
     C*          S U B R O U T I N E S
     C*---------------------------------------------------------------
     C*---------------------------------------------------------------
     C*
     C*
     C*---------------------------------------------------------------
     C*  SR02    CREATE ADJUSTMENT RECORD
     C*---------------------------------------------------------------
     C     SR02          BEGSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARPODT
     C                   MOVE      'AJ'          ARPYCD
     C                   MOVE      'A'           ARPYTY
     C                   MOVEL     XXDESC        ARDESC
     C                   Z-ADD     0             ARCHK#
     C                   Z-ADD     0             ARAGPC
     C                   Z-ADD     XXTOT         ARAMT
     C                   MOVE      'A'           ARSTAT
     C                   MOVE      Q1USER        ARUSER
     C                   MOVE      Q1PGM         ARPGM
     C                   Z-ADD     DSTIME        ARTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C                   Z-ADD     0             ARBUL1
     C                   Z-ADD     0             ARBUL3
     C                   Z-ADD     XXTOT         ARGAMT
     C                   Z-ADD     0             ARCOMA
     C                   WRITE     PAR01
     C                   ADD       1             ADJCNT            7 0
     C                   SETON                                        80
     C                   ENDSR
      /EJECT
     O*------------------------------------------------------
     O* PRINTER AUDIT FILE
     O*------------------------------------------------------
     OQSYSPRT   H    1P                     1 06
     O         OR    OF
     O              N1P      DSTIME               8 '  :  :  '
     O                                           60 'A/R $5.00 WRITE OFF'
     O                                           75 '- AUDIT REPORT'
     O          H    1P                     3
     O         OR    OF
     O                       Q1PGM               10
     O                       PAGE          Z    132
     O                                          128 'PAGE'
     O                       UDATE         Y    132
     O          H    1P                     2
     O         OR    OF
     O                                            3 'CO#'
     O                                           15 'POLICY #'
     O                                           27 'SECTION'
     O                                           41 'AMOUNT'
     O          T    L1 80                  1
     O                       ARCO#                3
     O                       ARPOL#              18
     O                       ARCC                24
     O                       ARAMT         K     42
     O          T    LR                  2  1
     O                       ADJCNT        Z     20
     O                                           37 'RECORDS ADDED TO'
     O                                           57 'ACCOUNTS RECEIVABLE'
     O          T    LR                     1
     O                       LRTOT         K     20
     O                                           34 'TOTAL DOLLARS'
