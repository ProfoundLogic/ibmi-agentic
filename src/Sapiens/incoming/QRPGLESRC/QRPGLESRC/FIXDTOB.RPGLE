     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***** Beg Add *** 06/09/20 ************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H*****************************************************************
     H/TITLE FIXDTOB - Fix FMCDP CDDTOB
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program reads FMCDP;
     H**                                update DTOB from WMCDP
     H**   CALLED FROM:                                              **
     H**     Command Line                                            **
     H**   CALLS TO:                                                 **
     H**     None                                                    **
     H**   PROGRAMMER: C:58                                          **
     H**   CREATED:   02/26/99
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*****************************************************************
     H*
     H* 06/09/20 C:295 Project: SY420                                 *
     H*                -Mask SSN                                      *
     H*                                                               *
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      50      General usage                                  **
     H*****************************************************************
     H*
     H*****************************************************************
     H**   FILES:                                                    **
     H*****************************************************************
     H*
     FFMCDP     UF   E             DISK
     FWMCDL     IF   E           K DISK
     F                                     RENAME(PCD01:PCD02)
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D* ---------------------------------------------------------------
     D* EXTERNAL SYSTEM INFORMATION DATA STRUCTURE
     D* ---------------------------------------------------------------
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D* ---------------------------------------------------------------
     D* SYSTEM DATE AND TIME
     D* ---------------------------------------------------------------
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     I*
     IPCD02
     I              CDLSN                       C2LSN
     I              CDFSN                       C2FSN
     I              CDMID                       C2MIC
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
     I              CDIUSR                      C2IUSR
     I              CDCNTY                      C2CNTY
     I              CDWARA                      C2WARA
     I              CDWPH#                      C2WPH#
     I              CDIPGM                      C2IPGM
     I              CDITM                       C2ITM
     I              CDIDT                       C2IDT
     I*

     C*
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*          M A I N L I N E
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*
     C*          GET TIME AND PRINT HEADING
     C*          ------------------------------------------------------
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*
     C*          Read all FMCDP records
     C*          ------------------------------------------------------
     C                   READ      PCD01                                  50
     C     *IN50         DOWEQ     *OFF
     C                   ADD       1             INCNT             7 0
     C     CDSS#         CHAIN     PCD02                              55
     C     *IN55         IFEQ      *OFF
     C*
     C     C2DTOB        IFNE      *ZERO
     C     C2DTOB        ANDNE     CDDTOB
     C*
     C     *INOA         IFEQ      *ON
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOA
     C                   ENDIF                                                  . OA ON     nks
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
     C*   UPDATE FILE
     C                   Z-ADD     C2DTOB        CDDTOB
     C                   UPDATE    PCD01
     C                   ADD       1             UPCNT             7 0
     C                   ENDIF                                                  . 55 OFF    nks
     C                   ENDIF                                                  . DTOB<2DTOBnks
     C*
     C*             Read next record
     C*             ---------------------------------------------------
     C                   READ      PCD01                                  50
     C                   ENDDO                                                  *IN50 = Off
     C*
     C*          End of Program
     C*          ------------------------------------------------------
     C                   SETON                                        LR
      /EJECT
     O*------------------------------------------------------
     O* PRINTER AUDIT FILE
     O*------------------------------------------------------
     O*
     OQSYSPRT   T    LR                     1
     O                       INCNT         Z     10
     O                                           28 'RECORDS READ FROM'
     O                                           37 'FMCDP   '
     O          T    LR                     2
     O                       UPCNT         Z     10
     O                                           35 'RECORDS UPDATED IN FMCDP'
     O*
     O          E            HEAD           1 06
     O                       DSTIME               8 '  :  :  '
     O                                           66 'UPDATE INVALID DTOB '
     O                                           74 'IN FMCDP'
     O                       UDATE         Y    132
     O*
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       PAGE          Z    132
     O                                          128 'PAGE'
     O          E            HEAD           3
     O                                            8 'SOC SEC#'
     O                                           19 'DATE WAS'
     O                                           31 'CHANGED TO'
     O*
     O          E            DET            1
     O                       CDSS#               10
     O                       CDDTOB        Y     19
     O                       C2DTOB        Y     31
