     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE BILLLD - MASS BILLING
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This creates billing records for all policys.          **
     H**   CALLED FROM:                                             **
     H**     NONE                                                   **
     H**   CALLS TO:                                                **
     H**     NONE                                                   **
     H**   PROGRAMMER: 31                                           **
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
     H**                                                            **
     H**      50       General usage                                **
     H**      51       General usage                                **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H** FILES:                                                     **
     H**    CMPML   : Policy Master File                            **
     H**    CAPTP   : Policy Transaction File                       **
     H**    SMCOL   : System Company Master File (CO#)              **
     H****************************************************************
     H*
     FSMCOL     UF   E           K DISK
     FCMPML     IF   E           K DISK
     FCAPTP     O    E           K DISK

     D*
     D TABPOL          S             13    DIM(6) CTDATA PERRCD(1)              EXCLUDE POLICIES
     D*
     D* -----------------------------------------------------
     D* EXTERNAL SYSTEM INFORMATION DATA STRUCTURE
     D* -----------------------------------------------------
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D* -----------------------------------------------------
     D*        TIME AND DATE
     D* -----------------------------------------------------
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C* -----------------------------------------------------
     C*        PARM LIST
     C* -----------------------------------------------------
     C*
     C     *ENTRY        PLIST
     C                   PARM                    XXCO#             3
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
     C* -----------------------------------------------------
     C*        KEY LISTS
     C* -----------------------------------------------------
     C*
     C*          COMPANY FILE
     C     COKEY         KLIST
     C                   KFLD                    PMCO#
     C*
     C*          POLICY FILE
     C     PMKEY         KLIST
     C                   KFLD                    XXCONO
      /EJECT
     C*---------------------------------------------------------------
     C*---------------------------------------------------------------
     C*          M A I N L I N E
     C*---------------------------------------------------------------
     C*---------------------------------------------------------------
     C*
     C*          GET FIRST POLICY RECORD
     C*          -----------------------------------------------------
     C                   MOVE      XXCO#         XXCONO            3 0
     C     PMKEY         CHAIN     PPM01                              50
     C*
     C*          GET BATCH UPDATE LOG NUMBER FROM COMPANY MASTER
     C*          -----------------------------------------------------
     C     *IN50         IFEQ      *OFF
     C     COKEY         CHAIN     PCO01                              50
     C                   ELSE
     C                   GOTO      END
     C                   END
     C                   ADD       1             COBUL1
     C                   UPDATE    PCO01
     C                   Z-ADD     COBUL1        PTBUL1
     C*
     C*          SET UP FIXED VALUE DATA FIELDS
     C*          -----------------------------------------------------
     C                   Z-ADD     PMCO#         PTCO#
     C                   MOVE      'NB'          PTTRNS
     C                   MOVE      'NB'          PTPYCD
     C                   MOVE      'B'           PTPYTY
     C                   MOVE      ' '           PTSTAT
     C*
     C*          SET UP TIME STAMP FIELDS
     C*          -----------------------------------------------------
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        PTUSER
     C                   MOVE      Q1PGM         PTPGM
     C                   Z-ADD     DSTIME        PTTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PTDT
     C*
     C*          LOOP THROUGH POLICY FILE
     C*          -----------------------------------------------------
     C     *IN50         DOUEQ     *ON
     C                   MOVE      PMPOL#        PTPOL#
     C                   Z-ADD     PMEFDT        PTEFDT
     C*
     C*                    WRITE POLICIES NOT EXCLUDED BY LOKUP
     C*                    -------------------------------------------
     C     PTPOL#        LOOKUP    TABPOL                                 51
     C     *IN51         IFEQ      *OFF
     C                   WRITE     PPT01
     C                   END
     C*
     C*                    READ NEXT RECORD
     C*                    -------------------------------------------
     C     PMKEY         READE     PPM01                                  50
     C                   ENDDO
     C*
     C*          END OF PROGRAM
     C*          -----------------------------------------------------
     C     END           TAG
     C                   SETON                                        LR
**
CA-000001501A
CA-000001970A
CA-000001971A
CA-000002321A
CA-000002343A
CA-000002498A
