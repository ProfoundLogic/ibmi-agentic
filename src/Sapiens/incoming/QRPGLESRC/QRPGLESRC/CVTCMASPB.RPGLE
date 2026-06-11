     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE CVTCMASP - CONVERT DEBI FROM CMA1P TO CMASP
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program takes the DEBI code from the first CMA1P  **
     H**     record for a policy/section and propagates it through  **
     H**     the remaining associated CMA1P and CMASP records.      **
     H**   CALLED FROM:                                             **
     H**     NONE                                                   **
     H**   CALLS TO:                                                **
     H**     NONE                                                   **
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
     H**      50       General usage.                               **
     H**      51       General usage.                               **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H**   FILES:                                                   **
     H**   SMUPL  (PUP01): User Parameter File (USR)                **
     H**   CMA1L  (PA101): Auto Liability Cov Detail (CO#/POL#/CC)  **
     H**   CMASL  (PAS01): Auto Liability Symbols (CO#/POL#/CC)     **
     H****************************************************************
     FSMUPL     IF   E           K DISK
     FCMA1L     UF   E           K DISK
     FCMASL     UF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*
     D* -----------------------------------------------------
     D* EXTERNAL SYSTEM INFORMATION DATA STRUCTURE          -
     D* -----------------------------------------------------
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D* -----------------------------------------------------
     D* SYSTEM DATE AND TIME                                -
     D* -----------------------------------------------------
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C* -----------------------------------------------------
     C*                 SEARCH KEYS                         -
     C* -----------------------------------------------------
     C*
     C*          PUP01 - USER PARAMETER FILE
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
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*          PAS01 - AUTO LIABILITY SYMBOLS FILE
     C*
     C     ASKY          KLIST
     C                   KFLD                    A1CO#
     C                   KFLD                    A1POL#
     C                   KFLD                    A1CC
     C     *LIKE         DEFINE    ASCO#         XXCO#
     C     *LIKE         DEFINE    ASPOL#        XXPOL#
     C     *LIKE         DEFINE    ASCC          XXCC
     C     *LIKE         DEFINE    ASDEBI        XXDEBI
     C     *LIKE         DEFINE    ASDEBI        XYDEBI
     C*
     C* -----------------------------------------------------
     C*         M A I N L I N E   R O U T I N E
     C* -----------------------------------------------------
     C*
     C                   EXSR      SR01
     C     *INLR         IFEQ      *OFF
     C*
     C*          LOOP THROUGH CMA1P
     C*          --------------------------------------------
     C                   READ      PA101                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C*               UPDATE CMA1P RECORDS WITH EQUAL KEY
     C*               ---------------------------------------
     C     A1CO#         IFEQ      XXCO#
     C     A1POL#        ANDEQ     XXPOL#
     C     A1CC          ANDEQ     XXCC
     C                   MOVE      A1DEBI        XYDEBI
     C                   MOVE      XXDEBI        A1DEBI
     C                   UPDATE    PA101
     C     XXCNT         IFEQ      0
     C     XXCNT         OREQ      55
     C                   EXCEPT    HEAD
     C                   Z-ADD     0             XXCNT             3 0
     C                   END
     C                   EXCEPT    DET1
     C                   ADD       1             XXCNT
     C                   ADD       1             A1CNT             7 0
     C*
     C*               NEW KEY - SAVE IT AND UPDATE CMASP
     C*               ---------------------------------------
     C                   ELSE
     C                   MOVE      A1DEBI        XXDEBI
     C     ASKY          CHAIN     PAS01                              51
     C     *IN51         DOWEQ     *OFF
     C                   MOVE      XXDEBI        ASDEBI
     C                   UPDATE    PAS01
     C     XXCNT         IFEQ      0
     C     XXCNT         OREQ      55
     C                   EXCEPT    HEAD
     C                   Z-ADD     0             XXCNT             3 0
     C                   END
     C                   EXCEPT    DET2
     C                   ADD       1             XXCNT
     C                   ADD       1             ASCNT             7 0
     C     ASKY          READE     PAS01                                  51
     C                   ENDDO
     C                   END
     C                   MOVE      A1CO#         XXCO#
     C                   MOVE      A1POL#        XXPOL#
     C                   MOVE      A1CC          XXCC
     C                   READ      PA101                                  50
     C                   ENDDO
     C                   END
     C*
     C*          END THE PROGRAM
     C*          --------------------------------------------
     C                   SETON                                        LR
     C*
     C* *****************************************************
     C*               S U B R O U T I N E S                 *
     C* *****************************************************
     C*
     C* -----------------------------------------------------
     C* SR01 - 1ST PASS PROCESSING
     C* -----------------------------------------------------
     C*
     C*          GET TIME AND COMPANY
     C*          --------------------------------------------
     C     SR01          BEGSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            6 0
     C     UPKY          CHAIN     PUP01                              50
     C     *IN50         IFEQ      *ON
     C                   SETON                                        LR
     C                   END
     C*
     C     SR01X         ENDSR
     O*------------------------------------------------------
     O* PRINTER AUDIT FILE
     O*------------------------------------------------------
     OQSYSPRT   T    LR                  2  1
     O                       A1CNT         Z     10
     O                                           29 'RECORDS UPDATED IN'
     O                                           35 'CMA1P'
     O          T    LR                     1
     O                       ASCNT         Z     10
     O                                           29 'RECORDS UPDATED IN'
     O                                           35 'CMASP'
     O          E            HEAD           1 06
     O                       DSTIME               8 '  :  :  '
     O                                           61 'CMASP FILE UPDATE  '
     O          E            HEAD           3
     O                       Q1PGM               10
     O                       PAGE          Z    132
     O                                          128 'PAGE'
     O                       UDATE         Y    132
     O*
     O*       DETAIL RECORDS
     O*
     O          E            DET1           1
     O                       A1CO#               35
     O                       A1POL#              55
     O                       A1CC                50
     O                       A1DEBI              65
     O                       XYDEBI              70
     O*
     O          E            DET2           1
     O                       ASCO#                5
     O                       ASPOL#              20
     O                       ASCC                25
     O                       ASDEBI              30
