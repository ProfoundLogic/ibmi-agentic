     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE W7442R - POSTER LABELS FOR EFF DATE RANGE - WORKFILE BUILD
     H*****************************************************************
     H** PROGRAM DESCRIPTION:                                         *
     H**                                                              *
     H** CALLED FROM:                                                 *
     H**    W7441C                                                    *
     H**                                                              *
     H** CALLS TO:                                                    *
     H**                                                              *
     H** PROGRAMMER: C:35                                             *
     H** CREATED:    10/30/95         PROJECT: CM008                  *
     H**                                                              *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/26/00  C:35   PROJECT: SUPPORT                             *
     H*                  LOAD FULL NINE DIGITS TO XXTEMP.             *
     H*                                                               *
     H*****************************************************************
     H* INDICATORS USED:                                              *
     H*                                                               *
     H*                                                               *
     H* 60 - GENERAL CHAIN                                            *
     H*                                                               *
     H*****************************************************************
     H**                                                              *
     H** FILE USAGE:                                                  *
     H**                                                              *
     H** DISPLAY FILES:                                               *
     H**    (NONE)                                                    *
     H**                                                              *
     H** DATA FILES:                                                  *
     H**    WDELL3 (PEL01): CO#/FND/PFDT                              *
     H**    WMEML  (PEM01): CO#/FND/EMP#/DIV                          *
     H**    WXERL  (PER01): --------                                  *
     H**                                                              *
     H*****************************************************************
     F*
     FWDELL3    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWXERL     O    E           K DISK
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C*****************************************************************
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELPFDT        XXFFDT
     C     *LIKE         DEFINE    ELPFDT        XXTFDT
     C     *LIKE         DEFINE    ELPFDT        XXPFDT
     C     *LIKE         DEFINE    ELEMP#        XXFEMP
     C     *LIKE         DEFINE    ELEMP#        XXTEMP
     C     *LIKE         DEFINE    EMRGN#        XXFRGN
     C     *LIKE         DEFINE    EMRGN#        XXTRGN
     C*
     C                   MOVE      *BLANK        XXSORT            1
     C*
     C*****************************************************************
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*****************************************************************
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPFDT
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*****************************************************************
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFFDT            8
     C                   PARM                    QQTFDT            8
     C                   PARM                    QQSORT            1
     C                   PARM                    QQFEMP            9
     C                   PARM                    QQTEMP            9
     C                   PARM                    QQFRGN            3
     C                   PARM                    QQTRGN            3
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
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFFDT        X1FFDT            8 0
     C                   Z-ADD     X1FFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFFDT
     C                   MOVE      QQTFDT        X1TFDT            8 0
     C                   Z-ADD     X1TFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTFDT
     C                   MOVE      QQSORT        XXSORT
     C                   MOVE      QQFEMP        XXFEMP
     C                   MOVE      QQTEMP        XXTEMP
     C                   MOVE      QQFRGN        XXFRGN
     C                   MOVE      QQTRGN        XXTRGN
     C*
     C     XXFEMP        IFEQ      0
     C     XXTEMP        ANDEQ     0
     C                   Z-ADD     1             XXFEMP
      **** BEGIN CHG **** 05/26/00 *********************************************
     C******************** Z-ADD99999     XXTEMP
     C                   Z-ADD     999999999     XXTEMP
      **** END   CHG **** 05/26/00 *********************************************
     C                   ENDIF                                                  XXFEMP IFEQ
     C*
     C     XXFRGN        IFEQ      0
     C     XXTRGN        ANDEQ     0
     C                   Z-ADD     1             XXFRGN
     C                   Z-ADD     999           XXTRGN
     C                   ENDIF                                                  XXFRGN IFEQ
     C*
     C                   Z-ADD     0             EMRGN#
     C*
     C* READ EMPLOYER DETAIL RECORDS IN EFFECTIVE DATE ORDER
     C*
     C                   MOVE      XXFFDT        XXPFDT
     C*
     C     ELKY          SETLL     PEL01
     C     ELKY2         READE     PEL01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ELPFDT        ANDLE     XXTFDT
     C*
     C     XXSORT        IFEQ      'R'
     C*
     C                   MOVE      ELEMP#        XXEMP#
     C                   MOVE      ELDIV         XXDIV
     C*
     C     EMKY          CHAIN     PEM01                              61
     C*
     C     *IN61         IFEQ      *ON
     C                   Z-ADD     0             EMRGN#
     C                   ENDIF                                                  *IN61 IFEQ
     C*
     C                   ENDIF                                                  *IN40 IFEQ
     C*
     C     ELEMP#        IFGE      XXFEMP
     C     ELEMP#        ANDLE     XXTEMP
     C*
     C     XXSORT        IFEQ      'E'
     C     EMRGN#        ORGE      XXFRGN
     C     EMRGN#        ANDLE     XXTRGN
     C*
     C                   MOVE      ELCO#         ERCO#
     C                   MOVE      ELFND         ERFND
     C                   MOVE      ELFYR         ERFYR
     C                   MOVE      ELEMP#        EREMP#
     C                   MOVE      ELDIV         ERDIV
     C                   MOVE      EMRGN#        ERRGN#
     C*
     C                   WRITE     PER01
     C*
     C                   ENDIF                                                  *IN40 IFEQ
     C*
     C                   ENDIF                                                  ELEMP# IFGE
     C*
     C     ELKY2         READE     PEL01                                  60
     C*
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
