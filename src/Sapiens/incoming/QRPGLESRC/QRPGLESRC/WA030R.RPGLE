     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE WA030R - API TO UPDATE WMCMP WITH MMI DATE
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This API updates the Final MMI Date field in WMCMP      **
     H**   CALLED FROM:                                              **
     H**     F0160R01 - Work with MMI Information update/display     **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   04/20/98      PROJECT: 51MCK                   **
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H* 05/21/98 C:15 Support: WC051/11762                            *
     H*               Removed line which loads 'A' into CMSTAT.  This *
     H*               field should only be updated in W0070R12 and    *
     H*               W8608R.                                         *
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      50      General usage                                  **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      WMCML1 (PCM02)  : WCMS Claim File (CO#/FND/FYR/CASE)   **
     H*****************************************************************
     H*
     FWMCML1    UF   E           K DISK

     D*================================================================
     D*  External definitions
     D*================================================================
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*================================================================
     D*  TIME Work area
     D*================================================================
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQDATE
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*          WCMS Claim File (PCM01)
     C     CMKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CMCO#         QQCO#
     C     *LIKE         DEFINE    CMFND         QQFND
     C     *LIKE         DEFINE    CMFYR         QQFYR
     C     *LIKE         DEFINE    CMCASE        QQCASE
     C     *LIKE         DEFINE    CMMMI         QQDATE
     C*
     C*================================================================
     C*          M A I N L I N E
     C*================================================================
     C*
     C*          Update WMCMP
     C*          ---------------------------------------------------------
     C     CMKY          CHAIN     PCM01                              50
     C     *IN50         IFEQ      *OFF
     C                   Z-ADD     QQDATE        CMMMI
     C*** BEGIN DLT *** 05/21/98 **************************************
     C******************** MOVE 'A'       CMSTAT
     C*** END DLT ***** 05/21/98 **************************************
     C                   MOVE      Q1USER        CMUSER
     C                   MOVE      Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CMTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   UPDATE    PCM01
     C                   ENDIF                                                  *IN50 = Off
     C*
     C*          End the API
     C*          ---------------------------------------------------------
     C                   RETURN
