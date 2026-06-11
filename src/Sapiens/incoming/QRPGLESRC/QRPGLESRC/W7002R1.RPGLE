     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR**dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W7002R1 - GROUP SERVICE DATE RETRIEVAL
     H*****************************************************************
     H* DESCRIPTION:                                                  *
     H*      THIS PROGRAM WILL GO OUT TO THE GROUP CHECK PROCESSING   *
     H*      FILE AND GET THE FROM AND TO DATE RANGE AND PASS THEM    *
     H*      BACK TO THE CHECK PRINT PGM.                             *
     H* CALLED FROM: W7002R                                           *
     H* CALLS TO:    NONE                                             *
     H* 09/08/94  C:15  PROJECT: WC361                                *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     FWAC3L     IF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    CPTODT        XXDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
     C                   PARM                    QQFRDT            6
     C                   PARM                    QQTODT            6
     C                   PARM                    QQFND             3
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*****************************************************************
     C*   KEY LISTING FOR PROGRAM FILES
     C*****************************************************************
     C*
     C*   KEY LIST FOR THE CHECK PROCESSING FILE
     C*
     C     CPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C                   KFLD                    XXFND
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQDATE        XXDT8             8 0
     C*
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   MOVE      QQFND         XXFND             3 0
     C*
     C*   GO OUT AND GET FROM AND TO SERVICE DATES FOR GROUP PROCESSING
     C*
     C     CPKY          CHAIN     PCP01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     CPFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         QQFRDT
     C*
     C                   Z-ADD     CPTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         QQTODT
     C*
     C                   END
     C*
     C*  END PGM
     C*
     C                   MOVE      '1'           *INLR
     C*
