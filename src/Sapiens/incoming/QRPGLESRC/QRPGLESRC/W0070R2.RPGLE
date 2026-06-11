     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0070R2 - CLAIM DELETION PGM
     H*
     H*****************************************************************
     H* DESCRIPTION:  THIS PGM IS CALLED WHEN A CLAIM IS DELETED.     *
     H*    IT DELETES THE CLAIM INFORMATION FROM THE FILES ADDED FOR  *
     H*    THE EDI PROCESS.                                           *
     H*                                                               *
     H* CALLED FROM:  W0070R, W7625R                                  *
     H* CALLS TO:     NONE                                            *
     H*                                                               *
     H* CREATED BY:   15                                              *
     H* PROJECT:      25WDE             01/04/96                      *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*  *IN60 - GENERAL USAGE (CHAIN, READ, ect.)                    *
     H*****************************************************************
     FSSYLF     IF   E           K DISK
     FWMAWL     UF   E           K DISK
     FWMBNL     UF   E           K DISK
     FWMBYL     UF   E           K DISK
     FWMNIL     UF   E           K DISK    USROPN

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*  CLAIM KEY LIST
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQRTCD            1
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
     C     *LIKE         DEFINE    AWCO#         XXCO#
     C     *LIKE         DEFINE    AWFND         XXFND
     C     *LIKE         DEFINE    AWFYR         XXFYR
     C     *LIKE         DEFINE    AWCASE        XXCASE
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   TIME                    DSDTTM
     C*
     C*  GET THE SYSTEM CONTROL RECORD
     C*
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C*
     C*  IF EDI IS INSTALLED, OPEN THE APPROPRIATE FILES.
     C*
     C     WCFL09        IFEQ      'Y'
     C                   OPEN      WMNIL
     C                   END
     C*
     C*  DELETE RECORDS FROM AWW HISTORY FILE
     C*
     C     *IN60         DOUEQ     '1'
     C     CMKY          DELETE    PAW01                              60
     C                   END
     C*
     C*  DELETE RECORDS FROM THE BODY PART FILE
     C*
     C     *IN60         DOUEQ     '1'
     C     CMKY          DELETE    PBY01                              60
     C                   END
     C*
     C*  DELETE RECORDS FROM THE BENEFIT ADJUSTMENT FILE
     C*
     C     *IN60         DOUEQ     '1'
     C     CMKY          DELETE    PBN01                              60
     C                   END
     C*
     C*  DELETE RECORDS FROM THE STATE ENTRY FILES IF EDI IS INSTALLED.
     C*
     C     WCFL09        IFEQ      'Y'
     C*
     C*
     C*
     C     *IN60         DOUEQ     '1'
     C     CMKY          DELETE    PNI01                              60
     C                   END
     C*
     C                   END
     C*
     C*  END OF PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
