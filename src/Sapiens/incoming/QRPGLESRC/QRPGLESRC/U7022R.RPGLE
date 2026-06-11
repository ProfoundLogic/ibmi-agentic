     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE U7022R - EDIT SELECTED CLAIM WORKFILE BUILD PROGRAM
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    WORKFILE BUILD PROGAM FOR C/R REVEIW                       *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*   U7020C                                                      *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*   NONE                                                        *
     H*                                                               *
     H*  PROGRAMMER: KEN RUTHVEN        PROJECT: 99AAF                *
     H*  CREATED: 10/10/92                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*  60        READ PCM01/DOWEQ '0'                               *
     H*****************************************************************
     FFMCML1    IF   E           K DISK
     FUAESP     O  A E           K DISK

     D*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQEDIT            5
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C* INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    CMSUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    CMFND         XXFND                          3.0N
     C*
     C* MOVE ENTRY PARMS TO WORK FIELDS
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C*
     C* READ CLAIM FILE AND CHECK AGAINST SELECTION CRITERIA FOR WORK
     C* FILE BUILD
     C*
     C     XXSUB#        IFEQ      *ZEROS
     C     XXFND         ANDEQ     *ZEROS
     C                   READ      PCM01                                  60
     C     *IN60         DOWEQ     '0'
     C                   EXSR      WRTWK
     C                   READ      PCM01                                  60
     C                   END                                                    END *IN60
     C*
     C                   ELSE                                                   NE *ZEROS
     C*
     C     XXSUB#        IFNE      *ZEROS
     C     XXFND         ANDEQ     *ZEROS
     C     CMKY1         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C                   EXSR      WRTWK
     C     CMKY1         READE     PCM01                                  60
     C                   END                                                    END *IN60
     C*
     C                   ELSE                                                   EQ *ZEROS
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C                   EXSR      WRTWK
     C     CMKEY         READE     PCM01                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    XXSUB <> ZEROS
     C                   END                                                    XXSUB = ZEROS
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C* WRTWK - SUBROUTINE TO WRITE RECORDS TO WORKFILE
     C*****************************************************************
     C     WRTWK         BEGSR
     C*
     C                   Z-ADD     CMCO#         ESCO#
     C                   Z-ADD     CMSUB#        ESSUB#
     C                   Z-ADD     CMFND         ESFND
     C                   Z-ADD     CMFYR         ESFYR
     C                   Z-ADD     CMCASE        ESCASE
     C                   Z-ADD     *ZEROS        ESADT
     C                   MOVE      *BLANKS       ESFL01
     C                   MOVE      *BLANKS       ESFL02
     C                   MOVE      *BLANKS       ESFL03
     C                   MOVE      *BLANKS       ESFL04
     C                   MOVE      *BLANKS       ESFL05
     C                   Z-ADD     *ZEROS        ESDT01
     C                   Z-ADD     *ZEROS        ESDT02
     C                   Z-ADD     *ZEROS        ESDT03
     C                   Z-ADD     *ZEROS        ESDT04
     C                   Z-ADD     *ZEROS        ESDT05
     C                   MOVE      ' '           ESSTAT
     C                   MOVEL     Q1USER        ESUSER
     C                   MOVEL     Q1PGM         ESPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ESDT
     C*
     C                   Z-ADD     DSTIME        ESTIME
     C                   MOVEL     Q1USER        ESIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ESIDT
     C*
     C                   WRITE     PES01
     C*
     C                   ENDSR
