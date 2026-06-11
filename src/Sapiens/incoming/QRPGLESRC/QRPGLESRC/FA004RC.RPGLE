     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - FA004RC - WRITE WCMC HISTORY RECORD.                          A
     H*                                                                 *
     H***************************************************************** *
     H*  Created on 09/07/04 by c:62 for Project 114WIN                 *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*  06/22/11 C:198Project 114MYS                                 *
     H*                Made SEPTA group to process as TML's.          *
     H*                                                               *
     H*  12/12/06 C:62 PROJECT 144MAC                                 *
     H*                ADD PROCESSING FOR STANDALONE                  *
     H*                                                               *
     H*****************************************************************
     FFMCHP     O    E             DISK
     FFMFNL1    IF   E           K DISK
     F*

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@EFDT            7 0
     C                   PARM                    @@OCST            1
     C                   PARM                    @@EXPL           60
     C                   PARM                    @@CLCD            3
     C                   PARM                    @@RTCD            1
     C*** BEGIN ADD *** 12/12/06 **************************************
     C                   PARM                    @@SUB#            5 0
     C*** END   ADD *** 12/12/06 **************************************
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
     C     *LIKE         DEFINE    CHSUB#        XXSUB#
     C     *LIKE         DEFINE    CHFND         XXFND
     C*
     C     FNKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C*** Beg Add *** 06/22/11 ****************************************
     C     Q1PRMS        IFGT      9
     C*
     C                   MOVE      @@CO#         ##CO#
     C                   MOVE      @@SUB#        ##SUB#
     C                   MOVE      @@FND         ##FND
     C*
     C                   CALL      'FA701R'
     C                   PARM                    ##RTCD            1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##SUB#            5
     C                   PARM                    ##FND             3
     C                   PARM      *BLANKS       ##OUTS            1
     C                   ENDIF                                                  Q1PRMS > 9
     C*** End Add *** 06/22/11 ****************************************
     C*** BEGIN ADD *** 12/12/06 **************************************
     C*
     C* RETRIEVE CLIENT ID
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C*
     C* IS THIS TML?
     C*
     C                   MOVE      *OFF          C144              1
     C     ##CLT#        IFEQ      '144'
     C                   MOVE      *ON           C144
     C                   ENDIF
     C*** END   ADD *** 12/12/06 **************************************
     C*
     C     @@RTCD        IFEQ      '*'
     C                   SETON                                        LR
     C                   ELSE
     C*
     C*** BEGIN ADD *** 12/12/06 **************************************
     C     Q1PRMS        IFGT      9
     C     C144          ANDEQ     *ON
     C*** Beg Add *** 06/22/11 ****************************************
     C     Q1PRMS        ORGT      9
     C     ##OUTS        ANDEQ     'Y'
     C*** End Add *** 06/22/11 ****************************************
     C                   MOVE      @@SUB#        XXSUB#
     C                   MOVE      @@FND         XXFND
     C                   ELSE
     C*** END   ADD *** 12/12/06 **************************************
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     FNSUB#        XXSUB#
     C                   Z-ADD     FNFND         XXFND
     C                   ENDIF
     C*** BEGIN ADD *** 12/12/06 **************************************
     C                   ENDIF
     C*** END   ADD *** 12/12/06 **************************************
     C*
     C                   EXSR      SRWRIT
     C                   RETURN
     C                   ENDIF
     C*================================================================
     C* SRWRIT - WRITES FMCHP
     C*================================================================
     C     SRWRIT        BEGSR
     C*
     C                   CLEAR                   PCH01
     C*
     C                   Z-ADD     @@CO#         CHCO#
     C                   Z-ADD     XXSUB#        CHSUB#
     C                   Z-ADD     XXFND         CHFND
     C                   Z-ADD     @@FYR         CHFYR
     C                   Z-ADD     @@CASE        CHCASE
     C                   MOVEL     @@OCST        CHOCST
     C                   MOVEL(P)  @@EXPL        CHEXPL
     C                   Z-ADD     @@EFDT        CHEFDT
     C                   MOVEL     @@CLCD        CHCLCD
     C*
     C                   MOVE      'A'           CHSTAT
     C*
     C                   MOVEL     Q1USER        CHIUSR
     C                   MOVEL     Q1USER        CHUSER
     C                   MOVEL     Q1PGM         CHIPGM
     C                   MOVEL     Q1PGM         CHPGM
     C                   TIME                    DSDTTM
     C                   MOVEL     DSTIME        CHITIM
     C                   MOVEL     DSTIME        CHTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CHIDT
     C                   MOVEL     SRDTO         CHDT
     C*
     C                   WRITE     PCH01
     C*
     C                   FEOD      FMCHP
     C*
     C                   ENDSR
