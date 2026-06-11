     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* API - FA004R - UPDATE WCMC CLAIM MASTER WITH STATUS AND WRITE       A
     H*                HISTORY RECORD.
     H*                                                                 *
     H*  Called From:  FA003R                                           *
     H*                                                                 *
     H*                                                                 *
     H*  Calls To:                                                      *
     H*    None                                                         *
     H*                                                                 *
     H*  Created:  08/26/97  C:53  Project:  87MAZ                      *
     H*                                                                 *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 00/00/00 C:00 Req/Proj:                                       *
     H*                                                               *
     H* 02/05/98 C:62 Req/Proj: MC145                                 *
     H*               1) REMOVE QQEFDT AND QQEXPL FROM ENTRY PARMS.   *
     H*               2) REMOVE UPDATE OF CLAIM MASTER.               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*    99         GENERAL USAGE (CHAIN)                           *
     H*                                                               *
     H*****************************************************************
     F*
     FFMCHP     O    E             DISK
     FFMCMP     IF   E             DISK
     FFMFNL1    IF   E           K DISK
     F*

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D XXEXP1          C                   CONST('Original open of ca-
     D                                     se')
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  CLAIM MASTER KEY LIST
     C*
     C     CMKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@OCST            1
     C                   PARM                    @@RRN             9 0
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
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C*
     C     @@RRN         CHAIN(N)  PCM01                              99
     C*
     C                   MOVE      CMCO#         KKCO#
     C                   MOVE      CMSUB#        KKSUB#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMFYR         KKFYR
     C                   MOVE      CMCASE        KKCASE
     C*
     C     CMSUB#        IFEQ      *ZEROS
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     FNSUB#        KKSUB#
     C                   Z-ADD     FNFND         KKFND
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*** ORIGINAL RECORD ONLY
     C     @@RTCD        IFEQ      'R'                                          ORIG
     C     CMSTAT        ANDEQ     'O'                                          OPEN
     C*
     C                   EXSR      SRWRIT
     C*                    MOVE *ON       *INLR
     C                   RETURN
     C*
     C                   ENDIF
     C*** BEGIN DEL *** 02/05/97 **************************************
     C*
     C*** GET CLAIM MASTER AND UPDATE
     C*
     C********** CMKEY     CHAINPCM01                60
     C**********
     C*** MOVE FIELDS COMMON TO ADD AND CHANGE OF RECORD
     C**********
     C********** *IN60     IFEQ '0'
     C**********
     C**********           SELEC
     C********** QQSTAT    WHEQ 'O'
     C********** CMSTAT    ANDEQ'C'
     C********** QQEXPL    ANDNE*BLANKS
     C**********           MOVE 'R'       CMSTAT
     C**********           EXSR SRUPDT
     C**********           MOVE 'R'       CHOCST
     C**********           MOVELQQEXPL    CHEXPL    P
     C**********           EXSR SRWRIT
     C**********
     C********** QQSTAT    WHEQ 'C'
     C**********           MOVE QQSTAT    CMSTAT
     C**********           EXSR SRUPDT
     C*** END   DEL *** 02/05/97 **************************************
     C*
     C                   CALL      'F0175R01'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      'A'           ##MODE            1
     C                   PARM      @@OCST        ##OCST            1
     C                   PARM      *ZEROS        ##RRN             9 0          RRN FMCHP
     C                   PARM      @@RRN         ##RRN2            9 0          RRN FMCMP
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   MOVEL     'F'           @@RTCD
     C*
     C                   ENDIF
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C                   RETURN
     C*
     C*** BEGIN DEL *** 02/05/98 **************************************
     C*****************************************************
     C**********
     C********** SRUPDT    BEGSR
     C**********
     C**********           MOVE Q1USER    CMUSER
     C**********           MOVE Q1PGM     CMPGM
     C**********           TIME           DSDTTM
     C**********           Z-ADDDSTIME    CMTIME
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     CMDT
     C**********
     C**********           UPDATPCM01
     C**********           ENDSR
     C*** END   DEL *** 02/05/98 **************************************
     C*****************************************************
     C*
     C     SRWRIT        BEGSR
     C*
     C                   CLEAR                   PCH01
     C*
     C                   MOVE      CMCO#         CHCO#
     C                   MOVE      CMSUB#        CHSUB#
     C                   MOVE      CMFND         CHFND
     C                   MOVE      CMFYR         CHFYR
     C                   MOVE      CMCASE        CHCASE
     C                   MOVE      'O'           CHOCST
     C                   MOVEL(P)  XXEXP1        CHEXPL
     C                   MOVE      'A'           CHSTAT
     C*
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CHEFDT
     C*
     C                   MOVEL     Q1USER        CHIUSR
     C                   MOVEL     Q1USER        CHUSER
     C                   MOVEL     Q1PGM         CHIPGM
     C                   MOVEL     Q1PGM         CHPGM
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
