     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - FA010R - WRITE/UPDATE WCMC EMPLOYER INFO BASED ON WCMS   DATA
     H*                FEE SHEDULE VERSION
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA010R                                            *
     H*    CREATED: 02/17/97  C:53                                    *
     H*    DESCRIPTION: WRITE/UPDATE WCMC EMPLOYER  NFO BASED ON      *
     H*                 WCMS DATA                                     *
     H*                                                               *
     H*  Program Return Codes:                                        *
     H*    '1' - WCMC Fund Master NOT Found (FMFNL1)                  *
     H*          No WCMC Employer record Updated or Written.          *
     H*                                                               *
     H*    '*' - Request code: Caller request to close files          *
     H*          No WCMC Employer record Updated or Written.          *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  11/29/00 C:58 PROJECT: MCXXX
     H*            ADD PARM RTCD = '*' -- To close files at end of
     H*                                    batch processing
     H*
     H*  10/16/98 C:53 PROJECT: MC145                                 *
     H*                REMOVE EMRGN# UPDATE - FIELD IS NO LONGER USED *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       20      PARMS PASED WERE ALL FILLED                     *
     H*    60 - 61    GENERAL USAGE (CHAIN)                           *
     H*                                                               *
     H*****************************************************************
     F*
     FFMEML     UF A E           K DISK
     FFMFNL1    IF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMSUB#        XXSUB#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE KEY LIST
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     *ENTRY        PLIST
     C*
     C                   PARM                    QQRTCD            1
     C*
     C*** THE FOLLOWING 5 FIELDS ARE REQUIRED TO HAVE AN ENTRY.
     C*** QQSUB# SHOULD BE: BLANK OR ZEROS IF COMING FROM WCMS
     C***                  A) BLANK OR ZEROS IF COMING FROM WCMS
     C***                  B) VALID SUB# IF COMING FROM WCMC
     C*
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND#            3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C*
     C                   PARM                    QQNAME           40
     C                   PARM                    QQMAD1           40
     C                   PARM                    QQMAD2           40
     C                   PARM                    QQMCTY           25
     C                   PARM                    QQMST             2
     C                   PARM                    QQMZIP            9
     C                   PARM                    QQPAD1           40
     C                   PARM                    QQPAD2           40
     C                   PARM                    QQPCTY           25
     C                   PARM                    QQPST             2
     C                   PARM                    QQPZIP            9
     C                   PARM                    QQAREA            3
     C                   PARM                    QQPHN#            7
     C                   PARM                    QQRGN#            3
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQVIP             1
     C                   PARM                    QQMPFL            1
     C                   PARM                    QQSTRP            1
     C                   PARM                    QQDBA            40
     C                   PARM                    QQNOB            40
     C                   PARM                    QQMCO            10
     C                   PARM                    QQMCO#           20
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
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND#        XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C*
     C**** BEG ADD *** 11/29/00 ***************************************
     C*** IF PARM RTCD = '*' END PGM AND RETURN
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF
     C**** END ADD *** 11/29/00 ***************************************
     C*
     C*** TRANSLATE SUB # IF COMMING FROM WCMS
     C*
     C     QQSUB#        IFEQ      *BLANKS
     C     QQSUB#        OREQ      *ZEROS
     C     FNKY          CHAIN     PFN01                              61
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     FNSUB#        XXSUB#
     C                   Z-ADD     FNFND         XXFND
     C                   ELSE
     C                   MOVE      '1'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C                   END
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C*** LOAD THE FIELDS FOR WRITE / UPDATE
     C*
     C                   MOVE      QQCO#         EMCO#
     C                   MOVE      XXSUB#        EMSUB#
     C                   MOVE      XXFND         EMFND
     C                   MOVE      QQEMP#        EMEMP#
     C                   MOVE      QQDIV         EMDIV
     C                   MOVE      QQNAME        EMNAME
     C                   MOVE      QQMAD1        EMMAD1
     C                   MOVE      QQMAD2        EMMAD2
     C                   MOVE      QQMCTY        EMMCTY
     C                   MOVE      QQMST         EMMST
     C                   MOVE      QQMZIP        EMMZIP
     C                   MOVE      QQPAD1        EMPAD1
     C                   MOVE      QQPAD2        EMPAD2
     C                   MOVE      QQPCTY        EMPCTY
     C                   MOVE      QQPST         EMPST
     C                   MOVE      QQPZIP        EMPZIP
     C                   MOVE      QQAREA        EMAREA
     C                   MOVE      QQPHN#        EMPHN#
     C                   MOVE      QQFEIN        EMFEIN
     C                   MOVE      QQVIP         EMVIP
     C                   MOVE      QQMPFL        EMMPFL
     C                   MOVE      QQSTRP        EMSTRP
     C****************     MOVE QQDBA     EMDBA
     C                   MOVE      QQNOB         EMNOB
     C***
     C*** BEG DEL *** 10/16/98 *****************************
     C***        EMRGN#    IFEQ *ZEROS
     C***        QQRGN#    ANDNE*BLANKS
     C***                  MOVE QQRGN#    EMRGN#
     C***                  ENDIF
     C*** END DEL *** 10/16/98 *****************************
     C*
     C                   MOVEL     QQMCO         EMMCO
     C                   MOVEL     QQMCO#        EMMCO#
     C*
     C                   MOVE      *BLANK        EMSTAT
     C                   MOVE      Q1USER        EMUSER
     C                   MOVE      Q1PGM         EMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EMTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C*
     C*************************************************
     C*
     C*** WRITE/UPDATE EMPLOYER INFO
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      Q1USER        EMIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMIDT
     C*
     C                   Z-ADD     DSTIME        EMITIM
     C                   WRITE     PEM01
     C                   ELSE
     C                   UPDATE    PEM01
     C                   ENDIF
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   RETURN
     C*
