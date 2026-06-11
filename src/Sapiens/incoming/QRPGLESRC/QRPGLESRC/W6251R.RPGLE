     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W6251R - WCMS 837 TRIGGER API
     H*
     H*****************************************************************
     H* Program description:  This program calls the 837 log API to   *
     H*         create log records for EOMBs which have been paid.    *
     H*         The payment transaction date and claim's jurisdiction *
     H*         state must match the parms and the payment must be    *
     H*         the provider payment.                                 *
     H*                                                               *
     H* Called from:  F6251R                                          *
     H* Calls to:     none                                            *
     H*                                                               *
     H* Created: 08/10/99    C:15     Project: KY010                  *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  06/13/08 C:079 PROJECT:FL061                                 *
     H*                 CHECK POLICY STATE FLAG FOR THE EOB           *
     H*                                                               *
     H*  08/28/07 C:079 SUPPORT:58691                                 *
     H*                 ONLY PROCESS SYSTEM TYPE P AND H              *
     H*                                                               *
     H*  04/23/07 C:188 PROJECT:FL061                                 *
     H*                 USE POLICY STATE FOR COMPARE WHEN NOT BLANK   *
     H*                                                               *
     H*  07/17/06 C:079 PROJ: 135MHP                                  *
     H*                 1. USE THE ADJ. STATE NOT CLAIM JUR. STATE    *
     H*                 2. ONLY SELECT RECORDS WHERE EOTAPE=Y         *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*
     FWDPALH    IF   E           K DISK
     F*** BEG CHG *** 07/17/06 ****************************************
     F*** WMCML1  IF  E           K        DISK
     FFAEOL31   IF   E           K DISK
     F*** END CHG *** 07/17/06 ****************************************
     F*

     D*================================================================
     D*================================================================
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*================================================================
     C*  WDPALH - Payment Detail File
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTRDT
     C*** BEG ADD *** 07/17/06 ****************************************
     C*
     C*  WMCML1 - Claim Master File
     C********
     C********
     C********   CMKY      KLIST
     C********             KFLD           XXCO#
     C********             KFLD           PAFND
     C********             KFLD           PAFYR
     C********             KFLD           PACASE
     C*** END DEL *** 07/17/06 ****************************************
     C*** BEG ADD *** 07/17/06 ****************************************
     C     EOKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAEOB#
     C*** END ADD *** 07/17/06 ****************************************
     C*
     C*  Entry parm list
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3            company
     C                   PARM                    QQPRDT            8            process date
     C                   PARM                    QQJRST            2            jurisdiction state
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  Define and initialize fields
     C*
     C     *LIKE         DEFINE    PACO#         XXCO#
     C     *LIKE         DEFINE    PATRDT        XXTRDT
     C*** BEG CHG *** 07/17/06 ****************************************
     C********** *LIKE     DEFN CMJRST    XXJRST
     C     *LIKE         DEFINE    EOAST         XXJRST
     C*** END CHG *** 07/17/06 ****************************************
     C     *LIKE         DEFINE    PAFEIN        XXFEIN
     C     *LIKE         DEFINE    PAMOD         XXMOD
     C*** BEG ADD *** 04/23/07 ****************************************
     C     *LIKE         DEFINE    EOAST         EDITST
     C*** END ADD *** 04/23/07 ****************************************
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQJRST        XXJRST
     C*
     C                   MOVE      QQPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
     C*
     C*****************************************************************
     C*  Read all payments processed on the parm date.
     C*
     C     PAKY          CHAIN     PPA01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C*  Determine if this is an EOMB provider payment.
     C*
     C     PAEOB#        IFNE      0
     C*
     C*  Only process system or manual payments
     C*
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C*
     C*  Determine if the EOB Adjudication state matches the one
     C*  being processed.
     C*
     C*** BEG CHG *** 07/17/06 ****************************************
     C********** CMKY      CHAINPCM01                99
     C     EOKY          CHAIN     PEO01                              99
     C*** END CHG *** 07/17/06 ****************************************
     C*
     C*** BEG ADD *** 04/23/07 ****************************************
     C     EOPOST        IFNE      *BLANKS
     C*** BEG ADD *** 06/13/08 ****************************************
     C     EOCPST        ANDEQ     'Y'
     C*** END ADD *** 06/13/08 ****************************************
     C                   MOVEL     EOPOST        EDITST
     C                   ELSE
     C                   MOVEL     EOAST         EDITST
     C                   ENDIF
     C*** END ADD *** 04/23/07 ****************************************
     C*
     C     *IN99         IFEQ      *OFF
     C*** BEG CHG *** 07/17/06 ****************************************
     C********** CMJRST    ANDEQXXJRST
    C*** BEG CHG *** 04/23/07 ****************************************
     C*****      EOAST     ANDEQXXJRST
     C     EDITST        ANDEQ     XXJRST
    C*** END CHG *** 04/23/07 ****************************************
     C     EOTAPE        ANDEQ     'Y'
     C*** END CHG *** 07/17/06 ****************************************
     C*** BEG ADD *** 08/28/07 ****************************************
     C     EOSYS         IFEQ      'P'
     C     EOSYS         OREQ      'H'
     C*** END ADD *** 08/28/07 ****************************************
     C                   EXSR      CALL
     C*** BEG ADD *** 08/28/07 ****************************************
     C                   ENDIF
     C*** END ADD *** 08/28/07 ****************************************
     C                   ENDIF
     C*
     C                   ENDIF                                                  PATYPE = 'P'
     C*
     C                   ENDIF                                                  PAEOB# <> 0
     C*
     C*  Get the next payment record.
     C*
     C     PAKY          READE     PPA01                                  99
     C                   ENDDO
     C*
     C*  End the program.
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C*  CALL - This subroutine calls the 837 Log API.
     C*****************************************************************
     C     CALL          BEGSR
     C*
     C*  Retrieve the WCMC SUB# and FND
     C*
     C                   MOVE      PAFND         QQWCF#            3
     C                   CALL      'FA700R'
     C                   PARM      *BLANK        QQRTCD
     C                   PARM                    QQCO#
     C                   PARM                    QQWCF#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C*
     C*  CAll the 837 Log API
     C*
     C                   MOVE      PAFYR         QQFYR
     C                   MOVE      PACASE        QQCASE
     C                   MOVE      PAEOB#        QQEOB#
     C                   MOVE      PAEOBS        QQRCN
     C                   CALL      'F6250R'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQEOB#            9
     C                   PARM                    QQRCN             3
     C                   PARM      *BLANK        QQACTF            1
     C                   PARM      *BLANK        QQRTCD            1
     C*
     C                   ENDSR
     C*****************************************************************
