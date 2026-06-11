     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H* *****************************************************************
     H/TITLE FA714R - API TO RETURN CORRECT CODING REL RECORD #
     H* *****************************************************************
     H*  Description:                                                   *
     H*                                                                 *
     H*  THIS API WILL DETERMINE THE RELATIVE RECORD NUMBER OF EITHER   *
     H*  OF THE TWO CORRECT CODING FILES FMBNP AND FMMUP                *
     H*******************************************************************
     H*                                                                 *
     H*  Called From:                                                   *
     H*    F0390R: Work With Provider Bill Detail                       *
     H*                                                                 *
     H*  Calls To:                                                      *
     H*    None                                                         *
     H*                                                                 *
     H*  Created:  01/16/02  C:79  Project:  114MAO                     *
     H*                                                                 *
     H*******************************************************************
     H** REVISIONS:
     H*******************************************************************
     H*  Indicators:                                                    *
     H*                                                                 *
     H*    99 - General File Access                                     *
     H*******************************************************************
     F*
     F* Primary physician detail
     FFDDHP     IF   E             DISK
     F*
     F* Detail Charge File
     FFDCAL5    IF   E           K DISK
     F*
     F* Comprehensive and Component file
     FFMBNL3    IF   E           K DISK
     F                                     INFDS(DBFBN1)
     F*
     F* Mutually Exclusive Code file
     FFMMUL3    IF   E           K DISK
     F                                     INFDS(DBFMU1)
     F*

     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D DBFBN1          DS
     D  RRNBN1               397    400B 0
     D DBFMU1          DS
     D  RRNMU1               397    400B 0
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@RRN             9 0          FDCAP
     C                   PARM                    @@RRN2            9 0          RETURN
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*MDY'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
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
     C*  KEY LISTS
     C*
     C     CCKY          KLIST
     C                   KFLD                    KKBLCD
     C                   KFLD                    KKCCOD
     C                   KFLD                    KKEFDT
     C*
     C     CCKY1         KLIST
     C                   KFLD                    KKBLCD
     C                   KFLD                    KKCCOD
     C*
     C     GBKY          KLIST
     C                   KFLD                    KKBLCD
     C                   KFLD                    KKEFDT
     C*
     C     GBKY1         KLIST
     C                   KFLD                    KKBLCD
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     *LIKE         DEFINE    BNCPCD        KKBLCD
     C     *LIKE         DEFINE    BNCOCD        KKCCOD
     C     *LIKE         DEFINE    CAFDAT        KKEFDT
     C     *LIKE         DEFINE    CACO#         KKCO#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    CAFND         KKFND
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    CAEOB#        KKEOB#
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C*
     C* CHAIN TO PRIMARY FILE TO RETRIEVE KEY FIELDS
     C*
     C     @@RRN         CHAIN     PDH01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     DHCO#         KKCO#
     C                   Z-ADD     DHSUB#        KKSUB#
     C                   Z-ADD     DHFND         KKFND
     C                   Z-ADD     DHFYR         KKFYR
     C                   Z-ADD     DHCASE        KKCASE
     C                   Z-ADD     DHEOB#        KKEOB#
     C                   Z-ADD     DHDSEQ        KKDSEQ
     C*
     C     CAKY          CHAIN     PCA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C* SET KEY FIELDS
     C*
     C                   Z-ADD     CAFDAT        KKEFDT
     C*
     C                   SELECT
     C*
     C* Get RRN for Comprehensive and Component code file
     C*
     C     CACCDF        WHENEQ    '1'
     C*
     C                   MOVEL     CACODE        KKBLCD
     C                   MOVE      CACCOD        KKCCOD
     C*
     C     CCKY          SETGT     PBN01
     C     CCKY1         READPE    PBN01                                  99
     C*
     C  N99              Z-ADD     RRNBN1        @@RRN2
     C*
     C* Get RRN for Comprehensive and Component code file reverse keys
     C*
     C     CACCDF        WHENEQ    '2'
     C*
     C                   MOVEL     CACODE        KKCCOD
     C                   MOVE      CACCOD        KKBLCD
     C*
     C     CCKY          SETGT     PBN01
     C     CCKY1         READPE    PBN01                                  99
     C*
     C  N99              Z-ADD     RRNBN1        @@RRN2
     C*
     C* Get RRN for Mutually exclusive code file.
     C*
     C     CACCDF        WHENEQ    '3'
     C*
     C                   MOVEL     CACODE        KKBLCD
     C                   MOVE      CACCOD        KKCCOD
     C*
     C     CCKY          SETGT     PMU01
     C     CCKY1         READPE    PMU01                                  99
     C*
     C  N99              Z-ADD     RRNMU1        @@RRN2
     C*
     C* Get RRN for Mutually exclusive code file reverse keys
     C*
     C     CACCDF        WHENEQ    '4'
     C*
     C                   MOVEL     CACODE        KKCCOD
     C                   MOVE      CACCOD        KKBLCD
     C*
     C     CCKY          SETGT     PMU01
     C     CCKY1         READPE    PMU01                                  99
     C*
     C  N99              Z-ADD     RRNMU1        @@RRN2
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* CLOSE FILES AND RETURN TO CALLING PROGRAM
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   RETURN
