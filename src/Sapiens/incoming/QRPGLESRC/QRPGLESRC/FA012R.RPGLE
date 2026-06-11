     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*******************************************************************
     H/TITLE FA012R - API to Delete claims
     H*******************************************************************
     H*  Description:                                                   *
     H*                                                                 *
     H*  This API accomodates a deletion request for claims from        *
     H*  the WCMS side for WCMC files.                                  *
     H*                                                                 *
     H*  Called From:                                                   *
     H*                                                                 *
     H*                                                                 *
     H*  Calls To:                                                      *
     H*    None                                                         *
     H*                                                                 *
     H*  Created:  05/09/97  C:56  Project:  87MAZ                      *
     H*                                                                 *
     H*******************************************************************
     H*  Revisions:                                                     *
     H*                                                                 *
     H*                                                                 *
     H*******************************************************************
     H*  Indicators:                                                    *
     H*                                                                 *
     H*    99 & 62 - General File Access                                *
     H*    90 - Delete Criteria                                         *
     H*******************************************************************
      *
     FFMCMLA    UF   E           K DISK
     FFAEOL4    IF   E           K DISK
     FNMARL2    IF   E           K DISK
     FFATHP     O    E           K DISK
     FFDCAL     IF   E           K DISK
     FFMFNL1    IF   E           K DISK
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D  DSYR                  11     12  0

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@SS#
     C                   PARM                    @@ACDT            8 0
     C                   PARM                    @@CO#
     C                   PARM                    @@SUB#
     C                   PARM                    @@FND
     C                   PARM                    @@FYR
     C                   PARM                    @@CASE
     C                   PARM                    @@RTCD
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
     C*** KEY LIST FOR WCMC CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXSS#
     C                   KFLD                    XXACDT
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*** KEY LIST FOR EOMB TRANSACTION FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*** KEY LIST FOR ACCOUNTS RECEIVABLE FILE
     C*
     C     ARKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*** KEY LIST FOR DETAIL CHARGE FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*** KEY LIST FOR FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXWCF#
     C*
     C     *LIKE         DEFINE    CMSS#         @@SS#
     C     *LIKE         DEFINE    CMCO#         @@CO#
     C     *LIKE         DEFINE    CMFND         @@FND
     C     *LIKE         DEFINE    CMFYR         @@FYR
     C     *LIKE         DEFINE    CMCASE        @@CASE
     C     *LIKE         DEFINE    CMSUB#        @@SUB#
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMACDT        XXACDT
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMSUB#        XXSUB#
     C     *LIKE         DEFINE    CMFND         XXWCF#
     C*
     C                   MOVE      *ZEROS        XXCO#
     C                   MOVE      *ZEROS        XXFND
     C                   MOVE      *ZEROS        XXFYR
     C                   MOVE      *ZEROS        XXSUB#
     C                   MOVE      *ZEROS        XXCASE
     C                   MOVE      *BLANKS       XXSS#
     C                   MOVE      *ZEROS        XXACDT
     C*
     C                   MOVEL     @@SS#         XXSS#
     C*
     C                   Z-ADD     @@ACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C*
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@FYR         XXFYR
     C                   MOVE      @@CASE        XXCASE
     C*
     C*** IF CALLED FROM WCMS SIDE, TRANSTLATE FUND AND GET SUB#
     C*
     C     @@SUB#        IFEQ      *ZEROS
     C*
     C                   MOVE      @@FND         XXWCF#
     C     FNKEY         CHAIN     PFN01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVE      FNFND         XXFND
     C                   MOVE      FNSUB#        XXSUB#
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVE      @@FND         XXFND
     C                   MOVE      @@SUB#        XXSUB#
     C                   ENDIF                                                  @@SUB# IFEQ
     C*
     C*** GET CLAIM
     C*
     C                   MOVE      '0'           *IN90
     C     CMKEY         CHAIN     PCM01                              99
     C     *IN99         IFEQ      '1'
     C                   MOVE      'X'           @@RTCD            1
     C                   MOVE      '1'           *IN90
     C     *IN90         CABEQ     '1'           ENDPGM
     C                   ENDIF
     C*
     C*** DO NOT DELETE IF BALANCE IN A/R
     C*
     C     ARKEY         CHAIN     PAR01                              62
     C     *IN62         IFEQ      '0'
     C                   Z-ADD     *ZERO         TOTAMT            9 2
     C     *IN62         DOWEQ     '0'
     C                   ADD       ARAMT         TOTAMT
     C     ARKEY         READE     PAR01                                  62
     C                   ENDDO
     C     TOTAMT        IFNE      *ZERO
     C                   MOVE      'E'           @@RTCD                         NO DELETE
     C                   MOVE      '1'           *IN90
     C     *IN90         CABEQ     '1'           ENDPGM
     C                   ENDIF                                                  TOTAMT IFNE
     C                   ENDIF                                                  *IN62 IFEQ
     C*
     C*** DO NOT DELETE IF EOMB EXISTS FOR CLIAM
     C*
     C     EOKEY         CHAIN     PEO01                              62
     C     *IN62         IFEQ      '0'
     C                   MOVE      'E'           @@RTCD                         NO DELETE
     C                   MOVE      '1'           *IN90
     C     *IN90         CABEQ     '1'           ENDPGM
     C                   ENDIF
     C*
     C*** DO NOT DELETE IF DETAIL CHAREGE EXISTS
     C*
     C     CAKEY         CHAIN     PCA01                              62
     C     *IN62         IFEQ      '0'
     C                   MOVE      'E'           @@RTCD                         NO DELETE
     C                   MOVE      '1'           *IN90
     C     *IN90         CABEQ     '1'           ENDPGM
     C                   ENDIF
     C*
     C*** ALL DELETE CRITERIA HAS BEEN PASSED, ADD RECORD TO CASE
     C*** TRANSFER FILE AND DELETE THE CLAIM
     C*
     C                   Z-ADD     CMCO#         THFCO#
     C                   Z-ADD     CMSUB#        THFSUB
     C                   Z-ADD     CMFND         THFFND
     C                   Z-ADD     CMFYR         THFFYR
     C                   Z-ADD     CMCASE        THFCAS
     C                   MOVE      CMSS#         THFSS#
     C                   Z-ADD     CMACDT        THFACD
     C                   Z-ADD     CMCO#         THTCO#
     C                   Z-ADD     CMSUB#        THTSUB
     C                   Z-ADD     CMFND         THTFND
     C                   Z-ADD     CMFYR         THTFYR
     C                   Z-ADD     CMCASE        THTCAS
     C                   MOVE      CMSS#         THTSS#
     C                   Z-ADD     CMACDT        THTACD
     C                   MOVEL     Q1USER        THUSER
     C                   MOVEL     Q1PGM         THPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         THDT
     C*
     C                   Z-ADD     DSTIME        THTIME
     C                   MOVEL     Q1USER        THIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         THIDT
     C*
     C                   MOVE      'D'           TH1A1
     C                   MOVE      *BLANKS       TH1A2
     C                   MOVE      *BLANKS       TH1A3
     C                   MOVE      *BLANKS       TH1A4
     C                   MOVE      *BLANKS       TH1A5
     C                   MOVE      *BLANKS       TH1A6
     C                   MOVE      *BLANKS       TH1A7
     C                   MOVE      *BLANKS       TH1A8
     C                   MOVE      *BLANKS       TH1A9
     C                   MOVE      *BLANKS       TH1A10
     C                   MOVEL     '*DELETED'    TH40A1
     C                   WRITE     PTH01
     C                   DELETE    PCM01
     C*
     C     ENDPGM        TAG
     C                   RETURN
