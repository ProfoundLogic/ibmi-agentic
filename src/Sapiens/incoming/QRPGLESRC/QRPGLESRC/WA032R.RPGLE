     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE WA032R - API - CROSS CHECK WMEMP LIVE/QUOTE FOR FEIN
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This API will check for the existence of the FEIN passed in*
     H*                                                               *
     H*    CREATED:   08/25/98    C:33       PROJECT: 48WLD           *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWMEML12   IF   E           K DISK

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FEIN            9
     C                   PARM                    @@ERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMFEIN        XXFEIN
     C*
     C*================================================================
     C*          M A I N L I N E
     C*================================================================
     C*
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@FEIN        XXFEIN
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           @@ERR
     C                   ELSE
     C                   MOVE      *OFF          @@ERR
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   MOVE      *ON           *INLR
