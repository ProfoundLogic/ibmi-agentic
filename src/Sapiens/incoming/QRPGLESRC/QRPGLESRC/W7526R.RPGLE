     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7526R - PURGE CERTIFICATE HOLDERS RECORDS
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  10/29/97  C:33  PROJECT: WC577                               *
     H*                  Recompiled due to fields being added to WMCEP*
     H*****************************************************************
     F*
     F*  CERTIFICATE HOLDERS MASTER FILE
     F*
     FWMCEL     UF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR THE CERTIFICATE HOLDERS FILE
     C*
     C     CEKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C*
     C                   TIME                    DSDTTM
     C*
     C*  DELETE THE RECORDS
     C*
     C     CEKEY         CHAIN     PCE01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     CEKEY         DELETE    PCE01                              60
     C*
     C                   END
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
