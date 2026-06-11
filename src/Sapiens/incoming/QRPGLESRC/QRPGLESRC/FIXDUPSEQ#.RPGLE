     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE CHKDUPSEQ# - CHECK PAYMENT DETAIL FOR DUPLICATE SEQ NUMBERS
     H*****************************************************************
     FWDPAL     UF   E           K DISK

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C*
     C     *LIKE         DEFINE    PACO#         XXCO#
     C     *LIKE         DEFINE    PAFND         XXFND
     C     *LIKE         DEFINE    PACO#         SVCO#
     C     *LIKE         DEFINE    PAFND         SVFND
     C     *LIKE         DEFINE    PAFYR         SVFYR
     C     *LIKE         DEFINE    PACASE        SVCASE
     C     *LIKE         DEFINE    PASEQ#        SVSEQ#
     C     *LIKE         DEFINE    PASEQ#        XXSEQ#
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     *ZERO         XXCO#
     C                   Z-ADD     *ZERO         XXFND
     C                   Z-ADD     *ZERO         XXSEQ#
     C*
     C                   CLEAR                   SVCO#
     C                   CLEAR                   SVFND
     C                   CLEAR                   SVFYR
     C                   CLEAR                   SVCASE
     C                   CLEAR                   SVSEQ#
     C*
     C     Q1PRMS        IFGE      1
     C                   MOVE      QQCO#         XXCO#
     C                   ENDIF                                                  Q1PRMS>=1
     C*
     C     Q1PRMS        IFGE      2
     C                   MOVE      QQFND         XXFND
     C                   ENDIF                                                  Q1PRMS>=2
     C*
     C                   SELECT
     C     XXFND         WHENNE    *ZERO
     C     PAKY          CHAIN     PPA01                              60
     C     XXCO#         WHENNE    *ZERO
     C     XXCO#         CHAIN     PPA01                              60
     C                   OTHER
     C                   READ      PPA01                                  60
     C                   ENDSL
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     PACO#         IFEQ      SVCO#
     C     PAFND         ANDEQ     SVFND
     C     PAFYR         ANDEQ     SVFYR
     C     PACASE        ANDEQ     SVCASE
     C     PASEQ#        ANDEQ     SVSEQ#
     C*
     C                   ADD       1             XXSEQ#
     C     XXSEQ#        ADD       9000          PASEQ#
     C                   MOVEL(P)  'FIXSEQ#'     PAPGM
     C*
     C                   UPDATE    PPA01
     C*
     C                   ELSE
     C*
     C     PACO#         IFNE      SVCO#
     C     PAFND         ORNE      SVFND
     C     PAFYR         ORNE      SVFYR
     C     PACASE        ORNE      SVCASE
     C                   Z-ADD     *ZERO         XXSEQ#
     C                   ENDIF                                                  PACO#<>SVCO#
     C*
     C                   Z-ADD     PACO#         SVCO#
     C                   Z-ADD     PAFND         SVFND
     C                   Z-ADD     PAFYR         SVFYR
     C                   Z-ADD     PACASE        SVCASE
     C                   Z-ADD     PASEQ#        SVSEQ#
     C*
     C                   ENDIF                                                  PACO#=SVCO#
     C*
     C                   SELECT
     C     XXFND         WHENNE    *ZEROS
     C     PAKY          READE     PPA01                                  60
     C     XXCO#         WHENNE    *ZEROS
     C     XXCO#         READE     PPA01                                  60
     C                   OTHER
     C                   READ      PPA01                                  60
     C                   ENDSL
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   MOVE      *ON           *INLR
