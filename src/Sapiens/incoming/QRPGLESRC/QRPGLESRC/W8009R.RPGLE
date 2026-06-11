     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     Hdftactgrp(*no) option(*nodebugio:*srcstmt)
     H*****************************************************************
     H/TITLE W8009R - CREATE BILLING JOBQs FOR EACH GROUP
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM CREATE BILLING JOBQs FOR EACH GROUP              *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W8002C                                                      *
     H* CALLS TO:                                                     *
     H*   W0010C02                                                    *
     H*                                                               *
     H* PROGRAMMER: C:255                                             *
     H* CREATED: 04/30/10               PROJECT: WC1866               *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*****************************************************************
      *
      *Worker Compensation Fund Master File
     FWMFNL     IF   E           K DISK
      *
     DQCA              s             70    Dim(3) CTDATA PERRCD(1)
     DLENGTH           s             15  5
      *==================================================================
      *  PLIST
      *==================================================================
     C     *ENTRY        PLIST
     C                   PARM                    CO#               3
      *
     C                   MOVE      CO#           @@CO#             3 0
      *
      *Creating Data Area
     C                   MOVEL(P)  QCA(1)        CMDST1           70
     c                   EVAL      LENGTH = %LEN(CMDST1)
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDST1
     C                   PARM                    LENGTH
     C*
     C     @@CO#         Setll     PFN01
     C     @@CO#         READE     PFN01                                  60
     C                   Dow       *in60 = *off
     C*Call Program for each active Group
     C                   IF        FNSTAT = *BLANKS
     C                   Move      FNFND         XXFND
     C*
     C                   Call      'W0010C02'
     C                   Parm                    CO#
     C                   Parm                    XXFND             3
     C                   Parm                    @@ERRFLG          1
     C*
     C                   ENDIF
     C*
     C     @@CO#         READE     PFN01                                  60
     C                   EndDo
      *Deleting Data Area
     C                   MOVEL(P)  QCA(2)        CMDST1
     c                   EVAL      LENGTH = %LEN(CMDST1)
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDST1
     C                   PARM                    LENGTH
     C
      *
      *  End of Processing
      *
     C     END           TAG
     C                   MOVE      *ON           *INLR
** QCA
CRTDTAARA DTAARA(QTEMP/TRACKSEQNO) TYPE(*DEC) LEN(5 0) VALUE(1000)
DLTDTAARA DTAARA(QTEMP/TRACKSEQNO)
