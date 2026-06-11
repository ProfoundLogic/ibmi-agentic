     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H* PROGRAM: SA007R                                              *
     H* DESCRIPTION: JOBQ FLAG                                       *
     H****************************************************************
     H* CREATED 06/23/2005 C:62 PROJECT WC1307
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                              *
     H****************************************************************
     FSMCOL     IF   E           K DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@BFLG            1
     C*
     C     *LIKE         DEFINE    COCO#         KKCO#
     C*
     C                   MOVE      @@CO#         KKCO#
     C*
     C                   CLEAR                   @@BFLG
     C     KKCO#         CHAIN     PCO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  COBFLG        @@BFLG
     C                   ENDIF
     C*
     C                   RETURN
