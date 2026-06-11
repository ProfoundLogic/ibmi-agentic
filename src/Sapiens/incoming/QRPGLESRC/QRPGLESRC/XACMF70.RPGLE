     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* XACMF70  CHECK MISMATCH ANALYSIS PGM                                A
     H*          PLACE JUST BEFORE CALL TO F0086R
     H*
     H*  Called From:  F0070R
     H*
     H*
     H*  Calls To:     NONE
     H*
     H*  Created:  10/03/97  C:53  Project:  MC158
     H*
     H*****************************************************************
     H*  REVISIONS:
     H*
     H* 00/00/00 C:00 Req/Proj:
     H*
     H*****************************************************************
     H*  INDICATOR USAGE:
     H*    INDICATOR            FUNCTION DESCRIPTION
     H*
     H*    60 - 61    GENERAL USAGE (CHAIN)
     H*
     H*****************************************************************
     FFXCKL     UF A E           K DISK

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@DCS#            5 0
     C*
     C                   PARM                    @@PGM            10
     C                   PARM                    @@USER           10
     C                   PARM                    @@DATE            7 0
     C                   PARM                    @@TIME            6 0
     C*
     C                   PARM                    @@SYS             1
     C                   PARM                    @@CTLC            9 2
     C                   PARM                    @@CTLD            9 2
     C                   PARM                    @@CTLO            9 2
     C                   PARM                    @@CTLN            9 2
     C******************** PARM           @@CTLA  92
     C                   PARM                    @@CTLP            9 2
     C*
     C     KXCKL1        KLIST
     C                   KFLD                    KXCO#             3 0
     C                   KFLD                    KXSUB#            5 0
     C                   KFLD                    KXFND             3 0
     C                   KFLD                    KXFYR             3 0
     C                   KFLD                    KXCASE            7 0
     C                   KFLD                    KXDCS#            5 0
     C*
     C                   Z-ADD     @@CO#         KXCO#
     C                   Z-ADD     @@SUB#        KXSUB#
     C                   Z-ADD     @@FND         KXFND
     C                   Z-ADD     @@FYR         KXFYR
     C                   Z-ADD     @@CASE        KXCASE
     C                   Z-ADD     @@DCS#        KXDCS#
     C*
     C     KXCKL1        CHAIN     PCK01                              60
     C*
     C                   MOVE      @@SYS         CKSYS
     C                   Z-ADD     @@CTLC        CKCTLC
     C                   Z-ADD     @@CTLD        CKCTLD
     C                   Z-ADD     @@CTLO        CKCTLO
     C                   Z-ADD     @@CTLN        CKCTLN
     C********             Z-ADD          CKCTLA
     C                   Z-ADD     @@CTLP        CKCTLP
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   Z-ADD     @@CO#         CKCO#
     C                   Z-ADD     @@SUB#        CKSUB#
     C                   Z-ADD     @@FND         CKFND
     C                   Z-ADD     @@FYR         CKFYR
     C                   Z-ADD     @@CASE        CKCASE
     C                   Z-ADD     @@DCS#        CKDCS#
     C*
     C                   MOVEL     @@USER        CKIUSR
     C                   MOVEL     @@PGM         CKIPGM
     C                   Z-ADD     @@DATE        CKIDT
     C                   Z-ADD     @@TIME        CKITIM
     C*
     C                   MOVEL     @@USER        CKUSR1
     C                   MOVEL     @@PGM         CKPGM1
     C                   Z-ADD     @@DATE        CKDT1
     C                   Z-ADD     @@TIME        CKTM1
     C*
     C                   WRITE     PCK01
     C                   ELSE
     C*
     C                   MOVEL     @@USER        CKUSR1
     C                   MOVEL     @@PGM         CKPGM1
     C                   Z-ADD     @@DATE        CKDT1
     C                   Z-ADD     @@TIME        CKTM1
     C*
     C                   MOVEL     @@USER        CKUSER
     C                   MOVEL     @@PGM         CKPGM
     C                   Z-ADD     @@DATE        CKDT
     C                   Z-ADD     @@TIME        CKTIME
     C*
     C                   UPDATE    PCK01
     C                   ENDIF
     C*
     C                   MOVE      *ON           *INLR
