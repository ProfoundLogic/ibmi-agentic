     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* XACMF86  CHECK MISMATCH ANALYSIS PGM                                A
     H*          F7301R USAGE - PLACE JUST AFTER WRITE/UPDATE TO FAE1P
     H*
     H*  Called From:  F7301R
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
     FFXCKL1    UF A E           K DISK

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@FUNC            1
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@EOB#            9 0
     C*
     C                   PARM                    @@PGM            10
     C                   PARM                    @@USER           10
     C                   PARM                    @@DATE            7 0
     C                   PARM                    @@TIME            6 0
     C*
     C                   PARM                    @@CAMT            9 2
     C                   PARM                    @@CHKW            1
     C*
     C*
     C     KXCKL1        KLIST
     C                   KFLD                    KXCO#             3 0
     C                   KFLD                    KXSUB#            5 0
     C                   KFLD                    KXFND             3 0
     C                   KFLD                    KXFYR             3 0
     C                   KFLD                    KXCASE            7 0
     C                   KFLD                    KXEOB#            9 0
     C*
     C                   Z-ADD     @@CO#         KXCO#
     C                   Z-ADD     @@SUB#        KXSUB#
     C                   Z-ADD     @@FND         KXFND
     C                   Z-ADD     @@FYR         KXFYR
     C                   Z-ADD     @@CASE        KXCASE
     C                   Z-ADD     @@EOB#        KXEOB#
     C*
     C     KXCKL1        CHAIN     PCK01                              60
     C*
     C                   SELECT
     C*
     C     @@FUNC        WHENEQ    'P'
     C                   MOVEL     @@USER        CKUSR6
     C                   MOVEL     @@PGM         CKPGM6
     C                   Z-ADD     @@DATE        CKDT6
     C                   Z-ADD     @@TIME        CKTM6
     C*
     C                   Z-ADD     @@CAMT        CKAMT6
     C                   MOVE      @@CHKW        CKFL6
     C*
     C     @@FUNC        WHENEQ    'B'
     C                   MOVEL     @@USER        CKUSR7
     C                   MOVEL     @@PGM         CKPGM7
     C                   Z-ADD     @@DATE        CKDT7
     C                   Z-ADD     @@TIME        CKTM7
     C*
     C                   Z-ADD     @@CAMT        CKAMT7
     C                   MOVE      @@CHKW        CKFL7
     C*
     C     @@FUNC        WHENEQ    'N'
     C                   MOVEL     @@USER        CKUSR8
     C                   MOVEL     @@PGM         CKPGM8
     C                   Z-ADD     @@DATE        CKDT8
     C                   Z-ADD     @@TIME        CKTM8
     C*
     C                   Z-ADD     @@CAMT        CKAMT8
     C                   MOVE      @@CHKW        CKFL8
     C*
     C                   ENDSL
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   Z-ADD     @@CO#         CKCO#
     C                   Z-ADD     @@SUB#        CKSUB#
     C                   Z-ADD     @@FND         CKFND
     C                   Z-ADD     @@FYR         CKFYR
     C                   Z-ADD     @@CASE        CKCASE
     C                   Z-ADD     @@EOB#        CKEOB#
     C*
     C                   MOVEL     @@USER        CKIUSR
     C                   MOVEL     @@PGM         CKIPGM
     C                   Z-ADD     @@DATE        CKIDT
     C                   Z-ADD     @@TIME        CKITIM
     C*
     C                   WRITE     PCK01
     C                   ELSE
     C*
     C                   MOVEL     @@USER        CKUSER
     C                   MOVEL     @@PGM         CKPGM
     C                   Z-ADD     @@DATE        CKDT
     C                   Z-ADD     @@TIME        CKTIME
     C*
     C                   UPDATE    PCK01
     C                   ENDIF
     C*
     C                   RETURN
     C*******************  MOVE *ON       *INLR
