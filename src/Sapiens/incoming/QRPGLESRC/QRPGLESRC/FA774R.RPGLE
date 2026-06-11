     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H* This program will return tax amount from EOMB detail transacti*
     H* on file                                                       *
     H*****************************************************************
     H*  Created by:                                                  *
     H*  12/13/11  C:191 Project: LA025                               *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FFDDAL2    IF   E           K DISK
     FFDCAP     IF   E             DISK
     F*
     D KKRRN           S              9  0
     D QQRRN           S              9  0
     D QQTAX           S                   LIKE(DATAX)
     D*
     C     DAKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C                   KFLD                    CARCN
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQRRN
     C                   PARM                    QQTAX
     C*
     C                   EVAL      *INLR = *ON
     C                   EVAL      KKRRN = QQRRN
     C                   EVAL      QQTAX = 0
     C*
     C     KKRRN         CHAIN     PCA01
     C                   IF        %FOUND
     C     DAKY          CHAIN     PDA01
     C                   IF        %FOUND
     C                   EVAL      QQTAX = DATAX
     C                   ENDIF
     C                   ENDIF
     C                   RETURN
