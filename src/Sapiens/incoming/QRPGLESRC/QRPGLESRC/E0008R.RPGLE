     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE
     FEMUPL     IF   E           K DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C     *ENTRY        PLIST
     C                   PARM                    PRT#A             2
     C                   PARM                    STCN              1
     C                   MOVE      PRT#A         PRT#              2 0
     C*
     C* * * GET THE USER PARAMETER FILE TO RETRIEVE THE DEFAULT OUTQ
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C     STCN          IFEQ      'S'
     C                   CALL      'E0008C'
     C                   PARM                    PRT#
     C                   PARM                    UPOUTQ
     C                   ELSE
     C                   CALL      'E0008C1'
     C                   PARM                    PRT#
     C*
     C                   END
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
