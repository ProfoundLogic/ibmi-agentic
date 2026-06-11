     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE E0004R - GET DEFAULT USER OUTQ FROM USER PARM FILE
     FEMUPL     IF   E           K DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQOUTQ           10
     C*
     C* * * GET THE USER PARAMETER FILE TO RETRIEVE THE DEFAULT OUTQ
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVEL     UPOUTQ        QQOUTQ
     C*
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
