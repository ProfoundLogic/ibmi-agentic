     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE E0003R - DISPLAY OUT QUEUE IN USER PARAMETER FILE
     FEMUPL     IF   E           K DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* * * GET THE USER PARAMETER FILE TO RETRIEVE THE DEFAULT OUTQ
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   CALL      'E0003C'
     C                   PARM                    UPOUTQ
     C*
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
