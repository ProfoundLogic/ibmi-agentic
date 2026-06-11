     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE E0004R1 - GET DEFAULT USER COMPANY # FROM USER PARM FILE
     FEMUPL     IF   E           K DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C*
     C* * * GET THE USER PARAMETER FILE TO RETRIEVE THE DEFAULT OUTQ
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      UPCO#         QQCO#
     C*
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
