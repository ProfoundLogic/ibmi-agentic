     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE F8282R2 - Get network fee credits
     H*****************************************************************
     H* Created on 09/02/2006 by c:79 for Project 139MCV              *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     FSMUPL     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Call the override program.
     C*
     C     Q1USER        CHAIN     PUP01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL     'QPRINT'      UPOUTQ
     C                   ENDIF
     C*
     C                   CALL      'F8282C2'
     C                   PARM      UPOUTQ        ##OUTQ           10
     C*
     C                   SETON                                        LR
