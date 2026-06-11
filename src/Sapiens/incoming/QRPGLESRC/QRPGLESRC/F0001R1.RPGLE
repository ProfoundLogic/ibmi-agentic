     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H**********************************************************
     H* PROGRAM: F0001R1                                       *
     H* DESCRIPTION: PASS COMPANY AUXILIARY INFO TO BILL ENTRY *
     H**********************************************************
     H* REVISIONS:                                             *
     H**********************************************************
     FFMC1L     IF   E           K DISK
     FSMUPL     IF   E           K DISK

     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* KEY LIST FOR THE USER PARM FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* KEY LIST FOR THE COMP AUX FILE
     C*
     C     C1KEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C* ENTRY PARMS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQRECN            3
     C                   PARM                    QQPRE             3
     C                   PARM                    QQPOST            3
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     C1KEY         CHAIN     PC101                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     C1CSC1        QQRECN
     C                   MOVEL     C1CSC2        QQPRE
     C                   MOVEL     C1CSC3        QQPOST
     C                   ELSE
     C                   MOVE      *BLANKS       QQRECN
     C                   MOVE      *BLANKS       QQPRE
     C                   MOVE      *BLANKS       QQPOST
     C                   END
     C*
     C                   END
     C*
     C* END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
