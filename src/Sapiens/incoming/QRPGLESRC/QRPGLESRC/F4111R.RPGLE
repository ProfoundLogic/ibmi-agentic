     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller)
     H/define CONVERT
     H/TITLE F4111R - COPY SINGLE REQUEST TO FILE IN QTEMP AND DELETE
     H/TITLE          REQUEST FROM FILE FDD1P IN THE LIVE ENVIRONMENT
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*     THIS PROGRAM IS USED BY THE PRINT DEPT LETTER JOB TO      *
     H*     COPY A SINGLE REQUEST FROM THE LIVE REQUEST FILE TO THE   *
     H*     FILE SET UP IN QTEMP BY THE PRINT JOB (F4111C).  ONCE     *
     H*     THE REQUEST HAS BEEN COPIED INTO FDD1P IN QTEMP IT IS     *
     H*     REMOVED FROM THE LIVE REQUEST FILE.                       *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     F4111C                                                    *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*     NONE                                                      *
     H*                                                               *
     H*  PROGRAMMER: C:62                                             *
     H*  CREATED:    05/20/98               PROJECT: MC177            *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                      GENERAL USAGE                            *
     H*   99   GENERAL READS, CHAINS                                  *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*
     H*   FDD1P, FDD1P1 AND FDD1P2 (PD101, PD102 AND PD103)           *
     H*      NO KEY                                - THIS FILE        *
     H*      IS THE VERSION OF FDD1P THAT EXISTS IN QTEMP.            *
     H*                                                               *
     H*   FDD1L, FDD1L1 AND FDD1L2 (PD104, PD105 AND PD106)           *
     H*      CO#/SUB#/FND/REQ#/DPCD/FLDR/DCID      - THIS FILE        *
     H*      IS THE VERSION OF FDD1P THAT EXISTS IN THE LIVE          *
     H*      ENVIRONMENT.                                             *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     FFDD1P     O    E             DISK
     FFDD1P1    O    E             DISK
     F                                     RENAME(PD101:PD102)
     FFDD1P2    O    E             DISK
     F                                     RENAME(PD101:PD103)
     FFDD1L     UF   E           K DISK
     F                                     RENAME(PD101:PD104)
     FFDD1L1    UF   E           K DISK
     F                                     RENAME(PD101:PD105)
     FFDD1L2    UF   E           K DISK
     F                                     RENAME(PD101:PD106)
     FFMCMP     IF   E             DISK

     C*
     C     D1KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKREQ#
     C*
     C     *LIKE         DEFINE    D1CO#         KKCO#
     C     *LIKE         DEFINE    D1SUB#        KKSUB#
     C     *LIKE         DEFINE    D1FND         KKFND
     C     *LIKE         DEFINE    D1FYR         KKFYR
     C     *LIKE         DEFINE    D1CASE        KKCASE
     C     *LIKE         DEFINE    D1REQ#        KKREQ#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN            15 5
     C                   PARM                    @@REQ#           15 5
     C*
     C                   Z-ADD     @@RRN         ##RRN             9 0
     C                   Z-ADD     @@REQ#        ##REQ#            5 0
     C*
     C     ##RRN         CHAIN     PCM01                              99
     C*
     C                   MOVE      CMCO#         KKCO#
     C                   MOVE      CMSUB#        KKSUB#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMFYR         KKFYR
     C                   MOVE      CMCASE        KKCASE
     C                   MOVE      ##REQ#        KKREQ#
     C*
     C     D1KY          CHAIN     PD104                              99
     C*
     C     *IN99         DOWEQ     '0'
     C*
     C                   WRITE     PD101
     C                   DELETE    PD104
     C     D1KY          READE     PD104                                  99
     C*
     C                   ENDDO
     C*
     C     D1KY          CHAIN     PD105                              99
     C*
     C     *IN99         DOWEQ     '0'
     C*
     C                   WRITE     PD102
     C                   DELETE    PD105
     C     D1KY          READE     PD105                                  99
     C*
     C                   ENDDO
     C*
     C     D1KY          CHAIN     PD106                              99
     C*
     C     *IN99         DOWEQ     '0'
     C*
     C                   WRITE     PD103
     C                   DELETE    PD106
     C     D1KY          READE     PD106                                  99
     C*
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
