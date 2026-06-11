     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*========================================================================
     H*========================================================================
     H/TITLE SC951R1- Darts Extraction program - POLICY Processing
     H*========================================================================
     H* DESCRIPTION:
     H*  This program will process through the POLICIES
     H*  and copy the objects to the directory.
     H*
     H*========================================================================
     H* Created on 12/29/2009 by c:02 for Project 135int
     H*========================================================================
     H* REVISIONS:
     H*
     H* 03/15/18  C:62  Project: 114IWR (Ticket 48148)
     H*                 Speed up process
     H*
     H* 02/18/16  C:53   Project: 114IWR
     H*                  Code for Opt to Delete DARTS/attachments
     H*
     H*========================================================================
     F* File Definitions
     F*========================================================================
     F****
     FSXDEL     UF   E           K DISK
     F                                     RENAME(PDE01:PATCH)
     F*

     D*
     D** MAKE DEFAULT IFS DIRECTORY HERE
     D***
     D*
     D                 DS
     D  @@KEY                  1     40
     D  @@CO#                  1      3
     D  @@FND                  4      6
     D  @@EMP                  7     15
     D  @@DIV                 16     20
     D  @@FILL                21     40

     D holdCMkey       S                   Like(CMkey) Inz(' ')
     D ##Key           S                   Like(CMkey)
     D ##Seq#          S                   Like(CMseq#)
     D ##Call          s              1

     I*

     C     *ENTRY        PLIST
     C                   PARM                    QQGRP             3
     C                   PARM                    QQFCYD            7
     C                   PARM                    QQTCYD            7
     C                   PARM                    QQJDAT            7
     C                   PARM                    QQPPTH           22
     C                   PARM                    QQJNBR            6
     C                   PARM                    QQDLT             1
     ***
     C     DARTS         KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     ****
     C*
     C                   READ      PATCH                                  60
     ****
     C     *IN60         DOWEQ     *OFF
     C     CMSTAT        ANDNE     'P'
     C     CMSTAT        ANDNE     'X'
     *****
     C                   MOVE      CMSEQ#        QQSEQ#            5
     C                   Eval      holdCMkey = CMkey
     ****
     C                   CALL      'SC952R1'
     C                   PARM                    CMKEY
     C                   PARM                    QQSEQ#
     C                   PARM      *BLANKS       QQPTH           255
     C                   PARM      *BLANK        @@RC              1
     C                   PARM                    QQFCYD            7
     C                   PARM                    QQTCYD            7
     C                   PARM                    QQJDAT            7
     C                   PARM                    QQPPTH           22
     C                   PARM                    QQJNBR            6
     C                   PARM                    QQDLT
     ***
     **
     C                   MOVEL     @@RC          CMSTAT                          IF ERR, THEN UNCN I
     C                   MOVEL     QQPTH         CMUNCN                             ERRDESC
     C**
     C                   UPDATE    PATCH
     C*
     C                   READ      PATCH                                  60
     C                   ENDDO
     C     QQDLT         IFEQ      'Y'
     C                   CALL      'SC954R2'
     C                   PARM                    QQGRP
     C                   PARM                    QQFCYD
     C                   PARM                    QQTCYD
     C                   PARM      'W0050R'      QQTYPE           10
     C                   ENDIF
     C*
     C                   MOVE      *ON           *INLR
