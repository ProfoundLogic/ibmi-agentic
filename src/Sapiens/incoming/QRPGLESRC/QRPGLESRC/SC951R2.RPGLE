     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*========================================================================
     H*========================================================================
     H/TITLE SC951R2- Darts Extraction program - AGENCY Processing
     H*========================================================================
     H* DESCRIPTION:
     H*  This program will process through the AGENCIES
     H*  and copy the objects to the directory.
     H*
     H*========================================================================
     H* Created on 09/20/2017 by c:62 for Project 114WCS
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
     FWMAAL     IF   E           K DISK
     F                                     RENAME(PAA01:PAA00)
     F*
     D*
     D** MAKE DEFAULT IFS DIRECTORY HERE
     D***
     D*
     D                 DS
     D  @@KEY                  1     40
     D  @@FEIN                 1      9
     D  @@MOD                 10     12
     D  @@FILL                13     40

     D HoldCMkey       S                   Like(CMkey) Inz(' ')
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
     ****
     C     AAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKFND
     ***
     C     DARTS         KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     C
     C                   READ      PATCH                                  60
     C
     C     *IN60         DOWEQ     *OFF
     C     CMSTAT        ANDNE     'P'
     C     CMSTAT        ANDNE     'X'
     C
     C* Skip if Agent not assigned to group
     C                   MOVEL     CMKEY         @@KEY
     C                   Z-ADD     777           KKCO#             3 0
     C                   MOVE      @@FEIN        KKFEIN            9 0
     C                   MOVE      @@MOD         KKMOD             3 0
     C                   MOVE      QQGRP         KKFND             3 0
     C
     C     AAKY          Chain     PAA00
     C                   If        %Found
     C
     C                   MOVE      CMSEQ#        QQSEQ#            5
     C                   Eval      holdCMkey = CMkey
     C
     C                   CALL      'SC952R2'
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
     C
     C                   MOVEL     @@RC          CMSTAT                          IF ERR, THEN UNCN I
     C                   MOVEL     QQPTH         CMUNCN                             ERRDESC
     C                   EndIf
     C**
     C                   UPDATE    PATCH
     C
     C                   Read      PATCH                                  60
     C                   EndDo
     C
     C     QQDLT         IfEQ      'Y'
     C                   CALL      'SC954R2'
     C                   PARM                    QQGRP
     C                   PARM                    QQFCYD
     C                   PARM                    QQTCYD
     C                   PARM      'W0040R'      QQTYPE           10
     C                   ENDIF
     C*
     C                   MOVE      *ON           *INLR
