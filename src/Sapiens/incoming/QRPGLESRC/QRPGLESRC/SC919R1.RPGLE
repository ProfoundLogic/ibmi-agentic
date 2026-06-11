     *====================================================================*
     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     *====================================================================*
     H/TITLE SC919R1- IDARTS MULTIPLE OBJECT FILE COPY/TRANSFER
     *====================================================================*
     * Program Description:                                               *
     *   This will copy/transfer the Multiple Object File records for the *
     *   DARTS Single Remark Copy/Transfer process.                       *
     *                                                                    *
     * Called From: SC919R - DARTS SINGLE REMARK COPY/TRANSFER            *
     *                                                                    *
     * Calls To:    NONE                                                  *
     *                                                                    *
     * Created:     11/25/06                                              *
     * Programmer:  C:002                                                 *
     * Project:     DR214                                                 *
     *====================================================================*
     * Revisions:                                                         *
     *                                                                    *
     *  XX/XX/XXXX C:XXX PROJECT: XXXXX                                   *
     *             XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
     *                                                                    *
     * ------------------------------------------------------------------ *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.                *
     * ------------------------------------------------------------------ *
     *====================================================================*
     Hdatfmt(*usa)

      ** IDARTS Multiple-Object file
     FSMMDL     UF A E           K DISK

      *====================================================================*
      * Data Structures                                                    *
      *====================================================================*
      ** Date and Time data structure
     D                 DS
     D dsDateTime                    12  0
     D  dsTime                        6  0 Overlay(dsDateTime)
     D  dsDate                        6  0 Overlay(dsDateTime:7)

      ** External program data structure
     D PSDS          ESDS                  ExtName(szq1p)

      *====================================================================*
      * Stand-alone fields                                                 *
      *====================================================================*
      ** Entry Parameters
     D @@FromKey       S             40
     D @@FromSeq#      S              5
     D @@Mode          S              1
     D @@InUs          S             10
     D @@ToKey         S             40
     D @@ToSeq#        S              5

      ** Date check/conversion parameters
     D qqDateIn        S              8  0
     D qqFromFmt       S              8
     D qqToFmt         S              8
     D qqDateOut       S             10
     D qqError         S              1

      ** Working variables for keys
     D xxFromKey       S             40
     D xxFromSeq#      S              5  0
     D xxToKey         S             40
     D xxToSeq#        S              5  0

      ** Copy or transfer flag
     D xxCopy          S              1

      *====================================================================*
      * PLIST definitions                                                  *
      *====================================================================*
     ** Entry parameters
     C     *entry        PList
     C                   Parm                    @@FromKey
     C                   Parm                    @@FromSeq#
     C                   Parm                    @@ToKey
     C                   Parm                    @@ToSeq#
     C                   Parm                    @@Mode
     C                   Parm                    @@INUS
    
     ** Date check/conversion parameters
     C     dtCYMD        PList
     C                   Parm                    qqDateIn
     C                   Parm      '*MDY'        qqFromFmt
     C                   Parm      '*CYMD'       qqToFmt
     C                   Parm                    qqDateOut
     C                   Parm                    qqError

      *====================================================================*
      * KLIST definitions                                                  *
      *====================================================================*
      ** From key
     C     FromKey       KList
     C                   KFld                    xxFromKey
     C                   KFld                    xxFromSeq#

      ** To key
     C     ToKey         KList
     C                   KFld                    xxToKey
     C                   KFld                    xxToSeq#

      *====================================================================*
      * Main                                                               *
      *====================================================================*
     C     FromKey       Chain     pmd01                              60
     C                   Dow       *in60 = *off

      ** Load TO key and sequence.
     C                   Eval      MDKEY = xxToKey
     C                   Eval      MDSEQ# = xxToSeq#

      ** Load initial fields
     C                   Eval      MDIPGM = Q1PGM

     C                   Eval      qqDateIn = dsDate
     C                   CALL      'SRCVTDAT'    dtCYMD
     C                   MoveL     qqDateOut     MDIDT

     C                   Eval      MDITIM = dsTime
     C                   Eval      MDIUSR = @@INUS

      ** Zero maint fields
     C                   Eval      MDLPGM = *blanks
     C                   Eval      MDLDT = *zeros
     C                   Eval      MDLTIM = *zeros
     C                   Eval      MDLUSR = *blanks

     C                   If        xxCopy = *on
      ** Write record
     C                   WRITE     pmd01
     C                   Else
      ** Update record
     C                   UPDATE    pmd01
     C                   EndIf

     C     FromKey       ReadE     pmd01                                  60
     C                   EndDo

     C                   Move      *on           *inlr

      *====================================================================*
      * *inzsr - Program Initialization Subroutine                         *
      *====================================================================*
     C     *inzsr        BEGSR

     C                   time                    dsDateTime

     C                   If        @@Mode = 'C'
     C                   Eval      xxCopy = *on
     C                   Else
     C                   Eval      xxCopy = *off
     C                   EndIf

     C                   Eval      xxFromKey = @@FromKey
     C                   Move      @@fromSeq#    xxFromSeq#

     C                   Eval      xxToKey = @@ToKey
     C                   Move      @@ToSeq#      xxToSeq#

     C                   ENDSR                                                  *inzsr
