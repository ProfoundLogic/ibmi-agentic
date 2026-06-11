     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE ISDL09R - DELETE SMMDP RECORDS
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *   Marks Additional Remarks records 'Inactive' and deletes Index        *
     *   records, for a given Key/Seq#.                                       *
     *                                                                        *
     * CREATED   : 01/13/2000                                                 *
     * PROGRAMMER: C:113                                                      *
     * PROJECT   : DR084                                                      *
     *========================================================================*
     FSMMDL     UF   E           K DISK
     FSMARL     UF   E           K DISK

      ** External data structure
     D psds          ESDS                  ExtName(szq1p)

      ** Entry parms
     D @@Key           S             40a
     D @@Seq#          S              5a
     D @@RetnCode      S              1a

      ** Date check/conversion parms
     D srDateIn        S              8  0
     D srDateOut       S             10
     D srError         S              1
     D srFromFmt       S              8
     D srToFmt         S              8

      ** Working variables
     D inactive        S              1a   Inz('I')
     D xxKey           S             40a
     D xxSeq#          S                   Like(MDSEQ#)
     D xxDate          S              8s 0 Inz
     D xxTime          S              6s 0 Inz
     *========================================================================*
     * PLIST Definitions
     *========================================================================*
      ** Entry parameters
     C     *entry        PList
     C                   Parm                    @@Key
     C                   Parm                    @@Seq#
     C                   Parm                    @@RetnCode

      ** Date check/conversion parameters
     C     dtcymd        PList
     C                   Parm                    srDateIn
     C                   Parm      *blanks       srFromFmt
     C                   Parm      '*CYMD'       srToFmt
     C                   Parm                    srDateOut
     C                   Parm                    srError
     *========================================================================*
     * KLIST definitions
     *========================================================================*
     ** Generic Key
     C     GNkey         KList
     C                   KFld                    xxKey
     C                   KFld                    xxSeq#

     *========================================================================*
     * Main
     *========================================================================*
      ** Mark the Additional Remarks records 'inactive'.
     C     GNkey         Chain     PAR01                              60
     C                   DoW       *in60 = *off
     C                   Eval      ARSTAT = inactive
     C                   Eval      ARUSER = q1user
     C                   Eval      ARPGM  = q1pgm
     C                   Eval      ARTIME = xxTime
     C                   Eval      srDateIn = xxDate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srDateOut     ARDT

     C                   Update    PAR01

     C     GNkey         ReadE     PAR01                                  60
     C                   EndDo

      ** Delete the Index Entries records.
     C     GNkey         Chain     PMD01                              60
     C                   DoW       *in60 = *off
     C                   Delete    PMD01
     C     GNkey         ReadE     PMD01                                  60
     C                   EndDo

     C                   Eval      *inlr = *on
     *========================================================================*
     * *inzsr - Initialization subroutine
     *========================================================================*
     C     *inzsr        BEGSR

     C                   Move      @@Key         xxKey
     C                   Move      @@Seq#        xxSeq#
     C                   Eval      @@RetnCode = *blank
     c                   Eval      xxDate = *date
     C                   Time                    xxTime

     C                   ENDSR                                                  *inzsr
