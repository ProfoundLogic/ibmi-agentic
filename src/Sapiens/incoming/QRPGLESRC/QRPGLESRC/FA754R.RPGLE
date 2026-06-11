     Hcopyright('Fiserv, Inc., 2008')                                                              *
     H/title FA753R - API for Network Contract Tracking                                            *
     H option(*nodebugio: *srcstmt)                                                                *
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H debug(*yes)                                                                                 *
     H*============================================================================================*
     H*  PROGRAMMER: C 183                                                                         *
     H*  CREATED:    02/21/08                                                                      *
     H*  PROJECT#:   114MPX                                                                        *
      *============================================================================================*
      *  REVISIONS:                                                                                *
      *                                                                                            *
      *============================================================================================*
     FFXS4P     IF A E             DISK
      *============================================================================================*
      * DATA STRUCTURES
      *============================================================================================*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D                 DS
     D DSDTTM                        14  0
     D  DSTIME                        6  0 OVERLAY(DSDTTM)
     D  DSDATE                        8  0 OVERLAY(DSDTTM:7)
      *============================================================================================*
      * PROGRAM FIELDS
      *============================================================================================*
     D SRDTI           S              8  0
     D SRDTO           S             10
     D SRERR           S              1
     D SRFFMT          S              8
     D SRTFMT          S              8
     D @@FEIN          S              9  0
     D @@MOD           S              3  0
     D @@NWCD          S              5
     D @@BGDT          S              7  0
     D @@FLD           S             10
     D @@FVAL          S             80
     D @@TVAL          S             80
      *============================================================================================*
      * PLIST'S
      *============================================================================================*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@FEIN
     C                   PARM                    @@MOD
     C                   PARM                    @@NWCD
     C                   PARM                    @@BGDT
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
      *============================================================================================*
      * MAINLINE
      *============================================================================================*
      *
     C                   TIME                    DSDTTM
      *
     C                   EVAL      S4FEIN = @@FEIN
     C                   EVAL      S4MOD = @@MOD
     C                   EVAL      S4NWCD =  @@NWCD
     C                   EVAL      S4BGDT = @@BGDT
     C                   EVAL      S4FLD  =  @@FLD
     C                   EVAL      S4FVAL =  @@FVAL
     C                   EVAL      S4TVAL =  @@TVAL
     C                   EVAL      SRDTI  = DSDATE
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         S4DT
     C                   EVAL      S4TIME =  DSTIME
     C                   EVAL      S4USER =  Q1USER
     C                   EVAL      S4PGM  =  Q1PGM
     C                   WRITE     PS401
     C                   EVAL      *INLR  =  '1'
