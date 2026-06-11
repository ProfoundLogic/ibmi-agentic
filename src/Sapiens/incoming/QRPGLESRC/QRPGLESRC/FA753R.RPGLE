     Hcopyright('Fiserv, Inc., 2008')                                                              *
     H/title FA753R - API for Discount Algorithm Tracking                                          *
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
     FNXBDP     IF A E             DISK
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
     D @@CODE          S              3
     D @@TYPE          S              2
     D @@RCDT          S              1
     D @@EFDT          S              7  0
     D @@CPT4          S              5
     D @@PMOD          S              2
     D @@10A1          S             10
     D @@10A2          S             10
     D @@10A3          S             10
     D @@10A4          S             10
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
     C                   PARM                    @@CODE
     C                   PARM                    @@TYPE
     C                   PARM                    @@RCDT
     C                   PARM                    @@EFDT
     C                   PARM                    @@CPT4
     C                   PARM                    @@PMOD
     C                   PARM                    @@10A1
     C                   PARM                    @@10A2
     C                   PARM                    @@10A3
     C                   PARM                    @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
      *============================================================================================*
      * MAINLINE
      *============================================================================================*
      *
     C                   TIME                    DSDTTM
      *
     C                   EVAL      BDCODE = @@CODE
     C                   EVAL      BDTYPE =  @@TYPE
     C                   EVAL      BDRCDT =  @@RCDT
     C                   EVAL      BDEFDT = @@EFDT
     C                   EVAL      BDCPT4 =  @@CPT4
     C                   EVAL      BDPMOD =  @@PMOD
     C                   EVAL      BD10A1 =  @@10A1
     C                   EVAL      BD10A2 =  @@10A2
     C                   EVAL      BD10A3 =  @@10A3
     C                   EVAL      BD10A4 =  @@10A4
     C                   EVAL      BDFLD  =  @@FLD
     C                   EVAL      BDFVAL =  @@FVAL
     C                   EVAL      BDTVAL =  @@TVAL
     C                   EVAL      SRDTI  = DSDATE
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BDDT
     C                   EVAL      BDTIME =  DSTIME
     C                   EVAL      BDUSER =  Q1USER
     C                   EVAL      BDPGM  =  Q1PGM
     C                   WRITE     PBD01
     C                   EVAL      *INLR  =  '1'
