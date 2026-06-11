     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS811R1  GET NEW UNIQUE DOCID FOR COPY PROCESSING
     H*****************************************************************
     H*                                                               *
     H*    CALLS TO:                                                 **
     H*                                                              **
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* 60  GENERAL CHAIN INDICATOR                                   *
     H* 61  GENERAL CHAIN INDICATOR                                   *
     H*****************************************************************
     H** FILES:                                                      **
     H*****************************************************************
     F*
     F*

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSMIN                  3      4  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FC6             C                   CONST('                    -
     D                                        ')
     D FXDCID         UDS
     D  #XDCID                 1      7
     D FXFOLD         UDS
     D  #XFOLD                 1     63

     C*
     C*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           63
     C                   PARM                    QQRTCD            1
     C*
     C                   UNLOCK    FXDCID
     C                   UNLOCK    FXFOLD
     C*
     C                   MOVEL     *BLANKS       QQDCID
     C                   MOVEL     *BLANKS       QQFOLD
     C                   MOVEL     *BLANKS       QQRTCD
     C*
     C                   TIME                    DSDTTM
     C*
      *================================================================
     C*
     C     *DTAARA       DEFINE                  FXDCID
     C     *DTAARA       DEFINE                  FXFOLD
     C*
     C*
     C**** DETERMINE DOC NAME AND FOLDER NAME
     C                   IN        FXFOLD
     C*
     C                   MOVEL     #XFOLD        QQFOLD
     C                   MOVEL     QQFOLD        ##FOLD           80
     C***
     C*
     C     *LOCK         IN        FXDCID
     C*
     C                   MOVE      #XDCID        ##DOC             7 0
     C                   ADD       1             ##DOC
     C                   MOVE      ##DOC         #XDCID
     C                   OUT       FXDCID
     C     'A'           CAT       #XDCID        ##8A              8
     C     ##8A          CAT       '.TIF'        QQDCID
     C*
     C*
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
