     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS215R2  UPDATE THE ORIG SCAN IE RCD WITH "CM" STATUS     SCN
     H*****************************************************************
     H*                                                               *
     H*    CALLS TO:                                                  *
     H*                                                               *
     H*                                                               *
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
      * INDEX QUEUE FILE   *** SEL DSTS=HD, STAT<>D
     FIMIELK    UF   E           K DISK
     F*

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C     IEKY1         KLIST
     C                   KFLD                    QQDCID
     C*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDCID           12              DOC ID
     C                   PARM                    QQRTCD            1
     C*
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      XXDT              6 0          YY/MM/DD
     C                   MOVE      DSTIME        XXTM              6 0
     C*
     C                   MOVE      XXDT          ##DATE            6
     C*
     C                   MOVEL     *BLANKS       QQRTCD
     C                   Z-ADD     *ZEROS        XXSDT             6 0
     C                   Z-ADD     *ZEROS        XXSTM             6 0
     C*
      *================================================================
     C*
     C     IEKY1         CHAIN     PIE01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     'D'           IESTAT
     C                   MOVEL     'CM'          IEDSTS
      *
     C                   MOVEL     Q1PGM         IELPGM
     C                   MOVEL     Q1USER        IELUSR
     C                   MOVE      XXDT          IELDT
     C                   MOVE      XXTM          IELTIM
     C*
     C*
     C                   UPDATE    PIE01
     C                   ENDIF
      *
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*
