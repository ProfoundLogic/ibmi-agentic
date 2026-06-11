     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE FA021R - Check for deleted EOMB.
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*    This program writes a record to the WCMC Claim Transfer    *
     H*    workfile based on the entry parms.                         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W7265R                                                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 11/04/97  C:15  Project: WC729                      *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     FFXCTP     O    E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C**************************************************************************
     C*  Entry parm list
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1            Call code
     C                   PARM                    @@FCO#            3            'From' Company #
     C                   PARM                    @@FFND            3            'From' WCMS Fund
     C                   PARM                    @@FFYR            3            'From' Policy Year
     C                   PARM                    @@FCAS            7            'From' Claim #
     C                   PARM                    @@FSS#            9            'From' SS#
     C                   PARM                    @@FACD            7            'From' Acc dt
     C                   PARM                    @@TCO#            3            'To' Company #
     C                   PARM                    @@TFND            3            'To' WCMS Fund
     C                   PARM                    @@TFYR            3            'To' Policy Year
     C                   PARM                    @@TCAS            7            'To' Claim #
     C                   PARM                    @@TSS#            9            'To' SS#
     C                   PARM                    @@TACD            7            'To' Acc dt
     C                   PARM                    @@RC              1            Return Code
     C*
     C*  Date routine parm list
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*================================================================
     C*  Program Start
     C*================================================================
     C*
     C*  Load the WCMC workfile.
     C*
     C                   MOVE      @@FCO#        CTFCO#                         'From' fields
     C                   MOVE      @@FFND        CTFFND
     C                   MOVE      @@FFYR        CTFFYR
     C                   MOVE      @@FCAS        CTFCAS
     C                   MOVE      @@FSS#        CTFSS#
     C                   MOVE      @@FACD        CTFACD
     C*
     C                   MOVE      @@TCO#        CTTCO#                         'To' fields
     C                   MOVE      @@TFND        CTTFND
     C                   MOVE      @@TFYR        CTTFYR
     C                   MOVE      @@TCAS        CTTCAS
     C                   MOVE      @@TSS#        CTTSS#
     C                   MOVE      @@TACD        CTTACD
     C*
     C                   MOVE      *BLANKS       CTSTAT
     C                   MOVE      Q1USER        CTUSER
     C                   MOVE      Q1PGM         CTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTDT
     C                   Z-ADD     DSTIME        CTTIME
     C                   MOVE      Q1USER        CTIUSR
     C                   Z-ADD     CTDT          CTIDT
     C                   WRITE     PCT01                                           FXCTP
     C*
     C*  End of program
     C*
     C                   RETURN
