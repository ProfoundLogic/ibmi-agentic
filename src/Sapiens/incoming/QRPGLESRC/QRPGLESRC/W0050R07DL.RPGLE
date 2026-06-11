     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0050R07DL - EMPLOYER OWNER/OFFICER INFORMATION DELETION
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DELETE THE OWNER/OFFICER BY  *
     H*                CHANGING THE STATUS FROM 'A' TO 'I'.           *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0050R07 - EMPLOYER OWNER/OFFICER INFORMATION              *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 04/03/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     FWMOOP     UF   E             DISK

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@MODE            1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*================================================================
     C*  Normal Programming Area
     C*================================================================
     C*
     C     @@RRN         CHAIN     POO01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     'I'           OOSTAT
     C*
     C                   MOVEL     Q1USER        OOUSER
     C                   MOVEL     Q1PGM         OOPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OODT
     C*
     C                   MOVEL     DSTIME        OOTIME
     C*
     C                   UPDATE    POO01
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   RETURN
