     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0051R16 - CALL OWNER/OFFICER SEARCH FROM MID TERM ENDORSE
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL CALL THE OWNER/OFFICER       *
     H*                WORK WITH PROGRAM W1057R FROM THE EMPLOYER     *
     H*                MID TERM ENDORSEMENT PROCESS.                  *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0051R1  - EMPLOYER MID TERM ENDORSEMENTS                  *
     H*  CALLS TO:                                                    *
     H*    W1057R   - OWNER/OFFICER WORK WITH                         *
     H*                                                               *
     H*  CREATED: 04/13/99  PROGRAMMER: C:09  PROJECT: CM027          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  04/13/99 C:09 PROJECT: CM027                                 *
     H*                MIDTERM OWNER/OFFICER ENDORSEMENT REWRITE.     *
     H*                                                               *
     H*****************************************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@RRN             9 0          PEM01 RRN
     C                   PARM                    @@RRN1            9 0          PEL01 RRN
     C                   PARM                    @@RRN2            9 0          PMY01 RRN
     C                   PARM                    @@ENDT            8 0
     C*
     C     W1057P        PLIST
     C     @@KC          PARM                    ##KC              1
     C     @@KL          PARM                    ##KL              1
     C                   PARM      *BLANKS       ##CALL            1
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM      *BLANKS       ##ELFL            1
     C                   PARM      'C'           @@QTE             1
     C*
     C                   CALL      'W1057R'      W1057P                 99
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
