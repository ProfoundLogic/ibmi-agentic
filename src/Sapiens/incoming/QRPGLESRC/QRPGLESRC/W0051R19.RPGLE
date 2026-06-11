     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W1057R19 - CALL EMPLOYER MAINT (ADD MODE) FROM MID TERM ENDORSE
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL CALL THE EMPLOYER MAINT PGM  *
     H*                (W0050R02) IN ADD MODE FROM THE EMPLOYER       *
     H*                MID TERM ENDORSEMENT PROCESS.                  *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0051R1  - EMPLOYER MID TERM ENDORSEMENTS                  *
     H*  CALLS TO:                                                    *
     H*    W0050R02 - EMPLOYER MASTER MAINT                           *
     H*                                                               *
     H*  CREATED: 02/01/98  PROGRAMMER: C:57  PROJECT: CM027          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  07/10/98  C:33  SEQ#: WA091/12598                            *
     H*                  CALL EMPLOYER MASTER W/LIVE FLAG.            *
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
     C     W0502P        PLIST
     C     @@KC          PARM                    ##KC              1
     C     @@KL          PARM                    ##KL              1
     C                   PARM      'V'           ##MODE            1
     C                   PARM      @@RRN         ##RRN             9 0
     C***  BEGIN CHG  ***  07/10/98  *********************************
     C**********           PARM *BLANKS   ##QTE   1
     C                   PARM      'W'           ##QTE             1
     C***  END   CHG  ***  07/10/98  *********************************
     C*
     C                   CALL      'W0050R02'    W0502P                 99
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
