     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W1057R15 - CALL ENTITY/LOCATION SEARCH FROM MID TERM ENDORSE
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL CALL THE ENTITY/LOCATION     *
     H*                WORK WITH PROGRAM W0076R FROM THE EMPLOYER     *
     H*                MID TERM ENDORSEMENT PROCESS.                  *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0051R1  - EMPLOYER MID TERM ENDORSEMENTS                  *
     H*  CALLS TO:                                                    *
     H*    W0076R   - ENTITY/LOCATION WORK WITH                       *
     H*                                                               *
     H*  CREATED: 04/28/99  PROGRAMMER: C:09  PROJECT: CM027          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  04/28/99 C:09 PROJECT: CM027                                 *
     H*                MIDTERM ENTITY/LOCATION ENDORSEMENT REWRITE.   *
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
     C     W0076P        PLIST
     C     @@KC          PARM                    ##KC              1
     C     @@KL          PARM                    ##KL              1
     C                   PARM      *BLANKS       ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
     C                   PARM      'X'           @@QTE             1
     C*
     C                   CALL      'W0076R'      W0076P                 99
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
