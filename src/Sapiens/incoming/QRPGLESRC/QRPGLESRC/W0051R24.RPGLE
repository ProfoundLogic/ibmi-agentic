     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0051R24 - CALL WAIVER OF SUBRO MAINT FROM MID TERM ENDORSE
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL CALL THE WIAVER OF SUBRO     *
     H*                WORK WITH PROGRAM W0810R FROM THE EMPLOYER     *
     H*                MID TERM ENDORSEMENT PROCESS.                  *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0051R1  - EMPLOYER MID TERM ENDORSEMENTS                  *
     H*  CALLS TO:                                                    *
     H*    W0815R   - WAIVER OF SUBRO WORK WITH                       *
     H*                                                               *
     H*  CREATED: 07/01/98  PROGRAMMER: C:57  PROJECT: 95WBM          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  07/27/99 C:113 PROJECT: CM045                                *
     H*                 Changed to call W0815R as part of waiver      *
     H*                 rewrite.                                      *
     H* ------------------------------------------------------------- *
     H*  NOTE: Please add comments to the top of Revisions.           *
     H* ------------------------------------------------------------- *
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
     C*** BEGIN CHG *** 07/27/99 ***********************************************
     C********** W0810P    PLIST
     C     W0815P        PLIST
     C*** BEGIN CHG *** 07/27/99 ***********************************************
     C     @@KC          PARM                    ##KC              1
     C     @@KL          PARM                    ##KL              1
     C                   PARM      *BLANKS       ##CALL            1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      @@RRN1        ##RRN2            9 0          RRN WDELP
     C*** BEGIN DEL *** 07/27/99 ***********************************************
     C**********           PARM 1         ##VIEW  10
     C**********           PARM 'S'       ##TYPE  1        SUBRO PROCESSING
     C*** END   DEL *** 07/27/99 ***********************************************
     C*
     C*** BEGIN CHG *** 07/27/99 ***********************************************
     C**********           CALL 'W0810R'  W0810P       99
     C                   CALL      'W0815R'      W0815P                 99
     C*** BEGIN CHG *** 07/27/99 ***********************************************
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
