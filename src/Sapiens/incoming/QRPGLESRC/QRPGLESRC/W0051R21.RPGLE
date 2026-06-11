     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H/TITLE W1057R21 - CALL EMPLOYER REVISED BILL FROM MID TERM ENDORSE
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL CALL THE EMPLOYER REVISED    *
     H*                BILL PROCESS FROM THE EMPLOYER MID TERM        *
     H*                ENDORSEMENT PROCESS.                           *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0051R1  - EMPLOYER MID TERM ENDORSEMENTS                  *
     H*  CALLS TO:                                                    *
     H*    W0090R   - EMPLOYER REVISED BILL                           *
     H*                                                               *
     H*  CREATED: 02/01/98  PROGRAMMER: C:57  PROJECT: CM027          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 09/16/10 C:180 SUPPORT 74221
     H*                Check for record in use to prevent a system
     H*                error when 2 or more users are trying to
     H*                do a revised bill endorsement to the same
     H*                policy number.
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
     C     W0090P        PLIST
     C                   PARM      *BLANKS       ##MODE            1
     C     @@KC          PARM                    ##KC              1
     C     @@KL          PARM                    ##KL              1
     C                   PARM      @@RRN1        ##RRN             9 0
     C***** BEGIN ADD ****** 09/16/10 *********************************
     C                   PARM      @@RRN         ##RRN2            9 0
     C                   PARM                    ##RTCD            1
     C***** END   ADD ****** 09/16/10 *********************************
     C*
     C***** BEGIN ADD ****** 09/16/10 *********************************
     C     RIUKY         KLIST
     C                   KFLD                    @@FNM
     C                   KFLD                    @@RRN
     C***** END   ADD ****** 09/16/10 *********************************
      *
     C                   CALL      'W0090R'      W0090P                 99
      *
     C***** BEGIN ADD ****** 09/16/10 *********************************
     C     ##RTCD        IFEQ      *OFF
     C                   MOVE      @@RRN         ##RRN3            9
     C                   CALL      'W0105R'
     C                   PARM      'B'           @@MODE            1
     C                   PARM                    ##RRN3
     C                   PARM      'WMEMP'       @@FNM            10
     C                   ENDIF
     C***** END   ADD ****** 09/16/10 *********************************
     C                   MOVE      *ON           *INLR
     C                   RETURN
