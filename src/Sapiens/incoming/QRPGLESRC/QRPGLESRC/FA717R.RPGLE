     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      * *****************************************************************
      /TITLE FA703R - API TO BUILD DARTS KEY AND OPTIONALLY CALL DARTS
      * *****************************************************************
      *  Description:                                                   *
      *
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SEND TO THIS PGM:  IF @@RTCD = BLANK, CALL DARTS
      *                     IF @@RTCD = 'K' (KEY ONLY), BUILD DARTS KEY
      *                       AND RETURN WITHOUT CALLING DARTS +
      *                     CO#-SUB#
      *                                                                 *
      *  SEND BACK       :  IF @@RTCD = BLANK, NOTHING
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *                                                                 *
      *  Called From:                                                   *
      *    VARIOUS PROGRAMS                                             *
      *                                                                 *
      *  Calls To:                                                      *
      *    SC010R                                                       *
      *                                                                 *
      *  Created:  12/01/98  C:94  Project:  MC145                      *
      *                                                                 *
      * *****************************************************************
      *  Revisions:                                                     *
      *                                                                 *
      *  02/19/99  C:94 PROJ:MC145                                      *
      *                 ADD PARM TO RETURN THE PROGRAM ID.              *
      *                                                                 *
      * *****************************************************************
      *  Indicators:                                                    *
      *                                                                 *
      * *****************************************************************
      *
      *
      *  DARTS KEY (SC010R)
      *

     D                 DS
     D  DSKEY                  1     40
     D  DSINFO                 1      8  0
     D  DSCO#                  1      3  0
     D  DSSUB#                 4      8  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C*** BEG ADD *** 02/19/99 ****************************************
     C                   PARM                    @@KEY            40
     C                   PARM                    @@PGM            10
     C                   PARM                    @@RTCD            1
     C*** END ADD *** 02/19/99 ****************************************
     C*
     C                   MOVE      *BLANKS       DSKEY
     C                   MOVE      *ZEROS        DSINFO
     C                   MOVE      @@CO#         DSCO#
     C                   MOVE      @@SUB#        DSSUB#
     C*
     C*** BEG ADD *** 02/19/99 ****************************************
     C     Q1PRMS        IFGT      2
     C     @@RTCD        IFEQ      'K'
     C                   MOVE      *BLANK        @@RTCD
     C                   MOVEL(P)  DSKEY         @@KEY
     C                   MOVEL(P)  'F0055R'      @@PGM
     C*
     C                   ELSE
     C*
     C                   MOVEL     DSKEY         ##KEY            40
     C                   MOVEL(P)  'F0055R'      ##PGMN           40
     C*
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C* CALL DARTS
     C*
     C                   CALL      'SC010R'
     C                   PARM                    ##KEY
     C                   PARM                    ##PGMN
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*** END ADD *** 02/19/99 ****************************************
     C*
     C                   MOVEL     DSKEY         ##KEY            40
     C                   MOVEL(P)  'F0055R'      ##PGMN           40
     C*
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C* CALL DARTS
     C*
     C                   CALL      'SC010R'
     C                   PARM                    ##KEY
     C                   PARM                    ##PGMN
     C*** BEG ADD *** 02/19/99 ***************************************
     C                   ENDIF
     C*** END ADD *** 02/19/99 ***************************************
     C*
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
