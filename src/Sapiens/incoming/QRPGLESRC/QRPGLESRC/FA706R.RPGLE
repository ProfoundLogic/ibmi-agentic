     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*******************************************************************
     H/TITLE FA706R - API TO BUILD DARTS KEY AND OPTIONALLY CALL DARTS
     H*******************************************************************
     H*  Description:                                                   *
     H*
     H*  SEND TO THIS PGM:  IF @@RTCD = BLANK, CALL DARTS
     H*                     IF @@RTCD = 'K' (KEY ONLY), BUILD DARTS KEY
     H*                       AND RETURN WITHOUT CALLING DARTS +
     H*                     WCMS INTERFACE FLAG-WCMS DARTS KEY FLAG +
     H*                     CO#-SUB#-GROUP-YEAR-CASE
     H*                                                                 *
     H*  SEND BACK       :  IF @@RTCD = BLANK, NOTHING
     H*                     IF @@RTCD = 'K' (KEY ONLY):
     H*                           @@RTCD WILL BE 'W' IF USING WCMS KEY
     H*                       OR  @@RTCD WILL BE 'M' IF USING WCMC KEY
     H**************************************************************** - -
     H*                                                                 *
     H*  Called From:                                                   *
     H*    VARIOUS PROGRAMS                                             *
     H*                                                                 *
     H*  Calls To:                                                      *
     H*    SC010R                                                       *
     H*                                                                 *
     H*  Created:   4/28/98  C:62  Project:  MC145                      *
     H*                                                                 *
     H*******************************************************************
     H*  Revisions:                                                     *
     H*                                                                 *
     H*  02/19/99  C:94 PROJ:MC145                                      *
     H*                 ADD PARM TO RETURN THE PROGRAM ID.              *
     H*                                                                 *
     H*   11/30/98  C:94  PROJECT: SUP15271
     H*                   CHANGE PROGRAM TO BLANK FILL DSKEY INSTEAD
     H*                   OF ZERO FILL.
     H*
     H*******************************************************************
     H*  Indicators:                                                    *
     H*                                                                 *
     H*    60 - General File Access                                     *
     H*******************************************************************

     D*
     D*
     D*  DARTS KEY (SC010R) FOR LINK WITH WCMS
     D*
     D                 DS
     D  DSKEY                  1     40
     D  DSINFO                 1     12  0
     D  DSFEIN                 1      9  0
     D  DSMOD                 10     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C     *ENTRY        PLIST
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@WDCK            1
     C                   PARM                    @@FEIN            9
     C                   PARM                    @@MOD             3
     C                   PARM      ##KEY         @@KEY            40
     C*** BEG ADD *** 02/19/99 ****************************************
     C                   PARM                    @@PGM            10
     C*** END ADD *** 02/19/99 ****************************************
     C*
     C     @@RTCD        IFEQ      'K'
     C                   MOVEL(P)  @@RTCD        XXRTCD            1
     C                   MOVE      *BLANK        @@RTCD
     C*** BEG ADD *** 02/19/99 ****************************************
     C     Q1PRMS        IFGT      5
     C                   MOVEL(P)  'F0032R'      @@PGM
     C                   ENDIF
     C*** END ADD *** 02/19/99 ****************************************
     C                   END
     C*
     C*** BEG CHG *** 11/30/98 ****************************************
     C**********           MOVE *ZEROS    DSKEY            STANDALONE
     C                   MOVE      *BLANKS       DSKEY                          STANDALONE
     C*** END CHG *** 11/30/98 ****************************************
     C                   MOVE      *ZEROS        DSINFO
     C                   MOVE      @@FEIN        DSFEIN
     C                   MOVE      @@MOD         DSMOD
     C                   MOVEL     DSKEY         ##KEY            40
     C                   MOVEL(P)  'F0032R'      ##PGMN           40
     C                   MOVEL     'M'           @@RTCD                         WCMC
     C*
     C* CALL DARTS
     C*
     C     XXRTCD        IFNE      'K'
     C                   CALL      'SC010R'
     C                   PARM                    ##KEY
     C                   PARM                    ##PGMN
     C                   END
     C*
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
