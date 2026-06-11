     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
      /TITLE FA719R - API TO BUILD DARTS KEY AND OPTIONALLY CALL DARTS
      * *****************************************************************
      *  Description:                                                   *
      *
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SEND TO THIS PGM:  IF @@RTCD = BLANK, CALL DARTS
      *                     IF @@RTCD = 'K' (KEY ONLY), BUILD DARTS KEY
      *                       AND RETURN WITHOUT CALLING DARTS +
      *                                                                 *
      *  SEND BACK       :  IF @@RTCD = BLANK, NOTHING
      *                     IF @@RTCD = 'K' (KEY ONLY):
      *                           @@RTCD WILL BE 'W' IF USING WCMS KEY
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
     H** REVISIONS:
     H*  04/25/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Add missing key logic
     H*                 2. Call SC010R only if key present
     H*
     H*  02/19/99  C:94 PROJ:MC145                                      *
     H*                 ADD PARM TO RETURN THE PROGRAM ID.              *
     H*                                                                 *
      * *****************************************************************
      *  Indicators:                                                    *
      *                                                                 *
      *    60 - General File Access                                     *
      * *****************************************************************
      *
      *
      *  DARTS KEY (SC010R) FOR LINK WITH WCMS
      *

     D                 DS
     D  DSKEY                  1     40
     D  DSINFO                 1     20  0
     D  DSCO#                  1      3  0
     D  DSFND                  4      6  0
     D  DSEMP                  7     15  0
     D  DSDIV                 16     20  0
      *
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     *ENTRY        PLIST
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@EMP             9
     C                   PARM                    @@DIV             5
     C                   PARM      ##KEY         @@KEY            40
     C                   PARM                    @@PGM            10
      *
     C     @@RTCD        IFEQ      'K'
     C                   MOVEL(P)  @@RTCD        XXRTCD            1
     C                   MOVE      *BLANK        @@RTCD
     C                   END
     C*
     C                   MOVE      @@CO#         DSCO#
     C*
     C                   MOVE      @@FND         DSFND
     C*
      *
     C                   MOVE      @@EMP         DSEMP
     C                   MOVE      @@DIV         DSDIV
     C                   MOVEL     DSKEY         ##KEY            40
     C                   MOVEL(P)  'W0050R'      ##PGMN           40
     C     Q1PRMS        IFGT      9
     C                   MOVEL(P)  ##PGMN        @@PGM
     C                   ENDIF
     C                   MOVEL     'W'           @@RTCD                         WCMS
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CALL DARTS
      *
     C     XXRTCD        IFNE      'K'
     C     ##KEY         ANDNE     *BLANKS
     C                   CALL      'SC010R'
     C                   PARM                    ##KEY
     C                   PARM                    ##PGMN
     C                   END
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
     C                   MOVE      *ON           *INLR
     C                   RETURN
