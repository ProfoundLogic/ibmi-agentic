     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SA703R - API TO BUILD DARTS KEY AND OPTIONALLY CALL DARTS
     H*******************************************************************
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
      *  Created:   1/02/98  C:47  Project:  87MCB                      *
      *                                                                 *
      * *****************************************************************
     H** REVISIONS:
     H*  04/25/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Add missing key logic
     H*                 2. Call SC010R only if key present
     H*
      *  02/01/99  C:62 PROJ:87MHL                                      *
      *                 ADD PARM TO RETURN THE PROGRAM ID.              *
      *                                                                 *
      *  04/30/98  47                                                   *
      *            ADDED CONDITION TO LOAD FIELDS IF WCMS IS NOT        *
      *            INSTALLED.                                           *
      *                                                                 *
      *  04/01/98  47  87MEA                                            *
      *            MADE THE CALL TO DARTS OPTIONAL SO THAT ONLY THE     *
      *            DARTS KEY CAN BE CREATED, IF DESIRED.                *
      *            ALSO, @@RTCD WILL RETURN A 'W' FOR WCMS              *
      *            OR  , @@RTCD WILL RETURN A 'M' FOR WCMC.             *
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
     D  DSINFO                 1     16  0
     D  DSCO#                  1      3  0
     D  DSFND                  4      6  0
     D  DSFYR                  7      9  0
     D  DSCASE                10     16  0
      *
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     *ENTRY        PLIST
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM      ##KEY         @@KEY            40
     C                   PARM                    @@PGM            10
      *
     C     @@RTCD        IFEQ      'K'
     C                   MOVEL(P)  @@RTCD        XXRTCD            1
     C                   MOVE      *BLANK        @@RTCD
     C                   END
      *
     C                   MOVE      *BLANKS       DSKEY
     C                   MOVE      *ZEROS        DSINFO
     C                   MOVE      @@CO#         DSCO#
     C                   MOVE      @@FND         DSFND
     C                   MOVE      @@FYR         DSFYR
     C                   MOVE      @@CASE        DSCASE
     C                   MOVEL     DSKEY         ##KEY            40
     C                   MOVEL(P)  'W0070R'      ##PGMN           40
     C                   MOVEL     'W'           @@RTCD                         WCMS
     C*
     C* PASS BACK PROGRAM ID IF THE PARM IS USED.
     C*
     C     Q1PRMS        IFGT      9
     C*
     C                   MOVEL(P)  ##PGMN        @@PGM
     C*
     C                   ENDIF
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
