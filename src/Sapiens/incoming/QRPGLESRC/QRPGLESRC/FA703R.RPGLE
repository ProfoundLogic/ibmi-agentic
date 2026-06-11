     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE FA703R - API TO BUILD DARTS KEY AND OPTIONALLY CALL DARTS
     H*******************************************************************
      *  Description:                                                   *
      *
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SEND TO THIS PGM:  IF @@RTCD = BLANK, CALL DARTS
      *                     IF @@RTCD = 'K' (KEY ONLY), BUILD DARTS KEY
      *                       AND RETURN WITHOUT CALLING DARTS +
      *                     WCMS INTERFACE FLAG-WCMS DARTS KEY FLAG +
      *                     CO#-SUB#-GROUP-YEAR-CASE
      *                                                                 *
      *  SEND BACK       :  IF @@RTCD = BLANK, NOTHING
      *                     IF @@RTCD = 'K' (KEY ONLY):
      *                           @@RTCD WILL BE 'W' IF USING WCMS KEY
      *                       OR  @@RTCD WILL BE 'M' IF USING WCMC KEY
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
      *  DARTS KEY (SC010R) FOR OUTSIDE ACCOUNT
      *
     D                 DS
     D  DRKY                   1     40
     D  DRINFO                 1     21  0
     D  DRCO#                  1      3  0
     D  DRSUB#                 4      8  0
     D  DRFND                  9     11  0
     D  DRFYR                 12     14  0
     D  DRCASE                15     21  0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     *ENTRY        PLIST
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@WCIN            1
     C                   PARM                    @@WDCK            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM      ##KEY         @@KEY            40
     C*** BEGIN ADD *** 02/01/99 **************************************
     C                   PARM                    @@PGM            10
     C*** END   ADD *** 02/01/99 **************************************
      *
      *** BEG ADD *** 04/01/98 **************************************
     C     @@RTCD        IFEQ      'K'
     C                   MOVEL(P)  @@RTCD        XXRTCD            1
     C                   MOVE      *BLANK        @@RTCD
     C                   END
      *** END ADD *** 04/01/98 **************************************
     C*
     C     @@WCIN        IFEQ      'Y'                                          WC INTERFACE
     C*
     C                   CALL      'FA701R'
     C                   PARM      @@RTCD        ##RTCD            1
     C                   PARM      @@CO#         ##CO#             3
     C                   PARM      @@SUB#        ##SUB#            5
     C                   PARM      @@FND         ##FND             3
     C                   PARM      *BLANKS       ##OUTS            1
     C*
     C*** VALID RECORD ' ' = YES
     C*
     C     ##RTCD        IFEQ      ' '
     C*
     C     ##OUTS        IFNE      'Y'                                          NOT AN OUT ACCT
      *
     C     @@WDCK        IFEQ      'Y'                                          WC DARTS KEY
     C                   MOVE      *BLANKS       DSKEY
     C                   MOVE      *ZEROS        DSINFO
     C                   MOVE      @@CO#         DSCO#
     C*
     C*** API FOR SUB OR GROUP XLATE
     C*
     C                   CALL      'FA700R'
     C                   PARM      @@RTCD        ##RTCD
     C                   PARM      @@CO#         ##CO#
     C                   PARM      *BLANKS       ##WCF#            3
     C                   PARM      @@SUB#        ##SUB#
     C                   PARM      @@FND         ##FND
     C*
     C*** VALID RECORD ' ' = YES
     C*
     C     ##RTCD        IFEQ      ' '
     C                   MOVE      ##WCF#        DSFND
     C                   ELSE
     C                   Z-ADD     *ZEROS        DSFND
     C                   END
      *
     C                   MOVE      @@FYR         DSFYR
     C                   MOVE      @@CASE        DSCASE
     C                   MOVEL     DSKEY         ##KEY            40
     C                   MOVEL(P)  'W0070R'      ##PGMN           40
      *** BEG ADD *** 04/01/98 **************************************
     C                   MOVEL     'W'           @@RTCD                         WCMS
      *** END ADD *** 04/01/98 **************************************
     C*** BEGIN ADD *** 04/25/00 **************************************
     C                   ELSE                                                   WDCK = 'Y'
     C                   MOVE      *BLANK        DRKY
     C                   MOVE      *ZEROS        DRINFO
     C                   MOVE      @@CO#         DRCO#
     C                   MOVE      @@SUB#        DRSUB#
     C                   MOVE      @@FND         DRFND
     C                   MOVE      @@FYR         DRFYR
     C                   MOVE      @@CASE        DRCASE
     C                   MOVEL     DRKY          ##KEY
     C                   MOVEL(P)  'F0270R'      ##PGMN
     C                   MOVEL     'M'           @@RTCD                         WCMC
     C*** END ADD *** 04/25/00 ****************************************
     C                   END                                                    WDCK = 'Y'
     C*
     C                   ELSE                                                   F1OUTA = Y
     C                   MOVE      *BLANK        DRKY                           STANDALONE
     C                   MOVE      *ZEROS        DRINFO
     C                   MOVE      @@CO#         DRCO#
     C                   MOVE      @@SUB#        DRSUB#
     C                   MOVE      @@FND         DRFND
     C                   MOVE      @@FYR         DRFYR
     C                   MOVE      @@CASE        DRCASE
     C                   MOVEL     DRKY          ##KEY
     C                   MOVEL(P)  'F0270R'      ##PGMN
      *** BEG ADD *** 04/01/98 **************************************
     C                   MOVEL     'M'           @@RTCD                         WCMC
      *** END ADD *** 04/01/98 **************************************
     C                   ENDIF                                                  F1OUTA = N
     C*
     C                   ENDIF                                                  RTCD = blank
     C                   ENDIF                                                  @@WCIN = Y
     C*
      *** BEG ADD *** 04/30/98 **************************************
      *
     C     @@WCIN        IFNE      'Y'                                          WC INTERFACE
     C                   MOVE      *BLANK        DRKY                           STANDALONE
     C                   MOVE      *ZEROS        DRINFO
     C                   MOVE      @@CO#         DRCO#
     C                   MOVE      @@SUB#        DRSUB#
     C                   MOVE      @@FND         DRFND
     C                   MOVE      @@FYR         DRFYR
     C                   MOVE      @@CASE        DRCASE
     C                   MOVEL     DRKY          ##KEY
     C                   MOVEL(P)  'F0270R'      ##PGMN
     C                   MOVEL     'M'           @@RTCD                         WCMC
     C                   ENDIF                                                  @@WCIN <> Y
      *
      *** END ADD *** 04/30/98 **************************************
     C*** BEGIN ADD *** 02/01/99 **************************************
     C*
     C* PASS BACK PROGRAM ID IF THE PARM IS USED.
     C*
     C     Q1PRMS        IFGT      9
     C*
     C                   MOVEL(P)  ##PGMN        @@PGM
     C*
     C                   ENDIF
     C*** END   ADD *** 02/01/99 **************************************
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CALL DARTS
      *
      *** BEG ADD *** 04/01/98 **************************************
     C     XXRTCD        IFNE      'K'
     C*** BEGIN ADD *** 04/25/00 **************************************
     C     ##KEY         ANDNE     *BLANKS
     C*** END ADD *** 04/25/00 ****************************************
      *** END ADD *** 04/01/98 **************************************
     C                   CALL      'SC010R'
     C                   PARM                    ##KEY
     C                   PARM                    ##PGMN
      *** BEG ADD *** 04/01/98 **************************************
     C                   END
      *** END ADD *** 04/01/98 **************************************
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
     C                   MOVE      *ON           *INLR
     C                   RETURN
