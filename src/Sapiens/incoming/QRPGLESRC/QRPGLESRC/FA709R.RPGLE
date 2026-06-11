     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FA709R - API TO BUILD DARTS KEY AND OPTIONALLY CALL DARTS
     H*******************************************************************
     H*  Description:
     H*
     H*  SEND TO THIS PGM:  IF @@RTCD = BLANK, CALL DARTS
     H*                     IF @@RTCD = 'K' (KEY ONLY), BUILD DARTS KEY
     H*                       AND RETURN WITHOUT CALLING DARTS
     H*
     H*  SEND BACK       :  IF @@RTCD = BLANK, NOTHING
     H*                     IF @@RTCD = 'K' (KEY ONLY):
     H**************************************************************** - -
     H*                                                                 *
     H*  Called From:                                                   *
     H*    VARIOUS PROGRAMS                                             *
     H*                                                                 *
     H*  Calls To:                                                      *
     H*    SC010R                                                       *
     H*                                                                 *
     H*  Created:  05/04/98  C:61  Project:  98MAC                      *
     H*******************************************************************
     H*  Revisions:                                                     *
     H*                                                                 *
     H*  02/19/99  C:94 PROJ:MC145                                      *
     H*                 ADD PARM TO RETURN THE PROGRAM ID.              *
     H*                                                                 *
     H*******************************************************************
     H*  Indicators:                                                    *
     H*                                                                 *
     H*    60 - General File Access                                     *
     H*******************************************************************
      *
      *  DARTS KEY
      *

     D                 DS
     D  DRKY                   1     40
     D  DRCO#                  1      3  0
     D  DRSUB#                 4      8  0
     D  DRFND                  9     11  0
     D  DRFYR                 12     14  0
     D  DRCASE                15     21  0
     D  DRMSCD                22     25
     D  DRSEQ#                26     30  0
      *
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     *ENTRY        PLIST
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@MSCD            4
     C                   PARM                    @@SEQ#            5
     C                   PARM                    @@KEY            40
     C*** BEG ADD *** 02/19/99 ****************************************
     C                   PARM                    @@PGM            10
     C*** END ADD *** 02/19/99 ****************************************
      *
      *
     C                   MOVE      @@CO#         DRCO#
     C                   MOVE      @@SUB#        DRSUB#
     C                   MOVE      @@FND         DRFND
     C                   MOVE      @@FYR         DRFYR
     C                   MOVE      @@CASE        DRCASE
     C                   MOVE      @@MSCD        DRMSCD
     C                   MOVE      @@SEQ#        DRSEQ#
      *
     C     @@RTCD        IFEQ      'K'
     C                   MOVE      *BLANK        @@RTCD
     C                   MOVEL(P)  DRKY          @@KEY
     C*** BEG ADD *** 02/19/99 ****************************************
     C*
     C* PASS BACK PROGRAM ID IF THE PARM IS USED.
     C*
     C     Q1PRMS        IFGT      9
     C                   MOVEL(P)  'F0276R'      @@PGM
     C                   ENDIF
     C*** END ADD *** 02/19/99 ****************************************
      *
     C                   ELSE
      *
      * CALL DARTS
      *
     C                   MOVEL     *BLANKS       ##KEY
     C                   MOVEL     DRKY          ##KEY
      *
     C                   MOVEL(P)  'F0276R'      ##PGMN
      *
     C                   CALL      'SC010R'
     C                   PARM                    ##KEY            40
     C                   PARM                    ##PGMN           10
      *
     C                   ENDIF
      *
      *
     C                   MOVE      *ON           *INLR
     C                   RETURN
