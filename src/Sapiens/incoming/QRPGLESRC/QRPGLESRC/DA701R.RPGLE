     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
      /TITLE DA701R - API TO WRITE A DARTS ADDITIONAL REMARK
      * *****************************************************************
      *  Description:                                                   *
      *
      *  THIS API WILL WRITE A DARTS ADDITIONAL REMARK.
      *  IT WILL NOT DO ANY VALIDATING OF THE PASSED IN PARAMETERS.
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SEND TO THIS PGM:  WHATEVER YOU WANT WRITTEN TO DARTS.
      *
      *  SEND BACK       :  NEXT AVAILIBLE SEQUENCE# FOR A DARTS
      *                     ADDITIONAL REMARK.
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *                                                                 *
      *  Called From:                                                   *
      *    VARIOUS PROGRAMS                                             *
      *                                                                 *
      *  Calls To:                                                      *
      *    NONE                                                         *
      *                                                                 *
      *  Created:   4/01/98  C:47  Project:  87MCB                      *
      *                                                                 *
      * *****************************************************************
      ** REVISIONS:
      *
      *  03/15/04 C:128  Support:                                       *
      *                  Changed pgm to update the first position of    *
      *                  the field CMIQUE in the SDCMP file to indicate *
      *                  that there are additional remarks.             *
      *
      *  02/05/04 C:128  Support:                                       *
      *                  Changed pgm to replace any character less      *
      *                  than Hex 40 and greater than Hex F9 with       *
      *                  Hex 40. These characters can not be displayed  *
      *                  on the AS400.
      *
      *  07/22/02 C:128  Support: 33334                                 *
      *                  Changed pgm to check for Hex 15. This un-      *
      *                  displayable character is being keyed by users. *
      *                  Hex 15 is being replaced by Hex 40(blank).     *
      *                                                                 *
      *  04/25/00 C:31  Project: DR097
      *                 Chg ALL PDS pgms
      *                 1. Add parms
      *                                                                 *
      *  05/15/98 C:53    Project:  MC145
      *                   REMOVE LR INDICATOR
      *                                                                 *
      * *****************************************************************
      *  Indicators:                                                    *
      *                                                                 *
      *    60 - General File Access                                     *
      * *****************************************************************
      *
     FSMARL1    IF   E           K DISK
     F                                     RENAME(PAR01:PAR0I)
     FSMARP     O    E             DISK
      *** Begin Add *** 03/15/04 **************************************
     FSDCMLE    UF   E           K DISK
      *** End   Add *** 03/15/04 **************************************
      *
      *** Begin Add *** 02/05/04 **************************************

     D CM              S              1    DIM(75)
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** End   Add *** 02/05/04 **************************************
      *

     IPAR0I
     I              ARASQ#                      A2ASQ#
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
      *  DARTS FILE
      *

     C     A2KY          KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    XXSEQ#
     C                   KFLD                    XXASQ#
      *
     C     A2KY2         KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    XXSEQ#
      *
      *** Begin Add *** 03/15/04 **************************************
     C     CMKY          KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    @@SEQ#
      *** End   Add *** 03/15/04 **************************************
      *
     C     *LIKE         DEFINE    ARDT          XXDT
     C     *LIKE         DEFINE    ARSEQ#        XXSEQ#
     C     *LIKE         DEFINE    ARASQ#        XXASQ#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQSEQ#            5
     C                   PARM                    QQCM75           75
     C                   PARM                    QQPNAM           10
     C                   PARM                    QQSECL            3
     C                   PARM                    QQUSER           10
     C                   PARM                    QQPGM            10
     C                   PARM                    QQRMCD            5
     C                   PARM                    QQASQ#            5
     C*** BEGIN ADD *** 04/25/00 **************************************
     C                   PARM                    QQCSID            2            10
     C                   PARM                    QQCDAT            7            11
     C                   PARM                    QQCODE            2            12
     C*** END ADD *** 04/25/00 ****************************************
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
     C                   CLEAR                   PAR01                          ADD 05/15/98
      *
      * GET TODAY'S DATE
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C*   GET LAST SEQ# USED
     C*
     C                   MOVE      QQSEQ#        XXSEQ#
     C                   Z-ADD     *HIVAL        XXASQ#
     C     A2KY          SETGT     PAR0I
     C     A2KY2         READPE    PAR0I                                  60
     C     *IN60         IFEQ      *OFF
     C     A2ASQ#        ADD       1             ARASQ#
     C                   ELSE
     C                   Z-ADD     1             ARASQ#
     C                   END
      *
      *   ESTABLISH RECORD
      *
     C                   MOVEL     QQKEY         ARKEY
     C                   MOVE      QQSEQ#        ARSEQ#
     C                   MOVEL(P)  QQCM75        ARCM75                         COMMENT
      *
     C                   MOVE      ARASQ#        QQASQ#
     C                   MOVE      *BLANK        ARCSID                         CUST GROUP ID
     C                   Z-ADD     XXDT          ARCDAT                         REMARK DATE
     C                   MOVE      *BLANK        ARCODE                         REMARK TYPE
     C*** BEGIN ADD *** 04/25/00 **************************************
     C     Q1PRMS        IFGE      10
     C                   MOVE      QQCSID        ARCSID                                      ID
     C                   ENDIF                                                  PRMS >= 10
     C     Q1PRMS        IFGE      11
     C                   MOVE      QQCDAT        ARCDAT                                      ID
     C                   ENDIF                                                  PRMS >= 11
     C     Q1PRMS        IFGE      12
     C                   MOVE      QQCODE        ARCODE                                      ID
     C                   ENDIF                                                  PRMS >= 12
     C*** END ADD *** 04/25/00 ****************************************
     C                   MOVEL     QQPNAM        ARPNAM                         KEYED PGM
     C                   Z-ADD     XXDT          ARTRDT                         DATE
     C                   Z-ADD     DSTIME        ARTRTM                         TIME
     C                   MOVE      QQSECL        ARSECL                         SECURITY LEVEL
      *
      * SECURE BY LEVEL
      *
     C     ARSECL        IFEQ      *ZERO
     C                   MOVE      'N'           ARSBYL
     C                   ELSE
     C                   MOVE      'Y'           ARSBYL
     C                   END
      *
     C                   MOVEL(P)  'A'           ARSTAT
     C                   MOVEL(P)  QQUSER        ARUSER
     C                   MOVEL(P)  QQPGM         ARPGM
     C                   Z-ADD     DSTIME        ARTIME
     C                   Z-ADD     XXDT          ARDT
     C                   MOVEL(P)  Q1USER        ARIUSR
     C                   MOVEL(P)  QQRMCD        ARRMCD                         REMARK CODE
     C                   Z-ADD     XXDT          ARIDT
      *** Begin Chg *** 02/05/04 **************************************
      *** Begin add *** 07/22/02 **************************************
      *
     C******               MOVE X'15'     BAD     1
     C******               MOVELARCM75    T75    75
     C******     BAD:' '   XLATET75       R75    75
     C******               MOVELR75       ARCM75
      *
      *** End   add *** 07/22/02 **************************************
      * Filter out characters that can not be displayed on the AS400
     C                   CLEAR                   CM
     C                   MOVEA     ARCM75        CM
      *
     C                   DO        75            X                 3 0
     C     CM(X)         IFLT      X'40'
      *
     C     CM(X)         IFNE      X'08'
     C     CM(X)         ANDNE     X'09'
     C                   MOVE      X'40'         CM(X)
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     CM(X)         IFGT      X'F9'
     C                   MOVE      X'40'         CM(X)
     C                   ENDIF
     C                   ENDDO
      *
     C                   MOVEA     CM            ARCM75
      *** End   Chg *** 02/05/04 **************************************
      *
     C                   WRITE     PAR01
      *
      *** Begin Add *** 03/15/04 **************************************
      *
     C                   CLEAR                   PCM01                          Clear prev. Rcd
      *
     C                   MOVE      QQSEQ#        @@SEQ#            5 0
     C     CMKY          CHAIN     PCM01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     'Y'           CMIQUE
     C                   UPDATE    PCM01
     C                   ENDIF
      *
      *** End   Add *** 03/15/04 **************************************
     C**********           MOVE *ON       *INLR            DEL 05/15/98
     C                   RETURN
