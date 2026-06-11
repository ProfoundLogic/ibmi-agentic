     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8070R1 - FIRST REPORT OF INJURY SUBMITTER
     H*
     H*****************************************************************
     H* PROGRAM DESCRIPTION:  THIS PROGRAM SELECTS THE APPROPRIATE    *
     H*               PGM, BASED ON THE LOG RECORD HARDCOPY FORMAT,   *
     H*               TO PRINT THE FROI HARDCOPY.                     *
     H*                                                               *
     H* CALLED FROM:  W8070C1                                         *
     H* CALLS TO:     W8070CXX     WHERE 'XX' = WMIXP/IXFROI          *
     H*                                                               *
     H* PROJECT:      25WDE         C:15           02/19/96           *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/14/10 C:220 Project: 139WNN                                *
     H*                EDI extract and FTP automation - add new parm  *
     H*                                                               *
     H* 03/24/10 C:232 Project: 139WNN                                *
     H*                Update programs to use the new EDI Control API *
     H*                WA076R instead of using WMIXP.                 *
     H*                                                               *
     H* 04/07/05 C:156 PROJECT: 126WDK                                *
     H*          C:15  Project: 126WBC                                *
     H*                Added code to call all print pgms with parms   *
     H*                for the FND/FYR/CASE.                          *
     H*                                                               *
     H*  CREATED: 06/18/97  for Version 3 BASE
     H*                    C:58  PROJECT: WC468
     H*                                                               *
     H*   04/96 - ENHANCED PROCESSING TO INCLUDE A49 IN ADDITION TO   *
     H*           148 PROCESSING.                                     *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*  60  -  GENERAL USAGE (CHAIN, READE)                          *
     H*****************************************************************
     F*** BEGIN DLT *** 03/24/10 **************************************
     F***WMIXL   IF  E           K        DISK
     F*** END DLT ***** 03/24/10 **************************************

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR THE EDI CLAIM CONTROL FILE
     C*
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJRST
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQJRST            2
     C                   PARM                    QQDATE            8
     C                   PARM                    QQTSET            1
     C*** BEG ADD ****** 05/14/10 *************************************
     C                   PARM                    QQEDIF            3
     C*** END ADD ****** 05/14/10 *************************************
      *
     C*** BEG ADD ****** 03/24/10 *************************************
     C     IXAPI         PLIST
     C                   PARM                    XXCO#             3 0
     C                   PARM                    XXJRST            2
     C                   PARM                    XXTSET            3
     C                   PARM                    XXFND             3 0
     C                   PARM      *BLANKS       @RTNCD            1
     C*** BEG CHG ****** 05/14/10 *************************************
     C*******              PARM *BLANKS   @EDIF   3
     C                   PARM      QQEDIF        @EDIF             3
     C*** END CHG ****** 05/14/10 *************************************
     C                   PARM      *BLANKS       IXELT             1
     C                   PARM      *ZEROS        IXELTD            3 0
     C                   PARM      *BLANKS       IXFROI            2
     C                   PARM      *ZEROS        IXSDUE            3 0
     C                   PARM      *BLANKS       IXRLS             1
     C                   PARM      *ZEROS        IXCSDT            7 0
     C                   PARM      *BLANKS       IXEADC            5
     C                   PARM      *BLANKS       IXHCPR            5
     C                   PARM      *BLANKS       IXFNRC            5
     C                   PARM      *BLANKS       IXHCED            1
     C*** END ADD ****** 03/24/10 *************************************
      *
     C*
     C*** BEG DEL ****** 03/24/10 *************************************
     C***        *LIKE     DEFN IXCO#     XXCO#
     C***        *LIKE     DEFN IXJRST    XXJRST
     C***        *LIKE     DEFN IXTRID    XXTSET
     C*** END DEL ****** 03/24/10 *************************************
     C*
     C*** BEG CHG ****** 03/24/10 *************************************
     C***                  MOVE QQCO#     XXCO#
     C***                  MOVE QQJRST    XXJRST
     C***                  MOVE *BLANKS   XXTSET
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQJRST        XXJRST            2
     C                   MOVE      *BLANKS       XXTSET            3
     C*** END CHG ****** 03/24/10 *************************************
     C     QQTSET        IFEQ      '1'
     C*** BEG ADD ****** 03/24/10 *************************************
     C     QQTSET        OREQ      'B'
     C*** END ADD ****** 03/24/10 *************************************
     C                   MOVE      '148'         XXTSET
     C*** BEG ADD ****** 03/24/10 *************************************
     C                   EXSR      SRAPI
     C*** END ADD ****** 03/24/10 *************************************
     C                   END
     C     QQTSET        IFEQ      'A'
     C*** BEG ADD ****** 03/24/10 *************************************
     C     QQTSET        OREQ      'B'
     C*** END ADD ****** 03/24/10 *************************************
     C                   MOVE      'A49'         XXTSET
     C*** BEG ADD ****** 03/24/10 *************************************
     C                   EXSR      SRAPI
     C*** END ADD ****** 03/24/10 *************************************
     C                   END
     C*
     C*** BEG DEL ****** 03/24/10 *************************************
     C*  SUBMIT A HARDCOPY PRINT FOR EACH TRANSACTION TYPE
     C*
     C***        IXKY      CHAINPIX01                60
     C***        *IN60     DOWEQ'0'
     C*
     C***        IXTRID    IFEQ XXTSET
     C***        QQTSET    OREQ 'B'
     C***        IXFROI    IFNE *BLANKS
     C***                  MOVE *BLANKS   PRTPGM  8
     C***                  MOVEL'W8070C'  PRTPGM
     C***                  MOVE IXFROI    PRTPGM
     C*** Begin add *** 02/18/03 **************************************
     C***                  MOVE *BLANKS   QQFND
     C***                  MOVE *BLANKS   QQFYR
     C***                  MOVE *BLANKS   QQCASE
     C*** End add ***** 02/18/03 **************************************
     C***                  CALL PRTPGM                 99
     C***                  PARM           QQCO#
     C***                  PARM           IXFROI           HARDCOPY FORMAT
     C***                  PARM           QQJRST
     C***                  PARM           QQDATE
     C*** Begin add *** 02/18/03 **************************************
     C***                  PARM           QQFND   3
     C***                  PARM           QQFYR   3
     C***                  PARM           QQCASE  7
     C*** End add ***** 02/18/03 **************************************
     C***                  ENDIF
     C***                  ENDIF
     C*
     C***        IXKY      READEPIX01                    60
     C***                  END
     C*** BEG DEL ****** 03/24/10 *************************************
     C*
     C*  END OF PROGRAM
     C*
     C                   MOVE      '1'           *INLR                    60
     C*
     C*** BEG ADD ****** 03/24/10 *************************************
     C     SRAPI         BEGSR
     C*
     C                   CALL      'WA076R'      IXAPI                  99
     C*
     C     IXFROI        IFNE      *BLANKS
     C                   MOVE      *BLANKS       PRTPGM            8
     C                   MOVEL     'W8070C'      PRTPGM
     C                   MOVE      IXFROI        PRTPGM
     C*** Begin add *** 02/18/03 **************************************
     C                   MOVE      *BLANKS       QQFND
     C                   MOVE      *BLANKS       QQFYR
     C                   MOVE      *BLANKS       QQCASE
     C*** End add ***** 02/18/03 **************************************
     C                   CALL      PRTPGM                               99
     C                   PARM                    QQCO#
     C                   PARM                    IXFROI                         HARDCOPY FORMAT
     C                   PARM                    QQJRST
     C                   PARM                    QQDATE
     C*** Begin add *** 02/18/03 **************************************
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C*** End add ***** 02/18/03 **************************************
     C                   ENDIF
     C                   ENDSR
     C*** END ADD ****** 03/24/10 *************************************
