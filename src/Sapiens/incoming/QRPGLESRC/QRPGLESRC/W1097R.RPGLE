     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR dftactgrp(*no) actgrp('QILE')
     H Dftactgrp(*no) ActGrp(*Caller)
     H/define CONVERT
     H/TITLE W1097R - CREATE AGENT WORKFILE WXACP
     H** =========================================================== **
     H*  (C) Copyright Progressive Data Solutions, Inc., 1990
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*         W1099C - CREATE 1099 WORKFILE AND REPORT              *
     H*  CALLS TO:                                                    *
     H*         NONE                                                  *
     H** =========================================================== **
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/28/15  C:33  Project: WC2396                               *
     H*                 Account for no group passed.                  *
     H*                                                               *
     H* 01/23/13  C:287 Seq#: SY266_OVR                               *
     H*                 Added DFTACTGRP and ACTGRP compile options    *
     H*                                                               *
     H* 09/17/02  C:132 PROJECT: WC1096                               *
     H*                 ADD Processing of WXT5P for all groups to be  *
     H*                 included for agent 1099 creation. Skip all    *
     H*                 Records without any commission amount.        *
     H*                 Check all records to be in the date range     *
     H*                                                               *
     H*                                                               *
     H* 01/24/00  C:33  PROJECT: WC899                                *
     H*                 ADD ACCOUNT CURRENT PROCESSING.               *
     H** =========================================================== **
     FWMACL3    IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     F***  BEGIN ADD  ***  01/24/00  *********************************
     FWDABL2    IF   E           K DISK
     F***  END   ADD  ***  01/24/00  *********************************
     FWXACP     O    E             DISK
     F                                     RENAME(PAC01:PAC02)
     F**** BEG ADD ***** 09/17/02 ************************************
     FWXT5P     IF   E           K DISK
     F**** END ADD ***** 09/17/02 ************************************

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C** =========================================================== **
     C*    KEY LISTINGS
     C** =========================================================== **
     C*   KEY LIST FOR THE AGENT COMMISSION FILE
     C*
     C     ACKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C*
     C*   KEY LIST FOR THE AGENT ASSIGNMENT FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    ACFND
     C                   KFLD                    ACAGT#
      **** BEG ADD ***** 09/17/02 ************************************
     C     ABKY          KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
      **** END ADD ***** 09/17/02 ************************************
     C*
     C***  BEGIN ADD  ***  01/24/00  *********************************
     C     AAKY1         KLIST
     C                   KFLD                    ABCO#
     C                   KFLD                    ABFND
     C                   KFLD                    ABAGT#
     C***  END ADD ******  01/24/00  *********************************
     C*
      **** BEG ADD ***** 09/17/02 ************************************
     C     T5KY          KLIST
     C                   KFLD                    QQCO#
      *
     C     T5KY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQFND
      **** END ADD ***** 09/17/02 ************************************
     C*
     C** =========================================================== **
     C*    ENTRY PARMS
     C** =========================================================== **
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFRDT            7
     C                   PARM                    QQTODT            7
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFRDT        XXFRDT            7 0
     C                   MOVE      QQTODT        XXTODT            7 0
     C*
     C** =========================================================== **
     C*    MAIN LINE
     C** =========================================================== **
     C*
      **** BEG ADD ***** 09/17/02 ************************************
      *  Start Reading the Group Selection file, replace xxfnd if
      *  Not same as passed Parm QQFND
     C***  Begin Chg  ***  10/28/15  **************************************
     C*****T5KY1         CHAIN     PT501                              51
     C                   Select
     C                   When      qqFnd = *BLANKS or qqFnd = *zeros
     C     T5KY          Chain     PT501                              51
     C                   Other
     C     T5KY1         Chain     PT501                              51
     C                   Endsl
     C***  End   Chg  ***  10/28/15  **************************************
     C     *IN51         IFEQ      *OFF
     C                   MOVE      T5FND         XXFND
     C                   ELSE
     C     T5FND         IFEQ      *BLANKS
     C     T5FND         OREQ      *ZEROS
     C                   GOTO      TERM
     C                   ENDIF
     C                   ENDIF
     C     NXTGRP        TAG
      **** END ADD ***** 09/17/02 ************************************
      *
     C*   START READING THE AGENT COMMISSION FILE
     C*
     C     ACKEY         CHAIN     PAC01                              50
     C     *IN50         DOWEQ     '0'
     C*
      * IF TRANSACTION DATE NOT IN RANGE, BYPASS THIS RECORD
      *
     C     ACTRDT        IFLT      XXFRDT
     C     ACTRDT        ORGT      XXTODT
     C                   GOTO      NXTRCD
     C                   ENDIF
     C*
     C*   GET FEIN AND MOD FROM THE AGENT ASSIGNMENT FILE
     C*
     C     AAKEY         CHAIN     PAA01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      AAFEIN        ACFEIN
     C                   MOVE      AAMOD         ACMOD
     C                   ELSE
     C                   MOVE      *ZEROS        ACFEIN
     C                   MOVE      *ZEROS        ACMOD
     C                   END
     C*
     C*   WRITE RECORD, IF NO HIT ON AGENT ASSIGNMENT FILE THEN
     C*     FEIN AND MOD WILL BE ZEROS AND 1099 PGM WILL GENERATE
     C*     ERROR.
     C*
      **** BEG CHG ***** 09/17/02 ************************************
      * If have a commission amount or Collected amount, write record
     C     ACTOCL        IFNE      *ZEROS
     C     ACAMT         ORNE      *ZEROS
     C                   WRITE     PAC02
     C                   ENDIF
     C*************        WRITEPAC02
      **** END CHG ***** 09/17/02 ************************************
     C*
     C     NXTRCD        TAG
     C*
     C*   READ NEXT RECORD
     C*
     C     ACKEY         READE     PAC01                                  50
     C                   END
     C*
      **** BEG ADD ***** 09/17/02 ************************************
      *
      *  Check Group Selection file for more groups, if so go back
      *  and process the next group
     C     T5KY          READE     PT501                                  51
     C     *IN51         IFEQ      *OFF
     C                   MOVE      T5FND         XXFND
     C                   GOTO      NXTGRP
     C                   ENDIF
     C** ========================================================= **
     C*  Process the Account Current Commission file Now
     C** ========================================================= **
     C***  BEGIN ADD  ***  01/24/00  *********************************
     C*
      **** BEG ADD ***** 09/17/02 ************************************
      *  Start Reading the Group Selection file to reset for Agent
      *  Current file group processing
     C     T5KY1         CHAIN     PT501                              51
     C     *IN51         IFEQ      *OFF
     C                   MOVE      T5FND         XXFND
     C                   ELSE
     C                   GOTO      TERM
     C                   ENDIF
     C     NXTGP2        TAG
      **** END ADD ***** 09/17/02 ************************************
     C     ABKY          SETLL     PAB01
     C     ABKY          READE     PAB01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
      *
      * IF TRANSACTION DATE NOT IN RANGE, BYPASS THIS RECORD
      *
     C     ABDUE         IFLT      XXFRDT
     C     ABDUE         ORGT      XXTODT
     C                   GOTO      NXTABR
     C                   ENDIF
     C*
     C*  GET FEIN AND MOD FROM THE AGENT ASSIGNMENT FILE
     C*
     C     AAKY1         CHAIN     PAA01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   AAFEIN
     C                   CLEAR                   AAMOD
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   EXSR      MAPFLD
     C*
     C*   WRITE RECORD, IF NO HIT ON AGENT ASSIGNMENT FILE THEN
     C*     FEIN AND MOD WILL BE ZEROS AND 1099 PGM WILL GENERATE
     C*     ERROR.
     C*
      **** BEG CHG ***** 09/17/02 ************************************
      * If have a commission amount or Collected amount, write record
     C     ACTOCL        IFNE      *ZEROS
     C     ACAMT         ORNE      *ZEROS
     C                   WRITE     PAC02
     C                   ENDIF
     C*************        WRITEPAC02
     C*
     C     NXTABR        TAG
      **** END CHG ***** 09/17/02 ************************************
     C*   READ NEXT RECORD
     C*
     C                   READ      PAB01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C***  END   ADD  ***  01/24/00  *********************************
     C*
      **** BEG ADD ***** 09/17/02 ************************************
      *
      *  Check Group Selection file for more groups, if so go back
      *  and process the next group
     C     T5KY          READE     PT501                                  51
     C     *IN51         IFEQ      *OFF
     C                   MOVE      T5FND         XXFND
     C                   GOTO      NXTGP2
     C                   ENDIF
      *
     C     TERM          TAG                                                    END Program
      **** END ADD ***** 09/17/02 ************************************
     C                   MOVE      '1'           *INLR
     C*
     C***  BEGIN ADD  ***  01/24/00  *********************************
     C*===============================================================
     C* MAPFLD - MAP WDABP FIELDS TO WMACP
     C*===============================================================
     C*
     C     MAPFLD        BEGSR
     C*
     C                   CLEAR                   PAC02
     C*
     C                   Z-ADD     ABCO#         ACCO#
     C                   Z-ADD     ABFND         ACFND
     C                   Z-ADD     ABFYR         ACFYR
     C                   Z-ADD     ABEMP#        ACEMP#
     C                   Z-ADD     ABDIV         ACDIV
     C                   Z-ADD     ABAGT#        ACAGT#
     C                   Z-ADD     ABPROD        ACPROD
     C                   Z-ADD     ABPODT        ACPODT
     C                   Z-ADD     ABDUE         ACTRDT
     C                   Z-ADD     ABAMT         ACTOCL
     C                   Z-ADD     ABRATE        ACRATE
     C                   Z-ADD     ABCOMM        ACAMT
     C                   Z-ADD     ABCHK#        ACCHCK
     C                   MOVEL     *BLANKS       ACCOCD
     C                   MOVEL     'P'           ACCOTP
     C                   MOVEL     *BLANKS       ACDESC
     C*
     C                   Z-ADD     AAFEIN        ACFEIN
     C                   Z-ADD     AAMOD         ACMOD
     C*
     C                   MOVEL     ABSTAT        ACSTAT
     C                   MOVEL     ABUSER        ACUSER
     C                   MOVEL     ABPGM         ACPGM
     C                   Z-ADD     ABTIME        ACTIME
     C                   Z-ADD     ABDT          ACDT
     C                   MOVEL     ABIUSR        ACIUSR
     C                   MOVEL     ABIPGM        ACIPGM
     C                   Z-ADD     ABITIM        ACITM
     C                   Z-ADD     ABIDT         ACIDT
     C*
     C                   ENDSR                                                  MAPFLD
     C***  END   ADD  ***  01/24/00  *********************************
