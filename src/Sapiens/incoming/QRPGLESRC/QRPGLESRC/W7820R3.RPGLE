     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7820R3 - COMMERCIAL ROLLOVER
     H*****************************************************************
     H**                                                              *
     H** PROGRAM DESCRIPTION:                                         *
     H**    THIS IS AN EXTENSION OF THE ROLLOVER PROGRAM.             *
     H**                                                              *
     H**    MODE 'L':                                                 *
     H**        ROLL LARGE DEDUCTIBLE RECORDS TO A NEW YEAR.          *
     H**                                                              *
     H**                                                              *
     H**                                                              *
     H** CALLED FROM:                                                 *
     H**    W7820R                                                    *
     H**                                                              *
     H** PROGRAMMER: C:65                                             *
     H** CREATED:    02/05/10         PROJECT: 25WRB                  *
     H**                                                              *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/06/10 C:062 PROJECT: 25WRB PRISS 19                        *
     H*               -ADD NEW YEAR TO ENTRY PARMS                    *
     H*                                                               *
     H* 03/22/10 C:136 PROJECT: 25WRB PRISS 17                        *
     H*               -ADD NEW YEAR TO ENTRY PARMS                    *
     H*               -REMOVE PROCESSING FOR SECOND RENEWAL FLAG      *
     H*                                                               *
     H*****************************************************************
      *** BEG CHG ***** 03/22/10 **************************************
     F*** WMLDL1  IF  E           K        DISK                      A
     F**********  PLD01                             KRENAMEPLD02
     F*** WMLDL1Q O   E                    DISK                      A
     F**********  PLD01                             KRENAMEPLD02Q
     FWMLDL1    IF A E           K DISK    USROPN
     F                                     RENAME(PLD01:PLD02)
     FWMLDL1Q   O  A E             DISK    USROPN
     F                                     RENAME(PLD01:PLD02Q)
      *** END CHG ***** 03/22/10 **************************************
      *
      *  KEY LIST
      *

     C     KEY1          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQFND
     C                   KFLD                    QQEMP#
     C                   KFLD                    QQDIV
      *** BEG CHG ***** 03/22/10 **************************************
     C***********          KFLD           QQKYR
     C                   KFLD                    QQPYR
      *** END CHG ***** 03/22/10 **************************************
      *
     C     *LIKE         DEFINE    LDCO#         QQCO#
     C     *LIKE         DEFINE    LDFND         QQFND
     C     *LIKE         DEFINE    LDEMP#        QQEMP#
     C     *LIKE         DEFINE    LDDIV         QQDIV
      *** BEG CHG ***** 03/22/10 **************************************
     C********** *LIKE     DEFN LDFYR     QQKYR
     C     *LIKE         DEFINE    LDFYR         QQPYR
     C     *LIKE         DEFINE    LDFYR         QQFYR
      *** END CHG ***** 03/22/10 **************************************
     C     *LIKE         DEFINE    LDEFD1        QQPFDT
      *
      *   ENTRY PARAMETERS
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
      *** BEG CHG ***** 03/22/10 **************************
     C***********          PARM           QQKYR
     C                   PARM                    QQPYR
     C                   PARM                    QQFYR
      *** END CHG ***** 03/22/10 **************************
     C                   PARM                    QQPFDT
     C                   PARM                    QQRTOQ            1
     C                   PARM                    QQMODE            1
     C*
     C     QQMODE        IFEQ      'L'
      *** BEG CHG ***** 03/22/10 **************************
     C***********          EXSR MODEL
     C                   OPEN      WMLDL1
     C                   EXSR      MODEL
     C                   CLOSE     WMLDL1
      *** END CHG ***** 03/22/10 **************************
     C                   ENDIF
     C*
     C                   MOVE      *ON           *INLR
     C*
     C********************************************************************
     C*
     C*  If the 1st Deductible is expired and the 2nd is not, move the 2nd
     C*   information to the first and clear the 2nd.
     C*  If the 1st and 2nd Deductible are expired, clear both.
     C*  In order for there to be a 2nd deductible, the first must expire.
     C*
     C     MODEL         BEGSR
     C*
     C     KEY1          CHAIN     PLD02                              60
     C     *IN60         IFEQ      *OFF
      *** BEG CHG ***** 03/22/10 ************************************
     C***********          MOVE LDRNW1    XX1RNW  1
     C***********          MOVE LDRNW2    XX2RNW  1
     C     LDRNW1        IFNE      'Y'
     C                   GOTO      SUBEND
     C                   ENDIF
      *** END CHG ***** 03/22/10 ************************************
     C*
     C                   SELECT
     C*
     C     LDEXP1        WHENNE    *ZEROS
     C     LDEXP1        ANDLE     QQPFDT
     C     LDEXP2        ANDEQ     *ZEROS
     C                   EXSR      MVLD
      *** BEG DEL ***** 03/22/10 **********************************
     C***********          MOVE XX2RNW    XX1RNW
      *** BEG DEL ***** 03/22/10 **********************************
     C                   EXSR      CLRD2
      *** BEG DEL ***** 03/22/10 **********************************
     C***********          MOVE *BLANKS   XX2RNW
      *** END DEL ***** 03/22/10 **********************************
     C*
     C     LDEXP1        WHENNE    *ZEROS
     C     LDEXP1        ANDLE     QQPFDT
     C     LDEXP2        ANDNE     *ZEROS
     C                   EXSR      CLRD1
     C     LDEXP2        IFNE      *ZEROS
     C     LDEXP2        ANDGT     QQPFDT
     C                   EXSR      MVLD
      *** BEG DEL ***** 03/22/10 **************************
     C***********          MOVE XX2RNW    XX1RNW
      *** END DEL ***** 03/22/10 **************************
     C                   EXSR      CLRD2
      *** BEG DEL ***** 03/22/10 **************************
     C***********          MOVE *BLANKS   XX2RNW
      *** END DEL ***** 03/22/10 **************************
     C                   ENDIF                                                  LDEXP2>QQPFDT
     C     LDEXP2        IFNE      *ZEROS
     C     LDEXP2        ANDLE     QQPFDT
     C                   EXSR      CLRD2
     C                   ENDIF                                                  LDEXP2<=QQPFDT
     C                   ENDSL
     C*
     C*  If a deductible code still exists, the prior years deductible did
     C*   NOT expire, therefor the effective date of the deductible for the
     C*   current year will be the policy from date.
     C*
      *** BEG ADD ***** 04/06/10 *******************************
     C                   MOVE      QQFYR         LDFYR
     C                   Z-ADD     QQPFDT        LDEFD1
      *** END ADD ***** 04/06/10 *******************************
     C     LDPCT1        IFNE      *ZERO
      *** BEG ADD ***** 03/22/10 *******************************
      *** BEG DEL ***** 04/06/10 *******************************
     C**********           MOVE QQFYR     LDFYR
      *** END DEL ***** 04/06/10 *******************************
     C                   MOVE      'Y'           LDRNW1
      *** END ADD ***** 03/22/10 *******************************
      *** BEG DEL ***** 04/06/10 *******************************
     C**********           Z-ADDQQPFDT    LDEFD1
      *** END DEL ***** 04/06/10 *******************************
     C                   ENDIF
      *
      *** BEG DEL ***** 03/22/10 *******************************
     C********** XX1RNW    IFNE 'Y'
     C**********           EXSR CLRD1
     C**********           ENDIF                           DDRNW
     C**********
     C********** XX2RNW    IFNE 'Y'
     C**********           EXSR CLRD2
     C**********           ENDIF
      *** END DEL ***** 03/22/10 *******************************
     C*
     C     QQRTOQ        IFEQ      'Y'
      *** BEG CHG ***** 03/22/10 *******************************
     C***********          WRITEPLD02Q
     C                   OPEN      WMLDL1Q
     C                   WRITE     PLD02Q
     C                   CLOSE     WMLDL1Q
      *** END CHG ***** 03/22/10 *******************************
     C                   ELSE
     C                   WRITE     PLD02
      *** BEG ADD ***** 03/22/10 *******************************
     C                   CLOSE     WMLDL1
      *** END ADD ***** 03/22/10 *******************************
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
      *** BEG CHG ***** 03/22/10 **********************************
     C**********           ENDSR                           MOVEL SUBR
     C     SUBEND        ENDSR
      *** END CHG ***** 03/22/10 **********************************
     C************************************************************************
     C     MVLD          BEGSR
     C                   MOVE      LDEFD2        LDEFD1
     C                   MOVE      LDEXP2        LDEXP1
     C                   MOVE      LDDES2        LDDES1
     C                   MOVE      LDPCT2        LDPCT1
     C                   MOVE      LDTYP2        LDTYP1
     C                   MOVE      LDAMP2        LDAMP1
     C                   MOVE      LDAMG2        LDAMG1
     C                   MOVE      LDPLN2        LDPLN1
     C                   MOVE      LDVRB2        LDVRB1
     C                   MOVE      LDOCV2        LDOCV1
     C                   MOVE      LDTOT2        LDTOT1
     C                   MOVE      LDCLS2        LDCLS1
     C                   MOVE      LDPLV2        LDPLV1
     C                   MOVE      LDALA2        LDALA1
     C                   MOVE      LDRNW2        LDRNW1
     C                   MOVE      LDR201        LDR101
     C                   MOVE      LDR202        LDR102
     C                   MOVE      LDR203        LDR103
     C                   MOVE      LDR204        LDR104
     C                   MOVE      LDR205        LDR105
     C                   MOVE      LDR206        LDR106
     C                   MOVE      LDR207        LDR107
     C                   MOVE      LDR208        LDR108
     C                   MOVE      LDR209        LDR109
     C                   MOVE      LDR210        LDR110
     C                   ENDSR
     C**************************************************************
     C     CLRD1         BEGSR
     C                   CLEAR                   LDEFD1
     C                   CLEAR                   LDEXP1
     C                   CLEAR                   LDDES1
     C                   CLEAR                   LDPCT1
     C                   CLEAR                   LDTYP1
     C                   CLEAR                   LDAMP1
     C                   CLEAR                   LDAMG1
     C                   CLEAR                   LDPLN1
     C                   CLEAR                   LDVRB1
     C                   CLEAR                   LDOCV1
     C                   CLEAR                   LDTOT1
     C                   CLEAR                   LDCLS1
     C                   CLEAR                   LDPLV1
     C                   CLEAR                   LDALA1
     C                   CLEAR                   LDRNW1
     C                   CLEAR                   LDR101
     C                   CLEAR                   LDR102
     C                   CLEAR                   LDR103
     C                   CLEAR                   LDR104
     C                   CLEAR                   LDR105
     C                   CLEAR                   LDR106
     C                   CLEAR                   LDR107
     C                   CLEAR                   LDR108
     C                   CLEAR                   LDR109
     C                   CLEAR                   LDR110
     C                   ENDSR
     C**************************************************************
     C     CLRD2         BEGSR
     C                   CLEAR                   LDEFD2
     C                   CLEAR                   LDEXP2
     C                   CLEAR                   LDDES2
     C                   CLEAR                   LDPCT2
     C                   CLEAR                   LDTYP2
     C                   CLEAR                   LDAMP2
     C                   CLEAR                   LDAMG2
     C                   CLEAR                   LDPLN2
     C                   CLEAR                   LDVRB2
     C                   CLEAR                   LDOCV2
     C                   CLEAR                   LDTOT2
     C                   CLEAR                   LDCLS2
     C                   CLEAR                   LDPLV2
     C                   CLEAR                   LDALA2
     C                   CLEAR                   LDRNW2
     C                   CLEAR                   LDR201
     C                   CLEAR                   LDR202
     C                   CLEAR                   LDR203
     C                   CLEAR                   LDR204
     C                   CLEAR                   LDR205
     C                   CLEAR                   LDR206
     C                   CLEAR                   LDR207
     C                   CLEAR                   LDR208
     C                   CLEAR                   LDR209
     C                   CLEAR                   LDR210
     C                   ENDSR
     C**************************************************************
