     H* (C) Copyright Fiserv, Inc., 2002 - 2007
     H*
     H/TITLE W1551R02 - SCHEDULED RATING INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE SCHEDULED RATING FILE.   *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0051R02 - EMPLOYER DETAIL INFORMATION                     *
     H*                                                               *
     H*                                                               *
     H*  CREATED: 10/17/05  PROGRAMMER: C:156 PROJECT: 114WKY         *
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 08/08/07 C:132  PROJECT: 114WMS
     H*               - Compiled for screen changes, no chgs in code
     H*
     H* 07/13/07 C:132  PROJECT: 114WMS
     H*               - Chgs as per QA findings.
     H*
     H* 04/13/07 C:132  PROJECT: 114WMS
     H*               - ADD edits on DB/CR Factors to make sure there
     H*                 is a Matching Reason for each entered factor.
     H*               - If RFPRTF = "Y" and any screen field change &
     H*                 does not match the DB fields, then blank out
     H*                 RFPRTF before the update record.
     H*
     H* 04/04/07    C:180      WA114/53245                            *
     H*                        Ensure sched rating info remarks are
     H*                        written to DARTS when in quote mode.
     H*
     H* 10/27/06 C:61 PROJECT 114WMS
     H*               Do not ADD or UPDATE if called in DISPLAY mode.
     H*
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  81 - (WCR0590)                                               *
     H*  82 - (WCR0807)                                               *
     H*  83 - (WCR0807)                                               *
     H*  84 - (WCR0010)                                               *
     H*  85 - (WCR0500)                                               *
     H*  86 - (WCR0585)                                               *
     H*  87 - (WCR0500)                                               *
     H*  88 - (WCR0539)                                               *
     H*  89 - (WCR1058)                                               *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR02:                                                       *
     H*  ERROR:                                                       *
     H****************************************************************
     H*
     H*  MAINLINE (INTERNAL) INDICATOR REFERENCE SUMMARY
     H*
     H*  09 - Reserved for maintenance/inquiry conditions
     H*  31 - 37, Iindicators used as error indicators for Missing Reasons
     H*  71 - These indicators are to used as error indicators for
     H* thru  the various screens. They will set off after
     H*  89 - displaying each input capible screen.
     H*  90 - Used to general purpose error indicator
     H*  99 - Used when chaining to file being updated
     H*
     H****************************************************************
     H*
     H*  MAINLINE (INTERNAL) SUBROUTINE REFERENCE SUMMARY
     H*
     H*  FINISH - Final processing for file update/display
     H*  PREADD - Pre-processing before add process
     H*  PRECHG - Pre-processing before change process
     H*  PREDLT - Pre-processing before delete process
     H*  PREDSP - Pre-processing before display process
     H*  XSCR02 - Display second screen and edit check
     H*  XSCR03 - Display third screen and edit check
     H*
     H****************************************************************
     H*  MAINLINE (EXTERNAL) SUBROUTINE REFERENCE SUMMARY
     H*
     H*  DLTPRE - Used for any processing before delete occurs
     H*  DSPLYR - After displaying information
     H*  ERRCK1 - Used to determine errors on the first screen
     H*  ERRCK2 - Used to determine errors on the second screen
     H*  ERRCK3 - Used to determine errors on the third screen
     H*  MVDBSC - Used to move data base fields to screen fields
     H*  MVSCDB - Used to move screen fields to data base fields
     H*  OTHERR - Used for processing other modes
     H*  PGMEND - Used for any end of program processing
     H*  PGMSTR - Used for any start of program processing
     H*  PREOTH - Pre-processing for other modes
     H*  UPDATR - Used to update data base record
     H*  WRITER - Used to write data base record
     H*
     H****************************************************************
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW1551F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     FWMRFP     UF A E             DISK     RENAME(RFP01:DBRCDF)
     FWMRFL     IF   E           K DISK
     F                                     INFDS(RFPDS)
     FWMUPL     IF   E           K DISK
     F***WDELP     UF A E             DISK     RENAME(PEL01:DBRCDF)
      *
     D COMAPI          C                   CONST('WA001R')
     D OPTNM           C                   CONST('Scheduled Rating Inf-
     D                                     ormation')
      *
     D RFPDS           DS
     D  RFRRN                397    400B 0

      ***** BEG ADD ***** 04/13/07 *********************************************
      * Compare fields for screen - SCR01
     D xxscr01         DS
     D  SCPRCA
     D  SCPRDA
     D  SCPRRS                             LIKE(RFPRRS)
     D  SCCPCA
     D  SCCPDA
     D  SCCPRS                             LIKE(RFCPRS)
     D  SCMFCA
     D  SCMFDA
     D  SCMFRS                             LIKE(RFMFRS)
     D  SCSDCA
     D  SCSDDA
     D  SCSDRS                             LIKE(RFSDRS)
     D  SCEMCA
     D  SCEMDA
     D  SCEMRS                             LIKE(RFEMRS)
      * Compare fields for screen - SCR02
     D xxscr02         DS
     D  SCMCCA
     D  SCMCDA
     D  SCMCRS                             LIKE(RFMCRS)
     D  SCMSCA
     D  SCMSDA
     D  SCMSRS                             LIKE(RFMSRS)
     D  SCDBCR
      * Compare fields for screen - SCR02
     D xxscr03         DS
     D  SCLSCF
     D  SCLSDT
     D  SCPEFL
     D  SCPEEM
     D  SCCOM1
     D  SCCOM2
     D  SCCOM3
      * Compare fields for saved file fields - SCR01
     D zzscr01         DS
     D  ZZPRCA                             LIKE(RFPRCA)
     D  ZZPRDA                             LIKE(RFPRDA)
     D  ZZPRRS                             LIKE(RFPRRS)
     D  ZZCPCA                             LIKE(RFCPCA)
     D  ZZCPDA                             LIKE(RFCPDA)
     D  ZZCPRS                             LIKE(RFCPRS)
     D  ZZMFCA                             LIKE(RFMFCA)
     D  ZZMFDA                             LIKE(RFMFDA)
     D  ZZMFRS                             LIKE(RFMFRS)
     D  ZZSDCA                             LIKE(RFSDCA)
     D  ZZSDDA                             LIKE(RFSDDA)
     D  ZZSDRS                             LIKE(RFSDRS)
     D  ZZEMCA                             LIKE(RFEMCA)
     D  ZZEMDA                             LIKE(RFEMDA)
     D  ZZEMRS                             LIKE(RFEMRS)
      * Compare fields for screen - SCR02
     D zzscr02         DS
     D  ZZMCCA                             LIKE(RFMCCA)
     D  ZZMCDA                             LIKE(RFMCDA)
     D  ZZMCRS                             LIKE(RFMCRS)
     D  ZZMSCA                             LIKE(RFMSCA)
     D  ZZMSDA                             LIKE(RFMSDA)
     D  ZZMSRS                             LIKE(RFMSRS)
     D  ZZDBCR                             LIKE(SCDBCR)
      * Compare fields for screen - SCR02
     D zzscr03         DS
     D  ZZLSCF                             LIKE(RFLSCF)
     D  ZZLSDT                             LIKE(SCLSDT)
     D  ZZPEFL                             LIKE(RFPEFL)
     D  ZZPEEM                             LIKE(RFPEEM)
     D  ZZCOM1                             LIKE(RFCOM1)
     D  ZZCOM2                             LIKE(RFCOM2)
     D  ZZCOM3                             LIKE(RFCOM3)
      ***** END ADD ***** 04/13/07 *********************************************

     D @@KC            s                   like(*INKC)
     D @@KL            s                   like(*INKL)
      ***** BEG 04/04/07 ********+++++++++**********************************
     D XXQTE           S              1
     D QQQTE           S              1    INZ
      ***** END 04/04/07 ********++++++++++*********************************
      ***** BEG ADD ***** 07/13/07 **+++++++++**********************************
     D TOTPCC          s                   like(RFMSCA)                         CREDIT APPLIED
     D TOTPCD          s                   like(RFMSCA)                         DEBIT APPLIED
     D TOTALC          s                   like(SCDBCR)                         TOT CRDT APPLIED
     D TOTALD          s                   like(SCDBCR)                         TOT DEBIT APPLIED
     D TOTPCT          s                   like(SCDBCR)                         CATEGORY DB/CR
      ***** END ADD ***** 07/13/07 +++++++++**********************************
     C*
      /copy scopymt01c
     C*
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
      *
      *
      * Both can not be entered.
     C  N90SCPRDA        IFNE      *ZERO
     C     SCPRCA        ANDNE     *ZERO
     C                   SETON                                          7090
     C                   ENDIF
      *
      * Value can not exceed range.
     C  N90              SELECT
     C                   when      SCPRDA < 0 or SCPRDA > 10
     C                   SETON                                          7190
     C                   when      SCPRCA < 0 or SCPRCA > 10
     C                   SETON                                          7290
     C                   ENDSL
      ***** BEG ADD ***** 04/13/07 *********************************************
      * If either factor not = zero, must enter the Factor Reason Description
     C  N90              IF        SCPRDA > 0  AND SCPRR1 = *BLANKS
     C                             OR SCPRCA > 0 AND SCPRR1 = *BLANKS
     C                   SETON                                          3190
     C                   ENDIF
      ***** END ADD ***** 04/13/07 *********************************************
      *
      * Both can not be entered.
     C  N90SCCPDA        IFNE      *ZERO
     C     SCCPCA        ANDNE     *ZERO
     C                   SETON                                          7390
     C                   ENDIF
      * Value can not exceed range.
     C  N90              SELECT
     C                   when      SCCPDA < 0 or SCCPDA > 10
     C                   SETON                                          7490
     C                   when      SCCPCA < 0 or SCCPCA > 10
     C                   SETON                                          7590
     C                   ENDSL
      ***** BEG ADD ***** 04/13/07 *********************************************
      * If either factor not = zero, must enter the Factor Reason Description
     C  N90              IF        SCCPDA > 0  AND SCCPR1 = *BLANKS
     C                             OR SCCPCA > 0 AND SCCPR1 = *BLANKS
     C                   SETON                                          3290
     C                   ENDIF
      ***** END ADD ***** 04/13/07 *********************************************
      *
      * Both can not be entered.
     C  N90SCMFDA        IFNE      *ZERO
     C     SCMFCA        ANDNE     *ZERO
     C                   SETON                                          7690
     C                   ENDIF
      * Value can not exceed range.
     C  N90              SELECT
     C                   when      SCMFDA < 0 or SCMFDA > 5
     C                   SETON                                          7790
     C                   when      SCMFCA < 0 or SCMFCA > 5
     C                   SETON                                          7890
     C                   ENDSL
      *
      ***** BEG ADD ***** 04/13/07 *********************************************
      * If either factor not = zero, must enter the Factor Reason Description
     C  N90              IF        SCMFDA > 0  AND SCMFR1 = *BLANKS
     C                             OR SCMFCA > 0 AND SCMFR1 = *BLANKS
     C                   SETON                                          3390
     C                   ENDIF
      ***** END ADD ***** 04/13/07 *********************************************
      * Both can not be entered.
     C  N90SCSDDA        IFNE      *ZERO
     C     SCSDCA        ANDNE     *ZERO
     C                   SETON                                          7990
     C                   ENDIF
      * Value can not exceed range.
     C  N90              SELECT
     C                   when      SCSDDA < 0 or SCSDDA > 5
     C                   SETON                                          8090
     C                   when      SCSDCA < 0 or SCSDCA > 5
     C                   SETON                                          8190
     C                   ENDSL
      *
      ***** BEG ADD ***** 04/13/07 *********************************************
      * If either factor not = zero, must enter the Factor Reason Description
     C  N90              IF        SCSDDA > 0  AND SCSDR1 = *BLANKS
     C                             OR SCSDCA > 0 AND SCSDR1 = *BLANKS
     C                   SETON                                          3490
     C                   ENDIF
      ***** END ADD ***** 04/13/07 *********************************************
      *
      * Both can not be entered.
     C  N90SCEMDA        IFNE      *ZERO
     C     SCEMCA        ANDNE     *ZERO
     C                   SETON                                          8290
     C                   ENDIF
      * Value can not exceed range.
     C  N90              SELECT
     C                   when      SCEMDA < 0 or SCEMDA > 10
     C                   SETON                                          8390
     C                   when      SCEMCA < 0 or SCEMCA > 10
     C                   SETON                                          8490
     C                   ENDSL
      *
      ***** BEG ADD ***** 04/13/07 *********************************************
      * If either factor not = zero, must enter the Factor Reason Description
     C  N90              IF        SCEMDA > 0  AND SCEMR1 = *BLANKS
     C                             OR SCEMCA > 0 AND SCEMR1 = *BLANKS
     C                   SETON                                          3590
     C                   ENDIF
      ***** END ADD ***** 04/13/07 *********************************************
      *
      * Add up percentages to compare to Rate factor entered.
      *
     C                   Z-ADD     *ZERO         TOTPCT
      ***** BEG ADD ***** 07/13/07 **+++++++++**********************************
     C                   Z-ADD     *ZERO         TOTPCC
     C                   Z-ADD     *ZERO         TOTPCD
      ***** END ADD ***** 07/13/07 +++++++++**********************************
     C                   ADD       SCPRDA        TOTPCD
     C                   ADD       SCPRCA        TOTPCC
     C                   ADD       SCCPDA        TOTPCD
     C                   ADD       SCCPCA        TOTPCC
     C                   ADD       SCMFDA        TOTPCD
     C                   ADD       SCMFCA        TOTPCC
     C                   ADD       SCSDDA        TOTPCD
     C                   ADD       SCSDCA        TOTPCC
     C                   ADD       SCEMDA        TOTPCD
     C                   ADD       SCEMCA        TOTPCC
      *
     C                   ADD       SCMCDA        TOTPCD
     C                   ADD       SCMCCA        TOTPCC
     C                   ADD       SCMSDA        TOTPCD
     C                   ADD       SCMSCA        TOTPCC

      ***** BEG ADD ***** 04/13/07 *********************************************
      *  Setup for changes in Screen ONE
     C                   MOVEl(p)  SCPRR1        SCPRRS
     C                   MOVE      SCPRR1        SCPRRS
     C                   MOVEl(p)  SCCPR1        SCCPRS
     C                   MOVE      SCCPR1        SCCPRS
     C                   MOVEl(p)  SCMFR1        SCMFRS
     C                   MOVE      SCMFR1        SCMFRS
     C                   MOVEl(p)  SCSDR1        SCSDRS
     C                   MOVE      SCSDR1        SCSDRS
     C                   MOVEl(p)  SCEMR1        SCEMRS
     C                   MOVE      SCEMR1        SCEMRS
      ***** END ADD ***** 04/13/07 *********************************************
      *
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
      * Both can not be entered.
     C  N90SCMCDA        IFNE      *ZERO
     C     SCMCCA        ANDNE     *ZERO
     C                   SETON                                          8590
     C                   ENDIF
      * Value can not exceed range.
     C  N90              SELECT
     C                   when      SCMCDA < 0 or SCMCDA > 5
     C                   SETON                                          8690
     C                   when      SCMCCA < 0 or SCMCCA > 5
     C                   SETON                                          8790
     C                   ENDSL
      *
      ***** BEG ADD ***** 04/13/07 *********************************************
      * If either factor not = zero, must enter the Factor Reason Description
     C  N90              IF        SCMCDA > 0  AND SCMCR1 = *BLANKS
     C                             OR SCMCCA > 0 AND SCMCR1 = *BLANKS
     C                   SETON                                          3690
     C                   ENDIF
      ***** END ADD ***** 04/13/07 *********************************************
      * Both can not be entered.
     C  N90SCMSDA        IFNE      *ZERO
     C     SCMSCA        ANDNE     *ZERO
     C                   SETON                                          8890
     C                   ENDIF
      * Value can not exceed range.
     C  N90              SELECT
     C                   when      SCMSDA < 0 or SCMSDA > 5
     C                   SETON                                          8990
     C                   when      SCMSCA < 0 or SCMSCA > 5
     C                   SETON                                          6990
     C                   ENDSL
      *
      ***** BEG ADD ***** 04/13/07 *********************************************
      * If either factor not = zero, must enter the Factor Reason Description
     C  N90              IF        SCMSDA > 0  AND SCMSR1 = *BLANKS
     C                             OR SCMSCA > 0 AND SCMSR1 = *BLANKS
     C                   SETON                                          3790
     C                   ENDIF
      ***** END ADD ***** 04/13/07 *********************************************
      *
      * Add up percentages to compare to Rate factor entered.
      *
      ***** BEG ADD ***** 07/13/07 **+++++++++**********************************
     C                   CLEAR                   TOTPCC
     C                   CLEAR                   TOTPCD
     C                   CLEAR                   TOTALC
     C                   CLEAR                   TOTALD
     C                   CLEAR                   TOTPCT
      ***** end ADD ***** 07/13/07 **+++++++++**********************************
      *
     C                   ADD       SCPRDA        TOTPCD
     C                   ADD       SCPRCA        TOTPCC
     C                   ADD       SCCPDA        TOTPCD
     C                   ADD       SCCPCA        TOTPCC
     C                   ADD       SCMFDA        TOTPCD
     C                   ADD       SCMFCA        TOTPCC
     C                   ADD       SCSDDA        TOTPCD
     C                   ADD       SCSDCA        TOTPCC
     C                   ADD       SCEMDA        TOTPCD
     C                   ADD       SCEMCA        TOTPCC
      *
     C                   ADD       SCMCDA        TOTPCD
     C                   ADD       SCMCCA        TOTPCC
     C                   ADD       SCMSDA        TOTPCD
     C                   ADD       SCMSCA        TOTPCC
      *
      ***** BEG ADD ***** 07/13/07 **+++++++++**********************************
     C                   EVAL      TOTALC = (TOTPCC * -1) / 100
     C                   EVAL      TOTALD = (TOTPCD / 100)
     C                   IF        SCDBCR <> *ZEROS
     C                   EVAL      TOTPCT = 1.000 + TOTALC + TOTALD
     C                   ELSE
     C                   EVAL      TOTPCT = TOTALC + TOTALD
     C                   ENDIF                                                  SCDBCR = *ZEROS
      *
      * Check TOTPCT = SCDBCR, if not is Error
     C  N90              If        TOTPCT <> SCDBCR
     C                   SETON                                        39  90
     C                   EVAL      SCWPCT = TOTPCT
     C                   EVAL      SCWDBC = SCDBCR
     C                   EXFMT     WDWMSG
     C                   ENDIF                                                  TOTPCT<>SCDBCR
      ***** END ADD ***** 07/13/07 +++++++++**********************************
      *

      ***** BEG ADD ***** 04/13/07 *********************************************
      *  Check for changes in Screen TWO
     C                   MOVEl(p)  SCMCR1        SCMCRS
     C                   MOVE      SCMCR1        SCMCRS
     C                   MOVEl(p)  SCMSR1        SCMSRS
     C                   MOVE      SCMSR1        SCMSRS
      ***** END ADD ***** 04/13/07 *********************************************
      *
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
     C  N90SCLSCF        IFEQ      'Y'
     C                   Z-ADD     SCLSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      *ON
     C                   SETON                                        717490
     C                   ENDIF                                                  SRERR=*ON
     C                   ELSE
     C     SCLSDT        IFNE      *ZERO
     C                   SETON                                        707490
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90SCPEFL        IFEQ      'Y'
     C     SCPEEM        IFEQ      *ZERO
     C                   SETON                                        72  90
     C                   ENDIF
     C                   ELSE
     C     SCPEEM        IFNE      *ZERO
     C                   SETON                                        73  90
     C                   ENDIF
     C                   ENDIF

      ***** BEG DEL ***** 07/13/07 **+++++++++**********************************
      ***** BEG ADD ***** 04/13/07 *********************************************
      * If User presses F21, check TOTPCT = SCDBCR, if not is Error
     C**********         If        not *in90  AND *INKV = *ON AND
     C**********                   TOTPCT <> SCDBCR
     C**********         if        scDBCR <> totPct / 100
     C**********         SETON                                        39  90
     C**********         ENDIF                                                  TOTPCT<>SCDBCR
     C********           ENDIF                                                  Not *in90
      ***** END ADD ***** 04/13/07 *********************************************
      ***** END DEL ***** 07/13/07 +++++++++**********************************

     C                   ENDSR                                                  ERRCK3
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C                   Z-ADD     RFPRCA        SCPRCA
     C                   Z-ADD     RFPRDA        SCPRDA
     C                   MOVE      RFPRRS        SCPRR2
     C                   MOVEL     RFPRRS        SCPRR1
     C                   Z-ADD     RFCPCA        SCCPCA
     C                   Z-ADD     RFCPDA        SCCPDA
     C                   MOVE      RFCPRS        SCCPR2
     C                   MOVEL     RFCPRS        SCCPR1
     C                   Z-ADD     RFMFCA        SCMFCA
     C                   Z-ADD     RFMFDA        SCMFDA
     C                   MOVE      RFMFRS        SCMFR2
     C                   MOVEL     RFMFRS        SCMFR1
     C                   Z-ADD     RFSDCA        SCSDCA
     C                   Z-ADD     RFSDDA        SCSDDA
     C                   MOVE      RFSDRS        SCSDR2
     C                   MOVEL     RFSDRS        SCSDR1
     C                   Z-ADD     RFEMCA        SCEMCA
     C                   Z-ADD     RFEMDA        SCEMDA
     C                   MOVE      RFEMRS        SCEMR2
     C                   MOVEL     RFEMRS        SCEMR1
     C                   Z-ADD     RFMCCA        SCMCCA
     C                   Z-ADD     RFMCDA        SCMCDA
     C                   MOVE      RFMCRS        SCMCR2
     C                   MOVEL     RFMCRS        SCMCR1
     C                   Z-ADD     RFMSCA        SCMSCA
     C                   Z-ADD     RFMSDA        SCMSDA
     C                   MOVE      RFMSRS        SCMSR2
     C                   MOVEL     RFMSRS        SCMSR1
     C                   MOVE      RFLSCF        SCLSCF
     C                   Z-ADD     RFLSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCLSDT
     C                   MOVE      RFPEFL        SCPEFL
     C                   Z-ADD     RFPEEM        SCPEEM
     C                   MOVE      RFCOM1        SCCOM1
     C                   MOVE      RFCOM2        SCCOM2
     C                   MOVE      RFCOM3        SCCOM3

      ***** BEG ADD ***** 04/13/07 *********************************************
     C                   IF        XX1STM  <> 'Y'
     C                   MOVE      'Y'           XX1STM            1
     C                   Z-ADD     RFPRCA        ZZPRCA
     C                   Z-ADD     RFPRDA        ZZPRDA
     C                   MOVEL     RFPRRS        ZZPRRS
     C                   Z-ADD     RFCPCA        ZZCPCA
     C                   Z-ADD     RFCPDA        ZZCPDA
     C                   MOVEL     RFCPRS        ZZCPRS
     C                   Z-ADD     RFMFCA        ZZMFCA
     C                   Z-ADD     RFMFDA        ZZMFDA
     C                   MOVEL     RFMFRS        ZZMFRS
     C                   Z-ADD     RFSDCA        ZZSDCA
     C                   Z-ADD     RFSDDA        ZZSDDA
     C                   MOVEL     RFSDRS        ZZSDRS
     C                   Z-ADD     RFEMCA        ZZEMCA
     C                   Z-ADD     RFEMDA        ZZEMDA
     C                   MOVEL     RFEMRS        ZZEMRS
     C                   Z-ADD     RFMCCA        ZZMCCA
     C                   Z-ADD     RFMCDA        ZZMCDA
     C                   MOVEL     RFMCRS        ZZMCRS
     C                   Z-ADD     RFMSCA        ZZMSCA
     C                   Z-ADD     RFMSDA        ZZMSDA
     C                   MOVEL     RFMSRS        ZZMSRS
     C                   MOVE      RFLSCF        ZZLSCF
     C                   MOVEL     SRDTO         ZZLSDT
     C                   MOVE      RFPEFL        ZZPEFL
     C                   Z-ADD     RFPEEM        ZZPEEM
     C                   MOVE      RFCOM1        ZZCOM1
     C                   MOVE      RFCOM2        ZZCOM2
     C                   MOVE      RFCOM3        ZZCOM3
     C                   ENDIF                                                  XX1STM <> "Y"
      ***** END ADD ***** 04/13/07 *********************************************
      *
     C                   ENDSR                                                  MVDBSC
      *
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
      *
     C                   SELECT
     C     ENCAR1        WHENEQ    10975
     C                   MOVE      '1'           RFCOFL
     C     ENCAR1        WHENEQ    22713
     C                   MOVE      '2'           RFCOFL
     C     ENCAR1        WHENEQ    36188
     C                   MOVE      '3'           RFCOFL
     C                   ENDSL
      *
     C                   Z-ADD     XXSREF        RFSREF
     C                   Z-ADD     XXSREX        RFSREX
     C                   Z-ADD     SCPRCA        RFPRCA
     C                   Z-ADD     SCPRDA        RFPRDA
     C                   MOVE      SCPRR2        RFPRRS
     C                   MOVEL     SCPRR1        RFPRRS
     C                   Z-ADD     SCCPCA        RFCPCA
     C                   Z-ADD     SCCPDA        RFCPDA
     C                   MOVE      SCCPR2        RFCPRS
     C                   MOVEL     SCCPR1        RFCPRS
     C                   Z-ADD     SCMFCA        RFMFCA
     C                   Z-ADD     SCMFDA        RFMFDA
     C                   MOVE      SCMFR2        RFMFRS
     C                   MOVEL     SCMFR1        RFMFRS
     C                   Z-ADD     SCSDCA        RFSDCA
     C                   Z-ADD     SCSDDA        RFSDDA
     C                   MOVE      SCSDR2        RFSDRS
     C                   MOVEL     SCSDR1        RFSDRS
     C                   Z-ADD     SCEMCA        RFEMCA
     C                   Z-ADD     SCEMDA        RFEMDA
     C                   MOVE      SCEMR2        RFEMRS
     C                   MOVEL     SCEMR1        RFEMRS
     C                   Z-ADD     SCMCCA        RFMCCA
     C                   Z-ADD     SCMCDA        RFMCDA
     C                   MOVE      SCMCR2        RFMCRS
     C                   MOVEL     SCMCR1        RFMCRS
     C                   Z-ADD     SCMSCA        RFMSCA
     C                   Z-ADD     SCMSDA        RFMSDA
     C                   MOVE      SCMSR2        RFMSRS
     C                   MOVEL     SCMSR1        RFMSRS
     C                   MOVE      SCLSCF        RFLSCF
     C                   Z-ADD     SCLSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RFLSDT
     C                   MOVE      SCPEFL        RFPEFL
     C                   Z-ADD     SCPEEM        RFPEEM
     C                   MOVE      SCCOM1        RFCOM1
     C                   MOVE      SCCOM2        RFCOM2
     C                   MOVE      SCCOM3        RFCOM3
      *
     C                   ENDSR                                                  MVSCDB
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
      *
      ***** BEG ADD ***** 04/13/07 *********************************************
      *  If any screen fields changed, & print flag is on, blank the print flag.
     C                   IF        RFPRTF = 'Y' AND XX1STM = 'Y'
     C                   SELECT
     C                   WHEN      XXSCR01 <> ZZSCR01
     C                   EVAL      RFPRTF = *BLANKS
     C                   EXSR      UPDATR
     C                   WHEN      XXSCR02 <> ZZSCR02
     C                   EVAL      RFPRTF = *BLANKS
     C                   EXSR      UPDATR
     C                   WHEN      XXSCR03 <> ZZSCR03
     C                   EVAL      RFPRTF = *BLANKS
     C                   EXSR      UPDATR
     C                   ENDSL
     C                   ENDIF                                                  END PRPRTF = "Y"
      ***** END ADD ***** 04/13/07 *********************************************
      *
      ******* beg add ****** 10/27/06 *********************************
     c                   eval      *inlr = *on
      ******* end add ****** 10/27/06 *********************************
     C*
     C                   ENDSR                                                  PGMEND
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C                   MOVEL     Q1USER        SCUSER           10
     C                   MOVE      *OFF          *INKL
     C                   MOVE      *OFF          OUTFLG            1
     C                   MOVE      *OFF          DSPTYP            1
      *
      ******* beg del ****** 10/27/06 *********************************
     C*******            clear                   RFRRN
     C*****RFPKY1        CHAIN(N)  WMRFL                              99
     C******IN99         IFEQ      *ON
     C*******            eval      *in09 = @@mode = 'D'
     C*******            MOVE      'A'           @@MODE
     C*******            else
     C*******            eval      @@rrn = RFRRN
     C*******            END
      ******* end del ****** 10/27/06 *********************************
      ******* beg add ****** 10/27/06 *********************************
      *
     c                   clear                   @@rrn
     c                   clear                   RFRRN
     c     rfpky1        chain     wmrfl
      *
     c                   if        %found
     c                   eval      @@rrn = RFRRN
      *
     c                   else
      *
     c                   if        @@mode = 'C'
     c                   eval      @@mode = 'A'
     c                   endif
      *
     c                   endif
      *
      ******* end add ****** 10/27/06 *********************************
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C                   Z-ADD     3             #OFSCR            1 0
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
      *
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
     C     PREOTH        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     WMRFP                              60
     C*
B02  C     *IN60         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        RFUSER
     C                   MOVEL     Q1PGM         RFPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RFDT
     C                   MOVEL     DSTIME        RFTIME
     C*
     C                   UPDATE    dbRcdF
     C*
E02  C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   dbRcdF
     C*
     C                   Z-ADD     XXCO#         RFCO#
     C                   Z-ADD     XXFND         RFFND
     C                   Z-ADD     XXEMP#        RFEMP#
     C                   Z-ADD     XXDIV         RFDIV
     C                   Z-ADD     XXFYR         RFFYR
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        RFIUSR
     C                   MOVEL     Q1USER        RFUSER
     C                   MOVEL     Q1PGM         RFIPGM
     C                   MOVEL     Q1PGM         RFPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RFIDT
     C                   MOVEL     SRDTO         RFDT
     C                   MOVEL     DSTIME        RFTIME
     C*
     C                   WRITE     dbRcdF
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  WRITER
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKV         CASEQ     *ON           KVDO
     C                   endcs
     C*
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* KVDO - SUBROUTINE FOR PRINT
     C*===============================================================
     C*
     C*  F21=PRINT SCHECULE
     C*
     C     KVDO          BEGSR
     C*
B03  C                   if        not *in90 and @@mode <> 'D'
     C                   exsr      ErrCk3
B03  C                   if        not *in90
     C                   exsr      finish
     C                   eval      @@mode = 'C'
E03  C                   endif
E03  C                   endif
     C*
     C                   if        not *in90
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      XXFND         QQFND             3
     C                   MOVE      XXFYR         QQFYR             3
     C                   MOVE      XXEMP#        QQEMP#            9
     C                   MOVE      XXDIV         QQDIV             5
      ***** BEGIN ADD ******* 04/04/07 *************************************
     C                   MOVE      XXQTE         QQQTE
      ***** END   ADD ******* 04/04/07 *************************************
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   CALL      'W1551C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQOUTQ
      ***** BEG 04/04/07 ********+++++++++++++******************************
     C                   PARM                    QQQTE
      ***** END 04/04/07 ********+++++++++++++******************************
      *
     C                   call      'WA007R'
     C*
     C                   eval      *in90 = *on
     C*
     C                   endif
      *
     C                   ENDSR                                                  KVDO
      *
     C*===============================================================
     C* *INZSR - INITIALIZE
     C*===============================================================
     C*
     C     *INZSR        BEGSR
     C*
     C     *LIKE         DEFINE    RFCO#         XXCO#
     C     *LIKE         DEFINE    RFFND         XXFND
     C     *LIKE         DEFINE    RFEMP#        XXEMP#
     C     *LIKE         DEFINE    RFDIV         XXDIV
     C     *LIKE         DEFINE    RFSREF        XXSREF
     C     *LIKE         DEFINE    RFSREX        XXSREX
     C     *LIKE         DEFINE    RFFYR         XXFYR
     C     *LIKE         DEFINE    RFCPCA        XXDBCR
     C     *LIKE         DEFINE    RFSREF        SYSDT
     C     *LIKE         DEFINE    *IN90         ERROR
     C                   Z-ADD     *ZERO         @@RRN             9 0          wdelp
     C*
     C* ENTRY LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    XXEMP#
     C                   PARM                    XXDIV
     C                   PARM                    XXFYR
     C                   PARM                    ENCAR1            5 0
     C                   PARM                    @@DBCR            5 4
     C                   PARM                    XXSREF
     C                   PARM                    XXSREX
     C                   PARM                    @@MODE            1
     C                   PARM                    SCCONM
     C                   PARM                    SCFND
     C                   PARM                    SCNAME
     C                   PARM                    SCEDN
     C                   PARM      *IN90         ERROR
     C                   PARM      *INKC         @@KC
     C                   PARM      *INKL         @@KL
      ******* Begin Add  ******* 04/04/07 **********************************
     C                   PARM                    XXQTE
      ******* End   Add  ******* 04/04/07 **********************************
      *
      ***** BEG ADD ***** 04/13/07 *********************************************
     C                   Z-ADD     @@DBCR        SCDBCR
      ***** END ADD ***** 04/13/07 *********************************************
      *
      *================================================================
      *  KLIST definitions
      *================================================================
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *  Scheduled Rating
      *
     C     RFPKY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
     C                   ENDSR
