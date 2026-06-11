     HCopyright('(c) 2019 Sapiens International. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H*** Begin Change *** 11/21/19 ******************************
     H**dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller) bnddir('WTBNDDIR')
     H*** End   Change *** 11/21/19 ******************************
     H/define CONVERT
     H*
     H/TITLE W0160R - Prompt for Cancellation notice workfile build   *
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* This program prompts the user for information necessary to    *
     H* build a Cancellation Batch.  The screen requires entry of     *
     H* Account Type (E = Estimating, S = Self reporting, B = Both)   *
     H* and Cancellation Reason Code.  The user must also enter       *
     H* criteria to select accounts on the basis of either A/R        *
     H* Balance or missing Self-reporting monthly reports.  For A/R   *
     H* Balance analysis, the screen requires entry of a Policy       *
     H* Effective Date range, Balance Date, and Minimum Balance.      *
     H* A/R Balance analysis can be used for any Account Type.  For   *
     H* selection based on missing S/R reports, the screen requires   *
     H* entry of a Reporting Month/Century/Year.  The user may also   *
     H* select exclusion of Policies which begin after a specified    *
     H* day of the specified reporting Month.  The screen also allows *
     H* the user to enter comments which will print on each           *
     H* Cancellation Notice produced by the Issue Mass Cancellation   *
     H* process.                                                      *
     H*                                                               *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*                                                               *
     H*   (MENU)                                                      *
     H*                                                               *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H*   W0160C - Build Cancellation Notice Batch Submitter          *
     H*   W2196R - Cancellation Reason Search/Select                  *
     H*   M0006C - Online Help                                        *
     H*   W0006R - Change User Parameters                             *
     H*   SRCENTER - Center Alpha Field                               *
     H*   SRCVTDAT - Date Convert & Validate                          *
     H*   SRCVTMY  - Month/Year Convert & Validate                    *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 02/17/21 C:300 Support: 83101                                 *
     H*                Cancellation Scriptura Form Change Needed      *
     H*                Populate Reporting Month/Year from Week Ending Date
     H*                                                               *
     H* 06/24/20 C:180 PROJECT WC2710                                 *
     H*                Display File changes only:                     *
     H*                   Change default text for the                 *
     H*                   inclusion/exclusion dropdown                *
     H*                                                               *
     H* 11/21/19 C:180 PROJECT 102WIC                                 *
     H*                Add State Inclusion/Exclusion Dropdown.        *
     H*                                                               *
     H* 09/02/05 C:61  PROJECT WC1359 129WLT                          *
     H*                Add prompt to optionally include "Agency       *
     H*                Billed Employers".                             *
     H*                                                               *
     H* 03/07/05 C:136 PROJECT: WC1199 (REVAMP DUNNING II)            *
     H*                ADD FLAG TO INCLUDE DEPOSITS IN BALANCE CALC   *
     H*                AND PASS TO W0160C                             *
     H*                                                               *
     H* 10/27/03 C:33  Project: WC1132                                *
     H*                Revamp Dunning Process                         *
     H*                                                               *
     H* 10/23/03 C:61  PROJECT WC1098                                 *
     H*                Add prompt for "Include current accounts       *
     H*                 for prior-year missing reports (Y/ )".        *
     H*                                                               *
     H* 06/30/03 C:136 SUPPORT: WA045 37754                           *
     H*                Billing date and payment through date not      *
     H*                being cleared after reading previous batch     *
     H*                record (WXCCP).                                *
     H*                                                               *
     H* 05/02/03 C:61  PROJECT WC1108                                 *
     H*              - ADD "Percentage of premium"                    *
     H*                                                               *
     H* 02/18/03 C:136 PROJECT: WC1115 (PAYROLL/PREM EXPORT/IMPORT)   *
     H*                Modify prompt if vendor processing is          *
     H*                enabled to include report frequency and        *
     H*                report date for self-reporting employers.      *
     H*                                                               *
     H* 04/28/00  C:35   PROJECT: WC912                               *
     H*                  ADD EST/AUD FLAG TO ERROR CHECK FOR A/R      *
     H*                  BALANCE--MISSING REPORT CRITERIA             *
     H*                  EXCLUSIVITY.                                 *
     H*                                                               *
     H* 12/01/99  C:90  PROJECT: WC912                                *
     H*                 Add new field Estimated/Audited flag.         *
     H*                                                               *
     H* 10/21/99  C:87  PROJECT: WC577                                *
     H*                 Chg to use WTCRL1 instead of WTABL for the    *
     H*                 cancellation reason codes.                    *
     H*                                                               *
     H* 10/07/99  C:87  PROJECT: WC894                                *
     H*                 Replace 'Balance date' with 'Billings through *
     H*                 date' and 'Payments through date'.  Both      *
     H*                 fields are optional and will default to the   *
     H*                 current date if left blank.  Adjust parms for *
     H*                 call to W0160C.  Load Billing and Payment     *
     H*                 thru dts to CCBLDT and CCPTDT respectively.   *
     H*                                                               *
     H* 08/10/99  C:35   PROJECT: SUPPORT WA107                       *
     H*                  WHEN VALIDATING REASON CODE READ THROUGH ALL *
     H*                  MATCHING CODES UNTIL FAIL OR ACTIVE CODE IS  *
     H*                  FOUND.                                       *
     H*                                                               *
     H* 06/01/99  C:87  PROJECT: SUPP WA101 #17550                    *
     H*                 Restrict 'to date' to be less than or equal   *
     H*                 to run date.                                  *
     H*                                                               *
     H* 11/24/98  C:87  PROJECT: WC852                                *
     H*                 Add an optional entry for "Estimated issue    *
     H*                 date". Must be >= to current date.            *
     H*                                                               *
     H* 11/10/98  C:87  PROJECT: WC852                                *
     H*                 Add prompt for office # (optional) below      *
     H*                 reason code. Load to Canx batch file.         *
     H*                 Add processing for office # search.           *
     H*                                                               *
     H* 06/15/98  C:35  PROJECT: WC577                                *
     H*                 ADDED CODE TO PUT DATES IN CORRECT FORMAT     *
     H*                 AFTER F10=SEARCH IS USED.                     *
     H*                                                               *
     H* 10/14/96  C:57  PROJECT: CM024                                *
     H*                 GENERAL CLEANUP.                              *
     H*                 CONVERT TO COMMERCIAL BASIS. REMOVE FYR, ADD  *
     H*                 POLICY FROM/TO DATES, EXCLUDE ON/AFTER DAY.   *
     H*                 ADD BATCH FILE TO ALLOW MULTIPLE RUNS.        *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* GENERAL USAGE:                                                *
     H*                                                               *
     H*   60 - CHAIN INDICATOR                                        *
     H*                                                               *
     H* SCR01:                                                        *
     H*                                                               *
     H*   21 - INVALID ACCOUNT TYPE                                   *
     H*   22 - CANNOT SELECT BOTH A/R BAL AND MISSING MONTH CRITERIA  *
     H*   23 - MUST SELECT AT LEAST ONE SET OF CRITERIA               *
     H*   24 - MISSING MONTH CRITERIA IS ONLY VALID WITH ACCT TYPE 'S'*
     H*                                                               *
     H*   26 - INVALID 'FROM' POLICY EFFECTIVE DATE                   *
     H*   27 - INVALID 'TO' POLICY EFFECTIVE DATE                     *
     H*   28 - 'FROM' DATE CAN NOT BE GREATER THAN 'TO' DATE          *
     H*   29 - INVALID BALANCE DATE                                   *
     H*   30 - MINIMUM BALANCE MUST BE GREATER THAN 0                 *
     H*   31 - INVALID REPORTING MONTH/YEAR                           *
     H*   32 - INVALID DAY OF MONTH                                   *
     H*   33 - INVALID CANCELLATION REASON                            *
     H*   40 - INVALID PAYMENT THROUGH DATE                           *
     H*   71 - If "% Prem" entered, then must be less than 100%       *
     H*   72 - Min Bal and % Prem fields mutually exclusive.          *
     H*   73 - One of: "Min Bal" or "% Prem" fields must be entered.  *
     H*                                                               *
     H*****************************************************************
     H* FILE USAGE:                                                   *
     H*                                                               *
     H* DISPLAY FILES:                                                *
     H*                                                               *
     H*   W0160F - BUILD MASS CANCELLATION NOTICE PROMPT SCREEN       *
     H*                                                               *
     H* DATA FILES:                                                   *
     H*                                                               *
     H*   SMCOL    (PCO01): CO#                                       *
     H*   WMUPL    (PUP01): USER                                      *
     H*   WMFNL    (PFN01): CO#/FND                                   *
     H*   WXCCL    (PCC02): CO#/FND/BAT#/FYR                          *
     H*   WTABL    (PCR01): CO#/KEY                                   *
     H*                                                               *
     H*****************************************************************
      *
     FW0160F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWXCCL     IF A E           K DISK
     F                                     RENAME(PCC01:PCC02)
     F***  BEGIN CHG  ***  10/21/99  **********************************
     F***WTABL   IF  E           K        DISK
     F***            PCC01                             KIGNORE
     FWTCRL1    IF   E           K DISK
     F***  END   CHG  ***  10/21/99  **********************************
     F***  BEGIN ADD  ***  11/10/98  **********************************
     FWMOFL     IF   E           K DISK
     F***  END   ADD  ***  11/10/98  **********************************
      **** BEG   ADD **** 02/18/03 *********************************************
     FSSYLF     IF   E           K DISK
      **** END   ADD **** 02/18/03 *********************************************
      *
      *** BEG ADD *** 11/21/19 *************************************************
     FSTSTL5    IF   E           K DISK    USROPN
      *
     D/Copy sPrString
     d IntPos          s              5  0
     D ieSTcodes       s            155A   inz
      *** END ADD *** 11/21/19 *************************************************

     D OPTNM           C                   CONST('Build Mass Cancellat-
     D                                     ion Batch')
     D***  BEGIN CHG  ***  11/24/98  **********************************
     D*********** DS ***********************
     D**************************************  1  120DSDTTM
     D**************************************  1   60DSTIME
     D**************************************  7  120DSDATE
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D***  END   CHG  ***  11/24/98  **********************************
      *
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      *
     D                 DS
     D  XXDATE                 1      8  0
     D  XXDTMM                 1      2  0
     D  XXDTDD                 3      4  0
     D  XXDTCY                 5      8  0
      *
     D                 DS
     D  SCRPMY                 1      6  0
     D  XXRPMM                 1      2  0
     D  XXRPCY                 3      6  0
      *
     D                 DS
     D  QQCOM1                 1     60
     D  XXCOM1                60     60
      *
     D                 DS
     D  QQCOM2                 1     60
     D  XXCOM2                60     60
      *
      *** BEG ADD ***** 02/18/03 **********************************************
     D                 DS
     D  XXWEDT                 1      8  0
     D  XXWEMM                 1      2  0
     D  XXWEDD                 3      4  0
     D  XXWECY                 5      8  0
      *** END ADD ***** 02/18/03 **********************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *   KEY LIST FOR THE COMPANY FILE
      *

     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      *  KEY LIST FOR THE USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    XXUSER
      *
      *  KEY LIST FOR THE FUND MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      * KEY LIST FOR THE CANCELLATION SELECTION CRITERIA WORKFILE
      *
     C     CCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  KEY LIST FOR THE TABLE FILE
      *
     C***  BEGIN CHG  ***  10/21/99  **********************************
     C********** TBKY      KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXKEY
     C*
     C     CRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C                   KFLD                    KKEFDT
     C*
     C     CRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C***  END   CHG  ***  10/21/99  **********************************
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C     OFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCOFF#
     C***  END   ADD  ***  11/10/98  **********************************
      *
      **** BEG   ADD **** 02/18/03 *********************************************
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      **** END   ADD **** 02/18/03 *********************************************
      *****************************************************************
      *
     C     *LIKE         DEFINE    CRKEY         XXKEY
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    Q1USER        XXUSER
     C     *LIKE         DEFINE    FNFND         XXFND
     C     *LIKE         DEFINE    Q1USER        FILNAM
     C     *LIKE         DEFINE    Q1USER        LIBNAM
     C     *LIKE         DEFINE    Q1USER        MBRNAM
     C     *LIKE         DEFINE    CCBAT#        XXBAT#
     C***  BEGIN ADD  ***  10/21/99  **********************************
     C     *LIKE         DEFINE    CREFDT        KKEFDT
     C***  END   ADD  ***  10/21/99  **********************************
      *
     C                   Z-ADD     0             JUNK              2 0
     C                   Z-ADD     0             REM               1 0
      *
      *****************************************************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     STRT01        TAG
      *
      *  GET THE USER PARAMETER FILE
      *
     C                   MOVE      Q1USER        XXUSER
      *
     C     UPKY          CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      *ON
      *
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPFND         XXFND
      *
     C     FNKY          CHAIN     PFN01                              60
      *
     C                   MOVEL     FNNAME        SCNAME
      *
      *  GET THE COMPANY RECORD
      *
     C     COKY          CHAIN     PCO01                              60
      *
     C                   MOVEL(P)  CONAME        SRDFLD
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
      *
     C                   MOVEL     SRDFLD        SCCONM
      *
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVEL     OPTNM         SCPRDS
      ***** BEG ADD ******* 05/02/03 **********************************
      *
      * Center Screen Prompt in display field
      *
     C                   MOVEL(P)  SCPRDS        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
      ***** END ADD ******* 05/02/03 **********************************
      *
      *   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
      *** BEG ADD **** 02/18/03 **************************************
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL23        IFEQ      'Y'
     C                   SETON                                        43
     C                   ELSE
     C                   SETOFF                                       43
     C                   ENDIF                                                  WCFL23 IFEQ
      *** END ADD **** 02/18/03 **************************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCETYP                         TYPE OF ACCOUNT
     C                   MOVE      *ZERO         SCDATE                         BALANCE DATE
     C***  Begin Chg  ***  10/27/03  **************************************
     C*****BEGIN ADD  ***  10/07/99  **********************************
     C**********           MOVE *ZERO     SCPYDT           PAYMENTS THRU DT
     C*****END** ADD  ***  10/07/99  **********************************
     C                   MOVE      *ZERO         SCPYDT            8 0          PAYMENTS THRU DT
     C***  End   Chg  ***  10/27/03  **************************************
      ***** BEG CHG ******* 05/02/03 **********************************
     C*********            MOVE *ZERO     SCMIN            MINIMUM BALANCE
     C                   Z-ADD     *ZERO         SCMIN                          MINIMUM BALANCE
      ***** END CHG ******* 05/02/03 **********************************
     C                   MOVE      *ZERO         SCRPMY                         REPORTING MONTH/YEAR
     C                   MOVE      *BLANK        SCRSN                          REASON CODE
     C                   MOVE      *BLANK        SCCOM1                         COMMENT 1
     C                   MOVE      *BLANK        SCCOM2                         COMMENT 2
     C                   MOVE      *ZERO         SCFRDT                         POL EFF FROM DT
     C                   MOVE      *ZERO         SCTODT                         POL EFF TO DT
     C                   MOVE      *ZERO         SCDAY                          EXCL NEW ACCTS FR DA
      **** BEGIN ADD **** 12/01/99 ************************************
     C                   MOVE      *BLANK        SCEAFL                         EST/AUDIT FLAG
      **** END   ADD **** 12/01/99 ************************************
      /Free
       //**** Begin Add 11/21/19 **************************************
        MsStCodIE = *blanks;
        ErStCodIE = *Off;
        LoadSTcodes();
       //**** End   Add 11/21/19 **************************************
      /End-Free
      *
      *   DISPLAY ALL FORMATS
      *
     C     STRT          TAG
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF                                                  Q2RECF IFEQ
      ***** BEG ADD ******* 09/02/05 **********************************
      *
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   ENDIF                                                  Q2RECF IFEQ
      ***** END ADD ******* 09/02/05 **********************************
      *
      *   PROCESS ALL COMMAND KEYS
      *
     C     *IN11         IFEQ      *ON
      *
      *   F3=EXIT
      *
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC IFEQ
      *
      **** BEGIN ADD **** 06/15/98 *********************************************
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
      *
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C*
     C***  Begin Del  ***  10/27/03  **************************************
     C*****BEGIN ADD  ***  10/07/99  **********************************
     C**********           Z-ADDSCPYDT    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********           MOVELSRDTO     SCPYDT
     C*****END** ADD  ***  10/07/99  **********************************
     C***  End   Del  ***  10/27/03  **************************************
      *
     C                   Z-ADD     SCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     C                   MOVEL     SRDTO         SCRPMY
      **** END   ADD **** 06/15/98 *********************************************
      *
     C***  BEGIN ADD  ***  02/18/03  **********************************
     C                   Z-ADD     SCWEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCWEDT
      *
     C                   Z-ADD     SCXADT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCXADT
     C***  END   ADD  ***  02/18/03  **********************************
      *
      *   F10=SEARCH
      *
     C     *INKJ         IFEQ      *ON
      *
     C     HFLD          IFEQ      'SCRSN'
      *
     C                   MOVEL(P)  SCRSN         QQKEY
      *
     C                   CALL      'W2196R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQKEY            10
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         IFEQ      *ON
     C     QQKEY         OREQ      *BLANKS
     C                   MOVE      *ON           *IN34
      *
     C                   ELSE
      *
     C***  BEGIN CHG  ***  11/10/98  **********************************
     C*******************  MOVE *ON       *IN35
     C                   MOVE      *ON           *IN36
     C***  BEGIN CHG  ***  11/10/98  **********************************
      *
     C                   ENDIF                                                  *INKL IFEQ
      *
     C                   MOVEL     QQKEY         SCRSN
     C                   MOVEL(P)  'SCR01'       Q2RECF
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C     HFLD          IFEQ      'SCOFF#'
     C*
     C                   CALL      'W0002R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##OFF#            5
     C*
     C     ##OFF#        IFNE      *BLANKS
     C*
     C                   MOVE      *ON           *IN35
     C                   MOVEL     ##OFF#        SCOFF#
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN36
     C*
     C                   ENDIF                                                  ##OFF#<>*BLANKS
     C*
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR02'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   ENDIF                                                  HFLD='SCOFF#'
     C*
     C                   GOTO      STRT
     C***  END   ADD  ***  11/10/98  **********************************
      *
     C                   ENDIF                                                  *INKJ IFEQ
      *
      *   PROCESS HELP REQUESTS
      *
     C     *IN10         IFEQ      *ON
      *
     C                   MOVE      Q2RECF        XXRECF
     C                   MOVEL     'QHLP    '    FILNAM
     C                   MOVEL     '*LIBL   '    LIBNAM
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM
      *
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
      *
     C                   MOVE      XXRECF        Q2RECF
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *IN10 IFEQ
      *
      *   F12=CANCEL
      *
     C     *INKL         IFEQ      *ON
      *
      ***** BEG DEL ******* 09/02/05 **********************************
     C*********            GOTO TERM
      ***** END DEL ******* 09/02/05 **********************************
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   SELECT
      *
     C     Q2RECF        WHENEQ    'SCR01'
     C                   GOTO      TERM
      *
     C     Q2RECF        WHENEQ    'SCR02'
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDSL
      ***** END ADD ******* 09/02/05 **********************************
      *
     C                   ENDIF                                                  *INKL
      *
     C                   ENDIF                                                  *IN11 IFEQ
      ***** BEG ADD ******* 09/02/05 **********************************
      *
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
      *
      ***** END ADD ******* 09/02/05 **********************************
      *
      *   PROCESS REQUEST SCREEN
      *
     C     SCR01T        TAG
      *
      *  TYPE OF ACCOUNT MUST BE E, S, OR B.
      *
     C     SCETYP        IFNE      'E'
     C     SCETYP        ANDNE     'S'
     C     SCETYP        ANDNE     'B'
      *
     C                   MOVE      *ON           *IN21
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCETYP IFNE
      *
      *  EDIT FOR BOTH GROUPS FILLED OR NEITHER GROUP FILLED
      *
     C     SCFRDT        IFNE      *ZERO
     C     SCTODT        ORNE      *ZERO
     C     SCDATE        ORNE      *ZERO
     C***  Begin Del  ***  10/27/03  **************************************
     C*****BEGIN ADD  ***  10/07/99  **********************************
     C********** SCPYDT    ORNE *ZERO
     C*****END** ADD  ***  10/07/99  **********************************
     C***  End   Del  ***  10/27/03  **************************************
     C     SCMIN         ORNE      *ZERO
      ***** BEG ADD ******* 05/02/03 **********************************
     C     SCPPRM        ORNE      *ZERO
      ***** END ADD ******* 05/02/03 **********************************
      **** BEGIN ADD **** 04/28/00 *********************************************
     C     SCEAFL        ORNE      *BLANK
      **** END   ADD **** 04/28/00 *********************************************
      *** BEG ADD ***** 03/07/05 ***********************************************
     C     SCINDP        ORNE      *BLANK
      *** END ADD ***** 03/07/05 ***********************************************
      *
      *  BOTH GROUPS OF FIELDS HAVE DATA ENTERED
      *
      **** BEG CHG ****** 02/18/03 *********************************************
     C********** SCRPMY    IFNE *ZERO
     C  N43SCRPMY        IFNE      *ZERO
      **** END CHG ****** 02/18/03 *********************************************
     C     SCDAY         ORNE      *ZERO
     C                   MOVE      *ON           *IN22
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SCRPMY IFNE
      *
      **** BEG ADD ****** 02/18/03 *********************************************
     C   43SCWEDT        IFNE      *ZERO
     C     SCXADT        ORNE      *ZERO
     C     SCFREQ        ORNE      *BLANK
     C                   MOVE      *ON           *IN22
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SCWEDT IFNE
      **** END ADD ****** 02/18/03 *********************************************
      ***** BEG ADD ******* 10/23/03 **********************************
      *
     C     SCSRPY        IFNE      *BLANK
     C                   MOVE      *ON           *IN22
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SCWEDT IFNE
      *
      ***** END ADD ******* 10/23/03 **********************************
      *
     C                   MOVE      *ON           *IN61
      *
     C                   ELSE
      *
      *  BOTH GROUPS OF FIELDS ARE EMPTY
      *
      **** BEG CHG ****** 02/18/03 *********************************************
     C********** SCRPMY    IFEQ *ZERO
     C  N43SCRPMY        IFEQ      *ZERO
      **** END CHG ****** 02/18/03 *********************************************
     C     SCDAY         ANDEQ     *ZERO
     C                   MOVE      *ON           *IN23
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SCRPMY IFEQ
      *
      **** BEG ADD ****** 02/18/03 *********************************************
     C   43SCWEDT        IFEQ      *ZERO
     C     SCXADT        ANDEQ     *ZERO
     C     SCFREQ        ANDEQ     *BLANK
     C                   MOVE      *ON           *IN23
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SCWEDT IFNE
      **** END ADD ****** 02/18/03 *********************************************
      *
     C                   MOVE      *OFF          *IN61
      *
     C                   ENDIF                                                  SCFRDT IFNE
      *
      *  CANNOT ENTER MISSING MONTH DATA IF SCETYP <> 'S'
      *
     C     SCETYP        IFNE      'S'
      *
      **** BEG CHG ****** 02/18/03 *********************************************
     C********** SCRPMY    IFNE *ZERO
     C  N43SCRPMY        IFNE      *ZERO
      **** END CHG ****** 02/18/03 *********************************************
     C     SCDAY         ORNE      *ZERO
     C                   MOVE      *ON           *IN24
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SCRPMY IFNE
      *
      **** BEG ADD ****** 02/18/03 *********************************************
     C   43SCWEDT        IFNE      *ZERO
     C     SCXADT        ORNE      *ZERO
     C     SCFREQ        ORNE      *BLANK
     C                   MOVE      *ON           *IN24
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SCWEDT IFNE
      **** END ADD ****** 02/18/03 *********************************************
      *
     C                   ENDIF                                                  SCETYP IFNE
      *
      * IF ANY A/R BALANCE CRITERIA ENTERED, EDIT ALL A/R BALANCE FIELDS
      *
     C     *IN61         IFEQ      *ON
      *
      *  EDIT FROM POLICY EFFECTIVE DATE
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN26
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR IFEQ
      *
      *  EDIT TO POLICY EFFECTIVE DATE
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN27
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR IFEQ
      *
      *  DATES MUST BE ASCENDING ORDER
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C***  BEGIN CHG  ***  06/01/99  **********************************
     C*******************  MOVELSRDTO     XXFRDT  60
     C                   MOVEL     SRDTO         XXFRDT            7 0
     C***  END   CHG  ***  06/01/99  **********************************
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C***  BEGIN CHG  ***  06/01/99  **********************************
     C*******************  MOVELSRDTO     XXTODT  60
     C                   MOVEL     SRDTO         XXTODT            7 0
     C***  END   CHG  ***  06/01/99  **********************************
      *
     C     XXFRDT        IFGT      XXTODT
     C                   MOVE      *ON           *IN28
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  XXFRDT IFGT
     C*
     C***  BEGIN ADD  ***  06/01/99  **********************************
     C*  Check that 'to date' is less than or equal to run date
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
     C*
     C     XXTODT        IFGT      XXRNDT
     C                   MOVE      *ON           *IN41
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  XXTODT>XXRNDT
     C*
     C***  END   ADD  ***  06/01/99  **********************************
      *
      *  BILLING THROUGH DATE MUST BE VALID IF ENTERED
      *
     C     SCDATE        IFNE      *ZERO
      *
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN29
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR IFEQ
      *
     C                   ENDIF                                                  SCDATE IFNE
     C*
     C***  Begin Del  ***  10/27/03  **************************************
     C*****BEGIN ADD  ***  10/07/99  **********************************
     C***Payment through date must be valid if entered
     C********** SCPYDT    IFNE *ZERO
     C**********
     C**********           Z-ADDSCPYDT    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********           MOVELSRDTO     SCPYDT
     C**********
     C********** SRERR     IFEQ *ON
     C**********           MOVE *ON       *IN40
     C**********           GOTO STRT
     C**********           ENDIF                           SRERR=*ON
     C**********
     C**********           ENDIF                           SCPYDT<>*ZERO
     C*****END** ADD  ***  10/07/99  **********************************
     C***  End   Del  ***  10/27/03  **************************************
      *
      *  VALIDATE SCMIN > *ZERO
      *
      ***** BEG CHG ******* 05/02/03 **********************************
     C*********  SCMIN     IFLE *ZERO
     C     SCMIN         IFLT      *ZERO
      ***** END CHG ******* 05/02/03 **********************************
     C                   MOVE      *ON           *IN30
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SCMIN IFLE
      ***** BEG ADD ******* 05/02/03 **********************************
      *
      *  VALIDATE SCPPRM < 100
      *
     C     SCPPRM        IFGT      100
     C                   MOVE      *ON           *IN71
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SCPPRM < 100
      *
      *  "Min Balance" an "% Premium" are mutually exclusive.
      *
     C     SCPPRM        IFGT      0
     C     SCMIN         ANDGT     0
     C                   MOVE      *ON           *IN72
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  "Min Balance" OR "% Premium" must be entered.
      *
     C     SCPPRM        IFEQ      0
     C     SCMIN         ANDEQ     0
     C                   MOVE      *ON           *IN73
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF
      *
      ***** END ADD ******* 05/02/03 **********************************
      **** BEGIN ADD **** 12/01/99 ************************************
     C*
     C* ESTIMATED/AUDITED FLAG MUST BE 'A' OR 'E'
     C*
     C     SCEAFL        IFNE      'A'
     C     SCEAFL        ANDNE     'E'
     C***  Begin Add  ***  10/27/03  **************************************
     C     SCEAFL        ANDNE     'B'
     C***  End   Add  ***  10/27/03  **************************************
     C                   MOVE      *ON           *IN42
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF
      **** END   ADD **** 12/01/99 ************************************
      *
      *** BEG ADD ***** 03/07/05 **************************************
     C     SCINDP        IFNE      *BLANK
     C     SCINDP        ANDNE     'Y'
     C                   MOVE      *ON           *IN46
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF
      *** END ADD ***** 03/07/05 **************************************
      *
     C                   ELSE
      *
      * IF NO A/R BALANCE CRITERIA ENTERED, EDIT MISSING S/R REPORT CRITERIA
      *
      *
      *  VALIDATE REPORTING MM/CCYY
      *
      *** BEG ADD ***** 02/18/03 ***********************************************
     C     *IN43         IFEQ      *OFF
      *** END ADD ***** 02/18/03 ***********************************************
      *
     C                   Z-ADD     SCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     C                   MOVEL     SRDTO         SCRPMY
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN31
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR IFEQ
      *
      * VALIDATE DAY OF MONTH FOR NEW POLICY EXCLUSION
      *
     C     SCDAY         IFNE      0
      *
     C                   MOVE      XXRPMM        XXDTMM
     C                   MOVE      XXRPCY        XXDTCY
     C                   Z-ADD     SCDAY         XXDTDD
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN32
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR IFEQ
      *
     C                   ENDIF                                                  SCDAY IFNE
      *
      *** BEG ADD ***** 02/18/03 ***********************************************
     C                   ELSE                                                   *IN43 IFEQ *OFF
      *
      * IF VENDOR PROCESSING, VALIDATE REPORT DATE AND EXCLUDE DATE
      *
     C                   Z-ADD     SCWEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCWEDT
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN31
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR IFEQ
     **
     ** ENSURE THE REPORT DATE IS A MONTH END DATE IF REPORT FREQ IS 'M' OR 'B'
     **
     C                   CALL      'SRFUTR8'
     C                   PARM      SCWEDT        ##WEDT            8 0
     C                   PARM      1             ###DAY            5 0
     C                   PARM                    ##ERR             1
     **
     C                   MOVEL     ##WEDT        XXWED4            4
     C     2             SUBST     XXWED4:3      XX1STD            2
     C     SCFREQ        IFEQ      'M'
     C     SCFREQ        OREQ      'B'
     **
     C     XX1STD        IFNE      '01'
     C                   SETON                                        45
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  XX1STD IFNE 1'
     **
     C                   ENDIF                                                  SCFREQ = 'M'
     **
     C     SCXADT        IFNE      0
      *
     C                   Z-ADD     SCXADT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN32
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR IFEQ
      *
     C                   ENDIF                                                  SCXADT IFNE 0
      *
     C     SCFREQ        IFNE      'M'
     C     SCFREQ        ANDNE     'W'
     C     SCFREQ        ANDNE     'B'
     C                   SETON                                        44
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ENDIF                                                  *IN43 IFEQ *OFF
      *** END ADD ***** 02/18/03 ***********************************************
      ***** BEG ADD ******* 10/23/03 **********************************
      *
     C     SCSRPY        IFNE      *BLANK
     C     SCSRPY        ANDNE     'Y'
     C                   MOVE      *ON           *IN74
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SCWEDT IFNE
      *
      ***** END ADD ******* 10/23/03 **********************************
      *
     C                   ENDIF                                                  *IN61 IFEQ
      ***** BEG ADD ******* 09/02/05 **********************************
      *
      *   Include Agency Bill Employers (Y/ )
      *
     C     SCABIL        IFNE      *BLANK
     C     SCABIL        ANDNE     'Y'
     C                   SETON                                        75
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
      ***** END ADD ******* 09/02/05 **********************************
      *
      *   CANCEL REASON CODE MUST BE VALID
      *
     C     SCRSN         IFNE      *BLANK
     C                   MOVEL(P)  SCRSN         XXKEY
      *
     C***  BEGIN CHG  ***  10/21/99  **********************************
     C********** TBKY      CHAINPCR01                33
      **** BEGIN ADD **** 08/10/99 *********************************************
     C********** *IN33     DOWEQ*OFF
     C********** CRSTAT    ANDNE'A'
     C********** TBKY      READEPCR01                    33
     C**********           ENDDO                           *IN33 DOWEQ *OFF
      **** END   ADD **** 08/10/99 *********************************************
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  33
     C     *IN33         DOWEQ     *OFF
     C     CRSTAT        ANDNE     'A'
     C     CRKY1         READPE    PCR01                                  33
     C                   ENDDO                                                  *IN33=*OFF
     C*
     C***  END   CHG  ***  10/21/99  **********************************
      *
     C                   ENDIF                                                  SCRSN IFNE
      *
     C     *IN33         CABEQ     *ON           STRT
      *
     C     SCRSN         IFEQ      *BLANK
     C     CRSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN33
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SCRSN IFEQ
     C*
      *
      ***** BEG DEL ******* 09/02/05 **********************************
     C*****BEGIN ADD  ***  11/10/98  **********************************
     C***Validate office number
     C*********  SCOFF#    IFNE *BLANK
     C*********
     C*********  OFKY      CHAINPOF01                60
     C*********  *IN60     IFEQ *ON
     C*********            SETON                     37
     C*********            GOTO STRT
     C*********            ENDIF                           *IN60=*ON
     C*********
     C*********            ENDIF                           SCOFF#<>*BLANK
     C*****END*  ADD  ***  11/10/98  **********************************
     C*********
     C******BEGIN ADD  ***  11/24/98  *********************************
     C***Validate Estimated Issue date
     C*********  SCISDT    IFNE *ZERO
     C*********
     C*********            Z-ADDSCISDT    SRDTI
     C*********            CALL 'SRCVTDAT'DATCHK
     C*********  SRERR     IFEQ '1'
     C*********            MOVE *ON       *IN38
     C*********            GOTO STRT
     C*********            ELSE
     C*********            MOVELSRDTO     SCISDT
     C*********            ENDIF                           SRERR='1'
     C*********
     C***Check*that date is greater than or equal to run date
     C*********            Z-ADDSCISDT    SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     XXISDT  70
     C*********
     C*********            Z-ADDDSDATE    SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     XXRNDT  70
     C*********
     C*********  XXISDT    IFLT XXRNDT
     C*********            MOVE *ON       *IN39
     C*********            GOTO STRT
     C*********            ENDIF                           XXISDT<XXRNDT
     C*********
     C*********            ENDIF                           SCISDT<>*BLANK
     C******END   ADD  ***  11/24/98  *********************************
      ***** END DEL ******* 09/02/05 **********************************
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
      *
      *  VALIDATE SCREEN 2
      *
     C     SCR02T        TAG
      *
      *  Validate office number
      *
     C     SCOFF#        IFNE      *BLANK
     C     OFKY          CHAIN     POF01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   SETON                                        37
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60=*ON
      *
     C                   ENDIF                                                  SCOFF#<>*BLANK
      *
      *  Validate Estimated Issue date
      *
     C     SCISDT        IFNE      *ZERO
     C                   Z-ADD     SCISDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN38
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ELSE
      *
     C                   MOVEL     SRDTO         SCISDT
     C                   ENDIF                                                  SRERR='1'
      *
      *  Check that date is greater than or equal to run date
      *
     C                   Z-ADD     SCISDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXISDT            7 0
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT            7 0
      *
     C     XXISDT        IFLT      XXRNDT
     C                   MOVE      *ON           *IN39
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   MOVEL(P)  'SCR02'       Q2RECF
      ***** END ADD ******* 09/02/05 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  XXISDT<XXRNDT
      *
     C                   ENDIF                                                  SCISDT<>*BLANK
      ***   END   ADD  ***  11/24/98  *********************************
      /Free
       //*** Begin Add *** 11/21/19 ***********************************

       // Validation Check for State Codes Inclusion/Exclusion.
       If scSTcodes = *Blanks and scSTCodIE <> ' ';
          erStCodIE  = *on;
          msStCodIE  = 'WCR3178';
       Else;
          erStCodIE = *Off;
          msStCodIE  = *Blanks;
       EndIf;
      /End-Free
     C     erStCodIE     IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
      *** End Add      *** 11/21/19 ***********************************
      ***** END ADD ******* 09/02/05 **********************************
      *
      *  PASSED ALL VALIDATION, WILL SUBMIT JOB TO JOBQ.
      *
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      UPFND         QQFND             3
      *
     C*** BEG CHG ****** 02/18/03 *************************************
     C********** SCDATE    IFNE 0
     C  N43SCDATE        IFNE      0
     C*** END CHG ****** 02/18/03 *************************************
     C     SCRPMY        ORNE      0
      *
     C                   MOVE      SCDATE        QQDATE
      *
     C                   ELSE
      *
     C                   Z-ADD     DSDATE        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C                   MOVEL     SRDTO         QQDATE
      *
     C                   ENDIF                                                  SCDATE IFNE
     C*
     C*** BEG ADD ****** 02/18/03 *************************************
     C*
     C* IF VENDOR PROCESSING ENABLED (*IN43 = *ON), USE SCWEDT
     C*
     C   43SCDATE        IFNE      0
     C     SCWEDT        ORNE      0
     C                   MOVE      SCDATE        QQDATE
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         QQDATE
     C                   ENDIF                                                  SCDATE IFNE
     C*** END ADD ****** 02/18/03 *************************************
     C*
     C***  Begin Chg  ***  10/27/03  **************************************
     C*****BEGIN ADD  ***  10/07/99  **********************************
     C****BEG*CHG ****** 02/18/03 *************************************
     C********** SCPYDT    IFNE 0
     C**N43***** SCPYDT    IFNE 0
     C****END*CHG ****** 02/18/03 *************************************
     C********** SCRPMY    ORNE 0
     C**********           MOVE SCPYDT    QQPYDT
     C**********
     C**********           ELSE
     C**********
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********           MOVELSRDTO     QQPYDT
     C**********           ENDIF                           SCPYDT<>0
     C*****END** ADD  ***  10/07/99  **********************************
     C****BEG*ADD ****** 02/18/03 *************************************
     C**********
     C**IF*VENDOR PROCESSING ENABLED (*IN43 = *ON), USE SCWEDT INSTEAD OF SCRPMY
     C**********
     C***43***** SCPYDT    IFNE 0
     C********** SCWEDT    ORNE 0
     C**********           MOVE SCPYDT    QQPYDT
     C**********           ELSE
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********           MOVELSRDTO     QQPYDT
     C**********           ENDIF                           SCDATE IFNE
     C****END*ADD ****** 02/18/03 *************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         QQPYDT
     C***  End   Chg  ***  10/27/03  **************************************
      *
     C                   MOVE      SCETYP        QQETYP
      ***** BEG DEL ******* 05/02/03 **********************************
     C*************        MOVE SCMIN     QQMIN
      ***** END DEL ******* 05/02/03 **********************************
      ***** BEG ADD ******* 05/02/03 **********************************
      *
     C                   SELECT
      *
     C     SCPPRM        WHENGT    *ZERO
     C                   Z-ADD     SCPPRM        XXMIN             7 2
     C                   MOVE      XXMIN         QQMIN
      *
     C     SCMIN         WHENGT    *ZERO
     C                   MOVE      SCMIN         QQMIN
      *
     C                   OTHER
     C                   MOVE      *ZERO         QQMIN
      *
     C                   ENDSL
      *
      ***** END ADD ******* 05/02/03 **********************************
     C*** BEG CHG ****** 02/18/03 *************************************
     C******************** MOVE SCRPMY    QQRPMY
     C  N43              MOVE      SCRPMY        QQRPMY
     C   43              MOVE      SCWEDT        QQWEDT
     C   43              MOVE      SCXADT        QQXADT
     C   43              MOVE      SCFREQ        QQFREQ
     C*** END CHG ****** 02/18/03 *************************************
     C*** BEG ADD ****** 06/30/03 *************************************
     C   43              MOVE      *ZEROS        QQRPMY
     C  N43              MOVE      *ZEROS        QQWEDT
     C*** END CHG ****** 06/30/03 *************************************
      *
      *   GET NEXT AVAILABLE BATCH # & ADD THIS BATCH TO BATCH FILE
      *
     C     CCKY          SETGT     PCC02
     C     CCKY          READPE    PCC02                                  60
      *
     C     *IN60         IFEQ      *ON
      *
     C                   Z-ADD     1             XXBAT#
      *
     C                   ELSE
      *
     C     CCBAT#        ADD       1             XXBAT#
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
      *** BEG ADD ***** 06/30/03 *****************************************
     C                   CLEAR                   PCC02
      *** END ADD ***** 06/30/03 *****************************************
      *
     C                   MOVE      UPCO#         CCCO#
     C                   MOVE      UPFND         CCFND
     C                   MOVE      *ZERO         CCFYR
     C                   MOVE      SCETYP        CCETYP
      *
     C     SCRPMY        IFEQ      0
     C*** BEG ADD ****** 02/18/03 *************************************
     C     SCWEDT        ANDEQ     0
     C*** END ADD ****** 02/18/03 *************************************
      *
     C     SCDATE        IFNE      0
     C                   MOVE      SCDATE        SRDTI
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   ENDIF                                                  SCDATE IFNE
      *
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CCBLDT
     C*
     C***  BEGIN ADD  ***  10/07/99  **********************************
     C     SCPYDT        IFNE      0
     C                   MOVE      SCPYDT        SRDTI
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   ENDIF                                                  SCPYDT<>0
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CCPTDT
     C***  END   ADD  ***  10/07/99  **********************************
      *
     C                   ENDIF                                                  SCRPMY IFEQ
      *
      ***** BEG DEL ******* 05/02/03 **********************************
     C*************        MOVE SCMIN     CCMIN
      ***** END DEL ******* 05/02/03 **********************************
      ***** BEG ADD ******* 05/02/03 **********************************
      *
     C                   SELECT
      *
     C     SCPPRM        WHENGT    *ZERO
     C                   Z-ADD     SCPPRM        CCMIN
     C                   MOVE      'P'           CCADPP
      *
      *
     C     SCMIN         WHENGT    *ZERO
     C                   Z-ADD     SCMIN         CCMIN
     C                   MOVE      'A'           CCADPP
      *
     C                   OTHER
     C                   Z-ADD     *ZERO         CCMIN
     C                   MOVE      *BLANK        CCADPP
      *
     C                   ENDSL
      *
      ***** END ADD ******* 05/02/03 **********************************
      *
     C*** BEG ADD ****** 02/18/03 *************************************
     C     *IN43         IFEQ      *OFF
     C*** END ADD ****** 02/18/03 *************************************
      *
     C                   Z-ADD     SCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         CCRPMY
     C*** BEG ADD ****** 02/18/03 *************************************
      *
     C                   ELSE
      *
     C                   Z-ADD     SCWEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CCWEDT
     C*** BEG ADD ****** 02/17/21 *************************************
      * Move Week Ending Date into Reporting Month/Year
      /Free
       If CCWEDT <> 0;
          CCRPMY = %dec(%subst(%editc(CCWEDT:'X'):1:5):5:0);
       EndIf;
      /End-Free
     C*** END ADD ****** 02/17/21 *************************************
      *
     C                   Z-ADD     SCXADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CCXADT
      *
     C                   MOVEL     SCFREQ        CCFREQ
      *
     C                   ENDIF
     C*** END ADD ****** 02/18/03 *************************************
      *
     C                   MOVE      'A'           CCSTAT
     C                   MOVE      Q1USER        CCUSER
     C                   MOVE      Q1PGM         CCPGM
     C                   MOVE      DSTIME        CCTIME
     C                   Z-ADD     DSDATE        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C                   MOVEL     SRDTO         CCDT
     C                   Z-ADD     XXBAT#        CCBAT#
     C                   MOVEL     SCRSN         CCRSN
     C                   MOVEL     SCCOM1        CCCOM1
     C                   MOVEL     SCCOM2        CCCOM2
     C                   MOVE      'N'           CCBPF
     C*** BEG CHG ****** 02/18/03 *************************************
     C******************** MOVE SCDAY     CCDAY
     C  N43              MOVE      SCDAY         CCDAY
     C*** END CHG ****** 02/18/03 *************************************
     C                   Z-ADD     SCFRDT        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C                   MOVEL     SRDTO         CCFRDT
     C                   Z-ADD     SCTODT        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C                   MOVEL     SRDTO         CCTODT
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C                   MOVE      SCOFF#        CCOFF#
     C***  END   ADD  ***  11/10/98  **********************************
      **** BEGIN ADD **** 12/01/99 ************************************
     C                   MOVE      SCEAFL        CCEAFL
      **** END   ADD **** 12/01/99 ************************************
      ***** BEG ADD ******* 03/07/05 **********************************
     C                   MOVE      SCINDP        CCINDP
      ***** END ADD ******* 03/07/05 **********************************
      *
     C                   WRITE     PCC02
      *
     C                   MOVE      XXBAT#        QQBAT#
     C                   MOVEL     SCRSN         QQRSN
     C                   MOVEL     SCCOM1        QQCOM1
      *
     C     XXCOM1        IFEQ      *BLANK
     C                   MOVE      '¢'           XXCOM1
     C                   ENDIF                                                  XXCOM1 IFEQ
      *
     C                   MOVEL     SCCOM2        QQCOM2
      *
     C     XXCOM2        IFEQ      *BLANK
     C                   MOVE      '¢'           XXCOM2
     C                   ENDIF                                                  XXCOM1 IFEQ
      *
     C                   MOVE      SCDAY         QQDAY
      *
     C*** BEG CHG ****** 02/18/03 *************************************
     C********** SCRPMY    IFEQ *ZERO
     C  N43SCRPMY        IFEQ      *ZERO
     C*** END CHG ****** 02/18/03 *************************************
      *
     C                   MOVE      SCFRDT        QQFRDT
     C                   MOVE      SCTODT        QQTODT
     C                   Z-ADD     DSDATE        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C                   MOVEL     SRDTO         QQRNDT
      *
     C                   ELSE
      *
     C                   MOVE      XXRPMM        XXDTMM
     C                   MOVE      XXRPCY        XXDTCY
      *
     C                   SELECT
      *
     C     XXDTMM        WHENEQ    2
      *
     C     XXDTCY        DIV       4             JUNK
     C                   MVR                     REM
      *
     C     REM           IFEQ      *ZERO
      *
     C                   Z-ADD     29            XXDTDD
      *
     C                   ELSE
      *
     C                   Z-ADD     28            XXDTDD
      *
     C                   ENDIF                                                  REM IFEQ
      *
     C     XXDTMM        WHENEQ    4
     C     XXDTMM        OREQ      6
     C     XXDTMM        OREQ      9
     C     XXDTMM        OREQ      11
      *
     C                   Z-ADD     30            XXDTDD
      *
     C                   OTHER
      *
     C                   Z-ADD     31            XXDTDD
      *
     C                   ENDSL                                                  XXDTMM WHEQ
      *
     C                   MOVE      XXDATE        QQTODT
     C                   Z-ADD     1             XXDTDD
     C                   MOVE      XXDATE        QQFRDT
     C                   MOVE      *ZEROS        QQRNDT
      *
     C                   ENDIF                                                  SCRPMY IFEQ
     C*
     C***  BEG ADD ******  02/18/03  **********************************
     C   43SCWEDT        IFEQ      *ZERO
     C                   MOVE      SCFRDT        QQFRDT
     C                   MOVE      SCTODT        QQTODT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         QQRNDT
      *
     C                   ELSE
     C                   MOVE      *ZEROS        QQRNDT
      *
      *  QQFRDT AND QQTODT MUST BE FIRST AND LAST OF MONTH IF FREQ = M OR B
      *
     C     SCFREQ        IFEQ      'M'
     C     SCFREQ        OREQ      'B'
     C                   MOVE      SCWEDT        QQTODT
     C                   Z-ADD     SCWEDT        XXWEDT
     C                   Z-ADD     1             XXWEDD
     C                   MOVE      XXWEDT        QQFRDT
     C                   ENDIF
      *
      *  IF FREQ = W, QQTODT MUST BE REPORT DATE AND QQFRDT MUST BE 7 DAYS LESS
      *
     C     SCFREQ        IFEQ      'W'
     C                   MOVE      SCWEDT        QQTODT
     C                   CALL      'SRPAST8'
     C                   PARM      SCWEDT        ##BEDT            8 0
     C                   PARM      7             ###DAY
     C                   PARM                    ##ERR
     C                   MOVE      ##BEDT        QQFRDT
     C                   ENDIF
      *
     C                   ENDIF
     C***  END ADD ******  02/18/03  **********************************
     C*
     C***  BEGIN ADD  ***  11/24/98  **********************************
     C                   MOVE      SCISDT        QQISDT            8
     C***  END   ADD  ***  11/24/98  **********************************
      **** BEGIN ADD **** 12/01/99 ************************************
     C                   MOVE      SCEAFL        QQEAFL            1
      **** END   ADD **** 12/01/99 ************************************
      **** BEGIN ADD **** 11/21/19 ************************************
      /Free
       //Remove commas separating states
         If scSTcodes  <> *blanks;
            ieSTcodes = scSTcodes;
            IntPos = %scan(',':ieSTcodes);
            If IntPos > *zero;
               Dou IntPos = *zero;
                  ieSTcodes = %subst(ieSTcodes:1:IntPos-1) +
                        %subst(ieSTcodes:IntPos+1:155-IntPos);
                  IntPos = %scan(',':ieSTcodes);
               Enddo;
               outSTcodes = %subst(ieSTcodes:1:150);
            Else;
               outStcodes = scSTcodes;
            Endif;
         Else;
            OutSTcodes = *blanks;
         Endif;
      /End-Free
      **** END   ADD **** 11/21/19 ************************************
      *
      *   CALL THE CLP TO SUBMIT THE LIST.
      *
     C                   CALL      'W0160C'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFRDT            8            POL EFF FROM DT
     C                   PARM                    QQTODT            8            POL EFF TO DT
     C                   PARM                    QQDATE            8            BILLING THRU DT
     C***  BEGIN CHG  ***  10/07/99  **********************************
     C                   PARM                    QQPYDT            8            PAYMENT THRU DT
     C***  END   CHG  ***  10/07/99  **********************************
     C                   PARM                    QQETYP            1            TYPE OF ACCOUNT
     C                   PARM                    QQMIN             7            MINIMUM BALANCE
     C                   PARM                    QQRPMY            6            REPORTING MONTH/YEAR
     C                   PARM                    QQDAY             2            EXCL NEW ACCT FRM DA
     C                   PARM                    QQBAT#            9            BATCH #
     C                   PARM                    QQRSN             2            CANCEL REASON CODE
     C                   PARM                    QQCOM1                         COMMENT 1
     C                   PARM                    QQCOM2                         COMMENT 2
     C                   PARM                    UPOUTQ           10
     C                   PARM      'D'           QQMODE            1
     C                   PARM                    QQRNDT            8
     C***  BEGIN ADD  ***  11/24/98  **********************************
     C                   PARM                    QQISDT                         EST. ISSUE DT
     C***  END   ADD  ***  11/24/98  **********************************
      **** BEGIN ADD **** 12/01/99 ************************************
     C                   PARM                    QQEAFL            1
      **** END   ADD **** 12/01/99 ************************************
     C***  BEG ADD ******  02/18/03  **********************************
     C                   PARM                    QQWEDT            8
     C                   PARM                    QQXADT            8
     C                   PARM                    QQFREQ            1
     C***  END ADD ******  02/18/03  **********************************
      ***** BEG ADD ******* 10/23/03 **********************************
     C                   PARM      SCSRPY        ##SRPY            1
      ***** END ADD ******* 10/23/03 **********************************
      *** BEG ADD ***** 03/07/05 **************************************
     C                   PARM      SCINDP        QQINDP            1
      *** END ADD ***** 03/07/05 **************************************
      ***** BEG ADD ******* 09/02/05 **********************************
     C                   PARM      SCABIL        ##ABIL            1
      ***** END ADD ******* 09/02/05 **********************************
      *** BEG ADD *** 11/21/19 ****************************************
     C                   PARM      SCstCodIe     outStIEflg        1
     C                   PARM                    outSTcodes      150
      *** END ADD *** 11/21/19 ****************************************
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
      *
     C                   MOVE      *ON           *INLR
      *** Begin Add *** 11/21/19 *******************************************
      *//===================================================================
      *// LoadSTCodes - Load State codes dropdown
      *//===================================================================
     p LoadSTCodes     b
     d LoadSTCodes     pi
     d strSTdesc       s             30
      /Free

       If Not %Open(STSTL5);
          Open STSTL5;
       Endif;

       scSTsel = *Blanks;
       scSTval = *Blanks;
       strSTdesc = *Blanks;

       // Read WTTTP file from starting
       read STSTL5;
       Dow not %Eof(STSTL5);
          strSTdesc = TitleCase(STname);
          If scSTsel = *Blanks;
             scSTsel = %trim(STkey)+'-'+%trim(strSTdesc);
             scSTval = %trim(STkey);
          else;
             scSTsel=%trim(scSTsel)+','+%trim(STkey)+'-'+%trim(strSTdesc);
             scSTval = %trim(scSTval)+','+%trim(STkey);
          endif;
          read STSTL5;
       enddo;

       If %Open(STSTL5);
          Close STSTL5;
       Endif;
      /End-Free

     p LoadSTCodes     e
      *** End   Add *** 11/21/19 *******************************************
