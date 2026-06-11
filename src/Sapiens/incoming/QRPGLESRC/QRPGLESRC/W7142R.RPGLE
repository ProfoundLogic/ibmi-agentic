     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7142R - EMPLOYER LIST ALPHA OR NUMERIC
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 01/31/02 C:61  PROJECT WC1070                                 *
     H*                Get Employer Contact info from WMECP.          *
     H*                                                               *
     H* 12/16/99  C:35   PROJECT: WA107 20994                         *
     H*                  CANCELLATION DATE WAS NOT PRINTING FOR       *
     H*                  99 POLICY YEAR BECAUSE OF 2-DIGIT YEAR FIELD *
     H*                  (Y2K ISSUE).                                 *
     H*                                                               *
     H*  01/25/99  C:90  PROJECT: WA065/16038                         *
     H*            REMOVE MOVE *BLANKS TO FNCGFL.                     *
     H*                                                               *
     H*  05/01/98  C:78  PROJECT: WC745                               *
     H*            WDF2P/F2CT01 NOT BEING USED ANYMORE,SO DELETE FILE.*
     H*            USING WMFNL/FNCGFL INSTEAD, WHICH IS ALREADY A ONE *
     H*            CHAR FIELD, SO WE DONT NEED TO USE XXFCT1 ANYMORE. *
     H*                                                               *
     H*  10/27/97  C:86  VERSION 3 PROJECT: WC577                     *
     H*            REVISED TO PRINT THE ALTERNATE FUND NAME IN PLACE  *
     H*            OF COMPANY NAME, IF ONE EXISTS, AND CENTER NAME.   *
     H*****************************************************************
     FWMEML1    IF   E           K DISK
     FWMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
     FWDELL     IF   E           K DISK
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      * Employer Contact
     FWMECL1    IF   E           K DISK
      ***** END ADD ******* 01/31/02 **********************************
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     F**** BEGIN DEL ****** 05/01/98 **********************************
     F******WDF2L   IF  E           K        DISK
     F**** END   DEL ****** 05/01/98 **********************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D                 DS
     D  CX                     1    035  0
     D                                     DIM(5)
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  EMPZIP                 1      9  0
     D  XXZPP1                 1      5  0
     D  XXZPP2                 6      9  0
     D                 DS
     D  EMMZIP                 1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D                 DS
     D  XXENPO                 1      7  0
      **** BEGIN CHG **** 12/16/99 *********************************************
     D*************************************** 2   30XXENYR
     D  XXENYR                 1      3  0
      **** END   CHG **** 12/16/99 *********************************************
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    EMEMP#        XXEMP
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMDT          XXDATE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQQUE            10            MLS 1/15/88
     C                   PARM                    QQMODE            1                 "
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQMODE        XXMODE            1            MLS 1/15/88
     C*                                                         "
     C*   SET-UP AND INITIALIZE                                 "
     C*                                                         "
     C     XXMODE        IFEQ      'D'                                               "
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE                                                        "
     C                   SETON                                        08        BATCH MODE
     C                   END                                                         "
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      *
      *===============================================================
      * KLIST
      *===============================================================
      *
      *  Employer Contact
      *
     C     ECKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKPCNT            1
      *
      ***** END ADD ******* 01/31/02 **********************************
     C*
     C*  AGENT HISTORY MASTER FILE
     C*
     C     AHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR                          LJQ 7-29-88
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    AHAGT#
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE REPORT OVERRIDE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE BILLING SCHEDULE FILE
     C*
     C     ABKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE FUND DETAIL FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C****** BEGIN DEL ***** 05/01/98 *********************************
     C*
     C*   KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C******     F2KY      KLIST
     C******               KFLD           XXCO#
     C******               KFLD           XXFND
     C****** END   DEL ***** 05/01/98 *********************************
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C                   MOVE      UPPRDT        XXDATE
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDT              6 0
     C*
     C                   TIME                    DSDTTM
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*
     C*   CHAIN TO THE FUND FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*** BEGIN ADD *** 10/27/97 *******************************
     C     *IN60         IFEQ      '0'
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL(P)  FNANAM        ##DATA
     C                   ELSE
     C                   MOVEL(P)  CONAME        ##DATA
     C****** BEGIN ADD ****** 05/01/98 ********************************
     C**** BEGIN DEL **** 01/25/99 ************************************
     C*******************  MOVE *BLANK    FNCGFL
     C**** END   DEL **** 01/25/99 ************************************
     C****** END   ADD ****** 05/01/98 ********************************
     C                   ENDIF
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XCNAME           60
     C                   ENDIF
     C*
     C*** END ADD ***** 10/27/97 ********************************
     C*
     C*  LOAD FLAG THAT DETERMINES WHETHER OR NOT THIS FUND WAS SET-UP
     C*  FOR NON-COMMON EXPIRATION DATE POLICIES
     C*
     C**** FLAG FNCGFL LOADED A FEW LINES AGO WHEN CHAINED TO WMFNL ***
     C****** BEGIN DEL ****** 05/01/98 ********************************
     C*****                MOVE *BLANK    XXFCT1  1
     C*****      F2KY      CHAINPF201                60
     C***** N60                MOVELFNCGFL    XXFCT1
     C****** END   DEL ****** 05/01/98 ********************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
     C     QQTYPE        IFEQ      'A'
     C                   MOVEL     'ALPHA'       TITL2             8
     C                   ELSE
     C                   MOVEL     'NUMERIC'     TITL2
     C                   ENDIF
      ***** Beg Del ***** 08/26/14 **************************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 **************************************************
     C                   END
     C                   EXCEPT    HEAD
     C     QQTYPE        IFEQ      'A'
     C     EMKEY         CHAIN     PEM01                              60
     C                   ELSE
     C     EMKEY         CHAIN     PEM02                              60
     C                   ENDIF
     C     *IN60         CABEQ     '1'           END                      LR
     C*
     C     TOP           TAG
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   CHAIN TO BILLING SCHEDULE TABLE FILE
     C*
     C                   MOVE      *BLANKS       XXKEY            10
     C                   MOVEL     ELBSCD        XXKEY
     C     ABKEY         CHAIN     PBS01                              60
     C*                                                     LJQ 7-29-88
     C***  FIRST GET THE MOST CURRENT "ACTIVE" AGENT HISTORY RECORD.
     C*                                                           "
     C     AHKEY         SETGT     PAH01                                               "
     C                   READP     PAH01                                  58           "
     C     *IN58         DOWEQ     '0'                                                 "
     C     AHSTAT        ANDEQ     'I'                                                 "
     C                   READP     PAH01                                  58           "
     C                   END                                                           "
     C*                                                           "
     C***  TEST THE SELECTION CRITERIA.                           "
     C*                                                           "
     C     *IN58         IFEQ      '0'                                                 "
     C     EMCO#         ANDEQ     AHCO#                                               "
     C     EMFND         ANDEQ     AHFND                                               "
     C     EMEMP#        ANDEQ     AHEMP#                                              "
     C     EMDIV         ANDEQ     AHDIV                                               "
     C     XXFYR         ANDEQ     AHFYR                                               "
     C*                                                           "
     C***  GET THE FEIN & MOD FROM THE AGENT ASSIGNMENT           "
     C*                                                           "
     C     AAKEY         CHAIN     PAA01                              54               "
     C*                                                           "
     C***  AND FINALLY... GET THE AGENT FILE.                     "
     C*                                                           "
     C  N54AGKEY         CHAIN     PAG01                              54               "
     C*                                                           "
     C  N54              MOVEL     AGNAME        XXANAM           20                   "
     C  N54              MOVEL     AAAGT#        XXAG#             5 0                 "
     C                   ELSE                                                          "
     C                   MOVE      *BLANK        XXANAM                                "
     C                   MOVE      *BLANK        XXAG#                                 "
     C                   END                                                           "
     C*
     C                   MOVEL     EMEMP#        XXEMP
     C                   MOVEL     EMDIV         XXDIV
     C                   MOVEL     EMNAME        XXNAM            34
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      * Get Employer Contact Name
      *
     C                   MOVE      '2'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         EMATTN
      *
      ***** END ADD ******* 01/31/02 **********************************
     C                   MOVEL     EMATTN        XXATN            40
     C*
     C                   MOVE      *BLANKS       XXAD1
     C                   MOVE      *BLANKS       XXAD2
     C                   MOVE      *BLANKS       XXAD3
     C*
     C     EMMPFL        IFEQ      'M'
     C*
     C                   MOVE      *BLANKS       XXAD3            40
     C                   MOVEL     EMMAD1        XXAD1            40
     C                   MOVEL     EMMAD2        XXAD2            40
     C*
     C*   LOAD CITY, STATE, & ZIP FOR THE MAILING ADDRESS
     C*
     C                   MOVE      EMMST         XXSTAT            2
     C                   MOVE      XXZPM1        XXZIP1            5
     C                   MOVE      XXZPM2        XXZIP2            4 0
     C                   MOVE      EMMCTY        XXCITY           25
     C*
     C                   EXSR      SR2ND
     C*
     C                   MOVE      XXCSTZ        XXAD3
     C*
     C     XXAD2         IFEQ      *BLANK
     C                   MOVEL     XXAD3         XXAD2
     C                   MOVE      *BLANK        XXAD3            40
     C                   END
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       XXAD3            40
     C                   MOVEL     EMPAD1        XXAD1
     C                   MOVEL     EMPAD2        XXAD2
     C*
     C*   LOAD CITY, STATE, & ZIP FOR THE PHYSICAL ADDRESS
     C*
     C                   MOVE      EMPST         XXSTAT            2
     C                   MOVE      XXZPP1        XXZIP1            5
     C                   MOVE      XXZPP2        XXZIP2            4 0
     C                   MOVE      EMPCTY        XXCITY           25
     C*
     C                   EXSR      SR2ND
     C*
     C                   MOVE      XXCSTZ        XXAD3
     C*
     C     XXAD2         IFEQ      *BLANK
     C                   MOVEL     XXAD3         XXAD2
     C                   MOVE      *BLANK        XXAD3
     C                   END
     C*
     C                   END
     C*
     C     EMAREA        IFNE      *ZERO
     C                   MOVEL     '('           XXARE4            4
     C                   MOVE      EMAREA        XXARE4
     C                   MOVEL     XXARE4        XXAREA            5
     C                   MOVE      ')'           XXAREA
     C                   ELSE
     C                   MOVE      *BLANK        XXAREA
     C                   END
     C*
     C                   MOVEL     EMAREA        XXPH#            10 0
     C                   MOVE      EMPHN#        XXPH#
     C                   MOVE      EMRGN#        XXRGN             3 0
     C                   MOVE      EMCNTY        XXCNTY            2
     C*
     C*   LOOK THRU EACH OCCURRENCE INORDER TO FIND ONE OF THE
     C*   FOLLOWING:
     C*          1. THE EFFECTIVE DATE PRESENT WITHOUT A CANCEL DATE.
     C*   -OR-   2. THE EFFECTIVE DATE PRESENT WITH THE CANCEL DATE
     C*             BEING GREATER THAN THE POLICY BEGIN DATE.
     C*
     C                   MOVE      '0'           *IN60
     C     1             DO        5             X                 1 0
     C     EF(X)         IFNE      *ZERO
     C     CX(X)         ANDEQ     *ZERO
     C     EF(X)         ORNE      *ZERO
     C     CX(X)         ANDGE     ELPFDT
     C                   MOVE      '1'           *IN60
     C                   END
     C  N60              END
     C*
     C*   TEST THE INDICATOR *IN60 TO SEE IF WE FOUND THE CORRECT
     C*   OCCURRENCE. IF NOT, READ THE NEXT EMPLOYER RECORD.
     C*
     C  N60              GOTO      NEXT
     C*
     C     F1KY          CHAIN     PF101                              61
     C****** BEGIN CHG ******** 05/01/98 ******************************
     C********   XXFCT1    IFEQ 'Y'
     C     FNCGFL        IFEQ      'Y'
     C****** END   CHG ******** 05/01/98 ******************************
     C                   Z-ADD     ELPFDT        XXENPO
     C                   ADD       1             XXENYR
     C                   ELSE
     C                   Z-ADD     F1ENPO        XXENPO
     C                   END
     C*
     C                   Z-ADD     EF(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEFDT            6 0
     C*
     C     CX(X)         IFLT      XXENPO
     C*
     C                   Z-ADD     CX(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXCXDT            6 0
     C*
     C                   ELSE
     C                   Z-ADD     0             XXCXDT
     C                   END
     C*
     C*** ADD THE ACTIVE & CANCELLED EMPLOYERS
     C*
     C     XXCXDT        IFEQ      *ZERO
     C                   ADD       1             XXACT             5 0
     C                   ELSE
     C                   ADD       1             XXCXL             5 0
     C                   END
     C*
     C                   MOVE      ELEXM1        XXEM1             5 4
     C                   MOVE      ELEXM2        XXEM2             5 4
     C                   MOVE      ELEXM3        XXEM3             5 4
     C*
     C                   Z-ADD     ELEMD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDT1             6 0
     C*
     C                   Z-ADD     ELEMD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDT2             6 0
     C*
     C                   Z-ADD     ELEMD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDT3             6 0
     C*
     C   22              EXCEPT    HEAD
     C                   SETOFF                                           22
     C                   EXCEPT    DET
     C     XXAD3         IFNE      *BLANK
     C     XXEM2         ORNE      *ZERO
     C                   EXCEPT    DET2
     C                   ENDIF
     C     XXEM3         IFNE      *ZERO
     C                   EXCEPT    DET3
     C                   ENDIF
     C   OF              SETON                                            22
     C*
     C*
     C                   END
     C*
     C     NEXT          TAG
     C*** READ THE NEXT EMPLOYEE
     C*
     C     QQTYPE        IFEQ      'A'
     C     EMKEY         READE     PEM01                                  60
     C                   ELSE
     C     EMKEY         READE     PEM02                                  60
     C                   ENDIF
     C     *IN60         IFEQ      '1'
     C     XXACT         ANDEQ     0
     C                   EXCEPT    DETL2
     C                   ENDIF
     C  N60              GOTO      TOP
     C*
     C*
     C     END           TAG
     C                   MOVE      '1'           *INLR
     C                   EXCEPT    TOTL
      /Free
       // ***** Beg Add ***** 08/26/14 ****************************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       // ***** End Add ***** 08/26/14 ****************************************
      /End-free
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      *
      *
      *===============================================================
      * SRCONT - Get Employer Contact Name
      *===============================================================
      *
     C     SRCONT        BEGSR
     C                   CLEAR                   XXCNT
      *
      * Get Contact Name
      *
     C     ECKY          CHAIN     PEC01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     ECFSN         IFNE      *BLANKS
     C     ECFSN         CAT       ECLSN:1       XXCNT            50
      *
     C                   ELSE
      *
     C     ECTITL        CAT       ECLSN:1       XXCNT
     C                   ENDIF
      *
     C                   ENDIF                                                  ECKY
      *
     C                   ENDSR                                                  SRCONT
      *
      *
      ***** END ADD ******* 01/31/02 **********************************
     C*
     C******************************************************
     C*    -SUBROUTINE TO LOAD THE CITY, STATE, AND ZIP    *
     C******************************************************
     CSR   SR2ND         BEGSR
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     XXSTAT        XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZIP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZIP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZIP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCITY        XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSTZ           40
     C                   ENDSR
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *****************************************************
     O*********                                   8 'XXXXXXXX'
     O*********                                  20 'XXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXX     XX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXXX    XX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XXXX    XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   8 'XXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   4 'XXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   5 'XXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   6 'XX XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   7 'XX  XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX    XXXX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   8 'XX   XXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  33 'XXXXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX    XXXX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          3
     O*********                                   9 'XX    XXX'
     O*********                                  20 'XXXXXX'
     O*********                                  32 'XXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX     XXX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          1
     O*********                                  15 'ROUTE TO:'
     O*  MLS 1/15/88 ADD INDICTORS AND DISTRIBUTION INFORMATION
     O*********      08      ORDIS1              60
     O*********         09   UPUSNM              67
     O********* E            ROUTE          1
     O*********      08      ORDIS2              60
     O********* E            ROUTE          1
     O*********      08      ORDIS3              60
     O********* E            ROUTE          2
     O*********      08      ORDIS4              60
      ***** End Del ***** 08/26/14 *****************************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O*  MLS 1/15/88 ADD INDICTORS AND DISTRIBUTION INFORMATION
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                                           53 'EMPLOYER LISTING - '
     O                       TITL2             +  0
      ***** Beg Del ***** 08/26/14 *****************************************************
     O********* E            ROUTE          2
     O*********                                  19 'PROGRAM NAME:'
     O*********              ORPNAM              33
     O*********                                  39 'FILE:'
     O*********              ORFIL#              41
     O*********                                  49 'COPIES:'
     O*********              ORCOPY        Z     53
     O*********                                  59 'HOLD:'
     O*********              ORHOLD              64
     O********* E            ROUTE          2
     O*********                                  19 'OUTPUT QUEUE:'
     O*********      08      OROUTQ              33
     O*  MLS 1/15/88 ADD INDICATORS AND OUT QUEUE
     O*********         09   QQQUE               33
     O*********                                  40 'FORMS:'
     O*********              ORFORM              51
     O*********                                  59 'SAVE:'
     O*********              ORSAVE              64
      ***** End Del ***** 08/26/14 *****************************************************
     O          E            ROUTE          2
     O                                         +  6 'POLICY YEAR:'
     O                       XXFYR         Z   +  5
     O          E            ROUTE          2
     O                                         +  6 'LIST TYPE(A/N):'
     O                       QQTYPE            +  2
     O                       TITL2             +  2
     O***  BEGIN CHG  ***  01/28/10  *********************************
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      *****END   CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XCNAME              96
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           72 'EMPLOYER LISTING - '
     O                       TITL2             +  0
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                              'GROUP:'
     O                       FNFND         Z   +  1
     O                       FNNAME            +  2
     O          E            HEAD           1
     O                                           24 '------------------------'
     O                                           48 '------------------------'
     O                                           72 '------------------------'
     O                                           96 '------------------------'
     O                                          120 '------------------------'
     O                                          132 '------------'
     O          E            HEAD           1
     O                                              'EMPLOYER    DIV NAME'
     O                                           65 'EFF DATE'
     O                                           81 'CNL DATE'
     O                                           92 'BILL CDE'
     O                                          105 'CUR POL YR'
     O                                          111 'AGT#'
     O                                         +  1 'AGENT NAME'
     O          E            HEAD           1
     O                                           23 'ADDRESS'
     O                                           64 'CONTACT'
     O          E            HEAD           1
     O                                           65 'PHONE # '
     O                                           79 'REGION'
     O                                           90 'COUNTY'
     O                                          105 'EXP MOD'
     O                                          119 'EFF DATE  '
     O          E            HEAD           0
     O                                           24 '------------------------'
     O                                           48 '------------------------'
     O                                           72 '------------------------'
     O                                           96 '------------------------'
     O                                          120 '------------------------'
     O                                          132 '------------'
     O          E            DETL2          1
     O                                           90 '***   NO RECORDS MEET'
     O                                         +  1 'SELECTION CRITERIA   ***'
     O          E            DET         2  1
     O                       XXEMP         ZB    09
     O                       XXDIV         ZB    15
     O                       XXNAM          B    50
     O                       XXEFDT         B    64 '  /  /  '
     O                       XXCXDT         B    81 '  /  /  '
     O                       ELBSCD        ZB    86
     O                       BSSEFL         B    88
     O                       XXFYR         Z    105
     O                       XXAG#         ZB   111
     O                       XXANAM         B   132
     O          E            DET            1
     O                       XXAD1          B    56
     O                       XXATN          B    97
     O          E            DET            1
     O                       XXAD2          B    56
     O                       XXAREA         B    62
     O                       EMPHN#         B    70 '   -    '
     O                       XXRGN          B    76 '   '
     O                       XXCNTY         B    88
     O                       XXEM1         KB   106
     O                       XXDT1          B   117 '  /  /  '
     O          E            DET2           1
     O                       XXAD3          B    56
     O                       XXEM2         KB   106
     O                       XXDT2          B   117 '  /  /  '
     O          E            DET3           1
     O                       XXEM3         KB   106
     O                       XXDT3          B   117 '  /  /  '
     O          E            TOTL        1  2
     O                                           38 'COUNT OF ACTIVE'
     O                                           49 'EMPLOYERS:'
     O                       XXACT               55 '     '
     O                                           90 'COUNT OF CANCELLED'
     O                                          102 'EMPLOYERS:'
     O                       XXCXL              113 '     '
