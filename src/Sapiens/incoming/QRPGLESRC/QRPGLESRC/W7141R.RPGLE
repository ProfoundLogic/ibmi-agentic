     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7141R - EMPLOYER LIST
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
     H* 04/10/00 C:118 PROJECT: WC987                                 *
     H*                LOAD ALTERNATE COMPANY NAME FROM FUND MASTER   *
     H*                IF AVAILABLE.                                  *
     H*                                                               *
     H* 04/24/98 C:78 PROJECT WC745                                   *
     H*               CORRECT PROJ DONE ON 09/08/97. F2CT01 REPLACED  *
     H*               WITH WMFNP/FNCGFL.                              *
     H*                                                               *
     H**** PLACE NEW REVISION REMARKS ON TOP **************************
     H*                                                               *
     H**  1.  CORRECTED REPORT PROBLEM ASSOCIATED WITH TESTING FOR   **
     H**      A VALID EMPLOYER.                                      **
     H**   1. CHANGED TOTALS ON # OF ACTIVE ACCTS AND # OF CANCELLED **
     H**      ACCTS TO REFLECT THE FYR FOR WHICH THE REPORT IS       **
     H**      SUBMITTED.                                             **
     H*                                                               *
     H*  02/18/93 C:12     PROJ: WC082                                *
     H*                CORRECTED THE CALCULATION FOR ACTIVE AND       *
     H*                CANCELLED EMPLOYERS.                           *
     H*                                                               *
     H*  11/29/95 C:33 PROJECT CM010                                  *
     H*                ON NON-COMMON EXPIRATION DATE POLICIES, WHEN   *
     H*                CHECKING IF AN ACCOUNT IS CANCELLED, LOAD THE  *
     H*                CANCEL DATE IF IT IS LESS THAN ELPFDT + 1 YR.  *
     H*                                                               *
     H* 09/12/96 C:65  PROJECT: NONE                                  *
     H*                FIXED ERROR FROM PROJECT CM010 ON 11/29/95     *
     H*                BECAUSE IF FIRST EMPLOYER IS CANCELLED IN      *
     H*                A COMMON FUND, IT WILL BE COUNTED AS ACTIVE.   *
     H*                                                               *
     H* 09/08/97  C:78  PROJ:WC745                                    *
     H*                 F2CT01 REPLACED WITH WMFNP/FNCGFL             *
     H*                                                               *
     H*****************************************************************
     FWMEML     IF   E           K DISK
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      * Employer Contact
     FWMECL1    IF   E           K DISK
      ***** END ADD ******* 01/31/02 **********************************
     FWDELL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     F**** BEGIN DEL *** 04/24/98 *************************************
     F***** WDF2L   IF  E           K        DISK
     F**** END   DEL *** 04/24/98 *************************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D SZ              S              1    DIM(13)
     D ST              S              2    DIM(1)
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
     D  XXENYR                 2      3  0
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
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQMODE        XXMODE            1
     C*                                                         "
     C*   SET-UP AND INITIALIZE                                 "
     C*                                                         "
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09
     C                   ELSE
     C                   SETON                                        08
     C                   END
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
     C                   KFLD                    XXFYR
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
     C***** BEGIN DEL ***** 04/24/98 **********************************
     C*
     C*   KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C****       F2KY      KLIST
     C****                 KFLD           XXCO#
     C****                 KFLD           XXFND
     C***** END   DEL ***** 04/24/98 **********************************
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C                   MOVE      UPPRDT        XXDATE
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'HRCVTDAT'    DTMDY
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
     C*  LOAD FLAG THAT DETERMINES WHETHER OR NOT THIS FUND WAS SET-UP
     C*  FOR NON-COMMON EXPIRATION DATE POLICIES
     C*
     C*   CHAIN TO THE FUND FILE AND GET FNCGFL FLAG
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C******* BEGIN ADD ***** 04/24/98 ********************************
     C   60              MOVE      *BLANK        FNCGFL
     C******* END   ADD ***** 04/24/98 ********************************
     C******* BEGIN DEL ***** 04/24/98 ********************************
     C****                 MOVE *BLANK    XXFCT1  1
     C****       F2KY      CHAINPF201                60
      ****** BEGIN CHG ****** 09/08/97 ********************************
     C**N60****************MOVELF2CT01    XXFCT1
     C**N60******          MOVELFNCGFL    XXFCT1
      ****** END   CHG ****** 09/08/97 ********************************
     C******* END   DEL ***** 04/24/98 ********************************
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF
     C                   ENDIF
     C***** END   ADD **** 04/10/00 ***********************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C***********                   MOVE      '0'           XXPRT             1
     C***********     ORKEY         CHAIN     POR01
     C***********     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
     C***********        EXCEPT    ROUTE
     C***********        END
      ***** End Del ***** 08/26/14 *********************************************
     C                   EXCEPT    HEAD
     C     EMKEY         SETLL     PEM01
     C     EMKEY         READE     PEM01                                  60
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
     C*
     C                   MOVE      XXFYR         YEAR              4
     C     XXFYR         IFGT      100
     C                   MOVEL     '20'          YEAR              4
     C                   ELSE
     C                   MOVEL     '19'          YEAR
     C                   END
     C*
     C***  FIRST GET THE MOST CURRENT "ACTIVE" AGENT HISTORY RECORD.
     C*
     C     AHKEY         SETGT     PAH01
     C                   READP     PAH01                                  58
     C     *IN58         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  58
     C                   END
     C*
     C***  TEST THE SELECTION CRITERIA.
     C*
     C     *IN58         IFEQ      '0'
     C     EMCO#         ANDEQ     AHCO#
     C     EMFND         ANDEQ     AHFND
     C     EMEMP#        ANDEQ     AHEMP#
     C     EMDIV         ANDEQ     AHDIV
     C     XXFYR         ANDEQ     AHFYR
     C*
     C***  GET THE FEIN & MOD FROM THE AGENT ASSIGNMENT
     C*
     C     AAKEY         CHAIN     PAA01                              54
     C*
     C***  AND FINALLY... GET THE AGENT FILE.
     C*
     C  N54AGKEY         CHAIN     PAG01                              54
     C*
     C  N54              MOVEL     AGNAME        XXANAM           30
     C  N54              MOVEL     AAAGT#        XXAG#             5 0
     C                   ELSE
     C                   MOVE      *BLANK        XXANAM
     C                   MOVE      *BLANK        XXAG#
     C                   END
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
     C***** BEGIN CHG ***** 04/24/98 **********************************
     C******     XXFCT1    IFEQ 'Y'
     C     FNCGFL        IFEQ      'Y'
     C***** END   CHG ***** 04/24/98 **********************************
     C                   Z-ADD     ELPFDT        XXENPO
     C                   ADD       1             XXENYR
     C                   ELSE
     C                   Z-ADD     F1ENPO        XXENPO
     C                   END
     C*
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
     C                   MOVE      ELEXM1        XXEM1             3 2
     C                   MOVE      ELEXM2        XXEM2             3 2
     C                   MOVE      ELEXM3        XXEM3             3 2
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
     C                   EXCEPT    DET2
     C                   END
     C   OF              SETON                                            22
     C*
     C*
     C                   END
     C*
     C     NEXT          TAG
     C*** READ THE NEXT EMPLOYEE
     C*
     C     EMKEY         READE     PEM01                                  60
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
     C                   ENDIF
      *
     C                   ENDSR
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
      ***** Beg Del ***** 08/26/14 *********************************************
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
      ***** End Del ***** 08/26/14 *********************************************
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
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O********* E            ROUTE          2
     O*********                                  19 'PROGRAM NAME:'
     O*********              ORPNAM              30
     O*********                                  36 'FILE:'
     O*********              ORFIL#              38
     O*********                                  46 'COPIES:'
     O*********              ORCOPY        Z     50
     O*********                                  56 'HOLD:'
     O*********              ORHOLD              61
     O********* E            ROUTE          2
     O*********                                  19 'OUTPUT QUEUE:'
     O*********      08      OROUTQ              30
     O*  MLS 1/15/88 ADD INDICATORS AND OUT QUEUE
     O*********         09   QQQUE               30
     O*********                                  37 'FORMS:'
     O*********              ORFORM              48
     O*********                                  56 'SAVE:'
     O*********              ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O***  BEGIN CHG  ***  01/28/10  *********************************
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                                51 '.1990'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      *****END   CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              85
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                           05 'FUND:'
     O                       FNFND         Z     09
     O                       FNNAME              70
     O          E            HEAD           1
     O                                           24 '------------------------'
     O                                           48 '------------------------'
     O                                           72 '------------------------'
     O                                           96 '------------------------'
     O                                          120 '------------------------'
     O                                          132 '------------'
     O          E            HEAD           1
     O                                           15 'EMPLR DIV NAME'
     O                                           59 'EFF DATE'
     O                                           70 'CNL DATE'
     O                                           80 'BILL CDE'
     O                                           92 'CUR FND YR'
     O                                           98 'AGENT'
     O          E            HEAD           1
     O                                           18 'ADDRESS'
     O                                           58 'CONTACT'
     O          E            HEAD           1
     O                                           59 'PHONE # '
     O                                           70 'RGN'
     O                                           78 'COUNTY'
     O                                           87 'EXP MOD'
     O                                           98 'EFF DATE  '
     O          E            HEAD           0
     O                                           24 '------------------------'
     O                                           48 '------------------------'
     O                                           72 '------------------------'
     O                                           96 '------------------------'
     O                                          120 '------------------------'
     O                                          132 '------------'
     O          E            DET         2  1
     O                       XXEMP         ZB    09
     O                       XXDIV         ZB    15
     O                       XXNAM          B    50
     O                       XXEFDT         B    59 '  /  /  '
     O                       XXCXDT         B    70 '  /  /  '
     O                       ELBSCD        ZB    76
     O                       BSSEFL         B    78
     O                       YEAR                86
     O                       XXAG#         ZB    98
     O                       XXANAM         B   130
     O**
     O          E            DET            1
     O                       XXAD1          B    50
     O                       XXATN          B    91
     O**
     O          E            DET            1
     O                       XXAD2          B    50
     O                       XXAREA         B    56
     O                       EMPHN#         B    65 '   -    '
     O                       XXRGN          B    70 '   '
     O                       XXCNTY         B    77
     O                       XXEM1         KB    85
     O                       XXDT1          B    95 '  /  /  '
     O                       XXEM2         KB   105
     O                       XXDT2          B   115 '  /  /  '
     O                       XXEM3         KB   122
     O                       XXDT3          B   132 '  /  /  '
     O**
     O          E            DET2           1
     O                       XXAD3          B    50
     O**
     O          E            TOTL        1  2
     O                                           38 'COUNT OF ACTIVE'
     O                                           49 'EMPLOYERS:'
     O                       XXACT               55 '     '
     O                                           90 'COUNT OF CANCELLED'
     O                                          102 'EMPLOYERS:'
     O                       XXCXL              113 '     '
