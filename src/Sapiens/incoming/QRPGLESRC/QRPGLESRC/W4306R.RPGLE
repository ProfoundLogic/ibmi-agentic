     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE W4306R  - ASSIGN/CHANGE EMPLOYER STAFF CODE & REPORT
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION:                                         *
     H*     THIS PROGRAM ASSIGNS OR CHANGES STAFF CODES ACCORDING TO  *
     H*     ANY OF THE FOLLOWING RANGES: EMPLOYER ALPHA, EMPLOYER #   *
     H*     CHAPTER, REGION, EMPLOYER ZIP, COUNTY, ESTIMATED ANNUAL   *
     H*     PREMIUM, AGENT ALPHA, AGENT #, AND AGENT ZIP. STAFF CODES *
     H*     ARE: UNDERWRITER, POLICY SERVICE REP., PREMIUM ACCOUNT REP*
     H*     AUDIT REVIEWER, AUDIT MANAGER, AND LOSS CONTROL REP.      *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     W4306C - OVERRIDE CL                                      *
     H*  CALLS TO:                                                    *
     H*     NONE                                                      *
     H*                                                               *
     H*  PROGRAMMER: MARIE NGUYEN                                     *
     H*  CREATED:    06/14/93               PROJECT: AVI              *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 11/22/13  C:190  Project: 129WSB                              *
     H*                  Process From and To claims.                  *
     H*                                                               *
     H* 12/23/11  C:279  Support: 78800                               *
     H*                  Allow 'From' field to be blank.              *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 08/21/08 C:191 Project 102WEP                                 *
     H*                Back to Base: Base Option                      *
     H*                 - Emplr entry: default field rep, rgn, chapter*
     H*                                                               *
     H* 05/16/02 C:61  SUPPORT WA093 32544                            *
     H*                Correct print of "TO" user ID code on output.  *
     H*                                                               *
     H*  07/24/98 C:61 PROJECT 60ARN                                  *
     H*              - Correct update of staff codes                  *
     H*                                                               *
     H*  01/12/98 C:79 PROJECT WC707                                  *
     H*                IF EMPLOYER MAILING ZIP AND PHYSICAL ZIP ARE   *
     H*                BOTH ZERO THEN THE RECORD IS NOT SELECTED EVEN *
     H*                IF THE "FROM" ZIP = ZERO.                      *
     H*                                                               *
     H*  10/30/97 C:79 PROJECT WC707                                  *
     H*                V3 IMPLEMENTATION                              *
     H*                                                               *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*   SMCOL   PCO01: CO                                   Company *
     H*   SMORL   POR01: PGM/PRT                             OVERRIDE *
     H*   WMFNL   PFN01: CO/FND                          GROUP Master *
     H*   WMUPL   PUP01: USER                               User Parm *
     H*   WMEML   PEM01: CO/FND/EMP                   EMPLOYER MASTER *
     H*   WMEML1  PEM02: CO/FND/NAME                  EMPLOYER MASTER *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  08  MODE - BATCH MODE                                        *
     H*  09  MODE - DEMAND MODE                                       *
     H*  30  PROCESS CHANGE STAFF CODES. FROM CODE MATCHES            *
     H* N30  DO NOT PROCESS CHANGE STAFF CODES. FROM CODE DOESNT MATCH*
     H*  31  STAFF CODE FOR THIS EMPLOYER WAS CHANGED                 *
     H* N31  STAFF CODE FOR THIS EMPLOYER WAS NOT CHANGED             *
     H*  40  PARTICULAR FUNDS WERE SELECTED                           *
     H* N40  ALL FUNDS WERE SELECTED                                  *
     H*  80  AN EMPLOYER ALPHA RANGE WAS ENTERED                      *
     H*  81  AN EMPLOYER NUMBER RANGE WAS ENTERED                     *
     H*  82  A CHAPTER RANGE WAS ENTERED                              *
     H*  83  A REGION RANGE WAS ENTERED                               *
     H*  84  AN EMPLOYER ZIP CODE RANGE WAS ENTERED                   *
     H*  85  AN EMPLOYER COUNTY CODE RANGE WAS ENTERED                *
     H*  86  AN EMPLOYER ANNUAL PREMIUM RANGE WAS ENTERED             *
     H*  87  AN AGENT ALPHA RANGE WAS ENTERED                         *
     H*  88  AN AGENT NUMBER RANGE WAS ENTERED                        *
     H*  89  AN AGENT ZIP CODE RANGE WAS ENTERED                      *
     H*  60  GENERAL CHAIN                                            *
     H* N99  ASSIGN STAFF CODES                                       *
     H*  99  CHANGE STAFF CODES                                       *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F** BEG ADD *** 01/12/98 *****************************************
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     F** END ADD *** 01/12/98 *****************************************
      * EMPLOYER MASTER LOGICAL (CO#/FND/EMP#/DIV)
     FWMEML     UF   E           K DISK
      * EMPLOYER DETAIL FILE (CO#/FND/EMP#/DIV/FYR)
     FWDELL     IF   E           K DISK
      * AGENT HISTORY MASTER FILE (CO#/FND/EMP#/DIV/FYR/EFDT/AGT#)
     FWMAHL     IF   E           K DISK
      * AGENT ASSIGNMENT MASTER FILE (CO#/FND/AGT#)
     FWMAAL1    IF   E           K DISK
      * AGENT MASTER FILE (CO#/FEIN/MOD)
     FWMAGL     IF   E           K DISK
      * DIARY TRANSFER FILE (CO#/FND/EMP#/DIV)
     F*** BEG ADD ****** 08/21/08 ************************************
     F*  City/Demographic Cross Reference file (CO#/LREP)
     FWMCYL     IF   E           K DISK
     F*** END ADD ****** 08/21/08 ************************************
     FWADTL1    O  A E           K DISK
      * OUTPUT PRINTER FILE
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*================================================================
     *** beg chg *** 11/22/13 *****************************************
     D********** FCD             S              3    DIM(6)                     FROM & TO STAFF CODE
     D********** TCD             S              3    DIM(6)
     D FCD             S              3    DIM(7)                               FROM & TO STAFF CODE
     D TCD             S              3    DIM(7)
     *** end chg *** 11/22/13 *****************************************
     D FD              S              3  0 DIM(10)                              GROUP ARRAY
     D QQFD            S              3    DIM(10)                              GROUP ARRAY
     *** beg chg *** 11/22/13 *****************************************
     D*CTG****         S              1    DIM(6) CTDATA PERRCD(6)              CATEGORY      F CODE
     D******** FRCD            S             56    DIM(6)                        STAFF CODE NAME
     D******** TOCD            S             56    DIM(6)                        STAFF CODE NAME
     D CTG             S              1    DIM(7) CTDATA PERRCD(6)              CATEGORY      F CODE
     D FRCD            S             56    DIM(7)                               STAFF CODE NAME
     D TOCD            S             56    DIM(7)                               STAFF CODE NAME
     *** end chg *** 11/22/13 *****************************************
     D** END ADD *** 01/12/98 **************************************************
     D*================================================================
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D ACT             C                   CONST('A - ACTIVE')
     D CAN             C                   CONST('C - CANCELLED')
     D BTH             C                   CONST('B - BOTH')
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D*
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
      *
     D ASGN            C                   CONST('EMPLOYER STAFF CODE-
     D                                      ASSIGNMENT')
     D CHGN            C                   CONST('EMPLOYER STAFF CODE-
     D                                      CHANGES')
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *================================================================
      *  COMPANY MASTER FILE
      *

     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      *  USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     *** BEG ADD *** 01/12/98 *****************************************
      *
      *  USER PARAMETER FILE
      *
     C     UPKY1         KLIST
     C                   KFLD                    XXSTAF
     *** END ADD *** 01/12/98 *****************************************
      *
      *  GROUP MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *   KEY LIST FOR THE OVERRIDE FILE.
      *
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
      *
      *  EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      *  AGENT HISTORY MASTER FILE
      *
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      *  AGENT ASSIGNMENT MASTER FILE
      *
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
      *
      *  AGENT MASTER FILE
      *
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*** BEG ADD ****** 08/21/08 ************************************
     C*  City/Demographic Cross Reference
     C*
     C     CYKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EMLREP
     C*** END ADD ****** 08/21/08 ************************************
      *================================================================
      *  VARIABLE DEFINITIONS
      *
     C     *LIKE         DEFINE    FNNAME        XXFNAM
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMEMP#        XXFREN
     C     *LIKE         DEFINE    EMEMP#        XXTOEN
     C     *LIKE         DEFINE    EMCHAP        XXFCHP
     C     *LIKE         DEFINE    EMCHAP        XXTCHP
     C     *LIKE         DEFINE    EMRGN#        XXFRGN
     C     *LIKE         DEFINE    EMRGN#        XXTRGN
     C     *LIKE         DEFINE    EMPZIP        XXFEZP
     C     *LIKE         DEFINE    EMPZIP        XXTEZP
     C     *LIKE         DEFINE    EMEFD1        XXRNDT
     C     *LIKE         DEFINE    EMUWTR        XXFCDE
     C     *LIKE         DEFINE    EMUWTR        XXTCDE
     C     *LIKE         DEFINE    ELENMP        XXENMP
     C     *LIKE         DEFINE    AGMZIP        XXFAZP
     C     *LIKE         DEFINE    AGMZIP        XXTAZP
     C     *LIKE         DEFINE    AAAGT#        XXAGT#
     C     *LIKE         DEFINE    AAAGT#        XXFRAN
     C     *LIKE         DEFINE    AAAGT#        XXTOAN
     C     *LIKE         DEFINE    AGFEIN        XXFEIN
     C     *LIKE         DEFINE    AGMOD         XXMOD
     C     *LIKE         DEFINE    DTCTGY        XXCTGY
     *** BEG ADD *** 01/12/98 *****************************************
     C     *LIKE         DEFINE    EMPZIP        XXZIP
     C     *LIKE         DEFINE    UPSTAF        XXSTAF
     *** END ADD *** 01/12/98 *****************************************
      *================================================================
     C                   MOVE      *BLANK        XXOUT             1            Y/N FLAG
      *================================================================
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFUWR            3
     C                   PARM                    QQTUWR            3
     C                   PARM                    QQFPSV            3
     C                   PARM                    QQTPSV            3
     C                   PARM                    QQFPAC            3
     C                   PARM                    QQTPAC            3
     C                   PARM                    QQFARV            3
     C                   PARM                    QQTARV            3
     C                   PARM                    QQFAMG            3
     C                   PARM                    QQTAMG            3
     C                   PARM                    QQFLRP            3
     C                   PARM                    QQTLRP            3
     C                   PARM                    QQACST            1
     C                   PARM                    QQFREN            9
     C                   PARM                    QQTOEN            9
     C                   PARM                    QQFREA            2
     C                   PARM                    QQTOEA            2
     C                   PARM                    QQFCHP            3
     C                   PARM                    QQTCHP            3
     C                   PARM                    QQFRGN            3
     C                   PARM                    QQTRGN            3
     C                   PARM                    QQFEZP            9
     C                   PARM                    QQTEZP            9
     C                   PARM                    QQFCTY            4
     C                   PARM                    QQTCTY            4
     C                   PARM                    QQFANP           11
     C                   PARM                    QQTANP           11
     C                   PARM                    QQFRAA            2
     C                   PARM                    QQTOAA            2
     C                   PARM                    QQFRAN            5
     C                   PARM                    QQTOAN            5
     C                   PARM                    QQFAZP            9
     C                   PARM                    QQTAZP            9
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQCHG             1
     C                   PARM                    QQFD
     *** beg add *** 11/22/13 *****************************************
     C                   PARM                    qqClmCFrom        3
     C                   PARM                    qqClmConTo        3
     *** end add *** 11/22/13 *****************************************
      *
     C     DTMDY8        PLIST
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
      *================================================================
      *  LOAD PARMS TO WORK FIELDS
      *
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFREN        XXFREN
     C                   MOVE      QQTOEN        XXTOEN
     C                   MOVE      QQFREA        XXFREA            2
     C                   MOVE      QQTOEA        XXTOEA            2
     C                   MOVE      QQFCHP        XXFCHP
     C                   MOVE      QQTCHP        XXTCHP
     C                   MOVE      QQFRGN        XXFRGN
     C                   MOVE      QQTRGN        XXTRGN
     C                   MOVE      QQFEZP        XXFEZP
     C                   MOVE      QQTEZP        XXTEZP
     C                   MOVE      QQFCTY        XXFCTY            4
     C                   MOVE      QQTCTY        XXTCTY            4
     C                   MOVE      QQFANP        XXFANP           11 2
     C                   MOVE      QQTANP        XXTANP           11 2
     C                   MOVE      QQFRAA        XXFRAA            2
     C                   MOVE      QQTOAA        XXTOAA            2
     C                   MOVE      QQFRAN        XXFRAN
     C                   MOVE      QQTOAN        XXTOAN
     C                   MOVE      QQFAZP        XXFAZP
     C                   MOVE      QQTAZP        XXTAZP
      *
      *  MOVE THE ALPHA FUND ARRAY TO NUMERIC ARRAY
      *
     C                   DO        10            X                 2 0
      *
     C                   MOVE      QQFD(X)       FD(X)
      *
     C                   ENDDO
      *================================================================
      *  WAS AN EMPLOYER NAME OR NUMBER RANGE ENTERED
      *
     C     XXTOEA        IFNE      'ZZ'                                                     S
     C     XXFREA        ORNE      *BLANKS                                                  S
      *
     C                   MOVE      *ON           *IN80                                      S
      *
     C                   ENDIF                                                  XXTOEA = ZZ
      *
     C     XXTOEN        IFNE      *HIVAL                                                   S
     C     XXFREN        ORNE      *ZEROS                                                   S
      *
     C                   MOVE      *ON           *IN81                                      S
      *
     C                   ENDIF                                                  XXTOEN = 99999999999
      *================================================================
      *  WAS A CHAPTER RANGE ENTERED
      *
     C     XXTCHP        IFNE      *HIVAL                                                   S
     C     XXFCHP        ORNE      *ZEROS                                                   S
      *
     C                   MOVE      *ON           *IN82                                      S
      *
     C                   ENDIF                                                  XXTCHP NE 999
      *================================================================
      *  WAS AN EMPLOYER REGION RANGE ENTERED
      *
     C     XXTRGN        IFNE      *HIVAL                                                   S
     C     XXFRGN        ORNE      *ZEROS                                                   S
      *
     C                   MOVE      *ON           *IN83                                      S
      *
     C                   ENDIF                                                  XXTRGN NE HIVAL
      *================================================================
      *  WAS AN EMPLOYER ZIP CODE RANGE ENTERED
      *
     C     XXTEZP        IFNE      *HIVAL                                                   S
     C     XXFEZP        ORNE      *ZEROS                                                   S
      *
     C                   MOVE      *ON           *IN84                                      S
      *
     C                   ENDIF                                                  XXTEZP NE 999
      *================================================================
      *  WAS A COUNTY CODE RANGE ENTERED
      *
     C     XXTCTY        IFNE      'ZZ99'                                                   S
     C     XXFCTY        ORNE      *BLANKS                                                  S
      *
     C                   MOVE      *ON           *IN85                                      S
      *
     C                   ENDIF                                                  XXTCTY NE ZZ99
      *================================================================
      *  WAS A PREMIUM RANGE ENTERED
      *
     C     XXTANP        IFNE      *HIVAL                                                   S
     C     XXFANP        ORNE      *ZEROS                                                   S
      *
     C                   MOVE      *ON           *IN86                                      S
      *
     C                   ENDIF                                                  XXTANP NE HIVAL
      *================================================================
      *  WAS AN AGENT NAME RANGE ENTERED
      *
     C     XXTOAA        IFNE      'ZZ'                                                     S
     C     XXFRAA        ORNE      *BLANKS                                                  S
      *
     C                   MOVE      *ON           *IN87                                      S
      *
     C                   ENDIF                                                  XXTOAA NE ZZ
      *================================================================
      *  WAS AN AGENT NUMBER RANGE ENTERED
      *
     C     XXTOAN        IFNE      *HIVAL                                                   S
     C     XXFRAN        ORNE      *ZEROS                                                   S
      *
     C                   MOVE      *ON           *IN88                                      S
      *
     C                   ENDIF                                                  XXTOAN NE *HIVAL
      *================================================================
      *  WAS AN AGENT ZIP CODE RANGE ENTERED
      *
     C     XXTAZP        IFNE      *HIVAL                                                   S
     C     XXFAZP        ORNE      *ZEROS                                                   S
      *
     C                   MOVE      *ON           *IN89                                      S
      *
     C                   ENDIF                                                  XXTAZP NE *HIVAL
      *================================================================
     C     QQCHG         COMP      '2'                                    99     CHANGE CODES
      *
     C                   TIME                    DSDTTM
      *
      *  CONVERT THE DATE TO CENTURY/YEAR/MONTH/DAY
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
      *
      *  CONVERT THE DATE TO MONTH/DAY/CENTURY-YEAR
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         XXDATE            8 0
      *
     C     QQMODE        IFEQ      'D'
     C                   MOVE      *ON           *IN09                          DEMAND MODE
      *
     C                   ELSE
     C                   MOVE      *ON           *IN08                          BATCH MODE
      *
     C                   END
      *
      *  RUN FOR ALL FUNDS OR SELECTED FUNDS
      *
     C     QQFD(1)       COMP      *BLANK                             4040      SELECTED FUNDS
      *
      *================================================================
      *   GET USER, COMPANY AND GROUP INFORMATION
      *
     C     UPKY          CHAIN     PUP01                              60
     C     COKY          CHAIN     PCO01                              60
      *
      *  REMOVE LEADING BLANKS & CENTER NAME
      *
     C                   MOVEL(P)  CONAME        ##DATA
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
      *
     C                   MOVEL     ##DATA        XXCONM           40
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
      *
      *   GET THE DISTRIBUTION INFORMATION AND PRINT
      *
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
      *
     C                   SELECT
      *
     C     QQACST        WHENEQ    'A'
     C                   MOVEL(P)  ACT           ZZACST           13
      *
     C     QQACST        WHENEQ    'C'
     C                   MOVEL(P)  CAN           ZZACST           13
      *
     C     QQACST        WHENEQ    'B'
     C                   MOVEL(P)  BTH           ZZACST           13
      *
     C                   ENDSL
      *
      *   PRINT THE DISTRIBUTION INFORMATION, IF A VALID RECORD IS FOUND.
      *
     C     *IN60         IFEQ      '0'
     C     ORSTAT        ANDEQ     'A'
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
      *
      *  LOAD STAFF CODE ARRAYS
      *
     C                   MOVEL     QQFUWR        FCD(1)
     C                   MOVEL     QQFPSV        FCD(2)
     C                   MOVEL     QQFPAC        FCD(3)
     C                   MOVEL     QQFARV        FCD(4)
     C                   MOVEL     QQFAMG        FCD(5)
     C                   MOVEL     QQFLRP        FCD(6)
     *** beg add *** 11/22/13 *****************************************
     C                   MOVEL     qqClmCFrom    FCD(7)
     *** end add *** 11/22/13 *****************************************
     C                   MOVEL     QQTUWR        TCD(1)
     C                   MOVEL     QQTPSV        TCD(2)
     C                   MOVEL     QQTPAC        TCD(3)
     C                   MOVEL     QQTARV        TCD(4)
     C                   MOVEL     QQTAMG        TCD(5)
     C                   MOVEL     QQTLRP        TCD(6)
     *** beg add *** 11/22/13 *****************************************
     C                   MOVEL     qqClmConTo    TCD(7)
     *** end add *** 11/22/13 *****************************************
     *** beg chg *** 11/22/13 *****************************************
     C**********         DO        6             I                 2 0
     c                   DO        7             I                 2 0
     *** end chg *** 11/22/13 *****************************************
      *
      *  LOAD "FROM" STAFF CODE NAME
      *
     C     FCD(I)        IFNE      *BLANK
     C                   MOVEL     FCD(I)        XXSTAF
      *
      *  CONCATENATE USER NAME TO STAFF CODE
      *
     C     UPKY1         CHAIN     PUP02                              60
      *
     C     *IN60         IFEQ      *ZERO
      *
     C                   MOVEL     '('           FRCD(I)
     C                   CAT       FCD(I):0      FRCD(I)
     C                   CAT       ')':0         FRCD(I)
     C                   CAT       UPUSNM:1      FRCD(I)
      *
      *  CENTER NAME AND CODE
      *
     C                   MOVE(P)   FRCD(I)       ##DATA
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      56            ##LENG            3 0
      *
     C                   MOVEL     ##DATA        FRCD(I)
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
      *  LOAD "TO" STAFF CODE NAME
      *
     C     TCD(I)        IFNE      *BLANK
      *
     C                   MOVEL     TCD(I)        XXSTAF
      *
      *  CONCATENATE USER NAME TO STAFF CODE
      *
     C     UPKY1         CHAIN     PUP02                              60
      *
     C     *IN60         IFEQ      *ZERO
      *
     C                   MOVEL     '('           TOCD(I)
     C                   CAT       TCD(I):0      TOCD(I)
     C                   CAT       ')':0         TOCD(I)
     C                   CAT       UPUSNM:1      TOCD(I)
      *
      *  CENTER NAME AND CODE
      *
     C                   MOVE(P)   TOCD(I)       ##DATA
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      56            ##LENG            3 0
      *
     C                   MOVEL     ##DATA        TOCD(I)
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDDO
      *
      *****************************************************************
      *     BEGIN UPDATING EMPLOYER MASTER FILE
      *****************************************************************
      *  MAIN LOOP TO PROCESS ALL SIX STAFF ASSIGNMENTS/CHANGES
      *
     *** beg chg *** 11/22/13 *****************************************
     C**********         DO        6             X                 2 0
     C                   DO        7             X                 2 0
     *** end chg *** 11/22/13 *****************************************
      *
     C     FCD(X)        IFNE      *BLANKS
     C     FCD(X)        OREQ      *BLANKS
     C     TCD(X)        ANDNE     *BLANKS
      *
      *  SET UP FIELDS FOR DIARY TRANSFER
      *
     C                   MOVE      CTG(X)        XXCTGY
     C                   MOVE      FCD(X)        XXFCDE
     C                   MOVE      TCD(X)        XXTCDE
      *
     C                   MOVEA     '000000'      *IN(21)
     C     X             ADD       20            X1                2 0
     C                   MOVE      '1'           *IN(X1)
      *
      *  SET UP KLIST FOR SELECTED FUNDS OR ALL FUNDS
      *
     C     *IN40         IFEQ      *ON
      *
     C                   Z-ADD     1             Y                 2 0
     C                   MOVE      FD(Y)         XXFND
      *
      *  LOAD COMPANY AND FUND NAMES AND PRINT FIRST HEADER
      *
     C                   EXSR      SRGTNM
     C                   EXCEPT    HEAD
      *
     C     EMKY          CHAIN     PEM01                              60
      *
     C     *IN60         DOWEQ     *OFF
     C     Y             ANDLE     10
     C     XXFND         ANDNE     *ZERO
      *
      *  LOAD THE KEY LIST WITH NEW EMPLOYER AND DIVISION
      *
     C                   MOVE      EMEMP#        XXEMP#
     C                   MOVE      EMDIV         XXDIV
     C                   Z-ADD     *ZERO         XXZIP
      *
     C     EMPZIP        IFNE      *ZERO
      *
     C                   MOVE      EMPZIP        XXZIP
      *
     C                   ELSE
      *
     C                   MOVE      EMMZIP        XXZIP
      *
     C                   ENDIF                                                  EMPZIP NE 0
      *
      *  PROCESS SELECTED EMPLOYER RECORD USING SELECTED RANGES
      *
     C                   EXSR      SRSTFF
      *
      *  READ THE NEXT RECORD FOR THE SELECTED CO#
      *
     C     EMKY1         READE     PEM01                                  60
      *
     C     *IN60         IFEQ      *ON
      *
     C                   MOVE      *ZEROS        EMFND
      *
     C                   ENDIF                                                  *IN60=ON
      *
      *  PROCESS FOR A NEW GROUP
      *
     C     EMFND         IFNE      XXFND
      *
      *  NO EMPLOYERS SELECTED FOR THIS GROUP
      *
     C     *IN31         IFEQ      *OFF
      *
     C                   EXCEPT    ERRMSG
      *
     C                   ENDIF
      *
     C                   MOVE      '0'           *IN31
      *
     C                   ADD       1             Y
      *
      *  WAS THAT THE LAST ELEMENT IN THE GROUP ARRAY
      *
     C     Y             IFLE      10
      *
     C                   MOVE      FD(Y)         XXFND
      *
     C                   ENDIF                                                  Y LE 10
      *
     C     XXFND         IFNE      *ZERO
     C     Y             ANDLE     10
      *
      *  LOAD COMPANY AND FUND NAMES
      *
     C                   EXSR      SRGTNM
      *
     C                   EXCEPT    HEAD
      *
      *  CHAIN TO THE NEXT GROUP LEVEL
      *
     C     EMKY          CHAIN     PEM01                              60
      *
     C                   ENDIF                                                  XXFND NE 0
      *
     C                   ENDIF                                                  EMFND NE XXFND
      *
     C                   ENDDO                                                  *IN60=OFF
      *================================================================
      *  PROCESS ALL FUNDS
      *
     C                   ELSE
      *
      *  READ THE FIRST EMPLOYER MASTER RECORD FOR THE SELECTED CO#
      *
     C                   MOVE      *LOVAL        XXFND
      *
     C     EMKY          SETLL     PEM01
     C     EMKY1         READE     PEM01                                  60
      *
     C                   MOVE      EMFND         XXFND
      *
      *  LOAD COMPANY AND FUND NAMES AND PRINT FIRST HEADER
      *
     C                   EXSR      SRGTNM
     C                   EXCEPT    HEAD
      *
     C     *IN60         DOWEQ     *OFF
      *
      *  LOAD THE KEY LIST WITH NEW EMPLOYER AND DIVISION
      *
     C                   MOVE      EMEMP#        XXEMP#
     C                   MOVE      EMDIV         XXDIV
      *
      *  PROCESS SELECTED EMPLOYER RECORD USING SELECTED RANGES
      *
     C                   EXSR      SRSTFF
      *
     C     EMKY1         READE     PEM01                                  60
      *
     C     *IN60         IFEQ      *ON
      *
     C                   MOVE      *ZEROS        EMFND
      *
     C                   ENDIF                                                  *IN60=ON
      *
     C     EMFND         IFNE      XXFND
      *
      *  NO EMPLOYERS SELECTED FOR THIS GROUP
      *
     C     *IN31         IFEQ      *OFF
     C                   EXCEPT    ERRMSG
     C                   ENDIF
      *
     C                   MOVE      '0'           *IN31
      *
     C                   MOVE      EMFND         XXFND
      *
      *  IF NOT LAST GROUP THEN PRINT NEW GROUP HEADER
      *
     C     XXFND         IFNE      *ZERO
      *
      *  LOAD COMPANY AND FUND NAMES
      *
     C                   EXSR      SRGTNM
      *
     C                   EXCEPT    HEAD
      *
     C                   ENDIF                                                  XXFND NE 0
      *
     C                   ENDIF                                                  EMFND NE XXFND
      *
     C                   ENDDO                                                  *IN60=OFF
      *
     C                   ENDIF                                                  *IN40=ON
      *
     C                   END                                                    IF FCD,X <> BLANK
      *
     C                   END                                                    DO 4
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
      *================================================================
      *  SUBROUTINE SRSTFF- UPDATE STAFF CODES IF RECORD MEETS
      *                     SELECTION CRITERIA
      *================================================================
     C     SRSTFF        BEGSR                                                              UPDATED
      *
     C                   MOVE      '0'           *IN30                          N30=EMP NOT UPDATED
     C                   MOVE      'N'           XXOUT                          EMP WITHIN RANGE
      *
      *  GET CURRENT CANCELLATION DATE BUCKET
      *
     C                   Z-ADD     1             C                 1 0
     C     *ZERO         LOOKUP    ED(C)                                  60
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     5             C
      *
     C                   ELSE
     C                   SUB       1             C
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C     C             DOWNE     *ZERO
      *
     C     XXRNDT        IFGE      ED(C)
     C                   LEAVE
     C                   ENDIF
      *
     C                   SUB       1             C
      *
     C                   ENDDO                                                  C DOWNE
      *
      *  SETUP THE ACCOUNT STATUS FLAGS
      *
     C     C             IFGT      *ZERO                                        ACTIVE
      *
     C     CD(C)         IFEQ      *ZERO                                        ACTIVE
     C                   MOVE      'A'           XXACST            1
     C                   ENDIF
      *
     C     CD(C)         IFNE      *ZERO                                        CANCELLED
     C     CD(C)         ANDGT     XXRNDT
      *
     C                   MOVE      'A'           XXACST
      *
     C                   ENDIF                                                  CD,C NE 0
      *
     C     CD(C)         IFNE      *ZERO                                        CANCELLED
     C     CD(C)         ANDLE     XXRNDT
      *
     C                   MOVE      'C'           XXACST
      *
     C                   ENDIF                                                  CD,C NE 0
      *
      *  NO EFFECTIVE DATES FOUND
      *
     C                   ELSE
     C                   MOVE      'I'           XXACST
      *
     C                   ENDIF                                                  C NE 0
      *
      *  BUILD THE COUNTY CODE FROM STATE AND CODE
      *
     C     EMPST         IFNE      *BLANK
     C     EMPST         CAT       EMCNTY:0      XX1CTY            4
      *
     C                   ELSE
     C     EMMST         CAT       EMCNTY:0      XX1CTY
      *
     C                   ENDIF                                                  EMPST NE BLANK
      *================================================================
      *
      *  WAS AN AGENT RANGE SELECTED
      *
     C     *IN87         IFEQ      *ON
     C     *IN88         OREQ      *ON
     C     *IN89         OREQ      *ON
      *
      *  FIND MOST RECENT ACTIVE AGENT NUMBER FOR SELECTED EMPLOYER
      *
     C     AHKY          SETGT     PAH01
     C     AHKY          READPE    PAH01                                  60
      *
     C     AHSTAT        DOWNE     'A'
     C     *IN60         ANDEQ     *OFF
      *
     C     AHKY          READPE    PAH01                                  60
      *
     C                   ENDDO
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      AHAGT#        XXAGT#
      *
     C                   ELSE
     C                   MOVE      *ZEROS        XXAGT#
     C                   MOVE      'Y'           XXOUT
      *
     C                   ENDIF                                                  *IN60=OFF
      *================================================================
      *
      *  FIND AGENT NAME FOR SELECTED AGENT NUMBER
      *
     C     AAKY          CHAIN     PAA01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      AAFEIN        XXFEIN
     C                   MOVE      AAMOD         XXMOD
      *
     C                   ELSE                                                               NK
     C                   MOVE      *ZEROS        XXFEIN
     C                   MOVE      *ZEROS        XXMOD
      *
     C                   ENDIF                                                  *IN60 = 0N  NK
      *
     C     AGKY          CHAIN     PAG01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     AGNAME        XXAGNM            2
      *
     C                   ELSE                                                               NK
     C                   MOVE      'Y'           XXOUT
      *
     C                   ENDIF                                                  *IN60 = 0N  NK
      *
     C                   ENDIF                                                  *IN88=ON
      *================================================================
      *  FIND THE MOST CURRENT EST. ANNUAL PREMIUM FOR THE EMPLOYER
      *
      *  WAS AN ANNUAL PREMIUM RANGE SELECTED
      *
     C     *IN86         IFEQ      *ON
      *
     C     ELKY          SETGT     PEL01
     C     ELKY          READPE    PEL01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ELENMP        XXENMP
      *
     C                   ELSE                                                               NK
     C                   MOVE      'Y'           XXOUT
      *
     C                   ENDIF                                                  *IN60 = 0N  NK
      *
     C                   ENDIF                                                  *IN86=ON
      *================================================================E
      *  IS THIS RECORD WITHIN THE SELECTED RANGES                     E
      *                                                                E
      *  EMPLOYER RANGES                                               E
      *                                                                E
     C     QQACST        IFNE      'B'
      *                                                                E
     C     QQACST        IFNE      XXACST
      *                                                                E
     C                   MOVE      'Y'           XXOUT
      *                                                                E
     C                   ENDIF                                                  QQACST NE XXACST
      *                                                                E
     C                   ENDIF                                                  QQACST NE B
      *                                                                E
     C     *IN80         IFEQ      *ON
      *                                                                E
      *  CONVERT 1ST TWO CHARS OF EMP NAME TO UPPER CASE
      *
     C     LO:UP         XLATE     EMNAME        XXEMNM            2
      *                                                                E
     C     XXEMNM        IFLT      XXFREA
     C     XXEMNM        ORGT      XXTOEA
      *                                                                E
     C                   MOVE      'Y'           XXOUT
      *                                                                E
     C                   ENDIF                                                  EMEMNM LT XXFREA
      *                                                                E
     C                   ENDIF                                                  *IN82=ON
      *                                                                E
     C     *IN81         IFEQ      *ON
      *                                                                E
     C     EMEMP#        IFLT      XXFREN
     C     EMEMP#        ORGT      XXTOEN
      *                                                                E
     C                   MOVE      'Y'           XXOUT
      *                                                                E
     C                   ENDIF                                                  EMEMP# LT XXFREN
      *                                                                E
     C                   ENDIF                                                  *IN81=ON
      *                                                                E
     C     *IN82         IFEQ      *ON
      *                                                                E
     C     EMCHAP        IFLT      XXFCHP
     C     EMCHAP        ORGT      XXTCHP
      *                                                                E
     C                   MOVE      'Y'           XXOUT
      *                                                                E
     C                   ENDIF                                                  EMCHAP LT XXFCHP
      *                                                                E
     C                   ENDIF                                                  *IN83=ON
      *                                                                E
     C     *IN83         IFEQ      *ON
      *                                                                E
     C     EMRGN#        IFLT      XXFRGN
     C     EMRGN#        ORGT      XXTRGN
      *                                                                E
     C                   MOVE      'Y'           XXOUT
      *                                                                E
     C                   ENDIF                                                  EMRGN# LT XXFRGN
      *                                                                E
     C                   ENDIF                                                  *IN84=ON
      *                                                                E
     C     *IN84         IFEQ      *ON
      *                                                                E
      *                                                                E
     C     XXZIP         IFEQ      *ZERO
     C     XXZIP         ORLT      XXFEZP
     C     XXZIP         ORGT      XXTEZP
      *                                                                E
     C                   MOVE      'Y'           XXOUT
      *                                                                E
     C                   ENDIF                                                  XXZIPP EQ 0 FEZP
      *                                                                E
     C                   ENDIF                                                  *IN85=ON
      *                                                                E
     C     *IN85         IFEQ      *ON
      *                                                                E
     C     XX1CTY        IFLT      XXFCTY
     C     XX1CTY        ORGT      XXTCTY
      *                                                                E
     C                   MOVE      'Y'           XXOUT
      *                                                                E
     C                   ENDIF                                                  XX1CTY LT XXFCTY
      *                                                                E
     C                   ENDIF                                                  *IN86=ON
      *                                                                E
     C     *IN86         IFEQ      *ON
      *                                                                E
     C     XXENMP        IFLT      XXFANP
     C     XXENMP        ORGT      XXTANP
      *                                                                E
     C                   MOVE      'Y'           XXOUT
      *                                                                E
     C                   ENDIF                                                  XXENMP LT XXFANP
      *                                                                E
     C                   ENDIF                                                  *IN87=ON
      *                                                                E
      *
      *  AGENT RANGES                                                  E
      *                                                                E
     C     *IN87         IFEQ      *ON
      *                                                                E
      *  CONVERT 1ST TWO CHARS OF EMP NAME TO UPPER CASE
      *
     C     LO:UP         XLATE     XXAGNM        XXAGNM            2
      *                                                                E
     C     XXAGNM        IFLT      XXFRAA
     C     XXAGNM        ORGT      XXTOAA
      *                                                                E
     C                   MOVE      'Y'           XXOUT
      *                                                                E
     C                   ENDIF                                                  XXAGNM LT XXFRAA
      *                                                                E
     C                   ENDIF                                                  *IN87=ON
      *                                                                E
     C     *IN88         IFEQ      *ON
      *                                                                E
     C     XXAGT#        IFLT      XXFRAN
     C     XXAGT#        ORGT      XXTOAN
      *                                                                E
     C                   MOVE      'Y'           XXOUT
      *                                                                E
     C                   ENDIF                                                  XXAGT# LT XXFRAN
      *                                                                E
     C                   ENDIF                                                  *IN88=ON
      *                                                                E
     C     *IN89         IFEQ      *ON
      *                                                                E
     C     AGMZIP        IFLT      XXFAZP
     C     AGMZIP        ORGT      XXTAZP
      *                                                                E
     C                   MOVE      'Y'           XXOUT
      *                                                                E
     C                   ENDIF                                                  AGPZIP LT XXFAZP
      *                                                                E
     C                   ENDIF                                                  *IN89=ON
      *
      *  ASSIGN STAFF CODES
      *
     C     XXOUT         IFEQ      'N'                                          EMP WITHIN RANGE
      *
     C     *IN99         IFEQ      '0'                                          STAFF ASSIGNMENT
      *
     C   21              MOVEL     FCD(1)        EMUWTR
     C   22              MOVEL     FCD(2)        EMPSRV
     C   23              MOVEL     FCD(3)        EMPACC
     C   24              MOVEL     FCD(4)        EMARVW
     C   25              MOVEL     FCD(5)        EMAMGR
     C   26              MOVEL     FCD(6)        EMLREP
     *** beg add *** 11/22/13 *****************************************
     C   27              MOVEL     FCD(7)        EMclmc
     *** end add *** 11/22/13 *****************************************
     C*
     C*  If the Loss Control Rep is being assigned, also assign
     C*  the region code.
     C*
     C     *IN26         IFEQ      *ON
     C*
     C     CYKY          CHAIN     PCY01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CYRGN         EMRGN#
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN26=*ON
      *
      *  UPDATE MAINTENANCE FIELDS IN WMEMP
      *
     C                   EXSR      SRUPMT
      *
     C                   MOVE      '1'           *IN30
      *
     C   OF              EXCEPT    HEAD
      *
     C   OF              MOVE      '1'           *INOF
      *
     C                   EXCEPT    DETAIL
      *
     C                   ELSE                                                   STAFF CHANGE
      *
     C   21EMUWTR        COMP      FCD(1)                                 30
     C   22EMPSRV        COMP      FCD(2)                                 30
     C   23EMPACC        COMP      FCD(3)                                 30
     C   24EMARVW        COMP      FCD(4)                                 30
     C   25EMAMGR        COMP      FCD(5)                                 30
     C   26EMLREP        COMP      FCD(6)                                 30
     *** beg add *** 11/22/13 *****************************************
     C   27EMclmc        COMP      FCD(7)                                 30
     *** end add *** 11/22/13 *****************************************
      *
     C     *IN30         IFEQ      '1'
      *
     C   21              MOVEL     TCD(1)        EMUWTR
     C   22              MOVEL     TCD(2)        EMPSRV
     C   23              MOVEL     TCD(3)        EMPACC
     C   24              MOVEL     TCD(4)        EMARVW
     C   25              MOVEL     TCD(5)        EMAMGR
     C   26              MOVEL     TCD(6)        EMLREP
     *** beg add *** 11/22/13 *****************************************
     C   27              MOVEL     TCD(7)        EMclmc
     *** end add *** 11/22/13 *****************************************
     C*
     C*  If the Loss Control Rep is being reassigned, also reassign
     C*  the region code.
     C*
     C     *IN26         IFEQ      *ON
     C*
     C     CYKY          CHAIN     PCY01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CYRGN         EMRGN#
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  *IN26=*ON
      *
      *  UPDATE DIARY TRANSFER FIELDS
      *
     C                   EXSR      SRDIAR
      *
      *  UPDATE MAINTENANCE FIELDS IN WMEMP
      *
     C                   EXSR      SRUPMT
      *
     C   OF              EXCEPT    HEAD
      *
     C   OF              MOVE      *ON           *INOF
      *
     C                   EXCEPT    DETAIL
      *
     C                   END
      *
     C                   END                                                    IF *99
      *
     C                   END                                                    IF XXOUT = N
      *
     C  N30              EXCEPT    REM01
     C   30              MOVE      *ON           *IN31                          SOME EMP UPDATED/STA
      *
     C                   ENDSR
      *================================================================
      * SUBROUTINE SRGTNM- LOADS THE COMPANAY NAME AND FUND NAME
      *================================================================
      *
     C     SRGTNM        BEGSR
      *
      * USE ALTERNATE GROUP NAME IF FOUND IN GROUP MASTER
      *
     C     FNKY          CHAIN     PFN01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVEL     FNNAME        XXFNAM
      *
     C     FNANAM        IFNE      *BLANKS
      *
     C                   MOVEL     FNANAM        CONAME
      *
     C                   ENDIF                                                  FNANAM NE BLANK
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDSR
      *
      *================================================================
      * SUBROUTINE SRUPMT- UPDATES MAINTENCE FIELDS IN WMEMP
      *================================================================
      *
     C     SRUPMT        BEGSR
      *
     C                   MOVE      Q1USER        EMUSER
     C                   MOVE      Q1PGM         EMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EMTIME
      *
      *  CONVERT THE DATE TO CENTURY/YEAR/MONTH/DAY
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
      *
     C                   UPDATE    PEM01
      *
     C                   ENDSR
      *===============================================================*
      *  SUBROUTINE TO ADD A RECORD TO THE DIARY TRANSFER FILE        *
      *===============================================================*
      *
     C     SRDIAR        BEGSR
      *
      *  CHANGE MODE ONLY
      *
     C                   MOVE      EMCO#         DTCO#
     C                   MOVE      EMFND         DTFND
     C                   MOVE      EMEMP#        DTEMP#
     C                   MOVE      EMDIV         DTDIV
     C                   MOVE      XXCTGY        DTCTGY
     C                   MOVE      XXFCDE        DTOSTF
     C                   MOVE      XXTCDE        DTNSTF
     C                   MOVE      Q1USER        DTUSER
     C                   MOVE      Q1PGM         DTPGM
     C                   MOVE      EMDT          DTDT
     C                   MOVE      DSTIME        DTTIME
      *
     C                   WRITE     PDT01
      *
     C                   ENDSR
     O*************************************************************************
     OQSYSPRT   E            ROUTE          1 03
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
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
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O*
     O*    SELECTION CRITERIA
     O*
     O          E            ROUTE          1
     O                                           22 'EMPLOYER NAME RANGE  :'
     O                       XXFREA              34
     O                                           36 '-'
     O                       XXTOEA              50
     O          E            ROUTE          1
     O                                           22 'EMPLOYER NUMBER RANGE:'
     O                       XXFREN        3     34
     O                                           36 '-'
     O                       XXTOEN        3     50
     O          E            ROUTE          1
     O                                           22 'CHAPTER RANGE        :'
     O                       XXFCHP        3     34
     O                                           36 '-'
     O                       XXTCHP        3     50
     O          E            ROUTE          1
     O                                           22 'REGION RANGE         :'
     O                       XXFRGN        3     34
     O                                           36 '-'
     O                       XXTRGN        3     50
     O          E            ROUTE          1
     O                                           22 'EMPLOYER ZIP RANGE   :'
     O                       XXFEZP        3     34
     O                                           36 '-'
     O                       XXTEZP        3     50
     O          E            ROUTE          1
     O                                           22 'COUNTY RANGE         :'
     O                       XXFCTY              34
     O                                           36 '-'
     O                       XXTCTY              50
     O          E            ROUTE          1
     O                                           22 'EST. ANNUAL PREMIUM  :'
     O                       XXFANP              34 '         0.  '
     O                                           36 '-'
     O                       XXTANP              50 '         0.  '
     O          E            ROUTE          1
     O                                           22 'AGENT NAME RANGE     :'
     O                       XXFRAA              34
     O                                           36 '-'
     O                       XXTOAA              50
     O          E            ROUTE          1
     O                                           22 'AGENT NUMBER RANGE   :'
     O                       XXFRAN        3     34
     O                                           36 '-'
     O                       XXTOAN        3     50
     O          E            ROUTE          1
     O                                           22 'AGENT ZIP CODE RANGE :'
     O                       XXFAZP        3     34
     O                                           36 '-'
     O                       XXTAZP        3     50
     O          E            ROUTE       1  0
     O                                           22 'STAFF CODES           '
     O                  99                       33 'FROM    TO'
     O          E            ROUTE          2
     O                  99                       33 '____  ____'
     O          E            ROUTE          1
     O                                           22 'UNDERWRITER          :'
     O                       QQFUWR              27
     O                  99                       29 '-'
     O                  99   QQTUWR              33
     O          E            ROUTE          1
     O                                           22 'POLICY SERVICE REP   :'
     O                       QQFPSV              27
     O                  99                       29 '-'
     O                  99   QQTPSV              33
     O          E            ROUTE          1
     O                                           22 'PREMIUM ACCT REP     :'
     O                       QQFPAC              27
     O                  99                       29 '-'
     O                  99   QQTPAC              33
     O          E            ROUTE          1
     O                                           22 'AUDIT REVIEWER       :'
     O                       QQFARV              27
     O                  99                       29 '-'
     O                  99   QQTARV              33
     O*
     O          E            ROUTE          1
     O                                           22 'AUDIT MANAGER        :'
     O                       QQFAMG              27
     O                  99                       29 '-'
     O                  99   QQTAMG              33
     O*
     O          E            ROUTE          2
     O                                           22 'LOSS CONTROL REP     :'
     O                       QQFLRP              27
     O                  99                       29 '-'
     O                  99   QQTLRP              33
     O          E            ROUTE          0
     O                                           22 'FUNDS SELECTED       :'
     O          E            ROUTE          0
     O                  40   FD(1)         Z     26
     O                 N40                       27 '*ALL'
     O          E            ROUTE          0
     O                  40   FD(2)         Z     30
     O          E            ROUTE          0
     O                  40   FD(3)         Z     34
     O          E            ROUTE          0
     O                  40   FD(4)         Z     38
     O          E            ROUTE          0
     O                  40   FD(5)         Z     42
     O          E            ROUTE          0
     O                  40   FD(6)         Z     46
     O          E            ROUTE          0
     O                  40   FD(7)         Z     50
     O          E            ROUTE          0
     O                  40   FD(8)         Z     54
     O          E            ROUTE          0
     O                  40   FD(9)         Z     58
     O          E            ROUTE          0
     O                  40   FD(10)        Z     62
     O          E            ROUTE       2
     O                                           22 'ACCOUNT STATUS       :'
     O                       ZZACST              36
     O*
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O****************************************************************
     O**REPORT DETAIL
     O****************************************************************
     O*
     O          E            HEAD           1 06
     O                       DSTIME              10 '  :  :  '
     O                       XXCONM              85
     O                       XXDATE        Y    130
     O          E            HEAD           1
     O                       Q1PGM               12
     O                 N99   ASGN                80
     O                  99   CHGN                78
     O                                          124 'PAGE:'
     O                       PAGE          Z    130
     O**
     O** STAFF CODE HEADER
     O**
     O          E    21      HEAD           2
     O                                           70 'UNDERWRITER'
     O          E    21      HEAD           1
     O                       FRCD(1)             92
     O          E    21 99   HEAD           1
     O                                           65 'TO'
     O          E    21 99   HEAD           1
     O                       TOCD(1)             92
     O          E    22      HEAD           2
     O                                           75 'POLICY SERVICE REP'
     O          E    22      HEAD           1
     O                       FRCD(2)             92
     O          E    22 99   HEAD           1
     O                                           65 'TO'
     O          E    22 99   HEAD           1
     O                       TOCD(2)             92
     O          E    23      HEAD           2
     O                                           74 'PREMIUM ACCT REP'
     O          E    23      HEAD           1
     O                       FRCD(3)             92
     O          E    23 99   HEAD           1
     O                                           65 'TO'
     O          E    23 99   HEAD           1
     O                       TOCD(3)             92
     O          E    24      HEAD           2
     O                                           72 'AUDIT REVIEWER'
     O          E    24      HEAD           1
     O                       FRCD(4)             92
     O          E    24 99   HEAD           1
     O                                           65 'TO'
     O          E    24 99   HEAD           1
     O                       TOCD(4)             92
     O          E    25      HEAD           2
     O                                           71 'AUDIT MANAGER'
     O          E    25      HEAD           1
     O                       FRCD(5)             92
     O          E    25 99   HEAD           1
     O                                           65 'TO'
     O          E    25 99   HEAD           1
     O                       TOCD(5)             92
     O          E    26      HEAD           2
     O                                           74 'LOSS CONTROL REP'
     O          E    26      HEAD           1
     O                       FRCD(6)             92
     O          E    26 99   HEAD           1
     O                                           65 'TO'
     O          E    26 99   HEAD           1
     O                       TOCD(6)             92
     *** beg add *** 11/22/13 *****************************************
     O          E    27      HEAD           2
     O                                           74 'CLAIM CONTACT'
     O          E    27      HEAD           1
     O                       FRCD(7)             92
     O          E    27 99   HEAD           1
     O                                           65 'TO'
     O          E    27 99   HEAD           1
     O                       TOCD(7)             92
     *** end add *** 11/22/13 *****************************************
     **
     O*
     O          E            HEAD        1  2
     O                                            8 'GROUP:'
     O                       XXFND         3     12
     O                       XXFNAM              73
     O          E            HEAD           2
     O                                           09 'EMPLR'
     O                                           16 'DIV'
     O                                           25 'NAME'
     O          E            DETAIL         1
     O                       EMEMP#        3     09
     O                       EMDIV         3     16
     O                       EMNAME              61
     O**
     O**** ERROR MESSAGE, IF NO RECORDS WERE CHANGED    *************
     O          E            ERRMSG         1
     O                                           19 '*** NO RECORDS MEET'
     O                                           33 'THE SELECTION'
     O                                           46 'CRITERIA ***'
     OPEM01     E            REM01
** CTG
WPTRMLC
