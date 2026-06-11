     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE W7384R5 - WORK FILE FOR A/R DETAIL REPORT.
     H****************************************************************
     H*   PROGRAM DESCRIBTION:
     H*       THIS PROGRAM READS THE A/R FILE AND DETERMINES WHICH
     H*       TRANSACTION DATE TO BE USED BASED ON THE TYPE OF ACCOUNT.
     H*       ONLY TRANSACTION CODES THAT HAVE BEEN REQUESTED FROM THE
     H*       PROMPT SCREEN WILL BE PROCESSED.
     H*       THIS PROGRAM WRITES TO TWO (2) WORK FILES, WXEAP WHICH
     H*       IS USED BY W7383R5 TO CALCULATE PREVIOUS AND CURRENT
     H*       BALANCES BASED ON SELECTED CODES.  THE SECOND FILE
     H*       WXDTP WRITES ONE RECORD FOR EACH EMPLOYER/DIVISION
     H*       AND IS USED TO SORT THE DATA BASED ON PROMPT SCREEN.
     H*
     H*  CALLED FROM: W7381C5
     H*
     H*****************************************************************
     H*    REVISIONS:                                                 *
     H*                                                               *
     H* 01/10/18 C:33  Support 63341                                  *
     H*                Clear agt# before WA002R call                  *
     H*                                                               *
     H*  07/19/99  C:33   PROJECT: WC899                              *
     H*                   WRITE TO FILE BASED ON PLAN TYPE. A=ALL     *
     H*                   POLICIES, E=EXCLUDE ACCT CURRENT POLICIES'  *
     H*                   C=ACCT CURRENT POLICIES ONLY.               *
     H*                                                               *
     H*  03/22/99  C:87   PROJECT: SUPP WA025 17229                   *
     H*                   Remove changes from 03/01/99 because prev   *
     H*                   balance was not being calculated. Seq #16744*
     H*                   is now resolved in W7383R5.                 *
     H*                                                               *
     H*  03/01/99  C:87   PROJECT: SUPP WA065 #16744                  *
     H*                   Only write records to workfile if EAXXDT is *
     H*                   within dates entered on prompt.             *
     H*                                                               *
     H*    3/25/91  DVM    PROJECT CODE - MDABV ADDED CODE TO ONLY    *
     H*                    PROCESS CODES SELECTED FROM PROMPT SCREEN. *
     H*                    IF NO CODES SELECTED ON PROMPT SCREEN THEN *
     H*                    ALL CODES WILL BE PROCESSED.               *
     H*                    COMBINED TWO WORK FILE BUILD PROGRAMS      *
     H*                    INTO ONE.                                  *
     H*                                                               *
     H*    4/02/92  LPK    PROJECT: WC128                             *
     H*                    ALWAYS USE TRANSACTION DATE WHEN READING   *
     H*                    P, I, AND Q TYPES.                         *
     H*                                                               *
     H*    5/04/93  C04    PROJECT: WC233                             *
     H*                    IF S/R ACCT, USE PODT IF > TRDT, THIS WILL *
     H*                    ACCT FOR:                                  *
     H*                      1. EXP CONST BILLS FROM A ROLLOVER RUN   *
     H*                         TRDT MAY BE 12/1/XX, POSTED 1/1XX     *
     H*                      2. AN ESTIMATING ACCT CHANGING TO S/R    *
     H*                         AN RE-RUNNING A PRIOR MONTHS A/R RPT  *
     H*                                                               *
     H*    8/28/95  C:28   PROJECT: WC448                             *
     H*                    DON'T REQUIRE WDELP RECORD TO PRINT WMEAP  *
     H*                    RECORDS.                                   *
     H*                                                               *
     H*  10/10/95 C:11 PROJECT WC453                                  *
     H*                GET THE LAST ACTIVE AGENT AS OF THE FYR, NOT   *
     H*                AS OF THE RUN DATE                             *
     H*                                                               *
     H*  04/23/97 C:62 PROJECT WC684                                  *
     H*                SELECT R + N TYPE TRANSACTIONS BASED ON THE    *
     H*                TRANSACTION DATE (EATRDT).                     *
     H*                                                               *
     H*  NOTE: PUT REVISION COMMENTS AT TOP                           *
     H*****************************************************************
     FWMEAL8    IF   E           K DISK
     FWDELL1    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWXEAL     O    E             DISK
     F                                     RENAME(PEA01:PEA02)
     F*
     F* Work File in SORT Sequence
     FWXDTL     O    E             DISK

     D*
     D* Define arrays for A/R Code selection criteria
     D COD             S              2    DIM(30) ASCEND                         A/R code
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C*****************************************************************
     C*     LIKE DEFINES
     C*****************************************************************
     C*
     C     *LIKE         DEFINE    EACO#         XXCO#
     C     *LIKE         DEFINE    EAFND         XXFND
     C     *LIKE         DEFINE    EAFYR         XXFYR
     C     *LIKE         DEFINE    DTAGT#        XXAGT#
     C     *LIKE         DEFINE    EAEMP#        XXEMP#
     C     *LIKE         DEFINE    EADIV         XXDIV
     C     *LIKE         DEFINE    EAXXDT        XXFRDT
     C     *LIKE         DEFINE    EAXXDT        XXTODT
     C     *LIKE         DEFINE    EAEMP#        SVEMP#
     C     *LIKE         DEFINE    EADIV         SVDIV
     C     *LIKE         DEFINE    BSSEFL        XXSEFL
     C*
     C*  KEY LIST FOR THE EMPLOYER ACCOUNTS  RECEIVABLE FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     EAKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C* Key List for the Employer Master File
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
     C*
     C* Key List for the Agent History Master File
     C*
     C     AHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  KEY LISY FOR THE BILLING SCHEDULE CODE FILE
     C*
     C     BSKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQAGT             5
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFDT             8
     C                   PARM                    QQTDT             8
     C                   PARM                    QQSORT            1
     C                   PARM                    QQCOD            60
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C                   PARM                    QQPLAN            1
     C***  END   ADD  ***  07/19/99  *********************************
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
     C* INITIALIZE
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQAGT         XXAGT#
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFDT         XXFDT             8 0
     C                   MOVE      QQTDT         XXTDT             8 0
     C                   MOVEA     QQCOD         COD
     C                   MOVE      QQSORT        XXSORT            1
     C                   Z-ADD     XXFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C                   Z-ADD     XXTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     0             SVEMP#
     C                   Z-ADD     0             SVDIV
     C*
     C* CHECK IF EMPLOYER IS ENTERD
     C     XXEMP#        COMP      0                                      20
     C*
     C   20EAKEY         CHAIN     PEA01                              60
     C  N20EAKEY1        CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     EAEMP#        XXEMP#
     C                   Z-ADD     EADIV         XXDIV
     C                   MOVE      *BLANK        XXSEFL
     C*
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C                   CALL      'WA002R'
     C                   PARM      XXCO#         @@CO#             3 0
     C                   PARM      XXFND         @@FND             3 0
     C                   PARM      XXEMP#        @@EMP#            9 0
     C                   PARM      XXDIV         @@DIV             5 0
     C                   PARM      XXFYR         @@FYR             3 0
     C***   Begin Chg  ***  01/10/18  *************************************
     C***********        PARM                    @@AGT#            5 0
     C***********        PARM                    @@ANAM           60
     C                   PARM      *zeros        @@AGT#            5 0
     C                   PARM      *blanks       @@ANAM           60
     C***   End   Chg  ***  01/10/18  *************************************
     C                   PARM                    @@PROD            5 0
     C                   PARM                    @@PNAM           60
     C                   PARM                    @@AREA            3 0
     C                   PARM                    @@PHON            7 0
     C                   PARM      '*'           @@PLAN            1
     C                   PARM      XXTDT         @@DATE            8 0
     C***  END   ADD  ***  07/19/99  *********************************
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANK        XXKEY            10
     C                   MOVEL     ELBSCD        XXKEY
     C     BSKEY1        CHAIN     PBS01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      BSSEFL        XXSEFL
     C                   ELSE
     C                   MOVE      'E'           XXSEFL
     C                   END
     C                   ELSE
     C                   MOVE      'E'           XXSEFL
     C                   END
     C*
     C     XXSEFL        IFEQ      'S'
     C*
     C*   USE TRANSACTION DATE FOR SELF REPORTING ACCOUNTS, EXCEPT
     C*   IF RECORD IS A DEPOSIT (H) BECAUSE THEY MAY PUT THE DEPOSIT
     C*   ON A PAYMENT PLAN.   ALSO, USE PODT IF RECORD IS AN
     C*   ADJUSTMENT (A) BECAUSE THIS IS USED TO PUT AN AUDIT ON A
     C*   PAYMENT PLAN.  - ALL OTHER ADJUSTMENT WILL SHOW CORRECTLY
     C*   BECAUSE TRDT ALWAYS EQUALS PODT.
     C*
     C*** BEGIN DEL *** 04/23/97 **************************************
     C********** EAARTY    IFEQ 'H'
     C********** EAARTY    OREQ 'A'
     C*** END   DEL *** 04/23/97 **************************************
     C*** BEGIN ADD *** 04/23/97 **************************************
     C     EAARTY        IFEQ      'A'
     C*** END   ADD *** 04/23/97 **************************************
     C                   Z-ADD     EAPODT        EAXXDT
     C                   ELSE
     C     EAPODT        IFGT      EATRDT
     C     EAARTY        ANDNE     'P'
     C     EAARTY        ANDNE     'I'
     C     EAARTY        ANDNE     'Q'
     C*** BEGIN ADD *** 04/23/97 **************************************
     C     EAARTY        ANDNE     'R'
     C     EAARTY        ANDNE     'N'
     C*** END   ADD *** 04/23/97 **************************************
     C                   Z-ADD     EAPODT        EAXXDT
     C                   ELSE
     C                   Z-ADD     EATRDT        EAXXDT
     C                   END                                                    ADD C04 5/4/93
     C                   END
     C*
     C                   ELSE
     C*
     C*   USE POSTING DATE FOR ESTIMATING ACCOUNTS, EXCEPT WHEN THE
     C*   TRANSACTION DATE IS GREATER THAN POSTING DATE.  THIS WILL
     C*   OCCUR WHEN A SELF REPORTING ACCT CHANGES TO AN ESTIMATING
     C*   ACCT & HAD A BILL POSTED FOR THE PRIOR MONTH AS A S/R ACCT.
     C*   THIS CAN ALSO OCCUR IF THE KEY A LATE PAYMENT WITH PODT
     C*   FOR A PRIOR DATE.
     C*
     C     EATRDT        IFGT      EAPODT
     C     EAARTY        OREQ      'P'
     C     EAARTY        OREQ      'I'
     C     EAARTY        OREQ      'Q'
     C*** BEGIN ADD *** 04/23/97 **************************************
     C     EAARTY        OREQ      'R'
     C     EAARTY        OREQ      'N'
     C*** END   ADD *** 04/23/97 **************************************
     C                   Z-ADD     EATRDT        EAXXDT
     C                   ELSE
     C                   Z-ADD     EAPODT        EAXXDT
     C                   END
     C*
     C                   END
     C*
     C*   ONLY WRITE A RECORD IF EAXXDT IS WITH IN THE SELECTED
     C*   DATE RANGE. IF THE A/R CODE IS LEFT BLANK ON THE PROMPT
     C*   SCREEN THEN THE DATE RANGE WILL BE THE ONLY CRITERIA FOR
     C*   WRITING A RECORD ELSE THE A/R CODE MUST BE EQUAL TO A
     C*   SELECTED CODE.
     C*
     C***  BEGIN DEL  ***  03/22/99  **********************************
     C***  BEGIN ADD  ***  03/01/99  **********************************
     C********** EAXXDT    IFGE XXFRDT
     C********** EAXXDT    ANDLEXXTODT
     C***  END   ADD  ***  03/01/99  **********************************
     C***  END   DEL  ***  03/22/99  **********************************
     C*
     C*  LOOK UP EAARCD IN A/R CODE ARRAY TO DETERMIME IF CODE IS
     C*  IS TO BE WRITTEN.
     C*
     C     QQCOD         IFNE      *BLANKS
     C                   Z-ADD     1             Y                 2 0
     C     EAARCD        LOOKUP    COD(Y)                                 60
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C     QQCOD         OREQ      *BLANKS
     C*
     C***  BEGIN CHG  ***  07/19/99  *********************************
     C**********           WRITEPEA02
     C                   SELECT
     C     QQPLAN        WHENEQ    'A'
     C                   WRITE     PEA02
     C     QQPLAN        WHENEQ    'E'
     C     @@PLAN        ANDNE     'A'
     C                   WRITE     PEA02
     C     QQPLAN        WHENEQ    'C'
     C     @@PLAN        ANDEQ     'A'
     C                   WRITE     PEA02
     C                   ENDSL
     C***  END   CHG  ***  07/19/99  *********************************
     C*
     C* CHECK TO SEE IF EMPLOYER CHANGES, IF IT DOES THEN WRITE
     C*  INFORMATION TO WXDTP WORK FILE
     C*
     C     EAEMP#        IFNE      SVEMP#
     C     EADIV         ORNE      SVDIV
     C                   EXSR      SRWRIT
     C                   Z-ADD     EAEMP#        SVEMP#
     C                   Z-ADD     EADIV         SVDIV
     C                   END
     C*
     C*
     C                   END                                                     A/R CODE
     C*
     C***  BEGIN DEL  ***  03/22/99  **********************************
     C***  BEGIN ADD  ***  03/01/99  **********************************
     C*******************  ENDIF                            EAXXDT > XXFRDT
     C***  END   ADD  ***  03/01/99  **********************************
     C***  END   DEL  ***  03/22/99  **********************************
     C*
     C*
     C   20EAKEY         READE     PEA01                                  60
     C  N20EAKEY1        READE     PEA01                                  60
     C                   END
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C*    SRWRIT - SUBROUNTINE TO WRITE UNIQUE EMPLOYER TO WXDTP
     C*****************************************************************
     C*
     C     SRWRIT        BEGSR
     C*
     C* Move A/R Information to Work File Fields
     C*
     C                   MOVE      EACO#         DTCO#
     C                   MOVE      EAFND         DTFND
     C                   MOVE      EAEMP#        DTEMP#
     C                   MOVE      EADIV         DTDIV
     C                   MOVE      *BLANK        DTSTAT
     C                   MOVE      Q1USER        DTUSER
     C                   MOVE      Q1PGM         DTPGM
     C                   MOVE      DSTIME        DTTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DTDT
     C*
     C* Get the Most Current Active Agent
     C*
     C     AHKEY         SETGT     PAH01
     C                   READP     PAH01                                  60
     C     *IN60         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  60
     C                   END
     C     *IN60         IFEQ      '0'
     C     AHCO#         ANDEQ     XXCO#
     C     AHFND         ANDEQ     XXFND
     C     AHEMP#        ANDEQ     XXEMP#
     C     AHDIV         ANDEQ     XXDIV
     C     AHFYR         ANDEQ     XXFYR
     C*
     C                   Z-ADD     AHPROD        DTPROD
     C                   Z-ADD     AHAGT#        DTAGT#
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             DTPROD
     C                   Z-ADD     0             DTAGT#
     C*
     C                   END
     C*
     C* Determine if Agent, Employer # or Employer Name is to be moved
     C* into DTSORT field for correct sorting for Report.
     C*
     C     XXSORT        IFEQ      'A'
     C                   MOVEL     DTAGT#        DTSORT
     C                   ELSE
     C     XXSORT        IFEQ      'E'
     C                   MOVEL     DTEMP#        EMPDIV           14            COMBINE EMP#
     C                   MOVE      DTDIV         EMPDIV                         AND DIVISION
     C                   MOVEL     EMPDIV        DTSORT
     C                   ELSE
     C     XXSORT        IFEQ      'N'
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        DTSORT
     C                   ELSE
     C                   MOVE      *BLANKS       DTSORT
     C                   END                                                    PEM01 CHAIN
     C                   END                                                    XXSORT = "N"
     C                   END                                                    XXSORT = "E"
     C                   END                                                    XXSORT = "A"
     C*
     C* If Getting only ONE Agent Check against the Passed Parm Agt#
     C* Write the Record
     C*
     C     XXAGT#        IFNE      0
     C     AHAGT#        ANDEQ     XXAGT#
     C                   WRITE     PDT01
     C                   END
     C*
     C* Write the Record if getting all Agents
     C*
     C     XXAGT#        IFEQ      0
     C                   WRITE     PDT01
     C                   END
     C*
     C*
     C* Clear the Fields in the Work File
     C*
     C                   Z-ADD     0             DTCO#
     C                   Z-ADD     0             DTFND
     C                   Z-ADD     0             DTEMP#
     C                   Z-ADD     0             DTDIV
     C                   Z-ADD     0             DTAGT#
     C                   Z-ADD     0             DTPROD
     C                   MOVE      *BLANKS       DTSORT
     C                   MOVE      *BLANKS       DTUSER
     C                   MOVE      *BLANKS       DTPGM
     C                   Z-ADD     0             DTTIME
     C                   Z-ADD     0             DTDT
     C*
     C                   ENDSR
     C*
