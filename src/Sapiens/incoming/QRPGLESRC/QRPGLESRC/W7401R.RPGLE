     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7401R - BUILD WORK FILE FOR AGENT LATE NOTICE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/10/18 C:33  Support 63341                                  *
     H*                Clear agt# before WA002R call                  *
     H*                                                               *
     H* 04/20/05 C:09    PROJECT: WC899                               *
     H*                  Do not process employers on Account Current  *
     H*                                                               *
     H* 02/19/04 C:136   PROJECT: WC1033                              *
     H*                  Add code for new A/R type of 'T' (True Void) *
     H*                                                               *
     H* 07/20/00  C:35   PROJECT: SUPPORT WA107 #23541                *
     H*                  PAYMENTS SHOULD BE DISSOLVED INTO OLDEST DUE *
     H*                  AMOUNTS REGARDLESS OF PAYMENT POSTING DATE.  *
     H*                                                               *
     H* 12/16/99  C:87   PROJECT: SUPP Y2K LOG#59                     *
     H*                  B, A, and F a/r transactions were not being  *
     H*                  included in calculations for audited accts.  *
     H*                  Loaded XXDAT1 to rectify.                    *
     H*                                                               *
     H* 08/04/99  C:118  SUPPORT: WA091/18874                         *
     H*                  If policy is audited, only look at  trans-   *
     H*                  actions thru run date.                       *
     H*                                                               *
     H* 04/09/99  C:35   PROJECT: SUPPORT WA101 #17510                *
     H*                  DATE COMPARISON WAS NOT CORRECT FOR 7-DIGIT  *
     H*                  DIGIT DATE FIELD.                            *
     H*                                                               *
     H*  07/08/98 C:87 PROJECT: SUPPORT #12564                        *
     H*                IF THERE IS A CANCEL DATE WITH THE POLICY DATES*
     H*                THEN CALCULATE UP TO BUT NOT INCLUDING THE     *
     H*                CANCEL DATE.                                   *
     H*                                                               *
     H****************************************************************
     F*
     F* Added code to get emplyer detail record.  Check ELFAF flag;
     F* if = A include all A/R up to date 99/99/99.  If employer is
     F* not auditted then check emplyer's effective and cancel dates.
     F* If employer has been cancelled include A/R up to the cancel
     F* date; else include A/R up to system date.          LJQ 8-23-88
     F*
     FWXLNL     O    E             DISK
     FWMEML     IF   E           K DISK
     FWMEAL8    IF   E           K DISK
     FWMAHL     IF   E           K DISK
     F*
     F* Employer Detail File                               LJQ 8-23-88
     FWDELL1    IF   E           K DISK

     D*
     D* Effective and Cancel Date Arrays
     D COD             S              2    DIM(30) ASCEND                       A/R CODES
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
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    EAPODT        XXDAT
     C     *LIKE         DEFINE    EAPODT        XXDAT1
     C     *LIKE         DEFINE    EAPODT        XX30DT
     C     *LIKE         DEFINE    EAPODT        XX60DT
     C     *LIKE         DEFINE    EAPODT        XX90DT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQDATE            8            JJJ 9-2-88
     C                   PARM                    QQCODE           60            |
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
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQDATE        XXRNDT            8 0          JJJ 9-2-88
     C                   MOVEA     QQCODE        COD                            |
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE            LJQ 8-23-88
     C*                                                          "
     C     ELKEY         KLIST                                                        "
     C                   KFLD                    XXCO#                                "
     C                   KFLD                    XXFND                                "
     C                   KFLD                    XXFYR                                "
     C                   KFLD                    EMEMP#                               "
     C                   KFLD                    EMDIV                                "
     C*
     C*   KEY LIST FOR THE EMPLOYER ACCOUNTS RECEIVABLE FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C*   KEY LIST FOR THE AGENT HISTORY MASTER FILE
     C*
     C     AHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     XXRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT
     C*                                                    |
     C*   SORT THE CODE ARRAY                              |
     C*                                                    |
     C                   SORTA     COD                                          |
     C*
     C*   START READING THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     0             XXCURR            9 2          JJJ 9-2-88
     C                   Z-ADD     0             XX30DY            9 2          |
     C                   Z-ADD     0             XX60DY            9 2          |
     C                   Z-ADD     0             XX90DY            9 2          |
     C                   Z-ADD     0             XXAMTD            9 2
     C*
     C***** BEGIN CHG **** 08/04/99 ***********************************
     C****GET EMP. DETAIL RECORD. IF ACCOUNT IS AUDITED, INCLUDE ALL
     C****PAYMENTS EVER MADE FOR THAT FYR.                 LJQ 8-23-88
     C*                                                          "
     C*  GET EMPLOYER DETAIL RECORD.  IF ACCOUNT IS AUDITED,
     C*  ONLY LOOK AT TRANSACTIONS THRU RUN DATE.
     C*
     C********** ELKEY     CHAINPEL01                60          "
     C********** *IN60     IFEQ '0'                              "
     C********** ELFAF     ANDEQ'A'                              "
     C**********           Z-ADD9999999   XXDAT                  "
     C**********           ELSE                                  "
     C**********           Z-ADDXXRNDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXDAT
     C**********           END                                   "
     C*
     C     ELKEY         CHAIN     PEL01                              60              "
     C***** END   CHG **** 08/04/99 ***********************************
     C*
     C*  USE CANCEL DATE IF IT DOESN'T = 0 AND IT IS NOT AN AUDITTED
     C*  EMPLOYER                                          LJQ 8-23-88
     C*                                                          "
     C                   Z-ADD     0             XXDAT1                         LJQ 9-14-88
     C***** BEGIN CHG **** 08/04/99 ***********************************
     C********** XXDAT     IFNE 9999999                    LJQ 8-23-88
     C     ELFAF         IFNE      'A'
     C***** END   CHG **** 08/04/99 ***********************************
     C                   Z-ADD     1             X                 3 0                "
     C     *ZERO         LOOKUP    ED(X)                                  60          "
     C     *IN60         IFEQ      '0'                                                "
     C                   Z-ADD     5             X                                    "
     C                   ELSE                                                         "
     C                   SUB       1             X                                    "
     C                   END                                                          "
     C*                                                          "
     C                   Z-ADD     1             Y                 3 0                "
     C     *ZERO         LOOKUP    CD(Y)                                  60          "
     C     *IN60         IFEQ      '0'                                                "
     C                   Z-ADD     5             Y                                    "
     C                   ELSE                                                         "
     C                   SUB       1             Y                                    "
     C                   END                                                          "
     C*                                                          "
     C     Y             IFEQ      0                                                  "
     C     CD(Y)         ORLT      ED(X)                                              "
     C                   Z-ADD     XXRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1
     C*** BEGIN ADD *** 07/08/98 *************************************
     C                   MOVE      *OFF          *IN95                          NOT CANX
     C*** END   ADD *** 07/08/98 *************************************
     C                   ELSE                                                   LJQ 8-23-88
     C                   Z-ADD     CD(Y)         XXDAT1                         LJQ 9-14-88
     C*** BEGIN ADD *** 07/08/98 *************************************
     C                   MOVE      *ON           *IN95                          CANCELLED
     C*** END   ADD *** 07/08/98 *************************************
     C                   END                                                    LJQ 8-23-88
     C*                                                          "
     C***** BEGIN CHG **** 08/04/99 **********************************
     C**********           END
     C*
     C***  BEGIN ADD  ***  12/16/99  **********************************
     C                   ELSE                                                   ELFAF<>A
     C*
     C                   Z-ADD     XXRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1
     C***  END   ADD  ***  12/16/99  **********************************
     C                   ENDIF                                                  ELFAF<>A
     C***** END   CHG **** 08/04/99 **********************************
     C*** BEG ADD ******** 04/20/05  **********************************
     C                   Z-ADD     ELCO#         @@CO#
     C                   Z-ADD     ELFND         @@FND
     C                   Z-ADD     ELEMP#        @@EMP#
     C                   Z-ADD     ELDIV         @@DIV
     C                   Z-ADD     ELFYR         @@FYR
     C                   Z-ADD     XXRNDT        @@DATE
     C                   MOVEL     '*'           @@PLAN
      *
     C                   CALL      'WA002R'
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@DIV             5 0
     C                   PARM                    @@FYR             3 0
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
     C                   PARM                    @@PLAN            1
     C                   PARM                    @@DATE            8 0
      *
     C     @@PLAN        IFNE      'A'
     C*** END ADD ******** 04/20/05  **********************************
     C*
     C*   CALCULATE 30, 60, & 90 DAYS IN THE PAST ACCORDING TO DATE
     C*   ENTERED                                          JJJ 9-2-88
     C*                                                    |
     C                   Z-ADD     30            XXDAYS            5 0          |
     C                   EXSR      SR2ND                                        |
     C                   MOVE      SRDATE        X130DT            8 0          |
     C                   Z-ADD     X130DT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XX30DT
     C*                                                    |
     C                   Z-ADD     60            XXDAYS            5 0          |
     C                   EXSR      SR2ND                                        |
     C                   MOVE      SRDATE        X160DT            8 0          |
     C                   Z-ADD     X160DT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XX60DT
     C*                                                    |
     C                   Z-ADD     90            XXDAYS            5 0          |
     C                   EXSR      SR2ND                                        |
     C                   MOVE      SRDATE        X190DT            8 0          |
     C                   Z-ADD     X190DT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XX90DT
     C*
     C*
     C*   START READING THE EMPLOYER ACCOUNTS RECEIVABLE FILE
     C*
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*   A/R CODE MUST BE ONE OF THE CODES ENTERED        JJJ 9-2-88
     C*    UNLESS CODES WERE LEFT BLANK THEN ALL CODES WILL|
     C*    WILL BE USED.
     C*                                                    |
     C     QQCODE        IFNE      *BLANKS
     C                   Z-ADD     1             Y                              |
     C     EAARCD        LOOKUP    COD(Y)                                 60    |
     C*
     C                   END
     C     *IN60         IFEQ      '1'                                          |
     C     EAARCD        ANDNE     *BLANK                                       |
     C     QQCODE        OREQ      *BLANKS
     C*
     C*   ADD TO OR SUBTRACT FROM AMOUNT DUE IF POST DATE < OR = TODAY
     C*   CALCULATE AGING ON PAST DUE AMOUNTS ACCORDING TO THE ENTERED
     C*   RUN DATE.
     C*
     C     XXDAT         IFEQ      XXDAT1                                       LJQ 9-14-88
      **** BEGIN CHG **** 04/09/99 *********************************************
     C********** XXDAT     OREQ 999999                           "
     C     XXDAT         OREQ      9999999                                            "
      **** END   CHG **** 04/09/99 *********************************************
     C     EAPODT        IFLE      XXDAT
     C*
     C     EAARTY        IFEQ      'B'
     C     EAARTY        OREQ      'A'
     C     EAARTY        OREQ      'D'
     C     EAARTY        OREQ      'V'
     C     EAARTY        OREQ      'N'
     C     EAARTY        OREQ      'R'
     C     EAARTY        OREQ      'F'                                          LJQ 9-14-88
     C*
     C                   ADD       EAAMT         XXAMTD
     C     EAPODT        IFLE      XX90DT                                       JJJ 9-2-88
     C                   ADD       EAAMT         XX90DY                         |
     C                   ELSE                                                   |
     C     EAPODT        IFLE      XX60DT                                       |
     C                   ADD       EAAMT         XX60DY                         |
     C                   ELSE                                                   |
     C     EAPODT        IFLE      XX30DT                                       |
     C                   ADD       EAAMT         XX30DY                         |
     C                   ELSE                                                   |
     C                   ADD       EAAMT         XXCURR                         |
     C                   END                                                    |
     C                   END                                                    |
     C                   END                                                    |
     C*
     C                   END
     C*
      **** BEGIN DEL **** 07/20/00 *********************************************
     C********** EAARTY    IFEQ 'P'
     C********** EAARTY    OREQ 'Q'                        LJQ 9-14-88
     C**********
     C**********           SUB  EAAMT     XXAMTD
     C********** EAPODT    IFLE XX90DT                     JJJ 9-2-88
     C**********           SUB  EAAMT     XX90DY           |
     C**********           ELSE                            |
     C********** EAPODT    IFLE XX60DT                     |
     C**********           SUB  EAAMT     XX60DY           |
     C**********           ELSE                            |
     C********** EAPODT    IFLE XX30DT                     |
     C**********           SUB  EAAMT     XX30DY           |
     C**********           ELSE                            |
     C**********           SUB  EAAMT     XXCURR           |
     C**********           END                             |
     C**********           END                             |
     C**********           END                             |
     C**********
     C**********           END
      **** END   DEL **** 07/20/00 *********************************************
     C*
     C                   END
     C*
     C                   ELSE                                                   LJQ 9-14-88
     C*                                                          "
     C     EAPODT        IFLE      XXDAT                                              "
     C     EAPODT        IFLE      XXDAT1                                             "
     C*** BEGIN ADD *** 07/08/98 *************************************
     C     *IN95         ANDEQ     *OFF                                         NOT CANX
     C     EAPODT        ORLT      XXDAT1
     C     *IN95         ANDEQ     *ON                                          CANCELLED
     C*** END   ADD *** 07/08/98 *************************************
     C     EAARTY        IFEQ      'B'                                                "
     C     EAARTY        OREQ      'A'                                                "
     C     EAARTY        OREQ      'F'                                                "
     C                   ADD       EAAMT         XXAMTD                               "
     C     EAPODT        IFLE      XX90DT                                             "
     C                   ADD       EAAMT         XX90DY                               "
     C                   ELSE                                                         "
     C     EAPODT        IFLE      XX60DT                                             "
     C                   ADD       EAAMT         XX60DY                               "
     C                   ELSE                                                         "
     C     EAPODT        IFLE      XX30DT                                             "
     C                   ADD       EAAMT         XX30DY                               "
     C                   ELSE                                                         "
     C                   ADD       EAAMT         XXCURR                               "
     C                   END                                                          "
     C                   END                                                          "
     C                   END                                                          "
     C*                                                          "
     C                   END                                                          "
     C                   END                                                          "
     C*                                                          "
     C     EAARTY        IFEQ      'D'                                                "
     C     EAARTY        OREQ      'V'                                                "
     C     EAARTY        OREQ      'N'                                                "
     C     EAARTY        OREQ      'R'                                                "
     C                   ADD       EAAMT         XXAMTD                               "
     C     EAPODT        IFLE      XX90DT                                             "
     C                   ADD       EAAMT         XX90DY                               "
     C                   ELSE                                                         "
     C     EAPODT        IFLE      XX60DT                                             "
     C                   ADD       EAAMT         XX60DY                               "
     C                   ELSE                                                         "
     C     EAPODT        IFLE      XX30DT                                             "
     C                   ADD       EAAMT         XX30DY                               "
     C                   ELSE                                                         "
     C                   ADD       EAAMT         XXCURR                               "
     C                   END                                                          "
     C                   END                                                          "
     C                   END                                                          "
     C                   END                                                          "
     C*                                                          "
      **** BEGIN DEL **** 07/20/00 *********************************************
     C********** EAARTY    IFEQ 'P'                              "
     C********** EAARTY    OREQ 'Q'                              "
     C**********           SUB  EAAMT     XXAMTD                 "
     C********** EAPODT    IFLE XX90DT                           "
     C**********           SUB  EAAMT     XX90DY                 "
     C**********           ELSE                                  "
     C********** EAPODT    IFLE XX60DT                           "
     C**********           SUB  EAAMT     XX60DY                 "
     C**********           ELSE                                  "
     C********** EAPODT    IFLE XX30DT                           "
     C**********           SUB  EAAMT     XX30DY                 "
     C**********           ELSE                                  "
     C**********           SUB  EAAMT     XXCURR                 "
     C**********           END                                   "
     C**********           END                                   "
     C**********           END                                   "
     C**********           END                                   "
      **** END   DEL **** 07/20/00 *********************************************
     C*                                                          "
     C                   END                                                          "
     C*                                                          "
     C                   END                                                          "
     C*
     C                   END                                                    JJJ 9-2-88
     C*
     C     EAKEY         READE     PEA01                                  60
     C                   END
      **** BEGIN ADD **** 07/20/00 *********************************************
      *
      *   START READING THE EMPLOYER ACCOUNTS RECEIVABLE FILE
      *
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         DOWEQ     *OFF
      *
      *   A/R CODE MUST BE ONE OF THE CODES ENTERED
      *    UNLESS CODES WERE LEFT BLANK THEN ALL CODES WILL
      *    WILL BE USED.
      *
     C     QQCODE        IFNE      *BLANKS
     C                   Z-ADD     1             Y
     C     EAARCD        LOOKUP    COD(Y)                                 60
     C                   ENDIF                                                  QQCODE IFNE *BLANKS
      *
     C     *IN60         IFEQ      *ON
     C     QQCODE        ANDNE     *BLANKS
     C     QQCODE        OREQ      *BLANKS
      *
      *   SUBTRACT FROM AMOUNT DUE IF POST DATE < OR = TODAY
      *   DISSOLVE PAYMENTS STARTING WITH OLDEST DUE BUCKET
      *
     C     EAPODT        IFLE      XXDAT
      *
     C     EAARTY        IFEQ      'P'
     C     EAARTY        OREQ      'Q'
     C*** BEG ADD *******  02/19/04  **********************************
     C     EAARTY        OREQ      'T'
     C*** END ADD *******  02/19/04  **********************************
      *
     C                   SUB       EAAMT         XXAMTD
      *
     C                   EXSR      DISSLV
      *
     C                   ENDIF                                                  EAARTY IFEQ 'P'
      *
     C                   ENDIF                                                  EAPOST IFLE XXDAT
      *
     C                   ENDIF                                                  *IN60 IFEQ *ON
      *
     C     EAKEY         READE     PEA01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
      * DISSOLVE CREDIT BALANCES
      *
     C                   Z-ADD     0             EAAMT
      *
     C     XX90DY        IFLT      0
     C     XX90DY        MULT      -1            EAAMT
     C                   Z-ADD     0             XX90DY
     C                   ENDIF                                                  XX90DY OFLT 0
      *
     C     XX60DY        IFLT      0
     C                   MULT      -1            XX60DY
     C                   ADD       XX60DY        EAAMT
     C                   Z-ADD     0             XX60DY
     C                   ENDIF                                                  XX60DY IFLT 0
      *
     C     XX30DY        IFLT      0
     C                   MULT      -1            XX30DY
     C                   ADD       XX30DY        EAAMT
     C                   Z-ADD     0             XX30DY
     C                   ENDIF                                                  XX30DY IFLT 0
      *
     C     EAAMT         IFNE      0
     C                   EXSR      DISSLV
     C                   ENDIF                                                  EAAMT IFNE 0
      **** END   ADD **** 07/20/00 *********************************************
     C*
     C     XXAMTD        IFNE      0                                            JJJ 9-2-88
     C*
     C*   CLEAR THE FIELDS IN THE WORK FILE
     C*
     C                   MOVE      *ZEROS        LNCO#
     C                   MOVE      *ZEROS        LNFND
     C                   MOVE      *ZEROS        LNEMP#
     C                   MOVE      *ZEROS        LNDIV
     C                   MOVE      *ZEROS        LNAGT#
     C                   MOVE      *ZEROS        LNPROD
     C                   MOVE      *ZEROS        LNCURR                         JJJ 9-2-88
     C                   MOVE      *ZEROS        LN30DY                         |
     C                   MOVE      *ZEROS        LN60DY                         |
     C                   MOVE      *ZEROS        LN90DY                         |
     C                   MOVE      *ZEROS        LNAMT
     C*
     C*   MOVE THE INFORMATION TO CORRECT FIELDS
     C*
     C                   MOVE      EMCO#         LNCO#
     C                   MOVE      EMFND         LNFND
     C                   MOVE      EMEMP#        LNEMP#
     C                   MOVE      EMDIV         LNDIV
     C                   MOVE      *BLANK        LNSTAT
     C                   MOVE      Q1USER        LNUSER
     C                   MOVE      Q1PGM         LNPGM
     C                   MOVE      DSTIME        LNTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LNDT
     C                   Z-ADD     XXCURR        LNCURR                         JJJ 9-2-88
     C                   Z-ADD     XX30DY        LN30DY                         |
     C                   Z-ADD     XX60DY        LN60DY                         |
     C                   Z-ADD     XX90DY        LN90DY                         |
     C                   Z-ADD     XXAMTD        LNAMT
     C*                                                    JJJ 6-10-88
     C*  GET MOST CURRENT ACTIVE AGENT - IF NOT FOUND DONT WRITE RECORD
     C*
     C     AHKEY         SETGT     PAH01
     C                   READP     PAH01                                  60
     C     *IN60         DOWEQ     '0'                                          JJJ 6-10-88
     C     AHSTAT        ANDEQ     'I'                                          *
     C                   READP     PAH01                                  60    *
     C                   END                                                    *
     C     *IN60         IFEQ      '0'
     C     AHCO#         ANDEQ     XXCO#
     C     AHFND         ANDEQ     XXFND
     C     AHEMP#        ANDEQ     EMEMP#
     C     AHDIV         ANDEQ     EMDIV
     C     AHFYR         ANDEQ     XXFYR
     C*
     C                   Z-ADD     AHPROD        LNPROD            5 0
     C                   MOVE      AHAGT#        LNAGT#            5 0
     C*
     C                   WRITE     PLN01
     C                   END                                                    JJJ 6-10-88
     C                   END
     C*** BEG ADD ******** 04/20/05  **********************************
     C                   END
     C*** END ADD ******** 04/20/05  **********************************
     C*
     C     EMKEY         READE     PEM01                                  60
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
      **** BEGIN ADD **** 07/20/00 *********************************************
      *
      * SUBROUTINE TO DISSOLVE PAYMENTS
      *
     C     DISSLV        BEGSR
      *
     C     XX90DY        IFGE      EAAMT
     C                   SUB       EAAMT         XX90DY
     C                   Z-ADD     0             EAAMT
     C                   ELSE
     C     XX90DY        IFGT      0
     C                   SUB       XX90DY        EAAMT
     C                   Z-ADD     0             XX90DY
     C                   ENDIF                                                  XX90DY IFGT 0
     C                   ENDIF                                                  XX90DY IFGT EAAMT
      *
     C     XX60DY        IFGE      EAAMT
     C                   SUB       EAAMT         XX60DY
     C                   Z-ADD     0             EAAMT
     C                   ELSE
     C     XX60DY        IFGT      0
     C                   SUB       XX60DY        EAAMT
     C                   Z-ADD     0             XX60DY
     C                   ENDIF                                                  XX60DY IFGT 0
     C                   ENDIF                                                  XX60DY IFGT EAAMT
      *
     C     XX30DY        IFGE      EAAMT
     C                   SUB       EAAMT         XX30DY
     C                   Z-ADD     0             EAAMT
     C                   ELSE
     C     XX30DY        IFGT      0
     C                   SUB       XX30DY        EAAMT
     C                   Z-ADD     0             XX30DY
     C                   ENDIF                                                  XX30DY IFGT 0
     C                   ENDIF                                                  XX30DY IFGT EAAMT
      *
     C                   SUB       EAAMT         XXCURR
      *
     C                   ENDSR                                                  DISSLV
      **** END   ADD **** 07/20/00 *********************************************
     C******************************************************
     C*    -SUBROUTINE TO CALCULATE A DATE IN THE PAST     *
     C******************************************************
     C     SR2ND         BEGSR
     C*
     C                   Z-ADD     XXDAYS        SRPST             5 0
     C                   MOVE      XXRNDT        SRDATE            8 0
     C                   MOVE      *BLANK        SRERR             1
     C                   MOVE      *BLANK        SRTERM            1
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDATE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C*
     C                   ENDSR
     C*
