     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H BNDDIR('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7778R - WORKERS COMP RESERVE HISTORY REPORT
     H*****************************************************************
     H* THIS PROGRAM PRODUCES A REPORT FOR SELECTED FYRS, EMPS, CLMS  *
     H* ETC., SHOWING RESERVE HISTORY DETAIL WITHIN A GIVEN DATE RANGE*
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   MENU                                                        *
     H* CALLS TO: NONE                                                *
     H*                                                               *
     H* PROGAMMER: C:57                                               *
     H* CREATED:   03/19/97             PROJECT: 68AYS                *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H*  50  MAIN LOOP PROCESSING                                     *
     H*  60  GENERAL USAGE                                            *
     H*  71  READ DATA BY CO/FND                                      *
     H*  72  READ DATA BY CO/FND/FYR                                  *
     H*  73  READ DATA BY CO/FND/EMP/DIV                              *
     H*  74  READ DATA BY CO/FND/EMP/DIV/FYR                          *
     H*  75  READ DATA BY CO/FND/FYR/CASE                             *
     H*  76  READ DATA BY CO/FND/EMP/DIV/FYR/CASE                     *
     H*  81  ALL FUND YEARS SELECTED                                  *
     H*  82  ALL EMPLOYERS SELECTED                                   *
     H*  83  ALL CASES SELECTED                                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 03/11/14 C:282 PROJECT 129WSC Priss 28                        *
     H*                Aligning the 'Heading for Legal Reserve'       *
     H*                with the other reserves.                       *
     H*                                                               *
     H* 11/25/13 C:282 PROJECT 129WSC                                 *
     H*                Replace legal literal with control field       *
     H*                for cltid 129 only.                            *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H*                 for no-auth, do not include claimant info     *
     H*                                                               *
     H* 01/31/05 C:15  Support: WC129/44959                           *
     H*                Added code to only list claims that have rsv   *
     H*                activity within the selected date range.       *
     H*                                                               *
     H* 08/09/00 C:61  Project: WC577                                 *
     H*                Recompiled to write signed instead of packed   *
     H*                fields in WFRHP.                               *
     H*                                                               *
     H* 06/28/00 C:15  Project: WC577                                 *
     H*                Chgd QQDIV from 3A to 5A.                      *
     H*                                                               *
     H* 04/10/00 C:118 PROJECT: WC987                                 *
     H*                LOAD ALTERNATE COMPANY NAME FROM FUND MASTER   *
     H*                                                               *
     H* 12/14/99 C:90  PROJECT: WC577                                 *
     H*                Y2K FIX - CENTERED REPORT HEADING, LOAD        *
     H*                CLAIMANT NAME.                                 *
     H*                                                               *
     H* 10/29/99 C:15  Project: WC577                                 *
     H*                Fixed the usage of CMLTFL and fixed the date   *
     H*                range comparison when determining if the       *
     H*                reserve history record should be printed.      *
     H*                                                               *
     H* 02/26/99  C:35   PROJECT: WC840                               *
     H*                  RESERVE DETAIL WAS NOT PRINTING.             *
     H*                                                               *
     H* 04/30/97 C:62 PROJECT: 48WHP                                  *
     H*               PRINT INDIVIDUAL RHEXPL AND RHUSER NOT JUST ONE.*
     H*                                                               *
     H*****************************************************************
     F*
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMCML     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FWMRHL     IF   E           K DISK
     FWMRHL5    IF   E           K DISK
     F                                     RENAME(PRH01:PRH02)
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDPAL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)
     FWFRHP     UF A E             DISK
     F                                     RENAME(PRH01:PRHOUT)
      *** Beg Add ***  11/25/13  ******************************************************
     FWMCXL     IF   E           K DISK
      *** End Add ***  11/25/13  ***********************************************HEADING
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXSS#                  1      9
     D  XXSS#1                 1      3
     D  XXSS#2                 4      5
     D  XXSS#3                 6      9
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS
     D  SU                     1     90  2
     D                                     DIM(10)
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** BEG ADD ****** 05/25/07 **********************************
     D XXATTX          C                   CONST('*** NO AUTHORITY ***')
      *** Beg Add ***  11/25/13  ******************************************************
     D charLGRH        s             10                                         LEGAL RESERVE HEAD
     D CltId           s              3
      *
     d RtvCltID        pr                  ExtPgm('RTVCLTID')
     d cltID                          3    const
      *** End Add ***  11/25/13  ***********************************************HEADING
      *** Beg Add ***  03/11/14  ******************************************************
     D/copy sprString
      *** End Add ***  03/11/14  ******************************************************

     I*
     IPRHOUT
     I              RHCO#                       ROCO#
     I              RHFND                       ROFND
     I              RHFYR                       ROFYR
     I              RHCASE                      ROCASE
     I              RHEMP#                      ROEMP#
     I              RHDIV                       RODIV
     I              RHENAM                      ROENAM
     I              RHSTAT                      ROSTAT
     I              RHLTF                       ROLTF
     I              RHSS#                       ROSS#
     I              RHCLNM                      ROCLNM
     I              RHTITD                      ROTITD
     I              RHEFDT                      ROEFDT
     I              RHCCHG                      ROCCHG
     I              RHMCHG                      ROMCHG
     I              RHLCHG                      ROLCHG
     I              RHECHG                      ROECHG
     I              RHCRSV                      ROCRSV
     I              RHMRSV                      ROMRSV
     I              RHLRSV                      ROLRSV
     I              RHERSV                      ROERSV
     I              RHCINC                      ROCINC
     I              RHMINC                      ROMINC
     I              RHLINC                      ROLINC
     I              RHEINC                      ROEINC
     I              RHTOTI                      ROTOTI
     I              RHEXPL                      ROEXPL
     I              RHUSER                      ROUSER
      *** END ADD ****** 05/25/07 **********************************

     C*
     C********************************************************************
     C*  FIELD DEFINITIONS
     C********************************************************************
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMEMP#        XIEMP#
     C     *LIKE         DEFINE    CMDIV         XIDIV
     C     *LIKE         DEFINE    CMFYR         XIFYR
     C     *LIKE         DEFINE    CMCASE        XICASE
     C     *LIKE         DEFINE    RTKEY         XXKEY
     C     *LIKE         DEFINE    RHEFDT        XXEFDT
     C     *LIKE         DEFINE    RHTIME        XXTIME
     C     *LIKE         DEFINE    PAAMT         XXAMT
      **** BEGIN DEL **** 06/30/98 ************************************
     C********** *LIKE     DEFN RHEFDT    XXFRDT
     C********** *LIKE     DEFN RHEFDT    XXTRDT
     C********** *LIKE     DEFN RHEFDT    RSFRDT
     C********** *LIKE     DEFN RHEFDT    RSTRDT
      **** BEGIN DEL **** 06/30/98 ************************************
      **** BEGIN ADD **** 02/26/99 *********************************************
     C     *LIKE         DEFINE    RHEFDT        XXFRDT
     C     *LIKE         DEFINE    RHEFDT        XXTRDT
      **** END   ADD **** 02/26/99 *********************************************
      **** BEGIN DEL **** 02/26/99 *********************************************
     C********** *LIKE     DEFN RHEFDT    XOEFDT
      **** END   DEL **** 02/26/99 *********************************************
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C*** BEGIN ADD *** 04/30/97 **************************************
     C     *LIKE         DEFINE    RHEXPL        XXEXPL
     C     *LIKE         DEFINE    RHUSER        XXUSER
     C*** END   ADD *** 04/30/97 **************************************
     C********************************************************************
     C*  KEY LIST DEFINITIONS
     C********************************************************************
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE CLAIMS MASTER FILE
     C*
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     CMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XIFYR
     C*
     C     CMKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XIEMP#
     C                   KFLD                    XIDIV
     C*
     C     CMKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XIEMP#
     C                   KFLD                    XIDIV
     C                   KFLD                    XIFYR
     C*
     C     CMKY5         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XIFYR
     C                   KFLD                    XICASE
     C*
     C     CMKY6         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XIEMP#
     C                   KFLD                    XIDIV
     C                   KFLD                    XIFYR
     C                   KFLD                    XICASE
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE RESERVE HISTORY FILE
     C*
     C     RHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEFDT
     C                   KFLD                    XXTIME
     C*
     C     RHKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*** Begin add *** 01/31/05 **************************************
     C     RHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEFDT
     C*** End add ***** 01/31/05 **************************************
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C*** BEG CHG ****** 05/25/07 **********************************
     C********             KFLD           XXSS#
     C                   KFLD                    X1SS#             9
     C*** END CHG ****** 05/25/07 **********************************
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE PAYMENT DETAIL FILE
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE RESERVE TYPE TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
      *** Beg Add ***  11/25/13  ******************************************************
     C*   Claim/DARTS Control Definition File
     C*
     C     keyWMcxl      KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** End Add ***  11/25/13  ******************************************************
     C*
     C********************************************************************
     C*  ENTRY PARAMETERS
     C********************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
      **** BEGIN CHG **** 06/30/98 ************************************
     C                   PARM                    QQFYR             3
     C******************** PARM           QQFRDT  6
     C******************** PARM           QQTRDT  6
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTRDT            8
      **** BEGIN CHG **** 06/30/98 ************************************
     C                   PARM                    QQEMP#            9
     C*** BEGIN CHG *** 06/28/00 **************************************
     C******************** PARM           QQDIV   3
     C                   PARM                    QQDIV             5
     C*** END CHG ***** 06/28/00 **************************************
     C                   PARM                    QQCASE            7
     C                   PARM                    QQSTS             1
     C                   PARM                    QQLTF             1
     C                   PARM                    QQQUE            10
     C                   PARM                    QQRPT             1
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
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C*
     C********************************************************************
     C*  ONE TIME SETUP STUFF
     C********************************************************************
     C*** Beg Add ***  11/25/13  ******************************************************
      /Free
       RtvCltID(CltId);
      /End-Free
     C*** End Add ***  11/25/13  ******************************************************
     C                   TIME                    DSDTTM
     C                   MOVE      *ON           FIRST             1
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
      **** BEGIN CHG **** 06/30/98 ************************************
     C                   MOVE      QQFYR         XIFYR
     C******************** MOVE QQFRDT    XXFRDT
     C******************** Z-ADDXXFRDT    SRDTI
     C                   MOVE      QQFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
      **** BEGIN CHG **** 02/26/99 *********************************************
     C******************** MOVELSRDTO     XXFRDT  8
     C                   MOVEL     SRDTO         XXFRDT
      **** END   CHG **** 02/26/99 *********************************************
     C******************** MOVE QQTRDT    XXTRDT
     C******************** Z-ADDXXTRDT    SRDTI
      **** BEGIN CHG **** 06/30/98 ************************************
     C                   MOVE      QQTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
      **** BEGIN CHG **** 02/26/99 *********************************************
     C******************** MOVELSRDTO     XXTRDT  8
     C                   MOVEL     SRDTO         XXTRDT
      **** END   CHG **** 02/26/99 *********************************************
     C                   MOVE      QQEMP#        XIEMP#
     C                   MOVE      QQDIV         XIDIV
     C                   MOVE      QQCASE        XICASE
     C*
     C*
     C     XIFYR         COMP      *ZERO                                  81
     C     XIEMP#        COMP      *ZERO                                  82
     C     XICASE        COMP      *ZERO                                  83
     C*
     C                   MOVE      QQFRDT        RSFRDT            8 0
     C                   MOVE      QQTRDT        RSTRDT            8 0
     C*
     C                   SELECT
     C     QQSTS         WHENEQ    'O'
     C                   MOVEL     'OPEN'        RSSTS             6
     C     QQSTS         WHENEQ    'C'
     C                   MOVEL     'CLOSED'      RSSTS
     C                   OTHER
     C                   MOVEL     'BOTH'        RSSTS
     C                   ENDSL
     C*
     C                   SELECT
     C     QQLTF         WHENEQ    'L'
     C     'LOST'        CAT       'TIME':1      RSLTF             9
     C     QQLTF         WHENEQ    'N'
     C                   MOVEL     'MED ONLY'    RSLTF
     C                   OTHER
     C                   MOVEL     'BOTH'        RSLTF
     C                   ENDSL
     C*
     C     QQQUE         IFNE      *BLANK
     C                   SETON                                        09
     C                   ELSE
     C                   SETON                                        08
     C                   ENDIF
     C*
     C*  SET FILE ACCESS INDICATOR
     C*
     C                   SELECT
     C     XIFYR         WHENEQ    *ZERO
     C     XIEMP#        ANDEQ     *ZERO
     C     XICASE        ANDEQ     *ZERO
     C                   MOVE      *ON           *IN71
     C     XIFYR         WHENNE    *ZERO
     C     XIEMP#        ANDEQ     *ZERO
     C     XICASE        ANDEQ     *ZERO
     C                   MOVE      *ON           *IN72
     C     XIFYR         WHENEQ    *ZERO
     C     XIEMP#        ANDNE     *ZERO
     C     XICASE        ANDEQ     *ZERO
     C                   MOVE      *ON           *IN73
     C     XIFYR         WHENNE    *ZERO
     C     XIEMP#        ANDNE     *ZERO
     C     XICASE        ANDEQ     *ZERO
     C                   MOVE      *ON           *IN74
     C     XIFYR         WHENNE    *ZERO
     C     XIEMP#        ANDEQ     *ZERO
     C     XICASE        ANDNE     *ZERO
     C                   MOVE      *ON           *IN75
     C     XIFYR         WHENNE    *ZERO
     C     XIEMP#        ANDNE     *ZERO
     C     XICASE        ANDNE     *ZERO
     C                   MOVE      *ON           *IN76
     C                   ENDSL
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANK        UPUSNM
     C                   ENDIF
     C*
     C*  GET THE COMPANY AND FUND RECORDS
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANKS       CONAME
     C                   ENDIF
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANKS       FNNAME
     C                   ENDIF
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 04/10/00 ***********************************
     C*** Beg Add ***  11/25/13  ******************************************************
      /Free
       charLGRH = *Blanks;
       *IN79 = *Off;
       If CltId = '129';
          Chain keyWMcxl pcx01;
          If %Found(wmcxl);
             If CXlgrh <> *Blanks;
                charLGRH = CXlgrh;
             Else;
                charLGRH = 'LEGAL';
             EndIf;
       // *** Beg Add ***  03/11/14  **************************************
             charLGRH = RightJstfy(charLGRH:10);
       // *** End Add ***  03/11/14  ***************************************
             *IN79 = *On;
          Endif;
       EndIf;
      /End-Free
     C*** End Add ***  11/25/13  ******************************************************
     C*
     C*  GET THE DISTRIBUTION INFORMATION & PRINT ROUTE PAGE
     C*
      ***** Beg Del ***** 08/26/14 ***************************************
     C********           MOVE      '0'           XXPRT
     C********ORKY          CHAIN     POR01                              60
     C*
     C*********IN60         IFEQ      *OFF
     C*
     C********QQRPT         IFEQ      'P'
     C********QQRPT         OREQ      'B'
     C*
     C********           EXCEPT    ROUTE
     C*
     C********           ENDIF                                                  IFEQ QQRPT
     C********           ENDIF
      ***** End Del ***** 08/26/14 ***************************************
     C*
     C                   MOVE      *ON           *INOF
     C*
     C********************************************************************
     C*  MAIN LINE BEGINS HERE
     C********************************************************************
     C   71CMKY1         SETLL     PCM01
     C   72CMKY2         SETLL     PCM02
     C   73CMKY3         SETLL     PCM01
     C   74CMKY4         SETLL     PCM01
     C   75CMKY5         SETLL     PCM02
     C   76CMKY6         SETLL     PCM01
     C*
     C     *IN50         DOWEQ     *OFF
     C   71CMKY1         READE     PCM01                                  50
     C   72CMKY2         READE     PCM02                                  50
     C   73CMKY3         READE     PCM01                                  50
     C   74CMKY4         READE     PCM01                                  50
     C   75CMKY5         READE     PCM02                                  50
     C   76CMKY6         READE     PCM01                                  50
     C*
     C*  HANDLE EOF
     C*
     C     *IN50         IFEQ      *ON
     C                   ITER
     C                   ENDIF
     C*
     C*  VALIDATE OPEN/CLOSED STATUS
     C*
     C     QQSTS         IFNE      'B'
     C*
     C     QQSTS         IFNE      CMSTAT
     C                   ITER
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  VALIDATE AGAINST LOST TIME FLAG
     C*
     C     QQLTF         IFNE      'B'
     C*
     C     QQLTF         IFEQ      'L'
     C*** BEGIN CHG *** 10/29/99 **************************************
     C********** CMLTFL    ANDNE'L'
     C     CMLTFL        ANDNE     'I'
     C*** END CHG ***** 10/29/99 **************************************
     C     QQLTF         OREQ      'N'
     C*** BEGIN CHG *** 10/29/99 **************************************
     C********** CMLTFL    ANDEQ'L'
     C     CMLTFL        ANDEQ     'I'
     C*** END CHG ***** 10/29/99 **************************************
     C                   ITER
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  PROCESS THE RECORD
     C*
     C                   EXSR      SRDETL
     C*
     C                   ENDDO
     C*
     C     FIRST         IFEQ      *ON
     C*
     C     QQRPT         IFEQ      'P'
     C     QQRPT         OREQ      'B'
     C*
     C                   EXCEPT    NORECS
     C*
     C                   ENDIF                                                  IFEQ QQRPT
     C                   ENDIF
     C*
     C*  END THE PROGRAM
     C*
      /Free
       // ****** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKy POR01;
       If %found;
          If qqRpt = 'P' or qqRpt = 'B';
             Except Route;
          Endif;
       Endif;
       // ****** End Add ***** 08/26/14 ***********************************
      /End-free
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C*
     C*  THE FOLLOWING LINE WILL NEVER BE EXECUTED!!!!!  IT IS REQUIRED
     C*  TO ALLOW FOR I-SPECS FIELD RENAMING.  DO NOT REMOVE!!!!!
     C   LR
     CANNLR              READ      PRHOUT                                 LR
     C*
     C                   RETURN
     C********************************************************************
     C*  DETAIL PROCESSING FOR THE INPUT RECORD
     C********************************************************************
     C     SRDETL        BEGSR
     C*
     C*  SETUP KEY AND SAVE VALUES
     C*
     C                   MOVE      *ON           CLM1ST            1
     C                   MOVE      *ZERO         XXTITD           11 2
     C*
     C                   MOVE      CMFND         XXFND
     C                   MOVE      CMEMP#        XXEMP#
     C                   MOVE      CMDIV         XXDIV
     C*** BEG ADD ****** 05/25/07 **********************************************
     C******               MOVE CMSS#     XXSS#
     C                   MOVE      CMSS#         X1SS#
     C*** END ADD ****** 05/25/07 **********************************************
     C                   MOVE      CMFYR         XXFYR
     C                   MOVE      CMCASE        XXCASE
     C*
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *OFF          XXAUTH            1
     C                   MOVE      *BLANKS       XXSS#
     C                   ELSE
     C                   MOVE      *ON           XXAUTH
     C                   MOVE      CMSS#         XXSS#
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C*** Begin add *** 01/31/05 **************************************
     C                   MOVE      XXFRDT        XXEFDT
     C     RHKY1         SETLL     PRH01
     C     RHKY2         READE     PRH01                                  99
     C     *IN99         IFEQ      *OFF
     C     RHEFDT        ANDLE     XXTRDT
     C*** End add ***** 01/31/05 **************************************
     C*
     C                   MOVE      *HIVAL        XXTIME
     C                   MOVE      *HIVAL        XXEFDT
     C*
     C*  PROCESS THE RESERVE HISTORY RECORDS
     C*
     C     RHKY          SETLL     PRH02
     C     RHKY2         READE     PRH02                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*** BEGIN ADD *** 04/30/97 **************************************
     C                   MOVEL     RHEXPL        XXEXPL
     C                   MOVEL     RHUSER        XXUSER
     C*** END   ADD *** 04/30/97 **************************************
     C*
     C                   Z-ADD     RHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
      **** BEGIN CHG **** 02/26/99 *********************************************
     C******************** MOVELSRDTO     XOEFDT
     C                   MOVEL     SRDTO         XOEFDT            8 0
      **** END   CHG **** 02/26/99 *********************************************
     C                   EXSR      SRCHG
     C                   EXSR      SROUTS
     C*
     C*  FIRST TIME THRU FOR CLAIM, GET INFO & PRINT CLAIM HEADER
     C*
     C     CLM1ST        IFEQ      *ON
     C                   MOVE      *OFF          FIRST
     C                   MOVE      *OFF          CLM1ST
     C                   Z-ADD     XXTOTI        XXTITD
     C*
     C*  RETRIEVE EMPLOYER NAME
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  EMNAME        XXENAM           50
     C                   ELSE
     C                   MOVE      *BLANK        XXENAM
     C                   ENDIF
     C*
     C*  RETRIEVE CLAIMANT NAME
     C*
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      *OFF
     C     CDLSN         CAT(P)    ',':0         ROCLNM
     C                   CAT       CDFSN:1       ROCLNM
     C                   ELSE
     C                   MOVE      *BLANK        ROCLNM
     C                   ENDIF
     C*** BEG ADD ****** 05/25/07 **********************************************
     C* Check claim authority before moving claimant personal info
     C     XXAUTH        IFEQ      *OFF
     C                   MOVE      *BLANK        ROCLNM
     C                   MOVEL     XXATTX        ROCLNM
     C                   ENDIF
     C*** END ADD ****** 05/25/07 **********************************************
     C                   MOVEL     ROCLNM        XXCLNM           30
     C*
     C     CMSTAT        IFEQ      'C'
     C                   MOVEL(P)  'CLOSED'      XXSTAT            6
     C                   ELSE
     C                   MOVEL(P)  'OPEN'        XXSTAT
     C                   ENDIF
     C*
     C                   SELECT
     C*** BEGIN CHG *** 10/29/99 **************************************
     C********** CMLTFL    WHEQ 'L'
     C     CMLTFL        WHENEQ    'I'
     C*** END CHG ***** 10/29/99 **************************************
     C                   MOVEL(P)  'YES'         XXLTF            11
     C*** BEGIN CHG *** 10/29/99 **************************************
     C********** CMLTFL    WHEQ 'N'
     C     CMLTFL        WHENEQ    'M'
     C*** END CHG ***** 10/29/99 **************************************
     C                   MOVEL(P)  'NO'          XXLTF
     C     CMLTFL        WHENEQ    'H'
     C                   MOVEL(P)  'HELD'        XXLTF
     C     CMLTFL        WHENEQ    'R'
     C     'RECORD'      CAT(P)    'ONLY':1      XXLTF
     C                   ENDSL
     C*
     C*  PRINT THE CASE HEADER
     C*
     C     QQRPT         IFEQ      'P'
     C     QQRPT         OREQ      'B'
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'XXSS#'       QQNAME           25
     C     XXSS#         PARM      XXSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   EXCEPT    CASEHD
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'XXSS#'       QQNAME           25
     C     XXSS#         PARM      XXSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C*
     C                   ENDIF                                                   IFEQ QQRPT
     C*
     C                   ENDIF                                                  CLM1ST = *ON
     C*
     C*  IF RESERVE HISTORY DATE WITHIN DATE RANGE, PRINT THE DETAIL DATA
     C*
      **** BEGIN CHG **** 06/30/98 ************************************
     C********** RHEFDT    IFGE XXFRDT
     C********** RHEFDT    ANDLEXXTRDT
      **** BEGIN CHG **** 02/26/99 *********************************************
     C********** QQFRDT    IFGE XXFRDT
     C********** QQTRDT    ANDLEXXTRDT
     C*** BEGIN DLT *** 10/29/99 **************************************
     C********** RHEFDT    IFGE XXFRDT
     C********** RHEFDT    ANDLEXXTRDT
     C*** END DLT ***** 10/29/99 **************************************
      **** END   CHG **** 02/26/99 *********************************************
      **** END   CHG **** 06/30/98 ************************************
     C*** BEGIN ADD *** 10/29/99 **************************************
     C     XXEFDT        IFGE      XXFRDT
     C     XXEFDT        ANDLE     XXTRDT
     C*** END ADD ***** 10/29/99 **************************************
     C*
     C     *INOF         IFEQ      *ON
     C*
     C     QQRPT         IFEQ      'P'
     C     QQRPT         OREQ      'B'
     C*
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C*
     C                   ENDIF                                                  IFEQ QQRPT
     C                   ENDIF
     C*
     C     QQRPT         IFEQ      'P'
     C     QQRPT         OREQ      'B'
     C*
     C                   EXCEPT    DETL
     C*
     C                   ENDIF                                                  IFEQ QQRPT
     C*
     C     QQRPT         IFEQ      'F'
     C     QQRPT         OREQ      'B'
     C*
     C                   EXSR      SRWRKF
     C*
     C                   ENDIF                                                  IFEQ QQRPT
     C                   ENDIF
     C*
     C     RHKY2         READE     PRH02                                  60
     C                   ENDDO
     C*
     C*** Begin add *** 01/31/05 **************************************
     C                   ENDIF
     C*** End add ***** 01/31/05 **************************************
     C*
     C                   ENDSR
     C*****************************************************************
     C*  SUBROUTINE TO CALCULATE RESERVE CHANGES
     C*****************************************************************
     C     SRCHG         BEGSR
     C*
     C                   MOVE      *ZERO         XXCCHG           11 2
     C                   MOVE      *ZERO         XXMCHG           11 2
     C                   MOVE      *ZERO         XXLCHG           11 2
     C                   MOVE      *ZERO         XXECHG           11 2
     C*
     C                   DO        10            Z                 2 0
     C*
     C     RT(Z)         IFNE      *BLANKS
     C                   MOVEL(P)  RT(Z)         XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C*
     C                   SELECT
     C     RTTYPE        WHENEQ    'C'
     C                   ADD       SU(Z)         XXCCHG
     C     RTTYPE        WHENEQ    'M'
     C                   ADD       SU(Z)         XXMCHG
     C     RTTYPE        WHENEQ    'L'
     C                   ADD       SU(Z)         XXLCHG
     C     RTTYPE        WHENEQ    'E'
     C                   ADD       SU(Z)         XXECHG
     C                   ENDSL
     C*
     C                   ENDIF
     C                   ENDDO
     C*
     C                   ENDSR
     C*****************************************************************
     C*  SUBROUTINE TO CALCULATE OUTSTANDING RESERVES
     C*****************************************************************
     C     SROUTS        BEGSR
     C*
     C                   MOVE      *ZERO         XXCRSV           11 2
     C                   MOVE      *ZERO         XXMRSV           11 2
     C                   MOVE      *ZERO         XXLRSV           11 2
     C                   MOVE      *ZERO         XXERSV           11 2
     C                   MOVE      *ZERO         XXCINC           11 2
     C                   MOVE      *ZERO         XXMINC           11 2
     C                   MOVE      *ZERO         XXLINC           11 2
     C                   MOVE      *ZERO         XXEINC           11 2
     C                   MOVE      *ZERO         XXTOTI           11 2
     C                   MOVE      RHEFDT        XXEFDT
     C                   MOVE      RHTIME        XXTIME
     C*
     C*  GET THE SUM OF ALL RESERVE CHANGES UP TO THE DATE AND TIME OF
     C*  THE RECORD BEING DISPLAYED.
     C*
     C     RHKY2         SETLL     PRH01
     C     RHKY2         READE     PRH01                                  51
     C*
     C     *IN51         DOWEQ     *OFF
     C     RHEFDT        IFLT      XXEFDT
     C     RHEFDT        OREQ      XXEFDT
     C     RHTIME        ANDLE     XXTIME
     C*
     C                   DO        10            Z
     C*
     C     RT(Z)         IFNE      *BLANKS
     C                   MOVEL(P)  RT(Z)         XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C*
     C                   SELECT
     C     RTTYPE        WHENEQ    'C'
     C                   ADD       SU(Z)         XXCRSV
     C                   ADD       SU(Z)         XXCINC
     C     RTTYPE        WHENEQ    'M'
     C                   ADD       SU(Z)         XXMRSV
     C                   ADD       SU(Z)         XXMINC
     C     RTTYPE        WHENEQ    'L'
     C                   ADD       SU(Z)         XXLRSV
     C                   ADD       SU(Z)         XXLINC
     C     RTTYPE        WHENEQ    'E'
     C                   ADD       SU(Z)         XXERSV
     C                   ADD       SU(Z)         XXEINC
     C                   ENDSL
     C*
     C                   ADD       SU(Z)         XXTOTI
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C*
     C     RHKY2         READE     PRH01                                  51
     C                   ENDDO
     C*
     C*  SUBTRACT ALL PAYMENTS UP TO THE DATE AND TIME OF THE RECORD
     C*  BEING DISPLAYED.
     C*
     C     PAKY          CHAIN     PPA01                              52
     C     *IN52         DOWEQ     *OFF
     C*
     C     PATRDT        IFLT      XXEFDT
     C     PATRDT        OREQ      XXEFDT
     C     PATIME        ANDLE     XXTIME
     C*
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'A'
     C     PATYPE        OREQ      'Z'
     C                   Z-ADD     PAAMT         XXAMT
     C                   ELSE
     C                   Z-SUB     PAAMT         XXAMT
     C                   ENDIF
     C*
     C                   SELECT
     C     PARETP        WHENEQ    'C'
     C                   SUB       XXAMT         XXCRSV
     C     PARETP        WHENEQ    'M'
     C                   SUB       XXAMT         XXMRSV
     C     PARETP        WHENEQ    'L'
     C                   SUB       XXAMT         XXLRSV
     C     PARETP        WHENEQ    'E'
     C                   SUB       XXAMT         XXERSV
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C     PAKY          READE     PPA01                                  52
     C                   ENDDO
     C*
     C                   ENDSR
     C*****************************************************************
     C*  SUBROUTINE TO LOAD & WRITE THE WFRHP WORKFILE RECORD
     C*****************************************************************
     C*
     C     SRWRKF        BEGSR
     C*
     C*
     C                   CLEAR                   PRHOUT
     C*
     C                   MOVE      XXCO#         ROCO#
     C                   MOVE      XXFND         ROFND
     C                   MOVE      XXFYR         ROFYR
     C                   MOVE      XXCASE        ROCASE
     C                   MOVE      XXEMP#        ROEMP#
     C                   MOVE      XXDIV         RODIV
     C                   MOVEL     XXENAM        ROENAM
     C                   MOVEL     XXSTAT        ROSTAT
     C                   MOVEL     XXLTF         ROLTF
     C                   MOVEL     XXSS#         ROSS#
      **** BEGIN CHG **** 12/14/99 ************************************
     C*                    MOVELXXCLNM    ROCLNM
     C                   MOVEL     XXCLNM        ROCLNM
      **** END   CHG **** 12/14/99 ************************************
     C                   MOVE      XXTITD        ROTITD
     C                   MOVE      XXEFDT        ROEFDT
     C                   MOVE      XXCCHG        ROCCHG
     C                   MOVE      XXMCHG        ROMCHG
     C                   MOVE      XXLCHG        ROLCHG
     C                   MOVE      XXECHG        ROECHG
     C                   MOVE      XXCRSV        ROCRSV
     C                   MOVE      XXMRSV        ROMRSV
     C                   MOVE      XXLRSV        ROLRSV
     C                   MOVE      XXERSV        ROERSV
     C                   MOVE      XXCINC        ROCINC
     C                   MOVE      XXMINC        ROMINC
     C                   MOVE      XXLINC        ROLINC
     C                   MOVE      XXEINC        ROEINC
     C                   MOVE      XXTOTI        ROTOTI
     C*** BEGIN DEL ** 04/30/97 ***************************************
     C**********           MOVELRHEXPL    ROEXPL
     C**********           MOVELRHUSER    ROUSER
     C*** END   DEL ** 04/30/97 ***************************************
     C*** BEGIN ADD ** 04/30/97 ***************************************
     C                   MOVEL     XXEXPL        ROEXPL
     C                   MOVEL     XXUSER        ROUSER
     C*** END   ADD ** 04/30/97 ***************************************
     C*
     C                   WRITE     PRHOUT
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     O*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ***************************************
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
     O*********      08      ORDIS1              60
     O*********         09   UPUSNM              67
     O********* E            ROUTE          1
     O*********      08      ORDIS2              60
     O********* E            ROUTE          1
     O*********      08      ORDIS3              60
     O********* E            ROUTE          2
     O*********      08      ORDIS4              60
      ***** End Del ****** 08/26/14 *******************************************
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
      ***** Beg Del ****** 08/26/14 *******************************************
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
     O*********         09   QQQUE               30
     O*********                                  37 'FORMS:'
     O*********              ORFORM              48
     O*********                                  56 'SAVE:'
     O*********              ORSAVE              61
      ***** End Del ****** 08/26/14 *******************************************
     O          E            ROUTE          1
      **** BEGIN CHG **** 06/12/98 ************************************
     O********************************** 20 'FUND         :'
     O                                           20 'GROUP        :'
      **** END   CHG **** 06/12/98 ************************************
     O                       XXFND         Z   +002
     O          E            ROUTE          1
     O                                           20 'DATE RANGE   :'
      **** BEGIN CHG **** 06/12/98 ************************************
     O************************ RSFRDT    31 '  /  /   '
     O                       RSFRDT              31 '  /  /    '
     O                                           33 '-'
     O************************ RSTRDT    44 '  /  /   '
     O                       RSTRDT              44 '  /  /    '
     O*
     O          E            ROUTE          1
     O*** BEGIN CHG ***  10/20/98  ************************************
     O********************************** 20 'FUND YEAR    :'
     O*************** N81      XIFYR     25
     O***************  81                36 'ALL FUND YEARS'
     O                                           20 'POLICY YEAR  :'
     O              N81      XIFYR               25
     O               81                          38 'ALL POLICY YEARS'
     O*** END   CHG ***  10/20/98  ************************************
     O          E            ROUTE          1
     O                                           20 'EMPLOYER     :'
      **** END   CHG **** 06/12/98 ************************************
     O              N82      XIEMP#        Z     27
     O              N82      XIDIV         Z     31
     O               82                          35 'ALL EMPLOYERS'
     O          E            ROUTE          1
     O                                           20 'CASE NUMBER  :'
     O              N83      XICASE        Z     29
     O               83                          38 'ALL CASE NUMBERS'
     O          E            ROUTE          1
     O                                           20 'CASE STATUS  :'
     O                       RSSTS               28
     O          E            ROUTE          1
      **** BEGIN CHG **** 06/12/98 ************************************
     O*******************************    20 'LOST TIME    :'
     O                                           20 'CLAIM TYPE   :'
      **** END   CHG **** 06/12/98 ************************************
     O                       RSLTF               31
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
      *
      ****** BEG DLT ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS, INC.'
      ****** END DLT ***** 01/28/10 **********************************
      **** BEGIN CHG **** 06/12/98 ************************************
     O*******************************  +000 ', 1997'
      ****** BEG DLT ***** 01/28/10 **********************************
     O***                                47 ','
     O****                     *YEAR   +  1
      ****** END DLT ***** 01/28/10 **********************************
      **** END   CHG **** 06/12/98 ************************************
     O          E            HEAD           1 03
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              86
     O                       DSDATE        Y    130
     O          E            HEAD           2
     O                       Q1PGM               10
      **** BEGIN CHG **** 12/14/99 ************************************
     O********************     ORTITL   108
     O                       ORTITL              89
      **** END   CHG **** 12/14/99 ************************************
     O                                          126 'PAGE:'
     O                       PAGE          Z    130
     O          E            HEAD           1
      **** BEGIN CHG **** 06/12/98 ************************************
     O************************           06 'FUND:'
     O************************ XXFND Z   10
     O************************ FNNAME    71
     O                                           07 'GROUP:'
     O                       XXFND         Z     11
     O                       FNNAME              72
      **** END   CHG **** 06/12/98 ************************************
     O                                           93 'RESERVE DATE RANGE:'
     O                       RSFRDT        Y    104
     O                                          106 '-'
     O                       RSTRDT        Y    117
     O          E            HEAD           2
      **** BEGIN CHG **** 06/12/98 ************************************
     O***************                    11 'FUND YEAR:'
     O*************** N81      XIFYR Z   16
     O***************  81                16 '*ALL'
     O                                           13 'POLICY YEAR:'
     O              N81      XIFYR         Z     18
     O               81                          18 '*ALL'
     O                                           28 'EMPLOYER:'
     O*************** N82      XIEMP#Z   34
     O*************** N82      XIDIV     38
     O***************  82                33 '*ALL'
     O***************                    46 'CASE:'
     O*************** N83      XICASEZ   58
     O***************  83                52 '*ALL'
     O              N82      XIEMP#        Z     37
     O              N82      XIDIV         Z     41
     O               82                          33 '*ALL'
     O                                           49 'CASE:'
     O              N83      XICASE        Z     61
     O               83                          55 '*ALL'
      **** END   CHG **** 06/12/98 ************************************
     O          E            HEAD           0
     O                                           10 '_________'
     O                                           30 '_______________'
     O                                           50 '_______________'
     O                                           70 '_______________'
     O                                           90 '_______________'
     O                                          110 '_______________'
     O          E            HEAD           1
     O                                           10 'DATE CHGD'
     O                                           30 'COMPENSATION'
     O                                           50 'MEDICAL'
      *** Beg Add ***  11/25/13  ******************************************************
     O*****                                      70 'LEGAL'
     O              N79                          70 'LEGAL'
     O               79      charLGRH            70
      *** End Add ***  11/25/13  ******************************************************
     O                                           90 'EXPENSE'
     O                                          110 'TOTAL INCURRED'
     O          E            CASEHD      1  1
      **** BEGIN CHG **** 06/12/98 ************************************
     O******************************** +002 'FUND YEAR:'
     O                                         +002 'POLICY YEAR:'
      **** BEGIN CHG **** 06/12/98 ************************************
     O                       XXFYR         Z   +001
     O                                         +002 'CASE:'
     O                       XXCASE        Z   +001
     O                                         +003 'EMPLOYER:'
     O                       XXEMP#        Z   +001
     O                                         +000 '/'
     O                       XXDIV         Z   +000
     O                       XXENAM            +002
     O          E            CASEHD         1
     O                                         +003 'STS:'
     O                       XXSTAT            +001
     O                                         +002 'LOST TIME:'
     O                       XXLTF             +001
     O                                         +002 'SS#:'
     O                       XXSS#1            +001
     O                                         +000 '-'
     O                       XXSS#2            +000
     O                                         +000 '-'
     O                       XXSS#3            +000
     O                                         +002 'CLAIMANT:'
     O                       XXCLNM            +001
     O                                         +002 'TOT INCRD TO DT:'
     O                       XXTITD        J   +000
     O          E            DETL        1  1
     O                       XOEFDT              10 '  /  /    '
     O                       XXCCHG        J     30
     O                       XXMCHG        J     50
     O                       XXLCHG        J     70
     O                       XXECHG        J     90
     O          E            DETL           1
     O                                              'OUTSTANDING:'
     O                       XXCRSV        J     30
     O                       XXMRSV        J     50
     O                       XXLRSV        J     70
     O                       XXERSV        J     90
     O          E            DETL           1
     O                                              'TOTAL INCURRED:'
     O                       XXCINC        J     30
     O                       XXMINC        J     50
     O                       XXLINC        J     70
     O                       XXEINC        J     90
     O          E            DETL           1
     O                                            6 'USER:'
     O*** BEGIN DEL *** 04/30/97 **************************************
     O**********               RHUSER    17
     O*** END   DEL *** 04/30/97 **************************************
     O*** BEGIN ADD *** 04/30/97 **************************************
     O                       XXUSER              17
     O*** END   ADD *** 04/30/97 **************************************
     O                                           27 'REASON:'
     O*** BEGIN DEL *** 04/30/97 **************************************
     O**********               RHEXPL    88
     O*** END   DEL *** 04/30/97 **************************************
     O*** BEGIN ADD *** 04/30/97 **************************************
     O                       XXEXPL              88
     O*** END   ADD *** 04/30/97 **************************************
     O                       XXTOTI        J    110
     O          E            NORECS      3  1
     O                                         +044 'NO RECORDS FOUND FOR'
     O                                         +000 ' THE SELECTION CRITE'
     O                                         +000 'RIA'
