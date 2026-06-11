     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W4900R - CALENDAR EARNED/UNEARNED PREMIUM REPORT - PROMPT
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*   THIS PROGRAM PROMPTS FOR THE BEGINNING AND ENDING DATES     *
     H*   FOR THE CALENDAR EARNED/UNEARNED PREMIUM REPORT AND         *
     H*   SUBMITS THE REPORT.                                         *
     H* CALLED FROM:                                                  *
     H*   M0002R                                                      *
     H* CALLS TO:                                                     *
     H*   W4900C                                                      *
     H* PROGRAMMER: C:24                                              *
     H* CREATED: 08/14/95               PROJECT: WC311                *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/13/15 C:242 PHW37852
     H*                 Removed Compiler director if defined(web)
     H*
     H* 12/02/09 C:136 PROJECT: 25WPZ                                 *
     H*                ADD SCR02 FOR FISCAL YEAR PROCESSING           *
     H*                                                               *
     H* 04/22/09 C:136 SUPPORT: WA139 65431                           *
     H*                ADD FUNCTIONALITY FROM COMPARISON RUN          *
     H*                                                               *
     H* 02/29/08 C:136 PROJECT: WC1653                                *
     H*              - ADD VERBIAGE TO INDICATE LOCATION OF CEU       *
     H*                SUMMARY FILE (WFEUP2)                          *
     H*              - ALSO ADDED LOGIC TO POPULATE TITLE IF NO       *
     H*                PARMS ARE PASSED IN FOR EASE OF DEBUGGING      *
     H*                                                               *
     H* 06/26/03 C:61  SUPPORT WA129  37721                           *
     H*              - Beginning date must be Jan 1 for "C"alendar    *
     H*                type reports.                                  *
     H*              - Date range can not cross years for "C" type    *
     H*                reports.                                       *
     H*                                                               *
     H* 10/22/02  C:142  PROJECT: WC1094                              *
     H*                  ADD PARMS FOR SORT                           *
     H*                                                               *
     H* 10/01/01  C:35   PROJECT: WC577                               *
     H*                  CHANGE SO THAT LEAVING GROUP RANGE BLANK     *
     H*                  RUNS REPORT FOR ALL GROUPS.                  *
     H*                                                               *
     H* 05/14/01  C:136  PROJECT: WC577                               *
     H*                  Display location of output file.             *
     H*                                                               *
     H* 02/28/01  C:35   PROJECT: WC577                               *
     H*                  ADD CODE FOR GROUP RANGE AND INCL/EXCL EXP   *
     H*                  CONSTANT.                                    *
     H*                                                               *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H*  20  INVALID FROM DATE                                        *
     H*  21  INVALID TO DATE                                          *
     H*  22  DATES NOT IN SEQUENCE                                    *
     H*  23  INVALID PRINT TYPE                                       *
     H*  24  INVALID DATE RANGE TYPE                                  *
     H*  25  FROM DATE NOT EQUAL JANUARY 1 FOR CALENDAR TYPE          *
     H*  60  GENERAL INDICATOR USED TO CHAIN TO FILES                 *
     H*****************************************************************
     F*
     F*** BEGIN CHG *** 10/13/15 **************************************
     F*****W4900F    CF   E             WORKSTN
      *****/IF DEFINED (WEB)
     F*****                                HANDLER('PROFOUNDUI(HANDLER)')
      *****/ENDIF
     F*****                                INFDS(FIDS01)
     FW4900F    CF   E             WORKSTN HANDLER('PROFOUNDUI(HANDLER)')
     F                                     INFDS(FIDS01)
     F*** End   CHG *** 10/13/15 **************************************
     FSMCOL     IF   E           K DISK
      **** BEGIN DEL **** 10/01/01 *********************************************
     F**********WMFNL   IF  E           K        DISK
      **** END   DEL **** 10/01/01 *********************************************
     FWMUPL     IF   E           K DISK
      *** BEG ADD ***** 02/29/08 ***********************************************
     FSWCPF     IF   E             DISK
      *** BEG ADD ***** 12/02/09 ***********************************************
     FWDF2P     IF   E             DISK
      *** END ADD ***** 12/02/09 ***********************************************

     D VB              S             31    DIM(4) CTDATA PERRCD(1)
     D TIT             S             70    DIM(1) CTDATA PERRCD(1)
      *** END ADD ***** 02/29/08 ***********************************************
     D*
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXDATE                 1      8  0
     D  XXMMDD                 1      4  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#             3 0
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      **** BEGIN DEL **** 10/01/01 *********************************************
     C**********
     C** FUND MASTER FILE
     C**********
     C********** FNKY      KLIST
     C**********           KFLD           UPCO#
     C**********           KFLD           SCFND
      **** END   DEL **** 10/01/01 *********************************************
     C*
     C     *LIKE         DEFINE    SCFRDT        XXFRDT
     C     *LIKE         DEFINE    SCTODT        XXTODT
     C     *LIKE         DEFINE    SCVLDT        XXVLDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANK        SRFFMT            8
     C                   PARM      *BLANK        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANK        SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     STRT01        TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         XXCO#
     C                   ELSE
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPOUTQ        XXOUTQ           10
     C                   ENDIF
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *** BEG ADD ***** 02/29/08 ***********************************************
      *
      *  GET SUMMARY FLAG VALUE FROM SYSTEM TABLE VALUES
      *
     C                   READ      SWCPF                                  60
      *** END ADD ***** 02/29/08 ***********************************************
      *** BEG ADD ***** 12/02/09 *****************************************
      *
      *  SEE IF FISCAL YEAR PROCESSING IS INSTALLED BY LOOKING AT THE CEUINFO
      *  DATA AREA.  IF THE PRODUCTION VERSION OF THE CEU IS 101 OR GREATER,
      *  EXCLUDING THE V91 AND V92 VERSIONS THEN FISCAL YEAR PROCESSING IS
      *  ACTIVE.
      *
     C                   MOVE      *ON           COMPAR            1
     C                   MOVE      *OFF          FYRFLG            1
     C                   CALL      'CEUINFO'                            99
     C                   PARM                    CEUINF           40
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     CEUINF        XXVER1           10
     C                   MOVE      XXVER1        XXVER#            3 0
     C     XXVER#        IFGE      101
     C     XXVER#        ANDNE     910
     C     XXVER#        ANDNE     920
     C                   MOVE      *ON           FYRFLG            1
     C                   ENDIF                                                  XXVER# GE 101
     C                   MOVEL     CEUINF        XXVER2           20
     C                   MOVE      XXVER2        XXVER#            3 0
     C     FYRFLG        IFEQ      *OFF
     C     XXVER#        IFGE      101
     C     XXVER#        ANDNE     910
     C     XXVER#        ANDNE     920
     C     COFYSM        IFNE      0
     C                   MOVE      *OFF          COMPAR            1
     C                   ENDIF                                                  COFYSM NE 0
     C                   READ      PF201                                  60
     C     *IN60         DOWEQ     *OFF
     C     COMPAR        ANDEQ     *ON
     C     F2FYSM        IFNE      *ZERO
     C                   MOVE      *OFF          COMPAR
     C                   LEAVE
     C                   ENDIF                                                  F2FYSM NE 0
     C                   READ      PF201                                  60
     C                   ENDDO
     C                   ENDIF                                                  XXVER# GE 101
     C                   ENDIF                                                  FYRFLG = *OFF
     C     FYRFLG        IFEQ      *ON
     C     XXVER#        IFLT      101
     C     XXVER#        OREQ      910
     C     XXVER#        OREQ      920
     C     COFYSM        IFNE      0
     C                   MOVE      *OFF          COMPAR            1
     C                   ENDIF                                                  COFYSM NE 0
     C                   READ      PF201                                  60
     C     *IN60         DOWEQ     *OFF
     C     COMPAR        ANDEQ     *ON
     C     F2FYSM        IFNE      *ZERO
     C                   MOVE      *OFF          COMPAR
     C                   LEAVE
     C                   ENDIF                                                  F2FYSM NE 0
     C                   READ      PF201                                  60
     C                   ENDDO
     C                   ENDIF                                                  XXVER# GE 101
     C                   ENDIF                                                  FYRFLG = *OFF
     C                   ENDIF                                                  *IN99= *OFF
      *** END ADD ***** 12/02/09 *****************************************
     C*
     C*  GET THE FUND NAME
     C*
      **** BEGIN DEL **** 10/01/01 *********************************************
     C**********           MOVE UPFND     SCFND
     C********** FNKY      CHAINPFN01                60
     C**********           MOVE FNNAME    SCNAME
      **** END   DEL **** 10/01/01 *********************************************
     C*
     C                   MOVE      Q1USER        SCUSER
      *** BEG CHG ***** 02/29/08 ***********************************************
     C***********          MOVE QQPRDS    SCPRDS
     C     Q1PRMS        IFGE      1
     C                   MOVE      QQPRDS        SCPRDS
     C                   ELSE
     C                   MOVEL     TIT(1)        SCPRDS
     C                   ENDIF
      *
      * CREATED XXCALL IN CASE IT IS EVER NEEDED
      *
     C     Q1PRMS        IFGE      2
     C                   MOVE      QQCALL        XXCALL            1
     C                   ELSE
     C                   MOVEL     *BLANKS       XXCALL
     C                   ENDIF
      *** END CHG ***** 02/29/08 ***********************************************
     C                   MOVE      'C'           SCTYPE
     C                   MOVE      'D'           SCPRNT
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
      *** BEG CHG ***** 12/02/09 ***********************************************
     C***********          MOVEL'SCR01'   Q2RECF    P
     C                   SELECT
     C     FYRFLG        WHENEQ    *ON
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   OTHER
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   ENDSL
      *** END CHG ***** 12/02/09 ***********************************************
     C*
     C*** BEG ADD ******** 05/14/01 ********************************************
     C*
     C*   DISPLAY LOCATION OF OUTPUT FILE
     C*
     C                   CALL      'SA030R'
     C                   PARM                    QQSUFX            3
     C     'PDSRPT'      CAT(P)    QQSUFX        XXRPTL           10
     C*** BEGIN CHANGE *** 10/22/02 ********************************************
     C*                    CAT  '.':0     XXRPTL    P
     C*** BEG DEL ***** 02/29/08 ***********************************************
     C**********           MOVE XXRPTL    SCRPTL
      **********
     C********** XXRPTL    CAT  ', if s':0SCRPTL    P
     C**********           CAT  'electe':0SCRPTL
     C**********           CAT  'd.':0    SCRPTL
     C*** END DEL ***** 02/29/08 ***********************************************
     C*** END   CHANGE *** 10/22/02 ********************************************
     C*
     C*** END ADD ******** 05/14/01 ********************************************
     C*** BEG ADD ***** 02/29/08 ***********************************************
      *
      *  LODCATION OF WFEUP AND WFEUP2
      *
     C                   CLEAR                   SCVBG1
     C                   CLEAR                   SCVBG2
     C                   MOVEL     VB(1)         SCVBG1
     C     WCFL37        IFEQ      'Y'
     C                   CAT       VB(2):1       SCVBG1
     C                   CALL      'SRCENTER'
     C                   PARM      SCVBG1        SRDFLD           80
     C                   PARM      76            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCVBG1
     C                   ENDIF
     C     WCFL37        IFEQ      'Y'
     C     VB(3)         CAT       XXRPTL:1      SCVBG2
     C                   CAT       VB(4):0       SCVBG2
     C                   CALL      'SRCENTER'
     C                   PARM      SCVBG2        SRDFLD           80
     C                   PARM      76            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCVBG2
     C                   ELSE
     C                   CAT       VB(3):1       SCVBG1
     C                   CAT       XXRPTL:1      SCVBG1
     C                   CAT       VB(4):0       SCVBG1
     C                   CALL      'SRCENTER'
     C                   PARM      SCVBG1        SRDFLD           80
     C                   PARM      76            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCVBG1
     C                   ENDIF
     C*** END ADD ***** 02/29/08 ***********************************************
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
      *** BEG CHG ***** 12/02/09 ***********************************************
     C********** Q2RECF    IFEQ 'SCR01'
     C**********           EXFMTSCR01
     C**********           ENDIF
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR01'
     C                   EXFMT     SCR01
     C     Q2RECF        WHENEQ    'SCR02'
     C                   EXFMT     SCR02
     C                   ENDSL
      *** END CHG ***** 12/02/09 ***********************************************
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      *ON
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C*
     C                   CALL      'M0006C'
     C                   PARM      'QHLP'        FILNAM           10
     C                   PARM      '*LIBL'       LIBNAM           10
     C                   PARM                    MBRNAM
     C*
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      *ON
     C                   CALL      'W0006R'
     C                   GOTO      STRT01
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN11 = *ON
     C*
      *** BEG ADD ***** 12/02/09 *************************************
     C     Q2RECF        IFEQ      'SCR01'
      *** END ADD ***** 12/02/09 *************************************
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*   VALIDATE THE "FROM DATE"
     C*
     C     SCTYPE        IFNE      'C'
     C     SCFRDT        ORNE      *ZERO
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*   VALIDATE THE "TO DATE"
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  THE DATES MUST BE IN SEQUENCE
     C*
     C     SCTYPE        IFNE      'C'
     C     SCFRDT        ORNE      *ZERO
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C     XXFRDT        IFGT      XXTODT
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  VALIDATE THE DATE RANGE TYPE FLAG.
     C*
     C     SCTYPE        IFNE      'C'
     C     SCTYPE        ANDNE     'P'
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
     C                   ENDIF
      ***** BEG DEL ******* 06/26/03 **********************************
     C*********
     C***FOR*'C' TYPE DATE RANGE, THE FIRST DATE MUST BE JANUARY 1.
     C*********
     C*********  SCTYPE    IFEQ 'C'
     C*********
     C*********            MOVE SCTODT    XXDATE
     C*********            MOVE '0101'    XXMMDD
     C*********
     C*********  SCFRDT    IFNE XXDATE
     C*********
     C*********  SCFRDT    IFEQ *ZERO
     C*********            MOVE XXDATE    SCFRDT
     C*********            ELSE
     C*********            MOVE *ON       *IN25
     C*********            ENDIF
     C*********
     C*********            GOTO STRT
     C*********            ENDIF
     C*********
     C*********            ENDIF
     C*********
      ***** END DEL ******* 06/26/03 **********************************
      ***** BEG ADD ******* 06/26/03 **********************************
      *
      *  For 'C' type date range, the first date must be January 1.
      *
     C     SCTYPE        IFEQ      'C'
     C                   MOVEL     SCFRDT        XXTMP1            4
      *
     C     XXTMP1        IFNE      '0101'
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  For 'C'alendar rpt, the second date must be in the same year.
      *
     C                   MOVE      SCFRDT        XXTMP2            4 0
     C                   MOVE      SCTODT        XXTMP3            4 0
      *
     C     XXTMP2        IFNE      XXTMP3
     C                   MOVE      *ON           *IN71
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ENDIF
      *
      ***** END ADD ******* 06/26/03 **********************************
     C*
     C*  FOR 'P' TYPE DATE RANGE, EDIT THE EARNED PREM VALUED AS OF DATE.
     C*
     C     SCTYPE        IFEQ      'P'
     C*
     C*  THE VALUED AS OF DATE MUST BE VALID
     C*
     C                   Z-ADD     SCVLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCVLDT
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  THE VALUED AS OF DATE CANNOT BE LESS THAN THE "TO" DATE
     C*
     C                   Z-ADD     SCVLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXVLDT
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C     XXVLDT        IFLT      XXTODT
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCTYPE = P
      *
      *** BEG ADD ***** 12/02/09 **********************************************
     C                   ENDIF                                                  Q2RECF = 'SCR01'
      *
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C*  THE VALUED AS OF DATE MUST BE VALID
     C*
     C                   Z-ADD     SCVLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCVLDT
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF
      *
      * VALIDATE FROM AND TO DATE FOR P TYPE RUN ONLY
      *
     C     SCTYPE        IFEQ      'P'
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*   VALIDATE THE "TO DATE"
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  THE "FROM" DATE CANNOT BE GREATER THAN THE "TO" DATE
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C     XXFRDT        IFGT      XXTODT
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  THE VALUED AS OF DATE CANNOT BE LESS THAN THE "TO" DATE
     C*
     C                   Z-ADD     SCVLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXVLDT
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C     XXVLDT        IFLT      XXTODT
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF                                                  SCTYPE = 'P'
      *
      *  LET THE USER KNOW THAT THE RANGE DATES AREN'T USED UNLESS THE POLICY
      *  TYPE IS 'P'
      *
     C     SCTYPE        IFNE      'P'
     C     SCFRDT        ANDNE     0
     C     SCTYPE        ORNE      'P'
     C     SCTODT        ANDNE     0
     C                   MOVE      *ON           *IN37
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  VALIDATE THE DATE RANGE TYPE FLAG.
     C*
     C     SCTYPE        IFNE      'C'
     C     SCTYPE        ANDNE     'F'
     C     SCTYPE        ANDNE     'P'
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF                                                  Q2RECF = 'SCR02'
      *
      *  DO NOT ALLOW A COMPARISON RUN BETWEEN A VERSION OF THE CEU THAT
      *  WILL RUN BY FISCAL YEAR AND ONE THAT DOESN'T PROVIDING THAT A FISCAL
      *  YEAR START MONTH IS PROVIDED.
      *
     C     XXCALL        IFEQ      'C'
     C     COMPAR        ANDEQ     *OFF
     C                   MOVE      *ON           *IN36
     C                   GOTO      STRT
     C                   ENDIF
      *** END ADD ***** 12/02/09 **********************************************
     C*
     C*  VALIDATE THE PRINT TYPE FLAG.
     C*
     C     SCPRNT        IFNE      'D'
     C     SCPRNT        ANDNE     'S'
     C     SCPRNT        ANDNE     'B'
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   ENDIF
      **** BEGIN ADD **** 02/28/01 *********************************************
      *
      * VALIDATE THE GROUP RANGE
      *
     C     SCFGRP        IFGT      SCTGRP
     C                   SETON                                        42
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFGRP IFGT SCTGRP
      *
      *  'Exclude expense constant' must be 'Y' or ' '
      *
     C     SCPRTD        IFNE      'Y'
     C     SCPRTD        ANDNE     ' '
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   ENDIF
      **** BEGIN ADD **** 10/22/02 *********************************************
      *
      *  'SORT MUST BE Y OR ' '
      *
     C     SCSRST        IFNE      'Y'
     C     SCSRST        ANDNE     ' '
     C                   MOVE      *ON           *IN29
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  TYPE OF OUTPUT MUST BE P,F,OR B EXCEPT AFTER C
      *
     C     SCTYPO        IFNE      'P'
     C     SCTYPO        ANDNE     'F'
     C     SCTYPO        ANDNE     'B'
      *** BEG ADD ***** 12/02/09 ***********************************************
     C     XXCALL        ANDNE     'C'
      *** END ADD ***** 12/02/09 ***********************************************
     C                   MOVE      *ON           *IN30
     C                   GOTO      STRT
     C                   ENDIF
      **** END   ADD **** 10/22/02 *********************************************
      **** END   ADD **** 02/28/01 *********************************************
     C*
     C*  CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      XXCO#         QQCO#             3
      **** BEGIN ADD **** 10/01/01 *********************************************
     C                   Z-ADD     0             SCFND             3 0
      **** END   ADD **** 10/01/01 *********************************************
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFRDT        QQFRDT            8
     C                   MOVE      SCTODT        QQTODT            8
     C                   MOVE      XXOUTQ        QQOUTQ           10
      **** BEGIN ADD **** 02/28/01 *********************************************
     C     SCTGRP        IFEQ      0
      *
      **** BEGIN CHG **** 10/01/01 *********************************************
     C******************** MOVE SCFND     QQFGRP  3
     C******************** MOVE SCFND     QQTGRP  3
      ********************
     C******************** ELSE
      ********************
     C******************** MOVE SCFGRP    QQFGRP
     C******************** MOVE SCTGRP    QQTGRP
      ********************
     C******************** ENDIF                           SCTGRP IFEQ 0
      *
     C                   Z-ADD     0             SCFGRP
     C                   Z-ADD     999           SCTGRP
      *
     C                   ENDIF                                                  SCTGRP IFEQ 0
      *
     C                   MOVE      SCFGRP        QQFGRP            3
     C                   MOVE      SCTGRP        QQTGRP            3
      **** END   CHG **** 10/01/01 *********************************************
      **** END   ADD **** 02/28/01 *********************************************
     C*
     C*  LOAD THE VALUED AS OF DATE.  FOR CALENDAR REPORTS, DEFAULT
     C*  IT TO THE "TO" DATE.
     C*
      *** BEG CHG ***** 12/02/09 **********************************************
     C*********  SCTYPE    IFEQ 'P'
     C*********            MOVE SCVLDT    QQVLDT  8
     C*********            ELSE
     C*********            MOVE SCTODT    QQVLDT
     C*********            ENDIF
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR01'
     C     SCTYPE        IFEQ      'P'
     C                   MOVE      SCVLDT        QQVLDT            8
     C                   ELSE
     C                   MOVE      SCTODT        QQVLDT
     C                   ENDIF
     C     Q2RECF        WHENEQ    'SCR02'
     C                   MOVE      SCVLDT        QQVLDT            8
     C     SCTYPE        IFEQ      'C'
     C     SCTYPE        OREQ      'F'
     C                   MOVE      SCVLDT        QQFRDT
     C                   MOVEL     '0101'        QQFRDT
     C                   MOVE      SCVLDT        QQTODT
     C                   ENDIF
     C                   OTHER
     C                   ENDSL
      *** END CHG ***** 12/02/09 **********************************************
     C*
     C                   CALL      'W4900C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    QQVLDT
     C                   PARM                    QQOUTQ
     C                   PARM                    SCTYPE
     C                   PARM                    SCPRNT
      **** BEGIN ADD **** 02/28/01 *********************************************
     C                   PARM                    QQFGRP
     C                   PARM                    QQTGRP
     C                   PARM                    SCPRTD
      **** END   ADD **** 02/28/01 *********************************************
      **** BEGIN ADD **** 10/22/02 *********************************************
     C                   PARM                    SCSRST
     C                   PARM                    SCTYPO
      **** END   ADD **** 10/22/02 *********************************************
      **** BEG ADD ***** 04/22/09 **********************************************
     C                   PARM                    XXCALL
      **** END ADD ***** 04/22/09 **********************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
      *
** Verbiage for location of outfiles
Detail will be written to WFEUP                                      02/29/08
(summarized output to WFEUP2)                                        02/29/08
in library,                                                          02/29/08
, if selected.                                                       02/29/08
** Title for report
CALENDAR EARNED/UNEARNED PREMIUM REPORT                              02/29/08
