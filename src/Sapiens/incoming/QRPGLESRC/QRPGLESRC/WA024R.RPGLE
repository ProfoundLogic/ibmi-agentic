     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*                                                               *
     H* API - WA024R - UPDATE WCMS W/ WCMC: CLAIM MASTER INFO         *     A
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: WA024R                                            *
     H*    CREATED: 09/05/97  C:56  Proj: 87MAZ                       *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  USAGE                                                        *
     H*                                                               *
     H*  @@RTCD - Return Code                                         *
     H*  @@CO#  - Company Number                                      *
     H*  @@SUB# - WCMC Subscriber Number                              *
     H*  @@FND  - WCMC Group/Fund                                     *
     H*  @@FYR  - Fund Year                                           *
     H*  @@CASE - Case                                                *
     H*  @@DIV  - Division number                                     *
     H*  @@DEPT - Employer Department                                 *
     H*  @@ACDT - Accident Date                                       *
     H*  @@ACTM - Accident Time                                       *
     H*  @@LTFL - Lost Time Flag                                      *
     H*  @@CLAD - IMR                                                 *
     H*  @@CLSU - File Location                                       *
     H*  @@CLFA - Field Adjuster                                      *
     H*  @@MCON - Method of Contact                                   *
     H*  @@RCVD - Date Received                                       *
     H*  @@DTRP - Date Reported                                       *
     H*  @@DTRE - Date Rehab Assigned                                 *
     H*  @@DBDT - Disability Date                                     *
     H*  @@ENTD - Date Entered                                        *
     H*  @@DEDT - Date of Death                                       *
     H*  @@HIDT - Hire Date                                           *
     H*  @@MCO  - Managed Care                                        *
     H*  @@PREX - Pre-Existing Disability                             *
     H*  @@OCDS - Occupation Description                              *
     H*  @@ESTS - Employment Status                                   *
     H*  @@DYWK - # of Days over a Week                               *
     H*  @@HRDY - Hours per Day                                       *
     H*  @@WKPT - Shift                                               *
     H*  @@EMP# - Employer number                                     *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 00/00/00 C:00 Req/Proj:                                       *
     H*                                                               *
     H*                                                               *
     H* 10/21/98 C:94 PROJ: 87MFX                                     *
     H*               ADD UPDATE TO ACCIDENT DESCRIPTION 2            *
     H*                                                               *
     H* 09/22/98 C:94 PROJ: 87MFX                                     *
     H*               REPLACE CHAIN TO FMFNP WITH API                 *
     H*                                                               *
     H* 11/12/97 C:56 Proj: 87MCU                                     *
     H*               1. Remove update to WMEIP.                      *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*    60/61/63 GENERAL USAGE (CHAIN)                             *
     H*                                                               *
     H*****************************************************************
     F*
     FWMCML1    UF   E           K DISK
     F                                     INFDS(DBFDS1)
     F*** BEGIN DEL *** 11/12/97 **************************************
     F**********FMEIL1  UF  E           K        DISK
     F*** END DEL *** 11/12/97 ****************************************
     F*** BEG DEL *** 09/22/98 ****************************************
     F**********FMFNL   IF  E           K        DISK
     F*** BEG DEL *** 09/22/98 ****************************************

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D DBFDS1          DS
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*** BEGIN DEL *** 11/12/97 **************************************
     C********** EIKEY     KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXWCF#
     C**********           KFLD           XXFYR
     C**********           KFLD           XXCASE
     C*** BEGIN DEL *** 11/12/97 **************************************
     C     *ENTRY        PLIST
01   C                   PARM                    @@RTCD            1            WCMS fields
02   C                   PARM                    @@CO#             3 0          CO#
03   C                   PARM                    @@SUB#            5 0          -----------
04   C                   PARM                    @@FND             3 0          FND
05   C                   PARM                    @@FYR             3 0          FYR
06   C                   PARM                    @@CASE            7 0          CASE
07   C                   PARM                    @@EMP#            9 0          EMP#
08   C                   PARM                    @@DIV             5 0          DIV
09   C                   PARM                    @@ACDT            7 0          ACDT
10   C                   PARM                    @@ACDS           60            ACDS
     C*** BEG CHG *** 10/21/98 ***************************************
11   C                   PARM                    @@ACD2           60            ACD2
     C*** BEGIN DEL *** 11/12/97 **************************************
     C**********           PARM           @@ACD2 60
     C*** END DEL *** 11/12/97 ****************************************
     C*** END CHG *** 10/21/98 ***************************************
11   C                   PARM                    @@ACTM            6 0          ACTM
12   C                   PARM                    @@LTFL            1            LTFL
13   C                   PARM                    @@CLAD            3            CLEX
14   C                   PARM                    @@CLSU            3            SUPR
15   C                   PARM                    @@CLFA            3            FEAD
16   C                   PARM                    @@MCON            1            MCON
17   C                   PARM                    @@RCVD            7 0          RCVD
18   C                   PARM                    @@DTRP            7 0          DTRP
19   C                   PARM                    @@DTRE            7 0          DTRE
20   C                   PARM                    @@DBDT            7 0          DBDT
21   C                   PARM                    @@ENTD            7 0          ENTD
22   C                   PARM                    @@DEDT            7 0          DOD
23   C                   PARM                    @@MCO             1            MCO
24   C                   PARM                    @@PREX            1            PREX
25   C                   PARM                    @@ODIS            1            ODIS
     C*
26   C                   PARM                    @@CCL#           25            CMMC#
27   C                   PARM                    @@NATR            2            NATR
28   C                   PARM                    @@PART            2            PART
29   C                   PARM                    @@CAUS            2            CAUS
     C*
30   C                   PARM                    @@HIDT            7 0          HIDT
31   C                   PARM                    @@OCDS           40            OCDS
32   C                   PARM                    @@ESTS            2            ESTS
33   C                   PARM                    @@DYWK            5 0          DAYS
34   C                   PARM                    @@HRDY            3 0          HRSD
35   C                   PARM                    @@WKPT            1            SHFT
36   C                   PARM                    @@DEPT           10            DEPT
37   C                   PARM                    @@HID2            7 0          HIDT
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
     C*  CLAIM MASTER KEY LIST
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*  FUND MASTER KEY LIST
     C*
     C*** BEG DEL *** 09/22/98 ***************************************
     C********** FNKY      KLIST
     C**********           KFLD           KKCO#
     C**********           KFLD           KKSUB#
     C**********           KFLD           KKFND
     C*** END DEL *** 09/22/98 ***************************************
     C*
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C*** BEG DEL *** 09/22/98 ***************************************
     C********** *LIKE     DEFN FNSUB#    KKSUB#           DEL 09/22/98
     C*** END DEL *** 09/22/98 ***************************************
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C*** BEG DEL *** 09/22/98 ***************************************
     C********** *LIKE     DEFN FNWCF#    KKWCF#
     C*** END DEL *** 09/22/98 ***************************************
     C*
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@SUB#        KKSUB#            5 0
     C                   Z-ADD     @@FND         KKFND
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C*** BEG DEL *** 09/22/98 ***************************************
     C**********
     C********** FNKY      CHAINPFN01                99
     C**********
     C********** *IN99     IFEQ '0'
     C**********
     C**********           Z-ADDFNWCF#    KKWCF#
     C**********
     C**********           ENDIF
     C*** END DEL *** 09/22/98 ***************************************
     C*** BEG ADD *** 09/22/98 ***************************************
     C*
     C                   MOVE      KKCO#         ##CO#
     C                   MOVE      KKSUB#        ##SUB#
     C                   MOVE      KKFND         ##FND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    ##CO#             3
     C                   PARM      *BLANKS       ##WCF#            3
     C                   PARM                    ##SUB#            5
     C                   PARM                    ##FND             3
     C*
     C                   MOVE      ##WCF#        KKWCF#            3 0
     C*
     C*** END ADD *** 09/22/98 ***************************************
     C*
     C*** GET CLAIM MASTER AND UPDATE
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C*** MOVE FIELDS FOR UPDATE
     C*
     C     *IN99         IFEQ      '0'
     C*
     C                   SELECT
     C*
     C* WMCMP fields used in F0260F/R - SCR01, SCR02 and SCR03.
     C*
     C*** BEG CHG *** 10/21/98 ***************************************
     C     Q1PRMS        WHENEQ    30
     C********** Q1PRMS    WHEQ 29
     C*** END CHG *** 10/21/98 ***************************************
     C*
     C                   Z-ADD     @@EMP#        CMEMP#
     C                   Z-ADD     @@DIV         CMDIV
     C                   Z-ADD     @@ACDT        CMACDT
     C                   MOVEL(P)  @@ACDS        CMACDS
     C*** BEG ADD *** 10/21/98 ****************************************
     C                   MOVEL     @@ACD2        CMACD2
     C*** END ADD *** 10/21/98 ****************************************
     C                   Z-ADD     @@ACTM        CMACTM
     C                   MOVEL(P)  @@LTFL        CMLTFL
     C                   MOVEL(P)  @@CLAD        CMCLEX
     C                   MOVEL(P)  @@CLSU        CMSUPR
     C                   MOVEL(P)  @@CLFA        CMFEAD
     C                   MOVEL(P)  @@MCON        CMMCON
     C                   Z-ADD     @@RCVD        CMRCVD
     C                   Z-ADD     @@DTRP        CMDTRP
     C                   Z-ADD     @@DTRE        CMDTRE
     C                   Z-ADD     @@DBDT        CMDBDT
     C                   Z-ADD     @@ENTD        CMENTD
     C                   Z-ADD     @@DEDT        CMDOD
     C                   MOVEL     @@MCO         CMMCO
     C                   MOVEL     @@PREX        CMPREX
     C                   MOVEL     @@ODIS        CMODIS
     C                   MOVEL     @@CCL#        CMMC#
     C                   MOVEL     @@NATR        CMNATR
     C                   MOVEL     @@PART        CMPART
     C                   MOVEL     @@CAUS        CMCAUS
     C*
     C* WMCMP fields used in F0263F/R - SCR01.
     C*
     C*** BEG CHG *** 10/21/98 ***************************************
     C     Q1PRMS        WHENEQ    36
     C********** Q1PRMS    WHEQ 35
     C*** END CHG *** 10/21/98 ***************************************
     C*
     C                   Z-ADD     @@HIDT        CMHIDT
     C                   MOVEL     @@OCDS        CMOCDS
     C                   MOVEL     @@ESTS        CMESTS
     C                   Z-ADD     @@DYWK        CMDAYS
     C                   Z-ADD     @@HRDY        CMHRSD
     C                   MOVEL     @@WKPT        CMSHFT
     C*
     C* WMCMP field in F0177FOH/ROH - SCR01 and SCR02.
     C*
     C*** BEG ADD *** 10/21/98 ***************************************
     C     Q1PRMS        WHENEQ    38
     C********** Q1PRMS    WHEQ 37
     C*** END ADD *** 10/21/98 ***************************************
     C*
     C                   MOVEL     @@DEDT        CMDOD
     C                   MOVEL     @@DEPT        CMDEPT
     C                   Z-ADD     @@HID2        CMHIDT
     C*
     C                   ENDSL
     C*
     C* USER/PGM/TIME/DT fields,
     C*
     C                   MOVE(P)   Q1USER        CMUSER
     C                   MOVE(P)   Q1PGM         CMPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*
     C                   UPDATE    PCM01
     C*
     C                   ELSE
     C*
     C                   MOVEL     'F'           @@RTCD
     C*
     C                   ENDIF                                                  *IN99 IFEQ 0
     C*** BEGIN DEL *** 11/12/97 **************************************
     C** UPDATE EDI CLAIM DATA
     C**********
     C********** EIKEY     CHAINPEI01                63
     C********** *IN63     IFEQ '0'
     C**********           MOVEL@@ACD2    EIACD2    P
     C**********           MOVE Q1USER    EIUSER    P
     C**********           MOVE Q1PGM     EIPGM     P
     C**********           TIME           DSDTTM
     C**********           Z-ADDDSTIME    EITIME
     C********** DSDATE    MULT 10000.01  EIDT
     C**********           UPDATPEI01
     C**********           ENDIF                           *IN63 IFEQ 0
     C*** END DEL *** 11/12/97 ****************************************
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
