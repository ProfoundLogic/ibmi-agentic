     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.                **
     H* PROGRAM:  F0534R                                             **
     H* DESCRIPTION:  PROVIDER/MODIFIER SEARCH W/ NAME/ADDRESS       **
     H*****************************************************************
     H*  REVISIONS:                                                  **
     H*                                                              **
     H*  11/02/09 C:232 PROJECT: 139WMR                              **
     H*                 OFAC Reporting-Add Pending Status Indicator. **
     H*                                                              **
     H*  05/26/99 C:56  Support: MC095 #18173                        **
     H*                 Switch around usage of PDDBA1 flag so that   **
     H*                 the default display will be DBA 'N' if blank **
     H*                                                              **
     H*  04/01/99 C:56  Support: MC025 #17467                        **
     H*                 1. Do not allow load of inactive providers   **
     H*                                                              **
     H*  03/23/99 C:56  Project: MC152                               **
     H*                 Load the name and mailing address base on    **
     H*                 DBA flag (FMPDP/PDDBA1)                      **
     H*                                                              **
     H*  08/10/98 C:94  PROJECT: MC145                               **
     H*                 ADDED CONDITIONING IF PROVIDER IS PARTICIPATE**
     H*                 ING OR NOT PARTICIPATING                     **
     H*                                                              **
     H*  06/15/98 C:94  Project: MC145                               **
     H*                 ADDED NETWORK SEARCH                         **
     H*                                                              **
     H*  02/18/98 C:62  Project: MC159                               **
     H*                 1) BACKED OUT PROJECT 87MBE (02/06/97) EXCPT **
     H*                    CHANGE 3.                                 **
     H*                 2) CHANGED NMPHL & NMHSL WITH FMS4L2/L3.     **
     H*                                                              **
     H*  02/04/98 C:56  Project: MC087 seq# 4491                     **
     H*                 Correct how the physical addres GEOZIP is    **
     H*                 determined.                                  **
     H*                                                              **
     H*  12/02/97 C:85 PROJECT: MC145                                **
     H*                DELETED REFERENCES TO ANTHEM MOD.             **
     H*                REARRANGED REVISIONS SECTION.                 **
     H*                                                              **
     H*  11/18/97 C:05  REQ/PROJ:                                    **
     H*                                                              **
     H*  02/06/97 C:56  PROJECT: 87MBE                               **
     H*              1. ADD EMPLOYER AND NETWORK ASSIGNMENT TO SCR01 **
     H*              2. BASE PROVIDER'S CONTRACT COMPARISON ON       **
     H*                 NETWORK ASSIGNMENT CODE                      **
     H*              3. ADD COLUMN FOR 'ANTHEM MOD'                  **
     H*              4. REMOVE NETWORK CODE SEARCH (F6)              **
     H*                                                              **
     H*  07/13/95 C:20  REQ/PROJ: MCMNT                              **
     H*                 1. MODIFICATION TO ALLOW A PROVIDER TO BE    **
     H*                    DISPLAYED AS A NON-PAR PROVIDER IF THAT   **
     H*                    PROVIDER IS PARTICIPATING IN NETWORKS THAT**
     H*                    ARE NOT SUPPORTED BY THE GROUP.           **
     H*                                                              **
     H*  06/14/95 C:32  REQ/PROJ: MC120                              **
     H*                 1. CHG RETRIEVAL OF PHYSICAL ADDR (FROM FMP1L)*
     H*                 2. ADD PHYSICAL NAME TO DISPLAY              **
     H*                 3. CHG DISPLAY OF PROV INFO FROM 3 LINES TO 4**
     H*                 3. CHG TO NOT DUPLICATE PROV#/MOD ON SCROLL  **
     H*                                                              **
     H*  03/20/95 C:20  REQ/PROJ: FSMNT                              **
     H*                 1. IF NETWORK INSTALLED AND PHYSICAL ADDRESS **
     H*                    USE THE CORRECT FIELDS FROM THE NMHSP     **
     H*                    FILE(BILLING FIELDS)                      **
     H*                                                              **
     H*  01/05/95 C:20  REQ/PROJ: FSMNT                              **
     H*                 1. IF NETWORK INSTALLED AND PHYSICAL ADDRESS **
     H*                    IS NOT PRESENT LOAD BLANKS IN ITS PLACE.  **
     H*                                                              **
     H*  10/21/94 C:20  REQ/PROJ: 146/NW005                          **
     H*                 1. ADD DISPLAY OF PHYSICAL AND MAILING       **
     H*                    ADDRESS                                   **
     H*                                                              **
     H*  04/22/94 C:20  REQ/PROJ: MCMNT                              **
     H*                 1. IF NETWORK IS NOT INSTALLED BLANK OUT     **
     H*                    NETWORK CODE                              **
     H*                                                              **
     H*  03/21/94 C:20  REQ/PROJ: 98/NW001                           **
     H*                 1. ADD DISPLAY OF PPO NETWORK CODE           **
     H*                                                              **
     H*  05/18/93 C:20  REQ/PROJ: 87/MC087                           **
     H*                 1. ADD GEO ZIP AREA TO ADDRESS LINE          **
     H*                 2. ADD COUNTY CODE TO ADDRESS LINE           **
     H*                 3. ADD SPLTY RATING TO SCRREN                **
     H*                                                              **
     H*  03/12/93 C:10  REQ/PROJ: 83/FS103                           **
     H*                 1. Moved Provider ID to heading, removed     **
     H*                    column.                                   **
     H*                 2. Added Provider Type and Adj State         **
     H*                 3. Moved address to 2nd line and displayed   **
     H*                    more of it.                               **
     H*                 4. Allowed user to choose and item by        **
     H*                    placing the cursor on either line of it.  **
     H*                                                              **
     H* 01/06/93  C:20- ON ROLL UP AND DOWN CHGD TO USE CORRECT KEY  **
     H*                 LIST PDKEY1 INSTEAD OF PDKEY                 **
     H*                                                              **
     H* 07/22/92  KRR - ADDED SUB# TO ALL PROCESSING                 **
     H*                                                              **
     H* 04/16/91  DVM - ADDED CODE TO POSITION CURSOR AT THE PROVIDER**
     H*                 AND MODIFIER PASSED TO THE PROVIDER/MOD      **
     H*                 SEARCH (F0534R) WHICH IS CALLED WHEN MORE    **
     H*                 THAN 1 (ONE) MODIFIER EXIST FOR A PROVIDER.  **
     H*                                                              **
     H* 12/05/90  LJD - ADDED SPECIALTY CODE TO DISPLAY ON SCREEN    **
     H*               - REDUCED SIZE OF NAME/ADDRESS FIELD FROM 60   **
     H*                                                              **
     H*  9/15/89  LJD - ADDED PROVIDER TYPE, BECAUSE IT WAS A PARM   **
     H*  PROJECT: A9C    BUT WASN'T BEING LOADED WITH ANYTHING       **
     H*                  NOTE: IT IS NOT A SCREEN FIELD              **
     H*                                                              **
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H** INDICATORS:                                                 **
     H*****************************************************************
     FF0534F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FFMPDL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMP1L     IF   E           K DISK
     FFMGZL     IF   E           K DISK
     F*** BEGIN CHG *** 02/18/98 **************************************
     F********** NMPHL   IF  E           K        DISK
     F********** NMHSL   IF  E           K        DISK
     FFMS4L2    IF   E           K DISK
     FFMS4L3    IF   E           K DISK
     F                                     RENAME(PS401:PS402)
     F*** END   CHG *** 02/18/98 **************************************
     FSSYLF     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     F*** BEGIN DEL *** 02/18/98 **************************************
     F********** FMCML1  IF  E           K        DISK
     F********** FMEML   IF  E           K        DISK
     F*** END   DEL *** 02/18/98 **************************************

     D FL              S              1    DIM(3)                                 POSITIONAL FLAG
     D*** BEGIN DEL *** 12/02/97 **************************************
     D**********          AM          3  4                 ANTHEM MOD
     D*** END   DEL *** 12/02/97 **************************************
     D PN              S              5    DIM(10)                                NETWORK CODE NETWO
     D PE              S              8  0 DIM(10)                                NETWORK CODE EFF D
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEYF            S              9  0 DIM(2)
     D KEYM            S              3  0 DIM(2)
     D*
     D* ARRAYS USED FOR COMBINE SUBROUTINE
     D*
     D*
     D* ARRAY USED FOR COMBINING PROVIDER AND MODIFIER FOR LOOK UP
     D*  OPERATION.
     D PRM             S             12  0 DIM(14)
     D*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  FE                     1     27  0
     D                                     DIM(3)                                 PROVIDER FEIN
     D  SCFE01                 1      9  0
     D  SCFE02                10     18  0
     D  SCFE03                19     27  0
     D                 DS
     D  MO                     1      9  0
     D                                     DIM(3)                                 PROVIDER MODIFIER
     D  SCMO01                 1      3  0
     D  SCMO02                 4      6  0
     D  SCMO03                 7      9  0
     D                 DS
     D  DE                     1    195
     D                                     DIM(3)                                 MAILING ADDRESS
     D  SCDE01                 1     65
     D  SCDE02                66    130
     D  SCDE03               131    195
     D                 DS
     D  AD                     1    195
     D                                     DIM(3)                                 PHYSICAL ADDRESS
     D  SCAD01                 1     65
     D  SCAD02                66    130
     D  SCAD03               131    195
     D                 DS
     D  SP                     1      9
     D                                     DIM(3)                                 SPECIALTY CODE
     D  SCSP01                 1      3
     D  SCSP02                 4      6
     D  SCSP03                 7      9
     D                 DS
     D  PT                     1      3
     D                                     DIM(3)                                 PROVIDER TYPE
     D  SCPT01                 1      1
     D  SCPT02                 2      2
     D  SCPT03                 3      3
     D                 DS
     D  AS                     1      6
     D                                     DIM(3)                                 ADJ STATE   E
     D  SCAS01                 1      2
     D  SCAS02                 3      4
     D  SCAS03                 5      6
     D*** BEGIN DEL *** 12/02/97 **************************************
     D**********  DS
     D**********                              1  12 AM
     D**********                              1   4 SCAM01
     D**********                              5   8 SCAM02
     D**********                              9  12 SCAM03
     D*
     D*** END   DEL *** 12/02/97 **************************************
     D                 DS
     D  NM                     1    120
     D                                     DIM(3)                                 NAME        E
     D  SCNM01                 1     40
     D  SCNM02                41     80
     D  SCNM03                81    120
     D                 DS
     D  SR                     1      3
     D                                     DIM(3)                                 SPLTY RATING
     D  SCSR01                 1      1
     D  SCSR02                 2      2
     D  SCSR03                 3      3
     D                 DS
     D  NW                     1     15
     D                                     DIM(3)                                 NETWORK CODE
     D  SCNW01                 1      5
     D  SCNW02                 6     10
     D  SCNW03                11     15
     D                 DS
     D  EF                     1    024  0
     D                                     DIM(3)                                 NETWORK EF DT
     D  SCEF01               001    008  0
     D  SCEF02               009    016  0
     D  SCEF03               017    024  0
     D                 DS
     D  PM                     1    120
     D                                     DIM(3)                                 PHYSICAL NAME
     D  SCPM01                 1     40
     D  SCPM02                41     80
     D  SCPM03                81    120
     D                 DS
     D  GN                     1     50
     D                                     DIM(10)                                NETWORK CODE GROUP
     D  SCGN01                 1      5
     D  SCGN02                 6     10
     D  SCGN03                11     15
     D  SCGN04                16     20
     D  SCGN05                21     25
     D  SCGN06                26     30
     D  SCGN07                31     35
     D  SCGN08                36     40
     D  SCGN09                41     45
     D  SCGN10                46     50
     D*
     D* DATA STRUCTURES FOR COMBINE SUBROUTINE
     D*
     D                 DS
     D  SNAME                  1     65
     D  SN                     1     65
     D                                     DIM(65)
     D                 DS
     D  SADDR                  1     40
     D  SA                     1     40
     D                                     DIM(40)
     D                 DS
     D  SCITY                  1     25
     D  SC                     1     25
     D                                     DIM(25)                              ADD 03/15/93
     D                 DS
     D  SSTATE                 1      2
     D  ST                     1      2
     D                                     DIM(2)                               ADD 03/15/93
     D                 DS
     D  SZIP1                  1      5
     D  SZ1                    1      5
     D                                     DIM(5)                               ADD 03/15/93
     D                 DS
     D  SZIP2                  1      4
     D  SZ2                    1      4
     D                                     DIM(4)                               ADD 03/15/93
     D                 DS
     D  SGEOZ                  1      3
     D  SZ                     1      3
     D                                     DIM(3)                               ADD 05/18/93
     D                 DS
     D  SCNTY                  1     15
     D  SY                     1     15
     D                                     DIM(15)                              ADD 05/18/93
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN            9 0
     C*
     C     PDKEY1        KLIST
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C*** BEGIN ADD *** 07/22/92 **************************************
     C*
     C*   KEY LIST FOR THE SUBSCRIBER
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C*** END ADD *** 07/22/92 ****************************************
     C*** BEGIN ADD *** 03/15/93 **************************************
     C     P1KY          KLIST
     C                   KFLD                    PDFEIN
     C                   KFLD                    PDMOD
     C*** END   ADD *** 03/15/93 **************************************
     C*** BEGIN ADD *** 05/18/93 *************************************
     C*** Geographical Zip Area FILE
     C     GZKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFRZP
     C*** END ADD ***** 05/18/93 *************************************
     C*** BEGIN DEL *** 02/18/98 **************************************
     C*** BEGIN ADD *** 03/21/94 **************************************
     C*
     C*   KEY LIST FOR THE PHYSICIAN PPO NETWORK FILE
     C*
     C********** PHKEY     KLIST
     C**********           KFLD           FE,I1
     C**********           KFLD           MO,I1
     C*
     C********** PHKY1     KLIST
     C**********           KFLD           FE,I1
     C**********           KFLD           MO,I1
     C**********           KFLD           NW,I1
     C*
     C********** PHKY2     KLIST
     C**********           KFLD           PDFEIN
     C**********           KFLD           PDMOD
     C*
     C*   KEY LIST FOR THE HOSPITAL PPO NETWORK FILE
     C*
     C********** HSKEY     KLIST
     C**********           KFLD           FE,I1
     C**********           KFLD           MO,I1
     C*
     C********** HSKY1     KLIST
     C**********           KFLD           FE,I1
     C**********           KFLD           MO,I1
     C**********           KFLD           NW,I1
     C*
     C********** HSKY2     KLIST
     C**********           KFLD           PDFEIN
     C**********           KFLD           PDMOD
     C*** END   DEL *** 02/18/98 **************************************
     C*** BEGIN ADD *** 02/18/98 **************************************
     C*
     C*   KEY LIST PROVIDER NETWORK CONTRACT FILE
     C*
     C     S4KEY         KLIST
     C                   KFLD                    FE(I1)
     C                   KFLD                    MO(I1)
     C*
     C     S4KY1         KLIST
     C                   KFLD                    FE(I1)
     C                   KFLD                    MO(I1)
     C                   KFLD                    NW(I1)
     C*
     C     S4KY2         KLIST
     C                   KFLD                    PDFEIN
     C                   KFLD                    PDMOD
     C*
     C*** END   DEL *** 02/18/98 **************************************
     C*
     C*  FS BASE CONTROL FILE
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
     C*
     C*   KEY LIST FOR THE FUND AUXILIARY RECORD
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C*** END ADD *** 03/21/94 ****************************************
     C*
     C*** BEGIN DEL *** 02/18/98 **************************************
     C*** BEGIN ADD *** 02/06/97 **************************************
     C*
     C*   CLAIM MASTER KEY LIST
     C*
     C********** CMKEY     KLIST
     C**********           KFLD           UPCO#
     C**********           KFLD           UPSUB#
     C**********           KFLD           QQFND
     C**********           KFLD           QQFYR
     C**********           KFLD           QQCASE
     C*
     C*   EMPLOYER MASTER KEY LIST
     C*
     C********** EMKEY     KLIST
     C**********           KFLD           UPCO#
     C**********           KFLD           UPSUB#
     C**********           KFLD           QQFND
     C**********           KFLD           SCEMP#
     C**********           KFLD           SCDIV
     C*
     C*** END ADD *** 02/06/97 ****************************************
     C*** END   DEL *** 02/18/98 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFEIN            9 0
     C                   PARM                    QQMOD             3 0
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQNWCD            5
     C                   PARM                    QQEFDT            8
     C*** BEGIN DEL *** 02/18/98 **************************************
     C*** BEGIN ADD *** 02/06/97 **************************************
     C**********           PARM           QQFND   30
     C**********           PARM           QQFYR   30
     C**********           PARM           QQCASE  70
     C*** END ADD *** 02/06/97 ****************************************
     C*** END   DEL *** 02/18/98 **************************************
     C                   PARM                    QQRTCD            1
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
     C     *LIKE         DEFINE    GZFRZP        XXFRZP                         05/18/93 ADD
     C     *LIKE         DEFINE    GZZIPA        XXZIPA                         05/18/93 ADD
     C*
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   Z-ADD     0             XXMOD             3 0
     C*** BEGIN ADD *** 04/16/91 ************* DVM *******************
     C                   MOVEL     QQFEIN        XXPRM            12 0
     C                   MOVE      QQMOD         XXPRM
     C***** END ADD *** 04/16/91 ************* DVM *******************
     C*** BEGIN ADD *** 03/21/94 *************************************
     C                   MOVEL     'FB'          XSKEY
     C     FBKY          CHAIN     SFB01                              60
     C     *IN60         IFEQ      '0'
     C     FBFL06        IFEQ      'Y'
     C                   MOVE      '1'           *IN74
     C                   ELSE
     C                   MOVE      '0'           *IN74
     C                   END
     C                   END
     C*** END ADD *** 03/21/94 ***************************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C     FNKEY         CHAIN     PFN01                              60
     C*** BEGIN ADD *** 07/22/92 **************************************
     C     SMKEY         CHAIN     PSM01                              60
     C*** END ADD *** 07/22/92 ****************************************
     C*** BEGIN ADD *** 03/22/94 ***********************************************
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     F1FND         SCFND
     C                   MOVEL     F1NW01        SCGN01
     C                   MOVEL     F1NW02        SCGN02
     C                   MOVEL     F1NW03        SCGN03
     C                   MOVEL     F1NW04        SCGN04
     C                   MOVEL     F1NW05        SCGN05
     C                   MOVEL     F1NW06        SCGN06
     C                   MOVEL     F1NW07        SCGN07
     C                   MOVEL     F1NW08        SCGN08
     C                   MOVEL     F1NW09        SCGN09
     C                   MOVEL     F1NW10        SCGN10
     C                   ELSE
     C                   Z-ADD     *ZEROS        SCFND
     C                   MOVEL     *BLANKS       SCGN01
     C                   MOVEL     *BLANKS       SCGN02
     C                   MOVEL     *BLANKS       SCGN03
     C                   MOVEL     *BLANKS       SCGN04
     C                   MOVEL     *BLANKS       SCGN05
     C                   MOVEL     *BLANKS       SCGN06
     C                   MOVEL     *BLANKS       SCGN07
     C                   MOVEL     *BLANKS       SCGN08
     C                   MOVEL     *BLANKS       SCGN09
     C                   MOVEL     *BLANKS       SCGN10
     C                   END
     C*** END ADD *** 03/22/94 *************************************************
     C*
     C*** BEGIN DEL *** 02/18/98 **************************************
     C*** BEGIN ADD *** 02/06/97 **************************************
     C*
     C*   GET EMPLOYER & DIVISION FROM CLAIM MASTER
     C*
     C********** CMKEY     CHAINPCM01                60
     C********** *IN60     IFEQ '0'
     C**********           MOVE CMEMP#    SCEMP#
     C**********           Z-ADDCMDIV     SCDIV
     C**********           ENDIF                           *IN60 IFEQ
     C*
     C*   GET EMPLOYER NETWORK CODE
     C*
     C**********           MOVE *BLANKS   SCNWCD
     C********** EMKEY     CHAINPEM01                61
     C********** *IN61     IFEQ '0'
     C**********           MOVELEMNWCD    SCNWCD
     C**** ***** SCNWCD    IFEQ *BLANKS                    DEL 11/12/97
     C**** *****           MOVE '1'       *IN99            DEL 11/12/97
     C**** *****           ELSE                            DEL 11/12/97
     C**********           MOVE '0'       *IN99
     C**** *****           ENDIF                           DEL 11/12/97
     C*
     C**********           ENDIF                           *IN61 IFEQ
     C*
     C*** END ADD *** 02/06/97 ****************************************
     C*** END   DEL *** 02/18/98 **************************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C                   Z-ADD     QQFEIN        XXFEIN
     C                   Z-ADD     *ZERO         SCMOD
     C                   GOTO      REPOS
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     XXUP          IFEQ      '1'
     C                   MOVE      *BLANKS       SCUP
     C                   ELSE
     C                   MOVEL     RR(1)         SCUP
     C                   END
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
     C                   END
     C*** BEGIN ADD *** 03/22/94 ***********************************************
     C                   MOVEA     '000000'      *IN(43)
     C**********           DO   6         A       50       DEL 08/22/94
     C*** BEGIN CHG *** 06/14/95 ***********************************************
     C**********           DO   4         A       50       ADD 08/22/94
     C                   DO        3             A                 5 0
     C*** END   CHG *** 06/14/95 ***********************************************
     C     A             ADD       42            A1                2 0
     C     FE(A)         IFEQ      *ZEROS
     C     NW(A)         OREQ      *BLANKS
     C                   MOVE      '1'           *IN(A1)
     C                   END                                                    END FE,A EQ *ZEROS
     C                   END                                                    END DO
     C*** END ADD *** 03/22/94 *************************************************
     C*** BEG ADD *** 06/16/98 ****************************************
     C     DSPL          TAG
     C                   EXFMT     SCR01
     C*** END ADD *** 06/16/98 ****************************************
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   Z-ADD     *ZERO         QQFEIN
     C                   Z-ADD     *ZERO         QQMOD
     C*** BEGIN *** 03/21/94 ***********************************************
     C                   MOVE      *BLANKS       QQNWCD
     C                   MOVE      *BLANKS       QQEFDT
     C*** END ADD *** 03/21/94 *********************************************
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*** BEGIN ADD *** 06/15/98 ***********************************************
     C***************************************
     C*   COMMAND F10 = NWCD SEARCH         *
     C***************************************
     C     *INKJ         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C     HFLD          IFEQ      'SCNW01'
     C     HFLD          OREQ      'SCNW02'
     C     HFLD          OREQ      'SCNW03'
     C     HFLD          IFEQ      'SCNW01'
     C                   Z-ADD     1             A                 5 0
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCNW02'
     C                   Z-ADD     2             A
     C                   ENDIF
     C     HFLD          IFEQ      'SCNW03'
     C                   Z-ADD     3             A
     C                   ENDIF
     C                   MOVE      FE(A)         Q1FEIN            9
     C                   MOVE      MO(A)         Q1MOD             3
     C                   MOVEL     PT(A)         QQTYPE
     C                   MOVE      *BLANKS       QQNWCD            5
     C                   CALL      'F0534R1'
     C                   PARM                    QQPRDS
     C                   PARM                    Q1FEIN
     C                   PARM                    Q1MOD
     C                   PARM                    QQTYPE
     C                   PARM                    QQNWCD
     C                   PARM                    QQRTCD
     C     QQNWCD        IFNE      *BLANKS
     C                   MOVEL     QQNWCD        NW(A)
     C                   ENDIF
     C                   GOTO      DSPL
     C*
     C                   ENDIF                                                  END HFLD
     C                   ENDIF                                                  END IF
     C*** END ADD *** 06/15/98 *************************************************
     C*
     C*   COMMAND 12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR01T
     C*
     C*   ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR01T
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*
     C*  INVALID ROLL REQUEST
     C*
     C   18XXUP          IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C   19XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   Z-ADD     KEYF(2)       XXFEIN
     C                   Z-ADD     KEYM(2)       XXMOD
     C********** PDKEY     CHAINPPD01                60    DEL 01/06/93
     C     PDKEY1        CHAIN     PPD01                              60        ADD 01/06/93
     C  N60PDKEY         READE     PPD01                                  60    ADD 06/14/95
     C                   MOVE      '1'           *IN50                          ADD 06/14/95
     C  N60P1KY          CHAIN     PP101                              50        ADD 03/15/93
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYF(1)       XXFEIN
     C                   Z-ADD     KEYM(1)       XXMOD
     C********** PDKEY     CHAINPPD01                60    DEL 01/06/93
     C     PDKEY1        CHAIN     PPD01                              60        ADD 01/06/93
     C  N60              READP     PPD01                                  60    ADD 06/14/95
     C                   MOVE      '1'           *IN50                          ADD 06/14/95
     C  N60P1KY          CHAIN     PP101                              50        ADD 03/15/93
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCMOD         IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEYF
     C                   MOVE      *BLANK        KEYM
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C                   Z-ADD     SCMOD         XXMOD
     C                   MOVE      *BLANK        SCMOD
     C     PDKEY1        SETLL     PPD01
     C     PDKEY         READE     PPD01                                  60
     C                   MOVE      '1'           *IN50                          ADD 06/14/95
     C  N60P1KY          CHAIN     PP101                              50        ADD 03/15/93
     C                   EXSR      LOAD
     C*** BEGIN ADD *** 04/16/91 ************* DVM *******************
     C                   Z-ADD     0             X                 3 0
     C                   Z-ADD     1             Z                 3 0
     C     XXPRM         LOOKUP    PRM(Z)                                 98
     C     *IN98         IFEQ      '1'
     C     Z             ADD       29            X
     C                   MOVE      '1'           *IN(X)
     C                   END
     C***** END ADD *** 04/16/91 ************* DVM *******************
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     *INKF         IFNE      '1'
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C*** BEGIN CHG *** 03/15/93 **************************************
     C*** BEGIN CHG *** 06/14/95 **************************************
     C*********  LIN       SUB  7         I1      30
     C*********  LIN       SUB  9         I1      30
     C     LIN           SUB       10            I1                3 0
     C*********            ADD  1         I1
     C*********            ADD  1         I1
     C                   ADD       3             I1
     C*********            DIV  2         I1
     C*********            DIV  3         I1
     C                   DIV       4             I1
     C*** END   CHG *** 06/14/95 **************************************
     C*** END   CHG *** 03/15/93 **************************************
     C*
     C*  VALID CURSOR POSITION - RETURN WITH SELECTION
     C*
     C     SELRCD        TAG
     C     I1            IFGE      1
     C*** BEGIN CHG *** 03/15/93 **************************************
     C*** BEGIN CHG *** 08/22/93 **************************************
     C*** BEGIN CHG *** 06/14/95 **************************************
     C*********  I1        ANDLE14
     C*********  I1        ANDLE6
     C*********  I1        ANDLE4
     C     I1            ANDLE     3
     C*** END   CHG *** 06/14/95 **************************************
     C*** END   CHG *** 08/24/93 **************************************
 0   C*** END   CHG *** 03/15/93 **************************************
     C*** BEGIN ADD *** 03/22/94 ***********************************************
     C*
     C* BEFORE RETURNING WE MUST VALIDATE NWCD CODE FOR PROV AND GROUP
     C*
     C     NW(I1)        IFNE      *BLANKS
     C     I1            ADD       36            A2                2 0
     C                   Z-ADD     1             Z
     C*** BEGIN CHG *** 02/18/98 **************************************
     C*** BEGIN DEL *** 02/06/97 **************************************
     C     NW(I1)        LOOKUP    GN(Z)                                  70
     C     *IN70         IFEQ      '0'
     C*** END DEL *** 02/06/97 ****************************************
     C*** END   CHG *** 02/18/98 **************************************
     C*
     C*** BEGIN DEL *** 02/18/98 **************************************
     C*** BEGIN CHG *** 11/18/97 **************************************
     C*** BEGIN ADD *** 02/06/97 **************************************
     C* * * **   NW,I1     IFEQ SCNWCD
     C********   NW,I1     IFNE SCNWCD
     C*** END ADD *** 02/06/97 ****************************************
     C*** END CHG *** 11/18/97 **************************************
     C*** END   DEL *** 02/18/98 **************************************
     C*
     C                   MOVE      '1'           *IN36
     C                   MOVE      '1'           *IN(A2)
     C                   GOTO      STRT
     C                   END
     C*
     C     PT(I1)        IFEQ      '1'
     C*** BEGIN CHG *** 02/18/98 **************************************
     C********** PHKY1     CHAINPPH01                60
     C     S4KY1         CHAIN     PS402                              60
     C*** END   CHG *** 02/18/98 **************************************
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN36
     C                   MOVE      '1'           *IN(A2)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     PT(I1)        IFEQ      '4'
     C*** BEGIN CHG *** 02/18/98 **************************************
     C********** HSKY1     CHAINPHS01                60
     C     S4KY1         CHAIN     PS401                              60
     C*** END   CHG *** 02/18/98 **************************************
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN36
     C                   MOVE      '1'           *IN(A2)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   END
     C*** END ADD *** 03/22/94 *************************************************
     C                   MOVE      '*'           QQRTCD
     C                   Z-ADD     FE(I1)        QQFEIN
     C                   Z-ADD     MO(I1)        QQMOD
     C                   MOVE      PT(I1)        QQTYPE
     C                   MOVE      NW(I1)        QQNWCD
     C                   MOVE      EF(I1)        QQEFDT
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   Z-ADD     *ZERO         QQFEIN
     C                   Z-ADD     *ZERO         QQMOD
     C                   GOTO      TERM
     C                   ELSE                                                   ELSE *INKF EQ '1'
     C                   GOTO      STRT
     C                   END                                                    END *INKF EQ '1'
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       FE
     C                   MOVE      *BLANKS       MO
     C                   MOVE      *BLANKS       DE
     C                   MOVE      *BLANKS       AD                             ADD 08/22/94
     C                   MOVE      *BLANKS       SP                             12/05/90 ADD
     C                   MOVE      *BLANKS       PT                             9/15/89 ADD
     C                   MOVE      *BLANKS       SR
     C                   Z-ADD     *ZERO         I1
     C*** BEGIN ADD *** 03/15/93 *************************************
     C                   MOVE      *BLANKS       AS
     C                   MOVE      *BLANKS       NM
     C                   MOVE      *BLANKS       PM                             ADD 06/14/95
     C*** END   ADD *** 03/15/93 *************************************
     C*** BEGIN ADD *** 02/06/97 *************************************
     C*** BEGIN DEL *** 12/02/97 **************************************
     C**********           MOVE *BLANKS   AM
     C*** END   DEL *** 12/02/97 **************************************
     C*** END ADD *** 02/06/97 ***************************************
     C*
     C*** BEGIN ADD *** 03/21/94 *************************************
     C                   MOVE      *BLANKS       NW
     C                   Z-ADD     *ZEROS        EF
     C*** END   ADD *** 03/21/94 *************************************
     C*
     C*** BEGIN ADD *** 04/16/91 ************* DVM *******************
     C                   MOVEA     '0000000'     *IN(30)
     C                   MOVEA     '0000000'     *IN(37)
     C***** END ADD *** 04/16/91 ************* DVM *******************
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C*** BEGIN DEL *** 07/13/95 ***********************************************
     C*** BEGIN ADD *** 03/22/94 ***********************************************
     C********** *IN74     IFEQ '1'
     C*
     C* WE MUST VERIFY THAT NO NETWORK EXISTD OR THAT AT LEAST ONE NETWORK IS
     C* VALID FOR GROUP
     C*
     C********** SCGN01    IFGT *BLANKS
     C********** SCGN02    ORGT *BLANKS
     C********** SCGN03    ORGT *BLANKS
     C********** SCGN04    ORGT *BLANKS
     C********** SCGN05    ORGT *BLANKS
     C********** SCGN06    ORGT *BLANKS
     C********** SCGN07    ORGT *BLANKS
     C********** SCGN08    ORGT *BLANKS
     C********** SCGN09    ORGT *BLANKS
     C********** SCGN10    ORGT *BLANKS
     C*
     C********** PDTYPE    IFEQ '1'
     C**********           MOVE 'Y'       XFIRST  1
     C********** PHKY2     CHAINPPH01                65
     C********** *IN65     DOWEQ'0'
     C**********           MOVE *BLANKS   XFIRST
     C**********           Z-ADD1         Z
     C********** PHNWCD    LOKUPGN,Z                     70
     C********** *IN70     IFEQ '1'
     C**********           GOTO OKGRP
     C**********           END                             END *IN70
     C********** PHKY2     READEPPH01                    65
     C**********           END
     C********** XFIRST    IFEQ *BLANKS
     C**********           SUB  1         I1
     C**********           GOTO FILLE
     C**********           END                             END XFIRST
     C*
     C**********           ELSE
     C*
     C********** PDTYPE    IFEQ '4'
     C**********           MOVE 'Y'       XFIRST  1
     C********** HSKY2     CHAINPHS01                65
     C********** *IN65     DOWEQ'0'
     C**********           MOVE *BLANKS   XFIRST
     C**********           Z-ADD1         Z
     C********** HSNWCD    LOKUPGN,Z                     70
     C********** *IN70     IFEQ '1'
     C**********           GOTO OKGRP
     C**********           END                             END *IN70
     C********** HSKY2     READEPHS01                    65
     C**********           END
     C********** XFIRST    IFEQ *BLANKS
     C**********           SUB  1         I1
     C**********           GOTO FILLE
     C**********           END                             END XFIRST
     C*
     C**********           END                             END PDTYPE EQ '4'
     C**********           END                             END PDTYPE EQ '1'
     C**********           END                             END *IN74 EQ '1'
     C**********           END                             END GN GT *BLANKS
     C********** OKGRP     TAG
     C*** END ADD *** 03/22/94 *************************************************
     C*** END DEL *** 07/13/95 *************************************************
     C*
     C                   MOVE      PDFEIN        FE(I1)
     C                   MOVE      PDMOD         MO(I1)
     C*
     C*** BEG DEL *** 05/26/99 ****************************************
     C****BEG*ADD *** 03/23/99 ****************************************
     C********** PDDBA1    IFEQ 'N'
     C****END*ADD *** 03/23/99 ****************************************
     C********** PDFSN     IFEQ *BLANKS
     C**********           MOVELPDNAME    XXLAST 56
     C**********           ELSE
     C**********           MOVELPDNAME    XXLAST 56
     C**********           MOVELPDFSN     XXFRST 15
     C**********           MOVE '1'       XXTERM  1
     C**********           CALL 'SRNAME'
     C**********           PARM           XXLAST
     C**********           PARM           XXFRST
     C**********           PARM           XXTERM
     C**********           END
     C**********           MOVE *BLANKS   XXNAME 65
     C****BEGIN*CHG *** 03/15/93 **************************************
     C**********           MOVELXXLAST    XXNAME 65
     C**********           MOVE *BLANKS   XXNAM1 40
     C**********           MOVELXXLAST    XXNAM1
     C****END***CHG *** 03/15/93 **************************************
     C**********           MOVELPDADR1    XXADDR 40
     C****BEG*ADD *** 03/23/99 ****************************************
     C**********           ELSE
     C**********           MOVELPDADR1    XXNAM1    P
     C**********           MOVELPDADR2    XXADDR    P
     C**********           ENDIF                           PDDBA1 = N
     C*** END ADD *** 03/23/99 ****************************************
     C*** END DEL *** 05/26/99 ****************************************
     C*
     C*** BEG ADD *** 05/26/99 ****************************************
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        XXNAM1
     C                   MOVEL(P)  PDADR2        XXADDR
     C                   ELSE
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVEL     PDNAME        XXLAST
     C                   ELSE
     C                   MOVEL     PDNAME        XXLAST           56
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   ENDIF                                                  PDFSN = *BLANKS
     C                   MOVE      *BLANKS       XXNAME           65
     C                   MOVE      *BLANKS       XXNAM1           40
     C                   MOVEL     XXLAST        XXNAM1
     C                   MOVEL     PDADR1        XXADDR           40
     C                   ENDIF                                                  PDDBA1 = Y
     C*** END ADD *** 05/26/99 ****************************************
     C*
     C*
     C*
     C*
     C*** BEGIN CHG *** 03/15/93 **************************************
     C**************       MOVELPDCTY     XXCITY 15
     C                   MOVEL     PDCTY         XXCITY           25
     C                   MOVEL     PDST          XXST              2
     C                   MOVEL     PDZIP         XXZIP1            5
     C                   MOVE      PDZIP         XXZIP2            4
     C*** BEGIN ADD *** 05/18/93 **************************************
     C                   MOVEL     PDZIP         XXFRZP
     C     GZKY          SETGT     PGZ01
     C                   READP     PGZ01                                  60
     C     *IN60         IFEQ      '0'
     C     XXFRZP        ANDLE     GZTOZP
     C                   MOVE      GZZIPA        XXZIPA
     C                   END                                                    END 0 *IN60
     C*
     C                   MOVEL     P1CNTY        XXCNTY           15
     C                   MOVE      P1FL01        SR(I1)
     C*** END ADD *** 05/18/93 ****************************************
     C*** END   CHG *** 03/15/93 **************************************
     C                   MOVE      '1'           XXTERM            1
     C                   MOVE      *BLANKS       XXNAME                         ADD 03/23/99
     C                   EXSR      COMB
     C                   MOVEL     XXNAM1        NM(I1)                         03/15/93 ADD
     C                   MOVEL     XXNAME        DE(I1)
     C                   MOVE      PDSPCD        SP(I1)                         12/05/90 ADD
     C                   MOVE      PDTYPE        PT(I1)                         9/15/89 ADD
     C                   MOVEL     P1ST          AS(I1)                         03/15/93 ADD
     C*
     C*** BEGIN ADD *** 02/06/97 **************************************
     C*** BEGIN DEL *** 12/02/97 **************************************
     C**********           MOVE PDAMOD    AM,I1
     C*** END   DEL *** 12/02/97 **************************************
     C*** END ADD *** 02/06/97 ****************************************
     C*
     C*** BEGIN ADD **8 03/21/94 ***************************************
     C     *IN74         IFEQ      '1'
     C*** BEG ADD *** 08/10/98 ***************************************
     C     PDPARN        IFEQ      'P'
     C*** END ADD *** 08/10/98 ***************************************
8    C     PDTYPE        IFEQ      '1'
     C                   MOVE      *BLANKS       PN
     C                   MOVE      *ZEROS        PE
     C*** BEGIN CHG *** 02/18/98 **************************************
     C********** PHKEY     CHAINPPH01                60
     C     S4KEY         CHAIN     PS402                              60
     C*** END   CHG *** 02/18/98 **************************************
     C*** BEGIN ADD *** 08/22/94 ***********************************************
     C*** BEGIN DEL *** 06/14/95 **************************************
     C**********           MOVE *BLANKS   AD,I1
     C********** *IN60     IFEQ '0'
     C********** PHB1A1    ANDNE*BLANKS
     C**********           MOVELPHB1ZP    XXZIP1  5
     C**********           MOVELPHB1ZS    XXZIP2  4
     C********** PHB1A1    CAT  ',':0     AD,I1
     C********** AD,I1     CAT  PHB1CY:1  AD,I1
     C********** AD,I1     CAT  ',':0     AD,I1
     C********** AD,I1     CAT  PHB1ST:1  AD,I1
     C********** AD,I1     CAT  ',':0     AD,I1
     C********** AD,I1     CAT  XXZIP1:1  AD,I1
     C********** AD,I1     CAT  '-':0     AD,I1
     C********** AD,I1     CAT  XXZIP2:0  AD,I1
     C**********           ELSE
     C**********           MOVELDE,I1     AD,I1
     C**********           MOVE *BLANKS   AD,I1
     C**********           END
     C*** END   DEL *** 06/14/95 **************************************
     C*** END ADD *** 08/22/94 *************************************************
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     1             Z
     C*** BEGIN CHG *** 02/18/98 **************************************
     C********** PHNWCD    LOKUPGN,Z                     70
     C     S4NWCD        LOOKUP    GN(Z)                                  70
     C     *IN70         IFEQ      '1'
     C**********           MOVELPHNWCD    PN,Z
     C**********           Z-ADDPHNEDT    SRDTI
     C                   MOVEL     S4NWCD        PN(Z)
     C                   Z-ADD     S4BGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PE(Z)
     C*********            GOTO OKPRO
     C                   END                                                    END *IN70
     C********** PHKEY     READEPPH01                    60
     C     S4KEY         READE     PS402                                  60
     C*** END   CHG *** 02/18/98 **************************************
     C                   END
     C********** *IN60     IFEQ '1'
     C**********           MOVEL*BLANKS   NW,I1
     C**********           Z-ADD*ZEROS    EF,I1
     C**********           END                             END *IN60
     C********** OKPRO     TAG
     C                   MOVE      *BLANKS       NW(I1)
     C                   Z-ADD     *ZEROS        EF(I1)
     C                   DO        10            Z
     C     PN(Z)         IFNE      *BLANKS
     C                   MOVEL     PN(Z)         NW(I1)
     C                   Z-ADD     PE(Z)         EF(I1)
     C                   GOTO      OKPRO
     C                   END
     C                   END
     C     OKPRO         TAG
     C*
     C                   ELSE
     C*
     C     PDTYPE        IFEQ      '4'
     C                   MOVE      *BLANKS       PN
     C                   MOVE      *ZEROS        PE
     C*** BEGIN CHG *** 02/18/98 **************************************
     C********** HSKEY     CHAINPHS01                60
     C     S4KEY         CHAIN     PS401                              60
     C*** END   CHG *** 02/18/98 **************************************
     C*** BEGIN ADD *** 08/22/94 ***********************************************
     C*** BEGIN DEL *** 06/14/95 **************************************
     C**********           MOVE *BLANKS   AD,I1
     C********** *IN60     IFEQ '0'
     C********** HSADR1    ANDNE*BLANKS
     C********** HSB1A1    ANDNE*BLANKS
     C**********           MOVELHSZIP     XXZIP1  5
     C**********           MOVELHSB1ZP    XXZIP1  5
     C**********           MOVELHSZSUF    XXZIP2  4
     C**********           MOVELHSB1ZS    XXZIP2  4
     C********** HSADR1    CAT  ',':0     AD,I1
     C********** HSB1A1    CAT  ',':0     AD,I1
     C********** AD,I1     CAT  HSCTY:1   AD,I1
     C********** AD,I1     CAT  HSB1CY:1  AD,I1
     C********** AD,I1     CAT  ',':0     AD,I1
     C********** AD,I1     CAT  HSST:1    AD,I1
     C********** AD,I1     CAT  HSB1ST:1  AD,I1
     C********** AD,I1     CAT  ',':0     AD,I1
     C********** AD,I1     CAT  XXZIP1:1  AD,I1
     C********** AD,I1     CAT  '-':0     AD,I1
     C********** AD,I1     CAT  XXZIP2:0  AD,I1
     C**********           ELSE
     C**********           MOVELDE,I1     AD,I1
     C**********           MOVE *BLANKS   AD,I1
     C**********           END
     C*** END   DEL *** 06/14/95 **************************************
     C*** END ADD *** 08/22/94 *************************************************
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     1             Z
     C*** BEGIN CHG *** 02/18/98 **************************************
     C********** HSNWCD    LOKUPGN,Z                     70
     C     S4NWCD        LOOKUP    GN(Z)                                  70
     C     *IN70         IFEQ      '1'
     C**********           MOVELHSNWCD    PN,Z
     C**********           Z-ADDHSCEDT    SRDTI
     C                   MOVEL     S4NWCD        PN(Z)
     C                   Z-ADD     S4BGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PE(Z)
     C**********           GOTO OKPRO1
     C                   END                                                    END *IN70
     C********** HSKEY     READEPHS01                    60
     C     S4KEY         READE     PS401                                  60
     C*** END   CHG *** 02/18/98 **************************************
     C                   END
     C********** *IN60     IFEQ '1'
     C**********           MOVEL*BLANKS   NW,I1
     C**********           Z-ADD*ZEROS    EF,I1
     C**********           END                             END *IN60
     C********** OKPRO1    TAG
     C                   MOVE      *BLANKS       NW(I1)
     C                   Z-ADD     *ZEROS        EF(I1)
     C                   DO        10            Z
     C     PN(Z)         IFNE      *BLANKS
     C                   MOVEL     PN(Z)         NW(I1)
     C                   Z-ADD     PE(Z)         EF(I1)
     C                   GOTO      OKPRO1
     C                   END
     C                   END
     C     OKPRO1        TAG
     C*
     C                   END
     C                   END
     C*** BEG ADD *** 08/10/98 ***************************************
     C                   ENDIF                                                  IF PDPARN =P
     C*** END ADD *** 08/10/98 ***************************************
     C                   ELSE                                                   ELSE *IN74 EQ '1'
     C                   MOVE      *BLANKS       NW(I1)
     C                   Z-ADD     *ZEROS        EF(I1)
     C                   MOVE      *BLANKS       AD(I1)
     C                   END                                                    END *IN74 EQ '1'
     C*** END ADD *** 03/21/94 *****************************************
     C*
     C*** BEGIN ADD *** 04/16/91 ************* DVM *******************
     C                   MOVEL     PDFEIN        PROMOD           12 0
     C                   MOVE      PDMOD         PROMOD
     C                   MOVE      PROMOD        PRM(I1)
     C***** END ADD *** 04/16/91 ************* DVM *******************
     C*** BEGIN ADD *** 06/14/95 **************************************
     C*********  LOAD PHYSICAL NAME   FROM PROVIDER AUXILIARY FILE
     C                   MOVE      *BLANKS       PM(I1)
     C  N50P1FSN         IFEQ      *BLANKS
     C                   MOVEL     P1NAME        PM(I1)
     C                   ELSE
     C                   MOVE      *BLANKS       QQLAST
     C                   MOVE      *BLANKS       QQFRST
     C                   MOVEL     P1NAME        QQLAST           56
     C                   MOVEL     P1FSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        PM(I1)
     C                   END
     C*
     C*** GET GEOGRAPHICAL ZIP CODE AREA
     C                   MOVE      *BLANKS       XXZIPA
     C     *IN50         IFEQ      '0'
     C                   MOVEL     P1ZIP         XXFRZP
     C*** BEGIN CHG *** 02/04/98 **************************************
     C********** GZKY      CHAINPGZ01                60
     C     GZKY          SETGT     PGZ01
     C                   READP     PGZ01                                  60
     C*** END CHG *** 02/04/98 ****************************************
     C     *IN60         DOWEQ     '0'
     C*
     C     XXFRZP        IFLE      GZTOZP
     C                   MOVE      GZZIPA        XXZIPA
     C                   LEAVE
     C                   END                                                    END 0 *IN60
     C*** BEGIN ADD *** 02/04/98 *************************************
     C********** GZKY      READEPGZ01                    60
     C                   READP     PGZ01                                  60
     C*** END ADD *** 02/04/98 ***************************************
     C                   ENDDO
     C                   END                                                    END 0 *IN50
     C*
     C***        LOAD PHYSICAL ADDRESS (IF FMP101 READ)                *********
     C                   MOVE      *BLANKS       AD(I1)
     C  N50P1ADR1        IFNE      *BLANKS
     C                   MOVEL     P1ZIP         XXZIP1            5
     C                   MOVE      P1ZIP         XXZIP2            4
     C*** BEG ADD *** 03/23/99 ****************************************
     C                   MOVEL(P)  P1ADR1        XXADDR
     C                   MOVEL(P)  P1CTY         XXCITY
     C                   MOVEL(P)  P1PST         XXST
     C                   MOVEL(P)  P1CNTY        XXCNTY
     C                   MOVE      *BLANKS       XXNAME
     C                   MOVE      *BLANKS       SNAME
     C                   MOVE      *BLANKS       SADDR
     C                   MOVE      *BLANKS       SCITY
     C                   MOVE      *BLANKS       SSTATE
     C                   MOVE      *BLANKS       SZIP1
     C                   MOVE      *BLANKS       SZIP2
     C                   MOVE      *BLANKS       SGEOZ
     C                   MOVE      *BLANKS       SCNTY
     C                   EXSR      COMB
     C                   MOVEL     XXNAME        AD(I1)
     C*** END ADD *** 03/23/99 ****************************************
     C*
     C*** BEG DEL *** 03/23/99 ****************************************
     C********** P1ADR1    CAT  ',':0     AD,I1
     C********** AD,I1     CAT  P1CTY:1   AD,I1
     C********** AD,I1     CAT  ',':0     AD,I1
     C********** AD,I1     CAT  P1PST:1   AD,I1
     C********** AD,I1     CAT  XXZIP1:2  AD,I1
     C********** AD,I1     CAT  '-':0     AD,I1
     C********** AD,I1     CAT  XXZIP2:0  AD,I1
     C********** AD,I1     CAT  ',':0     AD,I1
     C********** AD,I1     CAT  XXZIPA:1  AD,I1
     C*
     C*** END DEL *** 03/23/99 ****************************************
     C*
     C                   END
     C*
     C***  IF THE PHYSICAL NAME OR ADDRESS EQUALS THE MAILING
     C***  DISPLAY 'SAME' INSTEAD OF REPEATING THE INFO
     C     PM(I1)        IFEQ      NM(I1)
     C                   MOVEL(P)  'SAME'        PM(I1)
     C                   END
     C*
     C*******    DE,I1     IFEQ AD,I1
     C*** BEG CHG *** 03/23/99 ****************************************
     C********** PDADR1    IFEQ P1ADR1
     C********** PDCTY     ANDEQP1CTY
     C********** PDST      ANDEQP1PST
     C********** PDZIP     ANDEQP1ZIP
     C     DE(I1)        IFEQ      AD(I1)
     C*** END CHG *** 03/23/99 ****************************************
     C                   MOVEL(P)  'SAME'        AD(I1)
     C                   END
     C*
     C*** END   ADD *** 06/14/95 **************************************
     C*
     C*
     C*
     C     FILLE         ENDSR
     C*
     C*  LOAD FORWARD SUBROUTINE
     C*
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C     OVER          TAG
     C*
     C*  END OF FILE
     C*
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C*** BEG ADD *** 04/01/99 ****************************************
     C*** BEG CHG *** 11/02/09 **************************************
     C****       PDSTAT    IFNE 'I'
     C     PDSTAT        IFEQ      'A'
     C     PDSTAT        OREQ      *BLANKS
     C*** END CHG *** 11/02/09 **************************************
     C*** END ADD *** 04/01/99 ****************************************
     C                   ADD       1             I1
     C                   EXSR      FILL
     C*** BEG ADD *** 04/01/99 ****************************************
     C                   ENDIF                                                  PDSTAT NE I
     C*** END ADD *** 04/01/99 ****************************************
     C     PDKEY         READE     PPD01                                  60
     C                   MOVE      '1'           *IN50                          ADD 06/14/95
     C  N60P1KY          CHAIN     PP101                              50        ADD 03/15/93
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C*** BEGIN CHG *** 03/15/93 **************************************
     C*** BEGIN CHG *** 08/22/94 **************************************
     C*** BEGIN CHG *** 06/14/95 **************************************
     C*********  I1        COMP 14                     61
     C*********  I1        COMP 6                      61
     C*********  I1        COMP 4                      61
     C     I1            COMP      3                                    61
     C*** END   CHG *** 06/14/95 **************************************
     C*** END   CHG *** 08/22/94 **************************************
     C*** END   CHG *** 03/15/93 **************************************
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     FE(1)         KEYF(1)
     C*** BEGIN CHG *** 03/15/93 **************************************
     C*** BEGIN CHG *** 08/22/94 **************************************
     C*** BEGIN CHG *** 06/14/95 **************************************
     C**************       Z-ADDFE,14     KEYF,2
     C**********           Z-ADDFE,6      KEYF,2
     C**********           Z-ADDFE,4      KEYF,2
     C                   Z-ADD     FE(3)         KEYF(2)
     C*** END   CHG *** 06/14/95 **************************************
     C*** END   CHG *** 08/22/94 **************************************
     C*** END   CHG *** 03/15/93 **************************************
     C                   Z-ADD     MO(1)         KEYM(1)
     C*** BEGIN CHG *** 03/15/93 **************************************
     C*** BEGIN CHG *** 08/22/93 **************************************
     C*** BEGIN CHG *** 06/14/95 **************************************
     C**************       Z-ADDMO,14     KEYM,2
     C**********           Z-ADDMO,6      KEYM,2
     C**********           Z-ADDMO,4      KEYM,2
     C                   Z-ADD     MO(3)         KEYM(2)
     C*** END   CHG *** 06/14/95 **************************************
     C*** END   CHG *** 08/22/93 **************************************
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C*** BEGIN CHG *** 03/15/93 **************************************
     C*** BEGIN CHG *** 08/22/94 **************************************
     C*** BEGIN CHG *** 06/14/95 **************************************
     C**************       Z-ADD15        I1
     C**********           Z-ADD7         I1
     C**********           Z-ADD5         I1
     C                   Z-ADD     4             I1
     C*** END   CHG *** 06/14/95 **************************************
     C*** END   CHG *** 08/22/94 **************************************
     C*** END   CHG *** 03/15/93 **************************************
     C     OVERP         TAG
     C*
     C*  BEGINNING OF FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   EXSR      LOADB
     C                   GOTO      DONEP
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C*** BEG ADD *** 04/01/99 ****************************************
     C*** BEG CHG *** 11/02/09 **************************************
     C****       PDSTAT    IFNE 'I'
     C     PDSTAT        IFEQ      'A'
     C     PDSTAT        OREQ      *BLANKS
     C*** END CHG *** 11/02/09 **************************************
     C*** END ADD *** 04/01/99 ****************************************
     C                   SUB       1             I1
     C                   EXSR      FILL
     C*** BEG ADD *** 04/01/99 ****************************************
     C                   ENDIF                                                  PDSTAT NE I
     C*** END ADD *** 04/01/99 ****************************************
     C                   READP     PPD01                                  60
     C                   MOVE      '1'           *IN50                          ADD 06/14/95
     C  N60P1KY          CHAIN     PP101                              50        ADD 03/15/93
     C     PDFEIN        IFNE      XXFEIN
     C                   MOVE      '1'           *IN60
     C                   END
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     FE(1)         KEYF(1)
     C*** BEGIN CHG *** 03/15/93 **************************************
     C*** BEGIN CHG *** 08/22/94 **************************************
     C*** BEGIN CHG *** 06/14/95 **************************************
     C**************       Z-ADDFE,14     KEYF,2
     C**********           Z-ADDFE,6      KEYF,2
     C**********           Z-ADDFE,4      KEYF,2
     C                   Z-ADD     FE(3)         KEYF(2)
     C*** END   CHG *** 06/14/95 **************************************
     C*** END   CHG *** 08/22/94 **************************************
     C*** END   CHG *** 03/15/93 **************************************
     C                   Z-ADD     MO(1)         KEYM(1)
     C*** BEGIN CHG *** 03/15/93 **************************************
     C*** BEGIN CHG *** 08/22/94 **************************************
     C*** BEGIN CHG *** 06/14/95 **************************************
     C**************       Z-ADDMO,14     KEYM,2
     C**********           Z-ADDMO,6      KEYM,2
     C**********           Z-ADDMO,4      KEYM,2
     C                   Z-ADD     MO(3)         KEYM(2)
     C*** END   CHG *** 06/14/95 **************************************
     C*** END   CHG *** 08/22/94 **************************************
     C*** END   CHG *** 03/15/93 **************************************
     C                   ENDSR
     C*
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     QQFEIN        XXFEIN
     C     PDKEY         SETLL     PPD01
     C     PDKEY         READE     PPD01                                  60
     C                   MOVE      '1'           *IN50                          ADD 06/14/95
     C  N60P1KY          CHAIN     PP101                              50        ADD 03/15/93
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C*** BEG ADD *** 04/01/99 ****************************************
     C*** BEG CHG *** 11/02/09 **************************************
     C****       PDSTAT    IFNE 'I'
     C     PDSTAT        IFEQ      'A'
     C     PDSTAT        OREQ      *BLANKS
     C*** END CHG *** 11/02/09 **************************************
     C*** END ADD *** 04/01/99 ****************************************
     C                   ADD       1             I1
     C                   EXSR      FILL
     C*** BEG ADD *** 04/01/99 ****************************************
     C                   ENDIF                                                  PDSTAT NE I
     C*** END ADD *** 04/01/99 ****************************************
     C     PDKEY         READE     PPD01                                  60
     C                   MOVE      '1'           *IN50                          ADD 06/14/95
     C  N60P1KY          CHAIN     PP101                              50        ADD 03/15/93
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C*** BEGIN CHG *** 03/15/93 **************************************
     C*** BEGIN CHG *** 08/22/94 **************************************
     C*** BEGIN CHG *** 06/14/95 **************************************
     C*********  I1        COMP 14                     61
     C*********  I1        COMP 6                      61
     C*********  I1        COMP 4                      61
     C     I1            COMP      3                                    61
     C*** END   CHG *** 06/14/95 **************************************
     C*** END   CHG *** 08/22/94 **************************************
     C*** END   CHG *** 03/15/93 **************************************
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
     C*
     C*
     C*  THIS SUB-PROGRAM ACCEPTS A NAME, ADDRESS AND CITY AND
     C*  COMBINES THEM LEFTMOST INTO A 65 LENGTH NAME
     C*
     C     COMB          BEGSR
     C*
     C                   MOVE      XXNAME        SNAME
     C                   MOVE      XXADDR        SADDR
     C                   MOVE      XXCITY        SCITY
     C*** BEGIN ADD *** 03/15/93 **************************************
     C                   MOVE      XXST          SSTATE
     C                   MOVE      XXZIP1        SZIP1
     C                   MOVE      XXZIP2        SZIP2
     C                   MOVE      XXZIPA        SGEOZ
     C                   MOVE      XXCNTY        SCNTY
     C*** END   ADD *** 03/15/93 **************************************
     C*
     C                   MOVE      '0'           *IN61
     C                   Z-ADD     65            S1                5 0
     C     *IN61         DOWEQ     '0'
     C     S1            ANDGT     *ZERO
     C     *BLANK        COMP      SN(S1)                             6161
     C     *IN61         IFEQ      '0'
     C                   SUB       1             S1
     C                   END
     C                   END
     C*
     C                   MOVE      '0'           *IN62
     C                   Z-ADD     40            S2                5 0
     C     *IN62         DOWEQ     '0'
     C     S2            ANDGT     *ZERO
     C     *BLANK        COMP      SA(S2)                             6262
     C     *IN62         IFEQ      '0'
     C                   SUB       1             S2
     C                   END
     C                   END
     C*
     C                   MOVE      '0'           *IN63
     C*** BEGIN CHG *** 03/15/93 **************************************
     C**************       Z-ADD15        S3      50
     C                   Z-ADD     25            S3                5 0
     C*** END   CHG *** 03/15/93 **************************************
     C     *IN63         DOWEQ     '0'
     C     S3            ANDGT     *ZERO
     C     *BLANK        COMP      SC(S3)                             6363
     C     *IN63         IFEQ      '0'
     C                   SUB       1             S3
     C                   END
     C                   END
     C*
     C*** BEGIN ADD *** 03/15/93 **************************************
     C*
     C                   MOVE      '0'           *IN64
     C                   Z-ADD     2             S4                5 0
     C     *IN64         DOWEQ     '0'
     C     S4            ANDGT     *ZERO
     C     *BLANK        COMP      ST(S4)                             6464
     C     *IN64         IFEQ      '0'
     C                   SUB       1             S4
     C                   END
     C                   END
     C*
     C                   MOVE      '0'           *IN65
     C                   Z-ADD     5             S5                5 0
     C     *IN65         DOWEQ     '0'
     C     S5            ANDGT     *ZERO
     C     *BLANK        COMP      SZ1(S5)                            6565
     C     *IN65         IFEQ      '0'
     C                   SUB       1             S5
     C                   END
     C                   END
     C*
     C                   MOVE      '0'           *IN66
     C                   Z-ADD     4             S6                5 0
     C     *IN66         DOWEQ     '0'
     C     S6            ANDGT     *ZERO
     C     *BLANK        COMP      SZ2(S6)                            6666
     C     *IN66         IFEQ      '0'
     C                   SUB       1             S6
     C                   END
     C                   END
     C*** BEGIN ADD *** 05/18/93 **************************************
     C*
     C                   MOVE      '0'           *IN67
     C                   Z-ADD     3             S7                5 0
     C     *IN67         DOWEQ     '0'
     C     S7            ANDGT     *ZERO
     C     *BLANK        COMP      SZ(S7)                             6767
     C     *IN67         IFEQ      '0'
     C                   SUB       1             S7
     C                   END
     C                   END
     C*
     C                   MOVE      '0'           *IN68
     C                   Z-ADD     15            S8                5 0
     C     *IN68         DOWEQ     '0'
     C     S8            ANDGT     *ZERO
     C     *BLANK        COMP      SY(S8)                             6868
     C     *IN68         IFEQ      '0'
     C                   SUB       1             S8
     C                   END
     C                   END
     C*** END ADD *** 05/18/93 ****************************************
     C*
     C*** END   ADD *** 03/15/93 **************************************
     C*
     C                   Z-ADD     1             SE                5 0
     C                   Z-ADD     1             SF                5 0
     C*** BEGIN ADD *** 03/15/93 **************************************
     C                   Z-ADD     1             SG                5 0
     C                   Z-ADD     1             SH                5 0
     C                   Z-ADD     1             SI                5 0
     C                   Z-ADD     1             SJ                5 0          ADD 05/18/93
     C                   Z-ADD     1             SK                5 0          ADD 05/18/93
     C*** END   ADD *** 03/15/93 **************************************
     C*
     C     S1            ADD       1             SX                5 0
     C*
     C     S1            IFGT      *ZERO
     C     S2            ANDGT     *ZERO
     C     S1            ORGT      *ZERO
     C     S3            ANDGT     *ZERO
     C     SX            CABGT     65            DONE3
     C                   MOVE      ','           SN(SX)
     C                   ADD       2             SX
     C     SX            CABGT     65            DONE3
     C                   END
     C*
     C     SX            DOWLE     65
     C     SE            ANDLE     S2
     C                   MOVE      SA(SE)        SN(SX)
     C                   ADD       1             SE
     C                   ADD       1             SX
     C                   END
     C*
     C     S2            IFGT      *ZERO
     C     S3            ANDGT     *ZERO
     C     SX            CABGT     65            DONE3
     C                   MOVE      ','           SN(SX)
     C                   ADD       2             SX
     C     SX            CABGT     65            DONE3
     C                   END
     C*
     C     SX            DOWLE     65
     C     SF            ANDLE     S3
     C                   MOVE      SC(SF)        SN(SX)
     C                   ADD       1             SF
     C                   ADD       1             SX
     C                   END
     C*
     C*** BEGIN ADD *** 03/15/93 **************************************
     C*
     C     S3            IFGT      *ZERO
     C     S4            ANDGT     *ZERO
     C     SX            CABGT     65            DONE3
     C                   MOVE      ','           SN(SX)
     C                   ADD       2             SX
     C     SX            CABGT     65            DONE3
     C                   END
     C*
     C     SX            DOWLE     65
     C     SG            ANDLE     S4
     C                   MOVE      ST(SG)        SN(SX)
     C                   ADD       1             SG
     C                   ADD       1             SX
     C                   END
     C*
     C     S4            IFGT      *ZERO
     C     S5            ANDGT     *ZERO
     C     SX            CABGT     65            DONE3
     C                   ADD       2             SX
     C     SX            CABGT     65            DONE3
     C                   END
     C*
     C     SX            DOWLE     65
     C     SH            ANDLE     S5
     C                   MOVE      SZ1(SH)       SN(SX)
     C                   ADD       1             SH
     C                   ADD       1             SX
     C                   END
     C*
     C     S5            IFGT      *ZERO
     C     S6            ANDGT     *ZERO
     C     SX            CABGT     65            DONE3
     C                   MOVE      '-'           SN(SX)
     C                   ADD       1             SX
     C     SX            CABGT     65            DONE3
     C                   END
     C*
     C     SX            DOWLE     65
     C     SI            ANDLE     S6
     C                   MOVE      SZ2(SI)       SN(SX)
     C                   ADD       1             SI
     C                   ADD       1             SX
     C                   END
     C*** END   ADD *** 03/15/93 **************************************
     C*** BEGIN ADD *** 05/18/93 **************************************
     C*
     C     S6            IFGT      *ZERO
     C     S7            ANDGE     *ZERO
     C     SX            CABGT     65            DONE3
     C                   MOVE      ','           SN(SX)
     C                   ADD       2             SX
     C     SX            CABGT     65            DONE3
     C                   END
     C*
     C     SX            DOWLE     65
     C     SJ            ANDLE     S7
     C                   MOVE      SZ(SJ)        SN(SX)
     C                   ADD       1             SJ
     C                   ADD       1             SX
     C                   END
     C*
     C     S7            IFGT      *ZERO
     C     S8            ANDGE     *ZERO
     C     SX            CABGT     65            DONE3
     C                   MOVE      ','           SN(SX)
     C                   ADD       2             SX
     C     SX            CABGT     65            DONE3
     C                   END
     C*
     C     SX            DOWLE     65
     C     SK            ANDLE     S8
     C                   MOVE      SY(SK)        SN(SX)
     C                   ADD       1             SK
     C                   ADD       1             SX
     C                   END
     C*** END ADD *** 05/18/93 ****************************************
     C*
     C     DONE3         TAG
     C*
     C                   MOVE      SNAME         XXNAME
     C*
     C                   ENDSR
**
ROLL UP
ROLL DOWN
