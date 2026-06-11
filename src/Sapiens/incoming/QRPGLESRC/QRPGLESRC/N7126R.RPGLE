     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE N7126R - RE-BILL (RE-INVOICE)
     H*
     H*****************************************************************
     H*  INDICATORS:                                                  *
     H* SCR01                                                         *
     H*                                                               *
     H*  29 - POSITION CURSOR AT YEAR                                 *
     H*  30 - POSITION CURSOR AT SUBCRIBER                            *
     H*  31 - POSITION CURSOR AT GROUP                                *
     H*  32 - POSITION CURSOR AT INV#                                 *
     H*  59 - POSITION CURSOR AT CASE                                 *
     H*  77 - ERROR MESSAGE FOR SUB#  VALIDATION                      *
     H*  78 - ERROR MESSAGE FOR GROUP VALIDATION                      *
     H*                                                               *
     H*****************************************************************
     H*                 R E V I S I O N S                             *
     H*                                                               *
     H*  06/09/20 C:212 Project : SY420                               *
     H*                  Mask SSN                                     *
     H*                  Recompile only                               *
     H*                                                               *
     H*  12/09/98 C:62  PROJ: MC068 13496                             *
     H*                 EXTEND KEY TO FYR CASE                        *
     H*                                                               *
     H*  04/20/98 C:91  PROJ: MC181                                   *
     H*                 COMBINED F6 & F11 KEYS INTO ONE F10 KEY       *
     H*                 ADDED GROUP SEARCH AND YEAR/CASE SEARCH TO    *
     H*                  F10 KEY                                      *
     H*                 CHANGED SCREEN DESIGN TO CURRENT STANDARDS    *
     H*                 ADDED SUB# AND GROUP VALIDATION               *
     H*                 CENTERED TITLE ON SCREEN                      *
     H*                                                               *
     H*  07/05/95 C:35  REQ/PROJ: NW008                               *
     H*                 ADDED MEDICAL MANAGEMENT BILLING FILE         *
     H*                                                               *
     H*  03/20/96 C:20  REQ/PROJ: ARMNT                               *
     H*                 1. MODIFICATION TO LOAD FWEOP/EOFLG1 AS BLANKS*
     H*                    THIS FIELD WAS BEING LOADED WITH THE VALUE*
     H*                    IN FAEOP/EOFLG1.  THIS FIELD IS NOT ALWAYS*
     H*                    BLANKS.                                   *
     H*                 1. MODIFICATION TO LOAD FNEOP/EOFLG1 AS BLANKS*
     H*                    THIS FIELD WAS BEING LOADED WITH THE VALUE*
     H*                    IN FAEOP/EOFLG1.  THIS FIELD IS NOT ALWAYS*
     H*                    BLANKS.                                   *
     H*                                                              *
     H*  06/12/97 C:59  PROJ: AR025                                  **
     H*                 1. Allow EOB's with a Reviewer Fee amount of **
     H*                    0 to display on the screen                *
     H*                                                              *
     H****************************************************************
     H*  FILES:                                                      *
     H*  FWEOP:  PBR/HBA REBILL WORK FILE (CO/SUB/FND/FYR/CASE/INV)  *
     H*          MEMBER CREATED IN F7126C                            *
     H*          RECORDS ADDED IN F7126C1 (CALLED FROM THIS RPG)     *
     H*  FNEOP:  NETWORK REBILL WORK FILE (CO/SUB/EOB#)              *
     H*          MEMBER CREATED IN F7126C                            *
     H*          RECORDS ADDED IN F7126C1 (CALLED FROM THIS RPG)     *
     H****************************************************************
     FN7126F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
      **** BEGIN DEL **** 04/20/98 ************************************
     F********** FMCMLA  IF  E           K        DISK
     F**********  PCM01                             KRENAMEPCM02
      **** END   DEL **** 04/20/98 ************************************
     FFMCDL     IF   E           K DISK
     FNMARL6    IF   E           K DISK
     F*** BEGIN CHG *** 03/29/93 **************************************
     F**********FAEOL7  IF  E           K        DISK
     FFAEOL8    IF   E           K DISK
     F*** END CHG *** 03/29/93 ****************************************
     F*** BEGIN ADD *** 12/09/98 **************************************
     FFAEOL7    IF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
     F*** END   ADD *** 12/09/98 **************************************
     F*** BEGIN OF ADD ***  CONNIE/A9C *** 9/11/89 ***
     F*** BEGIN DEL *** 03/29/93 **************************************
     F**********FMPDL   IF  E           K        DISK
     F**********FMSML   IF  E           K        DISK
     F*** END DEL *** 03/29/93 ****************************************
     FFAEOL3    IF   E           K DISK
     F                                     RENAME(PEO01:PEO03)
     FFAHBL     IF   E           K DISK
     F*** BEGIN ADD *** 03/16/93 **************************************
     FNAPBL1    IF   E           K DISK
     F*** END ADD *** 03/16/93 ****************************************
     F*** BEGIN ADD *** 05/08/95 ***********************************************
     FNAMBL1    IF   E           K DISK
     F*** END ADD *** 05/08/95 *************************************************
     F*** BEGIN CHG *** 03/29/93 **************************************
     F**********NATRL   UF  E           K        DISK
     FNATRL     IF   E           K DISK
     F*** END CHG *** 03/29/93 ****************************************
      **** BEGIN ADD **** 04/20/98 ************************************
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     FFMSMP     IF   E             DISK
     FFMFNL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     F                                     RENAME(PSM01:PSM02)
      **** END   ADD **** 04/20/98 ************************************
     F*** BEGIN CHG *** 07/14/93 **************************************
     F**********FWEOP   UF  E           K        DISK
     FFWEOP     UF A E           K DISK
     F                                     RENAME(PEO01:PEW01)
     F*** END CHG *** 07/14/93 ****************************************
     FFNEOP     UF A E           K DISK
     F                                     RENAME(PEO01:PEN01)
     F*
     F*** END OF ADD ***  CONNIE/A9C *** 9/11/89 ***
     F*

     D KEY             S              9  0 DIM(2)                               EOB KEY
     D********************MSG     1   1 45
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D*** END ADD *** 03/20/96 *************************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  FL                     1     14
     D                                     DIM(14)                              FLAG
     D  SCFL01                 1      1
     D  SCFL02                 2      2
     D  SCFL03                 3      3
     D  SCFL04                 4      4
     D  SCFL05                 5      5
     D  SCFL06                 6      6
     D  SCFL07                 7      7
     D  SCFL08                 8      8
     D  SCFL09                 9      9
     D  SCFL10                10     10
     D  SCFL11                11     11
     D  SCFL12                12     12
     D  SCFL13                13     13
     D  SCFL14                14     14
     D                 DS
     D  EO                     1    126  0
     D                                     DIM(14)                              EOB #
     D  SCEO01                 1      9  0
     D  SCEO02                10     18  0
     D  SCEO03                19     27  0
     D  SCEO04                28     36  0
     D  SCEO05                37     45  0
     D  SCEO06                46     54  0
     D  SCEO07                55     63  0
     D  SCEO08                64     72  0
     D  SCEO09                73     81  0
     D  SCEO10                82     90  0
     D  SCEO11                91     99  0
     D  SCEO12               100    108  0
     D  SCEO13               109    117  0
     D  SCEO14               118    126  0
     D                 DS
     D  SY                     1     14
     D                                     DIM(14)                              SYSTEM ID
     D  SCSY01                 1      1
     D  SCSY02                 2      2
     D  SCSY03                 3      3
     D  SCSY04                 4      4
     D  SCSY05                 5      5
     D  SCSY06                 6      6
     D  SCSY07                 7      7
     D  SCSY08                 8      8
     D  SCSY09                 9      9
     D  SCSY10                10     10
     D  SCSY11                11     11
     D  SCSY12                12     12
     D  SCSY13                13     13
     D  SCSY14                14     14
     D                 DS
     D  DT                     1    112  0
     D                                     DIM(14)                              TRANS DATE
     D  SCDT01               001    008  0
     D  SCDT02               009    016  0
     D  SCDT03               017    024  0
     D  SCDT04               025    032  0
     D  SCDT05               033    040  0
     D  SCDT06               041    048  0
     D  SCDT07               049    056  0
     D  SCDT08               057    064  0
     D  SCDT09               065    072  0
     D  SCDT10               073    080  0
     D  SCDT11               081    088  0
     D  SCDT12               089    096  0
     D  SCDT13               097    104  0
     D  SCDT14               105    112  0
     D                 DS
     D  DS                     1    420
     D                                     DIM(14)                              DESCRIPTION
     D  SCDS01                 1     30
     D  SCDS02                31     60
     D  SCDS03                61     90
     D  SCDS04                91    120
     D  SCDS05               121    150
     D  SCDS06               151    180
     D  SCDS07               181    210
     D  SCDS08               211    240
     D  SCDS09               241    270
     D  SCDS10               271    300
     D  SCDS11               301    330
     D  SCDS12               331    360
     D  SCDS13               361    390
     D  SCDS14               391    420
     D                 DS
     D  AM                     1    126  2
     D                                     DIM(14)                              AMOUNT
     D  SCAM01                 1      9  2
     D  SCAM02                10     18  2
     D  SCAM03                19     27  2
     D  SCAM04                28     36  2
     D  SCAM05                37     45  2
     D  SCAM06                46     54  2
     D  SCAM07                55     63  2
     D  SCAM08                64     72  2
     D  SCAM09                73     81  2
     D  SCAM10                82     90  2
     D  SCAM11                91     99  2
     D  SCAM12               100    108  2
     D  SCAM13               109    117  2
     D  SCAM14               118    126  2
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*** BEGIN ADD *** 03/20/96 ***********************************************
     IPEW01
     I              EOFLG1                      EWFLG1
     IPEN01
     I              EOFLG1                      ENFLG1

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
      **** BEGIN ADD **** 04/20/98 ************************************
     C*
     C* Key List for the FUND Master File
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C* Key List for the SUB# Master File
     C     SMKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C*
      **** END   ADD **** 04/20/98 ************************************
     C*
     C*   KEY LIST FOR THE CASE MASTER FILE (FMCML1 - PCM01)
     C*
     C     CMKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C*
     C*   KEY LIST FOR THE CASE MASTER FILE (FMCMLA - PCM02)
     C*
      **** BEGIN DEL **** 04/20/98 ************************************
     C********** CMKEY2    KLIST
     C**********           KFLD           XXSS#
     C**********           KFLD           XXACDT  70
      **** BEGIN DEL **** 04/20/98 ************************************
     C*
     C*   KEY LIST FOR THE EOB MASTER FILE
     C*
     C     EOKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCINV#
     C*
     C*** BEGIN CHG *** 03/29/93 **************************************
     C     EOKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C**********           KFLD           XXFYR
     C**********           KFLD           SCCASE
     C                   KFLD                    SCINV#
     C                   KFLD                    XXEOB#
     C*** END CHG *** 03/29/93 ****************************************
     C*
     C*** BEGIN OF ADD *** CONNIE/A9C *** 9/11/89
     C*
     C     EOKEY3        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    AREOB#
     C*** BEGIN ADD *** 12/09/98 **************************************
     C     EOKEY4        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCINV#
     C                   KFLD                    XXEOB#
     C*** END   ADD *** 12/09/98 **************************************
     C*
     C     EWKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C**********           KFLD           XXFYR            DEL 03/29/93
     C**********           KFLD           SCCASE           DEL 03/29/93
     C                   KFLD                    SCINV#
     C*** BEGIN ADD *** 12/09/98 **************************************
     C*** END   ADD *** 12/09/98 **************************************
     C     EWKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR                          DEL 03/29/93
     C                   KFLD                    SCCASE                         DEL 03/29/93
     C                   KFLD                    SCINV#
     C*** BEGIN ADD *** 07/06/93 **************************************
     C*
     C     EWKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCINV#
     C                   KFLD                    XXEOB#
     C*
     C     ENKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCINV#
     C                   KFLD                    XXEOB#
     C*** END ADD *** 07/06/93 ****************************************
     C**
     C     ENKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C*** BEGIN CHG *** 03/29/93 **************************************
     C                   KFLD                    SCFND
     C                   KFLD                    SCINV#
     C**********           KFLD           AREOB#
     C*** END CHG *** 03/29/93 ****************************************
     C*** BEGIN DEL *** 03/29/93 **************************************
     C**********
     C********** PDKEY     KLIST
     C**********           KFLD           EOFEIN
     C**********           KFLD           EOMOD
     C**********
     C********** SBKEY     KLIST
     C**********           KFLD           EOCO#
     C**********           KFLD           EOSUB#
     C*** END DEL *** 03/29/93 ****************************************
     C*
     C     TRKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C**
     C*
     C     HBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** BEGIN ADD *** 03/17/93 **************************************
     C* PRE-CERT BILLING FILE
     C     PBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** END ADD *** 03/17/93 ****************************************
     C*** BEGIN ADD *** 05/08/95 **************************************
     C* MED MGMT BILLING FILE
     C     MBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** END ADD *** 05/08/95 ****************************************
     C**
     C*** END OF ADD *** CONNIE/A9C *** 9/11/89
     C*
     C*   KEY LIST FOR THE A/R MASTER FILE
     C*
     C     ARKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCINV#
     C*
     C     ARKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCINV#
     C                   KFLD                    XXEOB#
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMOGRAPHIC FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   PARAMETER LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO              3
     C                   PARM                    QQSUB             5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C*** BEGIN CHG *** 09/23/99 **************************************
     C**********           PARM           QQINV#  5
     C                   PARM                    QQINV#            9
     C*** END   CHG *** 09/23/99 **************************************
     C                   PARM                    QQDATE            8
     C                   PARM                    QQTYPE            1             SYSTEM TYPE
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQMBR             1 0
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
     C*  INITIALIZATION SECTION
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C*** BEGIN ADD *** 06/12/97 **************************************
     C*  HOLDS SUM OF EOMB# FOR THE SAME CASE -- DISPLAYING 0 IF
     C*  AMOUNT HAS BEEN CLEARED.
     C*
     C     *LIKE         DEFINE    ARAMT         XXAMT
     C*** END ADD ***** 06/12/97 **************************************
     C*
     C*   LOAD THE OUTPUT CONSTANTS FROM THE PARMS
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*** INITIALIZE FIELDS
     C*
     C                   EXSR      INIT
     C*
     C*   GET THE USER PARAMETER FILE
     C*   GET THE BASE USER DEFAULT VALUES
     C*
     C     TOP           TAG
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *BLANK        SCFND
     C                   MOVE      *ZERO         UPSUB#
     C                   END
     C                   MOVE      UPFND         SCFND
     C                   Z-ADD     UPSUB#        SCSUB#
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
      **** BEGIN ADD **** 04/20/98 ************************************
     C*
     C                   MOVEL(P)  SCPRDS        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
     C*
      **** END   ADD **** 04/20/98 ************************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
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
     C                   EXFMT     SCR02
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C**
     C*** BEGIN OF ADD *** CONNIE/A9C *** 9/11/89
     C**
     C*** BEGIN DEL *** 03/29/93 **************************************
     C********** Q2RECF    IFEQ 'SCR04'
     C**********           EXFMTSCR04
     C**********           END
     C*** END DEL *** 03/29/93 ****************************************
     C**
     C*** END OF ADD *** CONNIE/A9C *** 9/11/89
     C**
     C****************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C**********************************
     C*   COMMAND 03 = END OF JOB      *
     C**********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   MOVE      ' '           QQRTC
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC
     C*
     C************************
     C*   F10 = SEARCH       *
     C************************
     C*
      **** BEGIN DEL **** 04/20/98 ************************************
     C********** *INKF     IFEQ '1'
     C********** Q2RECF    IFEQ 'SCR01'
     C**********           CALL 'F0545R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCSUB#
     C**********           PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C********** QQRTC     IFEQ 'B'
     C********** SCSUB#    OREQ *ZERO
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           MOVE '1'       *IN30            PC AT SUB#
     C**********           GOTO STRT
     C**********           END
     C**********
     C********** QQRTC     IFEQ '*'
     C**********           MOVE '1'       *IN31            PC AT SCFND
     C**********           GOTO STRT
     C**********           ENDIF                           QQRTC EQ *
     C**********           END                             SCR01
     C**********           END                             *INKF
      **** END   DEL **** 04/20/98 ************************************
     C*
      **** BEGIN ADD **** 04/20/98 ************************************
     C*
     C     *INKJ         IFEQ      *ON
     C     Q2RECF        IFEQ      'SCR01'
     C*
      *********************
      * SUBSCRIBER SEARCH *
      *********************
     C*
     C     HFLD          IFEQ      'SCSUB#'
     C*
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     30     PC AT SUB#
     C*
     C     ##RRN         CHAIN     PSM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVE      *ON           *IN31                          PC AT SCFND
     C                   ELSE
     C                   MOVE      *ON           *IN30                          PC AT SCSUB#
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCSUB#
     C*
      ****************
      * GROUP SEARCH *
      ****************
     C     HFLD          IFEQ      'SCFND'
     C*
     C                   MOVE      ##RRN         ##RRN2
     C*
     C                   CALL      'F0060R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN2            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     31     PC AT SCFND
     C*
     C     ##RRN2        CHAIN     PFN02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      FNFND         SCFND
     C                   MOVE      *ON           *IN29                          PC AT SCFYR
     C                   ELSE
     C                   MOVE      *ON           *IN31
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCFND
     C*
      **********************
      * YEAR & CASE SEARCH *
      **********************
     C     HFLD          IFEQ      'SCFYR'
     C     HFLD          OREQ      'SCCASE'
     C*
     C                   MOVE      SCSUB#        Q1SUB#
     C                   MOVE      SCFND         Q1FND
     C*
     C                   CALL      'F0269R'
     C                   PARM      '2'           Q1CALL            1
     C                   PARM                    Q1SUB#            5 0
     C                   PARM                    Q1FND             3 0
     C                   PARM      0             Q1FYR             3 0
     C                   PARM      0             Q1CASE            7 0
     C                   PARM                    Q1SS#             9
     C                   PARM                    Q1ACDT            8 0
     C                   PARM                    QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     Q1CASE        OREQ      *ZERO
     C     HFLD          CABEQ     'SCFYR'       STRT                     29      PC AT SCFYR
     C     HFLD          CABEQ     'SCCASE'      STRT                     59      PC AT SCCASE
     C                   ENDIF                                                  QQRTC EQ B
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      Q1SUB#        SCSUB#
     C                   MOVE      Q1FND         SCFND
     C                   MOVE      Q1FYR         SCFYR
     C                   MOVE      Q1CASE        SCCASE
     C                   MOVE      *ON           *IN32                          PC AT SCINV#
     C                   GOTO      STRT
     C                   ENDIF                                                  QQRTC EQ *
     C*
     C                   ENDIF                                                  SCFYR SCCASE
     C*
      **** END   ADD **** 04/20/98 ************************************
     C*
      **** BEGIN CHG **** 04/20/98 *********************************************
     C**********           END                             ENDIF *INKI
     C********** *INKK     IFEQ '1'
     C*
      ********************
      * INVOICE # SEARCH *
      ********************
     C*
     C     HFLD          IFEQ      'SCINV#'
     C*
      **** END   CHG **** 04/20/98 *********************************************
     C*
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCCASE        QQCASE            7
     C*** BEGIN CHG *** 09/23/99 **************************************
     C**********           MOVE SCINV#    QQINV#  5
     C                   MOVE      SCINV#        QQINV#
     C*** END   CHG *** 09/23/99 **************************************
     C                   CALL      'N0720R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQINV#
     C                   PARM                    QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     SCFND         OREQ      *ZEROS
     C                   MOVE      '1'           *IN32                          PC AT SCINV#
     C                   GOTO      STRT
     C                   ENDIF                                                  QQRTC EQ B
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      QQFND         SCFND
     C                   MOVE      QQFYR         SCFYR
     C                   MOVE      QQCASE        SCCASE
     C                   MOVE      QQINV#        SCINV#
     C                   GOTO      STRT
     C                   ENDIF                                                  QQRTC EQ *
     C                   ENDIF                                                  HFLD SCINV#
      **** BEG   DEL **** 04/20/98 ************************************
     C**********           ENDIF                           *INKK EQ '1'
     C**********************************
     C*   COMMAND 07                   *
     C**********************************
     C********** *INKG     IFEQ '1'
     C**********           MOVE SCFND     Q1FND   30       ADD 10/05/92
     C**********           CALL 'F0570R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCFYR
     C**********           PARM           SCCASE
     C**********           PARM           XXSS#   9
     C**********           PARM           QQRTC   1
     C**********           PARM           Q1FND            ADD 10/05/92
     C**********
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** SCFYR     OREQ *ZERO
     C**********           MOVE '1'       *IN31            PC AT GRP
     C**********           GOTO STRT
     C**********           ENDIF                           QQRTC EQ B
     C**********
     C********** QQRTC     IFEQ '*'
     C**********           MOVE Q1FND     SCFND            ADD 10/05/92
     C**********           MOVE '1'       *IN32            PC AT INV#
     C**********           GOTO STRT
     C**********           ENDIF                           QQRTC EQ *
     C**********
     C**********           ENDIF                           SCCASE
     C**********
     C**********************************
     C* COMMAND 08 - SS#/ACCIDENT DATE *
     C**********************************
     C********** *INKH     IFEQ '1'
     C**********           MOVE *BLANKS   XXSS#
     C**********           Z-ADD*ZEROS    X1ACDT  80
     C**********           CALL 'F0573R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           XXSS#   9
     C**********           PARM           X1ACDT
     C**********           PARM           QQRTC   1
     C**********
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** XXSS#     OREQ *BLANK
     C**********           MOVE '1'       *IN31            PC AT GRP
     C**********           GOTO STRT
     C**********           END                             ENDIF 'B'
     C**********
     C********** QQRTC     IFEQ '*'
     C**********           Z-ADDX1ACDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXACDT
     C********** CMKEY2    CHAINPCM02                60
     C********** *IN60     IFEQ '0'
     C**********           MOVE CMFND     SCFND
     C**********           MOVE CMFYR     SCFYR
     C**********           MOVE CMCASE    SCCASE
     C**********           MOVE '1'       *IN32            PC AT INV#
     C**********           GOTO STRT
     C**********           END                             ENDIF *IN60
     C**********
     C**********           END                             ENDIF '*'
     C**********
     C**********           END                             ENDIF *INKH
     C**********************************
     C*   COMMAND 09 = ALPHA SEARCH    *
     C**********************************
     C********** *INKI     IFEQ '1'
     C**********
     C*** BEGIN CHG *** 05/19/94 **************************************
     C**********           MOVE *BLANK    XXSS#   9
     C**********           CALL 'F0578R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           XXSS#
     C**********           PARM           QQRTC   1
     C**********
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** XXSS#     OREQ *ZERO
     C**********           MOVE '1'       *IN31            PC AT GRP
     C**********           GOTO STRT
     C**********           END                             ENDIF 'B'
     C**********
     C********** QQRTC     IFEQ '*'
     C**********           Z-ADD*ZEROS    X1FND
     C**********           CALL 'F0571R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           XXSS#   9
     C**********           PARM           XXYR    20
     C**********           PARM           XXCASE  70
     C**********           PARM           X1FND   30
     C**********           PARM           QQRTC   1
     C**********           Z-ADDX1FND     SCFND
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** XXSS#     OREQ *BLANK
     C**********           MOVE '1'       *IN31            PC AT GRP
     C**********           GOTO STRT
     C**********           END                             ENDIF 'B'
     C**********
     C********** QQRTC     IFEQ '*'
     C**********           MOVE XXYR      SCFYR
     C**********           MOVE XXCASE    SCCASE
     C**********           MOVE '1'       *IN32            PC AT INV#
     C**********           GOTO STRT
     C**********           END                             ENDIF '*'
     C**********
     C**********           END                             ENDIF '*'
     C**********
     C**********           MOVE *BLANK    XXSS#   9
     C**********           Z-ADD*ZEROS    X1ACDT
     C**********           CALL 'F0578R1'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           XXSS#
     C**********           PARM           X1ACDT
     C**********           PARM           SCSUB#
     C**********           PARM           SCFND
     C**********           PARM           SCFYR
     C**********           PARM           SCCASE
     C**********           PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** SCFYR     OREQ *ZEROS
     C**********           MOVE '1'       *IN31            PC AT GRP
     C**********           GOTO STRT
     C**********           END                             ENDEQ 'B'
     C**********
     C********** QQRTC     IFEQ '*'
     C**********           MOVE UPCO#     QQCO#   3
     C**********           MOVE SCSUB#    QQSUB#  5        ADD 06/29/94
     C**********           MOVE SCFND     QQFND
     C**********           CALL 'F0006R2'                  CHG 06/29/94
     C**********           PARM           QQCO#
     C**********           PARM           QQSUB#           ADD 06/29/94
     C**********           PARM           QQFND
     C**********           PARM           QQRTCD
     C**********           MOVE '1'       *IN32            PC AT INV#
     C**********           GOTO STRT
     C**********           END
     C*** END CHG *** 05/19/94 ****************************************
      **** END   DEL **** 04/20/98 ************************************
     C*
      **** BEGIN ADD **** 04/20/98 ************************************
     C                   ENDIF                                                  SCR01
     C                   ENDIF                                                  *INKJ
      **** END   ADD **** 04/20/98 ************************************
     C*
     C**********************************
     C*   COMMAND 12 = PRIOR SCREEN    *
     C**********************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   Z-ADD     *ZEROS        XXEOB#
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  SCR02
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   Z-ADD     *ZEROS        XXEOB#
     C     ARSYS         IFEQ      'N'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   ELSE
     C                   MOVEL     'SCR02'       Q2RECF
     C                   ENDIF                                                  ARSYS = N
     C*
     C                   GOTO      STRT
     C                   ENDIF                                                  SCR03
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C                   Z-ADD     *ZEROS        XXEOB#
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  SCR04
     C                   ENDIF                                                  *INKL
     C*
     C**********************************
     C*   ROLL UP REQUEST              *
     C**********************************
     C     *IN18         CABEQ     '1'           SCR02T
     C**********************************
     C*   ROLL DOWN REQUEST            *
     C**********************************
     C     *IN19         CABEQ     '1'           SCR02T
     C**********************************
     C*   PROCESS HELP REQUESTS        *
     C**********************************
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    ENDEQ *IN10
      **** BEGIN DEL **** 04/20/98 ************************************
     C**********************************
     C*   COMMAND 13 - CHG USER PARMS  *
     C**********************************
     C********** *INKM     IFEQ '1'
     C**********
     C**********           CALL 'F0006R'
     C**********
     C**********  GET THE USER PARAMETER FILE
     C**********
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C**********           MOVE *ZERO     UPCO#
     C**********           MOVE *BLANK    UPFND
     C**********           END
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO TOP
     C**********           END                             ENDEQ *INKM
      **** END   DEL **** 04/20/98 ************************************
     C*** BEGIN DEL *** 03/29/93 **************************************
     C**********************************
     C*   COMMAND 23  DELETE INVOICE   *
     C**********************************
     C********** *INKX     IFEQ '1'
     C**********           EXSR DELETE
     C**********           MOVEL'SCR03'   Q2RECF
     C**********           SETON                     99
     C**********           MOVELMSG,1     SCDMSG
     C**********           GOTO STRT
     C**********           END                             ENDEQ *INKU
     C*** END DEL **8 03/29/93 ****************************************
     C*
     C**********************************
     C*   COMMAND 24  SUBMIT RE-BILL   *
     C**********************************
     C     *INKY         IFEQ      '1'
     C*
     C     ARSYS         IFEQ      'N'
     C                   EXSR      RITEN
     C                   ELSE
     C                   EXSR      RITE
     C                   END                                                    ARSYS EQ N
     C*
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    ENDEQ *INKY
     C*
     C                   END                                                    END CMD KEYS
     C*****************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C*** ADD NEXT LINE ***  CONNIE/A9C *** 9/11/89 ***
     C********** Q2RECF    CABEQ'SCR04'   SCR04T           DEL 03/29/93
     C******************************************
     C*   PROCESS REQUEST SCREEN - SCREEN 01   *
     C******************************************
     C     SCR01T        TAG
     C*
      **** BEGIN ADD **** 04/20/98 ************************************
     C*
     C* Validate the SUB#, if entered.
     C*
     C                   Z-ADD     SCSUB#        XXSUB#            5 0
     C     SMKY          CHAIN     PSM02                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *ON           *IN77
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60 IFEQ 1
     C*
     C* Validate the FUND, if entered.
     C*
     C                   Z-ADD     SCFND         XXFND             3 0
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *ON           *IN78
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60 IFEQ 1
     C*
      **** END   ADD **** 04/20/98 ************************************
     C*
     C***  VALIDATE CASE NUMBER ENTERED
     C*
     C                   MOVE      SCFYR         XXFYR             3 0
     C     CMKEY1        CHAIN     PCM01                              20
     C     *IN20         CABEQ     '1'           STRT                           INVALID CASE
     C*
     C***  VALIDATE INVOICE # ENTERED
     C*
     C     ARKEY1        CHAIN     PAR01                              21
     C     *IN21         IFEQ      '1'                                          INVALID INV#
     C                   GOTO      STRT
     C                   ELSE
     C                   Z-ADD     ARTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDATE
     C                   END
     C**
     C*** BEGIN OF DELETE *** CONNIE/A9C *** 9/11/89
     C**
     C*
     C* Do not allow user to rebill a network invoice      LJQ 7-27-89
     C*                                                    |
     C********** ARSYS     IFEQ 'N'                        |
     C**********           MOVE '1'       *IN22            |
     C**********           GOTO STRT                       |
     C**********           END                             |
     C**
     C*** END OF DELETE *** CONNIE/A9C *** 9/11/89
     C*
     C* * * *  LAST,FIRST NAME SUBPROGRAM FOR CLAIMANT NAME
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANK        SRLNAM           56
     C                   MOVEL     CDLSN         SRLNAM
     C                   MOVEL     CDFSN         SRFNAM           15
     C                   MOVEL     ' '           SRTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SRTERM
     C                   MOVEL     SRLNAM        SCCLNM
     C                   ELSE
     C                   MOVE      *BLANKS       SCCLNM
     C                   END                                                    ENDIF *IN60
     C*
     C                   MOVE      *BLANK        Q2RECF
     C******************** MOVEL'SCR02'   Q2RECF
     C                   MOVE      *BLANK        KEY
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVE      *ZEROS        SCEOB#
     C     ARKEY2        SETLL     PAR01                                        INV/EOB
     C     ARKEY1        READE     PAR01                                  60    INV
     C*** BEGIN ADD *** 03/29/93 **************************************
     C     ARSYS         IFNE      'N'
     C                   EXSR      LOAD
     C                   ELSE
     C                   MOVE      'N'           QQTYPE
     C                   EXSR      LOADN
     C                   END
     C*** END ADD *** 03/29/93 ****************************************
     C**
     C*** BEGIN OF ADD *** CONNIE/A9C *** 9/11/89
     C*
     C                   MOVE      *BLANKS       QQCO
     C                   MOVE      *BLANKS       QQSUB
     C                   MOVE      *BLANKS       QQFND
     C                   MOVE      *BLANKS       QQFYR
     C                   MOVE      *BLANKS       QQCASE
     C                   MOVE      *BLANKS       QQINV#
     C                   MOVE      *BLANKS       QQTYPE
     C*
     C                   MOVE      UPCO#         QQCO
     C                   MOVE      SCSUB#        QQSUB
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      SCINV#        QQINV#
     C*
     C**
     C*** BEGIN DEL *** 07/14/93 **************************************
     C* CALL PGM TO BUILD WORK FILE TO STORE UPDATES FOR F7125R
     C********** ARSYS     IFEQ 'N'
     C**********           MOVE AREOB#    QQEOB#
     C**********           MOVE 'N'       QQTYPE
     C**********           END
     C********** ARSYS     IFNE 'N'                        ADD 03/29/93
     C**********
     C**********           CALL 'N7126C1'
     C**********           PARM           QQCO    3
     C**********           PARM           QQSUB   5
     C**********           PARM           QQFND   3
     C**********           PARM           QQFYR   3
     C**********           PARM           QQCASE  7
     C**********           PARM           QQINV#  5
     C**********           PARM           QQEOB#  9
     C**********           PARM           QQTYPE  1
     C**********           PARM           QQMBR   10
     C**********           END                             ADD 03/29/93
     C*** END DEL *** 07/14/93 ****************************************
     C**
     C*** BEGIN CHG *** 03/29/93 **************************************
     C     ARSYS         IFEQ      'N'
     C*********************MOVEL'SCR04'   Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*********************EXSR NETWRK
     C*** END CHG *** 03/29/93 ****************************************
     C**
     C                   ELSE
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END
     C**
     C*** END OF ADD *** CONNIE/A9C *** 9/11/89
     C**
     C                   GOTO      STRT
     C*
     C*****************************************************************
     C*   PROCESS EOB DETAIL SCREEN - SCREEN 02 -- USED FOR PBR/HBA'S *
     C*****************************************************************
     C     SCR02T        TAG
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
     C*** BEGIN CHG *** 03/29/93 **************************************
     C     ARSYS         IFNE      'N'
     C                   EXSR      RITE
     C                   ELSE
     C                   EXSR      RITEN
     C                   END
     C*** END CHG *** 03/29/93 ****************************************
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVE      KEY(2)        XXEOB#
     C     ARKEY2        CHAIN     PAR01                              60        INV/EOB
     C*** BEGIN ADD *** 03/29/93 **************************************
     C     ARSYS         IFNE      'N'
     C                   EXSR      LOAD
     C                   ELSE
     C                   EXSR      LOADN
     C                   END
     C*** END ADD *** 03/29/93 ****************************************
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C*** BEGIN CHG *** 03/29/93 **************************************
     C     ARSYS         IFNE      'N'
     C                   EXSR      RITE
     C                   ELSE
     C                   EXSR      RITEN
     C                   END
     C*** END CHG *** 03/29/93 ****************************************
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      KEY(1)        XXEOB#
     C     ARKEY2        CHAIN     PAR01                              60        INV/EOB
     C*** BEGIN ADD *** 03/29/93 **************************************
     C     ARSYS         IFNE      'N'
     C                   EXSR      LOADP
     C                   ELSE
     C                   EXSR      LOADNP
     C                   END
     C*** END ADD *** 03/29/93 ****************************************
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCEOB#        IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVE      SCEOB#        XXEOB#
     C                   MOVE      *BLANK        SCEOB#
     C     ARKEY2        SETLL     PAR01                                        INV/EOB
     C     ARKEY1        READE     PAR01                                  60    INV
     C*** BEGIN ADD *** 03/29/93 **************************************
     C     ARSYS         IFNE      'N'
     C                   EXSR      LOAD
     C                   ELSE
     C                   EXSR      LOADN
     C                   END
     C*** END ADD *** 03/29/93 ****************************************
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEGIN CHG *** 03/29/93 **************************************
     C     ARSYS         IFNE      'N'
     C                   EXSR      RITE
     C                   ELSE
     C                   EXSR      RITEN
     C                   END
     C*** END CHG *** 03/29/93 ****************************************
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C**************************************************
     C* PROCESS SUBMIT REBILL SCREEN - SCREEN 03       *
     C**************************************************
     C     SCR03T        TAG
     C*
     C*** VALIDATE INVOICE DATE
     C*
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C*
     C     SRERR         CABEQ     '1'           STRT                     20    INVALID DATE
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCSUB#        QQSUB#            5
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCCASE        QQCASE            7
     C*** BEGIN CHG *** 09/23/99 **************************************
     C**********           MOVE SCINV#    QQINV#  5
     C                   MOVE      SCINV#        QQINV#
     C*** END   CHG *** 09/23/99 **************************************
     C                   MOVE      SCDATE        QQDATE
     C                   MOVE      '*'           QQRTCD            1
     C*
     C*** BEGIN ADD *** 08/17/93 ************************
     C     ARSYS         IFEQ      'N'
     C                   MOVE      'N'           QQTYPE
     C                   END
     C*** END ADD ***** 08/17/93 ************************
     C*
     C*
     C*** ADD NEXT LINE *** CONNIE/A9C *** 9/11/89
     C*** ... AFTER RE-BILL SUBMITTED, RETURN TO MENU
     C*
     C                   GOTO      TERM
     C*
     C*** BEGIN OF DELETE *** CONNIE/A9C *** 9/11/89
     C*
     C*****                CALL 'F7126C'
     C*****                PARM           QQCO#
     C*****                PARM           QQSUB#
     C*****                PARM           QQFND
     C*****                PARM           QQFYR
     C*****                PARM           QQCASE
     C*****                PARM           QQINV#
     C*****                PARM           QQDATE
     C*****                PARM           QQQUE  10
     C*****                PARM           QQMODE  1
     C*** END OF DELETE *** CONNIE/A9C *** 9/11/89
     C**
     C*** BEGIN OF ADD *** CONNIE/A9C *** 9/11/89
     C**
     C*
     C*** BEGIN DEL *** 03/29/93 **************************************
     C*********************************************************
     C* PROCESS SCREEN FOR NETWORK INVOICES - SCREEN 04       *
     C*********************************************************
     C********** SCR04T    TAG
     C**********
     C**********           EXSR RITEN
     C**********
     C** RE-DISPLAY SCREEN IF NO CMD KEY PRESSED
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR04'   Q2RECF
     C**********           GOTO STRT
     C*** END DEL *** 03/29/93 ****************************************
     C*********************************************************
     C**
     C*** END OF ADD *** CONNIE/A9C *** 9/11/89
     C**
     C***********************
     C*  END THE PROGRAM    *
     C***********************
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C**************************************************
     C*  RITEN - UPDATE WORK FILE WITH SCIFEE          *
     C**************************************************
     C     RITEN         BEGSR
     C*** BEGIN CHG *** 03/29/93 **************************************
     C**********
     C*** VALIDATE INVOICE DATE
     C**********
     C********** SCIFEE    CABEQ*ZEROS    STRT           22INVALID AMT
     C*
     C* VALIDATE OPTION 4 OR BLANKS
     C*
     C                   DO        14            Y                 5 0
     C     Y             ADD       44            Y1                2 0
     C     FL(Y)         IFNE      *BLANK
     C     FL(Y)         ANDNE     '4'
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*** UPDATE WORK FILE WITH REVIEWER AMT
     C*
     C********** ENKEY     CHAINPEN01                66
     C********** ENKEY     SETLLPEN01
     C********** *IN66     IFEQ '0'
     C                   DO        14            V                 2 0
     C**********           Z-ADDSCIFEE    EOBAMT           BILL AMT
     C     EO(V)         IFGT      *ZERO
     C                   Z-ADD     EO(V)         XXEOB#
     C     ENKY1         CHAIN     PEN01                              70        INV/EOB
     C     *IN70         IFEQ      '0'
     C     EO(V)         ANDEQ     EOEOB#
     C*** ADD NEXT LINE *** CONNIE/A9C *** 9/12/89
     C                   Z-ADD     AM(V)         EOBAMT
     C                   MOVEL     FL(V)         EODIFL
     C                   MOVE      Q1USER        EOUSER
     C                   MOVE      Q1PGM         EOPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   Z-ADD     DSTIME        EOTIME
     C                   UPDATE    PEN01
     C                   END                                                    ENDEQ *IN70
     C                   END                                                    ENDGT *ZERO
     C                   END                                                    END DO
     C                   ENDSR
     C*** END CHG *** 03/29/93 ****************************************
     C**************************************************
     C*  INITIALIZE ARRAYS AND SCREEN 1 FIELDS         *
     C**************************************************
     CSR   INIT          BEGSR
     C                   Z-ADD     *ZEROS        SCSUB#
     C                   MOVE      *BLANK        SCFND
     C                   MOVE      *ZERO         SCFYR
     C                   MOVE      *ZERO         SCCASE
     C                   MOVE      *ZERO         SCINV#
     CSR                 ENDSR
     C**************************************************
     C*  CLEAR SCREEN ARRAYS - CLR                     *
     C**************************************************
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       FL
     C                   Z-ADD     *ZEROS        EO
     C                   MOVE      *BLANKS       SY
     C                   Z-ADD     *ZERO         DT
     C                   MOVE      *BLANKS       DS
     C                   Z-ADD     *ZERO         AM
     C                   Z-ADD     *ZERO         I1                3 0
     C                   Z-ADD     *ZERO         Z1                3 0
     C                   ENDSR
     C**************************************************
     C*  FILL SCREEN ARRAYS - FILL                     *
     C**************************************************
     C     FILL          BEGSR
     C                   MOVE      EODIFL        FL(I1)
     C                   MOVE      EOEOB#        EO(I1)
     C                   MOVE      EOSYS         SY(I1)
     C                   Z-ADD     EOTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C                   MOVEL     ARDESC        DS(I1)
     C*
     C***********EOSYS     IFNE 'H'                        DEL 03/17/93
     C     EOSYS         IFEQ      'P'                                          ADD 03/17/93
     C                   MOVE      EOBAMT        AM(I1)
     C                   ELSE
     C*
     C     EOSYS         IFEQ      'H'                                          ADD 03/17/93
     C     HBKEY         CHAIN     PHB01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      HB1AAM        AM(I1)
     C                   END                                                    *IN60 = 0
     C*** BEGIN ADD *** 03/16/93 **************************************
     C                   ELSE                                                   ADD 03/16/93
     C     EOSYS         IFEQ      'U'                                          ADD 03/17/93
     C     PBKEY         CHAIN     PPB01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PBBAMT        AM(I1)
     C                   END                                                    *IN60 = 0
     C*** BEGIN ADD *** 03/29/93 **************************************
     C*** BEGIN ADD *** 05/08/95 **************************************
     C                   ELSE
     C     EOSYS         IFEQ      'M'
     C     MBKEY         CHAIN     PMB01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      MBBAMT        AM(I1)
     C                   END                                                    *IN60 = 0
     C*** BEGIN ADD *** 05/08/95 **************************************
     C                   ELSE
     C     EOSYS         IFEQ      'N'
     C     TRKEY         CHAIN     PTR01                              70
     C     *IN70         IFEQ      '0'
     C                   Z-ADD     TRBAMT        AM(I1)                         REVIEWR FEE
     C                   END
     C                   END
     C                   END
     C*** END CHG *** 03/29/93 ****************************************
     C                   END
     C                   END
     C*** END ADD *** 03/16/93 ****************************************
     C                   END                                                    *EOSYS = H
     C*** BEGIN ADD *** 07/06/93 **************************************
     C     EWKY1         CHAIN     PEW01                              60
     C     *IN60         IFEQ      '0'
     C     EODIFL        IFNE      FL(I1)
     C                   MOVEL     EODIFL        FL(I1)
     C                   END
     C     EOSYS         IFEQ      'P'
     C     EOBAMT        IFNE      AM(I1)
     C                   Z-ADD     EOBAMT        AM(I1)
     C                   END
     C                   ELSE
     C     EOBAMT        IFNE      *ZEROS
     C     EOBAMT        ANDNE     AM(I1)
     C                   Z-ADD     EOBAMT        AM(I1)
     C                   END
     C                   END
     C                   END
     C*** END ADD *** 07/06/93 ****************************************
     C*
     C                   MOVE      '1'           *IN(Z1)                        COLM SEPARATORS
     C                   ENDSR
     C**************************************************
     C*  LOAD FORWARD - LOAD                           *
     C**************************************************
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
     C*** BEGIN ADD *** 06/12/97 **************************************
     C*  This edit will add up the offset and deletes for an eomb
     C*  returning a value of zero to be displayed.  If user clears
     C*  amount without using option 4 to delete EOMB the record will
     C*  stay in the file and will not allow the user to delete the
     C*  EOMB for EOMB delete.  Here we are forcing the user to see
     C*  even though they cleared out the field, it is not deleted.
     C*  Option '4' to delete is required.
     C*
     C*  Without this edit, the program will not display the deleted
     C*  EOMB#.  It displays the line once for 'O' offset,'D' delete
     C*  instead of a line for each item.
     C*
     C                   Z-ADD     *ZEROS        XXAMT
     C     *IN87         DOUEQ     '1'
     C*
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDNE     'B'
     C                   ADD       ARAMT         XXAMT
     C                   END
     C                   Z-ADD     AREOB#        XXEOB#
     C*
     C     ARSTAT        IFEQ      'B'
     C                   LEAVE
     C                   ENDIF
     C     ARKEY2        READE     PAR01                                  87
     C                   ENDDO
     C*
     C*** END ADD ***** 06/12/97 **************************************
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDEQ     'B'
     C*** BEGIN ADD *** 06/12/97 **************************************
     C* This will allow the EOMBs that have been cleared out by the
     C* user.
     C     ARTRTP        OREQ      'I'
     C     ARSTAT        ANDEQ     'D'
     C*** END ADD ***** 06/12/97 **************************************
     C                   Z-ADD     AREOB#        XXEOB#
     C*** BEGIN CHG *** 12/09/98 **************************************
     C     SCCASE        IFEQ      *ZEROS
     C*
     C     EOKEY2        CHAIN     PEO01                              65
     C*
     C                   ELSE
     C*
     C     EOKEY4        CHAIN     PEO02                              65
     C*
     C                   ENDIF
     C*** END   CHG *** 12/09/98 **************************************
     C     *IN65         IFEQ      '0'
     C*** BEGIN ADD *** 06/12/97 **************************************
     C*  This allows us to put in the accumulated total if it was
     C*  adjusted.  After it offsets the original EOMB it creates a
     C*  delete record.  We will display the delete record with the
     C*  sum of the two records ('O' + 'D') = $0
     C*
     C     ARSTAT        IFEQ      'D'
     C                   Z-ADD     XXAMT         EOBAMT
     C                   ELSE
     C                   Z-ADD     ARAMT         EOBAMT                         ADD 07/14/93
     C                   ENDIF
     C*** END ADD ***** 06/12/97 **************************************
     C                   MOVE      *BLANKS       EWFLG1                         ADD 03/20/96
     C                   WRITE     PEW01                                        ADD 07/14/93
     C                   ADD       1             I1
     C     I1            ADD       30            Z1
     C                   EXSR      FILL
     C                   END                                                    ENDEQ *IN65
     C                   END                                                    ENDEQ 'I'
     C*
     C     ARKEY2        SETGT     PAR01                                        ADD 06/12/97
     C     ARKEY1        READE     PAR01                                  60    INV
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVE      EO(1)         KEY(1)
     C                   MOVE      EO(14)        KEY(2)
     C                   ENDSR
     C**************************************************
     C*  LOAD BACKWARD - LOADP                         *
     C**************************************************
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     15            I1
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
     C*** BEGIN ADD *** 06/12/97 **************************************
     C*  IF STATIS IS EQUAL TO 'O', AMOUNT HAS BEEN PREVIOUSLY REMOVED
     C*  BUT THE EOMB HAS NOT BEEN DELETED. INV# STILL CONTAINS THE
     C*  INVOICE NUMBER.
     C*
     C     *IN87         DOUEQ     '1'
     C*
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDNE     'B'
     C                   ADD       ARAMT         EOBAMT
     C                   END
     C                   Z-ADD     AREOB#        XXEOB#
     C*
     C     ARSTAT        IFEQ      'B'
     C                   LEAVE
     C                   ENDIF
     C     ARKEY2        READE     PAR01                                  87
     C                   ENDDO
     C*** END ADD ***** 06/12/97 **************************************
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDEQ     'B'
     C     ARTRTP        OREQ      'I'                                          ADD 06/12/97
     C     ARSTAT        ANDEQ     'D'                                          ADD 06/12/97
     C                   Z-ADD     AREOB#        XXEOB#
     C*** BEGIN CHG *** 12/09/98 **************************************
     C     SCCASE        IFEQ      *ZEROS
     C*
     C     EOKEY2        CHAIN     PEO01                              65
     C*
     C                   ELSE
     C*
     C     EOKEY4        CHAIN     PEO02                              65
     C*
     C                   ENDIF
     C*** END   CHG *** 12/09/98 **************************************
     C     *IN65         IFEQ      '0'
     C                   SUB       1             I1
     C     I1            ADD       30            Z1
     C                   EXSR      FILL
     C                   END                                                    ENDEQ *IN65
     C                   END                                                    ENDEQ 'I'
     C*
     C                   READP     PAR01                                  60
     C  N60ARCO#         COMP      UPCO#                              6060
     C  N60ARSUB#        COMP      SCSUB#                             6060
     C  N60ARFND         COMP      SCFND                              6060
     C  N60ARFYR         COMP      XXFYR                              6060
     C  N60ARCASE        COMP      SCCASE                             6060
     C  N60ARINV#        COMP      SCINV#                             6060      ADD 07/06/93
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   MOVE      EO(1)         KEY(1)
     C                   MOVE      EO(14)        KEY(2)
     C                   ENDSR
     C**************************************************
     C*  LOAD FROM BEGINNING OF FILE  - LOADB          *
     C**************************************************
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANKS       XXEOB#
     C     ARKEY2        SETLL     PAR01                                        INV/EOB
     C     ARKEY1        READE     PAR01                                  60    INV
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C*
     C*** BEGIN ADD *** 06/12/97 **************************************
     C*  IF STATIS IS EQUAL TO 'O', AMOUNT HAS BEEN PREVIOUSLY REMOVED
     C*  BUT THE EOMB HAS NOT BEEN DELETED. INV# STILL CONTAINS THE
     C*  INVOICE NUMBER.
     C*
     C     *IN87         DOUEQ     '1'
     C*
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDNE     'B'
     C                   ADD       ARAMT         EOBAMT
     C                   END
     C                   Z-ADD     AREOB#        XXEOB#
     C*
     C     ARSTAT        IFEQ      'B'
     C                   LEAVE
     C                   ENDIF
     C     ARKEY2        READE     PAR01                                  87
     C                   ENDDO
     C*** END ADD ***** 06/12/97 **************************************
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDEQ     'B'
     C     ARTRTP        OREQ      'I'                                          ADD 06/12/97
     C     ARSTAT        ANDEQ     'D'                                          ADD 06/12/97
     C                   Z-ADD     AREOB#        XXEOB#
     C*** BEGIN CHG *** 12/09/98 **************************************
     C     SCCASE        IFEQ      *ZEROS
     C*
     C     EOKEY2        CHAIN     PEO01                              65
     C*
     C                   ELSE
     C*
     C     EOKEY4        CHAIN     PEO02                              65
     C*
     C                   ENDIF
     C*** END   CHG *** 12/09/98 **************************************
     C     *IN65         IFEQ      '0'
     C                   ADD       1             I1
     C     I1            ADD       30            Z1
     C                   EXSR      FILL
     C                   END                                                    ENDEQ *IN65
     C                   END                                                    ENDEQ 'I'
     C*
     C     ARKEY2        SETGT     PAR01                                        ADD 06/12/97
     C     ARKEY1        READE     PAR01                                  60    INV
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
     C******************************************************
     C*  UPDATE FWEOP FILE W/ DELETE FLAG (EODIFL) - RITE  *
     C******************************************************
     CSR   RITE          BEGSR
     C*
     C*  ONLY PERFORM THIS SUBROUTINE IF THIS IS NOT A NETWORK INVOICE
     C     ARSYS         IFNE      'N'
     C*
     C* VALIDATE OPTION 4 OR BLANKS
     C*
     C                   DO        14            Y                 5 0
     C     Y             ADD       44            Y1                2 0
     C     FL(Y)         IFNE      *BLANK
     C     FL(Y)         ANDNE     '4'
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C********** EWKEY     SETLLPEW01
     C*
     C                   DO        14            V                 2 0
     C     EO(V)         IFGT      *ZERO
     C                   Z-ADD     EO(V)         XXEOB#
     C     EWKY1         CHAIN     PEW01                              70        INV/EOB
     C     *IN70         IFEQ      '0'
     C     EO(V)         ANDEQ     EOEOB#
     C*** ADD NEXT LINE *** CONNIE/A9C *** 9/12/89
     C                   Z-ADD     AM(V)         EOBAMT
     C                   MOVEL     FL(V)         EODIFL
     C                   UPDATE    PEW01
     C                   END                                                    ENDEQ *IN70
     C                   END                                                    ENDGT *ZERO
     C                   END                                                    END DO
     C                   END                                                    END NE 'N'
     CSR                 ENDSR
     C*
     C*** BEGIN OF ADD *** CONNIE/A9C *** 9/12/89
     C*
     C*** BEGIN DEL *** 03/29/93 **************************************
     C*****************************************************************
     C********** DELETE    BEGSR
     C*****************************************************************
     C**********
     C*** FLAG THE WORK RECORD AS DELETED, F7124R WILL DELETE NATRP REC
     C**********
     C********** ENKEY     CHAINPEN01                70
     C********** *IN70     IFEQ '0'
     C**********           MOVE 'D'       EODIFL
     C**********           UPDATPEN01
     C**********           END
     C**********
     C**********           ENDSR
     C*** END DEL *** 03/29/93 ****************************************
     C*** BEGIN DEL *** 03/29/93 **************************************
     C*****************************************************************
     C********** NETWRK    BEGSR
     C*****************************************************************
     C**********
     C**********           MOVE CMSS#     SCSS#            SS#
     C********** CDDTOB    MULT 100.0001  SCDOB            BIRTH DATE
     C********** CMACDT    MULT 100.0001  SCACDT           ACC DATE
     C********** ARTRDT    MULT 100.0001  SCDATE
     C**********
     C**********           Z-ADD*ZEROS    SCAMTC
     C**********           Z-ADD*ZEROS    SCDISC
     C**********           Z-ADD*ZEROS    SCIFEE
     C**********
     C*** GET EOB RECORD - ONLY 1 NETWORK EOB PER INVOICE
     C**********
     C********** EOKEY3    CHAINPEO03                69
     C********** *IN69     IFEQ '0'
     C**********           MOVE EOFEIN    SCFEIN           PROVIDER
     C**********           MOVE EOMOD     SCMOD            PROVIDER MOD
     C********** EOFRDT    MULT 100.0001  SCFRDT           FROM DATE
     C********** EOTODT    MULT 100.0001  SCTODT           TO DATE
     C**********
     C**********           Z-ADDEOAMTC    SCAMTC           TOT CHARGES
     C**********           Z-ADDEONDIS    SCDISC           NET DISCOUNT
     C**********           ELSE
     C**********           MOVE *BLANKS   SCFEIN
     C**********           MOVE *BLANKS   SCMOD
     C**********           MOVE *ZEROS    SCFRDT
     C**********           MOVE *ZEROS    SCTODT
     C**********           END
     C**********
     C*** NETWORK BILLING AMT
     C**********
     C********** TRKEY     CHAINPTR01                70
     C********** *IN70     IFEQ '0'
     C**********           Z-ADDTRBAMT    SCIFEE           REVIEWR FEE
     C**********           EXCPTRLSTR
     C**********           ELSE
     C**********           MOVE '1'       *IN21            INVALID INV#
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END
     C**********
     C**********           MOVELSRLNAM    SCCNAM           CLAIMANT NAM
     C**********
     C*** GET PROVIDER NAME
     C**********
     C********** PDKEY     CHAINPPD01                67
     C********** *IN67     IFEQ '0'
     C**********           MOVELPDNAME    SCPRNM           PROV NAME
     C**********           ELSE
     C**********           MOVE *BLANKS   SCPRNM
     C**********           END
     C**********
     C*** GET SUBSCRIBER NAME
     C**********
     C********** SBKEY     CHAINPSM01                68
     C********** *IN68     IFEQ '0'
     C**********           MOVELSMNAME    SCSBNM           SUB NAME
     C**********           ELSE
     C**********           MOVE *BLANKS   SCSBNM
     C**********           END
     C**********
     C**********           ENDSR
     C**********
     C*** END DEL *** 03/29/93 ****************************************
     C*** BEGIN ADD *** 03/29/93 **************************************
     C**************************************************
     C*  FILL SCREEN ARRAYS - FILLN - NETWORK ONLY     *
     C**************************************************
     C     FILLN         BEGSR
     C                   MOVE      EODIFL        FL(I1)
     C                   MOVE      EOEOB#        EO(I1)
     C                   Z-ADD     EOTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C                   MOVEL     ARDESC        DS(I1)
     C*
     C     TRKEY         CHAIN     PTR01                              70
     C     *IN70         IFEQ      '0'
     C                   MOVE      TRSYS         SY(I1)
     C                   Z-ADD     TRBAMT        AM(I1)                         REVIEWR FEE
     C                   END
     C*** BEGIN ADD *** 07/06/93 **************************************
     C     ENKY1         CHAIN     PEN01                              60
     C     *IN60         IFEQ      '0'
     C     EODIFL        IFNE      FL(I1)
     C                   MOVEL     EODIFL        FL(I1)
     C                   END
     C     EOBAMT        IFNE      *ZEROS
     C     EOBAMT        ANDNE     AM(I1)
     C                   Z-ADD     EOBAMT        AM(I1)
     C                   END
     C                   END
     C*** END ADD *** 07/06/93 ****************************************
     C*
     C                   MOVE      '1'           *IN(Z1)                        COLM SEPARATORS
     C                   ENDSR
     C**************************************************
     C*  LOAD FORWARD - LOADN - NETWORK ONLY           *
     C**************************************************
     C     LOADN         BEGSR
     C                   EXSR      CLR
     C     OVERN         TAG
     C*
     C*  END OF FILE
     C*
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONEN
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C*** BEGIN ADD *** 06/12/97 **************************************
     C*  IF STATIS IS EQUAL TO 'O', AMOUNT HAS BEEN PREVIOUSLY REMOVED
     C*  BUT THE EOMB HAS NOT BEEN DELETED. INV# STILL CONTAINS THE
     C*  INVOICE NUMBER.
     C*
     C     *IN87         DOUEQ     '1'
     C*
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDNE     'B'
     C                   ADD       ARAMT         EOBAMT
     C                   END
     C                   Z-ADD     AREOB#        XXEOB#
     C*
     C     ARSTAT        IFEQ      'B'
     C                   LEAVE
     C                   ENDIF
     C     ARKEY2        READE     PAR01                                  87
     C                   ENDDO
     C*** END ADD ***** 06/12/97 **************************************
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDEQ     'B'
     C     ARTRTP        OREQ      'I'                                          ADD 06/12/97
     C     ARSTAT        ANDEQ     'D'                                          ADD 06/12/97
     C                   Z-ADD     AREOB#        XXEOB#
     C     EOKEY3        CHAIN     PEO03                              65
     C     *IN65         IFEQ      '0'
     C                   Z-ADD     ARINV#        EOINV#
     C*** BEGIN ADD *** 06/12/97 **************************************
     C     ARSTAT        IFEQ      'D'
     C                   Z-ADD     XXAMT         EOBAMT
     C                   ELSE
     C                   Z-ADD     ARAMT         EOBAMT                         ADD 07/14/93
     C                   ENDIF
     C*** END ADD ***** 06/12/97 **************************************
     C                   MOVE      *BLANKS       ENFLG1                         ADD 03/20/96
     C                   WRITE     PEN01
     C                   ADD       1             I1
     C     I1            ADD       30            Z1
     C                   EXSR      FILLN
     C                   END                                                    ENDEQ *IN65
     C                   END                                                    ENDEQ 'I'
     C*
     C     ARKEY2        SETGT     PAR01                                        ADD 06/12/97
     C     ARKEY1        READE     PAR01                                  60    INV
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEN
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVERN
     C     DONEN         TAG
     C                   MOVE      EO(1)         KEY(1)
     C                   MOVE      EO(14)        KEY(2)
     C                   ENDSR
     C**************************************************
     C*  LOAD BACKWARD - LOADNP - NETWORK ONLY         *
     C**************************************************
     C     LOADNP        BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     15            I1
     C     OVERNP        TAG
     C*
     C*  BEGINNING OF FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   EXSR      LOADNB
     C                   GOTO      DONENP
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C*** BEGIN ADD *** 06/12/97 **************************************
     C*  IF STATIS IS EQUAL TO 'O', AMOUNT HAS BEEN PREVIOUSLY REMOVED
     C*  BUT THE EOMB HAS NOT BEEN DELETED. INV# STILL CONTAINS THE
     C*  INVOICE NUMBER.
     C*
     C     *IN87         DOUEQ     '1'
     C*
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDNE     'B'
     C                   ADD       ARAMT         EOBAMT
     C                   END
     C                   Z-ADD     AREOB#        XXEOB#
     C*
     C     ARSTAT        IFEQ      'B'
     C                   LEAVE
     C                   ENDIF
     C     ARKEY2        READE     PAR01                                  87
     C                   ENDDO
     C*** END ADD ***** 06/12/97 **************************************
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDEQ     'B'
     C     ARTRTP        OREQ      'I'                                          ADD 06/12/97
     C     ARSTAT        ANDEQ     'D'                                          ADD 06/12/97
     C                   Z-ADD     AREOB#        XXEOB#
     C     EOKEY3        CHAIN     PEO03                              65
     C     *IN65         IFEQ      '0'
     C                   SUB       1             I1
     C     I1            ADD       30            Z1
     C                   EXSR      FILLN
     C                   END                                                    ENDEQ *IN65
     C                   END                                                    ENDEQ 'I'
     C*
     C                   READP     PAR01                                  60
     C  N60ARCO#         COMP      UPCO#                              6060
     C  N60ARSUB#        COMP      SCSUB#                             6060
     C  N60ARFND         COMP      SCFND                              6060
     C  N60ARFYR         COMP      XXFYR                              6060
     C  N60ARCASE        COMP      SCCASE                             6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERNP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERNP
     C     DONENP        TAG
     C                   MOVE      EO(1)         KEY(1)
     C                   MOVE      EO(14)        KEY(2)
     C                   ENDSR
     C**************************************************
     C*  LOAD FROM BEGINNING OF FILE  - LOADNB         *
     C**************************************************
     C     LOADNB        BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANKS       XXEOB#
     C     ARKEY2        SETLL     PAR01                                        INV/EOB
     C     ARKEY1        READE     PAR01                                  60    INV
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONENB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERNB        TAG
     C*
     C*** BEGIN ADD *** 06/12/97 **************************************
     C*  IF STATIS IS EQUAL TO 'O', AMOUNT HAS BEEN PREVIOUSLY REMOVED
     C*  BUT THE EOMB HAS NOT BEEN DELETED. INV# STILL CONTAINS THE
     C*  INVOICE NUMBER.
     C*
     C     *IN87         DOUEQ     '1'
     C*
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDNE     'B'
     C                   ADD       ARAMT         EOBAMT
     C                   END
     C                   Z-ADD     AREOB#        XXEOB#
     C*
     C     ARSTAT        IFEQ      'B'
     C                   LEAVE
     C                   ENDIF
     C     ARKEY2        READE     PAR01                                  87
     C                   ENDDO
     C*** END ADD ***** 06/12/97 **************************************
     C     ARTRTP        IFEQ      'I'
     C     ARSTAT        ANDEQ     'B'
     C     ARTRTP        OREQ      'I'                                          ADD 06/12/97
     C     ARSTAT        ANDEQ     'D'                                          ADD 06/12/97
     C                   Z-ADD     AREOB#        XXEOB#
     C     EOKEY3        CHAIN     PEO03                              65
     C     *IN65         IFEQ      '0'
     C                   ADD       1             I1
     C     I1            ADD       30            Z1
     C                   EXSR      FILLN
     C                   END                                                    ENDEQ *IN65
     C                   END                                                    ENDEQ 'I'
     C*
     C     ARKEY2        SETGT     PAR01                                        ADD 06/12/97
     C     ARKEY1        READE     PAR01                                  60    INV
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONENB
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVERNB
     C     DONENB        ENDSR
     C*****************************************************************
**
ROLL UP
ROLL DOWN
