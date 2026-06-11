     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N0160R - CASH POSTING (WITH CASE PROMPT)
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  08/15/00 C:62  PROJECT: MC215
     H*                 REPLACE SFBPF REFERENCES WITH APIS.
     H*
     H*  01/07/99 C:94  PROJECT: MC152
     H*                 CHANGE INV# FROM 5 TO 9
     H*
     H*  01/04/99 C:31  Project: MC152
     H*                 1. Change size of invoice number
     H*                                                               *
     H*  10/29/98 C:62  REQ/PROJ: MC098 14743                         *
     H*                 1) IF TRANSACTION CODE IS 'MI' NO EOMB.       *
     H*                 2) ADD USER PARM SEARCH TO CHANGE SUB#.       *
     H*                 3) CHANGE CASE SEARCH TO RETURN GROUP.        *
     H*                 4) EXPAND FYR FIELDS TO 3.0.                  *
     H*                                                               *
     H*  07/05/95 C:35  REQ/PROJ: NW008                               *
     H*                 1. ADDED CODE FOR M-MEDICAL MANAGEMENT SYSTEM *
     H*                 TYPE                                          *
     H*                                                               *
     H*  12/11/95 C:20  REQ/PROJ: NW008                               *
     H*                 1. ADD EOMB# TO SCREEN.                       *
     H*                 1. ADD EOMB# TO SCREEN.                       *
     H*                                                               *
     H*  02/25/98 C:56  Project: MCMNT                                *
     H*                 1. Change FAEOP logical to FAEOL3 for EOMB#   *
     H*                    validation                                 *
     H*                                                               *
     H*  05/07/98 C:91  Project: MC181                                *
     H*                 COMBINED F6, F7, F8, F9, AND F15 KEYS INTO    *
     H*                  ONE F10 SEARCH KEY                           *
     H*                 DELETED CHG USER PARMS                        *
     H*                                                               *
     F*****************************************************************
     F*                                                               *
     F* INDICATOR USAGE:                                              *
     F*    30    = INVALID CODE - NOT IN TABLE                        *
     F*    31    = INVALID CODE - CANNOT BE 'P'                       *
     F*    32    = INVALID SYSTEM ID - MUST BE P, H, N, OR U          *
     F*    33    = INVALID CASE                                       *
     F*    34    = INVALID INVOICE #                                  *
     F*    35    = INVALID DATE                                       *
     F*    36    = INVALID AMOUNT                                     *
     F*    37    = INVALID SYSTEM ID - NOT VALID FOR CASE/INVOICE #   *
     F*    38    = ON MI TRANSACTION CODES INVOICE # MUST BE BLANK    *
     F*    39    = ON MI TRANS CODES A POSTING DATE MUST BE GIVEN     *
     F*    39    = INVALID EONB#                                      *
     F*    70-87 = POSITION CURSOR                                    *
     F*    88    = MC40004-INVALID FIELD FOR SEARCH                   *
     F*    96    = OFF - GROUP FIELD NOT NUMERIC                      *
     F*    97    = OFF - YEAR FIELD NOT NUMERIC                       *
     F*
     F*****************************************************************
     F*
     FN0160F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
      **** BEGIN DEL **** 05/07/98 ************************************
     F*****FFMCMLA  IF  E           K        DISK
     F*****F            PCM01                             KRENAMEPCM02
      **** END   DEL **** 05/07/98 ************************************
     FNMARL5    IF   E           K DISK
     F                                     RENAME(PAR01:PAR02)
     FFTABL     IF   E           K DISK
     FNMARP     O    E           K DISK
     FFMSML     IF   E           K DISK
     FFMC1L     UF A E           K DISK
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F*** END   DEL *** 08/15/00 **************************************
      **** BEGIN DEL **** 05/07/98 ************************************
     F********* FFMFNL   IF  E           K        DISK
     F********* FFMF1L   IF  E           K        DISK
      **** END   DEL **** 05/07/98 ************************************
     F*** BEGIN ADD *** 12/11/95 ***********************************************
     F*** BEGIN CHG *** 02/25/98 **************************************
     F********* FAEOL4  IF  E           K        DISK
     FFAEOL3    IF   E           K DISK
     F*** END CHG *** 02/25/98 ****************************************
     FNMARL6    IF   E           K DISK
     F                                     RENAME(PAR01:PAR03)
     F*** END ADD *** 12/11/95 *************************************************
     F*
      **** BEGIN ADD **** 05/07/98 ************************************
     FFTGTP     IF   E             DISK
     FFMFNP     IF   E             DISK
     FFTACP     IF   E             DISK
     F                                     RENAME(PAC01:PAC02)
      **** END   ADD **** 05/07/98 ************************************
     F*

     D*===============================================================*
     D*     EXTENSION SECTION                                         *
     D*===============================================================*
     D*
     D* DEFINE ARRAYS
     D*
     D*** BEGIN CHG *** 10/29/98 ***********************************************
     D*************       YR         10  2  A              YEAR
     D*** END   CHG *** 10/29/98 ***********************************************
     D*** BEGIN CHG *** 12/11/95 ***********************************************
     D**********          DC         10 25  A              DESC
     D*** END CHG *** 12/11/95 *************************************************
     D*** BEG CHG *** 01/07/99 ***************************************
     D**********          IN         10  7 0A              INVOICE #
     D*** END CHG *** 01/07/99 ***************************************
     D*** BEGIN ADD *** 12/11/95 ***********************************************
     D*** END ADD *** 12/11/95 *************************************************
     D PT              S              1    DIM(10) ASCEND
     D*** BEGIN DEL *** 08/15/00 **************************************
     D**********SRC01
     D**********    RCKEY                           RCKEY1
     D**********SCS01
     D**********    CSKEY                           CSKEYW
     D*** END   DEL *** 08/15/00 **************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  TC                     1     20
     D                                     DIM(10) ASCEND                         TRANS CODE
     D  SCTC01                 1      2
     D  SCTC02                 3      4
     D  SCTC03                 5      6
     D  SCTC04                 7      8
     D  SCTC05                 9     10
     D  SCTC06                11     12
     D  SCTC07                13     14
     D  SCTC08                15     16
     D  SCTC09                17     18
     D  SCTC10                19     20
     D                 DS
     D  SY                     1     10
     D                                     DIM(10) ASCEND                         SYS ID
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
     D                 DS
     D  GR                     1     30
     D                                     DIM(10) ASCEND                         GROUP
     D  SCGR01                 1      3
     D  SCGR02                 4      6
     D  SCGR03                 7      9
     D  SCGR04                10     12
     D  SCGR05                13     15
     D  SCGR06                16     18
     D  SCGR07                19     21
     D  SCGR08                22     24
     D  SCGR09                25     27
     D  SCGR10                28     30
     D                 DS
     D*** BEGIN CHG *** 10/29/98 **************************************
     D*********                               1  20 YR
     D*********                               1   2 SCYR01
     D*********                               3   4 SCYR02
     D*********                               5   6 SCYR03
     D*********                               7   8 SCYR04
     D*********                               9  10 SCYR05
     D*********                              11  12 SCYR06
     D*********                              13  14 SCYR07
     D*********                              15  16 SCYR08
     D*********                              17  18 SCYR09
     D*********                              19  20 SCYR10
     D  YR                     1     30  0
     D                                     DIM(10)                                YEAR
     D  SCYR01                 1      3  0
     D  SCYR02                 4      6  0
     D  SCYR03                 7      9  0
     D  SCYR04                10     12  0
     D  SCYR05                13     15  0
     D  SCYR06                16     18  0
     D  SCYR07                19     21  0
     D  SCYR08                22     24  0
     D  SCYR09                25     27  0
     D  SCYR10                28     30  0
     D*** END   CHG *** 10/29/98 **************************************
     D                 DS
     D  CS                     1     70  0
     D                                     DIM(10) ASCEND                         CASE
     D  SCCS01                 1      7  0
     D  SCCS02                 8     14  0
     D  SCCS03                15     21  0
     D  SCCS04                22     28  0
     D  SCCS05                29     35  0
     D  SCCS06                36     42  0
     D  SCCS07                43     49  0
     D  SCCS08                50     56  0
     D  SCCS09                57     63  0
     D  SCCS10                64     70  0
     D*** BEG CHG *** 01/07/99 ***************************************
     D                 DS
     D  IN                     1     90  0
     D                                     DIM(10) ASCEND                         INVOICE #
     D  SCIN01                 1      9  0
     D  SCIN02                10     18  0
     D  SCIN03                19     27  0
     D  SCIN04                28     36  0
     D  SCIN05                37     45  0
     D  SCIN06                46     54  0
     D  SCIN07                55     63  0
     D  SCIN08                64     72  0
     D  SCIN09                73     81  0
     D  SCIN10                82     90  0
     D**********  DS
     D**********                              1  70 IN
     D**********                              1   70SCIN01
     D**********                              8  140SCIN02
     D**********                             15  210SCIN03
     D**********                             22  280SCIN04
     D**********                             29  350SCIN05
     D**********                             36  420SCIN06
     D**********                             43  490SCIN07
     D**********                             50  560SCIN08
     D**********                             57  630SCIN09
     D**********                             64  700SCIN10
     D*** END CHG *** 01/07/99 ***************************************
     D*** BEGIN ADD **8 12/11/95 ***********************************************
     D                 DS
     D  EO                     1     90  0
     D                                     DIM(10) ASCEND                         EOB#
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
     D*** END ADD *** 12/11/95 *************************************************
     D                 DS
     D  PD                     1    080  0
     D                                     DIM(10) ASCEND                         POST DATE
     D  SCPD01               001    008  0
     D  SCPD02               009    016  0
     D  SCPD03               017    024  0
     D  SCPD04               025    032  0
     D  SCPD05               033    040  0
     D  SCPD06               041    048  0
     D  SCPD07               049    056  0
     D  SCPD08               057    064  0
     D  SCPD09               065    072  0
     D  SCPD10               073    080  0
     D*** BEGIN CHG *** 12/11/95 ***********************************************
     D**********  DS
     D**********                              1 250 DC
     D**********                              1  25 SCDC01
     D**********                             26  50 SCDC02
     D**********                             51  75 SCDC03
     D**********                             76 100 SCDC04
     D**********                            101 125 SCDC05
     D**********                            126 150 SCDC06
     D**********                            151 175 SCDC07
     D**********                            176 200 SCDC08
     D**********                            201 225 SCDC09
     D**********                            226 250 SCDC10
     D                 DS
     D  DC                     1    150
     D                                     DIM(10) ASCEND                         DESC
     D  SCDC01                 1     15
     D  SCDC02                16     30
     D  SCDC03                31     45
     D  SCDC04                46     60
     D  SCDC05                61     75
     D  SCDC06                76     90
     D  SCDC07                91    105
     D  SCDC08               106    120
     D  SCDC09               121    135
     D  SCDC10               136    150
     D*** END CHG *** 12/11/95 *************************************************
     D                 DS
     D  AM                     1     90  2
     D                                     DIM(10) ASCEND                         AMOUNT
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
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCSUB#                 4      8  0
     D  QCFND                  9     11  0
     D  QCFYR                 12     14  0
     D  QCCASE                15     21  0
     D                 DS
     D  QWKEY                  1     40
     D  QWCO#                  1      3  0
     D  QWFND                  4      6  0
     D  QWFYR                  7      9  0
     D  QWCASE                10     16  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   COMPANY FILE  (SMCOL)
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C*
     C*  USER PARAMETER FILE  (FMUPL)
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  TABLE FILE (FTABL)
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  CLAIM FILE (FMCML1)
     C*
     C     CMKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    XXCASE            7 0
     C*
     C*  A/R FILE (NMARL5)
     C*
     C     ARKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    XXCASE            7 0
     C*** BEG CHG *** 01/07/99 ***************************************
     C                   KFLD                    XXINV#            9 0
     C**********           KFLD           XXINV#  50
     C*** END CHG *** 01/07/99 ***************************************
     C*
     C     ARKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    XXCASE            7 0
     C*** BEG CHG *** XX/XX/XX ***************************************
     C                   KFLD                    XXINV#            9 0
     C**********           KFLD           XXINV#  50
     C*** END CHG *** XX/XX/XX ***************************************
     C                   KFLD                    XXSYS             1
     C*** BEGIN ADD *** 12/11/95 ***********************************************
     C*
     C     ARKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXINV#
     C                   KFLD                    XXEOB#
     C*
     C* EOMB MASTER FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C*** BEGIN DEL *** 02/25/98 **************************************
     C**********           KFLD           XXFND
     C**********           KFLD           XXFYR
     C**********           KFLD           XXCASE
     C*** END DEL *** 02/25/98 ****************************************
     C                   KFLD                    XXEOB#
     C*** END ADD *** 12/11/95 *************************************************
     C*
     C*  CLAIM FILE (FMCMLA)
     C*
     C     CMKEY2        KLIST
     C                   KFLD                    XXSS#             9
     C                   KFLD                    XXACDT            7 0
     C*
     C*   KEY LIST FOR THE COMPANY AUXILIARY FILE
     C*
     C     C1KEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  FS BASE CONTROL FILE
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** FBKY      KLIST
     C**********           KFLD           XSKEY   2
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB
     C                   KFLD                    XXFND
     C*
     C*  FUND AUXILIARY FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB
     C                   KFLD                    XXFND
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C     *LIKE         DEFINE    FNWCF#        XXWCF#                         9A
     C     *LIKE         DEFINE    EOEOB#        XXEOB#                         ADD 12/11/95
     C*** BEGIN ADD *** 08/15/00 **************************************
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C*** END   ADD *** 08/15/00 **************************************
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           MOVE *BLANKS   XSKEY
     C**********           MOVEL'FB'      XSKEY
     C********** FBKY      CHAINSFB01                60
     C********** *IN60     IFEQ '0'
     C*** END   DEL *** 08/15/00 **************************************
     C*** BEGIN CHG *** 08/15/00 **************************************
     C********** FBFL01    IFEQ 'Y'
     C     ##WCIN        IFEQ      'Y'
     C*** END   CHG *** 08/15/00 **************************************
     C                   Z-ADD     4             X
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** 1         SUBSTFBAL01:X  XDKEY   1    65
     C*** END   DEL *** 08/15/00 **************************************
     C*** BEGIN CHG *** 08/15/00 **************************************
     C********** XDKEY     IFEQ 'Y'
     C     ##WCDK        IFEQ      'Y'
     C*** END   CHG *** 08/15/00 **************************************
     C                   MOVE      'Y'           XWCKEY            1
     C                   ELSE
     C                   MOVE      'N'           XWCKEY
     C                   END                                                    END XDKEY
     C                   ELSE                                                   ELSE FBFL01 = 'Y'
     C                   MOVE      'N'           XWCKEY
     C                   END                                                    END FBFL01
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           END                             END *IN60
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C*
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   END
     C*
     C*  GET COMPANY NAME
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SCCONM
     C                   ELSE
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   END
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*
     C* GET SUBSCRIBER NAME
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         SCSUB
     C                   MOVE      *BLANK        SCSBNM
     C                   ELSE
     C                   MOVEL     UPSUB#        SCSUB
     C                   MOVEL     SMNAME        SCSBNM
     C                   END
     C                   TIME                    DSDTTM
      **** BEGIN DEL **** 05/07/98 ************************************
     C**********           MOVE Q1USER    SCUSER
      **** END   DEL **** 05/07/98 ************************************
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL     *BLANKS       TC
     C                   MOVEL     *BLANKS       SY
     C                   MOVEL     *BLANKS       GR
     C*** BEGIN CHG *** 10/29/98 **************************************
     C**********           MOVEL*BLANKS   YR
     C                   MOVE      *ZEROS        YR
     C*** END   CHG *** 10/29/98 **************************************
     C                   MOVEL     *BLANKS       CS
     C                   MOVE      *BLANKS       IN
     C                   Z-ADD     *ZEROS        PD
     C                   MOVEL     *BLANKS       DC
     C                   Z-ADD     *ZEROS        AM
     C                   Z-ADD     *ZEROS        EO                             ADD 12/11/95
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
     C***************************************************************
     C*   PROCESS ALL COMMAND KEYS                                  *
     C***************************************************************
      **** BEGIN CHG **** 05/07/98 ************************************
     C**********
     C********** *IN11     IFEQ '1'
     C**********
     C**********
     C* EXIT   *
     C**********
     C********** *INKC     IFEQ '1'
     C**********           MOVE '1'       *INLR
     C**********           GOTO TERM
     C**********           END
     C**********
     C**********
     C* RESTART*
     C**********
     C********** *INKE     IFEQ '1'
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           MOVEL*BLANKS   TC
     C**********           MOVEL*BLANKS   SY
     C**********           MOVEL*BLANKS   GR
     C**********           MOVEL*BLANKS   YR
     C**********           MOVEL*BLANKS   CS
     C**********           MOVEL*BLANKS   IN
     C**********           Z-ADD*ZEROS    PD
     C**********           MOVEL*BLANKS   DC
     C**********           Z-ADD*ZEROS    AM
     C**********           GOTO STRT
     C**********           END
     C**********
     C**********
     C* GROUP  *
     C**********
     C********** *INKF     IFEQ '1'
     C**********
     C**********           Z-ADD1         L       20
     C**********           DO   10        L
     C********** GR,L      IFEQ '?  '
     C********** GR,L      OREQ '  ?'
     C**********
     C**********           CALL 'F0506R'
     C**********           PARM           QQPRDS 40
     C**********           PARM           QQCALL  1
     C**********           PARM           XXFND   30
     C**********           PARM           QQRTCD  1
     C**********
     C********** QQRTCD    CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTCD    IFEQ 'B'
     C********** XXFND     OREQ *ZERO
     C**********           GOTO STRT
     C**********           END
     C**********
     C********** QQRTCD    IFEQ '*'
     C**********           MOVE XXFND     GR,L
     C**********           GOTO STRT
     C**********           END
     C**********
     C**********           END                             ENDIF '? '
     C**********
     C**********           END                             END DO
     C**********
     C**********           END                             ENDIF *INKE
     C**********
     C* F7     *
     C**********
     C********** *INKG     IFEQ '1'
     C**********
     C**********           Z-ADD1         L       20
     C**********           DO   10        L
     C********** GR,L      IFEQ '?  '
     C********** GR,L      OREQ '  ?'
     C********** YR,L      OREQ '? '
     C********** YR,L      OREQ ' ?'
     C**********
     C**********
     C**********           MOVE *BLANK    QQSS#   9
     C**********           Z-ADD*ZEROS    QQACDT  80
     C**********           MOVE *ZEROS    Q1FND   30
     C**********           MOVE *ZEROS    Q1FYR   30
     C**********           MOVE *ZEROS    XXCASE
     C**********           CALL 'F0578R1'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQSS#
     C**********           PARM           QQACDT
     C**********           PARM           SCSUB
     C**********           PARM           Q1FND
     C**********           PARM           Q1FYR
     C**********           PARM           XXCASE
     C**********           PARM           QQRTCD  1
     C**********           MOVE Q1FYR     YR,L
     C**********           MOVE Q1FND     GR,L
     C**********           MOVE XXCASE    CS,L
     C********** QQRTCD    CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTCD    IFEQ 'B'
     C********** YR,L      OREQ *ZEROS
     C**********           GOTO STRT
     C**********           END                             ENDEQ 'B'
     C**********
     C********** QQRTCD    IFEQ '*'
     C**********           MOVE UPCO#     QQCO#   3
     C**********           MOVE SCSUB     QQSUB#  5
     C**********           MOVE GR,L      QQFND   3
     C**********           CALL 'F0006R2'
     C**********           PARM           QQCO#
     C**********           PARM           QQSUB#
     C**********           PARM           QQFND
     C**********           PARM           QQRTCD
     C**********           GOTO STRT
     C**********           END
     C**********
     C**********
     C**********           END                             ENDIF '? '
     C**********
     C**********           END                             END DO
     C**********
     C**********           END                             ENDIF *INKG
     C**********
     C*   F8   *
     C**********
     C********** *INKH     IFEQ '1'
     C**********
     C**********           Z-ADD1         L       20
     C**********           DO   10        L
     C********** GR,L      IFEQ '?  '
     C********** GR,L      OREQ '  ?'
     C********** YR,L      OREQ '? '
     C********** YR,L      OREQ ' ?'
     C**********
     C**********           CALL 'F0570R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           XXYR    20
     C**********           PARM           XXCASE  70
     C**********           PARM           XXSS#   9
     C**********           PARM           QQRTCD
     C**********           PARM           Q1FND   30
     C**********
     C********** QQRTCD    CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTCD    IFEQ 'B'
     C********** XXYR      OREQ *ZERO
     C**********           GOTO STRT
     C**********           END
     C**********
     C********** QQRTCD    IFEQ '*'
     C**********           MOVE XXYR      YR,L
     C**********           MOVE XXCASE    CS,L
     C**********           MOVE Q1FND     GR,L
     C**********           GOTO STRT
     C**********           END
     C**********
     C**********           END                             ENDIF '? '
     C**********
     C**********           END                             END DO
     C**********
     C**********           END                             ENDIF *INKF
     C**********
     C*   F9   *
     C**********
     C********** *INKI     IFEQ '1'
     C**********
     C**********           Z-ADD1         L       20
     C**********           DO   10        L
     C********** GR,L      IFEQ '?  '
     C********** GR,L      OREQ '  ?'
     C********** YR,L      OREQ '? '
     C********** YR,L      OREQ ' ?'
     C**********
     C**********           MOVE *BLANKS   XXSS#
     C**********           Z-ADD*ZEROS    XXACDT
     C**********           CALL 'F0573R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           XXSS#   9
     C**********           PARM           XXACDT
     C**********           PARM           QQRTCD  1
     C**********
     C********** QQRTCD    CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTCD    IFEQ 'B'
     C********** XXSS#     OREQ *BLANK
     C**********           GOTO STRT
     C**********           END
     C**********
     C********** QQRTCD    IFEQ '*'
     C**********           Z-ADDXXACDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXACDT
     C********** CMKEY2    CHAINPCM02                60
     C********** *IN60     IFEQ '0'
     C**********           MOVE CMFND     GR,L
     C**********           MOVE CMFYR     YR,L
     C**********           MOVE CMCASE    CS,L
     C**********           GOTO STRT
     C**********           END
     C**********
     C**********           END                             ENDIF '*'
     C**********
     C**********           END                             ENDIF '? '
     C**********
     C**********           END                             ENDDO
     C**********
     C**********           END                             ENDIF *INKI
     C**********
     C*   10   *
     C**********
     C********** *INKJ     IFEQ '1'
     C**********           DO   10        T       50
     C**********
     C**********           MOVELTC,T      XX2     2
     C**********
     C********** XX2       IFEQ '? '
     C**********
     C**********           MOVE *BLANK    QQKEY  10
     C**********           CALL 'F2181R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQKEY
     C**********           PARM           QQRTCD  1
     C**********           MOVELQQKEY     TC,T
     C********** QQRTCD    CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END
     C**********
     C**********           END
     C**********
     C**********           END                             ENDIF *INKJ
     C**********
     C* PARMS  *
     C**********
     C********** *INKM     IFEQ '1'
     C**********           CALL 'F0006R'
     C**********
     C**********   GET THE USER PARAMETER FILE
     C**********
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C**********           MOVE *ZERO     UPCO#
     C**********           END
     C**********
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO TOP
     C**********           END
     C**********
     C**********
     C* DARTS  *
     C**********
     C********** *INKN     IFEQ '1'
     C**********
     C**********    RETAIN SCREEN NUMBER
     C**********           MOVE Q2RECF    HLRECF 10
     C**********
     C**********    CHECK CURSOR POSTION
     C**********
     C********** Q2CPOS    DIV  256       LIN     20
     C********** LIN       SUB  7         I1      30
     C**********
     C**********   VALID CURSOR POSITION - GOTO SCR02
     C**********
     C********** I1        IFGE 1
     C********** I1        ANDLE10
     C**********   ESTABLISH KEY
     C**********           MOVE *BLANKS   QCKEY
     C**********           MOVE *BLANKS   QWKEY
     C**********           MOVE UPCO#     QWCO#
     C**********           MOVE GR,I1     XXFND
     C********** XWCKEY    IFEQ 'Y'
     C********** F1KEY     CHAINPF101                60
     C********** *IN60     IFEQ '0'
     C********** F1OUTA    IFEQ 'N'
     C********** FNKEY     CHAINPFN01                60
     C********** *IN60     IFEQ '0'
     C**********           Z-ADDFNWCF#    QWFND
     C**********           ELSE
     C**********           Z-ADD*ZEROS    QWFND
     C**********           END
     C**********           MOVE YR,I1     XXFYR
     C**********           Z-ADDCS,I1     XXCASE
     C**********           Z-ADDXXCASE    QWCASE
     C**********           Z-ADDXXFYR     QWFYR
     C**********
     C**********   CALL DARTS ENTRY PROGRAM
     C**********           CALL 'SC010R'               99
     C**********           PARM           QWKEY
     C**********           PARM           Q1PGM
     C**********           ELSE                            ELSE F1OUTA
     C**********           Z-ADDUPCO#     QCCO#
     C**********           Z-ADDSCSUB     QCSUB#
     C**********           MOVE GR,I1     XXFND
     C**********           MOVE YR,I1     XXFYR
     C**********           Z-ADDCS,I1     XXCASE
     C**********           Z-ADDXXFND     QCFND
     C**********           Z-ADDXXFYR     QCFYR
     C**********           Z-ADDXXCASE    QCCASE
     C**********
     C**********   CALL DARTS ENTRY PROGRAM
     C**********           CALL 'SC010R'               99
     C**********           PARM           QCKEY
     C**********           PARM           Q1PGM
     C**********           END                             END F1OUTA
     C**********           END                             END *IN60
     C**********           ELSE                            END XWCKEY
     C**********
     C**********           Z-ADDUPCO#     QCCO#
     C**********           Z-ADDSCSUB     QCSUB#
     C**********           MOVE GR,I1     XXFND
     C**********           MOVE YR,I1     XXFYR
     C**********           Z-ADDCS,I1     XXCASE
     C**********           Z-ADDXXFND     QCFND
     C**********           Z-ADDXXFYR     QCFYR
     C**********           Z-ADDXXCASE    QCCASE
     C**********
     C**********  CALL DARTS ENTRY PROGRAM
     C**********           CALL 'SC010R'               99
     C**********           PARM           QCKEY
     C**********           PARM           Q1PGM
     C**********           END                             END XWCKEY
     C**********
     C**********           END
     C**********           MOVE *BLANKS   Q2RECF
     C**********           MOVE HLRECF    Q2RECF
     C**********           GOTO STRT
     C**********           END                             END *INKN
     C**********
     C*INVOICE *
     C**********
     C********** *INKP     IFEQ '1'
     C**********
     C**********           Z-ADD1         L       20
     C**********           DO   10        L
     C**********
     C**********           CALL 'N0720R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQFND   3
     C**********           PARM           QQFYR   3
     C**********           PARM           QQCASE  7
     C**********           PARM           QQINV#  5
     C**********           PARM           QQRTCD
     C**********
     C********** QQRTCD    CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTCD    IFEQ 'B'
     C********** QQFND     OREQ *BLANKS
     C**********           GOTO STRT
     C**********           END
     C**********
     C********** QQRTCD    IFEQ '*'
     C**********           MOVE QQFND     GR,L
     C**********           MOVE QQFYR     YR,L
     C**********           MOVE QQCASE    CS,L
     C**********           MOVE QQINV#    IN,L
     C**********           GOTO STRT
     C**********           END
     C**********
     C**********           END                             END DO
     C**********
     C**********           END                             ENDIF *INKP
     C**********
     C**********           END                             END CMDKEY
     C*****************************************************************
     C*
     C     *IN11         IFEQ      *ON
     C*
     C******************
     C* F3 = EXIT      *
     C******************
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC
     C*
     C******************
     C* F5 = RESTART   *
     C******************
     C     *INKE         IFEQ      *ON
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVEL     *BLANKS       TC
     C                   MOVEL     *BLANKS       SY
     C                   MOVEL     *BLANKS       GR
     C*** BEGIN CHG *** 10/29/98 **************************************
     C**********           MOVEL*BLANKS   YR
     C                   MOVE      *ZEROS        YR
     C*** END   CHG *** 10/29/98 **************************************
     C                   MOVEL     *BLANKS       CS
     C                   MOVEL     *BLANKS       IN
     C                   Z-ADD     *ZEROS        PD
     C                   MOVEL     *BLANKS       DC
     C                   Z-ADD     *ZEROS        AM
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKE
      **************************
      *    F4 = SEARCH         *
      **************************
     C*
     C     *INKJ         IFEQ      *ON
     C*
      ******************
      * TRN CD SEARCH  *
      ******************
     C     HFLD          IFEQ      'SCTC01'
     C     HFLD          OREQ      'SCTC02'
     C     HFLD          OREQ      'SCTC03'
     C     HFLD          OREQ      'SCTC04'
     C     HFLD          OREQ      'SCTC05'
     C     HFLD          OREQ      'SCTC06'
     C     HFLD          OREQ      'SCTC07'
     C     HFLD          OREQ      'SCTC08'
     C     HFLD          OREQ      'SCTC09'
     C     HFLD          OREQ      'SCTC10'
     C*
     C                   CALL      'N2305R'
     C*
     C                   PARM      '2'           ##CALL
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##RRN             9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      0
     C                   SELECT
     C     HFLD          WHENEQ    'SCTC01'
     C                   SETON                                        71  81     PC @ SCTC01
     C     HFLD          WHENEQ    'SCTC02'
     C                   SETON                                        72  81     PC @ SCTC02
     C     HFLD          WHENEQ    'SCTC03'
     C                   SETON                                        73  81     PC @ SCTC03
     C     HFLD          WHENEQ    'SCTC04'
     C                   SETON                                        74  81     PC @ SCTC04
     C     HFLD          WHENEQ    'SCTC05'
     C                   SETON                                        75  81     PC @ SCTC05
     C     HFLD          WHENEQ    'SCTC06'
     C                   SETON                                        76  81     PC @ SCTC06
     C     HFLD          WHENEQ    'SCTC07'
     C                   SETON                                        77  81     PC @ SCTC07
     C     HFLD          WHENEQ    'SCTC08'
     C                   SETON                                        78  81     PC @ SCTC08
     C     HFLD          WHENEQ    'SCTC09'
     C                   SETON                                        79  81     PC @ SCTC09
     C     HFLD          WHENEQ    'SCTC10'
     C                   SETON                                        80  81     PC @ SCTC10
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                   *INKL
     C*
     C     ##RRN         CHAIN     PAC02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   SELECT
     C     HFLD          WHENEQ    'SCTC01'
     C                   MOVEL     ACKEY         SCTC01
     C                   SETON                                        71  82     PC @ SCSY01
     C     HFLD          WHENEQ    'SCTC02'
     C                   MOVEL     ACKEY         SCTC02
     C                   SETON                                        72  82     PC @ SCSY02
     C     HFLD          WHENEQ    'SCTC03'
     C                   MOVEL     ACKEY         SCTC03
     C                   SETON                                        73  82     PC @ SCSY03
     C     HFLD          WHENEQ    'SCTC04'
     C                   MOVEL     ACKEY         SCTC04
     C                   SETON                                        74  82     PC @ SCSY04
     C     HFLD          WHENEQ    'SCTC05'
     C                   MOVEL     ACKEY         SCTC05
     C                   SETON                                        75  82     PC @ SCSY05
     C     HFLD          WHENEQ    'SCTC06'
     C                   MOVEL     ACKEY         SCTC06
     C                   SETON                                        76  82     PC @ SCSY06
     C     HFLD          WHENEQ    'SCTC07'
     C                   MOVEL     ACKEY         SCTC07
     C                   SETON                                        77  82     PC @ SCSY07
     C     HFLD          WHENEQ    'SCTC08'
     C                   MOVEL     ACKEY         SCTC08
     C                   SETON                                        78  82     PC @ SCSY08
     C     HFLD          WHENEQ    'SCTC09'
     C                   MOVEL     ACKEY         SCTC09
     C                   SETON                                        79  82     PC @ SCSY09
     C     HFLD          WHENEQ    'SCTC10'
     C                   MOVEL     ACKEY         SCTC10
     C                   SETON                                        80  82     PC @ SCSY10
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  IFEQ *IN60
     C*
     C                   ENDIF                                                  SCTC
     C*
      ******************
      * SY SEARCH      *
      ******************
     C     HFLD          IFEQ      'SCSY01'
     C     HFLD          OREQ      'SCSY02'
     C     HFLD          OREQ      'SCSY03'
     C     HFLD          OREQ      'SCSY04'
     C     HFLD          OREQ      'SCSY05'
     C     HFLD          OREQ      'SCSY06'
     C     HFLD          OREQ      'SCSY07'
     C     HFLD          OREQ      'SCSY08'
     C     HFLD          OREQ      'SCSY09'
     C     HFLD          OREQ      'SCSY10'
     C*
     C                   CALL      'F2380R'
     C*
     C                   PARM      '2'           ##CALL
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##RRN2            9 0
     C                   PARM      'SYS'         ##TBID            4
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN2        OREQ      0
     C                   SELECT
     C     HFLD          WHENEQ    'SCSY01'
     C                   SETON                                        71  82     PC @ SCSY01
     C     HFLD          WHENEQ    'SCSY02'
     C                   SETON                                        72  82     PC @ SCSY02
     C     HFLD          WHENEQ    'SCSY03'
     C                   SETON                                        73  82     PC @ SCSY03
     C     HFLD          WHENEQ    'SCSY04'
     C                   SETON                                        74  82     PC @ SCSY04
     C     HFLD          WHENEQ    'SCSY05'
     C                   SETON                                        75  82     PC @ SCSY05
     C     HFLD          WHENEQ    'SCSY06'
     C                   SETON                                        76  82     PC @ SCSY06
     C     HFLD          WHENEQ    'SCSY07'
     C                   SETON                                        77  82     PC @ SCSY07
     C     HFLD          WHENEQ    'SCSY08'
     C                   SETON                                        78  82     PC @ SCSY08
     C     HFLD          WHENEQ    'SCSY09'
     C                   SETON                                        79  82     PC @ SCSY09
     C     HFLD          WHENEQ    'SCSY10'
     C                   SETON                                        80  82     PC @ SCSY10
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                   *INKL
     C*
     C     ##RRN2        CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   SELECT
     C     HFLD          WHENEQ    'SCSY01'
     C                   MOVEL     GTKEY         SCSY01
     C                   SETON                                        71  83     PC @ SCGR01
     C     HFLD          WHENEQ    'SCSY02'
     C                   MOVEL     GTKEY         SCSY02
     C                   SETON                                        72  83     PC @ SCGR02
     C     HFLD          WHENEQ    'SCSY03'
     C                   MOVEL     GTKEY         SCSY03
     C                   SETON                                        73  83     PC @ SCGR03
     C     HFLD          WHENEQ    'SCSY04'
     C                   MOVEL     GTKEY         SCSY04
     C                   SETON                                        74  83     PC @ SCGR04
     C     HFLD          WHENEQ    'SCSY05'
     C                   MOVEL     GTKEY         SCSY05
     C                   SETON                                        75  83     PC @ SCGR05
     C     HFLD          WHENEQ    'SCSY06'
     C                   MOVEL     GTKEY         SCSY06
     C                   SETON                                        76  83     PC @ SCGR06
     C     HFLD          WHENEQ    'SCSY07'
     C                   MOVEL     GTKEY         SCSY07
     C                   SETON                                        77  83     PC @ SCGR07
     C     HFLD          WHENEQ    'SCSY08'
     C                   MOVEL     GTKEY         SCSY08
     C                   SETON                                        78  83     PC @ SCGR08
     C     HFLD          WHENEQ    'SCSY09'
     C                   MOVEL     GTKEY         SCSY09
     C                   SETON                                        79  83     PC @ SCGR09
     C     HFLD          WHENEQ    'SCSY10'
     C                   MOVEL     GTKEY         SCSY10
     C                   SETON                                        80  83     PC @ SCGR10
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  IFEQ *IN60
     C*
     C                   ENDIF                                                  SCSY
     C*
      ****************
      * GROUP SEARCH *
      ****************
     C     HFLD          IFEQ      'SCGR01'
     C     HFLD          OREQ      'SCGR02'
     C     HFLD          OREQ      'SCGR03'
     C     HFLD          OREQ      'SCGR04'
     C     HFLD          OREQ      'SCGR05'
     C     HFLD          OREQ      'SCGR06'
     C     HFLD          OREQ      'SCGR07'
     C     HFLD          OREQ      'SCGR08'
     C     HFLD          OREQ      'SCGR09'
     C     HFLD          OREQ      'SCGR10'
     C*
     C                   CALL      'F0060R'
     C*
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN3            9 0
     C*
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN3        OREQ      0
     C                   SELECT
     C     HFLD          WHENEQ    'SCGR01'
     C                   SETON                                        71  83     PC @ SCGR01
     C     HFLD          WHENEQ    'SCGR02'
     C                   SETON                                        72  83     PC @ SCGR02
     C     HFLD          WHENEQ    'SCGR03'
     C                   SETON                                        73  83     PC @ SCGR03
     C     HFLD          WHENEQ    'SCGR04'
     C                   SETON                                        74  83     PC @ SCGR04
     C     HFLD          WHENEQ    'SCGR05'
     C                   SETON                                        75  83     PC @ SCGR05
     C     HFLD          WHENEQ    'SCGR06'
     C                   SETON                                        76  83     PC @ SCGR06
     C     HFLD          WHENEQ    'SCGR07'
     C                   SETON                                        77  83     PC @ SCGR07
     C     HFLD          WHENEQ    'SCGR08'
     C                   SETON                                        78  83     PC @ SCGR08
     C     HFLD          WHENEQ    'SCGR09'
     C                   SETON                                        79  83     PC @ SCGR09
     C     HFLD          WHENEQ    'SCGR10'
     C                   SETON                                        80  83     PC @ SCGR10
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                   *INKL
     C*
     C     ##RRN3        CHAIN     PFN01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*** BEGIN ADD *** 11/01/98 **************************************
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C                   Z-ADD     FNSUB#        SCSUB
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C*
     C     *IN60         IFEQ      '1'
     C*
     C                   MOVE      *ZERO         SCSUB
     C                   MOVE      *BLANK        SCSBNM
     C*
     C                   ELSE
     C*
     C                   MOVEL     UPSUB#        SCSUB
     C                   MOVEL     SMNAME        SCSBNM
     C*
     C                   ENDIF
     C*** END   ADD *** 11/01/98 **************************************
     C                   SELECT
     C     HFLD          WHENEQ    'SCGR01'
     C                   MOVEL     FNFND         SCGR01
     C                   SETON                                        71  84     PC @ SCYR01
     C     HFLD          WHENEQ    'SCGR02'
     C                   MOVEL     FNFND         SCGR02
     C                   SETON                                        72  84     PC @ SCYR02
     C     HFLD          WHENEQ    'SCGR03'
     C                   MOVEL     FNFND         SCGR03
     C                   SETON                                        73  84     PC @ SCYR03
     C     HFLD          WHENEQ    'SCGR04'
     C                   MOVEL     FNFND         SCGR04
     C                   SETON                                        74  84     PC @ SCYR04
     C     HFLD          WHENEQ    'SCGR05'
     C                   MOVEL     FNFND         SCGR05
     C                   SETON                                        75  84     PC @ SCYR05
     C     HFLD          WHENEQ    'SCGR06'
     C                   MOVEL     FNFND         SCGR06
     C                   SETON                                        76  84     PC @ SCYR06
     C     HFLD          WHENEQ    'SCGR07'
     C                   MOVEL     FNFND         SCGR07
     C                   SETON                                        77  84     PC @ SCYR07
     C     HFLD          WHENEQ    'SCGR08'
     C                   MOVEL     FNFND         SCGR08
     C                   SETON                                        78  84     PC @ SCYR08
     C     HFLD          WHENEQ    'SCGR09'
     C                   MOVEL     FNFND         SCGR09
     C                   SETON                                        79  84     PC @ SCYR09
     C     HFLD          WHENEQ    'SCGR10'
     C                   MOVEL     FNFND         SCGR10
     C                   SETON                                        80  84     PC @ SCYR10
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  IFEQ *IN60
     C*
     C                   ENDIF                                                  SCGR
     C*
      **********************
      * YEAR & CASE SEARCH *
      **********************
     C     HFLD          IFEQ      'SCYR01'
     C     HFLD          OREQ      'SCCS01'
     C     HFLD          OREQ      'SCYR02'
     C     HFLD          OREQ      'SCCS02'
     C     HFLD          OREQ      'SCYR03'
     C     HFLD          OREQ      'SCCS03'
     C     HFLD          OREQ      'SCYR04'
     C     HFLD          OREQ      'SCCS04'
     C     HFLD          OREQ      'SCYR05'
     C     HFLD          OREQ      'SCCS05'
     C     HFLD          OREQ      'SCYR06'
     C     HFLD          OREQ      'SCCS06'
     C     HFLD          OREQ      'SCYR07'
     C     HFLD          OREQ      'SCCS07'
     C     HFLD          OREQ      'SCYR08'
     C     HFLD          OREQ      'SCCS08'
     C     HFLD          OREQ      'SCYR09'
     C     HFLD          OREQ      'SCCS09'
     C     HFLD          OREQ      'SCYR10'
     C     HFLD          OREQ      'SCCS10'
     C*
     C                   MOVE      SCSUB         ##SUB#
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCYR01'
     C     HFLD          OREQ      'SCCS01'
     C                   MOVE      SCGR01        ##FND
     C     HFLD          WHENEQ    'SCYR02'
     C     HFLD          WHENEQ    'SCCS02'
     C                   MOVE      SCGR02        ##FND
     C     HFLD          WHENEQ    'SCYR03'
     C     HFLD          WHENEQ    'SCCS03'
     C                   MOVE      SCGR03        ##FND
     C     HFLD          WHENEQ    'SCYR04'
     C     HFLD          WHENEQ    'SCCS04'
     C                   MOVE      SCGR04        ##FND
     C     HFLD          WHENEQ    'SCYR05'
     C     HFLD          WHENEQ    'SCCS05'
     C                   MOVE      SCGR05        ##FND
     C     HFLD          WHENEQ    'SCYR06'
     C     HFLD          WHENEQ    'SCCS06'
     C                   MOVE      SCGR06        ##FND
     C     HFLD          WHENEQ    'SCYR07'
     C     HFLD          WHENEQ    'SCCS07'
     C                   MOVE      SCGR07        ##FND
     C     HFLD          WHENEQ    'SCYR08'
     C     HFLD          WHENEQ    'SCCS08'
     C                   MOVE      SCGR08        ##FND
     C     HFLD          WHENEQ    'SCYR09'
     C     HFLD          WHENEQ    'SCCS09'
     C                   MOVE      SCGR09        ##FND
     C     HFLD          WHENEQ    'SCYR10'
     C     HFLD          WHENEQ    'SCCS10'
     C                   MOVE      SCGR10        ##FND
     C                   ENDSL
     C*
     C                   CALL      'F0269R'
     C*
     C                   PARM      '2'           ##CALL            1
     C                   PARM      SCSUB         ##SUB#            5 0
     C                   PARM                    ##FND             3 0
     C                   PARM      *ZEROS        ##FYR             3 0
     C                   PARM      *ZEROS        ##CASE            7 0
     C                   PARM      *BLANKS       ##SS#             9
     C                   PARM      *ZEROS        ##ACDT            8 0
     C                   PARM      *BLANKS       ##RTC             1
     C*
     C     ##RTC         CABEQ     'A'           TERM
     C*
     C     ##RTC         IFEQ      'B'
     C     ##CASE        OREQ      *ZERO
     C                   SELECT
     C     HFLD          WHENEQ    'SCYR01'
     C                   SETON                                        71  84     PC @ SCYR01
     C     HFLD          WHENEQ    'SCYR02'
     C                   SETON                                        72  84     PC @ SCYR02
     C     HFLD          WHENEQ    'SCYR03'
     C                   SETON                                        73  84     PC @ SCYR03
     C     HFLD          WHENEQ    'SCYR04'
     C                   SETON                                        74  84     PC @ SCYR04
     C     HFLD          WHENEQ    'SCYR05'
     C                   SETON                                        75  84     PC @ SCYR05
     C     HFLD          WHENEQ    'SCYR06'
     C                   SETON                                        76  84     PC @ SCYR06
     C     HFLD          WHENEQ    'SCYR07'
     C                   SETON                                        77  84     PC @ SCYR07
     C     HFLD          WHENEQ    'SCYR08'
     C                   SETON                                        78  84     PC @ SCYR08
     C     HFLD          WHENEQ    'SCYR09'
     C                   SETON                                        79  84     PC @ SCYR09
     C     HFLD          WHENEQ    'SCYR10'
     C                   SETON                                        80  84     PC @ SCYR10
     C     HFLD          WHENEQ    'SCCS01'
     C                   SETON                                        71  85     PC @ SCCS01
     C     HFLD          WHENEQ    'SCCS02'
     C                   SETON                                        72  85     PC @ SCCS02
     C     HFLD          WHENEQ    'SCCS03'
     C                   SETON                                        73  85     PC @ SCCS03
     C     HFLD          WHENEQ    'SCCS04'
     C                   SETON                                        74  85     PC @ SCCS04
     C     HFLD          WHENEQ    'SCCS05'
     C                   SETON                                        75  85     PC @ SCCS05
     C     HFLD          WHENEQ    'SCCS06'
     C                   SETON                                        76  85     PC @ SCCS06
     C     HFLD          WHENEQ    'SCCS07'
     C                   SETON                                        77  85     PC @ SCCS07
     C     HFLD          WHENEQ    'SCCS08'
     C                   SETON                                        78  85     PC @ SCCS08
     C     HFLD          WHENEQ    'SCCS09'
     C                   SETON                                        79  85     PC @ SCCS09
     C     HFLD          WHENEQ    'SCCS10'
     C                   SETON                                        80  85     PC @ SCCS10
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  QQRTC EQ B
     C*
     C     ##RTC         IFEQ      '*'
     C                   SELECT
     C     HFLD          WHENEQ    'SCYR01'
     C     HFLD          OREQ      'SCCS01'
     C*** BEGIN ADD *** 10/29/98 **************************************
     C                   MOVE      ##FND         SCGR01
     C*** END   ADD *** 10/29/98 **************************************
     C                   MOVE      ##FYR         SCYR01
     C                   MOVE      ##CASE        SCCS01
     C                   SETON                                        71  86     PC @ SCIN01
     C     HFLD          WHENEQ    'SCYR02'
     C     HFLD          OREQ      'SCCS02'
     C*** BEGIN ADD *** 10/29/98 **************************************
     C                   MOVE      ##FND         SCGR02
     C*** END   ADD *** 10/29/98 **************************************
     C                   MOVE      ##FYR         SCYR02
     C                   MOVE      ##CASE        SCCS02
     C                   SETON                                        72  86     PC @ SCIN02
     C     HFLD          WHENEQ    'SCYR03'
     C     HFLD          OREQ      'SCCS03'
     C*** BEGIN ADD *** 10/29/98 **************************************
     C                   MOVE      ##FND         SCGR03
     C*** END   ADD *** 10/29/98 **************************************
     C                   MOVE      ##FYR         SCYR03
     C                   MOVE      ##CASE        SCCS03
     C                   SETON                                        73  86     PC @ SCIN03
     C     HFLD          WHENEQ    'SCYR04'
     C     HFLD          OREQ      'SCCS04'
     C*** BEGIN ADD *** 10/29/98 **************************************
     C                   MOVE      ##FND         SCGR04
     C*** END   ADD *** 10/29/98 **************************************
     C                   MOVE      ##FYR         SCYR04
     C                   MOVE      ##CASE        SCCS04
     C                   SETON                                        74  86     PC @ SCIN04
     C     HFLD          WHENEQ    'SCYR05'
     C     HFLD          OREQ      'SCCS05'
     C*** BEGIN ADD *** 10/29/98 **************************************
     C                   MOVE      ##FND         SCGR05
     C*** END   ADD *** 10/29/98 **************************************
     C                   MOVE      ##FYR         SCYR05
     C                   MOVE      ##CASE        SCCS05
     C                   SETON                                        75  86     PC @ SCIN05
     C     HFLD          WHENEQ    'SCYR06'
     C     HFLD          OREQ      'SCCS06'
     C*** BEGIN ADD *** 10/29/98 **************************************
     C                   MOVE      ##FND         SCGR06
     C*** END   ADD *** 10/29/98 **************************************
     C                   MOVE      ##FYR         SCYR06
     C                   MOVE      ##CASE        SCCS06
     C                   SETON                                        76  86     PC @ SCIN06
     C     HFLD          WHENEQ    'SCYR07'
     C     HFLD          OREQ      'SCCS07'
     C*** BEGIN ADD *** 10/29/98 **************************************
     C                   MOVE      ##FND         SCGR07
     C*** END   ADD *** 10/29/98 **************************************
     C                   MOVE      ##FYR         SCYR07
     C                   MOVE      ##CASE        SCCS07
     C                   SETON                                        77  86     PC @ SCIN07
     C     HFLD          WHENEQ    'SCYR08'
     C     HFLD          OREQ      'SCCS08'
     C*** BEGIN ADD *** 10/29/98 **************************************
     C                   MOVE      ##FND         SCGR08
     C*** END   ADD *** 10/29/98 **************************************
     C                   MOVE      ##FYR         SCYR08
     C                   MOVE      ##CASE        SCCS08
     C                   SETON                                        78  86     PC @ SCIN08
     C     HFLD          WHENEQ    'SCYR09'
     C     HFLD          OREQ      'SCCS09'
     C*** BEGIN ADD *** 10/29/98 **************************************
     C                   MOVE      ##FND         SCGR09
     C*** END   ADD *** 10/29/98 **************************************
     C                   MOVE      ##FYR         SCYR09
     C                   MOVE      ##CASE        SCCS09
     C                   SETON                                        79  86     PC @ SCIN09
     C     HFLD          WHENEQ    'SCYR10'
     C     HFLD          OREQ      'SCCS10'
     C*** BEGIN ADD *** 10/29/98 **************************************
     C                   MOVE      ##FND         SCGR10
     C*** END   ADD *** 10/29/98 **************************************
     C                   MOVE      ##FYR         SCYR10
     C                   MOVE      ##CASE        SCCS10
     C                   SETON                                        80  86     PC @ SCIN10
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  IFEQ *IN60
     C*
     C                   ENDIF                                                  SCGR
     C*
     C*********************
     C* INVOICE SEARCH    *
     C*********************
     C     HFLD          IFEQ      'SCIN01'
     C     HFLD          OREQ      'SCIN02'
     C     HFLD          OREQ      'SCIN03'
     C     HFLD          OREQ      'SCIN04'
     C     HFLD          OREQ      'SCIN05'
     C     HFLD          OREQ      'SCIN06'
     C     HFLD          OREQ      'SCIN07'
     C     HFLD          OREQ      'SCIN08'
     C     HFLD          OREQ      'SCIN09'
     C     HFLD          OREQ      'SCIN10'
     C*
     C                   CALL      'N0720R'
     C*
     C                   PARM                    Q1PRDS           40
     C                   PARM                    Q1CALL            1
     C                   PARM                    Q1FND             3
     C                   PARM                    Q1FYR             3
     C                   PARM                    Q1CASE            7
     C*** BEG CHG *** 01/07/99 ***************************************
     C                   PARM                    Q1INV#            9
     C**********           PARM           Q1INV#  5
     C*** END CHG *** 01/07/99 ***************************************
     C                   PARM                    Q1RTC             1
     C*
     C     Q1RTC         CABEQ     'A'           TERM
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     Q1RTC         IFEQ      'B'
     C                   SELECT
     C     HFLD          WHENEQ    'SCIN01'
     C                   SETON                                        71  86     PC @ SCIN01
     C     HFLD          WHENEQ    'SCIN02'
     C                   SETON                                        72  86     PC @ SCIN02
     C     HFLD          WHENEQ    'SCIN03'
     C                   SETON                                        73  86     PC @ SCIN03
     C     HFLD          WHENEQ    'SCIN04'
     C                   SETON                                        74  86     PC @ SCIN04
     C     HFLD          WHENEQ    'SCIN05'
     C                   SETON                                        75  86     PC @ SCIN05
     C     HFLD          WHENEQ    'SCIN06'
     C                   SETON                                        76  86     PC @ SCIN06
     C     HFLD          WHENEQ    'SCIN07'
     C                   SETON                                        77  86     PC @ SCIN07
     C     HFLD          WHENEQ    'SCIN08'
     C                   SETON                                        78  86     PC @ SCIN08
     C     HFLD          WHENEQ    'SCIN09'
     C                   SETON                                        79  86     PC @ SCIN09
     C     HFLD          WHENEQ    'SCIN10'
     C                   SETON                                        80  86     PC @ SCIN10
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  QQRTC EQ B
     C*
     C     Q1RTC         IFEQ      '*'
     C                   SELECT
     C     HFLD          WHENEQ    'SCIN01'
     C                   MOVE      Q1INV#        SCIN01
     C                   SETON                                        71  87     PC @ SCEO01
     C     HFLD          WHENEQ    'SCIN02'
     C                   MOVE      Q1INV#        SCIN02
     C                   SETON                                        72  87     PC @ SCEO02
     C     HFLD          WHENEQ    'SCIN03'
     C                   MOVE      Q1INV#        SCIN03
     C                   SETON                                        73  87     PC @ SCEO03
     C     HFLD          WHENEQ    'SCIN04'
     C                   MOVE      Q1INV#        SCIN04
     C                   SETON                                        74  87     PC @ SCEO04
     C     HFLD          WHENEQ    'SCIN05'
     C                   MOVE      Q1INV#        SCIN05
     C                   SETON                                        75  87     PC @ SCEO05
     C     HFLD          WHENEQ    'SCIN06'
     C                   MOVE      Q1INV#        SCIN06
     C                   SETON                                        76  87     PC @ SCEO06
     C     HFLD          WHENEQ    'SCIN07'
     C                   MOVE      Q1INV#        SCIN07
     C                   SETON                                        77  87     PC @ SCEO07
     C     HFLD          WHENEQ    'SCIN08'
     C                   MOVE      Q1INV#        SCIN08
     C                   SETON                                        78  87     PC @ SCEO08
     C     HFLD          WHENEQ    'SCIN09'
     C                   MOVE      Q1INV#        SCIN09
     C                   SETON                                        79  87     PC @ SCEO09
     C     HFLD          WHENEQ    'SCIN10'
     C                   MOVE      Q1INV#        SCIN10
     C                   SETON                                        80  87     PC @ SCEO10
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  IFEQ *IN60
     C*
     C                   ENDIF                                                  SCIN
     C*
     C                   MOVE      *ON           *IN88                          ERROR
     C*
     C                   ENDIF                                                  *INKJ
     C*
      **** END   CHG **** 05/07/98 ************************************
     C*
     C****************************
     C* END OF  10 KEY SEARCHES  *
     C****************************
     C*
     C*
     C*********************
     C* F11 = A/R INQUIRY *
     C*********************
     C     *INKK         IFEQ      *ON
     C*
     C                   CALL      'N0164R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C*** BEGIN DEL *** 10/29/98 **************************************
     C**********           PARM           QQRTCD  1
     C*
     C********** QQRTCD    CABEQ'A'       TERM
     C*** END   DEL *** 10/29/98 **************************************
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKK
     C*** BEGIN ADD *** 10/29/98 **************************************
     C*********************
     C* F13 = USER PARMS  *
     C*********************
     C     *INKM         IFEQ      *ON
     C*
     C                   CALL      'F0006R'
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      '1'
     C*
     C                   MOVE      *ZERO         UPCO#
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C*
     C                   ENDIF                                                  *INKM
     C*** END   ADD *** 10/29/98 **************************************
     C*
     C                   ENDIF                                                  *IN11
     C*
     C**********************************
     C*   PROCESS HELP REQUESTS        *
     C**********************************
     C     *IN10         IFEQ      *ON
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN10
     C*****************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C***********************************************
     C* PROCESS REQUEST SCREEN - SCREEN 01          *
     C***********************************************
     C     SCR01T        TAG
     C*
     C* BEGIN EDIT CHECKING
     C*
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLE     10
     C     *BLANK        LOOKUP    TC(X)                              55
     C     *IN55         IFEQ      '1'
     C*
     C     X             ADD       19            A                 2 0
     C*
     C*   CHECK A/R TRANSACTION CODE -- MUST BE IN FTABL (NO "P' TYPES)
     C*
     C                   MOVEL     TC(X)         XXKEY
     C     TBKEY         CHAIN     PAC01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN(A)                         NOT IN TABLE
     C                   MOVE      '1'           *IN30                          INVALID CODE
     C                   GOTO      STRT
     C                   END                                                    END  *IN60
     C*
     C     ACTYPE        IFEQ      'P'
     C                   MOVE      '1'           *IN(A)                         CANT BE 'P'
     C                   MOVE      '1'           *IN31                          INVALID CODE
     C                   GOTO      STRT
     C                   END                                                    ENDEQ 'P'
     C*
     C*   IF DESC IS BLANK, DEFAULT TO DESC IN FTABL
     C*
     C     DC(X)         IFEQ      *BLANK
     C                   MOVE      *BLANKS       DC(X)
     C                   MOVEL     ACDESC        DC(X)
     C                   END
     C*
     C*   VALIDATE SYSTEM CODE
     C*
     C*        SYSTEM CODE MUST EQUAL P (PROVIDER) OR
     C*                               H (HOSPITAL) OR
     C*                               N (NETWORK)
     C*                               U (UTILIZATION REVIEW)
     C*
     C     SY(X)         IFNE      'P'
     C     SY(X)         ANDNE     'H'
     C     SY(X)         ANDNE     'N'
     C     SY(X)         ANDNE     'U'
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C     SY(X)         ANDNE     'M'
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   MOVE      '1'           *IN(A)
     C                   MOVE      '1'           *IN32                          INVALID SYS
     C                   GOTO      STRT
     C                   END                                                    END IFNE 'P'
     C*
     C*
     C*   VALIDATE GROUP/YEAR FOR NUMERIC VALUES
     C*
     C                   MOVE      *BLANKS       HLDGR             3
     C                   MOVE      GR(X)         HLDGR
     C                   TESTN                   HLDGR                96
     C  N96              MOVE      *ZEROS        GR(X)
     C*
     C*** BEGIN DEL *** 10/29/98 **************************************
     C**********           MOVE *BLANKS   HLDYR   2
     C**********           MOVE YR,X      HLDYR
     C**********           TESTN          HLDYR      97
     C**N97*****           MOVE *ZEROS    YR,X
     C*** END   DEL *** 10/29/98 **************************************
     C*
     C*
     C*   VALIDATE GROUP/YEAR/CASE
     C*
     C                   MOVE      GR(X)         XXFND             3 0
     C                   MOVE      YR(X)         XXFYR             3 0
     C                   MOVE      CS(X)         XXCASE            7 0
     C     CMKEY1        CHAIN     PCM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN(A)
     C                   MOVE      '1'           *IN33                          INVALID CASE
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE INVOICE #
     C*
     C     TC(X)         IFNE      'MI'
     C                   MOVE      GR(X)         XXFND             3 0
     C                   MOVE      YR(X)         XXFYR             3 0
     C                   MOVE      CS(X)         XXCASE            7 0
     C*** BEG CHG *** XX/XX/XX ***************************************
     C                   MOVE      IN(X)         XXINV#            9 0
     C**********           MOVE IN,X      XXINV#  50
     C*** END CHG *** XX/XX/XX ***************************************
     C     ARKEY         CHAIN     PAR02                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN(A)
     C                   MOVE      '1'           *IN34                          INVALID INV#
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE SYSTEM ID IS VALID FOR THIS CASE/INVOICE#
     C*
     C     ACTYPE        IFNE      'I'
     C                   MOVE      GR(X)         XXFND             3 0
     C                   MOVE      YR(X)         XXFYR             3 0
     C                   MOVE      CS(X)         XXCASE            7 0
     C*** BEG CHG *** 01/07/99 ***************************************
     C                   MOVE      IN(X)         XXINV#            9 0
     C**********           MOVE IN,X      XXINV#  50
     C*** END CHG *** 01/07/99 ***************************************
     C                   MOVE      SY(X)         XXSYS             1
     C     ARKEY1        CHAIN     PAR02                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN(A)
     C                   MOVE      '1'           *IN37                          INVALID SYS
     C                   GOTO      STRT
     C                   END                                                    END *IN60
     C                   END                                                    ENDNE 'I'
     C*
     C                   ELSE                                                   MI TYPE
     C*
     C     IN(X)         IFNE      *ZEROS
     C                   MOVE      '1'           *IN(A)
     C                   MOVE      '1'           *IN38                          INV# <> 0
     C                   GOTO      STRT
     C                   END                                                    IN,X
     C*
     C                   END                                                    TC,X
     C*** BEGIN ADD *** 12/11/95 ***********************************************
     C*
     C* VALIDATE EOMB#
     C*
     C*** BEGIN DEL *** 02/25/98 **************************************
     C**********           MOVE GR,X      XXFND
     C**********           MOVE YR,X      XXFYR
     C**********           MOVE CS,X      XXCASE
     C*** END DEL *** 02/25/98 ****************************************
     C*** BEGIN ADD *** 10/29/98 **************************************
     C     TC(X)         IFNE      'MI'
     C*** END   ADD *** 10/29/98 **************************************
     C                   MOVE      EO(X)         XXEOB#
     C     EOKEY         CHAIN     PEO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN(A)
     C                   MOVE      '1'           *IN40                          INVALID EOB#
     C                   GOTO      STRT
     C                   END
     C*
     C* MAKE SURE THE EOMB IS ON THIS INVOICE
     C*
     C     ARKEY2        CHAIN     PAR03                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN(A)
     C                   MOVE      '1'           *IN41                          INVALID EOB# FOR INV
     C                   GOTO      STRT
     C                   END
     C*** BEGIN ADD *** 10/29/98 **************************************
     C*
     C                   ELSE
     C*
     C     EO(X)         IFNE      *ZEROS
     C*
     C                   MOVE      '1'           *IN(A)
     C                   MOVE      '1'           *IN42                          EOB# MUST BE BLANKS
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 10/29/98 **************************************
     C*** END ADD *** 12/11/95 *************************************************
     C*
     C*   VALIDATE POSTING DATE -- IF A/R TRANSACTION CODE IS MI TYPE
     C*                            A POSTING DATE MUST BE ENTERED
     C*
     C     PD(X)         IFEQ      *ZERO
     C     TC(X)         ANDEQ     'MI'
     C                   MOVE      '1'           *IN(A)
     C                   MOVE      '1'           *IN39                          INVALID
     C                   GOTO      STRT
     C*                                                    POSTING DT
     C                   END
     C*
     C*
     C*   VALIDATE POSTING DATE -- IF DATE BLANK, DSDATE IS MOVED IN.
     C*
     C     PD(X)         IFNE      *ZERO
     C                   MOVE      *BLANKS       SRDTE             6
     C                   MOVE      ' '           SRERR             1
     C                   MOVE      ' '           SRTERM            1
     C                   Z-ADD     PD(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         PD(X)
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN(A)
     C                   MOVE      '1'           *IN35                          INVALID DATE
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   AMOUNT MUST NOT BE ZERO
     C*
     C     AM(X)         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN(A)
     C                   MOVE      '1'           *IN36                          INVALID AMT
     C                   GOTO      STRT
     C                   END
     C*
     C                   ELSE                                                   ELSIF *IN55
     C                   Z-ADD     10            X
     C                   END                                                    ENDIF *IN55
     C*
     C                   ADD       1             X
     C*
     C                   END                                                    END DOWLE 10
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C*
     C*
     C*   ONLY WRITE RECORD WITH VALID TRANSACTION CODE
     C*
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLE     10
     C     *BLANK        LOOKUP    TC(X)                              55
     C     *IN55         IFEQ      '1'
     C*
     C*   ONLY UPDATE COMPANY AUXILLARY FILE WITH NEW INVOICE # IF ALL
     C*   EDITS WERE PASSED
     C*
     C     TC(X)         IFEQ      'MI'
     C     C1KEY         CHAIN     PC101                              60
     C     *IN60         IFEQ      '0'
     C                   ADD       1             C1LIN#
     C                   UPDATE    PC101
     C                   ELSE
     C                   Z-ADD     1             C1LIN#
     C                   Z-ADD     *ZERO         C1EOB#
     C                   Z-ADD     *ZERO         C1LIN#
     C                   Z-ADD     UPCO#         C1CO#
     C                   MOVE      *BLANKS       C1ADR1
     C                   MOVE      *BLANKS       C1ADR2
     C                   MOVE      *BLANKS       C1CTY
     C                   MOVE      *BLANKS       C1ST
     C                   Z-ADD     *ZEROS        C1ZIP
     C                   Z-ADD     *ZEROS        C1AREA
     C                   Z-ADD     *ZEROS        C1PHN#
     C                   Z-ADD     *ZEROS        C1FEIN
     C                   Z-ADD     *ZEROS        C1ANER
     C                   MOVE      *BLANK        C1STAT
     C                   MOVEL     Q1USER        C1USER
     C                   MOVEL     Q1PGM         C1PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        C1TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C1DT
     C                   MOVEL     Q1USER        C1IUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C1IDT
     C                   WRITE     PC101
     C                   END
     C*
     C                   MOVE      C1LIN#        IN(X)
     C                   END                                                     END EQ MI
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C*
     C*  GET A/R TRANSACTION TYPE
     C*
     C                   MOVEL     TC(X)         XXKEY
     C     TBKEY         CHAIN     PAC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      ACTYPE        ARTRTP                         TRAN TYPE
     C                   END
     C*
     C*  CONVERT POSTING DATE TO YMD FORMAT IF NOT MI TYPE
     C*
     C     PD(X)         IFEQ      *ZERO
     C     TC(X)         ANDNE     'MI'
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARPODT
     C                   ELSE
     C                   Z-ADD     PD(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARPODT
     C                   END
     C*
     C*  MOVE SCREEN FIELDS TO FILE FIELDS FOR NMARP
     C*
     C                   MOVEL     TC(X)         ARTRCD                         TRANS CODE
     C                   MOVEL     SY(X)         ARSYS                          SYSTEM CODE
     C                   MOVE      GR(X)         ARFND                          GROUP
     C                   MOVE      YR(X)         ARFYR                          FUND YEAR
     C                   MOVE      CS(X)         ARCASE                         CASE
     C                   MOVE      *BLANKS       ARDESC                         ADD 12/12/95
     C                   MOVEL     DC(X)         ARDESC                         DESCRIPTION
     C                   MOVE      IN(X)         ARINV#                         INVOICE #
     C                   MOVE      AM(X)         ARAMT                          AMOUNT
     C*** BEGIN CHG *** 12/11/95 ***********************************************
     C**********           Z-ADD*ZEROS    AREOB#           EOB #
     C                   MOVE      EO(X)         AREOB#                         EOB #
     C*** END CHG *** 12/11/95 *************************************************
     C                   Z-ADD     *ZEROS        ARCK#                          CHECK #
     C                   MOVE      *BLANKS       ARSTAT                         STATUS
     C*
     C                   MOVE      UPCO#         ARCO#                          COMPANY #
     C                   MOVE      UPSUB#        ARSUB#                         SUBSCR  #
     C                   MOVE      Q1USER        ARUSER
     C                   MOVE      Q1PGM         ARPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C                   Z-ADD     DSTIME        ARTIME
     C*
     C                   WRITE     PAR01
     C*
     C                   ELSE                                                   ELSIF *IN55
     C                   Z-ADD     10            X
     C                   END                                                    ENDIF *IN55
     C*
     C                   ADD       1             X
     C                   END                                                    END DOWLE 10
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVEL     *BLANKS       TC
     C                   MOVEL     *BLANKS       SY
     C                   MOVEL     *BLANKS       GR
     C*** BEGIN CHG *** 10/29/98 **************************************
     C**********           MOVEL*BLANKS   YR
     C                   MOVE      *ZEROS        YR
     C*** END   CHG *** 10/29/98 **************************************
     C                   MOVEL     *BLANKS       CS
     C                   MOVE      *BLANKS       IN
     C                   Z-ADD     *ZEROS        PD
     C                   MOVEL     *BLANKS       DC
     C                   Z-ADD     *ZEROS        AM
     C                   Z-ADD     *ZEROS        EO                             ADD 12/11/95
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
