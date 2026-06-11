     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H/TITLE N0165R - CASH POSTING (WITH CASE PROMPT)
     H*  COPYRIGHT PROGRESSIVE DATA SOLUTIONS, INC. 1997
     H*****************************************************************
     H**   PROGRAM DESCRIPTION: POST CASH TO ONE CASE & INVOICE ---  **
     H**                        WRITES A/R RECORDS TO FMARP.         **
     H**                                                             **
     H**   CALLED FROM: MENU ONLY                                    **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** MODIFICATIONS:                                              **
     H*                                                              **
     H*  06/09/20 C:212 Project : SY420                              **
     H*                 Mask SSN                                     **
     H*                                                              **
     H*  11/01/99 C:62  PROJECT:  MC212                              **
     H*                 ALLOW TRANSACTION CODES THAT ARE 'N' TYPES.  **
     H*                                                              **
     H*  01/07/99 C:94  PROJECT:  MC152                              **
     H*                 CHANGED LENGTH OF INV# FROM 5 TO 9           **
     H*                                                              **
     H*  04/27/98 C:90  PROJ: MC181                                  **
     H*                 SCR01 - CHANGED TO MEET VERSION 4 STANDARDS  **
     H*                 COMBINED SEARCHES INTO F10 SEARCH            **
     H*                                                              **
     H*  06/29/94 C:05  REQ/PROJ: 141/MC110                          **
     H*                 1. CHANGE F15 ALPHA SEARCH TO CALL NEW F0578R1/F1
     H*                 IF A RECORD IS SELECTED FROM THIS SEARCH     **
     H*                 PGM WILL AUTOMATICALLY CALL F0006R2 TO UPDATE**
     H*                 THE USER PARAMETER RECORD TO THE SAME SUB/GRP**
     H*                 AS THE RECORD THAT HAS BEEN SELECTED.        **
     H**                                                             **
     H** 11/16/93 C:20  PROJ: AR007                                  **
     H**                1. CHANGE CODE TO CALL NEW A/R INQUIRY N0164 **
     H*
     H** 07/27/93 C:20  PROJ: FSMNT                                  **
     H**                1. CORRECTED PARM LIST FOR CALL TO F0571R    **
     H**                   ADDED FND.                                **
     H**                                                             **
     H** 11/12/92 C:20  CHANGED F16 TO CALL CORRECT PROGRAM FOR A/R  **
     H**                INQUIRY                                      **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILES:                                                      **
     H**    SMCOL (PCO01): CO                   COMPANY MASTER FILE  **
     H**    FMUPL (PUP01): Q1USER               USER PARM FILE       **
     H**    FMFNL (PFN01): CO/SUBSC             FUND MASTER FILE     **
     H**                                                             **
     H*****************************************************************
     FN0165F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
      **** BEGIN DEL **** 04/27/98 ************************************
     F**********FFMCMLA  IF  E           K        DISK
     F******F            PCM01                             KRENAMEPCM02
      **** END   DEL **** 04/27/98 ************************************
     FFTABL     IF   E           K DISK
     FNMARL5    IF   E           K DISK
     F                                     RENAME(PAR01:PAR02)
     FNMARP     O    E           K DISK
      **** BEGIN ADD **** 04/27/98 ************************************
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     FFTACP     IF   E             DISK
     F                                     RENAME(PAC01:PAC02)
      **** END   ADD **** 04/27/98 ************************************
     F*

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D*===============================================================*
     D*     EXTENSION SECTION                                         *
     D*===============================================================*
     D*
     D* DEFINE ARRAYS
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  SY                     1     10
     D                                     DIM(10) ASCEND                         SYSTEM ID
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
     D  DC                     1    300
     D                                     DIM(10) ASCEND                         DESCRIPTION
     D  SCDC01                 1     30
     D  SCDC02                31     60
     D  SCDC03                61     90
     D  SCDC04                91    120
     D  SCDC05               121    150
     D  SCDC06               151    180
     D  SCDC07               181    210
     D  SCDC08               211    240
     D  SCDC09               241    270
     D  SCDC10               271    300
     D                 DS
     D  IA                     1     90  2
     D                                     DIM(10) ASCEND                         INVOICED AMT
     D  SCIA01                 1      9  2
     D  SCIA02                10     18  2
     D  SCIA03                19     27  2
     D  SCIA04                28     36  2
     D  SCIA05                37     45  2
     D  SCIA06                46     54  2
     D  SCIA07                55     63  2
     D  SCIA08                64     72  2
     D  SCIA09                73     81  2
     D  SCIA10                82     90  2
     D                 DS
     D  OA                     1     90  2
     D                                     DIM(10) ASCEND                         OPEN AMT
     D  SCOA01                 1      9  2
     D  SCOA02                10     18  2
     D  SCOA03                19     27  2
     D  SCOA04                28     36  2
     D  SCOA05                37     45  2
     D  SCOA06                46     54  2
     D  SCOA07                55     63  2
     D  SCOA08                64     72  2
     D  SCOA09                73     81  2
     D  SCOA10                82     90  2
     D                 DS
     D  DA                     1     90  2
     D                                     DIM(10) ASCEND                         DISBURSD AMT
     D  SCDA01                 1      9  2
     D  SCDA02                10     18  2
     D  SCDA03                19     27  2
     D  SCDA04                28     36  2
     D  SCDA05                37     45  2
     D  SCDA06                46     54  2
     D  SCDA07                55     63  2
     D  SCDA08                64     72  2
     D  SCDA09                73     81  2
     D  SCDA10                82     90  2
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    ARSUB#        XXSUB#
     C     *LIKE         DEFINE    CMDT          XXACDT
     C*
     C*
     C*   KEY LIST FOR THE COMPANY FILE  (SMCOL)
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
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
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    XXCASE            7 0
      **** BEGIN DEL **** 04/27/98 ************************************
     C*
     C**********  CLAIM FILE (FMCMLA)
     C*
     C********** CMKEY2    KLIST
     C******************** KFLD           XXSS#
     C******************** KFLD           XXACDT
      **** END   DEL **** 04/27/98 ************************************
     C*
     C*  A/R FILE   (NMARL5)
     C*
     C     ARKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*** BEG CHG *** 01/07/99 ***************************************
     C                   KFLD                    XXINV#            9 0
     C**********           KFLD           XXINV#  50
     C*** END CHG *** 01/07/99 ***************************************
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE (FMCDL)
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
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
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   Z-ADD     *ZERO         UPSUB#
     C                   ELSE
     C                   MOVE      UPFND         SCFND
     C                   Z-ADD     UPSUB#        SCSUB#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      '1'           *IN29                           CURSOR POST
     C                   Z-ADD     *ZEROS        SCFYR
     C                   Z-ADD     *ZEROS        SCCASE
     C                   MOVEL     *BLANKS       SCNAME
     C                   MOVEL     *BLANKS       SCSS#
     C                   Z-ADD     *ZEROS        SCACDT
     C                   Z-ADD     *ZEROS        SCINV#
     C                   MOVEL     *BLANKS       SCTRCD
     C                   Z-ADD     *ZEROS        SCIAMT
     C                   Z-ADD     *ZEROS        SCOAMT
     C                   Z-ADD     *ZEROS        SCDAMT
     C                   Z-ADD     *ZEROS        SCCAMT
     C                   Z-ADD     *ZEROS        SCCK#
      *** BEGIN DEL *** 10/08/99 **************************************
      **** BEGIN CHG **** 04/28/98 ************************************
     C******************** Z-ADDDSDATE    SCPODT
     C***********          Z-ADDDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DATCHK
     C***********          MOVELSRDTO     DSDATE
      **** BEGIN CHG **** 04/28/98 ************************************
      *** END   DEL *** 10/08/99 **************************************
     C                   Z-ADD     *ZEROS        XXIAMT
     C                   Z-ADD     *ZEROS        XXOAMT
     C                   Z-ADD     *ZEROS        XXBAMT
     C                   MOVEL     *BLANKS       SY
     C                   Z-ADD     *ZEROS        IA
     C                   Z-ADD     *ZEROS        OA
     C                   Z-ADD     *ZEROS        DA
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
     C                   EXFMT     SCR02
     C                   END
     C****************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C**********************************
     C*   F3 = EXIT                    *
     C**********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    ENDIF *INKC
     C**********************************
     C*   F5 = RESTART                 *
     C**********************************
     C     *INKE         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     *ZEROS        SCIAMT
     C                   Z-ADD     *ZEROS        SCOAMT
     C                   Z-ADD     *ZEROS        SCDAMT
     C                   Z-ADD     *ZEROS        XXIAMT
     C                   Z-ADD     *ZEROS        XXOAMT
     C                   Z-ADD     *ZEROS        XXBAMT
     C                   MOVEL     *BLANKS       SY
     C                   Z-ADD     *ZEROS        IA
     C                   Z-ADD     *ZEROS        OA
     C                   Z-ADD     *ZEROS        DA
     C                   END                                                    ENDIF SCR02
     C                   GOTO      STRT
     C                   END                                                    ENDIF *INKE
     C/SPACE 3
      *
     C**********************************
     C*   F10 = SEARCH                 *
     C**********************************
      *
     C**********************************
     C**********   COMMAND 06 = SUBSC SEARCH    *
     C**********************************
     C********** *INKF     IFEQ '1'
     C     *INKJ         IFEQ      '1'
     C**********************************
     C*     SUBSCRIBER SEARCH          *
     C**********************************
     C     Q2RECF        IFEQ      'SCR01'
     C     HFLD          IFEQ      'SCSUB#'
     C                   CALL      'F0055R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@RRNS            9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         CABEQ     *ON           STRT                     30
      *
     C     @@RRNS        CHAIN     PSM02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVE      *ON           *IN31
     C                   ELSE
     C                   MOVE      *ON           *IN30
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD SCSUB#
      *
     C*********************************
     C*        GROUP SEARCH           *
     C*********************************
     C     HFLD          IFEQ      'SCFND'
     C                   MOVE      @@RRNS        @@RRNF
     C                   CALL      'F0060R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@RRNF            9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         CABEQ     *ON           STRT                     31
      *
     C     @@RRNF        CHAIN     PFN02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      FNFND         SCFND
     C                   MOVE      *ON           *IN32
     C                   ELSE
     C                   MOVE      *ON           *IN31
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  IFEQ SCFND
      *
     C*********************************
     C*    YEAR & CASE SEARCH         *
     C*********************************
     C     HFLD          IFEQ      'SCFYR'
     C     HFLD          OREQ      'SCCASE'
     C                   MOVE      SCSUB#        @@SUB#
     C                   MOVE      SCFND         @@FND
     C                   CALL      'F0269R'
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM      *ZEROS        @@FYR             3 0
     C                   PARM      *ZEROS        @@CASE            7 0
     C                   PARM                    @@SS#             9
     C                   PARM                    @@ACDT            8 0
     C                   PARM                    @@RTCD            1
      *
     C     @@FYR         IFEQ      *ZEROS
     C     HFLD          CABEQ     'SCFYR'       STRT                     32    PC AT SCFYR
     C                   ENDIF                                                  IFEQ @@FYR
     C     @@CASE        IFEQ      *ZEROS
     C     HFLD          CABEQ     'SCCASE'      STRT                     33    PC AT SCCASE
     C                   ENDIF                                                  IFEQ @@CASE
      *
     C     @@RTCD        IFEQ      '*'
     C                   MOVE      @@SUB#        SCSUB#
     C                   MOVE      @@FND         SCFND             3 0
     C                   MOVE      @@FYR         SCFYR
     C                   MOVE      @@CASE        SCCASE
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF                                                  IFEQ '*'
     C                   ENDIF                                                  HFLD SCFYR
      *
     C*********************************
     C*   TRANS. CODE SEARCH          *
     C*********************************
     C     HFLD          IFEQ      'SCTRCD'
     C                   CALL      'N2305R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@RRN             9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         CABEQ     *ON           STRT                     35
      *
     C     @@RRN         CHAIN     PAC02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     ACKEY         SCTRCD
     C                   MOVE      *ON           *IN27
     C                   ELSE
     C                   MOVE      *ON           *IN35
     C                   ENDIF                                                  IFEQ *IN60
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD SCTRCD
      *
     C     HFLD          IFEQ      'SCINV#'
     C     HFLD          OREQ      'SCCAMT'
     C     HFLD          OREQ      'SCCK#'
     C     HFLD          OREQ      'SCPODT'
     C                   MOVE      *ON           *IN90
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD SCINV#
      *
     C                   ENDIF                                                  IFEQ SCR01
     C                   ENDIF                                                  IFEQ INKJ
     C******************** CALL 'F0545R'
     C******************** PARM           QQPRDS 40
     C******************** PARM           QQCALL  1
     C******************** PARM           XXSUB#
     C******************** PARM           QQRTCD  1
     C*
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** XXSUB#    OREQ *ZERO
     C******************** GOTO STRT
     C******************** END
     C*
     C********** QQRTCD    IFEQ '*'
     C******************** MOVE XXSUB#    SCSUB#
     C******************** GOTO STRT
     C******************** END
     C******************** END
     C**********************************
     C*   SCREEN 2 = DETAIL INQUIRY
     C**********************************
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCCASE        QQCASE
     C*** BEG CHG *** 01/07/99 ***************************************
     C                   MOVE      SCINV#        QQINV#            9
     C**********           MOVE SCINV#    QQINV#  5
     C*** END CHG *** 01/07/99 ***************************************
     C                   CALL      'N0725R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C*** BEG CHG *** 01/07/99 ***************************************
     C                   PARM                    QQINV#            9
     C**********           PARM           QQINV#  5
     C*** END CHG *** 01/07/99 ***************************************
     C                   PARM                    QQRTCD            1
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQRTCD        OREQ      '*'
     C                   GOTO      STRT
     C                   END                                                    ENDEQ 'B'
     C*
     C                   END                                                    ENDEQ SCR02
     C*
     C******************** END                             ENDIF *INKF
     C**********************************
     C********** COMMAND 07 - SS#/ACCIDENT DATE *
     C**********************************
     C********** *INKG     IFEQ '1'
     C******************** MOVE *BLANKS   XXSS#
     C******************** Z-ADD*ZEROS    X1ACDT  80
     C******************** CALL 'F0573R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           XXSS#   9
     C******************** PARM           X1ACDT
     C******************** PARM           QQRTCD  1
     C*
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** XXSS#     OREQ *BLANK
     C******************** GOTO STRT
     C******************** END                             ENDIF 'B'
     C*
     C********** QQRTCD    IFEQ '*'
     C*
     C******************** Z-ADDX1ACDT    SRDTI
     C******************** CALL 'SRCVTDAT'DTCYMD
     C******************** MOVELSRDTO     XXACDT
     C*
     C********** CMKEY2    CHAINPCM02                60
     C********** *IN60     IFEQ '0'
     C******************** MOVE CMFND     SCFND
     C******************** MOVE CMFYR     SCFYR
     C******************** MOVE CMCASE    SCCASE
     C******************** MOVE '1'       *IN26            CURSOR POST
     C******************** GOTO STRT
     C******************** END                             ENDIF *IN60
     C*
     C******************** END                             ENDIF '*'
     C*
     C******************** END                             ENDIF *INKG
     C**********************************
     C**********   F8 = CASE SEARCH             *
     C**********************************
     C********** *INKH     IFEQ '1'
     C*
     C******************** MOVE SCFND     Q1FND            ADD 10/05/92
     C******************** CALL 'F0570R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           SCFYR
     C******************** PARM           SCCASE
     C******************** PARM           XXSS#   9
     C******************** PARM           QQRTCD
     C******************** PARM           Q1FND   30       ADD 10/05/92
     C*
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** SCFYR     OREQ *ZERO
     C******************** GOTO STRT
     C******************** END                             ENDIF 'B'
     C*
     C********** QQRTCD    IFEQ '*'
     C******************** MOVE Q1FND     SCFND
     C******************** MOVE '1'       *IN26            CURSOR POST
     C******************** GOTO STRT
     C******************** END                             ENDIF '*'
     C*
     C******************** END                             ENDEQ *INKH
     C**********************************
     C**********   COMMAND 09 = ALPHA SEARCH    *
     C**********************************
     C********** *INKI     IFEQ '1'
     C*
     C******************** MOVE *BLANK    XXSS#   9
     C******************** Z-ADD*ZEROS    X1ACDT
     C******************** CALL 'F0578R1'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           XXSS#
     C******************** PARM           X1ACDT
     C******************** PARM           SCSUB#
     C******************** PARM           SCFND
     C******************** PARM           SCFYR
     C******************** PARM           SCCASE
     C******************** PARM           QQRTCD  1
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** SCFYR     OREQ *ZEROS
     C******************** MOVE '1'       *IN27
     C******************** GOTO STRT
     C******************** END                             ENDEQ 'B'
     C*
     C********** QQRTCD    IFEQ '*'
     C*
     C******************** Z-ADDX1ACDT    SRDTI
     C******************** CALL 'SRCVTDAT'DTCYMD
     C******************** MOVELSRDTO     XXACDT
     C*
     C******************** MOVE UPCO#     QQCO#   3
     C******************** MOVE SCSUB#    Q2SUB#  5
     C******************** MOVE SCFND     Q2FND   3
     C******************** CALL 'F0006R2'
     C******************** PARM           QQCO#
     C******************** PARM           Q2SUB#
     C******************** PARM           Q2FND
     C******************** PARM           QQRTCD
     C******************** MOVE '1'       *IN26
     C******************** GOTO STRT
     C******************** END
     C*
     C******************** END                             ENDIF *INKI
     C****************************************************************
     C************* F10 - TABLE FILE SEARCH FOR A/R TRANSACTION CODE *
     C****************************************************************
     C********** *INKJ     IFEQ '1'
     C*
     C******************** MOVELSCTRCD    XX2     2
     C*
     C********** XX2       IFEQ '? '
     C*
     C******************** MOVE *BLANK    QQKEY  10
     C******************** CALL 'F2181R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           QQKEY
     C******************** PARM           QQRTCD  1
     C******************** MOVELQQKEY     SCTRCD
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** MOVE '1'       *IN27            CURSOR POST
     C******************** GOTO STRT
     C******************** END                             ENDIF '?'
     C*
     C******************** END                             ENDIF *INKJ
      **** END   CHG **** 04/27/98 ************************************
     C*****************************************
     C*   COMMAND 11 = INVOICE SUMMARY SEARCH *
     C*****************************************
     C     *INKK         IFEQ      '1'
     C*
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCCASE        QQCASE
     C*** BEG CHG *** 01/07/99 ***************************************
     C                   MOVE      SCINV#        QQINV#            9
     C**********           MOVE SCINV#    QQINV#  5
     C*** END CHG *** 01/07/99 ***************************************
     C                   CALL      'N0720R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C*** BEG CHG *** 01/07/99 ***************************************
     C                   PARM                    QQINV#            9
     C**********           PARM           QQINV#  5
     C*** END CHG *** 01/07/99 ***************************************
     C                   PARM                    QQRTCD            1
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCFND         OREQ      *ZEROS
     C                   GOTO      STRT
     C                   END                                                    ENDIF 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      QQFND         SCFND
     C                   MOVE      QQFYR         SCFYR
     C                   MOVE      QQCASE        SCCASE
     C                   MOVE      QQINV#        SCINV#
     C                   MOVE      '1'           *IN28                          CURSOR POST
     C                   GOTO      STRT
     C                   END                                                    ENDIF '*'
     C*
     C                   END                                                    ENDIF *INKK
     C**********************************
     C*   F12 = PREVIOUS               *
     C**********************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     *ZEROS        SCIAMT
     C                   Z-ADD     *ZEROS        SCOAMT
     C                   Z-ADD     *ZEROS        SCDAMT
     C                   Z-ADD     *ZEROS        XXIAMT
     C                   Z-ADD     *ZEROS        XXOAMT
     C                   Z-ADD     *ZEROS        XXBAMT
     C                   MOVEL     *BLANKS       SY
     C                   Z-ADD     *ZEROS        IA
     C                   Z-ADD     *ZEROS        OA
     C                   Z-ADD     *ZEROS        DA
     C                   END                                                    ENDIF SCR02
     C                   GOTO      STRT
     C                   END                                                    ENDIF *INKL
     C****************************************
     C*   F15 = INVOICE DETAIL SEARCH        *
     C****************************************
     C     *INKP         IFEQ      '1'
     C*
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCCASE        QQCASE
     C*** BEG CHG *** 01/07/99 ***************************************
     C                   MOVE      SCINV#        QQINV#            9
     C**********           MOVE SCINV#    QQINV#  5
     C*** END CHG *** 01/07/99 ***************************************
     C                   CALL      'N0724R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C*** BEG CHG *** 01/07/99 ***************************************
     C                   PARM                    QQINV#            9
     C**********           PARM           QQINV#  5
     C*** END CHG *** 01/07/99 ***************************************
     C                   PARM                    QQRTCD            1
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCFND         OREQ      *ZEROS
     C                   GOTO      STRT
     C                   END                                                    ENDIF 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      QQFND         SCFND
     C                   MOVE      QQFYR         SCFYR
     C                   MOVE      QQCASE        SCCASE
     C                   MOVE      QQINV#        SCINV#
     C                   MOVE      '1'           *IN28                          CURSOR POST
     C                   GOTO      STRT
     C                   END                                                    ENDIF '*'
     C*
     C                   END                                                    ENDIF *INKP
     C**********************************
     C*   F16 = A/R INQUIRY            *
     C**********************************
     C     *INKQ         IFEQ      '1'
     C*
     C                   CALL      'N0164R'                                     ADD 11/16/93
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END                                                    ENDIF *INKQ
     C**********************************
     C*   PROCESS HELP REQUESTS        *
     C**********************************
     C     *IN10         IFEQ      '1'
     C                   GOTO      STRT
     C                   END                                                    ENDIF *IN10
      **** BEGIN DEL **** 04/27/98 ************************************
     C**********************************
     C*********** CHANGE USER PARMS              *
     C**********************************
     C********** *INKM     IFEQ '1'
     C******************** CALL 'F0006R'
     C******************** WRITESCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C******************** MOVE *ZERO     UPCO#
     C******************** END                             ENDIF *IN60
     C*
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO TOP
     C******************** END                             ENDIF *INKM
     C*
      **** END   DEL **** 04/27/98 ************************************
     C                   END                                                    END CMDKEY
     C****************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C***************************
     C* PROCESS REQUEST SCREEN  *
     C***************************
     C     SCR01T        TAG
     C*
     C* BEGIN EDIT CHECKING
     C*
     C*
     C*   VALIDATE CASE NUMBER
     C*
     C                   MOVE      SCSUB#        XXSUB#
     C                   MOVE      SCFND         XXFND
     C                   MOVE      SCFYR         XXFYR
     C                   MOVE      SCCASE        XXCASE
     C     CMKEY1        CHAIN     PCM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20                          INVALID CASE
     C                   GOTO      STRT
     C                   ELSE
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      CMSS#         SCSS#
      /Free
       scSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   END                                                    ENDIF *IN60
     C*
     C*   VALIDATE INVOICE#
     C*
     C                   MOVE      SCINV#        XXINV#
     C     ARKEY         CHAIN     PAR02                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN21                          INVALID INV#
     C                   GOTO      STRT
     C                   END                                                    ENDIF *IN60
     C*
     C*   VALIDATE TRANSACTION CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCTRCD        XXKEY
     C     TBKEY         CHAIN     PAC01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN22                          INVALID CODE
     C                   GOTO      STRT
     C                   END                                                    ENDIF *IN60
     C*
     C     ACTYPE        IFNE      'P'
     C*** BEGIN ADD *** 11/01/99 **************************************
     C     ACTYPE        ANDNE     'N'
     C*** END   ADD *** 11/01/99 **************************************
     C                   MOVE      '1'           *IN22                          INVALID CODE
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVEL     ACDESC        XXDESC           30            TRANS DESC
     C                   END                                                    ENDNE 'P'
     C*
     C*   VALIDATE CHECK AMOUNT
     C*
     C     SCCAMT        IFEQ      *ZERO
     C                   MOVE      '1'           *IN23                          INVALID AMT
     C                   GOTO      STRT
     C                   END                                                    ENDIF LE
     C*
     C*   VALIDATE CHECK NUMBER
     C*
     C     SCCK#         IFLE      *ZERO
     C                   MOVE      '1'           *IN24                          INVALID CHK#
     C                   GOTO      STRT
     C                   END                                                    ENDIF LE
     C*
     C*   VALIDATE POST DATE
     C*
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN25                          INVALID DATE
     C                   GOTO      STRT
     C                   END                                                    ENDIF SRERR
     C*
     C*
     C*  ALL EDITS HAVE BEEN PASSED: GET REMAINING SCRN2 INFORMATION
     C*  GET CLAIMANT NAME
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCNAME
     C                   ELSE
     C                   MOVEL     *BLANKS       SCNAME
     C                   END
     C*
     C*  CALCULATE SYSTEM INVOICE & DISBURSED AMOUNTS
     C*
     C     ARKEY         CHAIN     PAR02                              50
     C     *IN50         IFEQ      '0'
     C                   Z-ADD     SCCAMT        XXBAMT            9 2          BAL CHK AMT
     C                   MOVE      ARSYS         XXSYS             1            HOLD SYS ID
     C                   Z-ADD     1             X                              ARRAY INDEX
     C                   END                                                    ENDIF *IN60
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C*  DETERMINE IF SYSTEM CODE CHANGES
     C*
     C     ARSYS         IFNE      XXSYS
     C                   MOVE      XXSYS         SY(X)                          SYSTEM ID
     C                   MOVEL     XXDESC        DC(X)                          DESCRIPTION
     C                   Z-ADD     XXIAMT        IA(X)                          INVOICE AMT
     C                   Z-ADD     XXOAMT        OA(X)                          OPEN AMT
     C*
     C     XXOAMT        IFGT      *ZERO
     C     XXOAMT        IFGE      XXBAMT
     C                   Z-ADD     XXBAMT        DA(X)                          DISBURSE AMT
     C                   Z-ADD     *ZERO         XXBAMT
     C                   ELSE
     C                   Z-ADD     XXOAMT        DA(X)                          DISBURSE AMT
     C     XXBAMT        SUB       XXOAMT        XXBAMT
     C                   END                                                    ENDGE XXBAMT
     C                   END                                                    ENDGT *ZERO
     C*
     C                   MOVE      ARSYS         XXSYS                          HOLD SYS ID
     C                   Z-ADD     *ZERO         XXIAMT                         SYS INV AMT
     C                   Z-ADD     *ZERO         XXOAMT                         SYS OPN AMT
     C                   ADD       1             X
     C                   END                                                    ENDNE XXSYS
     C*
     C*  CALCULATE INVOICE AMOUNT
     C*
     C     ARTRTP        IFEQ      'I'
     C                   ADD       ARAMT         XXIAMT            9 2          SYS INV AMT
     C                   ADD       ARAMT         SCIAMT                         TOT INV AMT
     C                   END                                                    ENDIF 'I'
     C*
     C*  CALCULATE OPEN AMOUNT
     C*
     C     ARTRTP        IFEQ      'P'
     C                   SUB       ARAMT         XXOAMT            9 2          SYS OPEN AMT
     C                   SUB       ARAMT         SCOAMT                         TOT OPEN AMT
     C                   ELSE
     C                   ADD       ARAMT         XXOAMT
     C                   ADD       ARAMT         SCOAMT
     C                   END                                                    ENDIF 'P'
     C*
     C     ARKEY         READE     PAR02                                  50
     C*
     C                   END                                                    END DOWEQ
     C*
     C*  END OF FILE OR KEY HAS CHANGED - OUTPUT LAST SYSTEM'S AMOUNTS
     C*
     C                   MOVE      XXSYS         SY(X)                          SYSTEM ID
     C                   MOVEL     XXDESC        DC(X)                          DESCRIPTION
     C                   Z-ADD     XXIAMT        IA(X)                          INVOICE AMT
     C                   Z-ADD     XXOAMT        OA(X)                          OPEN AMT
     C*
     C     XXOAMT        IFGT      *ZERO
     C     XXOAMT        IFGE      XXBAMT
     C                   Z-ADD     XXBAMT        DA(X)                          DISBURSE AMT
     C                   Z-ADD     *ZERO         XXBAMT
     C                   ELSE
     C                   Z-ADD     XXOAMT        DA(X)                          DISBURSE AMT
     C     XXBAMT        SUB       XXOAMT        XXBAMT
     C                   END                                                    ENDGE XXBAMT
     C                   END                                                    ENDGT *ZERO
     C*
     C*  CALCULATE SUM OF DISBURSED AMOUNTS
     C*
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLE     10
     C                   ADD       DA(X)         SCDAMT            9 2          SUM OF DISB
     C                   ADD       1             X
     C                   END                                                    END DOWLE 10
     C*
     C*  IF OPEN AMOUNT DOES NOT EQUAL CHECK AMOUNT - DISPLAY WARNING
     C*
     C     SCOAMT        IFNE      SCCAMT
     C                   MOVE      '1'           *IN25                          WARNING
     C                   ELSE
     C                   MOVE      '0'           *IN25
     C                   END                                                    ENDNE SCCAMT
     C                   DO        10            Z                 5 0
     C     *BLANKS       LOOKUP    SY(Z)                                  60
     C     *IN60         IFEQ      '1'
     C     Z             ADD       30            A                 2 0
     C                   MOVE      '1'           *IN(A)
     C                   MOVE      '1'           *IN41
     C                   END                                                    END  *IN60
     C                   END                                                    END  DO
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C***************************
     C* PROCESS REQUEST SCREEN  *
     C***************************
     C     SCR02T        TAG
     C*
     C* BEGIN EDIT CHECKING
     C*
     C*  IF OPEN AMOUNT DOES NOT EQUAL CHECK AMOUNT - DISPLAY WARNING
     C*
     C     SCOAMT        IFNE      SCCAMT
     C                   MOVE      '1'           *IN25                          WARNING
     C                   ELSE
     C                   MOVE      '0'           *IN25
     C                   END                                                    ENDNE SCCAMT
     C*
     C*  VALIDATE THAT THE DESCRIPTIONS ARE NOT BLANK
     C*
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLE     10
     C     SY(X)         ANDNE     *BLANKS
     C     X             ADD       30            A                 2 0
     C     DC(X)         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN27                          NEEDS DESC
     C                   MOVE      '1'           *IN(A)
     C                   GOTO      STRT
     C                   END                                                    ENDIF *BLNKS
     C                   ADD       1             X
     C                   END                                                    END DOWLE 10
     C*
     C*  VALIDATE THAT SUM OF DISBURSED AMOUNTS EQUALS CHECK AMOUNT
     C*
     C                   Z-ADD     *ZERO         SCDAMT
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLE     10
     C                   ADD       DA(X)         SCDAMT            9 2          SUM OF DISB
     C                   ADD       1             X
     C                   END                                                    END DOWLE 10
     C*
     C     SCCAMT        IFNE      SCDAMT
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END                                                    ENDNE SCDAMT
     C*
     C*
     C*  ALL EDITS HAVE BEEN PASSED:  WRITE RECORDS TO A/R FILE
     C*
     C*
     C*  GET A/R TRANSACTION TYPE
     C*
     C                   MOVEL     *BLANKS       XXKEY
     C                   MOVEL     SCTRCD        XXKEY
     C     TBKEY         CHAIN     PAC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      ACTYPE        ARTRTP                         TRAN TYPE
     C                   END                                                    ENDIF *IN60
     C*
     C*  MOVE SCREEN FIELDS TO FILE FIELDS FOR NMARP
     C*
     C                   MOVE      UPCO#         ARCO#                          COMPANY #
     C                   MOVE      SCSUB#        ARSUB#                          10/18/89 CHG
     C                   MOVEL     SCFND         ARFND                          GROUP
     C                   MOVE      SCFYR         ARFYR                          FUND YEAR
     C                   MOVE      SCCASE        ARCASE                         CASE
     C                   MOVEL     SCTRCD        ARTRCD                         TRANS CODE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARPODT
     C*
     C                   MOVE      SCCK#         ARCK#                          CHECK NUMBER
     C                   MOVE      Q1USER        ARUSER                         USER
     C                   MOVE      Q1PGM         ARPGM                          PROGRAM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   Z-ADD     DSTIME        ARTIME                         TIME MAINT
     C                   MOVE      SCINV#        ARINV#                         INVOICE #
     C                   Z-ADD     *ZEROS        AREOB#                         EOB #
     C                   MOVE      *BLANKS       ARSTAT                         STATUS
     C*
     C*
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLE     10
     C     SY(X)         ANDNE     *BLANKS
     C     DA(X)         IFNE      *ZERO
     C                   MOVEL     SY(X)         ARSYS                          SYSTEM CODE
     C                   MOVEL     DC(X)         ARDESC                         DESCRIPTION
     C                   MOVE      DA(X)         ARAMT                          DISB CHK AMT
     C                   WRITE     PAR01
     C                   END                                                    ENDNE *ZERO
     C                   ADD       1             X
     C                   END                                                    END DOWLE
     C*
     C                   Z-ADD     *ZEROS        SCFYR
     C                   Z-ADD     *ZEROS        SCCASE
     C                   MOVEL     *BLANKS       SCNAME
     C                   MOVEL     *BLANKS       SCSS#
     C                   Z-ADD     *ZEROS        SCACDT
     C                   Z-ADD     *ZEROS        SCINV#
     C                   MOVEL     *BLANKS       SCTRCD
     C                   Z-ADD     *ZEROS        SCIAMT
     C                   Z-ADD     *ZEROS        SCOAMT
     C                   Z-ADD     *ZEROS        SCDAMT
     C                   Z-ADD     *ZEROS        SCCAMT
     C                   Z-ADD     *ZEROS        SCCK#
     C                   Z-ADD     *ZEROS        XXIAMT
     C                   Z-ADD     *ZEROS        XXOAMT
     C                   Z-ADD     *ZEROS        XXBAMT
     C                   MOVEL     *BLANKS       SY
     C                   Z-ADD     *ZEROS        IA
     C                   Z-ADD     *ZEROS        OA
     C                   Z-ADD     *ZEROS        DA
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C***********************
     C*  END THE PROGRAM    *
     C***********************
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
