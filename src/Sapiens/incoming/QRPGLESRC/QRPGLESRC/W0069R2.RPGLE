     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE W0069R2 - WCMC CHECK ISSUANCE CONTROL MAINT
     H*****************************************************************
     H* PROGRAM DESCRIPTION:  THIS PROGRAM ALLOWS THE USER TO DEFINE  *
     H*    THE VALUES USED TO CREATE THE WCMC CHECKS.                 *
     H* CALLED FROM: MENU                                             *
     H* CALLS TO:    NONE                                             *
     H* PROGRAMMER:  C:15                                             *
     H* CREATED:     07/24/95       PROJECT: 25WAR                    *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  05/04/17 C:309 Support# : 47069
     H*                 Run edits for CS fields only for Client id = '136'
     H*
     H*  07/14/16 C:309 Project: 136MGV
     H*                 Add new fields to hold CA, QME/AME, PT & DME payment trans and
     H*                 payment value 1 codes for regular and controverted.
     H*
     H*  03/19/12 C:079 SUPPORT: MC133 79545
     H*                 Correct error check on CA codes
     H*
     H*  10/14/10 C:190 PROJECT: WC1953
     H*                 Add 4 fields to hold CA payment trans and
     H*                 payment value 1 codes for regular and controverted.
     H*
     H*  04/07/05       PROJECT: 126WDK
     H*           C:62  PROJECT: 126MAD
     H*                 TAKE OUT EDIT FOR EXPENSE AND MEDICAL TRCDS.
     H*
     H*  01/21/99 C:94  PROJECT: 48MBV
     H*                 TAKE 'V' OUT OF PROGRAM
     H*
     H*  11/04/98 C:31  Project: 48MBV
     H*                 V4: Auto check issuance
     H*                 1. Y2K conversion
     H*                 2. Replace WTABL1 with WTPSL
     H*                                                               *
     H* 12/04/95  C:33  PROJECT# 25SUPT                               *
     H*                 AFTER CMD 13-CHG USER PARMS IS EXECUTED ALLOW *
     H*                 PROGRAM TO USE NEW PARMS                      *
     H*                                                               *
     H* 06/01/96  C:53  PROJECT# 25MGX                                *
     H*                 ALLOW 'V' ENTRY FOR SYSTEM                    *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*     40  -  NO HIT ON PRIMARY FILE                             *
     H*     60  -  GENERAL USAGE                                      *
     H*   SCR01                                                       *
     H*     20  -  RECORD NOT FOUND                                   *
     H*     21  -  INVALID PAYEE TYPE                                 *
     H*     22  -  INVALID SYSTEM TYPE                                *
     H*   SCR02                                                       *
     H*     20  -  INVALID PMT TRANS CODE                             *
     H*     21  -  RESERVE TYPE MUST BE 'M'                           *
     H*     22  -  INVALID CONTROVERT PMT TRANS CODE                  *
     H*     23  -  RESERVE TYPE MUST BE 'E'                           *
     H*     24  -  INVALID PMT VAL 1 CODE                             *
     H*     25  -  INVALID PMT STATUS CODE                            *
     H*     26  -  PMT STATUS CODE MUST BE A 'P' TYPE                 *
     H*     27  -  INVALID PMT VAL 2 CODE                             *
     H*     28  -  PMT VAL 2 CODE MUST BE A LOST TIME TYPE            *
     H*     29  -  INVALID PMT VAL 2 CODE                             *
     H*     30  -  PMT VAL 2 CODE MUST BE A NO LOST TIME TYPE         *
     H*     31  -  INVALID PAYEE ENTERED                              *
     H*     32  -  POSITION CURSOR ON CONTRO PMT TRANS FIELD          *
     H*     33  -  POSITION CURSOR ON PMT VAL 1 FIELD                 *
     H*     34  -  POSITION CURSOR ON PMT STATUS FIELD                *
     H*     35  -  POSITION CURSOR ON PMT VAL 2 - LOST TIME FIELD     *
     H*     36  -  POSITION CURSOR ON PMT VAL 2 - NO LOST TIME FIELD  *
     H*     37  -  POSITION CURSOR ON FEIN FIELD                      *
     H*     38  -  DISPLAY THE FEIN/MOD FIELDS                        *
     H*     43  -  INVALID CONTROVERT PMT VAL 1 CODE                  *
     H*     44  -  POSITION CURSOR ON CONTROVERT PMT VAL 1 CODE       *
     H*****************************************************************
     H* FILE USAGE:                                                   *
     H*     WMUPL (PUP01): Q1USER                                     *
     H*     SMCOL (PCO01): CO#                                        *
     H*     WMFNL (PFN01): CO#/FND                                    *
     H*     WMCIL (PCI01): CO#/FND/PTYP/SYS                           *
     H*     WMPDL (PPD01): FEIN/MOD                                   *
     H*     WTABL (P--01): CO#/KEY                                    *
     H*     WTABL1(P--01): CO#/KEY                                    *
     H*****************************************************************
     FW0069F2   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMCIL     UF A E           K DISK
     FWMPDL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F*** BEGIN CHG *** 11/04/98 **************************************
     F********WTABL1  IF  E           K        DISK
     FWTPSL     IF   E           K DISK
     F*** END CHG *** 11/04/98 ****************************************

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D DSC             S             21    DIM(8) CTDATA PERRCD(1)              CHG 06/01/96
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE COMPANY MASTER FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE WCMC CHECK ISSUANCE CONTROL FILE
     C*
     C     CIKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCPTYP
     C                   KFLD                    SCSYS
     C*
     C*  KEY LIST FOR THE PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  KEY LIST FOR THE TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  ENTRY PARAMETER LIST
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
     C     *LIKE         DEFINE    CICO#         XXCO#
     C     *LIKE         DEFINE    CIFND         XXFND
     C     *LIKE         DEFINE    PDFEIN        XXFEIN
     C     *LIKE         DEFINE    PDMOD         XXMOD
     C     *LIKE         DEFINE    PSKEY         XXKEY
     C     *LIKE         DEFINE    PSKEY         SCKEY
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      *BLANK        SCPTYP
     C                   MOVE      *BLANK        SCSYS
     C     QQCALL        COMP      '1'                                    09
     C*
     C     STRT01        TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C                   Z-ADD     0             XXCO#
     C                   Z-ADD     0             XXFND
     C     UPKY          CHAIN     PUP01                              60
     C  N60              Z-ADD     UPCO#         XXCO#
     C  N60              Z-ADD     UPFND         XXFND
     C*
     C*  GET THE COMPANY RECORD
     C*
     C                   MOVE      *BLANKS       SCCONM
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE FUND MASTER FILE
     C*
     C                   MOVE      *BLANKS       SCNAME
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        SCNAME
     C                   Z-ADD     XXFND         SCFND
     C*
    C***** Beg Add *** 07/14/16 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C*
     C                   MOVE      *Off          dspFld136
     C     ##CLT#        IFEQ      '136'
     C                   MOVE      *On           dspFld136
     C                   ENDIF
    C***** End Add *** 07/14/16 **************************************
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
     C                   EXFMT     SCR02
     C                   END
     C**************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C******************************************
     C*   CMD 1-END OF JOB                     *
     C******************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C******************************************
     C*   CMD 2-PRIOR SCREEN                   *
     C******************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*** BEGIN DEL *** 11/04/98 **************************************
     C******************************************
     C****CMD*5-PROVIDER # SEARCH              *
     C******************************************
     C********   *INKE     IFEQ '1'
     C********
     C********   Q2RECF    IFEQ 'SCR02'
     C********             CALL 'W0531R'
     C********             PARM           QQPRDS
     C********             PARM           QQCALL
     C********             PARM           SCFEIN
     C********             PARM           SCMOD
     C********             PARM           QQRTCD  1
     C********   QQRTCD    CABEQ'A'       TERM
     C********   QQRTCD    CABEQ'B'       STRT           37
     C********   SCKEY     CABEQ*BLANKS   STRT           37
     C********             END
     C********
     C********             GOTO STRT
     C********             END
     C******************************************
     C***CMD*10-TABLE FILE SEARCH              *
     C******************************************
     C********   *INKJ     IFEQ '1'
     C********   Q2RECF    IFEQ 'SCR02'
     C********
     C*****PMT TRANSACTION CODE SEARCH
     C********
     C********             MOVELSCTRCD    XX2     2
     C********   XX2       IFEQ '? '
     C********
     C********             MOVE *BLANK    SCKEY
     C********             CALL 'W2141R'
     C********             PARM           QQPRDS
     C********             PARM           QQCALL
     C********             PARM           SCKEY
     C********             PARM           QQRTCD
     C********             MOVELSCKEY     SCTRCD
     C********   QQRTCD    CABEQ'A'       TERM
     C********   QQRTCD    CABEQ'B'       STRT
     C********   SCKEY     CABEQ*BLANKS   STRT
     C********   QQRTCD    CABEQ'*'       STRT           32
     C********
     C********             GOTO STRT
     C********             END
     C********
     C*****CONTROVERT PMT TRANSACTION CODE SEARCH
     C********
     C********             MOVELSCTRCV    XX2     2
     C********   XX2       IFEQ '? '
     C********
     C********             MOVE *BLANK    SCKEY
     C********             CALL 'W2141R'
     C********             PARM           QQPRDS
     C********             PARM           QQCALL
     C********             PARM           SCKEY
     C********             PARM           QQRTCD
     C********             MOVELSCKEY     SCTRCV
     C********   QQRTCD    CABEQ'A'       TERM
     C********   QQRTCD    CABEQ'B'       STRT           32
     C********   SCKEY     CABEQ*BLANKS   STRT           32
     C********   QQRTCD    CABEQ'*'       STRT           33
     C********
     C********             GOTO STRT
     C********             END
     C********
     C*****PMT VAL 1 CODE SEARCH
     C********
     C********             MOVELSCBR2     XX2
     C********   XX2       IFEQ '? '
     C********
     C********             MOVE *BLANK    SCKEY
     C********             CALL 'W2146R'
     C********             PARM           QQPRDS
     C********             PARM           QQCALL
     C********             PARM           SCKEY
     C********             PARM           QQRTCD
     C********             MOVELSCKEY     SCBR2
     C********   QQRTCD    CABEQ'A'       TERM
     C********   QQRTCD    CABEQ'B'       STRT           33
     C********   SCKEY     CABEQ*BLANKS   STRT           33
     C********   QQRTCD    CABEQ'*'       STRT           44
     C********
     C********             GOTO STRT
     C********             END
     C********
     C*****CONTROVERT PMT VAL 1 CODE SEARCH
     C********
     C********             MOVELSCP1CV    XX2
     C********   XX2       IFEQ '? '
     C********
     C********             MOVE *BLANK    SCKEY
     C********             CALL 'W2146R'
     C********             PARM           QQPRDS
     C********             PARM           QQCALL
     C********             PARM           SCKEY
     C********             PARM           QQRTCD
     C********             MOVELSCKEY     SCP1CV
     C********   QQRTCD    CABEQ'A'       TERM
     C********   QQRTCD    CABEQ'B'       STRT           44
     C********   SCKEY     CABEQ*BLANKS   STRT           44
     C**N39***   QQRTCD    CABEQ'*'       STRT           35
     C***39***   QQRTCD    CABEQ'*'       STRT           42
     C********
     C********             GOTO STRT
     C********             END
     C********
     C*****PHARMACY PMT VAL 1 CODE SEARCH
     C********
     C********             MOVELSCXBR2    XX2
     C********   XX2       IFEQ '? '
     C********
     C********             MOVE *BLANK    SCKEY
     C********             CALL 'W2146R'
     C********             PARM           QQPRDS
     C********             PARM           QQCALL
     C********             PARM           SCKEY
     C********             PARM           QQRTCD
     C********             MOVELSCKEY     SCXBR2
     C********   QQRTCD    CABEQ'A'       TERM
     C********   QQRTCD    CABEQ'B'       STRT           42
     C********   SCKEY     CABEQ*BLANKS   STRT           42
     C********   QQRTCD    CABEQ'*'       STRT           35
     C********
     C********             GOTO STRT
     C********             END
     C********
     C*****PMT VAL 2 - LOST TIME CODE SEARCH
     C********
     C********             MOVELSCBR7L    XX2     2
     C********   XX2       IFEQ '? '
     C********
     C********             MOVE *BLANK    SCKEY
     C********             CALL 'W2151R'
     C********             PARM           QQPRDS
     C********             PARM           QQCALL
     C********             PARM           SCKEY
     C********             PARM           QQRTCD
     C********             MOVELSCKEY     SCBR7L
     C********   QQRTCD    CABEQ'A'       TERM
     C********   QQRTCD    CABEQ'B'       STRT           35
     C********   SCKEY     CABEQ*BLANKS   STRT           35
     C********   QQRTCD    CABEQ'*'       STRT           36
     C********
     C********             GOTO STRT
     C********             END
     C********
     C*****PMT VAL 2 - NO LOST TIME CODE SEARCH
     C********
     C********             MOVELSCBR7N    XX2
     C********   XX2       IFEQ '? '
     C********
     C********             MOVE *BLANK    SCKEY
     C********             CALL 'W2151R'
     C********             PARM           QQPRDS
     C********             PARM           QQCALL
     C********             PARM           SCKEY
     C********             PARM           QQRTCD
     C********             MOVELSCKEY     SCBR7N
     C********   QQRTCD    CABEQ'A'       TERM
     C********   QQRTCD    CABEQ'B'       STRT           36
     C********   SCKEY     CABEQ*BLANKS   STRT           36
     C********   QQRTCD    CABEQ'*'       STRT           34
     C********
     C********             GOTO STRT
     C********             END
     C********
     C*****PENDED PMT STATUS CODE SEARCH
     C********
     C********             MOVELSCSTS     XX2     2
     C********   XX2       IFEQ '? '
     C********
     C********             MOVE *BLANK    SCKEY
     C********             CALL 'W2143R'
     C********             PARM           QQPRDS
     C********             PARM           QQCALL
     C********             PARM           SCKEY
     C********             PARM           QQRTCD
     C********             MOVELSCKEY     SCSTS
     C********   QQRTCD    CABEQ'A'       TERM
     C********   QQRTCD    CABEQ'B'       STRT           34
     C********   SCKEY     CABEQ*BLANKS   STRT           34
     C********   QQRTCD    CABEQ'*'       STRT           37
     C********
     C********             GOTO STRT
     C********             END
     C*** END DEL *** 11/04/98 ****************************************
     C*** BEGIN ADD *** 11/04/98 **************************************
     C******************************************
     C*  CMD 10  SEARCH                        *
     C******************************************
    C****** Beg Add *** 07/14/16 *****************************
      /Free
         IndFoCTRCD = *Off;
         IndFoCTRCV = *Off;
         IndFoQTRCD = *Off;
         IndFoQTRCV = *Off;
         IndFoPTRCD = *Off;
         IndFoPTRCV = *Off;
         IndFoDTRCD = *Off;
         IndFoDTRCV = *Off;
         IndFoCPV1  = *Off;
         IndFoCPV1C = *Off;
         IndFoQPV1  = *Off;
         IndFoPPV1C = *Off;
         IndFoPPV1  = *Off;
         IndFoDPV1  = *Off;
         IndFoDPV1C = *Off;
         erDPV1CV   = *Off;
         msDPV1CV   = *Blanks;
         erDPV1     = *Off;
         msDPV1     = *Blanks;
         erPPV1CV   = *Off;
         msPPV1CV   = *Blanks;
         erPPV1     = *Off;
         msPPV1     = *Blanks;
         erQPV1CV   = *Off;
         msQPV1CV   = *Blanks;
         erCPV1CV   = *Off;
         msCPV1CV   = *Blanks;
         erQPV1     = *Off;
         msQPV1     = *Blanks;
         erCPV1     = *Off;
         msCPV1     = *Blanks;
         erCTRCD    = *Off;
         msCTRCD    = *Blanks;
         erCTRCD1   = *Off;
         msCTRCD1   = *Blanks;
         erCTRCDCV  = *Off;
         msCTRCDCV  = *Blanks;
         erCTRCDCV1 = *Off;
         msCTRCDCV1 = *Blanks;
         erQTRCD    = *Off;
         msQTRCD    = *Blanks;
         erQTRCD1   = *Off;
         msQTRCD1   = *Blanks;
         erQTRCDCV  = *Off;
         msQTRCDCV  = *Blanks;
         erQTRCDCV1 = *Off;
         msQTRCDCV1 = *Blanks;
         erPTRCD    = *Off;
         msPTRCD    = *Blanks;
         erPTRCD1   = *Off;
         msPTRCD1   = *Blanks;
         erPTRCDCV  = *Off;
         msPTRCDCV  = *Blanks;
         erPTRCDCV1 = *Off;
         msPTRCDCV1 = *Blanks;
         erDTRCD    = *Off;
         msDTRCD    = *Blanks;
         erDTRCD1   = *Off;
         msDTRCD1   = *Blanks;
         erDTRCDCV  = *Off;
         msDTRCDCV  = *Blanks;
         erDTRCDCV1 = *Off;
         msDTRCDCV1 = *Blanks;
      /End-Free
    C****** End Add *** 07/14/16 *****************************

     C     *INKJ         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C*             PAYMENT TRANSACTION CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCTRCD'
     C                   CALL      'W2141R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCTRCD
     C                   SETON                                        32
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = TRCD
     C*
     C*             CONTROVERT PAYMENT TRANSACTION CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCTRCV'
     C                   CALL      'W2141R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCTRCV
     C*** BEG   CHG *** 10/14/10 **************************************
     C*************        SETON                     33
    C***** Beg Add *** 07/14/16 *****************************************
     C     *IN08         IFEQ      *On
    C***** End Add *** 07/14/16 *****************************************
     C                   SETON                                        62
    C***** Beg Add *** 07/14/16 *****************************************
     C                   ELSE                                                   *INKL = Off
     C                   Eval      IndFoCTRCD = *On
     C                   ENDIF                                                  *INKL = Off
    C***** End Add *** 07/14/16 *****************************************
     C*** END   CHG *** 10/14/10 **************************************
     C                   ELSE                                                   *INKL = Off
     C                   SETON                                        32
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = TRCV
     C*
     C*** BEG   ADD *** 10/14/10 **************************************
     C*          CA PAYMENT TRANSACTION CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCCATR'
     C                   CALL      'W2141R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCCATR
     C                   SETON                                        64
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = TRCD
     C*
     C*             CONTROVERT PAYMENT TRANSACTION CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCCATC'
     C                   CALL      'W2141R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCCATC
    C***** Beg Chg *** 07/14/16 *****************************************
     C*****              SETON                                        33
     C                   Eval      IndFoCTRCD = *On
    C***** End Chg *** 07/14/16 *****************************************
     C                   ELSE                                                   *INKL = Off
     C                   SETON                                        64
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = TRCV
     C*
     C*** END   ADD *** 10/14/10 **************************************
    C***** Beg Add *** 07/14/16 **************************************
     C*          CS PAYMENT TRANSACTION CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCCTRCD'
     C                   CALL      'W2141R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCCTRCD
     C                   EVAL      IndFoCTRCV = *On
     C                   ELSE
     C                   EVAL      IndFoCTRCD = *On
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = TRCD
     C*
     C*          CS CONTROVERT PAYMENT TRANSACTION CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCCTRCDCV'
     C                   CALL      'W2141R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCCTRCDCV
     C                   EVAL      IndFoQTRCD = *On
     C                   ELSE
     C                   EVAL      IndFoCTRCV = *On
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = TRCV
     C*
     C*          QME/AME PAYMENT TRANSACTION CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCQTRCD'
     C                   CALL      'W2141R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCQTRCD
     C                   EVAL      IndFoQTRCV = *On
     C                   ELSE
     C                   EVAL      IndFoQTRCD = *On
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = TRCD
     C*
     C*          QME/AME CONTROVERT PAYMENT TRANSACTION CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCQTRCDCV'
     C                   CALL      'W2141R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCQTRCDCV
     C                   EVAL      IndFoPTRCD = *On
     C                   ELSE
     C                   EVAL      IndFoQTRCV = *On
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = TRCV
     C*
     C*          PT PAYMENT TRANSACTION CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCPTRCD'
     C                   CALL      'W2141R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCPTRCD
     C                   EVAL      IndFoPTRCV = *On
     C                   ELSE
     C                   EVAL      IndFoPTRCD = *On
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = TRCD
     C*
     C*          PT CONTROVERT PAYMENT TRANSACTION CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCPTRCDCV'
     C                   CALL      'W2141R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCPTRCDCV
     C                   EVAL      IndFoDTRCD = *On
     C                   ELSE
     C                   EVAL      IndFoPTRCV = *On
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = TRCV
     C*
     C*          DME PAYMENT TRANSACTION CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCDTRCD'
     C                   CALL      'W2141R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCDTRCD
     C                   EVAL      IndFoDTRCV = *On
     C                   ELSE
     C                   EVAL      IndFoDTRCD = *On
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = TRCD
     C*
     C*          DME CONTROVERT PAYMENT TRANSACTION CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCDTRCDCV'
     C                   CALL      'W2141R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCDTRCDCV
     C                   SETON                                        33
     C                   ELSE
     C                   EVAL      IndFoDTRCV = *On
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = TRCV
    C***** End Add *** 07/14/16 **************************************
     C*
     C*             PAYMENT VAL 1 CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCBR2'
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCBR2
     C                   SETON                                        44
     C                   ELSE                                                   *INKL = Off
     C                   SETON                                        33
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = BR2
     C*
     C*             CONTROVERT PAYMENT VAL 1 CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCP1CV'
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCP1CV
     C*** BEG   CHG *** 10/14/10 **************************************
     C** 39*******         SETON                     42
     C**N39*******         SETON                     35
    C***** Beg Add *** 07/14/16 *****************************************
     C     *IN08         IFEQ      *On
    C***** End Add *** 07/14/16 *****************************************
     C                   SETON                                        67
    C***** Beg Add *** 07/14/16 *****************************************
     C                   ELSE                                                   *INKL = Off
     C                   Eval      IndFoCPV1 = *On
     C                   ENDIF                                                  *INKL = Off
    C***** End Add *** 07/14/16 *****************************************
     C*** END   CHG *** 10/14/10 **************************************
     C                   ELSE                                                   *INKL = Off
     C                   SETON                                        44
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = P1CV
     C*** BEG   ADD *** 10/14/10 **************************************
     C*          CA PAYMENT VAL 1 CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCCAB2'
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCCAB2
     C                   SETON                                        69
     C                   ELSE                                                   *INKL = Off
     C                   SETON                                        67
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = AR2
     C*
     C*             CONTROVERT PAYMENT VAL 1 CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCCABC'
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCCABC
    C***** Beg Chg *** 07/14/16 **************************************
     C*  39              SETON                                        42
     C* N39              SETON                                        35
     C                   Eval      IndFoCPV1 = *On
    C***** End Chg *** 07/14/16 **************************************
     C                   ELSE
     C                   SETON                                        69
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = P1CV
     C*** BEG ADD *** 10/14/10 **************************************
     C*
    C***** Beg Add *** 07/14/16 **************************************
     C*          CS PAYMENT VAL 1 CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCCPV1'
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCCPV1
     C                   Eval      IndFoCPV1C=*On
     C                   ELSE                                                   *INKL = Off
     C                   Eval      IndFoCPV1 =*On
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = AR2
     C*
     C*          CS CONTROVERT PAYMENT VAL 1 CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCCPV1CV'
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCCPV1CV
     C                   EVAL      IndFoQPV1  = *On
     C                   ELSE
     C                   EVAL      IndFoCPV1C = *On
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = P1CV
     C*
     C*          QME/AME PAYMENT VAL 1 CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCQPV1'
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCQPV1
     C                   Eval      IndFoQPV1C= *On
     C                   ELSE                                                   *INKL = Off
     C                   Eval      IndFoQPV1 = *On
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = AR2
     C*
     C*          QME/AME CONTROVERT PAYMENT VAL 1 CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCQPV1CV'
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCQPV1CV
     C                   EVAL      IndFoPPV1  = *On
     C                   ELSE
     C                   EVAL      IndFoQPv1C = *On
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = P1CV
     C*
     C*
     C*          PT PAYMENT VAL 1 CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCPPV1'
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCPPV1
     C                   Eval      IndFoPPV1C= *On
     C                   ELSE                                                   *INKL = Off
     C                   Eval      IndFoPPV1 = *On
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = AR2
     C*
     C*          PT CONTROVERT PAYMENT VAL 1 CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCPPV1CV'
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCPPV1CV
     C                   EVAL      IndFoDPV1  = *On
     C                   ELSE
     C                   EVAL      IndFoPPV1C = *On
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = P1CV
     C*
     C*
     C*          DME PAYMENT VAL 1 CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCDPV1'
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCDPV1
     C                   Eval      IndFoDPV1C= *On
     C                   ELSE
     C                   Eval      IndFoDPV1 = *On
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = AR2
     C*
     C*          DME CONTROVERT PAYMENT VAL 1 CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCDPV1CV'
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCDPV1CV
     C   39              SETON                                        42
     C  N39              SETON                                        35
     C                   ELSE
     C                   EVAL      IndFoDPV1C = *On
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = P1CV
     C*
    C***** End Add *** 07/14/16 **************************************
     C*
     C*             PHARMACY PAYMENT VAL 1 CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCXBR2'
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCXBR2
     C                   SETON                                        35
     C                   ELSE                                                   *INKL = Off
     C                   SETON                                        42
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = XBR2
     C*
     C*             PAYMENT VALUE 2 - LOST TIME CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCBR7L'
     C                   CALL      'W2151R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCBR7L
     C                   SETON                                        36
     C                   ELSE                                                   *INKL = Off
     C                   SETON                                        35
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = BR7L
     C*
     C*             PAYMENT VALUE 2 - NO LOST TIME CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCBR7N'
     C                   CALL      'W2151R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCBR7N
     C                   SETON                                        34
     C                   ELSE                                                   *INKL = Off
     C                   SETON                                        36
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = BR7N
     C*
     C*             PENDED PAYMENT STATUS CODE SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCSTS'
     C                   CALL      'W2142R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C     *INKL         IFEQ      *OFF
     C     ##KEY         ANDNE     *BLANKS
     C                   MOVEL     ##KEY         SCSTS
     C   38              SETON                                        37
     C                   ELSE                                                   *INKL = Off
     C                   SETON                                        34
     C                   ENDIF                                                  *INKL = Off
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = STS
     C*
     C*             PROVIDER/MOD SEARCH
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
     C                   MOVE      SCFEIN        QQFEIN            9
     C                   MOVE      SCMOD         QQMOD             3
     C                   MOVE      '2'           QQCALL
     C                   CALL      'W0030R'
     C                   PARM                    QQCALL
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM      ' '           QQRTCD            1
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF                                                  RTCD = 'A'
     C     QQRTCD        IFEQ      'B'
     C                   SETON                                        37
     C                   END                                                    RTCD = 'B'
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL     QQFEIN        SCFEIN
     C                   MOVEL     QQMOD         SCMOD
     C                   ENDIF                                                  RTCD = '*'
     C                   ENDIF                                                  HFLD = FEIN
     C*** END ADD *** 11/04/98 ****************************************
     C*
     C                   GOTO      STRT
     C                   END
     C                   END
     C******************************************
     C*   PROCESS HELP REQUESTS                *
     C******************************************
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
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C******************************************
     C*   CMD 13-CHG USER PARAMETERS           *
     C******************************************
     C     *INKM         IFEQ      '1'
     C                   CALL      'W0006R'
     C***  BEGIN CHG  ***  12/04/95  *********************************
     C**********           GOTO STRT
     C                   GOTO      STRT01
     C***  END   CHG  ***  12/04/95  *********************************
     C                   END
     C*****************************************************************
     C                   END                                                    END CMDKEYS
     C******************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C*******************************************
     C     SCR01T        TAG
     C*******************************************
     C*
     C*  THE PAYEE TYPE MUST BE P/B/N
     C*
     C     SCPTYP        IFNE      'P'
     C     SCPTYP        ANDNE     'B'
     C     SCPTYP        ANDNE     'N'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF THE PAYEE TYPE IS P OR N, THE SYSTEM TYPE MUST BE P/H/N.
     C*  IF THE PAYEE TYPE IS P, INCLUDE SYSTEM TYPE OF V.     06/01/96
     C*
     C     SCPTYP        IFEQ      'P'
     C********** SCPTYP ** OREQ 'N' ***                    DEL 06/01/96
     C     SCSYS         IFNE      'P'
     C     SCSYS         ANDNE     'H'
     C     SCSYS         ANDNE     'N'
     C*** BEG DEL *** 01/21/99 ***************************************
     C********** SCSYS     ANDNE'V'                        ADD 06/01/96
     C*** END DEL *** 01/21/99 ***************************************
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
     C*** BEGIN ADD 06/01/96 ******************************
     C*
     C     SCPTYP        IFEQ      'N'
     C     SCSYS         IFNE      'P'
     C     SCSYS         ANDNE     'H'
     C     SCSYS         ANDNE     'N'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
     C*** END   ADD 06/01/96 ******************************
     C*
     C*  IF THE PAYEE TYPE IS B, THE SYSTEM TYPE MUST BE P/H/N/U/M.
     C*
     C     SCPTYP        IFEQ      'B'
     C     SCSYS         IFNE      'P'
     C     SCSYS         ANDNE     'H'
     C     SCSYS         ANDNE     'N'
     C     SCSYS         ANDNE     'U'
     C     SCSYS         ANDNE     'M'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  LOAD THE DESCRIPTION FOR THE PAYEE TYPE AND THE SYSTEM TYPE.
     C*
     C     SCPTYP        IFEQ      'P'
     C                   MOVE      DSC(1)        SCPDSC
     C                   ELSE
     C     SCPTYP        IFEQ      'B'
     C                   MOVE      DSC(2)        SCPDSC
     C                   ELSE
     C                   MOVE      DSC(3)        SCPDSC
     C                   END
     C                   END
     C*
     C     SCSYS         IFEQ      'P'
     C                   MOVE      DSC(4)        SCSDSC
     C                   ELSE
     C     SCSYS         IFEQ      'H'
     C                   MOVE      DSC(5)        SCSDSC
     C                   ELSE
     C     SCSYS         IFEQ      'U'
     C                   MOVE      DSC(6)        SCSDSC
     C                   ELSE
     C     SCSYS         IFEQ      'N'
     C                   MOVE      DSC(7)        SCSDSC
     C                   ELSE
     C*** BEG DEL *** 01/21/99 ***************************************
     C*** BEGIN ADD 06/01/96 ******************************
     C********** SCSYS     IFEQ 'V'
     C**********           MOVE DSC,9     SCSDSC
     C**********           ELSE
     C*** END   ADD 06/01/96 ******************************
     C*** END DEL *** 01/21/99 ***************************************
     C                   MOVE      DSC(8)        SCSDSC
     C*** BEG DEL *** 01/21/99 ***************************************
     C**********           END
     C*** END DEL *** 01/21/99 ***************************************
     C                   END
     C                   END
     C                   END
     C                   ENDIF                                                  ADD 06/01/96
     C*
     C*  IF THE PAYEE TYPE IS 'B', DISPLAY THE FEIN/MOD
     C*
     C                   MOVE      '0'           *IN39
     C     SCPTYP        IFEQ      'P'
     C     SCSYS         ANDEQ     'P'
     C                   MOVE      '1'           *IN39
     C                   END
     C*
     C*  IF THE PAYEE TYPE IS 'B', DISPLAY THE FEIN/MOD
     C*
     C     SCPTYP        COMP      'B'                                    38
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     CIKY          CHAIN(N)  PCI01                              40
     C     *IN40         IFEQ      '0'
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      CITRCD        SCTRCD
     C                   MOVE      CITRCV        SCTRCV
     C                   MOVE      CIBR2         SCBR2
     C                   MOVE      CIP1CV        SCP1CV
     C                   MOVE      CIXBR2        SCXBR2
     C                   MOVE      CISTS         SCSTS
     C                   MOVE      CIBR7L        SCBR7L
     C                   MOVE      CIBR7N        SCBR7N
     C                   Z-ADD     CIFEIN        SCFEIN
     C                   Z-ADD     CIMOD         SCMOD
     C*** BEG   ADD *** 10/14/10 **************************************
     C                   MOVE      CICATR        SCCATR
     C                   MOVE      CICATC        SCCATC
     C                   MOVE      CICAB2        SCCAB2
     C                   MOVE      CICABC        SCCABC
     C*** END   ADD *** 10/14/10 **************************************
    C***** Beg Add *** 07/14/16 **************************************
     C                   MOVE      CICTRCD       SCCTRCD
     C                   MOVE      CICTRCDCV     SCCTRCDCV
     C                   MOVE      CICPV1        SCCPV1
     C                   MOVE      CICPV1CV      SCCPV1CV
     C                   MOVE      CIQTRCD       SCQTRCD
     C                   MOVE      CIQTRCDCV     SCQTRCDCV
     C                   MOVE      CIQPV1        SCQPV1
     C                   MOVE      CIQPV1CV      SCQPV1CV
     C                   MOVE      CIPTRCD       SCPTRCD
     C                   MOVE      CIPTRCDCV     SCPTRCDCV
     C                   MOVE      CIPPV1        SCPPV1
     C                   MOVE      CIPPV1CV      SCPPV1CV
     C                   MOVE      CIDTRCD       SCDTRCD
     C                   MOVE      CIDTRCDCV     SCDTRCDCV
     C                   MOVE      CIDPV1        SCDPV1
     C                   MOVE      CIDPV1CV      SCDPV1CV
    C***** End Add *** 07/14/16 **************************************
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCTRCD
     C                   MOVE      *BLANKS       SCTRCV
     C                   MOVE      *BLANKS       SCBR2
     C                   MOVE      *BLANKS       SCP1CV
     C                   MOVE      *BLANKS       SCXBR2
     C                   MOVE      *BLANKS       SCSTS
     C                   MOVE      *BLANKS       SCBR7L
     C                   MOVE      *BLANKS       SCBR7N
     C                   Z-ADD     *ZEROS        SCFEIN
     C                   Z-ADD     *ZEROS        SCMOD
     C*** BEG   ADD *** 10/14/10 **************************************
     C                   MOVE      *BLANKS       SCCATR
     C                   MOVE      *BLANKS       SCCATC
     C                   MOVE      *BLANKS       SCCAB2
     C                   MOVE      *BLANKS       SCCABC
     C*** END   ADD *** 10/14/10 **************************************
    C***** Beg Add *** 07/14/16 **************************************
     C                   MOVE      *BLANKS       SCCTRCD
     C                   MOVE      *BLANKS       SCCTRCDCV
     C                   MOVE      *BLANKS       SCCPV1
     C                   MOVE      *BLANKS       SCCPV1CV
     C                   MOVE      *BLANKS       SCQTRCD
     C                   MOVE      *BLANKS       SCQTRCDCV
     C                   MOVE      *BLANKS       SCQPV1
     C                   MOVE      *BLANKS       SCQPV1CV
     C                   MOVE      *BLANKS       SCPTRCD
     C                   MOVE      *BLANKS       SCPTRCDCV
     C                   MOVE      *BLANKS       SCPPV1
     C                   MOVE      *BLANKS       SCPPV1CV
     C                   MOVE      *BLANKS       SCDTRCD
     C                   MOVE      *BLANKS       SCDTRCDCV
     C                   MOVE      *BLANKS       SCDPV1
     C                   MOVE      *BLANKS       SCDPV1CV
    C***** End Add *** 07/14/16 **************************************
     C*
     C                   END
     C*
     C*** BEG   CHG *** 10/14/10 **************************************
     C                   MOVE      *OFF          *IN08
     C     SCPTYP        IFEQ      'N'
     C     SCSYS         IFEQ      'P'
     C     SCSYS         OREQ      'H'
     C                   MOVE      *ON           *IN08
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 10/14/10 **************************************
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*******************************************
     C     SCR02T        TAG
     C*******************************************
     C     *IN09         CABEQ     '1'           INQ2
     C*
     C*  VALIDATE THE PMT TRANSACTION CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCTRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              20
     C     *IN20         CABEQ     '1'           STRT
     C     PCSTAT        CABEQ     'I'           STRT                     20
     C*
     C*  THE TRANS CODE RESERVE TYPE MUST BE AN 'M' TYPE.
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     PCRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              21
     C     *IN21         CABEQ     '1'           STRT
     C     PCSTAT        CABEQ     'I'           STRT                     21
     C***  BEGIN DEL  ***  04/07/05 ***********************************
     C********** RTTYPE    CABNE'M'       STRT       2121
     C***  END   DEL  ***  04/07/05 ***********************************
     C*
     C*  VALIDATE THE CONTROVERT PMT TRANSACTION CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCTRCV        XXKEY
     C     TBKY          CHAIN     PPC01                              22
     C     *IN22         CABEQ     '1'           STRT
     C     PCSTAT        CABEQ     'I'           STRT                     22
     C*
     C*  THE CONTROVERT TRANS CODE RESERVE TYPE MUST BE AN 'E' TYPE.
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     PCRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              23
     C     *IN23         CABEQ     '1'           STRT
     C     PCSTAT        CABEQ     'I'           STRT                     23
     C***  BEGIN DEL  ***  04/07/05 ***********************************
     C********** RTTYPE    CABNE'E'       STRT       2323
     C***  END   DEL  ***  04/07/05 ***********************************
     C*** BEG   ADD *** 10/14/10 **************************************
     C*
     C*** BEG   CHG *** 03/19/12 **************************************
     C******     *IN08     IFEQ *OFF
     C     *IN08         IFEQ      *ON
     C*** END   CHG *** 03/19/12 **************************************
     C*
     C*  VALIDATE THE CA PMT TRANSACTION CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCATR        XXKEY
     C     TBKY          CHAIN     PPC01                              63
     C     *IN63         CABEQ     '1'           STRT
     C     PCSTAT        CABEQ     'I'           STRT                     63
     C*
     C*  THE TRANS CODE RESERVE TYPE MUST BE AN 'E' TYPE.
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     PCRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              71
     C     *IN71         CABEQ     '1'           STRT
     C     PCSTAT        CABEQ     'I'           STRT                     71
     C     RTTYPE        CABNE     'E'           STRT                 7171
      *
     C*  VALIDATE THE CONTROVERT PMT TRANSACTION CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCATC        XXKEY
     C     TBKY          CHAIN     PPC01                              65
     C     *IN65         CABEQ     '1'           STRT
     C     PCSTAT        CABEQ     'I'           STRT                     65
      *
     C*  THE CA CONTROVERT TRANS CODE RESERVE TYPE MUST BE AN 'E' TYPE.
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     PCRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              66
     C     *IN66         CABEQ     '1'           STRT
     C     PCSTAT        CABEQ     'I'           STRT                     66
     C     RTTYPE        CABNE     'E'           STRT                 6666
     C*
     C                   ENDIF
     C*
     C*** END   ADD *** 10/14/10 **************************************
     C*
     C***** Beg Add *** 05/04/17 **************************************
    C*** Run below edits only for Client = '136'
     C     dspFld136     IFEQ      *ON
     C***** End Add *** 05/04/17 **************************************

    C***** Beg Add *** 07/14/16 **************************************
     C*
    C*   CS Fields Validations
     C*
     C*  VALIDATE THE CS PMT TRANSACTION CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCTRCD       XXKEY
     C     TBKY          CHAIN     PPC01
      /Free
           erCTRCD = *Off;
           msCTRCD = *Blanks;
           If Not %Found(WTABL) Or PCSTAT ='I';
              erCTRCD = *On;
              msCTRCD = 'WCR0513';
           Endif;
      /End-Free
     C     erCTRCD       CABEQ     '1'           STRT
      *
     C*  VALIDATE THE CONTROVERT PMT TRANSACTION CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCTRCDCV     XXKEY
     C     TBKY          CHAIN     PPC01
      /Free
           erCTRCDCV = *Off;
           msCTRCDCV = *Blanks;
           If Not %Found(WTABL) or PCSTAT ='I' ;
              erCTRCDCV = *On;
              msCTRCDCV = 'WCR1092';
           Endif;
      /End-Free
     C     erCTRCDCV     CABEQ     '1'           STRT
     C*
    C*   QME/AME Fields Validations
     C*
     C*  VALIDATE THE QME/AME PMT TRANSACTION CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCQTRCD       XXKEY
     C     TBKY          CHAIN     PPC01
      /Free
           erQTRCD = *Off;
           msQTRCD = *Blanks;
           If Not %Found(WTABL) Or PCSTAT ='I';
              erQTRCD = *On;
              msQTRCD = 'WCR0513';
           Endif;
      /End-Free
     C     erQTRCD       CABEQ     '1'           STRT
     C*
      *
     C*  VALIDATE THE CONTROVERT PMT TRANSACTION CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCQTRCDCV     XXKEY
     C     TBKY          CHAIN     PPC01
      /Free
           erQTRCDCV = *Off;
           msQTRCDCV = *Blanks;
           If Not %Found(WTABL) or PCSTAT ='I' ;
              erQTRCDCV = *On;
              msQTRCDCV = 'WCR1092';
           Endif;
      /End-Free
     C     erQTRCDCV     CABEQ     '1'           STRT
     C*
    C*   PT Fields Validations
     C*
     C*  VALIDATE THE PT PMT TRANSACTION CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCPTRCD       XXKEY
     C     TBKY          CHAIN     PPC01
      /Free
           erPTRCD = *Off;
           msPTRCD = *Blanks;
           If Not %Found(WTABL) Or PCSTAT ='I';
              erPTRCD = *On;
              msPTRCD = 'WCR0513';
           Endif;
      /End-Free
     C     erPTRCD       CABEQ     '1'           STRT
     C*
     C*  VALIDATE THE CONTROVERT PMT TRANSACTION CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCPTRCDCV     XXKEY
     C     TBKY          CHAIN     PPC01
      /Free
           erPTRCDCV = *Off;
           msPTRCDCV = *Blanks;
           If Not %Found(WTABL) Or PCSTAT ='I';
              erPTRCDCV = *On;
              msPTRCDCV = 'WCR1092';
           Endif;
      /End-Free
     C     erPTRCDCV     CABEQ     '1'           STRT
     C*
    C*   DME Fields Validations
     C*
     C*  VALIDATE THE DME PMT TRANSACTION CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCDTRCD       XXKEY
     C     TBKY          CHAIN     PPC01
      /Free
           erDTRCD = *Off;
           msDTRCD = *Blanks;
           If Not %Found(WTABL) Or PCSTAT ='I';
              erDTRCD = *On;
              msDTRCD = 'WCR0513';
           Endif;
      /End-Free
     C     erDTRCD       CABEQ     '1'           STRT
     C*
     C*  VALIDATE THE CONTROVERT PMT TRANSACTION CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCDTRCDCV     XXKEY
     C     TBKY          CHAIN     PPC01
      /Free
           erDTRCDCV = *Off;
           msDTRCDCV = *Blanks;
           If Not %Found(WTABL) or PCSTAT ='I' ;
              erDTRCDCV = *On;
              msDTRCDCV = 'WCR1092';
           Endif;
      /End-Free
     C     erDTRCDCV     CABEQ     '1'           STRT
     C*
    C***** End Add *** 07/14/16 **************************************
     C***** Beg Add *** 05/04/17 **************************************
     C                   ENDIF
     C***** End Add *** 05/04/17 **************************************

     C*  VALIDATE THE PMT VAL 1 CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBR2         XXKEY
     C     TBKY          CHAIN     PB201                              24
     C     *IN24         CABEQ     '1'           STRT
     C     B2STAT        CABEQ     'I'           STRT                     24
     C*
     C*  VALIDATE THE CONTROVERT PMT VAL 1 CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCP1CV        XXKEY
     C     TBKY          CHAIN     PB201                              43
     C     *IN43         CABEQ     '1'           STRT
     C     B2STAT        CABEQ     'I'           STRT                     43
     C*** BEG   ADD *** 10/14/10 **************************************
     C*
     C*** BEG   CHG *** 03/19/12 **************************************
     C******     *IN08     IFEQ *OFF
     C     *IN08         IFEQ      *ON
     C*** END   CHG *** 03/19/12 **************************************
     C*
     C*  VALIDATE THE CA PMT VAL 1 CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCAB2        XXKEY
     C     TBKY          CHAIN     PB201                              68
     C     *IN68         CABEQ     '1'           STRT
     C     B2STAT        CABEQ     'I'           STRT                     68
     C     B2CCW         CABNE     'Y'           STRT                 4646
      *
     C*  THE CA PMT VAL1 CODE MUST BE AN 'E' TYPE.
      *
     C                   MOVE      *BLANK        @@BRC0            2
     C                   SELECT
      *
     C     B2RC01        WHENNE    ' '
     C                   MOVE      B2RC01        @@BRC0
     C     B2RC02        WHENNE    ' '
     C                   MOVE      B2RC02        @@BRC0
     C     B2RC03        WHENNE    ' '
     C                   MOVE      B2RC03        @@BRC0
     C     B2RC04        WHENNE    ' '
     C                   MOVE      B2RC04        @@BRC0
     C     B2RC05        WHENNE    ' '
     C                   MOVE      B2RC05        @@BRC0
     C     B2RC06        WHENNE    ' '
     C                   MOVE      B2RC06        @@BRC0
     C     B2RC07        WHENNE    ' '
     C                   MOVE      B2RC07        @@BRC0
     C     B2RC08        WHENNE    ' '
     C                   MOVE      B2RC08        @@BRC0
     C     B2RC09        WHENNE    ' '
     C                   MOVE      B2RC08        @@BRC0
     C     B2RC10        WHENNE    ' '
     C                   MOVE      B2RC10        @@BRC0
      *
     C                   ENDSL
      *
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     @@BRC0        XXKEY
     C     TBKY          CHAIN     PRT01                              46
     C     *IN46         CABEQ     '1'           STRT
     C     B2STAT        CABEQ     'I'           STRT                     46
     C     RTTYPE        CABNE     'E'           STRT                 4646
     C*
     C*  VALIDATE THE CONTROVERT CA PMT VAL 1 CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCABC        XXKEY
     C     TBKY          CHAIN     PB201                              45
     C     *IN45         CABEQ     '1'           STRT
     C     B2STAT        CABEQ     'I'           STRT                     45
     C     B2CCW         CABNE     'Y'           STRT                 4747
      *
     C*  CONTROVERT CA PMT VAL 1 CODE MUST BE 'E' TYPE & COST CONTAINMENT
      *  FLAG MUST = Y"
      *
     C                   MOVE      *BLANK        @@CABC            2
     C                   SELECT
      *
     C     B2RC01        WHENNE    ' '
     C                   MOVE      B2RC01        @@CABC
     C     B2RC02        WHENNE    ' '
     C                   MOVE      B2RC02        @@CABC
     C     B2RC03        WHENNE    ' '
     C                   MOVE      B2RC03        @@CABC
     C     B2RC04        WHENNE    ' '
     C                   MOVE      B2RC04        @@CABC
     C     B2RC05        WHENNE    ' '
     C                   MOVE      B2RC05        @@CABC
     C     B2RC06        WHENNE    ' '
     C                   MOVE      B2RC06        @@CABC
     C     B2RC07        WHENNE    ' '
     C                   MOVE      B2RC07        @@CABC
     C     B2RC08        WHENNE    ' '
     C                   MOVE      B2RC08        @@CABC
     C     B2RC09        WHENNE    ' '
     C                   MOVE      B2RC08        @@CABC
     C     B2RC10        WHENNE    ' '
     C                   MOVE      B2RC10        @@CABC
      *
     C                   ENDSL
      *
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     @@CABC        XXKEY
     C     TBKY          CHAIN     PRT01                              47
     C     *IN47         CABEQ     '1'           STRT
     C     B2STAT        CABEQ     'I'           STRT                     47
     C     RTTYPE        CABNE     'E'           STRT                 4747
     C*
     C                   ENDIF
      *
     C*** END   ADD *** 10/14/10 **************************************
      *
     C***** Beg Add *** 05/04/17 **************************************
    C*** Run below edits only for Client = '136'
     C     dspFld136     IFEQ      *ON
     C***** End Add *** 05/04/17 **************************************

    C***** Beg Add *** 07/14/16 **************************************
    C*   CS Fields Validations
     C*
     C*  VALIDATE THE CS PMT VAL 1 CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCPV1        XXKEY
     C     TBKY          CHAIN     PB201
      /Free
           erCPV1 = *Off;
           msCPV1 = *Blanks;
           If Not %Found(WTABL) or B2STAT ='I' ;
              erCPV1 = *On;
              msCPV1 = 'WCR0513';
           Endif;
      /End-Free
     C*
     C*  VALIDATE THE CONTROVERT PMT VAL 1 CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCPV1CV      XXKEY
     C     TBKY          CHAIN     PB201
      /Free
           erCPV1CV = *Off;
           msCPV1CV = *Blanks;
           If Not %Found(WTABL) or B2STAT ='I' ;
              erCPV1CV = *On;
              msCPV1CV = 'WCR0513';
           Endif;
      /End-Free
     C     erCPV1CV      CABEQ     '1'           STRT
     C*
    C*   QME/AME Fields Validations
     C*
     C*  VALIDATE THE QME/AME PMT VAL 1 CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCQPV1        XXKEY
     C     TBKY          CHAIN     PB201
      /Free
           erQPV1  = *Off;
           msQPV1  = *Blanks;
           If Not %Found(WTABL) or B2STAT ='I' ;
              erQPV1  = *On;
              msQPV1  = 'WCR0513';
           Endif;
      /End-Free
     C     erQPV1        CABEQ     '1'           STRT
     C*
     C*  VALIDATE THE CONTROVERT PMT VAL 1 CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCQPV1CV      XXKEY
     C     TBKY          CHAIN     PB201
      /Free
           erQPV1CV = *Off;
           msQPV1CV = *Blanks;
           If Not %Found(WTABL) or B2STAT ='I' ;
              erQPV1CV = *On;
              msQPV1CV = 'WCR0513';
           Endif;
      /End-Free
     C     erQPV1CV      CABEQ     '1'           STRT
     C*
    C*   PT Fields Validations
     C*
     C*  VALIDATE THE PT PMT VAL 1 CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCPPV1        XXKEY
     C     TBKY          CHAIN     PB201
      /Free
           erPPV1  = *Off;
           msPPV1  = *Blanks;
           If Not %Found(WTABL) or B2STAT ='I' ;
              erPPV1  = *On;
              msPPV1  = 'WCR0513';
           Endif;
      /End-Free
     C     erPPV1        CABEQ     '1'           STRT
     C*
     C*  VALIDATE THE CONTROVERT PMT VAL 1 CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCPPV1CV      XXKEY
     C     TBKY          CHAIN     PB201
      /Free
           erPPV1CV = *Off;
           msPPV1CV = *Blanks;
           If Not %Found(WTABL) or B2STAT ='I' ;
              erPPV1CV = *On;
              msPPV1CV = 'WCR0513';
           Endif;
      /End-Free
     C     erPPV1CV      CABEQ     '1'           STRT
     C*
    C*   DME Fields Validations
     C*
     C*  VALIDATE THE DME PMT VAL 1 CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCDPV1        XXKEY
     C     TBKY          CHAIN     PB201
      /Free
           erDPV1 = *Off;
           msDPV1 = *Blanks;
           If Not %Found(WTABL) or B2STAT ='I' ;
              erDPV1   = *On;
              msDPV1   = 'WCR0513';
           Endif;
      /End-Free
     C     erDPV1        CABEQ     '1'           STRT
     C*
     C*  VALIDATE THE CONTROVERT PMT VAL 1 CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCDPV1CV      XXKEY
     C     TBKY          CHAIN     PB201
      /Free
           erDPV1CV = *Off;
           msDPV1CV = *Blanks;
           If Not %Found(WTABL) or B2STAT ='I' ;
              erDPV1CV = *On;
              msDPV1CV = 'WCR0513';
           Endif;
      /End-Free
     C     erDPV1CV      CABEQ     '1'           STRT
     C*
    C***** End Add *** 07/14/16 **************************************
     C***** Beg Add *** 05/04/17 **************************************
     C                   ENDIF
     C***** End Add *** 05/04/17 **************************************
     C*
     C*  VALIDATE THE PHARMACY PMT VAL 1 CODE
     C*
     C     *IN39         IFEQ      '1'
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCXBR2        XXKEY
     C     TBKY          CHAIN     PB201                              41
     C     *IN41         CABEQ     '1'           STRT
     C     B2STAT        CABEQ     'I'           STRT                     41
     C                   END
     C*
     C*  VALIDATE THE PMT VAL 2 - LOST TIME CODE.
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBR7L        XXKEY
     C     TBKY          CHAIN     PB701                              27
     C     *IN27         CABEQ     '1'           STRT
     C     B7STAT        CABEQ     'I'           STRT                     27
     C     B7LTLF        CABNE     'Y'           STRT                 2828
     C*
     C*  VALIDATE THE PMT VAL 2 - NO LOST TIME CODE.
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBR7N        XXKEY
     C     TBKY          CHAIN     PB701                              29
     C     *IN29         CABEQ     '1'           STRT
     C     B7STAT        CABEQ     'I'           STRT                     29
     C     B7LTLF        CABNE     'N'           STRT                 3030
     C*
     C*  VALIDATE THE PMT STATUS CODE - MUST BE A 'P' TYPE.
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCSTS         XXKEY
     C     TBKY          CHAIN     PPS01                              25
     C     *IN25         CABEQ     '1'           STRT
     C     PSSTAT        CABEQ     'I'           STRT                     25
     C     PSTYPE        CABNE     'P'           STRT                 2626
     C*
     C*  VALIDATE THE PROVIDER, IF PAYEE TYPE IS 'B'.
     C*
     C     SCPTYP        IFEQ      'B'
     C                   MOVE      SCFEIN        XXFEIN
     C                   MOVE      SCMOD         XXMOD
     C     PDKY          CHAIN     PPD01                              31
     C     *IN31         CABEQ     '1'           STRT
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     CIKY          CHAIN     PCI01                              40
     C                   Z-ADD     XXCO#         CICO#
     C                   Z-ADD     XXFND         CIFND
     C                   MOVE      SCPTYP        CIPTYP
     C                   MOVE      SCSYS         CISYS
     C                   MOVE      SCTRCD        CITRCD
     C                   MOVE      SCTRCV        CITRCV
     C                   MOVE      SCBR2         CIBR2
     C                   MOVE      SCP1CV        CIP1CV
     C                   MOVE      SCXBR2        CIXBR2
     C                   MOVE      SCSTS         CISTS
     C                   MOVE      SCBR7L        CIBR7L
     C                   MOVE      SCBR7N        CIBR7N
     C*** BEG   ADD *** 10/14/10 **************************************
     C                   MOVE      SCCATR        CICATR
     C                   MOVE      SCCATC        CICATC
     C                   MOVE      SCCAB2        CICAB2
     C                   MOVE      SCCABC        CICABC
     C*** END   ADD *** 10/14/10 **************************************
    C***** Beg Add *** 07/14/16 **************************************
     C                   MOVE      SCCTRCD       CICTRCD
     C                   MOVE      SCCTRCDCV     CICTRCDCV
     C                   MOVE      SCCPV1        CICPV1
     C                   MOVE      SCCPV1CV      CICPV1CV
     C                   MOVE      SCQTRCD       CIQTRCD
     C                   MOVE      SCQTRCDCV     CIQTRCDCV
     C                   MOVE      SCQPV1        CIQPV1
     C                   MOVE      SCQPV1CV      CIQPV1CV
     C                   MOVE      SCPTRCD       CIPTRCD
     C                   MOVE      SCPTRCDCV     CIPTRCDCV
     C                   MOVE      SCPPV1        CIPPV1
     C                   MOVE      SCPPV1CV      CIPPV1CV
     C                   MOVE      SCDTRCD       CIDTRCD
     C                   MOVE      SCDTRCDCV     CIDTRCDCV
     C                   MOVE      SCDPV1        CIDPV1
     C                   MOVE      SCDPV1CV      CIDPV1CV
    C***** End Add *** 07/14/16 **************************************
     C                   Z-ADD     SCFEIN        CIFEIN
     C                   Z-ADD     SCMOD         CIMOD
     C                   MOVE      'A'           CISTAT
     C                   MOVE      Q1USER        CIUSER
     C                   MOVE      Q1PGM         CIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CIDT
     C                   Z-ADD     DSTIME        CITIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PCI01
     C                   ELSE
     C                   MOVE      Q1USER        CIIUSR
     C                   Z-ADD     CIDT          CIIDT
     C                   WRITE     PCI01
     C                   END
     C*
     C     INQ2          TAG
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCPTYP
     C                   MOVE      *BLANKS       SCSYS
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *====================================================================*
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE DSC
PROVIDER
BILL REVIEW
NETWORK
PROVIDER BILL REVIEW
HOSPITAL BILL REVIEW
PRE-CERT
NETWORK SERVICES ONLY
MEDICAL CASE MGT
