     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
      * *****************************************************************
     H/TITLE F0200R - EOMB RE-PRINT SCREEN PROGRAM
     H*****************************************************************
     H**   PROGRAM: F0200R                                           **
     H**   DESCRIPTION: EOMB REPRINT                                 **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** REVISIONS:                                                  **
     H*                                                               *
     H*  06/09/20 C:212 Project : SY420                               *
     H*                 Mask SSN                                      *
     H*                                                               *
     H*  03/10/00 C:62 Support: MC220                                 *
     H*                USE FAEOLG INSTEAD OF FAEOL3.                  *
     H*                                                               *
     H*  04/08/99 C:56 Support: MC095 #17568                          *
     H*               1. Add CO#/SUB#/FND TO CALL OF F0575R           *
     H*                                                               *
     H*  07/22/92 KRR    PROJ: 99AAC                                  *
     H*                  - ADDED SUB# TO ALL PROCESSING               *
     H*                                                               *
     H*  12/07/92 C:20  PROJ: 99AAK                                   *
     H*                 1. CHANGED TO NOT ALLOW DELETE OF EOMB IF     *
     H*                    EOFLG4 IS EQUAL TO 'Y'                     *
     H*                                                               *
     H*  07/14/93 C:20  PROJ: FS121                                   *
     H*                 1. CHANGED TOTAL PAID AMOUNT TO REFLECT ANY   *
     H*                    NETWORK DISCOUNT TAKEN                     *
     H*                                                               *
     H*  02/27/95 C:20  REQ/PROJ: 152/MC119                           *
     H*                 1. SCR02 - ADDED F14 TO CALL DARTS.  WE WILL  *
     H*                    STILL HAVE SAME DARTS PROCESSING FOR WCMS  *
     H*                    VS. WCMC KEY AND THE KEY WILL STILL BE THE *
     H*                    CASE KEY.                                  *
     H*****************************************************************
     FF0200F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     F*** BEGIN CHG *** 03/10/00 **************************************
     F**********FAEOL3  IF  E           K        DISK
     FFAEOLG    IF   E           K DISK
     F*** END   CHG *** 03/10/00 **************************************
     FFMSML     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     F*** BEGIN ADD *** 03/10/00 **************************************
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     F*** END   ADD *** 03/10/00 **************************************

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D*
     D DS              S             40    DIM(2) CTDATA PERRCD(1)              SCRN DESC 1/2
     D DE              S             10    DIM(2) CTDATA PERRCD(1)              SCRN DESC 3
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D  DSYR                  11     12  0
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
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*** BEGIN CHG *** 03/10/00 **************************************
     C**********           KFLD           XXFND
     C                   KFLD                    SCFND
     C*** END   CHG *** 03/10/00 **************************************
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C*** BEGIN CHG *** 03/10/00 **************************************
     C**********           KFLD           XXSUB#
     C**********           KFLD           XXFND
     C**********           KFLD           EOFYR
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C*** END   CHG *** 03/10/00 **************************************
     C                   KFLD                    SCCASE
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C*  KEY LIST FOR THE PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C*
     C*  KEY LIST FOR THE EOB WORK FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*** BEGIN ADD *** 03/10/00 **************************************
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C*** END   ADD *** 03/10/00 **************************************
     C                   KFLD                    SCEOB#
     C*
     C*   KEY LIST FOR THE SUBSCRIBER
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C*
     C* FS BASE CONTROL FILE
     C*
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
     C*
     C* KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
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
     C*
     C*** INITIALIZE WORK FIELDS ***
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPSUB#        XXSUB#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    UPFYR         XUFYR
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    FNWCF#        XXWCF#
     C     *LIKE         DEFINE    CMDT          XXACDT
     C*
     C                   Z-ADD     *ZEROS        X                 5 0
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPSUB#        XXSUB#
     C                   MOVE      UPFND         XXFND
     C*
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPRDT            8 0
     C*
     C                   Z-ADD     UPFYR         XUFYR
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'FB'          XSKEY
     C     FBKY          CHAIN     SFB01                              60
     C     *IN60         IFEQ      '0'
     C     FBFL01        IFEQ      'Y'
     C     F1KEY         CHAIN     PF101                              50
     C     *IN50         IFEQ      '0'
     C     F1OUTA        IFEQ      'N'
     C                   Z-ADD     4             X
     C     1             SUBST     FBAL01:X      XDKEY             1    65
     C     XDKEY         IFEQ      'Y'
     C                   MOVE      'Y'           XWCKEY            1
     C                   ELSE
     C                   MOVE      'N'           XWCKEY
     C                   END                                                    END XDKEY
     C                   ELSE                                                   ELSE F1OUTA EQ 'N'
     C                   MOVE      'N'           XWCKEY
     C                   END                                                    END F1OUTA EQ 'N'
     C                   END                                                    END *IN50 EQ '0'
     C                   ELSE                                                   ELSE FBFL01 EQ 'Y'
     C                   MOVE      'N'           XWCKEY
     C                   END                                                    END FBFL01 EQ 'Y'
     C                   END                                                    END *IN60 FBKY
     C*
     C*  GET THE COMPANY, SUBSCRIBER AND FUND RECORDS
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVE      SMNAME        SCSBNM
     C*** BEGIN ADD *** 03/10/00 **************************************
     C                   MOVE      UPFND         SCFND
     C*** END   ADD *** 03/10/00 **************************************
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      UPFND         SCFND
     C*** BEGIN CHG *** 03/10/00 **************************************
     C**********           MOVE FNNAME    SCNAME
     C                   MOVEL     FNNAME        SCGNAM
     C*** END   CHG *** 03/10/00 **************************************
     C                   Z-ADD     FNWCF#        XXWCF#
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*****************************************************************
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
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C**********************************
     C*   F3 = END OF JOB
     C**********************************
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
     C*** BEGIN ADD *** 03/10/00 **************************************
     C**********************************
     C*   F10 = SEARCH
     C**********************************
     C     *INKJ         IFEQ      '1'
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCFND'
     C*
     C                   CALL      'F0060R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        31
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PFN02                              99
     C*
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL(P)  FNNAME        SCGNAM
     C                   SETON                                        32
     C*
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C*
     C     HFLD          WHENEQ    'SCFYR'
     C     HFLD          OREQ      'SCCASE'
     C*
     C                   CALL      'F0269R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      SCSUB#        ##SUB#            5 0
     C                   PARM      SCFND         ##FND             3 0
     C                   PARM      *ZEROS        ##FYR             3 0
     C                   PARM      *ZEROS        ##CASE            7 0
     C                   PARM      *BLANKS       ##SS#             9
     C                   PARM      *ZEROS        ##ACDT            8 0
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     ##CASE        IFEQ      *ZEROS
     C*
     C                   SETON                                        32
     C*
     C                   ELSE
     C*
     C                   Z-ADD     ##FYR         SCFYR
     C                   Z-ADD     ##CASE        SCCASE
     C*
     C     CMKEY         CHAIN     PCM01                              99
     C*
     C     CMLSN         CAT(P)    ',':0         SCCNAM
     C                   CAT       CMFSN:1       SCCNAM
     C*
     C                   SETON                                        33
     C*
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C*
     C     HFLD          WHENEQ    'SCEOB#'
     C*
     C                   CALL      'F0349R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      UPCO#         ##CO#             3 0
     C                   PARM      SCSUB#        ##SUB#            5 0
     C                   PARM      SCFND         ##FND             3 0
     C                   PARM      SCFYR         ##FYR             3 0
     C                   PARM      SCCASE        ##CASE            7 0
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      *ZEROS        ##EOB#            9 0
     C*
     C     ##EOB#        IFEQ      *ZEROS
     C*
     C                   SETON                                        33
     C*
     C                   ELSE
     C*
     C                   Z-ADD     ##FND         SCFND
     C                   Z-ADD     ##FYR         SCFYR
     C                   Z-ADD     ##CASE        SCCASE
     C                   Z-ADD     ##EOB#        SCEOB#
     C*
     C     CMKEY         CHAIN     PCM01                              99
     C*
     C     CMLSN         CAT(P)    ',':0         SCCNAM
     C                   CAT       CMFSN:1       SCCNAM
     C*
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C*
     C                   OTHER
     C*
     C                   GOTO      STRT
     C*
     C                   ENDSL
     C*
     C                   ENDIF
     C*** END   ADD *** 03/10/00 **************************************
     C**********************************
     C*   F12 = PRIOR SCREEN
     C**********************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C                   END
     C***************************************
     C*  F14 = DARTS                        *
     C***************************************
     C     *INKN         IFEQ      '1'
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      *BLANKS       QWKEY
     C     XWCKEY        IFEQ      'Y'
     C                   MOVE      UPCO#         QWCO#
     C                   MOVE      XXWCF#        QWFND
     C                   Z-ADD     SCFYR         QWFYR
     C                   MOVE      SCCASE        QWCASE
     C*
     C*** CALL DARTS ENTRY PROGRAM
     C                   CALL      'SC010R'                             99
     C                   PARM                    QWKEY
     C                   PARM                    Q1PGM
     C                   ELSE                                                   END XWCKEY
     C*
     C                   MOVE      UPCO#         QCCO#
     C                   MOVE      UPSUB#        QCSUB#
     C                   MOVE      UPFND         QCFND
     C                   Z-ADD     SCFYR         QCFYR
     C                   MOVE      SCCASE        QCCASE
     C*
     C*** CALL DARTS ENTRY PROGRAM
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM                    Q1PGM
     C                   END                                                    END XWCKEY
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END                                                    END *INKM EQ '1'
     C*** BEGIN DEL *** 03/10/00 **************************************
     C**********************************
     C*   F6 = EOMB SEARCH - SCREEN 1
     C**********************************
     C********** *INKF     IFEQ '1'
     C********** Q2RECF    IFEQ 'SCR01'
     C**********           CALL 'F0650R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCEOB#
     C**********           PARM           QQRTCD  1
     C*
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C**********           WRITESCR99
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** SCEOB#    OREQ *ZERO
     C**********           GOTO STRT
     C**********           END
     C*
     C********** QQRTCD    IFEQ '*'
     C**********           GOTO SCR01T
     C**********           END
     C**********           END
     C*
     C**********           END
     C**********************************
     C*   F7 = CASE # SEARCH - SCREEN 1
     C**********************************
     C********** *INKG     IFEQ '1'
     C********** Q2RECF    IFEQ 'SCR01'
     C**********           CALL 'F0651R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCEOB#
     C**********           PARM           QQRTCD
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C**********           WRITESCR99
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** SCEOB#    OREQ *ZERO
     C**********           GOTO STRT
     C**********           END
     C*
     C********** QQRTCD    IFEQ '*'
     C**********           GOTO SCR01T
     C**********           END
     C**********           END
     C*
     C**********           END
     C*
     C**********
     C*** END   DEL *** 03/10/00 **************************************
     C**********************************
     C*   F13 = CHG USER PARMS
     C**********************************
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'F0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   MOVE      *ZERO         UPFND
     C*** BEGIN ADD *** 03/10/00 **************************************
     C                   ELSE
     C*
     C                   Z-ADD     UPFND         SCFND
     C*
     C     FNKEY         CHAIN     PFN01                              99
     C*
     C  N99              MOVEL(P)  FNNAME        SCGNAM
     C   99              MOVEL(P)  *BLANKS       SCGNAM
     C*** END   ADD *** 03/10/00 **************************************
     C                   END
     C*** BEGIN ADD *** 03/10/00 **************************************
     C                   Z-ADD     *ZEROS        SCFYR
     C                   Z-ADD     *ZEROS        SCCASE
     C                   Z-ADD     *ZEROS        SCEOB#
     C                   MOVEL     *BLANKS       SCCNAM
     C*** END   ADD *** 03/10/00 **************************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*****************************************************
     C*  CMD 16 - SEARCH DISPLAY PREVIOUSLY PAID CHARGES  *
     C*****************************************************
     C     *INKQ         IFEQ      '1'
     C*
     C                   MOVEL     Q2RECF        XXSCRN            5
     C     Q2RECF        IFEQ      'SCR02'
     C                   Z-ADD     SCFYR         QQFYR
     C                   Z-ADD     SCCASE        QQCASE
     C                   Z-ADD     SCACDT        QQACDT
     C                   GOTO      STRT12
     C                   END
     C*
     C     EOKEY         CHAIN     PEO01                              30
     C     *IN30         CABEQ     '1'           STRT
     C*
     C                   Z-ADD     EOFYR         QQFYR
     C                   Z-ADD     EOCASE        QQCASE
     C*
     C     STRT12        TAG
     C     *IN60         CABEQ     '1'           STRT
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   MOVE(P)   XXCO#         Q1CO#
     C                   MOVE(P)   SCSUB#        Q1SUB#
     C                   MOVE(P)   SCFND         Q1FND
     C*** END ADD *** 04/08/99 ****************************************
     C                   CALL      'F0575R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
     C                   PARM                    XXSS#
     C                   PARM                    QQACDT            8 0
     C                   PARM                    QQRTCD            1
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   PARM                    Q1CO#             3 0
     C                   PARM                    Q1SUB#            5 0
     C                   PARM                    Q1FND             3 0
     C*** END ADD *** 04/08/99 ****************************************
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     XXSCRN        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     XXSCRN        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                      END CMDKEYS
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*****************************************************************
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*** BEGIN DEL *** 03/10/00 **************************************
     C*** EOB# CANT BE *BLANKS
     C********** SCEOB#    CABEQ*ZERO     STRT           20
     C*** END   DEL *** 03/10/00 **************************************
     C*** BEGIN ADD *** 03/10/00 **************************************
     C*
     C* GROUP NUMBER MUST BE VALID
     C*
     C     FNKEY         CHAIN     PFN01                              99
     C*
     C     *IN99         IFEQ      '1'
     C*
     C                   MOVE      '1'           *IN21                           INVLD SUB#
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C* CASE NUMBER MUST BE VALID
     C*
     C     CMKEY         CHAIN     PCM01                              99
     C*
     C     *IN99         IFEQ      '1'
     C     SCCASE        OREQ      *ZEROS
     C*
     C                   MOVE      '1'           *IN22                           INVLD SUB#
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*** END   ADD *** 03/10/00 **************************************
     C*
     C* EOMB MUST BE VALID
     C*
     C     EOKEY         CHAIN     PEO01                              40
     C*
     C     *IN40         IFEQ      '1'
     C*** BEGIN CHG *** 03/10/00 **************************************
     C**********           MOVE '1'       *IN21
     C                   MOVE      '1'           *IN23
     C*** END   CHG *** 03/10/00 **************************************
     C                   GOTO      STRT
     C                   END
     C*
     C* IF EOFLG4 IS EQUAL TO 'Y' DO NOT ALLOW DELETE AND SEND ERROR
     C* MESSAGE
     C*
     C     EOFLG4        IFEQ      'Y'
     C*** BEGIN CHG *** 03/10/00 **************************************
     C**********           MOVE '1'       *IN32
     C                   MOVE      '1'           *IN24
     C*** END   CHG *** 03/10/00 **************************************
     C                   GOTO      STRT
     C                   END
     C*
     C*** SEE IF EOB HAS BEEN PROCESSED
     C*
     C     EOCHK#        IFEQ      *ZERO
     C                   MOVE      '0'           *IN22                          CANNOT REPRINT
     C                   MOVEL     DS(1)         SCPRD1
     C                   MOVEL     DS(2)         SCPRD2
     C                   MOVEL     DE(1)         SCPRD3                         RE-SELECT
     C                   ELSE
     C                   MOVE      '1'           *IN22                          CAN REPRINT
     C                   MOVEL     *BLANKS       SCPRD1
     C                   MOVEL     *BLANKS       SCPRD2
     C                   MOVEL     DE(2)         SCPRD3                         SUBMIT
     C                   END
     C*
     C*** MOVE EOMB INFORMATION TO 2ND SCREEN FIELDS
     C*
     C                   Z-ADD     EOFEIN        SCFEIN
     C                   MOVE      EOFYR         SCFYR
     C                   Z-ADD     EOCASE        SCCASE
     C*
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C                   Z-ADD     EOTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     EOAMT         SUB       EONDIS        SCTOTP                         ADD 07/14/93
     C                   Z-ADD     EOAMTC        SCTOTC
     C*
     C*** GET SUBSCRIBER INFORMATION
     C*
     C                   MOVE      EOSUB#        XXSUB#
     C                   MOVE      EOSUB#        SCSUB#
     C     SMKEY         CHAIN     PSM01                              60
     C                   MOVE      SMNAME        SCSBNM
     C*
     C*** GET FUND INFORMATION
     C*
     C                   MOVE      EOFND         XXFND
     C                   MOVE      EOFND         SCFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      FNNAME        SCNAME
     C     PDKEY         CHAIN     PPD01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     PDNAME        QQLAST           56
     C                   MOVEL     PDFSN         QQFRST           15
     C                   MOVEL     '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCPNAM
     C                   END
     C*
     C*** GET CLAIM INFORMATION
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   Z-ADD     CMACDT        XXACDT
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      CMSS#         SCSS#
      /Free
       scSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   END
     C*
     C*** GET CLAIMAINT NAME
     C*
     C     CDKEY         CHAIN     PCD01                              70
     C     *IN70         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVEL     '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCCNAM
     C                   END
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*****************************************************************
     C* PROCESS SCREEN 2 - SUBMIT EOMB NUMBER FOR RE-PRINT
     C*
     C     SCR02T        TAG
     C*
     C*   CALL THE CLP PROGRAM TO SUBMIT THE RE-PRINT
     C*
     C     *IN22         IFEQ      '1'                                          SUBMIT
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      XXSUB#        QQSUB#            5
     C                   MOVE      SCEOB#        QQEOB#            9
     C*** BEGIN ADD *** 03/10/00 **************************************
     C                   MOVE      SCFND         Q2FND             3
     C                   MOVE      SCFYR         Q2FYR             3
     C                   MOVE      SCCASE        Q2CASE            7
     C*** END   ADD *** 03/10/00 **************************************
     C                   CALL      'F0200C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQEOB#
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C*** BEGIN ADD *** 03/10/00 **************************************
     C                   PARM                    Q2FND             3
     C                   PARM                    Q2FYR             3
     C                   PARM                    Q2CASE            7
     C*** END   ADD *** 03/10/00 **************************************
     C                   END                                                    ENDIF
     C*
     C                   Z-ADD     *ZERO         SCEOB#
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*****************************************************************
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*****************************************************************
      *====================================================================*
** DS - SCREEN DESCRIPTIONS 1 & 2
This EOMB has not been processed yet.
It cannot be Re-Printed at this time.
** DE - SCREEN DESCRIPTION 3
Re-Select
Submit
