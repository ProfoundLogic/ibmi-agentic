     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F9510R1                                           *
     H*    DESCRIPTION: PROMPT FOR PHYSICIAN, HOSPITAL OR BOTH FOR    *
     H*                 TAPE INITIALIZATION                           *
     H*                                                               *
     H*    CALLED FROM:                                               *
     H*      MENU                                                     *
     H*    CALLS TO:                                                  *
     H*      F9510C1 - INITIALIZE TAPE SUBMITTER                      *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  01/08/03 C:062 Project: FL0027
     H*                 Dental Data Submission
     H*
     H*  01/05/00 C:117 Project: FL0007
     H*                 Hospital Data Submission (Compile Only)
     H*
     H*  04/06/99 C:05  Project: FL019
     H*                 NOTE: Pgm names chgd from F951x to F961x
     H*                 Changeover to "Provider Bills" version.
     H*
     H*  03/27/98 C:31  Project: MC145
     H*                 Yr 2000, RISC Developemnt
     H*
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*  SCR01:                                                       *
     H*     25 = INVALID INPUT                                        *
     H*  SCR03:                                                       *
     H*     90 = LOAD TAPE/HOSPITAL                                   *
     H*     91 = LOAD TAPE/PHYSICIAN                                  *
     H*     95 = LOAD TAPE/DENTAL                                     *
     H*     92 = TAPE INITIALIZED/HOSPITAL                            *
     H*     93 = TAPE INITIALIZED/PHYSICIAN                           *
     H*     96 = TAPE INITIALIZED/DENTAL                              *
     H*     94 = DONE                                                 *
     H*****************************************************************
     F*** BEGIN CHG 04/06/99 ******************************************
     FF9609FFL  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*** END CHG   04/06/99 ******************************************
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMSML     IF   E           K DISK

     D* CURRENT DATE
     D                 DS
     D  XXDT                   1      8  0
     D  XXMM                   1      2  0
     D  XXDD                   3      4  0
     D  XXYY                   5      8  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER           10
     C*
     C* Key List for the SUBCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C*
     C*  Entry Parameters
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
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C                   TIME                    DSDTTM
     C*
     C*  GET COMPANY NAME
     C*
     C     STRT01        TAG
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
     C*
     C*  GET SUBCRIBER NAME
     C*
     C                   MOVE      UPSUB#        SCSUB#            5 0
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SCSBNM           20
     C                   ELSE
     C                   MOVEL     SMNAME        SCSBNM
     C                   END
     C*
     C* Get the Requested Record, Load Information
     C*
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C* Load the Record Format Information for the First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C* Display Screen Formats
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
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C******************************************************************
     C* Process All Command Keys
     C     *IN11         IFEQ      '1'
     C************************************
     C* F3 = EXIT                        *
     C************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C************************************
     C* CMD 12 - CANCEL INITIALIZATION   *
     C************************************
     C     *INKL         IFEQ      '1'
     C*** BEG CHG *** 03/17/00 ****************************************
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   ENDIF
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   ENDIF
     C*
     C*** END CHG *** 03/17/00 ****************************************
     C                   GOTO      STRT
     C                   END
     C************************************
     C* CMD 13 - CHG USER PARMS          *
     C************************************
     C     *INKM         IFEQ      '1'
     C                   CALL      'F0006R'
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT01
     C                   END                                                    END *INKM
     C******
     C                   END                                                    END CMDKEYS
     C*********************************************************************
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*** BEG ADD *** 03/17/00 ****************************************
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C*** END ADD *** 03/17/00 ****************************************
     C****************************
     C* HOSPITAL/PHYSICIAN/BOTH  *
     C****************************
     C     SCR01T        TAG
     C     SCPROC        IFNE      'P'
     C     SCPROC        ANDNE     'H'
     C*** BEGIN DEL *** 01/08/03 **************************************
     C********** SCPROC    ANDNE'B'
     C*** END   DEL *** 01/08/03 **************************************
     C*** BEGIN ADD *** 01/08/03 **************************************
     C     SCPROC        ANDNE     'D'
     C     SCPROC        ANDNE     'A'
     C*** END   ADD *** 01/08/03 **************************************
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*** BEGIN CHG *** 01/08/03 **************************************
     C********** SCPROC    IFNE 'P'
     C**********           MOVE '1'       *IN90
     C**********           ELSE
     C**********           MOVE '1'       *IN91
     C**********           END
     C                   SETOFF                                       909195
     C                   SELECT
     C     SCPROC        WHENEQ    'P'
     C                   SETON                                        91
     C     SCPROC        WHENEQ    'H'
     C                   SETON                                        90
     C     SCPROC        WHENEQ    'D'
     C                   SETON                                        95
     C     SCPROC        WHENEQ    'A'
     C                   SETON                                        90
     C                   MOVEL     'H'           XXPROC            1
     C                   ENDSL
     C*** END   CHG *** 01/08/03 **************************************
     C*
     C*****  WE HAVE PASSED ALL EDITS, GOTO NEXT SCREEN
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C*** BEG ADD *** 03/17/00 ****************************************
     C****************************
     C* CHOOSE TAPE OR PC FILE   *
     C****************************
     C     SCR02T        TAG
     C     SCSEL1        IFNE      *BLANKS
     C     SCSEL1        ANDNE     '1'
     C     SCSEL2        ANDNE     *BLANKS
     C     SCSEL2        ANDNE     '1'
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     SCSEL1        IFEQ      '1'
     C     SCSEL2        ANDEQ     '1'
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     SCSEL1        IFEQ      *BLANKS
     C     SCSEL2        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 01/08/03 **************************************
     C                   SETOFF                                       8889
     C*** END   ADD *** 01/08/03 **************************************
     C     SCSEL1        IFEQ      '1'
     C     SCSEL2        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN88
     C                   ENDIF
     C*
     C     SCSEL1        IFEQ      *BLANKS
     C     SCSEL2        ANDEQ     '1'
     C                   MOVE      *ON           *IN89
     C                   ENDIF
     C*
     C*****  WE HAVE PASSED ALL EDITS, GOTO NEXT SCREEN
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*** END ADD *** 03/17/00 ****************************************
     C****************************
     C* LOAD TAPE & INITIALIZE   *
     C****************************
     C*** BEG CHG *** 03/17/00 ****************************************
     C*********  SCR02T    TAG
     C     SCR03T        TAG
     C*** BEG CHG *** 03/17/00 ****************************************
     C*
     C*** BEGIN CHG *** 01/08/03 **************************************
     C********** SCPROC    IFEQ 'B'
     C**********           MOVE '1'       *IN92
     C**********           MOVE '1'       *IN91
     C**********           MOVEL'P'       SCPROC
     C                   MOVEA     '000000'      *IN(90)
     C     SCPROC        IFEQ      'A'
     C                   SELECT
     C     XXPROC        WHENEQ    'H'
     C                   MOVEL     'P'           XXPROC
     C                   MOVEL     '1'           *IN92
     C                   MOVEL     '1'           *IN91
     C     XXPROC        WHENEQ    'P'
     C                   MOVEL     'D'           XXPROC
     C                   MOVEL     '1'           *IN93
     C                   MOVEL     '1'           *IN95
     C     XXPROC        WHENEQ    'D'
     C                   MOVEL     'A'           XXPROC
     C                   MOVEL     '1'           *IN96
     C                   MOVE      '1'           *IN94
     C                   MOVEL     *BLANKS       SCPROC
     C                   ENDSL
     C*** END   CHG *** 01/08/03 **************************************
     C                   ELSE
     C     SCPROC        IFEQ      'P'
     C                   MOVE      '1'           *IN93
     C                   MOVE      '1'           *IN94
     C                   MOVE      *BLANK        SCPROC
     C                   ELSE
     C     SCPROC        IFEQ      'H'
     C                   MOVE      '1'           *IN92
     C                   MOVE      '1'           *IN94
     C                   MOVE      *BLANK        SCPROC
     C                   ELSE
     C*** BEGIN CHG *** 01/08/03 **************************************
     C     SCPROC        IFEQ      'D'
     C                   MOVE      '1'           *IN96
     C                   MOVE      '1'           *IN94
     C                   MOVE      *BLANK        SCPROC
     C                   ELSE
     C*** END   CHG *** 01/08/03 **************************************
     C     SCPROC        IFEQ      *BLANK
     C                   GOTO      TERM
     C                   END
     C*** BEGIN CHG *** 01/08/03 **************************************
     C                   ENDIF
     C*** END   CHG *** 01/08/03 **************************************
     C                   END
     C                   END
     C                   END
     C*
     C* Call the CLP to Submit the Program for INITIALIZING TAPES
     C*
     C*** BEG ADD *** 03/17/00 ****************************************
     C     *IN88         IFEQ      *ON
     C                   MOVEL     'T'           QQMDIA            1
     C                   ELSE
     C     *IN89         IFEQ      *ON
     C                   MOVEL     'D'           QQMDIA            1
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 03/17/00 ****************************************
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   CALL      'F9609CFL'                                   CHG 04/06/99
     C                   PARM                    QQCO#
     C                   PARM                    QQMDIA
     C                   GOTO      STRT
     C*
     C* End the Program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
