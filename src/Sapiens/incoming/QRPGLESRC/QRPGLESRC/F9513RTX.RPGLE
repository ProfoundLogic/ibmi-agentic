     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F9513RTX                                          *
     H*    DESCRIPTION: PROMPT FOR CPYF TO TAPE (MULTI COMPANY ONLY)  *
     H*                                                               *
     H*    CALLED FROM:                                               *
     H*      MENU                                                     *
     H*    CALLS TO:                                                  *
     H*      F9513CTX - COPY MAGNETIC TAPE WORKFILE TO TAPE           *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*  01/06/99 C:79 PROJ:TX009                                     *
     H*                Modify to work with YR 2000 dates              *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*  SCR01:                                                       *
     H*     25 = INVALID INPUT                                        *
     H*  SCR02:                                                       *
     H*     90 = LOAD TAPE/HOSPITAL                                   *
     H*     91 = LOAD TAPE/PHYSICIAN                                  *
     H*     92 = TAPE PROCESSED/HOSPITAL                              *
     H*     93 = TAPE PROCESSED/PHYSICIAN                             *
     H*     94 = DONE                                                 *
     H*****************************************************************
     FF9513FTX  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
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
     C                   KFLD                    Q1USER
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
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
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
     C* CMD 12 - CANCEL PROCESSING       *
     C************************************
     C     *INKL         IFEQ      '1'
     C                   MOVEL     'SCR01'       Q2RECF
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
     C****************************
     C* HOSPITAL/PHYSICIAN/BOTH  *
     C****************************
     C     SCR01T        TAG
     C*
     C     SCPROC        IFNE      'P'
     C     SCPROC        ANDNE     'H'
     C     SCPROC        ANDNE     'B'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C     SCPROC        IFNE      'H'
     C                   MOVE      '1'           *IN91
     C                   ELSE
     C                   MOVE      '1'           *IN90
     C                   END
     C*
     C*****  WE HAVE PASSED ALL EDITS, GOTO NEXT SCREEN
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C****************************
     C* WRITE TO TAPE            *
     C****************************
     C     SCR02T        TAG
     C*
     C     SCPROC        IFEQ      'B'
     C                   MOVE      '1'           *IN93
     C                   MOVE      '1'           *IN90
     C                   MOVE      'H'           SCPROC
     C                   MOVE      'P'           QQPROC
     C                   ELSE
     C     SCPROC        IFEQ      'H'
     C                   MOVE      '1'           *IN92
     C                   MOVE      '1'           *IN94
     C                   MOVE      SCPROC        QQPROC            1
     C                   MOVE      *BLANK        SCPROC
     C                   ELSE
     C     SCPROC        IFEQ      'P'
     C                   MOVE      '1'           *IN93
     C                   MOVE      '1'           *IN94
     C                   MOVE      SCPROC        QQPROC            1
     C                   MOVE      *BLANK        SCPROC
     C                   ELSE
     C     SCPROC        IFEQ      *BLANK
     C                   GOTO      TERM
     C                   END
     C                   END
     C                   END
     C                   END
     C** Once the workfiles have been successfully created,
     C** Call the CLP to Submit the Program to write workfile to the Tape
     C                   MOVE      UPCO#         QQCO#             3
     C                   CALL      'F9513CTX'
     C                   PARM                    QQCO#
     C                   PARM                    QQPROC
     C                   GOTO      STRT
     C*
     C* End the Program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
