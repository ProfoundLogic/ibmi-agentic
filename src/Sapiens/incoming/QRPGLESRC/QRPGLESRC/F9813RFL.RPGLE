     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H*    PROGRAM: F9813RFL                                          *
     H*    DESCRIPTION:  FL EDI SUBMISSION PART II - PROMPT REV E     *
     H*                                                               *
     H*    CALLED FROM:                                               *
     H*      MENU                                                     *
     H*    CALLS TO:                                                  *
     H*      F9813CFL -  FL EDI SUBMISSION PART II - COPY             *
     H*      F9818CFL -  FL PROGRAM TO DETECT DUPLICATE CONTROL#'S    *
     H*      F9819CFL -  FL PROGRAM TO DETECT DUPLICATE CONTROL#'S    *
     H*      F9820CFL -  FL PROGRAM TO DETECT DUPLICATE CONTROL#'S    *
     H*      F9821CFL -  FL PROGRAM TO DETECT DUPLICATE CONTROL#'S    *
     H*****************************************************************
     H* CREATED ON 03/11/2010 BY C:227 FOR PROJECT FL083
     H* CREATED USING F9713RFL AS BASIS
     H*****************************************************************
     H* REVISIONS:
     H* 04/26/07 C: 079 PROJ: FL067
     H*                 ADD CALLS TO SECTION AND TEMPORARILY COMMENT
     H*                 OUT THE CALL TO DUPPGM
     H*
     H*****************************************************************
     FF9813FFL  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D OPTNM           C                   CONST(' FL EDI Submiss-
     D                                     ion Part II        ')
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
     C                   PARM                    ##CALL            1
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
     C* Initialize fields
     C*
     C                   MOVEL     *BLANK        ##ERR             1
     C                   MOVEL     *BLANK        ##PROC            1
     C*
     C* Get the Requested Record, Load Information
     C*
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      OPTNM         SCPRDS
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
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR01'
     C                   EXFMT     SCR01
     C     Q2RECF        WHENEQ    'SCR02'
     C                   EXFMT     SCR02
     C                   ENDSL
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
     C                   ENDIF
     C************************************
     C* CMD 13 - CHG USER PARMS          *
     C************************************
     C     *INKM         IFEQ      '1'
     C                   CALL      'F0006R'
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C                   GOTO      STRT01
     C                   ENDIF                                                  END *INKM
     C                   ENDIF                                                  END CMDKEYS
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
     C     SCPROC        ANDNE     'D'
     C     SCPROC        ANDNE     'X'
     C     SCPROC        ANDNE     'A'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   SETOFF                                       909195
     C                   SELECT
     C     SCPROC        WHENEQ    'P'
     C                   SETON                                        90
     C     SCPROC        WHENEQ    'H'
     C                   SETON                                        91
     C     SCPROC        WHENEQ    'D'
     C                   SETON                                        92
     C     SCPROC        WHENEQ    'X'
     C                   SETON                                        93
     C     SCPROC        WHENEQ    'A'
     C                   SETON                                        90
     C                   MOVEL     'P'           XXPROC            1
     C                   ENDSL
     C*
     C**  WE HAVE PASSED ALL EDITS, GOTO NEXT SCREEN
     C*
     C                   SETON                                        94
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C*** BEG ADD *** 03/17/00 ****************************************
     C****************************
     C* CHOOSE TAPE OR DISKETTE  *
     C****************************
     C     SCR02T        TAG
     C*
     C                   MOVEA     '00000000'    *IN(90)
     C     SCPROC        IFEQ      'A'
     C                   SELECT
     C     XXPROC        WHENEQ    'P'
     C                   MOVEL     'H'           XXPROC
     C                   MOVEL     'P'           ##PROC
     C                   SETON                                        9194
     C     XXPROC        WHENEQ    'H'
     C                   MOVEL     'D'           XXPROC
     C                   MOVEL     'H'           ##PROC
     C                   SETON                                        9294
     C     XXPROC        WHENEQ    'D'
     C                   MOVEL     'X'           XXPROC
     C                   MOVEL     'D'           ##PROC
     C                   SETON                                        9394
     C     XXPROC        WHENEQ    'X'
     C                   MOVEL     'A'           XXPROC
     C                   MOVEL     'X'           ##PROC
     C                   SETON                                        8994
     C                   MOVEL     *BLANKS       SCPROC
     C                   ENDSL
     C*
     C                   ELSE
     C                   SELECT
     C     SCPROC        WHENEQ    'P'
     C                   SETON                                        91
     C     SCPROC        WHENEQ    'H'
     C                   SETON                                        92
     C     SCPROC        WHENEQ    'D'
     C                   SETON                                        93
     C     SCPROC        WHENEQ    'X'
     C                   SETON                                        89
     C     SCPROC        WHENEQ    *BLANK
     C                   GOTO      TERM
     C                   ENDSL
     C                   SETOFF                                       94
     C                   MOVEL     SCPROC        ##PROC
     C                   MOVEL     *BLANKS       SCPROC
     C                   ENDIF
     C*
     C* Call the CLP to Submit the Program to write workfile
     C*
     C                   SELECT
     C     ##PROC        WHENEQ    'P'
     C                   MOVEL(P)  'F9818CFL'    DUPPGM           10
     C     ##PROC        WHENEQ    'H'
     C                   MOVEL(P)  'F9819CFL'    DUPPGM
     C     ##PROC        WHENEQ    'D'
     C                   MOVEL(P)  'F9820CFL'    DUPPGM
     C     ##PROC        WHENEQ    'X'
     C                   MOVEL(P)  'F9821CFL'    DUPPGM
     C                   ENDSL
     C*
     C*** BEG DEL *** 04/26/07 ****************************************
     C*****************
     C*****************    CALL DUPPGM                 99
     C*****************    PARM           ##PROC
     C*****************    PARM 'N'       ##ERR
     C*** END DEL *** 04/26/07 ****************************************
     C*
     C     ##ERR         IFEQ      'Y'
     C                   MOVEA     '00000000'    *IN(90)
     C                   SETON                                        96
     C                   CLEAR                   SCPROC
     C                   GOTO      STRT
     C                   ELSE
     C     SCPROC        IFEQ      ' '
     C                   SETON                                        97
     C                   ENDIF
     C                   ENDIF
     C*
     C* Once the workfiles have been successfully created,
     C* Call the CLP to Submit the Program to write workfile to the Ta pe
     C*
     C                   MOVE      UPCO#         ##CO#             3
     C*
     C                   CALL      'F9813CFL'                           99
     C                   PARM                    ##CO#
     C                   PARM                    ##PROC            1
     C*
     C                   SETON                                        95
     C*
     C                   GOTO      STRT
     C*
     C* End the Program
     C*
     C     TERM          TAG
     C                   SETON                                        LR
