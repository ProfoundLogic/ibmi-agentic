     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F9613RTX                                          *
     H*    DESCRIPTION: PROMPT FOR CPYF TO TAPE (MULTI COMPANY ONLY)  *
     H*                                                               *
     H*    CALLED FROM:                                               *
     H*      MENU                                                     *
     H*    CALLS TO:                                                  *
     H*      F9613CTX - COPY MAGNETIC TAPE WORKFILE TO TAPE           *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  05/23/02 C:079 PROJ:TX018                                    *
     H*                 1. Add field for type of media                *
     H*                 2. Use generic table file for edit/search     *
     H*                                                               *
     H*  01/06/99 C:079 PROJ:TX009                                    *
     H*                 Modify to work with YR 2000 dates             *
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
     FF9613FTX  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     F*** BEG ADD *** 05/23/02 ****************************************
     FFTGTL     IF   E           K DISK
     F*** END ADD *** 05/23/02 ****************************************

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
     C*** BEG ADD *** 05/23/02 ****************************************
     C*
     C* Key List for the GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*** END ADD *** 05/23/02 ****************************************
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
     C*** BEG ADD *** 05/23/02 ****************************************
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*** END ADD *** 05/23/02 ****************************************
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
     C*** BEG ADD *** 05/23/02 ****************************************
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C*** END ADD *** 05/23/02 ****************************************
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
     C*** BEG ADD *** 05/23/02 ****************************************
     C************************************
     C* F10 = SEARCH                     *
     C************************************
     C     *INKJ         IFEQ      '1'
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCPROC'
     C*
     C                   MOVE      'DATY'        QQID
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVE      *BLANKS       QQRTCD
     C*
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQID              4
     C                   PARM                    QQKEY            10
     C                   PARM                    QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C*
     C                   MOVEL(P)  QQKEY         SCPROC            1
     C*
     C                   MOVEL(P)  'DATY'        KKTBID
     C                   MOVEL(P)  QQKEY         KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCDADS
     C   99              MOVE      *BLANKS       SCDADS
     C*
     C                   SETON                                        28
     C*
     C                   ELSE
     C                   SETON                                        27
     C*
     C                   ENDIF                                                  QQKEY <> BLANKS
     C*
     C     HFLD          WHENEQ    'SCMDTY'
     C*
     C                   MOVE      'MDTY'        QQID
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVE      *BLANKS       QQRTCD
     C*
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQID              4
     C                   PARM                    QQKEY            10
     C                   PARM                    QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C*
     C                   MOVEL(P)  QQKEY         SCMDTY
     C*
     C                   MOVEL(P)  'MDTY'        KKTBID
     C                   MOVEL(P)  QQKEY         KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCMDDS
     C   99              MOVE      *BLANKS       SCMDDS
     C*
     C                   SETON                                        27
     C*
     C                   ELSE
     C                   SETON                                        28
     C*
     C                   ENDIF
     C*
     C                   ENDSL                                                  HFLD = SCPROC
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  INKJ = 1
     C*** END ADD *** 05/23/02 ****************************************
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
     C*** BEG ADD *** 05/23/02 ****************************************
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C*** END ADD *** 05/23/02 ****************************************
     C****************************
     C* HOSPITAL/PHYSICIAN/BOTH  *
     C****************************
     C     SCR01T        TAG
     C*
     C*** BEG ADD *** 05/23/02 ****************************************
     C*
     C* TYPE OF TEXAS DATA SUBMISSION
     C*
     C                   MOVEL(P)  'DATY'        KKTBID
     C                   MOVEL(P)  SCPROC        KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL     *BLANKS       SCDADS
     C                   SETON                                        25
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVEL     GTDESC        SCDADS
     C                   ENDIF
     C*
     C* TYPE OF MEDIA BEING USED
     C*
     C                   MOVEL(P)  'MDTY'        KKTBID
     C                   MOVEL(P)  SCMDTY        KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL     *BLANKS       SCMDDS
     C                   SETON                                        26
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVEL     GTDESC        SCMDDS
     C                   ENDIF
     C*** END ADD *** 05/23/02 ****************************************
     C*
     C*** BEG DEL *** 05/23/02 ****************************************
     C********** SCPROC    IFNE 'P'
     C********** SCPROC    ANDNE'H'
     C********** SCPROC    ANDNE'B'
     C**********           MOVE '1'       *IN25
     C**********           GOTO STRT
     C**********           END
     C*** END DEL *** 05/23/02 ****************************************
     C*
     C     SCPROC        IFNE      'H'
     C                   MOVE      '1'           *IN91
     C                   ELSE
     C                   MOVE      '1'           *IN90
     C                   END
     C*
     C*****  WE HAVE PASSED ALL EDITS, GOTO NEXT SCREEN BASED ON MEDIA
     C*****  TYPE
     C*
     C*** BEG ADD *** 05/23/02 ****************************************
     C     SCMDTY        IFNE      'T'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*** END ADD *** 05/23/02 ****************************************
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
     C                   CALL      'F9613CTX'
     C                   PARM                    QQCO#
     C                   PARM                    QQPROC
     C*** BEG ADD *** 05/23/02 ****************************************
     C                   PARM      SCMDTY        QQMCTY            1
     C*** END ADD *** 05/23/02 ****************************************
     C*
     C                   GOTO      STRT
     C*** BEG ADD *** 05/23/02 ****************************************
     C*
     C*********************************
     C* WRITE TO CDR OR 3.5" DISKETTE *
     C*********************************
     C     SCR03T        TAG
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
     C                   CALL      'F9613CTX'
     C                   PARM                    QQCO#
     C                   PARM                    QQPROC
     C                   PARM      SCMDTY        QQMCTY            1
     C*
     C                   GOTO      STRT
     C*** END ADD *** 05/23/02 ****************************************
     C*
     C* End the Program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
