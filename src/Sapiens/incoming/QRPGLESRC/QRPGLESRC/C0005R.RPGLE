     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0005R - USER PARAMETER FILE MAINTENANCE
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program maintains the CDUPP (User Parameter File) **
     H**   CALLED FROM:                                             **
     H**     MENU                                                   **
     H**   CALLS TO:                                                **
     H**     C0505R - Usre Parameters Search                        **
     H**     SRDCK  - Check for Valid Date                          **
     H**     C0002C - Retreive User Profile Information             **
     H**     C0003C - Verify an Outque                              **
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                              *
     H* 11/25/02 C:15 Support: PC094/34878                           *
     H*               Added code to default the user name is 'Add'   *
     H*               mode.                                          *
     H*                                                              *
     H* 06/21/02 C:15 Support: PC051/33118                           *
     H*               Added code to prevent UPPAL from being updated *
     H*               in SMUPP.                                      *
     H*                                                              *
     H*   11/18/96 *** C:107 *** V2000 ***                           *
     H*            1. Version 2 format changes.                      *
     H*                                                              *
     H* 4/23/98 C:107 RE-IMPLEMENTED PROJ 99WC1 / 7/16/93            *
     H*              ADDED RESERVE AUTHORITY LIMIT .                 *
     H*                                                              *
     H****************************************************************
     H*
     H****************************************************************
     H** INDICATOR USAGE:                                           **
     H**   INDICATOR            FUNCTION DESCRIPTION                **
     H**      __       ______________________________________       **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H**   FILES:                                                   **
     H**   ______ (_____): ___________________ (___/___)            **
     H****************************************************************
     H*
     FC0005F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     UF A E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FCMUPL     UF A E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D*** End add ***** 06/21/02 *************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*** Begin add *** 06/21/02 *************************************
     IPUP02
     I              UPLVL                       U2LVL
     I              UPTYPE                      U2TYPE
     I              UPUSNM                      U2USNM
     I              UPTITL                      U2TITL
     I              UPOUTQ                      U2OUTQ
     I              UPPRDT                      U2PRDT
     I              UPPAL                       U2PAL
     I              UPRAL                       U2RAL

     C*
     C*   PARM LIST FOR INPUT DATA.
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
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    XXUSR            10
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C*
     C*   KEY LIST FOR THE FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C     QQCALL        COMP      '1'                                    09
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
     C*
     C*   PROCESS ALL FUNCTION KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  FUNCTION 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  FUNCTION 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*  FUNCTION 05 = SEARCH LIST CALL
     C*
     C     *INKE         IFEQ      '1'
     C                   CALL      'C0505R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCUSR
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCUSR         OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  GOTO THE SYSTEM FOR THE USER PROFILE INFO
     C*
     C*** Begin add *** 11/25/02 *************************************
     C                   MOVE      SCUSR         XXUSR
     C     UPKEY         CHAIN     PUP02                              30
     C*** End add ***** 11/25/02 *************************************
     C*** Begin dlt *** 11/25/02 *************************************
     C********** XXFOND    IFEQ '1'
     C**********
     C*  IF NO USER PROFILE IS FOUND; ADD RECORD WITH ONLY
     C*  TWO REQUIRED FIELDS- USER & STAFF CODE
     C**********
     C**********           MOVE '1'       *IN30
     C**********           END
     C*** End dlt ***** 11/25/02 *************************************
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCUSR         XXUSR
     C     UPKEY         CHAIN     PUP01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PUP01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      UPCO#         SCCO#
     C                   MOVE      UPUSNM        SCUSNM
     C                   MOVE      UPTITL        SCTITL
     C                   MOVE      UPSTAF        SCSTAF
     C                   MOVE      UPOUTQ        SCOUTQ
     C                   MOVE      UPIPGM        SCIPGM
     C                   Z-ADD     UPLVL         SCLVL
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT
     C                   MOVE      UPTYPE        SCTYPE
     C                   Z-ADD     UPPAL         SCPAL
      *** BEG ADD *** 4/23/98 *****************************************
     C                   Z-ADD     UPRAL         SCRAL
      *** END ADD *** 4/23/98 *****************************************
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *ZERO         SCCO#
     C*
     C     *IN30         IFEQ      '1'
     C                   MOVE      *BLANKS       SCUSNM
     C*** Begin add *** 11/25/02 *************************************
     C                   MOVE      *BLANK        SCTITL
     C                   MOVE      *BLANK        SCSTAF
     C                   MOVE      *BLANK        SCOUTQ
     C*** End add ***** 11/25/02 *************************************
     C                   ELSE
     C*** Begin dlt *** 11/25/02 *************************************
     C**********           MOVE UPUSNM    SCUSNM
     C*** End dlt ***** 11/25/02 *************************************
     C*** Begin add *** 11/25/02 *************************************
     C                   MOVE      U2USNM        SCUSNM
     C                   MOVE      U2TITL        SCTITL
     C                   MOVE      UPSTAF        SCSTAF
     C                   MOVE      U2OUTQ        SCOUTQ
     C*** End add ***** 11/25/02 *************************************
     C                   END
     C*
     C*** Begin dlt *** 11/25/02 *************************************
     C**********           MOVE *BLANK    SCTITL
     C**********           MOVE *BLANK    SCSTAF
     C**********           MOVE *BLANK    SCOUTQ
     C*** End dlt ***** 11/25/02 *************************************
     C                   MOVE      *ZERO         SCDT
     C                   MOVE      *BLANK        SCIPGM
     C                   Z-ADD     *ZERO         SCLVL
     C                   MOVE      *BLANK        SCTYPE
     C                   Z-ADD     0             SCPAL
      *** BEG ADD *** 4/23/98 *****************************************
     C                   Z-ADD     0             SCRAL
      *** END ADD *** 4/23/98 *****************************************
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C*  THE TITLE MUST NOT BE BLANK
     C*
     C     SCTITL        IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE COMPANY NUMBER MUST BE VALID
     C*
     C                   Z-ADD     SCCO#         XXCO#
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*   CHECK THE PROCESS DATE FOR A VALID DATE
     C*
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE STAFF CODE MUST NOT BE BLANK
     C*
     C     SCSTAF        CABEQ     *BLANK        STRT                     21
     C*
     C*  CHECK THE OUTQ FOR A VALID VALUE
     C*
     C                   CALL      'S0003C'
     C                   PARM                    SCOUTQ
     C                   PARM      ' '           XXFOND            1
     C     XXFOND        IFEQ      '1'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
      *** BEG ADD *** 4/23/98 **************************************
     C*
     C*   THE RESERVE AUTHORITY LIMIT CANNOT BE BLANK
     C*
     C     SCRAL         IFLE      *ZERO
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*   THE PAYMENT AUTHORITY LIMIT CANNOT BE BLANK
     C*
     C     SCPAL         IFLE      *ZERO
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
      *** END ADD *** 4/23/98 **************************************
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     UPKEY         CHAIN     PUP02                              41
     C     UPKEY         CHAIN     PUP01                              40
     C                   MOVE      SCUSR         UPUSR
     C                   MOVE      SCCO#         UPCO#
     C                   MOVE      SCUSNM        UPUSNM
     C                   MOVE      SCTITL        UPTITL
     C                   MOVE      SCSTAF        UPSTAF
     C                   MOVE      SCOUTQ        UPOUTQ
      *** BEG ADD *** 4/23/98 ****************************************
     C                   Z-ADD     SCRAL         UPRAL
      *** BEG ADD *** 4/23/98 ****************************************
     C                   Z-ADD     SCPAL         UPPAL
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPPRDT
     C                   MOVE      *BLANK        UPSTAT
     C                   MOVE      Q1USER        UPUSER
     C                   MOVE      Q1PGM         UPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPDT
     C                   Z-ADD     DSTIME        UPTIME
     C                   MOVE      SCIPGM        UPIPGM
     C                   Z-ADD     SCLVL         UPLVL
     C                   MOVE      SCTYPE        UPTYPE
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PUP01
     C     *IN41         IFEQ      '0'
     C                   UPDATE    PUP02
     C                   END
     C                   ELSE
     C                   WRITE     PUP01
     C     *IN41         IFEQ      '1'
     C                   WRITE     PUP02
     C                   END
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCUSR
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPUP01     E            PUP01E
**  TABLE UA
-- ADD --
- UPDATE -
