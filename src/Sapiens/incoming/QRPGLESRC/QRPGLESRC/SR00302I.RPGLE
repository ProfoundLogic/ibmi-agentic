     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SR00302I - ADDITIONAL INPUT INFO FOR WC000302
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*     THIS PROGRAM PROMPTS THE USER TO ENTER ADDITIONAL INFO FOR*
     H*     ENDORSEMENT WC000302.                                     *
     H*                                                               *
     H*  PROGRAMMER: C:114                                            *
     H*  CREATED:    10/04/98               PROJECT: CM027            *
     H*****************************************************************
     H*                                                               *
     H*  THIS PROGRAM COLLECTS CUSTOM DATA FOR ENDORSEMENTS AND       *
     H*  STORES IT IN FILE WXFMP1, USING GENERIC DATA FIELDS.  EACH   *
     H*  CUSTOMIZED FORM USES THESE FIELDS DIFFERENTLY.  THIS IS A    *
     H*  LIST OF THE AVAILABLE GENERIC FIELD FORMATS.                 *
     H*                                                               *
     H*  F3A521 - F3A52A  AMOUNT    5.2 PACKED                        *
     H*  F3A541 - F3A54A  AMOUNT    5.4 PACKED                        *
     H*  F3A701 - F3A70A  AMOUNT    7.0 PACKED                        *
     H*  F3A921 - F3A92A  AMOUNT    9.2 PACKED                        *
     H*  F3AM01 - F3AM10  AMOUNT   11.2 PACKED                        *
     H*  F3FL01 - F3FL20  FLAG        1 CHARACTER                     *
     H*  F3D601 - F3D616  DATE      6.0 PACKED                        *
     H*  F3DT01 - F3DT16  DATE      8.0 ZONED                         *
     H*  F3C101 - F3C116  GENERIC    10 CHARACTER                     *
     H*  F3C201 - F3C216  GENERIC    20 CHARACTER                     *
     H*  F3C301 - F3C316  GENERIC    30 CHARACTER                     *
     H*  F3C401 - F3C442  GENERIC    40 CHARACTER                     *
     H*  F3NM01 - F3NM14  NAME       46 CHARACTER                     *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  05/26/00 C:90 PROJECT:WA048/22762                            *
     H*                UPDATE MAINTENANCE FIELDS.                     *
     H*                                                               *
     H*  04/29/99 C:113 PROJECT:                                      *
     H*                 If F3 or F12 is pressed, pass back an 'F'     *
     H*                 through QQCALL.                               *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* MISCELLANEOUS                                                 *
     H*   60   CHAIN - GENERAL CHAIN INDICATOR                        *
     H*****************************************************************
     FSR00302F  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
     FWMEML     IF   E           K DISK
     FWXFMP1    UF A E           K DISK

     D*
     D XXFORM          C                   CONST('WC000302  ')
     D*
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D*
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      **** BEGIN ADD **** 05/26/00 ************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      **** END   ADD **** 05/26/00 ************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  EMPLOYER ENDORSEMENT
     C*
     C     FMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFRM1
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
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
     C*  LIKE DEFINES
     C*
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C                   Z-ADD     *ZEROS        XXFYR             3 0
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   Z-ADD     XXFND         SCFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C                   Z-ADD     XXFYR         SCFYR
     C                   MOVEL     XXFORM        XXFRM1           15
      **** BEGIN ADD **** 05/26/00 ************************************
     C                   TIME                    DSDTTM
      *
     C                   MOVE      *OFF          *IN20
      **** END   ADD **** 05/26/00 ************************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL(P)  FNNAME        SCFNAM
     C*
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVEL(P)  EMNAME        SCENAM
     C*
     C                   MOVE      Q1USER        SCUSER
     C*
     C* CHAIN TO ENDORSEMENT FILE
     C*
     C     FMKY1         CHAIN     PFM01                              60
     C     *IN60         IFEQ      *OFF
      **** BEGIN ADD **** 05/26/00 ************************************
     C                   MOVE      *ON           *IN20
      **** END   ADD **** 05/26/00 ************************************
     C*
     C* MOVE THE CUSTOM FIELDS TO THE SCREEN
     C*
     C                   MOVEL     F3C401        SCC401                         WORK 1
     C                   MOVEL     F3C402        SCC402                         WORK 2
     C                   MOVEL     F3C403        SCC403                         WORK 3
     C                   MOVEL     F3C404        SCC404                         WORK 4
     C                   MOVEL     F3C405        SCC405                         WORK 5
     C*
     C                   ENDIF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      *ON
     C*** BEGIN ADD *** 04/29/99 **************************************
     C                   MOVE      'F'           QQCALL
     C*** END   ADD *** 04/29/99 **************************************
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   FUNCTION 12- CANCEL
     C*
     C     *INKL         IFEQ      *ON
     C*** BEGIN ADD *** 04/29/99 **************************************
     C                   MOVE      'F'           QQCALL
     C*** END   ADD *** 04/29/99 **************************************
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  INKL=ON
     C*
     C                   END                                                    IN11=ON
     C*
     C*================= END OF FUNCTION KEY PROCESSING ===============
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  ALL EDITS PASSED FOR SCREEN 1
     C*
     C   60              CLEAR                   PFM01
     C                   Z-ADD     XXCO#         F3CO#
     C                   Z-ADD     XXFND         F3FND
     C                   Z-ADD     XXFYR         F3FYR
     C                   Z-ADD     XXEMP#        F3EMP#
     C                   Z-ADD     XXDIV         F3DIV
     C                   MOVEL     XXFORM        F3FORM
     C*
     C* MOVE THE CUSTOM FIELDS TO THE FILE
     C*
     C                   MOVEL     SCC401        F3C401                         WORK 1
     C                   MOVEL     SCC402        F3C402                         WORK 2
     C                   MOVEL     SCC403        F3C403                         WORK 3
     C                   MOVEL     SCC404        F3C404                         WORK 4
     C                   MOVEL     SCC405        F3C405                         WORK 5
      **** BEGIN ADD **** 05/26/00 ************************************
     C                   MOVEL     Q1USER        F3USER
     C                   MOVEL     Q1PGM         F3PGM
     C                   Z-ADD     DSDATE        F3DT
     C                   Z-ADD     DSTIME        F3TIME
     C*
     C  N20              MOVEL     Q1USER        F3IUSR
     C  N20              MOVEL     Q1PGM         F3IPGM
     C  N20              Z-ADD     DSDATE        F3IDT
      **** END   ADD **** 05/26/00 ************************************
     C*
     C   60              WRITE     PFM01
     C  N60              UPDATE    PFM01
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
