     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SR00106I - ADDITIONAL INPUT INFO FOR WC000106A
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*     THIS PROGRAM PROMPTS THE USER TO ENTER ADDITIONAL INFO FOR*
     H*     ENDORSEMENT WC000106A.                                    *
     H*                                                               *
     H*  PROGRAMMER: C:114                                            *
     H*  CREATED:    10/03/98               PROJECT: CM027            *
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
     H*                                                               *
     H*  07/09/01 C:90  PROJECT: SEQ#:28104                           *
     H*                 Expanded coverage % fields from 3,2 to 5,2.   *
     H*                                                               *
     H*  05/23/00 C:90  PROJECT:WA048/22762                           *
     H*                 UPDATE MAINTENANCE FIELDS.                    *
     H*                                                               *
     H*  04/29/99 C:113 PROJECT:                                      *
     H*                 If F3 or F12 is pressed, pass back an 'F'     *
     H*                 through QQCALL.                               *
     H*****************************************************************
     H* MISCELLANEOUS                                                 *
     H*   60   CHAIN - GENERAL CHAIN INDICATOR                        *
     H*****************************************************************
     FSR00106F  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
     FWMEML     IF   E           K DISK
     FWXFMP1    UF A E           K DISK
      *

     D XXFORM          C                   CONST('WC000106A ')
     D*
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
      *
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      **** BEGIN ADD **** 05/23/00 ************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      **** END   ADD **** 05/23/00 ************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *   KEY LIST FOR THE COMPANY FILE
      *

     C     COKEY         KLIST
     C                   KFLD                    XXCO#
      *
      *  FUND MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      *  EMPLOYER ENDORSEMENT
      *
     C     FMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFRM1
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *  LIKE DEFINES
      *
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
      **** BEGIN ADD **** 05/23/00 ************************************
     C                   TIME                    DSDTTM
      *
     C                   MOVE      *OFF          *IN20
      **** END   ADD **** 05/23/00 ************************************
      *
      *   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
      *
      *  GET THE COMPANY RECORD
      *
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL(P)  FNNAME        SCFNAM
      *
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVEL(P)  EMNAME        SCENAM
      *
     C                   MOVE      Q1USER        SCUSER
     C*
     C* CHAIN TO ENDORSEMENT FILE
     C*
     C     FMKY1         CHAIN     PFM01                              60
     C     *IN60         IFEQ      *OFF
      **** BEGIN ADD **** 05/23/00 ************************************
     C                   MOVE      *ON           *IN20
      **** END   ADD **** 05/23/00 ************************************
     C*
     C* MOVE THE CUSTOM FIELDS TO THE SCREEN
     C*
     C                   MOVEL     F3C101        SCC101                         STATE 1
     C                   MOVEL     F3C102        SCC102                         STATE 2
     C                   MOVEL     F3C103        SCC103                         STATE 3
     C                   MOVEL     F3C104        SCC104                         STATE 4
     C                   MOVEL     F3C105        SCC105                         STATE 5
     C                   MOVEL     F3C106        SCC106                         STATE 6
     C                   MOVEL     F3C107        SCC107                         STATE 7
     C                   MOVEL     F3C108        SCC108                         STATE 8
     C                   MOVEL     F3C109        SCC109                         STATE 9
     C                   MOVEL     F3C110        SCC110                         STATE 10
     C*
     C                   Z-ADD     F3A521        SCA521                         CVG PCT 1
     C                   Z-ADD     F3A522        SCA522                         CVG PCT 2
     C                   Z-ADD     F3A523        SCA523                         CVG PCT 3
     C                   Z-ADD     F3A524        SCA524                         CVG PCT 4
     C                   Z-ADD     F3A525        SCA525                         CVG PCT 5
     C                   Z-ADD     F3A526        SCA526                         CVG PCT 6
     C                   Z-ADD     F3A527        SCA527                         CVG PCT 7
     C                   Z-ADD     F3A528        SCA528                         CVG PCT 8
     C                   Z-ADD     F3A529        SCA529                         CVG PCT 9
     C                   Z-ADD     F3A52A        SCA52A                         CVG PCT 10
     C*
     C                   ENDIF
      *
      *   DISPLAY ALL FORMATS
      *
     C     STRT          TAG
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
      *
      *   PROCESS ALL COMMAND KEYS
      *
     C     *IN11         IFEQ      *ON
      *
      *   COMMAND 03 = END OF JOB
      *
     C     *INKC         IFEQ      *ON
     C*** BEGIN ADD *** 04/29/99 **************************************
     C                   MOVE      'F'           QQCALL
     C*** END   ADD *** 04/29/99 **************************************
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   END
      *
      *   FUNCTION 12- CANCEL
      *
     C     *INKL         IFEQ      *ON
     C*** BEGIN ADD *** 04/29/99 **************************************
     C                   MOVE      'F'           QQCALL
     C*** END   ADD *** 04/29/99 **************************************
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  INKL=ON
      *
     C                   END                                                    IN11=ON
      *
      *================= END OF FUNCTION KEY PROCESSING ===============
      *
      *   GOTO THE APPROPRIATE SCREEN
      *
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
      *
      *   PROCESS REQUEST SCREEN
      *
     C     SCR01T        TAG
      *
      *  ALL EDITS PASSED FOR SCREEN 1
      *
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
     C                   MOVEL     SCC101        F3C101                         STATE 1
     C                   MOVEL     SCC102        F3C102                         STATE 2
     C                   MOVEL     SCC103        F3C103                         STATE 3
     C                   MOVEL     SCC104        F3C104                         STATE 4
     C                   MOVEL     SCC105        F3C105                         STATE 5
     C                   MOVEL     SCC106        F3C106                         STATE 6
     C                   MOVEL     SCC107        F3C107                         STATE 7
     C                   MOVEL     SCC108        F3C108                         STATE 8
     C                   MOVEL     SCC109        F3C109                         STATE 9
     C                   MOVEL     SCC110        F3C110                         STATE 10
     C*
     C                   Z-ADD     SCA521        F3A521                         CVG PCT 1
     C                   Z-ADD     SCA522        F3A522                         CVG PCT 2
     C                   Z-ADD     SCA523        F3A523                         CVG PCT 3
     C                   Z-ADD     SCA524        F3A524                         CVG PCT 4
     C                   Z-ADD     SCA525        F3A525                         CVG PCT 5
     C                   Z-ADD     SCA526        F3A526                         CVG PCT 6
     C                   Z-ADD     SCA527        F3A527                         CVG PCT 7
     C                   Z-ADD     SCA528        F3A528                         CVG PCT 8
     C                   Z-ADD     SCA529        F3A529                         CVG PCT 9
     C                   Z-ADD     SCA52A        F3A52A                         CVG PCT 10
      **** BEGIN ADD **** 05/23/00 ************************************
     C                   MOVEL     Q1USER        F3USER
     C                   MOVEL     Q1PGM         F3PGM
     C                   Z-ADD     DSDATE        F3DT
     C                   Z-ADD     DSTIME        F3TIME
     C*
     C  N20              MOVEL     Q1USER        F3IUSR
     C  N20              MOVEL     Q1PGM         F3IPGM
     C  N20              Z-ADD     DSDATE        F3IDT
      **** END   ADD **** 05/23/00 ************************************
     C*
     C   60              WRITE     PFM01
     C  N60              UPDATE    PFM01
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
