     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SR00311I - ADDITIONAL INPUT INFO FOR WC000311A
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*     THIS PROGRAM PROMPTS THE USER TO ENTER ADDITIONAL INFO FOR*
     H*     ENDORSEMENT WC000311A.                                    *
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
     H*  F3C101 - F3C116  GENERIC    10 CHARACTER                     *
     H*  F3C101 - F3C116  GENERIC    10 CHARACTER                     *
     H*  F3NM01 - F3NM14  NAME       46 CHARACTER                     *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  12/07/09  C:190 PROJECT: WC1853                              *
     H*                  Added Additional Input Lines                 *
     H*
     H*  04/15/09  C:190 SEQ68062                                     *
     H*                  Update the pgm to reflect the wording        *
     H*                  changes for the default text.                *
     H*                                                               *
     H*  05/23/00  C:90 PROJECT:WA048/22762                           *
     H*                 UPDATE MAINTENANCE FIELDS.                    *
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
     FSR00311F  CF   E             WORKSTN
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
     D XXFORM          C                   CONST('WC000311A ')
     D*
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D*
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
     D EMP1            C                   CONST('All Employees')
     D EMP2            C                   CONST('except Domestic')
     D EMP3            C                   CONST('Servants and')
     D EMP4            C                   CONST('Agricultural')
     D EMP5            C                   CONST('Employees')
     D*
      **** BEGIN CHG **** 04/15/09 ************************************
     D STAT1           C                   CONST('All States')
     D STAT2           C                   CONST('listed in item')
     D STAT3           C                   CONST('3 A of the ')
     D STAT4           C                   CONST('Policy')
     D***********   'All States'          C         STAT1
     D***********   'except those'        C         STAT2
     D***********   'Listed in item'      C         STAT3
     D***********   '3 A of the '         C         STAT4
     D***********   'Policy'              C         STAT5
      **** END   CHG **** 04/15/09 ************************************
     D*
     D WCMP1           C                   CONST('Workers'' Compensati-
     D                                     on')
     D WCMP2           C                   CONST('Law of the State')
     D WCMP3           C                   CONST('shown in Item 3A')
     D WCMP4           C                   CONST('of the Policy')

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
      **** BEGIN DLT **** 12/07/09 ************************************
     C*************        Z-ADDXXFND     SCFND
      **** END   DLT **** 12/07/09 ************************************
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
      **** BEGIN DLT **** 12/07/09 ************************************
     C***********          Z-ADDXXFYR     SCFYR
      **** END   DLT **** 12/07/09 ************************************
     C                   MOVEL     XXFORM        XXFRM1           15
      **** BEGIN ADD **** 05/23/00 ************************************
     C                   TIME                    DSDTTM
      *
     C                   MOVE      *OFF          *IN20
      **** END   ADD **** 05/23/00 ************************************
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
      **** BEGIN DLT **** 12/07/09 ************************************
     C*************        MOVELFNNAME    SCFNAM    P
      **** END   DLT **** 12/07/09 ************************************
     C*
     C     EMKY          CHAIN     PEM01                              60
      **** BEGIN DLT **** 12/07/09 ************************************
     C************         MOVELEMNAME    SCENAM    P
      **** END   DLT **** 12/07/09 ************************************
     C*
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
     C                   MOVEL     F3C301        SCC301                         STATE 1
     C                   MOVEL     F3C302        SCC302                         STATE 2
     C                   MOVEL     F3C303        SCC303                         STATE 3
     C                   MOVEL     F3C304        SCC304                         STATE 4
      **** BEG   DLT **** 04/15/09 ************************************
      **** BEGIN CHG **** 12/07/09 ************************************
     C                   MOVEL     F3C305        SCC305                         STATE 5
     C***************      MOVELF3C305    SCC305           STATE 5
      **** END   CHG **** 12/07/09 ************************************
      **** END   DLT **** 04/15/09 ************************************
     C                   MOVEL     F3C306        SCC306                         STATE 6
     C                   MOVEL     F3C307        SCC307                         STATE 7
     C                   MOVEL     F3C308        SCC308                         STATE 8
     C                   MOVEL     F3C309        SCC309                         STATE 9
     C                   MOVEL     F3C310        SCC310                         STATE 10
      **** BEGIN ADD **** 12/07/09 ************************************
     C                   MOVEL     F3C311        SCC311                         STATE 11
     C                   MOVEL     F3C312        SCC312                         STATE 12
     C                   MOVEL     F3C313        SCC313                         STATE 13
     C                   MOVEL     F3C314        SCC314                         STATE 14
     C                   MOVEL     F3C315        SCC315                         STATE 15
     C                   MOVEL     F3C316        SCC316                         STATE 16
      **** END   ADD **** 12/07/09 ************************************
     C*
     C                   MOVEL     F3C201        SCC201                         EMPLOYEE 1
     C                   MOVEL     F3C202        SCC202                         EMPLOYEE 2
     C                   MOVEL     F3C203        SCC203                         EMPLOYEE 3
     C                   MOVEL     F3C204        SCC204                         EMPLOYEE 4
     C                   MOVEL     F3C205        SCC205                         EMPLOYEE 5
     C                   MOVEL     F3C206        SCC206                         EMPLOYEE 6
     C                   MOVEL     F3C207        SCC207                         EMPLOYEE 7
     C                   MOVEL     F3C208        SCC208                         EMPLOYEE 8
     C                   MOVEL     F3C209        SCC209                         EMPLOYEE 9
     C                   MOVEL     F3C210        SCC210                         EMPLOYEE 10
      **** BEGIN ADD **** 12/07/09 ************************************
     C                   MOVEL     F3C211        SCC211                         EMPLOYEE 11
     C                   MOVEL     F3C212        SCC212                         EMPLOYEE 12
     C                   MOVEL     F3C213        SCC213                         EMPLOYEE 13
     C                   MOVEL     F3C214        SCC214                         EMPLOYEE 14
     C                   MOVEL     F3C215        SCC215                         EMPLOYEE 15
     C                   MOVEL     F3C216        SCC216                         EMPLOYEE 16
      **** END   ADD **** 12/07/09 ************************************
     C*
     C                   MOVEL     F3C401        SCC401                         COMP LAW 1
     C                   MOVEL     F3C402        SCC402                         COMP LAW 2
     C                   MOVEL     F3C403        SCC403                         COMP LAW 3
     C                   MOVEL     F3C404        SCC404                         COMP LAW 4
     C                   MOVEL     F3C405        SCC405                         COMP LAW 5
     C                   MOVEL     F3C406        SCC406                         COMP LAW 6
     C                   MOVEL     F3C407        SCC407                         COMP LAW 7
     C                   MOVEL     F3C408        SCC408                         COMP LAW 8
     C                   MOVEL     F3C409        SCC409                         COMP LAW 9
     C                   MOVEL     F3C410        SCC410                         COMP LAW 10
      **** BEGIN ADD **** 12/07/09 ************************************
     C                   MOVEL     F3C411        SCC411                         COMP LAW 11
     C                   MOVEL     F3C412        SCC412                         COMP LAW 12
     C                   MOVEL     F3C413        SCC413                         COMP LAW 13
     C                   MOVEL     F3C414        SCC414                         COMP LAW 14
     C                   MOVEL     F3C415        SCC415                         COMP LAW 15
     C                   MOVEL     F3C416        SCC416                         COMP LAW 16
      **** END   ADD **** 12/07/09 ************************************
     C*
     C                   ELSE
     C*
     C* LOAD DISPLAY DEFAULTS
     C*
      **** BEGIN CHG **** 04/15/09 ************************************
     C                   MOVEL     STAT1         SCC301                         STATE 1
     C                   MOVEL     STAT2         SCC302                         STATE 2
     C                   MOVEL     STAT3         SCC303                         STATE 3
     C                   MOVEL     STAT4         SCC304                         STATE 4
     C***************      MOVELSTAT1     SCC301           STATE 1
     C***************      MOVELSTAT2     SCC302           STATE 2
     C***************      MOVELSTAT3     SCC303           STATE 3
     C***************      MOVELSTAT4     SCC304           STATE 4
      **** BEGIN CHG **** 12/07/09 ************************************
     C*************        MOVELSTAT5     SCC305           STATE 5
     C***************      MOVELSTAT5     SCC305           STATE 5
      **** END   CHG **** 12/07/09 ************************************
      **** END   CHG **** 04/15/09 ************************************
     C*
     C                   MOVEL     EMP1          SCC201                         EMPLOYEE 1
     C                   MOVEL     EMP2          SCC202                         EMPLOYEE 2
     C                   MOVEL     EMP3          SCC203                         EMPLOYEE 3
     C                   MOVEL     EMP4          SCC204                         EMPLOYEE 4
     C                   MOVEL     EMP5          SCC205                         EMPLOYEE 5
     C*
     C                   MOVEL     WCMP1         SCC401                         COMP LAW 1
     C                   MOVEL     WCMP2         SCC402                         COMP LAW 2
     C                   MOVEL     WCMP3         SCC403                         COMP LAW 3
     C                   MOVEL     WCMP4         SCC404                         COMP LAW 4
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
     C                   MOVEL     SCC301        F3C301                         STATE 1
     C                   MOVEL     SCC302        F3C302                         STATE 2
     C                   MOVEL     SCC303        F3C303                         STATE 3
     C                   MOVEL     SCC304        F3C304                         STATE 4
      **** BEG   DLT **** 04/15/09 ************************************
      **** BEGIN CHG **** 12/07/09 ************************************
     C                   MOVEL     SCC305        F3C305                         STATE 5
     C****************     MOVELSCC305    F3C305           STATE 5
      **** END   CHG **** 12/07/09 ************************************
      **** END   DLT **** 04/15/09 ************************************
     C                   MOVEL     SCC306        F3C306                         STATE 6
     C                   MOVEL     SCC307        F3C307                         STATE 7
     C                   MOVEL     SCC308        F3C308                         STATE 8
     C                   MOVEL     SCC309        F3C309                         STATE 9
     C                   MOVEL     SCC310        F3C310                         STATE 10
      **** BEGIN ADD **** 12/07/09 ************************************
     C                   MOVEL     SCC311        F3C311                         STATE 11
     C                   MOVEL     SCC312        F3C312                         STATE 12
     C                   MOVEL     SCC313        F3C313                         STATE 13
     C                   MOVEL     SCC314        F3C314                         STATE 14
     C                   MOVEL     SCC315        F3C315                         STATE 15
     C                   MOVEL     SCC316        F3C316                         STATE 16
      **** END   ADD **** 12/07/09 ************************************
     C*
     C                   MOVEL     SCC201        F3C201                         EMPLOYEE 1
     C                   MOVEL     SCC202        F3C202                         EMPLOYEE 2
     C                   MOVEL     SCC203        F3C203                         EMPLOYEE 3
     C                   MOVEL     SCC204        F3C204                         EMPLOYEE 4
     C                   MOVEL     SCC205        F3C205                         EMPLOYEE 5
     C                   MOVEL     SCC206        F3C206                         EMPLOYEE 6
     C                   MOVEL     SCC207        F3C207                         EMPLOYEE 7
     C                   MOVEL     SCC208        F3C208                         EMPLOYEE 8
     C                   MOVEL     SCC209        F3C209                         EMPLOYEE 9
     C                   MOVEL     SCC210        F3C210                         EMPLOYEE 10
      **** BEGIN ADD **** 12/07/09 ************************************
     C                   MOVEL     SCC211        F3C211                         EMPLOYEE 11
     C                   MOVEL     SCC212        F3C212                         EMPLOYEE 12
     C                   MOVEL     SCC213        F3C213                         EMPLOYEE 13
     C                   MOVEL     SCC214        F3C214                         EMPLOYEE 14
     C                   MOVEL     SCC215        F3C215                         EMPLOYEE 15
     C                   MOVEL     SCC216        F3C216                         EMPLOYEE 16
      **** END   ADD **** 12/07/09 ************************************
     C*
     C                   MOVEL     SCC401        F3C401                         COMP LAW 1
     C                   MOVEL     SCC402        F3C402                         COMP LAW 2
     C                   MOVEL     SCC403        F3C403                         COMP LAW 3
     C                   MOVEL     SCC404        F3C404                         COMP LAW 4
     C                   MOVEL     SCC405        F3C405                         COMP LAW 5
     C                   MOVEL     SCC406        F3C406                         COMP LAW 6
     C                   MOVEL     SCC407        F3C407                         COMP LAW 7
     C                   MOVEL     SCC408        F3C408                         COMP LAW 8
     C                   MOVEL     SCC409        F3C409                         COMP LAW 9
     C                   MOVEL     SCC410        F3C410                         COMP LAW 10
      **** BEGIN ADD **** 12/07/09 ************************************
     C                   MOVEL     SCC411        F3C411                         COMP LAW 11
     C                   MOVEL     SCC412        F3C412                         COMP LAW 12
     C                   MOVEL     SCC413        F3C413                         COMP LAW 13
     C                   MOVEL     SCC414        F3C414                         COMP LAW 14
     C                   MOVEL     SCC415        F3C415                         COMP LAW 15
     C                   MOVEL     SCC416        F3C416                         COMP LAW 16
      **** END   ADD **** 12/07/09 ************************************
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
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
