     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE W7440R - PROMPT FOR FYR, SORT, SELECTION - POSTER LABELS
     H*****************************************************************
     H** PROGRAM DESCRIPTION:                                         *
     H**                                                              *
     H** This program prompts for Fund Year, Number of Copies,        *
     H** Sort (by Emp/Div or by Region), Single Employer/Division,    *
     H** Employer Number Range and Region Number Range.  The user can *
     H** select either a single Employer and Division, a range of     *
     H** Employers or a range of Regions.  These options are mutually *
     H** exclusive.                                                   *
     H**                                                              *
     H** CALLED FROM:                                                 *
     H**                                                              *
     H**   (MENU)                                                     *
     H**                                                              *
     H** CALLS TO:                                                    *
     H**                                                              *
     H**   W0050R - EMPLOYER SEARCH                                   *
     H**   W0520R - REGION SEARCH                                     *
     H**   W0006R - CHANGE USER PARAMETERS                            *
     H**   W7440C - POSTER LABEL OVERRIDE CL                          *
     H**                                                              *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/10/00 C:118 SUPPORT: WC999/21316                           *
     H*                ADDED IN DATE CONVERSION TO CYMD IN DATE RANGE *
     H*                VALIDATION.                                    *
     H*                                                               *
     H* 05/07/99 C:118 PROJECT WC577                                  *
     H*                MODIFIED CODE SO ERROR INDICATORS WORK WHEN    *
     H*                MULTIPLE EMPLOYERS ARE CHOSEN AND 2 SORT       *
     H*                RANGES ARE SELECTED.                           *
     H*                                                               *
     H* 03/12/99 C:118 PROJECT WC577                                  *
     H*                ADDED 'F10=SEARCH' FOR EMPLOYER, POLICY YEAR,  *
     H*                EMPLOYER RANGE AND REGION RANGE.               *
     H*                ADDED VALIDATION CHECKS FOR 'EFF DATE RANGE',  *
     H*                'EMPLOYER RANGE' AND 'REGION RANGE'.           *
     H*                REMOVED HIGHLIGHT REQUIRED FIELD FROM "SORT    *
     H*                SELECTIONS". REMOVED FUNCTION KEYS "F5=EMPLR#  *
     H*                SRCH", "F7=EMPLR ALPHA SRCH", "F9=RGN SRCH".   *
     H*                ADDED "F10=SEARCH". DONE IN W7440F.            *
     H*                                                               *
     H* 07/30/98 C:78 PROJECT WC577                                   *
     H*               HIGHLIGHT REQUIRED FIELDS "NUMBER OF COPIES"    *
     H*               AND "SORT SELECTIONS". DONE IN W7440F.          *
     H*                                                               *
     H* 04/29/98 C:78 PROJECT WC745                                   *
     H*               CORRECT PROJ DONE ON 09/08/97. F2CT01 REPLACED  *
     H*               WITH WMFNP/FNCGFL.                              *
     H*                                                               *
     H* 09/08/97  C:78  PROJ:WC745                                    *
     H*                 F2CT01 REPLACED WITH WMFNP/FNCGFL             *
     H*                                                               *
     H* 10/30/95  C:35  PROJ:CM008                                    *
     H*                 CHANGED TO DISPLAY DIFFERENT OPTIONS BASED ON *
     H*                 WHETHER FUND IS COMMERCIAL OR NON-COMMERCIAL  *
     H*                 (F2CT01).  COMMERCIAL FUNDS CAN PRINT LABELS  *
     H*                 FOR A RANGE OF EFFECTIVE DATES.               *
     H*                                                               *
     H* 09/14/95  C:35  PROJ:WC405                                    *
     H*                 1. EMPLOYER SEARCH (CMD 5) WAS NOT WORKING.   *
     H*                    CORRECTED TO WORK FOR 'TO' AND 'FROM'      *
     H*                    EMPLOYER AS WELL AS NEW SINGLE EMPLOYER    *
     H*                    FIELD.                                     *
     H*                 2. REGION SEARCH (CMD 7) WAS NOT WORKING.     *
     H*                    CORRECTED TO WORK FOR 'TO' AND 'FROM'      *
     H*                    REGION FIELDS.                             *
     H*                                                               *
     H*****************************************************************
     H* INDICATORS USED:                                              *
     H***** BEGIN ADD ***** 05/07/99 **********************************
     H* 19 - EMPLOYER RANGE AND REGION RANGE CANNOT BOTH BE SELECTED  *
     H***** END ADD ******* 05/07/99 **********************************
     H* 20 - SORT CHOICE IS BLANK                                     *
     H* 21 - MORE THAN ONE RANGE WAS SELECTED                         *
     H* 22 - INVALID FUND YEAR                                        *
     H* 23 - INVALID NUMBER OF COPIES                                 *
     H* 24 - EMPLOYER/DIVISION NOT FOUND                              *
     H* 25 - CURSOR IS NOT POSITIONED ON EMP# OR DIV FOR SEARCH       *
     H* 26 - POSITION CURSOR AT EMPLOYER # (SCEMP#)                   *
     H* 27 - POSITION CURSOR AT FUND YEAR (SCFYR)                     *
     H* 28 - POSITION CURSOR AT 'FROM EMPLOYER' (SCFEMP)              *
     H* 29 - POSITION CURSOR AT 'TO EMPLOYER' (SCTEMP)                *
     H* 30 - CURSOR IS NOT POSITIONED ON REGION FOR SEARCH            *
     H* 31 - POSITION CURSOR AT 'FROM REGION' (SCFRGN)                *
     H* 32 - POSITION CURSOR AT 'TO REGION' (SCTRGN)                  *
     H*                                                               *
     H***** BEGIN ADD **** 03/12/99 **********************************
     H* 33 - POSITION CURSOR TO 'FROM EFFECTIVE DATE' (SCFFDT)        *
     H* 34 - POSITION CURSOR AT 'POLICY YEAR' (SCFYR)                 *
     H* 35 - INVALID POLICY YEAR RANGE ENTERED                        *
     H* 36 - INVALID EMPLOYER RANGE ENTERED                           *
     H* 37 - INVALID REGION RANGE ENTERED                             *
     H* 38 - POSITION CURSOR ON 'TO EFFECTIVE DATE' (SCTFDT)          *
     H* 39 - POSITION CURSOR ON 'DIVISION' (SCDIV)                    *
     H* 41 - INVALID 'FROM EFFECTIVE DATE' ENTERED                    *
     H* 42 - INVALID 'TO EFFECTIVE DATE' ENTERED                      *
     H* 43 - MUTUALLY EXCLUSIVE - SCR01A                              *
     H* 44 - SORT SELECTION DOES NOT MATCH RANGE                      *
     H* 45 - MUTUALLY EXCLUSIVE - SCR01B                              *
     H* 46 - RECORD NOT FOUND                                         *
     H* 47 - INVALID SEARCH FIELD                                     *
     H* 48 - INVALID SEARCH FIELD (SCCOPY)                            *
     H* 49 - POSITION CURSOR ON SCSORT                                *
     H* 50 - INVALID SEARCH FIELD (SCFFDT)                            *
     H* 51 - INVALID SEARCH FIELD (SCTFDT)                            *
     H* 52 - INVALID SEARCH FIELD (SCSORT)                            *
     H*                                                               *
     H***** END ADD ****** 03/12/99 ***********************************
     H*                                                               *
     H* 60 - GENERAL CHAIN INDICATOR                                  *
     H*                                                               *
     H*****************************************************************
     H** FILE USAGE:                                                  *
     H**                                                              *
     H** DISPLAY FILES:                                               *
     H**                                                              *
     H**    W7440F - POSTER LABEL PROMPT SCREEN                       *
     H**                                                              *
     H** DATA FILES:                                                  *
     H**                                                              *
     H**    WMFNL  (PFN01): CO#/FND                                   *
     H**    SMCOL  (PCO01): CO#                                       *
     H**    WMUPL  (PUP01): USR                                       *
     H**    WDF1L  (PF101): CO#/FND/FYR                               *
     H**    WMEML  (PEM01): CO#/FND/EMP/DIV                           *
     H**                                                              *
     H*****************************************************************
     FW7440F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMEMP     IF   E             DISK
     FWMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
     F***** BEGIN ADD **** 03/12/99 *********************************
     F                                     INFDS(DBFDSX)
     F***** END ADD ****** 03/12/99 *********************************
     F**** BEGIN DEL ******* 04/29/98 *********************************
     F***** WDF2L   IF  E           K        DISK
     F**** END   DEL ******* 04/29/98 *********************************
     FWDELL     IF   E           K DISK
     F*
     F***** BEGIN ADD **** 03/12/99 *******************************
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL02)
     F                                     INFDS(DBFDSY)
     F***** END ADD ****** 03/12/99 *******************************

     D DBFDS           DS
     D  DBRRN                397    400B 0
     D***** BEGIN ADD **** 03/12/99 ******************************
     D DBFDSX          DS
     D  DBRRNX               397    400B 0
     D DBFDSY          DS
     D  DBRRNY               397    400B 0
     D***** END ADD ****** 03/12/99 ******************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
     C*
     C*  FUND AUXILIARY FILE
     C*
     C***** BEGIN DEL ***** 04/29/98 **********************************
     C*****      F2KY      KLIST
     C*****                KFLD           XXCO#
     C*****                KFLD           XXFND
     C***** END   DEL ***** 04/29/98 **********************************
     C*****************************************************************
     C*
     C* EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  FUND YEAR KEY LIST
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR             3 0
     C*****************************************************************
     C*
     C* KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    HFLD          XXHFLD
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
     C                   END                                                    UPKEY CHAIN
     C                   MOVE      UPCO#         XXCO#
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCFYR
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C**** BEGIN DEL **** 04/29/98 ************************************
     C*******    F2KY      CHAINPF201                60
     C**** END   DEL **** 04/29/98 ************************************
     C*
     C     *IN60         IFEQ      *ON
      ****** BEGIN CHG ****** 09/08/97 ********************************
     C*********************MOVE *BLANK    F2CT01
     C                   MOVE      *BLANK        FNCGFL
      ****** END   CHG ****** 09/08/97 ********************************
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      *BLANK        SCSORT
     C                   Z-ADD     *ZERO         SCFEMP
     C                   Z-ADD     *ZERO         SCTEMP
     C                   Z-ADD     *ZERO         SCFRGN
     C                   Z-ADD     *ZERO         SCTRGN
     C                   Z-ADD     *ZERO         SCEMP#
     C                   Z-ADD     *ZERO         SCDIV
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
      ******* BEGIN CHG ******* 09/08/97 ******************************
     C***********F2CT01    IFEQ 'Y'
     C     FNCGFL        IFEQ      'Y'
      ******* END   CHG ******* 09/08/97 ******************************
     C                   MOVEL     'SCR01B'      Q2RECF
     C                   ELSE
     C                   MOVEL     'SCR01A'      Q2RECF
      ******* BEGIN CHG ******* 09/08/97 ******************************
     C*********************ENDIF                           F2CT01 IFEQ
     C                   ENDIF                                                  FNCGFL IFEQ
      ******* END   CHG ******* 09/08/97 ******************************
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   Z-ADD     1             SCCOPY
     C*
     C     Q2RECF        IFEQ      'SCR01A'
     C                   EXFMT     SCR01A
     C                   ENDIF                                                  Q2RECF IFEQ
     C*
     C     Q2RECF        IFEQ      'SCR01B'
     C                   EXFMT     SCR01B
     C                   ENDIF                                                  Q2RECF IFEQ
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    *INKC IFEQ
     C*
     C***** BEGIN CHG **** 03/12/99 ***********************************
     C*
     C*   F10 = SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C                   Z-ADD     SCFFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCFFDT
     C                   ENDIF                                                  SRERR IFEQ
     C*
     C                   Z-ADD     SCTFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCTFDT
     C                   ENDIF                                                  SRERR IFEQ
     C*
     C                   CLEAR                   @@RRN
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C                   MOVE      HFLD          XXHFLD
     C                   MOVE      '1'           @@VIEW
     C                   EXSR      SRESRC
     C                   GOTO      STRT
     C*
     C     HFLD          WHENEQ    'SCFEMP'
     C     HFLD          OREQ      'SCTEMP'
     C                   MOVE      HFLD          XXHFLD
     C                   MOVE      '2'           @@VIEW
     C                   EXSR      SRESRC
     C*
     C     HFLD          WHENEQ    'SCFYR'
     C     SCEMP#        IFEQ      *ZERO
     C                   MOVE      HFLD          XXHFLD
     C                   MOVE      '1'           @@VIEW
     C                   EXSR      SRESRC
     C                   ELSE
     C                   EXSR      POLYRS
     C                   ENDIF                                                  SCEMP# IFEQ
     C*
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C     @@RRN         OREQ      *ZERO
     C                   EXSR      POSCSR
     C                   ENDIF                                                  *INKC IFEQ
     C                   GOTO      STRT
     C*
     C********** *INKE     IFEQ '1'
     C********** HFLD      IFEQ 'SCEMP#'
     C********** HFLD      OREQ 'SCDIV'
     C********** HFLD      OREQ 'SCFEMP'
     C********** HFLD      OREQ 'SCTEMP'
     C**********           MOVE HFLD      XXHFLD
     C**********           CALL 'W0050R'
     C********** *INKC     PARM           @@KC    1
     C********** *INKL     PARM           @@KL    1
     C**********           PARM '2'       @@CALL  1
     C**********           PARM           @@RRN   90
     C**********           PARM DBRRN     @@RRN2  90
     C**********           PARM           @@QTE   1
     C**********           PARM 2         @@VIEW  10
     C**********           PARM           @@RRN3  90
     C**********
     C********** *INKC     CABEQ*ON       TERM
     C**********
     C********** *INKL     IFEQ *ON
     C********** @@RRN     OREQ *ZERO
     C********** XXHFLD    IFEQ 'SCEMP#'
     C**********           MOVE '1'               *IN26
     C**********
     C**********           ELSE
     C********** XXHFLD    IFEQ 'SCFEMP'
     C**********           MOVE '1'               *IN28
     C**********
     C**********           ELSE
     C**********           MOVE '1'               *IN29
     C**********
     C**********           ENDIF
     C**********           ENDIF
     C**********           GOTO STRT
     C**********
     C**********           ELSE
     C**********
     C********** @@RRN     CHAINWMEMP                60
     C**********
     C********** XXHFLD    IFEQ 'SCEMP#'
     C********** XXHFLD    OREQ 'SCDIV'
     C**********           MOVE EMEMP#    SCEMP#
     C**********           MOVE EMDIV     SCDIV
     C**********           MOVE '1'       *IN27
     C**********
     C**********           ELSE
     C**********
     C********** XXHFLD    IFEQ 'SCFEMP'
     C**********           MOVE EMEMP#    SCFEMP
     C**********           MOVE '1'       *IN29
     C**********
     C**********           ELSE
     C**********
     C**********           MOVE EMEMP#    SCTEMP
     C**********
     C**********           ENDIF                           HFLD IFEQ
     C**********
     C**********           ENDIF                           HFLD IFEQ
     C**********
     C**********           ENDIF                           QQRTC IFEQ
     C**********
     C**********           ELSE
     C**********
     C**********           MOVE '1'       *IN25
     C**********
     C**********           ENDIF                           HFLD IFEQ
     C**********
     C**********           GOTO STRT
     C**********
     C**********           END
     C**********
     C**********   F7 = SEARCH LIST CALL ALPHA
     C**********
     C********** *INKG     IFEQ '1'
     C**********
     C********** HFLD      IFEQ 'SCEMP#'
     C********** HFLD      OREQ 'SCDIV'
     C********** HFLD      OREQ 'SCFEMP'
     C********** HFLD      OREQ 'SCTEMP'
     C**********
     C**********           MOVE HFLD      XXHFLD
     C**********
     C**********           CALL 'W0050R'
     C********** *INKC     PARM           @@KC    1
     C********** *INKL     PARM           @@KL    1
     C**********           PARM '2'       @@CALL  1
     C**********           PARM           @@RRN   90
     C**********           PARM DBRRN     @@RRN2  90
     C**********           PARM           @@QTE   1
     C**********           PARM 1         @@VIEW  10
     C**********           PARM           @@RRN3  90
     C**********
     C********** *INKC     IFEQ *ON       TERM
     C**********
     C********** *INKL     IFEQ *ON
     C********** @@RRN     OREQ *ZERO
     C**********
     C********** XXHFLD    IFEQ 'SCEMP#'
     C**********           MOVE '1'       *IN26
     C**********
     C**********           ELSE
     C**********
     C********** XXHFLD    IFEQ 'SCFEMP'
     C**********           MOVE '1'       *IN28
     C**********
     C**********           ELSE
     C**********
     C**********           MOVE '1'       *IN29
     C**********
     C**********           ENDIF                           XXHFLD IFEQ
     C**********
     C**********           ENDIF                           XXHFLD IFEQ
     C**********
     C**********           GOTO STRT
     C**********
     C**********           ELSE                            QQRTC IFEQ
     C**********
     C********** @@RRN     CHAINWMEMP                60
     C**********
     C********** XXHFLD    IFEQ 'SCEMP#'
     C********** XXHFLD    OREQ 'SCDIV'
     C**********
     C**********           MOVE EMEMP#    SCEMP#
     C**********           MOVE EMDIV     SCDIV
     C**********           MOVE '1'       *IN27
     C**********
     C**********           ELSE
     C**********
     C********** XXHFLD    IFEQ 'SCFEMP'
     C**********           MOVE EMEMP#    SCFEMP
     C**********           MOVE '1'       *IN29
     C**********
     C**********           ELSE
     C**********
     C**********           MOVE EMEMP#    SCTEMP
     C**********
     C**********           ENDIF                           HFLD IFEQ
     C**********
     C**********           ENDIF                           HFLD IFEQ
     C**********
     C**********           ENDIF                           QQRTC IFEQ
     C**********
     C**********           ELSE
     C**********
     C**********           MOVE '1'       *IN25
     C**********
     C**********           ENDIF                           HFLD IFEQ
     C**********
     C**********           GOTO STRT
     C**********
     C**********           ENDIF                           *INKG IFEQ
     C*
     C*        REGION SEARCH LIST CALL
     C*
     C********** *INKI     IFEQ '1'
     C     HFLD          WHENEQ    'SCFRGN'
     C     HFLD          OREQ      'SCTRGN'
     C                   MOVE      HFLD          XXHFLD
     C*
     C                   CALL      'W0520R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM      0             @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@KEY             3 0
     C*
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C     @@KEY         OREQ      *ZERO
     C                   EXSR      POSCSR
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKC IFEQ
     C*
     C     XXHFLD        IFEQ      'SCFRGN'
     C                   MOVE      @@KEY         SCFRGN
     C*
     C                   ELSE
     C*
     C                   MOVE      @@KEY         SCTRGN
     C*
     C                   ENDIF                                                  XXHFLD IFEQ
     C*
     C                   EXSR      POSCSR
     C                   GOTO      STRT
     C*
     C                   OTHER
     C                   SELECT
     C     HFLD          WHENEQ    'SCCOPY'
     C                   MOVE      *ON           *IN48
     C     HFLD          WHENEQ    'SCFFDT'
     C                   MOVE      *ON           *IN50
     C     HFLD          WHENEQ    'SCTFDT'
     C                   MOVE      *ON           *IN51
     C     HFLD          WHENEQ    'SCSORT'
     C                   MOVE      *ON           *IN52
     C                   ENDSL
     C                   GOTO      STRT
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *INKJ IFEQ
     C*
     C********** *INKC     CABEQ*ON       TERM
     C**********
     C********** *INKL     IFEQ *ON
     C********** @@KEY     OREQ *ZERO
     C********** XXHFLD    IFEQ 'SCFRGN'
     C**********
     C**********           MOVE '1'       *IN31
     C**********
     C**********           ELSE
     C**********
     C**********           MOVE '1'       *IN32
     C**********
     C**********           ENDIF                           XXHFLD IFEQ
     C**********
     C**********           GOTO STRT
     C**********           ELSE
     C**********
     C********** XXHFLD    IFEQ 'SCFRGN'
     C**********           MOVE @@KEY     SCFRGN
     C**********           MOVE '1'       *IN32
     C**********
     C**********           ELSE
     C**********
     C**********           MOVE @@KEY     SCTRGN
     C**********
     C**********           ENDIF                           HFLD IFEQ
     C**********
     C**********           EXSR POSCSR
     C**********           GOTO STRT
     C**********           ENDIF
     C**********
     C**********           ELSE
     C**********
     C**********           MOVE '1'       *IN30
     C**********
     C**********           ENDIF                           HFLD IFEQ
     C**********
     C**********           GOTO STRT
     C**********
     C**********           ENDIF
     C**********
     C*
     C***** END CHG ****** 03/12/99 **********************************
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END                                                    UPKEY CHAIN
     C*
     C                   MOVE      UPCO#         XXCO#
     C                   GOTO      STRT01
     C*
     C                   END                                                    *INKM IFEQ
     C*
     C                   END                                                    *IN11 IFEQ
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*   SORT MUST BE EQUAL TO 'E' OR 'R'
     C*
     C     SCSORT        IFNE      'E'
     C     SCSORT        ANDNE     'R'
     C     SCEMP#        ANDEQ     0
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END                                                    SCSORT IFNE
     C*
     C*   EMPLOYER RANGE AND REGION RANGE CANNOT BE SELECTED TOGETHER
     C*
     C     SCTEMP        IFNE      *ZERO
     C     SCTRGN        ANDNE     *ZERO
     C***** BEGIN CHG **** 05/07/99 ***********************************
     C**********           MOVE '1'       *IN21
     C                   MOVE      '1'           *IN19
     C***** END CHG ****** 05/07/99 ***********************************
     C                   GOTO      STRT
     C                   END                                                    SCTEMP IFNE
     C*
     C* SINGLE EMP#/DIV CANNOT BE SELECTED IF OTHER RANGE SELECTED
     C*
     C     SCEMP#        IFNE      0
     C*
     C     SCTEMP        IFNE      0
     C     SCTRGN        ORNE      0
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   ENDIF                                                  SCTEMP IFNE
     C*
     C* VALIDATE EMPLOYER/DIV IF SINGLE EMPLOYER SELECTION
     C*
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
     C*
     C     EMKY          CHAIN     PEM02                              24
     C     *IN24         IFEQ      *ON
     C*                    GOTO STRT
     C                   ENDIF                                                  *IN24 IFEQ
     C                   ENDIF                                                  SCEMP# IFEQ
     C*
     C* SORT SELECTION MUST MATCH ENTERED RANGE
     C*
     C     SCSORT        IFEQ      'E'
     C     SCTRGN        ANDNE     0
     C                   MOVE      *ON           *IN44
     C                   GOTO      STRT
     C                   ENDIF                                                  SCSORT IFEQ
     C*
     C     SCSORT        IFEQ      'R'
     C     SCTEMP        ANDNE     0
     C                   MOVE      *ON           *IN44
     C                   GOTO      STRT
     C                   ENDIF                                                  SCSORT IFEQ
     C*
     C* SINGLE EMPLOYER, EMP# RANGE OR REGION RANGE MUST BE CHOSEN FOR
     C* NON-COMMERCIAL FUNDS
     C*
      ****** BEGIN CHG ****** 09/08/97 ********************************
     C***********F2CT01    IFNE 'Y'
     C     FNCGFL        IFNE      'Y'
     C****** END   CHG ****** 09/08/97 ********************************
     C     SCEMP#        ANDEQ     0
     C     SCTEMP        ANDEQ     0
     C     SCTRGN        ANDEQ     0
     C                   MOVE      '1'           *IN43
     C                   GOTO      STRT
     C****** BEGIN CHG ****** 09/08/97 ********************************
     C*********************ENDIF                           F2CT01 IFNE
     C                   ENDIF                                                  FNCGFL IFNE
     C****** END   CHG ****** 09/08/97 ********************************
     C*
     C*   VALIDATE THE FUND YEAR ENTERED.
     C*
     C     SCEMP#        IFNE      0
     C*
     C                   MOVE      SCFYR         XXFYR
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
     C*
     C     ELKY          CHAIN     PEL01                              46
     C*
     C     *IN46         CABEQ     *ON           STRT
     C                   ELSE
     C*
      ***** BEGIN CHG ****** 09/08/97 *********************************
     C***********F2CT01    IFNE 'Y'
     C     FNCGFL        IFNE      'Y'
      ***** END   CHG ****** 09/08/97 *********************************
     C                   Z-ADD     SCFYR         XXFYR
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END                                                    *IN60 IFEQ
      ***** BEGIN CHG ****** 09/08/97 *********************************
     C*********************ENDIF                           F2CT01 IFNE
     C                   ENDIF                                                  FNCGFL IFNE
      ***** END   CHG ****** 09/08/97 *********************************
     C*
     C                   ENDIF                                                  SCEMP# IFNE
     C*
     C*   VALIDATE THE NUMBER OF COPIES (1-99)
     C*
     C     SCCOPY        IFLT      1
     C     SCCOPY        ORGT      99
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END                                                    SCCOPY IFLT
     C*
      ******* BEGIN CHG ****** 09/08/97 *******************************
     C***********F2CT01    IFEQ 'Y'
     C     FNCGFL        IFEQ      'Y'
      ******* END   CHG ****** 09/08/97 *******************************
     C*
     C     SCEMP#        IFEQ      0
     C*
     C* VALIDATE 'FROM' EFFECTIVE DATE
     C*
     C     SCFFDT        IFEQ      0
     C*
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SCFFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFFDT
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR IFEQ
      *
     C                   ENDIF                                                  SCFTDT IFEQ
      *
      * VALIDATE 'TO' EFFECTIVE DATE
      *
     C     SCTFDT        IFEQ      0
      *
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
      *
     C                   ELSE
      *
     C                   Z-ADD     SCTFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTFDT
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR IFEQ
      *
     C                   ENDIF                                                  SCTFDT IFEQ
      *
     C                   ELSE
      *
     C     SCFFDT        IFNE      0
     C     SCTFDT        ORNE      0
     C                   MOVE      *ON           *IN45
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFFDT IFNE
     C*
     C                   ENDIF                                                  SCEMP# IFEQ
     C*
     C***** BEGIN ADD **** 03/12/99 *******************************
     C*
     C*   VALIDATE 'FROM' DATE LESS THAN 'TO DATE
     C*
     C     SCFFDT        IFNE      0
     C     SCTFDT        IFNE      0
     C***** BEGIN ADD **** 01/10/00 ***********************************
     C*
     C* Convert date to CYMD for comparison
     C*
     C                   Z-ADD     SCFFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFFDT            7 0
     C*
     C                   Z-ADD     SCTFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTFDT            7 0
     C***** END   ADD **** 01/10/00 ***********************************
     C*
     C***** BEGIN CHG **** 01/10/00 ***********************************
     C********** SCFFDT    IFGT SCTFDT
     C     XXFFDT        IFGT      XXTFDT
     C***** END   CHG **** 01/10/00 ***********************************
     C                   MOVE      *ON           *IN35
     C                   EXSR      POSCSR
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFFDT IFGT
     C                   ELSE
     C                   MOVE      *ON           *IN35
     C                   EXSR      POSCSR
     C                   GOTO      STRT
     C                   ENDIF                                                  SCTFDT IFNE
     C                   ENDIF                                                  SCFFDT IFNE
     C*
     C***** END ADD ****** 03/12/99 ******************************
      ******* BEGIN CHG ****** 09/08/97 *******************************
     C*********************ENDIF                           F2CT01 IFEQ
     C                   ENDIF                                                  FNCGFL IFEQ
      ******* END   CHG ****** 09/08/97 *******************************
     C*
     C***** BEGIN ADD **** 03/12/99 ******************************
     C*
     C*    VALIDATE EMPLOYER RANGE
     C*
     C     SCFEMP        IFNE      0
     C     SCTEMP        IFNE      0
     C     SCFEMP        IFGT      SCTEMP
     C                   MOVE      *ON           *IN36
     C                   EXSR      POSCSR
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFEMP IFGT
     C                   ELSE
     C                   MOVE      *ON           *IN36
     C                   EXSR      POSCSR
     C                   GOTO      STRT
     C                   ENDIF                                                  SCTEMP IFNE
     C                   ENDIF                                                  SCFEMP IFNE
     C*
     C*    VALIDATE REGION RANGE
     C*
     C     SCFRGN        IFNE      0
     C     SCTRGN        IFNE      0
     C     SCFRGN        IFGT      SCTRGN
     C                   MOVE      *ON           *IN37
     C                   EXSR      POSCSR
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFRGN IFGT
     C                   ELSE
     C                   MOVE      *ON           *IN37
     C                   EXSR      POSCSR
     C                   GOTO      STRT
     C                   ENDIF                                                  SCTRGN IFNE
     C                   ENDIF                                                  SCFRGN IFNE
     C*
     C***** END CHG ****** 03/12/99 ***********************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCCOPY        QQCOPY            2
     C                   MOVE      SCEMP#        QQQEMP            9
     C                   MOVE      SCDIV         QQQDIV            5
     C                   MOVE      SCFFDT        QQFFDT            8
     C                   MOVE      SCTFDT        QQTFDT            8
     C                   MOVE      SCSORT        QQSORT            1
     C                   MOVE      SCFEMP        QQFEMP            9
     C                   MOVE      SCTEMP        QQTEMP            9
     C                   MOVE      SCFRGN        QQFRGN            5
     C                   MOVE      SCTRGN        QQTRGN            5
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C*
     C                   CALL      'W7440C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCOPY
     C                   PARM                    QQQEMP
     C                   PARM                    QQQDIV
     C                   PARM                    QQFFDT
     C                   PARM                    QQTFDT
     C                   PARM                    QQSORT
     C                   PARM                    QQFEMP
     C                   PARM                    QQTEMP
     C                   PARM                    QQFRGN
     C                   PARM                    QQTRGN
     C                   PARM                    QQOUTQ
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C***** BEGIN ADD **** 03/12/99 ***********************************
     C*===============================================================
     C*  SUBROUTINE TO SEARCH FOR AND RETURN AN EMPLOYER NUMBER
     C*================================================================
     C     SRESRC        BEGSR
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM      0             @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM                    @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZERO
     C                   EXSR      POSCSR
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL IFEQ
     C*
     C     @@RRN         CHAIN     WMEMP                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     XXHFLD        IFEQ      'SCEMP#'
     C     XXHFLD        OREQ      'SCDIV'
     C     XXHFLD        OREQ      'SCFYR'
     C     SCFYR         IFNE      *ZERO
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   EXSR      POSCSR
     C                   ELSE
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   EXSR      POLYRS
     C                   ENDIF                                                  SCFYR IFNE
     C                   ENDIF                                                  XXHFLD IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     XXHFLD        IFEQ      'SCFEMP'
     C                   MOVE      EMEMP#        SCFEMP
     C                   EXSR      POSCSR
     C                   GOTO      STRT
     C                   ENDIF                                                  XXHFLD IFEQ
     C*
     C     XXHFLD        IFEQ      'SCTEMP'
     C                   MOVE      EMEMP#        SCTEMP
     C                   EXSR      POSCSR
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C                   ENDSR
     C*================================================================
     C*  SUBROUTINE TO POSITION CURSOR
     C*================================================================
     C     POSCSR        BEGSR
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC IFEQ
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZERO
     C*          @@RRN2    OREQ *ZERO
     C*
     C                   SELECT
     C     XXHFLD        WHENEQ    'SCEMP#'
     C                   MOVE      '1'           *IN26
     C     XXHFLD        WHENEQ    'SCFYR'
     C                   MOVE      '1'           *IN34
     C     XXHFLD        WHENEQ    'SCDIV'
     C                   MOVE      '1'           *IN39
     C     XXHFLD        WHENEQ    'SCFEMP'
     C                   MOVE      '1'           *IN28
     C     XXHFLD        WHENEQ    'SCTEMP'
     C                   MOVE      '1'           *IN29
     C     XXHFLD        WHENEQ    'SCFRGN'
     C                   MOVE      '1'           *IN31
     C     XXHFLD        WHENEQ    'SCTRGN'
     C                   MOVE      '1'           *IN32
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   SELECT
     C     XXHFLD        WHENEQ    'SCEMP#'
     C     XXHFLD        OREQ      'SCFYR'
     C     FNCGFL        IFEQ      'Y'
     C                   MOVE      '1'           *IN33
     C                   ELSE
     C                   MOVE      '1'           *IN49
     C                   ENDIF
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCFYR'
     C     HFLD          OREQ      'SCDIV'
     C     FNCGFL        IFEQ      'Y'
     C                   MOVE      '1'           *IN33
     C                   ELSE
     C                   MOVE      '1'           *IN49
     C                   ENDIF
     C                   GOTO      STRT
     C     XXHFLD        WHENEQ    'SCFEMP'
     C                   MOVE      '1'           *IN29
     C     XXHFLD        WHENEQ    'SCTEMP'
     C                   MOVE      '1'           *IN31
     C     XXHFLD        WHENEQ    'SCFRGN'
     C                   MOVE      '1'           *IN32
     C                   ENDSL
     C                   ENDIF                                                  XXHFLD IFEQ
     C                   ENDSR
     C*================================================================
     C*   SUBROUTINE TO SEARCH ON POLICY YEARS
     C*================================================================
     C     POLYRS        BEGSR
     C*
     C                   MOVEL     SCFND         XXFND
     C                   MOVEL     SCEMP#        XXEMP#
     C                   MOVEL     SCDIV         XXDIV
     C*
     C     EMKY          CHAIN     PEM02                              60
     C*
     C                   CALL      'W0051R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM      0             @@RRN             9 0
     C                   PARM      DBRRNX        @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C*
     C     @@RRN         IFNE      *ZERO
     C     @@RRN         CHAIN     PEL02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     ELFYR         SCFYR
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  @@RRN IFNE
     C                   EXSR      POSCSR
     C                   GOTO      STRT
     C*
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C     @@RRN         OREQ      *ZERO
     C                   EXSR      POSCSR
     C                   ENDIF                                                  *INKC IFEQ
     C                   GOTO      STRT
     C*
     C                   ENDSR
     C***** END ADD ****** 03/12/99 ***********************************
