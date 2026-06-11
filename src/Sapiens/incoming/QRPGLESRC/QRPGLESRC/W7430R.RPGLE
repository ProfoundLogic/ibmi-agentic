     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7430R - PROMPT FOR LOSS RATIO IN VARIOUS SEQUENCES
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  05/03/99 C:118 PROJECT: WC577                                *
     H*                 MADE V3 CHANGES TO 'SORT SELECTION' IN        *
     H*                 DISPLAY FILE.  CHANGED 'UPCO#' TO 'XXCO#'.    *
     H*                                                               *
     H*  09/24/98 C:78 PROJECT: WC577                                 *
     H*                DISPLAY Q1PGM INSTEAD OF W7430F IN SCREEN.     *
     H*                                                               *
     H* 04/29/98 C:78 PROJECT WC745                                   *
     H*               CORRECT PROJ DONE ON 09/08/97. F2CT01 REPLACED  *
     H*               WITH WMFNP/FNCGFL.                              *
     H*                                                               *
     H*  12/23/97 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 REPORT STANDARDS.                    *
     H*                CHANGED FIELD SCDTE TO DEFAULT TO CURRENT DATE *
     H*                INSTEAD OF USER PARM DATE.                     *
     H*                CHANGED ACCIDENT DATE RANGE TO DEFAULT TO ZERO *
     H*                INSTEAD OF USER PARM DATE.                     *
     H*                                                               *
     H* 09/08/97  C:78  PROJ:WC745                                    *
     H*                 F2CT01 REPLACED WITH WMFNP/FNCGFL             *
     H*                                                               *
     H* 08/23/95 C:24 PROJECT: CM447                                  *
     H*               COMMERCIAL ENHANCEMENTS - ADD SELECTION BY      *
     H*               ACCIDENT DATE RANGE FOR NONCOMMON EXPIRATION    *
     H*               DATE FUNDS.  ADD SELECTION OF AN EMPLOYER RANGE *
     H*               FOR REPORTS SORTED BY EMPLOYER NUMBER.          *
     H*                                                               *
     H*****************************************************************
     FW7430F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F**** BEGIN DEL **** 04/29/98 ************************************
     F**** WDF2L   IF  E           K        DISK
     F**** END   DEL **** 04/29/98 ************************************

     D*
     D                 DS
     D*** BEGIN CHG *** 12/23/97 **************************************
     D**********                              1  120DSDTTM
     D**********                              1   60DSTIME
     D**********                              7  120DSDATE
     D*
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*** END   CHG *** 12/23/97 **************************************
     D*
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C***** BEGIN CHG **** 05/03/99 ***********************************
     C**********           KFLD           UPCO#
     C                   KFLD                    XXCO#
     C***** END CHG ****** 05/03/99 ***********************************
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKEY         KLIST
     C***** BEGIN CHG **** 05/03/99 ***********************************
     C**********           KFLD           UPCO#
     C                   KFLD                    XXCO#
     C***** END CHG ****** 05/03/99 ***********************************
     C                   KFLD                    XXFND             3 0
     C*
     C*   KEY LIST FOR THE GROUP DETAIL
     C*
     C     F1KEY         KLIST
     C***** BEGIN CHG **** 05/03/99 ***********************************
     C**********           KFLD           UPCO#
     C                   KFLD                    XXCO#
     C***** END CHG ****** 05/03/99 ***********************************
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     *LIKE         DEFINE    SCFDAT        XXFDAT
     C     *LIKE         DEFINE    SCTDAT        XXTDAT
     C***** BEGIN ADD **** 05/03/99 ***********************************
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C***** END ADD ****** 05/03/99 ***********************************
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
     C***** BEGIN ADD **** 05/03/99 ***********************************
     C                   ELSE
     C                   Z-ADD     UPCO#         XXCO#
     C***** END ADD ****** 05/03/99 ***********************************
     C                   END
     C*
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCFYR
     C*  GET THE GROUP RECORD, LOAD GROUP NAME FOR SCREEN
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C**** BEGIN ADD ****** 04/29/98 **********************************
     C   60              MOVE      *BLANK        FNCGFL
     C**** END   ADD ****** 04/29/98 **********************************
     C**** BEGIN DEL ****** 04/29/98 **********************************
     C*
     C*  LOAD FLAG THAT DETERMINES WHETHER OR NOT THIS GROUP WAS SET-UP
     C*  FOR NON-COMMON EXPIRATION DATE POLICIES
     C*
     C****** FLAG FNCGFL LOADED WHEN CHAINED TO WMFNL *****************
     C********             MOVE *BLANK    XXFCT1  1
     C********   FNKEY     CHAINPF201                60
      **** BEGIN CHG **** 09/08/97 ************************************
     C**N60****************MOVELF2CT01    XXFCT1
     C**N60***             MOVELFNCGFL    XXFCT1
      **** END   CHG **** 09/08/97 ************************************
     C**** END   DEL ****** 04/29/98 **********************************
     C*
     C                   MOVE      *OFF          *IN70
     C**** BEGIN CHG ****** 04/29/98 **********************************
     C*********  XXFCT1    IFEQ 'Y'
     C     FNCGFL        IFEQ      'Y'
     C**** END   CHG ****** 04/29/98 **********************************
     C                   MOVE      *ON           *IN70
     C                   ENDIF
     C*
     C*** BEGIN CHG *** 12/23/97 **************************************
     C**********           Z-ADDUPPRDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDYY
     C**********           MOVELSRDTO     SCDTE
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         DSDATE
     C*
     C                   Z-ADD     DSDATE        SCDTE
     C*** END   CHG *** 12/23/97 **************************************
     C*
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
     C*
     C*** BEGIN DEL *** 12/23/97 **************************************
     C**********           TIME           DSDTTM
     C*** END   DEL *** 12/23/97 **************************************
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   Z-ADD     0             SCFRPR
     C                   Z-ADD     *HIVAL        SCTOPR
     C                   Z-ADD     *ZERO         SCFEMP
     C                   Z-ADD     *ZERO         SCFDIV
     C                   Z-ADD     *HIVAL        SCTEMP
     C                   Z-ADD     *HIVAL        SCTDIV
     C*** BEGIN CHG *** 12/23/97 **************************************
     C**********           Z-ADDSCDTE     SCFDAT
     C**********           Z-ADDSCDTE     SCTDAT
     C*
     C                   Z-ADD     *ZEROS        SCFDAT
     C                   Z-ADD     *ZEROS        SCTDAT
     C*** END   CHG *** 12/23/97 **************************************
     C*
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
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
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
     C***** BEGIN ADD **** 05/03/99 ***********************************
     C                   ELSE
     C                   Z-ADD     UPCO#         XXCO#
     C***** END ADD ****** 05/03/99 ***********************************
     C                   END
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCFYR
     C*  GET THE GROUP RECORD, LOAD GROUP NAME FOR SCREEN
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C***** BEGIN DEL ***** 04/29/98 **********************************
     C*
     C*  LOAD FLAG THAT DETERMINES WHETHER OR NOT THIS GROUP WAS SET-UP
     C*  FOR NON-COMMON EXPIRATION DATE POLICIES
     C*
     C******** FLAG FNCGFL LOADED WHEN CHAINED TO WMFNL ***************
     C*******              MOVE *BLANK    XXFCT1  1
     C********   FNKEY     CHAINPF201                60
      ***** BEGIN CHG ***** 09/08/97 **********************************
     C**N60****************MOVELF2CT01    XXFCT1
     C**N60****            MOVELFNCGFL    XXFCT1
      ***** END   CHG ***** 09/08/97 **********************************
     C***** END   DEL ***** 04/29/98 **********************************
     C*
     C                   MOVE      *OFF          *IN70
     C***** BEGIN CHG ***** 04/29/98 **********************************
     C*********  XXFCT1    IFEQ 'Y'
     C     FNCGFL        IFEQ      'Y'
     C***** END   CHG ***** 04/29/98 **********************************
     C                   MOVE      *ON           *IN70
     C                   ENDIF
     C*
     C*** BEGIN CHG *** 12/23/97 **************************************
     C**********           Z-ADDUPPRDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDYY
     C**********           MOVELSRDTO     SCDTE
     C*
     C                   Z-ADD     DSDATE        SCDTE
     C*** END   CHG *** 12/23/97 **************************************
     C*
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE POLICY YEAR FOR THE GROUP
     C*
     C     *IN70         IFEQ      *OFF
     C                   MOVE      SCFYR         XXFYR             3 0
     C*          F1KEY ----------------- CO#, FND, FYR
     C     XXFYR         IFNE      *ZERO
     C     F1KEY         CHAIN     PF101                              20
     C     *IN20         CABEQ     '1'           STRT
     C                   END
     C                   ENDIF
     C*
     C*  VALIDATE THE ENTERED DATE
     C*
     C                   Z-ADD     SCDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTE
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE ACCIDENT DATE RANGE
     C*
     C     *IN70         IFEQ      *ON
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFDAT
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C                   Z-ADD     SCTDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTDAT
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
     C                   Z-ADD     SCTDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDAT
     C     XXFDAT        IFGT      XXTDAT
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C                   ENDIF
     C*
     C*  VALIDATE SORT SELECTION
     C*
     C     SCSORT        IFNE      'E'
     C     SCSORT        ANDNE     'P'
     C     SCSORT        ANDNE     'L'
     C     SCSORT        ANDNE     'F'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE EMPLOYER RANGE
     C*
     C     SCFEMP        IFGT      SCTEMP
     C     SCFEMP        OREQ      SCTEMP
     C     SCFDIV        ANDGT     SCTDIV
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     SCFEMP        IFEQ      *ZERO
     C     SCFDIV        ANDEQ     *ZERO
     C     SCTEMP        ANDEQ     *ZERO
     C     SCTDIV        ANDEQ     *ZERO
     C                   Z-ADD     *HIVAL        SCTEMP
     C                   Z-ADD     *HIVAL        SCTDIV
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  PREMIUM RANGE MUST BE IN ASCENDING ORDER
     C*
     C     SCFRPR        IFGT      SCTOPR
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C***** BEGIN CHG **** 05/03/99 ***********************************
     C**********           MOVE UPCO#     QQCO#   3
     C                   MOVE      XXCO#         QQCO#             3
     C***** END CHG ****** 05/03/99 ***********************************
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCDTE         QQDTE             8
     C                   MOVE      SCSORT        QQSORT            1
     C                   MOVE      SCFRPR        QQFRPR           11
     C                   MOVE      SCTOPR        QQTOPR           11
     C                   MOVE      SCFDAT        QQFDAT            8
     C                   MOVE      SCTDAT        QQTDAT            8
     C*
     C     SCSORT        IFNE      'E'
     C                   Z-ADD     *ZERO         SCFEMP
     C                   Z-ADD     *ZERO         SCFDIV
     C                   Z-ADD     *HIVAL        SCTEMP
     C                   Z-ADD     *HIVAL        SCTDIV
     C                   ENDIF
     C*
     C                   MOVE      SCFEMP        QQFEMP            9
     C                   MOVE      SCFDIV        QQFDIV            5
     C                   MOVE      SCTEMP        QQTEMP            9
     C                   MOVE      SCTDIV        QQTDIV            5
     C                   MOVE      *BLANK        QQCMFL            1
     C*
     C     *IN70         IFEQ      *ON
     C                   MOVE      *ON           QQCMFL
     C                   MOVE      *ZERO         QQFYR
     C                   ENDIF
     C*
     C                   CALL      'W7430C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQDTE
     C                   PARM                    QQSORT
     C                   PARM                    QQFRPR
     C                   PARM                    QQTOPR
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C                   PARM                    QQFDAT
     C                   PARM                    QQTDAT
     C                   PARM                    QQFEMP
     C                   PARM                    QQFDIV
     C                   PARM                    QQTEMP
     C                   PARM                    QQTDIV
     C                   PARM                    QQCMFL
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
