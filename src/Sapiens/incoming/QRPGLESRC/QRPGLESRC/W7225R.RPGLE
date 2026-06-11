     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7225R - PROMPT FOR SETTLEMENT PROCESS
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  PROJECT:    114WHO      DATE:  08/23/05                      *
     H*  PROGRAMMER: 65                                               *
     H*                                                               *
     H*  THIS PROGRAM WILL BEGIN THE SETTLEMENT PROCESS FOR EITHER A  *
     H*  SINGLE EMPLOYER OR SEVERAL EMPLOYERS IN A POLICY YEAR/MONTH  *
     H*  IN A RANGE OF GROUPS.                                        *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*  11/23/10 C:255 PROJECT :114WVZ                               *
     H*                 1) Changed the Promt Text.                    *
     H*                 2) Added validation for Billing Schedule code *
     H*                    for client 114. (Controlled by *in70)      *
     H*                 3) Added new parameter in Call of W7225C.     *
     H*                                                               *
     H*  02/22/10 C:232 SUPPORT: OP114 71711                          *
     H*                 APPLY THE VALIDATION ON EMPOLYER.             *
     H*                                                               *
     H*  08/30/07 C:193 PROJECT: 114WOH                               *
     H*                 CHANGE RETRO CALCULATION BY POLICY            *
     H*                                                               *
     H*  06/11/07 C:136 SUPPORT: WA114 55857                          *
     H*                 ADD EDITS                                     *
     H*                -RESOLVE TO DARTS MUST HAVE A DARTS CODE       *
     H*                -DON'T REPRINT UNLESS THERE IS A SETTLEMENT    *
     H*                 AMOUNT.                                       *
     H*                                                               *
     H*  10/11/06  C:33  Seq#: WA099/41801                            *
     H*                  Use SC305R instead of SC580R.                *
     H*****************************************************************
     F*
     FW7225F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     F                                     INFDS(DBFDSE)
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PFRRNE)
     F*** BEGIN CHG *** 08/30/07 **************************************
     F*************WDELL   IF  E           K        DISK
     FWDELL1    IF   E           K DISK
     F*** END CHG ***** 08/30/07 **************************************
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PFRRND)
     F                                     INFDS(DBFDSY)
     FWMEXL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FWTABL     IF   E           K DISK
     F*

     D*
     D OPTNM           C                   CONST('Retrospective Settle-
     D                                     ment Process')
     D********BEG ADD******11/23/10*********************************************
     D OPTNM1          C                   CONST('Retro/Retention Sett-
     D                                     lement Process')
     D********END ADD******11/23/10*********************************************
     D*
     D DBFDSY          DS
     D  DBRRNY               397    400B 0
     D DBFDSE          DS
     D  DBRRNE               397    400B 0
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
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
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C*** BEGIN DLT *** 08/30/07 **************************************
     C*************        KFLD           SCDIV
     C*** END DLT ***** 08/30/07 **************************************
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C*** BEGIN CHG *** 08/30/07 **************************************
     C                   KFLD                    SCFYR
     C*** END CHG ***** 08/30/07 **************************************
     C                   KFLD                    SCEMP#
     C*** BEGIN DLT *** 08/30/07 **************************************
     C*************        KFLD           SCDIV
     C*************        KFLD           SCFYR
     C*** END DLT ***** 08/30/07 **************************************
     C*
     C*  KEY LIST FOR REMARK TYPE TABLE FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    SCRMCD
     C*
     C*  ADMIN/DARTS CONTROL FILE
     C*
     C     EXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  TABLE FILE CONTROL FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C     *LIKE         DEFINE    EMDT          XXFTDT
     C     *LIKE         DEFINE    EMDT          XXTTDT
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
     C*** BEG ADD ***** 11/23/10 **********************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C                   MOVE      CLTID         XXCLT#            3
     C*For client 114 set on *in70 to show Bill Sch. Plan Type field
     C     XXCLT#        IFEQ      '114'
     C                   MOVE      *ON           *IN70
     C                   ENDIF
     C*** END ADD ***** 11/23/10 **********************************
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    UPFYR         XXFYR
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C*
     C     STRT01        TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C                   Z-ADD     UPCO#         XXCO#
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   TIME                    DSDTTM
     C*
     C********BEG CHG******11/23/10*********************************************
     C**************       MOVELOPTNM     SRDFLD    P
     C     XXCLT#        IFEQ      '114'
     C                   MOVEL(P)  OPTNM1        SRDFLD
     C                   ELSE
     C                   MOVEL(P)  OPTNM         SRDFLD
     C                   ENDIF
     C********END CHG******11/23/10*********************************************
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C*  GET THE A/R TRANSACTION CODE FOR BILLED AND PAID.
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      'C'
     C     TTTYPE        ANDEQ     'B'
     C                   MOVEL     TTKEY         SCBAR
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C*  DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
     C*
     C*  PROCESS ALL F KEYS
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*  F3=EXIT
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC
     C*
     C*  F10=SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C* TABLE SEARCH - GROUP FIELDS.
     C*
     C     HFLD          IFEQ      'SCFFND'
     C     HFLD          OREQ      'SCTFND'
     C*
     C                   EXSR      GRPSCH
     C                   WRITE     SCR99
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C*  Get Default DARTS Remark Code From Admin/Darts Control File
     C*
     C     SCFFND        IFNE      *ZERO
     C     SCFFND        ANDEQ     SCTFND
     C*
     C                   Z-ADD     SCFFND        XXFND
     C*
     C     EXKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANK        SCRMCD
     C                   MOVE      *BLANK        SCDART
     C                   ELSE
     C                   MOVEL     EXSPCD        SCRMCD
     C                   MOVE      'Y'           SCDART
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCFFND IFNE
     C*
     C                   ENDIF
     C*
     C*  TABLE SEARCH - DARTS REMARK CODE
     C*
     C     HFLD          IFEQ      'SCRMCD'
     C*
     C*  RESET RRN TO ZERO, SINCE DARTS SEARCH DOESN'T USE RRN
     C*
     C                   MOVE      *ZERO         QQRRN
     C*
     C***  Begin Chg  ***  10/11/06  **************************************
     C**********           CALL 'SC580R'
     C**********           PARM           SCRMCD
     C**********           PARM           QQRTCD  1
     C**********
     C********** QQRTCD    IFEQ 'A'
     C**********           GOTO TERM
     C**********           ENDIF
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C     ##RMCD        IFNE      *BLANKS
     C                   MOVEL     ##RMCD        SCRMCD
     C                   MOVE      '*'           QQRTCD            1
     C                   ENDIF
     C***  End   Chg  ***  10/11/06  **************************************
     C*
     C                   EXSR      POSCSR
     C                   WRITE     SCR99
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C                   ENDIF                                                  SCRMCD = HFLD
     C*
      *  TABLE SEARCH-EMPLOYER
     C*
     C     HFLD          IFEQ      'SCEMP#'
     C*** BEGIN DLT *** 08/30/07 **************************************
     C***********HFLD      OREQ 'SCDIV'
     C*** BEGIN DLT *** 08/30/07 **************************************
     C     HFLD          OREQ      'SCFND'
     C*
     C                   MOVE      '2'           QQCALL
     C                   EXSR      SRCEMP
     C                   EXSR      POSCSR
     C*
     C*  IF NO EMPLOYER WAS SELECTED, REFRESH SCREEN-OTHERWISE GO DIRECTLY TO
     C*  FUND YEAR SEARCH
     C*
     C     *INKL         IFEQ      *ON
     C     QQRRN         OREQ      *ZERO
     C*
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   MOVEL     'SCFYR '      HFLD
     C                   MOVE      *OFF          *IN45
     C*
     C                   ENDIF                                                  *INKL EQ *ON
     C                   ENDIF                                                  HFLD EQ SCEMP#
     C*
     C*
      *  TABLE SEARCH-POLICY YEAR
     C*
     C     HFLD          IFEQ      'SCFYR'
     C     SCEMP#        IFEQ      *ZERO
     C     SCFND         OREQ      *ZERO
     C*
     C                   MOVE      'Y'           QQCALL
     C                   EXSR      SRCEMP
     C                   Z-ADD     QQRRN2        QQRRN
     C                   EXSR      POSCSR
     C*
     C                   ELSE
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   CALL      'W0051R'
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      '2'           QQCALL            1
     C                   PARM      0             QQRRN             9 0
     C                   PARM      DBRRNE        QQRRN2            9 0
     C                   PARM                    QQQTE             1
     C*
     C                   EXSR      POSCSR
     C*
     C                   ENDIF                                                  SCEMP# EQ *ZERO
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVEL     'SCR01'       Q2RECF
     C                   ENDIF
     C*
     C*          QQRRN     IFNE *ZERO
     C     QQRRN         CHAIN     PFRRND                             60
     C*                    ENDIF                           QQRRN NE *ZERO
     C*
     C     *IN60         IFEQ      *ZERO
     C                   MOVEL     ELFYR         SCFYR
     C                   ENDIF
     C*
     C*  Get Default DARTS Remark Code From Admin/Darts Control File
     C*
     C                   Z-ADD     SCFND         XXFND
     C     EXKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANK        SCRMCD
     C                   MOVE      *BLANK        SCDART
     C                   ELSE
     C                   MOVEL     EXSPCD        SCRMCD
     C                   MOVE      'Y'           SCDART
     C                   ENDIF
     C*
     C                   ENDIF                                                  HFLD EQ SCFYR
     C*
     C*  CMD 10 - TABLE FILE SEARCH - A/R TRANSACTION CODES
     C*
     C     HFLD          IFEQ      'SCBAR'
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C*
     C                   CALL      'W2161R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCBAR'
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C                   SETON                                        47
     C                   ELSE
     C                   MOVEL     SCKEY         SCBAR
     C                   SETON                                        48
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKJ
     C*
     C*  PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      *ON
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C*
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN10
     C*
     C*  PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      *ON
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C                   GOTO      STRT01
     C                   ENDIF                                                  *INKM
     C*
     C                   ENDIF                                                  *IN11 = *ON
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  CHECK IF NEITHER SINGLE OR MASS IS SELECTED.
     C*
     C     SCPMYR        IFEQ      *ZERO
     C     SCFND         ANDEQ     *ZERO
     C                   MOVE      *ON           *IN20
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  SCPMYR IFEQ
     C*
     C*  CHECK IF BOTH SINGLE AND MASS IS SELECTED.
     C*
     C     SCPMYR        IFNE      *ZERO
     C     SCFND         ANDNE     *ZERO
     C                   MOVE      *ON           *IN21
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  SCPMYR IFNE
     C*
     C*  VALIDATE THE FROM GROUP IF ENTERED.
     C*
     C     SCFFND        IFNE      *ZERO
     C                   Z-ADD     SCFFND        XXFND
     C     FNKY          CHAIN     PFN01                              22
     C     *IN22         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN22
     C                   ENDIF                                                  SCFFND IFNE
     C*
     C*  VALIDATE THE TO GROUP IF ENTERED.
     C*
     C     SCTFND        IFNE      *ZERO
     C                   Z-ADD     SCTFND        XXFND
     C     FNKY          CHAIN     PFN01                              23
     C     *IN23         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN23
     C                   ENDIF                                                  SCTFND IFNE
     C*
     C*  VALIDATE FROM GROUP IS NOT GREATHER THAN TO GROUP.
     C*
     C     SCFFND        IFGT      SCTFND
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  CHECK POLICY EXP MONTH/YEAR.
     C*
     C     SCPMYR        IFNE      *ZERO
     C*
     C                   Z-ADD     SCPMYR        SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     C                   MOVEL     SRDTO         SCPMYR
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        25
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCPMYR IFNE
     C*
     C*  VALIDATE THE EMPLOYER GROUP IF ENTERED.
     C*
     C     SCFND         IFNE      *ZERO
     C                   Z-ADD     SCFND         XXFND
     C     FNKY          CHAIN     PFN01                              26
     C     *IN26         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN26
     C                   ENDIF                                                  SCFND IFNE
     C*
     C*** BEG ADD ***** 02/22/10 **********************************
     C     SCFND         IFNE      *ZERO
     C     SCEMP#        ANDEQ     *ZERO
     C                   SETON                                        27
     C                   GOTO      STRT
     C                   ENDIF
     C*** END ADD ***** 02/22/10 **********************************
     C*
     C*  VALIDATE THE EMPLOYER/DIV IF ENTERED.
     C*
     C     SCEMP#        IFNE      *ZERO
     C     EMKY          CHAIN     PEM01                              27
     C     *IN27         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN27
     C                   ENDIF                                                  SCEMP#<>0
     C*
     C*  VALIDATE THE EMPLOYER/DIV/FYR IF ENTERED.
     C*
     C     SCEMP#        IFNE      *ZERO
     C     ELKY          CHAIN     PEL01                              28
     C     *IN28         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN28
      *
      * LOSSES AS OF DATE MUST BE VALID FOR SINGLE EMPLR RUN.
      *
     C                   Z-ADD     SCLVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCLVDT
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        37
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR=*ON
      *
     C                   ENDIF                                                  SCEMP#<>0
     C*
     C* VALIDATE THE BILLED A/R CODE.
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBAR         XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *ON
     C     *IN60         OREQ      *OFF
     C     TTSTAT        ANDEQ     'I'
     C                   MOVE      *ON           *IN29
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     TTTYPE        IFNE      'B'
     C     TTCTYP        ORNE      'C'
     C                   MOVE      *ON           *IN30
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* INTERIM DAYS CANNOT BE LESS THAN ZERO.
     C*
     C     SCINTD        IFLT      *ZERO
     C                   MOVE      *ON           *IN33
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  RESOLVE TO DARTS FLAG MUST BE 'Y' OR BLANK.
     C*
     C     SCDART        IFNE      'Y'
     C     SCDART        ANDNE     ' '
     C                   MOVE      *ON           *IN34
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  VALIDATE THE DARTS REMARK CODE IF NECESSARY.
     C*
     C     SCDART        IFEQ      'Y'
     C*
     C     SCRMCD        IFNE      *BLANKS
     C     SCRMCD        CHAIN     PCT01                              35
     C     *IN35         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN35 = *ON
      *** BEG ADD ***** 06/11/07 **********************************
     C                   ELSE
     C                   MOVE      *ON           *IN38
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
      *** END ADD ***** 06/11/07 **********************************
     C                   ENDIF                                                  SCRMCD <> *BLANKS
     C*
     C                   ENDIF                                                  SCDART IFEQ
     C*
     C*  VALIDATE THE REPRINT STATEMENTS FLAG.
     C*
     C     SCRPRN        IFNE      'Y'
     C     SCRPRN        ANDNE     ' '
     C                   MOVE      *ON           *IN36
     C                   GOTO      STRT
     C                   ENDIF
     C*
      *** BEG ADD ***** 06/11/07 **********************************
     C     SCRPRN        IFEQ      'Y'
     C     SCEMP#        ANDNE     *ZERO
     C     ELSTAM        ANDEQ     *ZERO
     C                   MOVE      *ON           *IN39
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *** END ADD ***** 06/11/07 **********************************
     C*** BEG ADD ***** 11/23/10 **********************************
     C*Validate Bill Schedule Plan Type for Client 114.
     C     XXCLT#        IFEQ      '114'
     C     SCBSPT        ANDNE     'I'
     C     SCBSPT        ANDNE     'R'
     C     SCBSPT        ANDNE     'B'
     C                   MOVE      *ON           *IN72
     C                   GOTO      STRT
     C                   ENDIF
     C*** END ADD ***** 11/23/10 **********************************
     C*
     C*  CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      SCFFND        QQFFND            3
     C                   MOVE      SCTFND        QQTFND            3
     C                   MOVE      SCPMYR        QQPMYR            6
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCEMP#        QQEMP#            9
     C*** BEGIN CHG *** 08/30/07 **************************************
     C*************        MOVE SCDIV     QQDIV   5
     C*** END CHG ***** 08/30/07 **************************************
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCLVDT        QQLVDT            8
     C                   MOVE      SCBAR         QQBAR             2
     C                   MOVE      SCINTD        QQINTD            2
     C                   MOVE      SCDART        QQDART            1
     C                   MOVE      SCRMCD        QQRMCD            5
     C                   MOVE      SCRPRN        QQRPRN            1
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C*
     C*** BEGIN ADD *** 08/30/07 **************************************
     C                   MOVE      *ZEROS        QQDIV             5
     C*** END ADD ***** 08/30/07 **************************************
     C*** BEG ADD ***** 11/23/10 **********************************
     C     XXCLT#        IFEQ      '114'
     C                   MOVE      SCBSPT        QQBSPT            1
     C                   ELSE
     C                   MOVE      'I'           QQBSPT
     C                   ENDIF
     C*** END ADD ***** 11/23/10 **********************************
     C                   CALL      'W7225C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFFND
     C                   PARM                    QQTFND
     C                   PARM                    QQPMYR
     C                   PARM                    QQFND
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQFYR
     C                   PARM                    QQLVDT
     C                   PARM                    QQBAR
     C                   PARM                    QQINTD
     C                   PARM                    QQDART
     C                   PARM                    QQRMCD
     C                   PARM                    QQRPRN
     C                   PARM                    QQOUTQ
     C*** BEG ADD ***** 11/23/10 **********************************
     C                   PARM                    QQBSPT
     C*** END ADD ***** 11/23/10 **********************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C*
     ******************************************************************
     **           INITIALIZATION SUBROUTINE                          **
     ******************************************************************
     C*
     C     *INZSR        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     ******************************************************************
     **           SUBROUTINE TO SEARCH FOR EMPLOYER NUMBER           **
     ******************************************************************
     C*
     C     SRCEMP        BEGSR
     C*
     C                   Z-ADD     0             QQRRN2
     C*
     C     SCFND         IFNE      *ZERO
     C                   Z-ADD     SCFND         XXFND
     C     FNKY          CHAIN     PFN01                              60
     C   60              Z-ADD     DBRRNF        QQRRN2
     C                   ENDIF
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQCALL            1
     C                   PARM      0             QQRRN             9 0
     C                   PARM                    QQRRN2            9 0
     C                   PARM                    QQQTE             1
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC IFEQ *ON
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVEL     'SCR01'       Q2RECF
     C                   ENDIF                                                  *INKL IFEQ *ON
     C*
     C     QQRRN         IFNE      *ZERO
     C*
     C     QQRRN         CHAIN     PFRRNE                             60
     C*
     C     *IN60         IFEQ      *ZERO
     C                   MOVEL     EMFND         SCFND
     C                   Z-ADD     SCFND         XXFND
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     EMEMP#        SCEMP#
     C*** BEGIN DLT *** 08/30/07 *************************************
     C*************        MOVELEMDIV     SCDIV
     C*** END DLT ***** 08/30/07 *************************************
     C                   ENDIF                                                  *IN60 IFEQ*ZERO
     C*
     C                   ENDIF                                                  QQRRN IFNE*ZERO
     C*
     C                   ENDSR
     C*====================================================================
     C* Search Groups
     C*====================================================================
     C     GRPSCH        BEGSR
     C*
     C                   MOVE      UPCO#         ##CO#
     C*
     C                   CALL      'W0010R'
     C     *INKC         PARM      *BLANK        ##KC              1
     C     *INKL         PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##CO#             3
     C                   PARM      *BLANKS       ##FND             3
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     ##FND         OREQ      *BLANK
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFFND'
     C                   MOVE      *ON           *IN40
     C     HFLD          WHENEQ    'SCTFND'
     C                   MOVE      *ON           *IN41
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFFND'
     C                   MOVE      *ON           *IN41
     C                   MOVE      ##FND         SCFFND
     C     HFLD          WHENEQ    'SCTFND'
     C                   MOVE      *ON           *IN42
     C                   MOVE      ##FND         SCTFND
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     ******************************************************************
     **                SUBROUTINE TO POSITION CURSOR                 **
     ******************************************************************
     C*
     C     POSCSR        BEGSR
     C*
      *  IF A SELECTION WAS MADE, POSITION CURSOR TO NEXT FIELD
     C*
     C     QQRRN         IFNE      *ZERO
     C     QQRTCD        OREQ      '*'
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFND'
     C     HFLD          OREQ      'SCEMP#'
     C*** BEGIN DLT *** 08/30/07 *************************************
     C***********HFLD      OREQ 'SCDIV'
     C*** END DLT ***** 08/30/07 *************************************
     C                   MOVE      *ON           *IN45
     C     HFLD          WHENEQ    'SCFYR'
     C                   MOVE      *ON           *IN46
     C     HFLD          WHENEQ    'SCRMCD'
     C                   MOVE      *ON           *IN51
     C                   MOVEL     *BLANKS       QQRTCD
     C                   ENDSL
     C*
     C                   ELSE
     C*
      *  IF NO SELECTION WAS MADE, POSITION CURSOR ON SAME FIELD
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFND'
     C                   MOVE      *ON           *IN43
     C     HFLD          WHENEQ    'SCEMP#'
     C*** BEGIN DLT *** 08/30/07 *************************************
     C***********HFLD      OREQ 'SCDIV'
     C*** END DLT ***** 08/30/07 *************************************
     C                   MOVE      *ON           *IN44
     C     HFLD          WHENEQ    'SCFYR'
     C                   MOVE      *ON           *IN45
     C     HFLD          WHENEQ    'SCRMCD'
     C                   MOVE      *ON           *IN50
     C                   ENDSL
     C*
     C                   ENDIF                                                  QQRRN NE *ZERO
     C*
     C                   ENDSR
