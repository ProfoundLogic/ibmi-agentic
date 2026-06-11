     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W8667R - PROMPT FOR SUMMARY INVOICE                      *
     H*****************************************************************
     H*                                                               *
     H* PROJECT:     126WAL                                           *
     H* PROGRAMMER:  65                                               *
     H* DATE:        09/03/02                                         *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  10/11/06  C:33  Seq#: WA099/41801                            *
     H*                  Use SC305R instead of SC580R.                *
     H*                                                               *
     H*****************************************************************
     FW8667F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FSTCTL1    IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     F                                     INFDS(DBFDS3)
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     FWDELL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL02)

     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
     D*
     D OPTNM           C                   CONST('Summary Invoice Proc-
     D                                     ess')
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C*
     C* Employer Master File
     C*
     C     EMKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C* EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCFYR
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
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
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C                   MOVE      UPFND         SCFND
     C                   MOVE      'Y'           SCPAYB
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
      *
      * Center Screen Prompt in display field
      *
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
     C*
     C*   GET THE FUND RECORD
     C*
     C                   MOVE      *BLANK        SCNAME
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        SCNAME
     C                   END
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
     C* ENSURE DATES DISPLAY CORRECTLY
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C                   Z-ADD     SCDUED        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDUED
     C*
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
      *
      *   COMMAND 10 = TABLE SEARCH
      *
     C     *INKJ         IFEQ      *ON
     C*
     C*  CMD 10 - TABLE FILE SEARCH - COMMENT 1,2,3
     C*
     C     HFLD          IFEQ      'SCCOM1'
     C     HFLD          OREQ      'SCCOM2'
     C     HFLD          OREQ      'SCCOM3'
     C*
     C                   MOVEL(P)  *BLANKS       QQKEY
     C*
     C                   CALL      'W2191R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQKEY            10
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCCOM1'
     C*
     C     *INKL         IFEQ      *ON
     C     QQKEY         OREQ      *BLANKS
     C                   SETON                                        42
     C                   ELSE
     C                   SETON                                        43
     C                   MOVEL     QQKEY         SCCOM1
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCCOM2'
     C*
     C     *INKL         IFEQ      *ON
     C     QQKEY         OREQ      *BLANKS
     C                   SETON                                        43
     C                   ELSE
     C                   MOVEL     QQKEY         SCCOM2
     C                   SETON                                        44
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCCOM3'
     C*
     C     *INKL         IFEQ      *ON
     C     QQKEY         OREQ      *BLANKS
     C                   SETON                                        44
     C                   ELSE
     C                   MOVEL     QQKEY         SCCOM3
     C                   SETON                                        45
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C*  CMD 10 - TABLE FILE SEARCH - DARTS REMARK CODE
     C*
     C     HFLD          IFEQ      'SCRMCD'
     C*
     C***  Begin Chg  ***  10/11/06  **************************************
     C**********           CALL 'SC580R'
     C**********           PARM           SCRMCD
     C**********           PARM           QQRTC   1
     C**********
     C********** SCRMCD    IFEQ *BLANKS
     C********** QQRTC     OREQ 'A'
     C********** QQRTC     OREQ 'B'
     C**********           MOVE *ON       *IN41
     C**********           ELSE
     C**********           MOVE *ON       *IN42
     C**********           ENDIF
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN41
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCRMCD
     C                   MOVE      *ON           *IN42
     C                   OTHER
     C                   MOVE      *ON           *IN41
     C                   ENDSL
     C***  End   Chg  ***  10/11/06  **************************************
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C*  F10 - TABLE FILE SEARCH - POLICY YEAR
     C*
     C     HFLD          IFEQ      'SCFYR'
     C*
     C                   MOVE      *ON           *IN46                          SCFYR POSITION
     C*
     C*  EMPLOYER NUMBER EQUAL TO ZERO
     C*
     C     SCEMP#        IFEQ      *ZEROS
     C*
     C                   MOVE      *ZEROS        @1RRN
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @1KC              1
     C     *INKL         PARM                    @1KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @1RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     @1RRN         ANDNE     *ZEROS
     C*
     C     @1RRN         CHAIN     PEM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *INKL=OFF,@1RRN
     C*
     C                   ENDIF                                                  SCEMP#=0
     C*
     C*  EMPLOYER NUMBER NOT EQUAL TO ZERO
     C*
     C     SCEMP#        IFNE      *ZEROS
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '2'           @@CALL
     C                   Z-ADD     SCFND         @1RRN
     C                   Z-ADD     DBRRN3        @1RRN2
     C*
     C                   CALL      'W0051R'
     C                   PARM                    @1KC              1
     C                   PARM                    @1KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @1RRN             9 0
     C                   PARM                    @1RRN2            9 0
     C                   PARM                    QQQTE             1
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     @1RRN         IFNE      *ZEROS
     C*
     C     @1RRN         CHAIN     PEL02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ELFYR         SCFYR
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  @1RRN<>0
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  SCEMP#<>0
     C*
     C                   ENDIF                                                  HFLD=SCFYR
     C*
     C*  F10 - TABLE FILE SEARCH - EMPLOYER NUMBER OR DIVISION
     C*
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C                   MOVE      *ON           *IN47                          SCEMP# POSITION
     C*
     C                   MOVE      *ZEROS        @1RRN
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @1KC              1
     C     *INKL         PARM                    @1KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @1RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     @1RRN         ANDNE     *ZEROS
     C*
     C     @1RRN         CHAIN     PEM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *INKL=OFF,@1RRN
     C*
     C                   ENDIF                                                  HFLD=SCEMP#
     C*
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKJ IFEQ
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
     C                   END                                                    *IN10 IFEQ
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      UPFND         SCFND
     C                   GOTO      STRT01
     C                   ENDIF                                                  *INKM IFEQ
     C*
     C                   ENDIF                                                  *IN11 IFEQ
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C* VALIDATE THE POSTING DATE RANGE
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT            7 0
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT            7 0
     C*
     C     XXFRDT        IFGT      XXTODT
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   ENDIF                                                  XXFRDT IFGE
     C*
     C* VALIDATE THE TYPE OF INVOICE
     C*
     C     SCTYPE        IFNE      'D'
     C     SCTYPE        ANDNE     'I'
     C     SCTYPE        ANDNE     'A'
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
     C                   ENDIF                                                  SCTYPE IFNE
     C*
     C* VALIDATE THE DUE DATE
     C*
     C                   Z-ADD     SCDUED        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDUED
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C* VALIDATE THE INTERIM DAYS FIELD
     C*
     C     SCDAYS        IFLT      0
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C* VALIDATE THE REMARK CODE
     C*
     C     SCRMCD        IFNE      *BLANKS
     C*
     C     SCRMCD        CHAIN     PCT01                              27
     C*
     C     *IN27         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     CTSTAT        IFEQ      'I'
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   ENDIF                                                  CTSTAT IFEQ
     C*
     C                   ENDIF                                                  SCRMCD IFNE
     C*
     C*  VALIDATE THE BILLING COMMENTS
     C*
     C                   MOVE      *BLANKS       XXCOM1           30
     C                   MOVE      *BLANKS       XXCOM2           30
     C                   MOVE      *BLANKS       XXCOM3           30
     C*
     C     SCCOM1        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCCOM1        XXKEY
     C     TBKY          CHAIN     PBC01                              29
     C  N29BCSTAT        COMP      'I'                                    29
     C     *IN29         CABEQ     '1'           STRT
     C*
     C                   MOVEL     BCDESC        XXCOM1
     C*
     C                   ENDIF                                                  SCCOM1 IFNE
     C*
     C     SCCOM2        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCCOM2        XXKEY
     C     TBKY          CHAIN     PBC01                              30
     C  N30BCSTAT        COMP      'I'                                    30
     C     *IN30         CABEQ     '1'           STRT
     C*
     C                   MOVEL     BCDESC        XXCOM2
     C*
     C                   ENDIF                                                  SCCOM2 IFNE
     C*
     C     SCCOM3        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCCOM3        XXKEY
     C     TBKY          CHAIN     PBC01                              31
     C  N31BCSTAT        COMP      'I'                                    31
     C     *IN31         CABEQ     '1'           STRT
     C*
     C                   MOVEL     BCDESC        XXCOM3
     C*
     C                   ENDIF                                                  SCCOM3 IFNE
     C*
     C* VALIDATE THE PENDING BATCH FLAG
     C*
     C     SCPAYB        IFNE      'Y'
     C     SCPAYB        ANDNE     'N'
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C                   ENDIF                                                  SCPAYB IFNE 'Y'
     C*
     C*  VALIDATE THE EMPLOYER AND POLICY YEAR
     C*
     C     SCEMP#        IFNE      0
     C     SCDIV         ORNE      0
     C     SCFYR         ORNE      0
     C*
     C*  THE EMPLOYER AND DIV MUST BE ON FILE
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*  THE EMPLOYER POLICY YEAR MUST BE ON FILE
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  SCEMP# IFNE
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFRDT        QQFRDT
     C                   MOVE      SCTODT        QQTODT
     C                   MOVE      SCTYPE        QQTYPE
     C                   MOVE      SCDUED        QQDUED
     C                   MOVE      SCDAYS        QQDAYS
     C                   MOVE      SCRMCD        QQRMCD
     C                   MOVE      XXCOM1        QQCOM1
     C                   MOVE      XXCOM2        QQCOM2
     C                   MOVE      XXCOM3        QQCOM3
     C                   MOVE      SCCOM4        QQCOM4
     C                   MOVE      SCCOM5        QQCOM5
     C                   MOVE      SCCOM6        QQCOM6
     C                   MOVE      SCPAYB        QQPAYB
     C                   MOVE      SCEMP#        QQEMP#
     C                   MOVE      SCDIV         QQDIV
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      UPOUTQ        QQOUTQ
      *
     C                   CALL      'W8667C'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQDUED            8
     C                   PARM                    QQDAYS            3
     C                   PARM                    QQRMCD            5
     C                   PARM                    QQCOM1           30
     C                   PARM                    QQCOM2           30
     C                   PARM                    QQCOM3           30
     C                   PARM                    QQCOM4           30
     C                   PARM                    QQCOM5           30
     C                   PARM                    QQCOM6           30
     C                   PARM                    QQPAYB            1
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQOUTQ           10
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
