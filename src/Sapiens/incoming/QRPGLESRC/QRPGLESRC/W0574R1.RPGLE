     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W0574R1 - WORKERS COMP RESERVE HISTORY SEARCH
     H*****************************************************************
     H* THIS PROGRAM ALLOWS THE USER TO VIEW ALL RESERVE CHANGES      *
     H* MADE TO A CASE.                                               *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W0070R                                                      *
     H*   W0360R                                                      *
     H* CALLS TO: NONE                                                *
     H* PROGAMMER: LARRY KORZON                                       *
     H* CREATED: 05/04/92               PROJECT: AMK                  *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/25/13 C:282 PROJECT 129WSC                                 *
     H*                Replace legal literal with control field       *
     H*                for cltid 129 only.                            *
     H*                                                               *
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H*                                                               *
     H* 05/08/98 C:33 SEQ#: WC095/11243                               *
     H*               ONLY INCLUDE PAYMENTS IN OUTSTANDING CALC.      *
     H*                                                               *
     H* 02/16/98 C:15 Project: WC577                                  *
     H*               Removed code implemented on 3/11/96 which was   *
     H*               suppose to fix a support issue (records with    *
     H*               same key).  However, this fix caused a new      *
     H*               issue with the outstanding amt and Tot Inc      *
     H*               amount.                                         *
     H*                                                               *
     H* 07/10/92 LPK  PROJECT:AMK                                     *
     H* 05/25/95 C:33 PROJECT 25WAR                                   *
     H* 03/11/96 C:47 PROJECT:SUPT                                    *
     H*****************************************************************
     FW0574F1   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMRHL     IF   E           K DISK
     FWMRHL5    IF   E           K DISK
     F                                     RENAME(PRH01:PRH02)
     FWDPAL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)
      *** Beg Add ***  11/25/13  ******************************************************
     FWMCXL     IF   E           K DISK
      *** End Add ***  11/25/13  ***********************************************HEADING

     D KEYD            S              7  0 DIM(2)
     D KEYT            S              6  0 DIM(2)
     D TM              S              6  0 DIM(3)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D                 DS
     D  LN1                    1     79
     D  LN1A                   1      6
     D  LN1B                   8     10
     D  LN1C                  12     71
     D*
     D                 DS
     D  LN2                    1     79
     D  LN2CSH                 1      6
     D  LN2CS                  8     18
     D  LN2A                  21     24
     D  LN2BB                 26     36
     D  LN2B1                 26     28
     D  LN2B2                 29     29
     D  LN2B3                 30     31
     D  LN2B4                 32     32
     D  LN2B5                 33     36
     D  LN2C                  39     60
     D  LN2D                  63     66
     D  LN2E                  69     78
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DT                     1    018  0
     D                                     DIM(3)
     D  SCDT01               001    006  0
     D  SCDT02               007    012  0
     D  SCDT03               013    018  0
     D                 DS
     D  CC                     1     27  2
     D                                     DIM(3)
     D  SCCC01                 1      9  2
     D  SCCC02                10     18  2
     D  SCCC03                19     27  2
     D                 DS
     D  MC                     1     27  2
     D                                     DIM(3)
     D  SCMC01                 1      9  2
     D  SCMC02                10     18  2
     D  SCMC03                19     27  2
     D                 DS
     D  LC                     1     27  2
     D                                     DIM(3)
     D  SCLC01                 1      9  2
     D  SCLC02                10     18  2
     D  SCLC03                19     27  2
     D                 DS
     D  EC                     1     27  2
     D                                     DIM(3)
     D  SCEC01                 1      9  2
     D  SCEC02                10     18  2
     D  SCEC03                19     27  2
     D                 DS
     D  CR                     1     27  2
     D                                     DIM(3)
     D  SCCR01                 1      9  2
     D  SCCR02                10     18  2
     D  SCCR03                19     27  2
     D                 DS
     D  MR                     1     27  2
     D                                     DIM(3)
     D  SCMR01                 1      9  2
     D  SCMR02                10     18  2
     D  SCMR03                19     27  2
     D                 DS
     D  LR                     1     27  2
     D                                     DIM(3)
     D  SCLR01                 1      9  2
     D  SCLR02                10     18  2
     D  SCLR03                19     27  2
     D                 DS
     D  ER                     1     27  2
     D                                     DIM(3)
     D  SCER01                 1      9  2
     D  SCER02                10     18  2
     D  SCER03                19     27  2
     D                 DS
     D  CI                     1     27  2
     D                                     DIM(3)
     D  SCCI01                 1      9  2
     D  SCCI02                10     18  2
     D  SCCI03                19     27  2
     D                 DS
     D  MI                     1     27  2
     D                                     DIM(3)
     D  SCMI01                 1      9  2
     D  SCMI02                10     18  2
     D  SCMI03                19     27  2
     D                 DS
     D  LI                     1     27  2
     D                                     DIM(3)
     D  SCLI01                 1      9  2
     D  SCLI02                10     18  2
     D  SCLI03                19     27  2
     D                 DS
     D  EI                     1     27  2
     D                                     DIM(3)
     D  SCEI01                 1      9  2
     D  SCEI02                10     18  2
     D  SCEI03                19     27  2
     D                 DS
     D  TI                     1     30  2
     D                                     DIM(3)
     D  SCTI01                 1     10  2
     D  SCTI02                11     20  2
     D  SCTI03                21     30  2
     D                 DS
     D  US                     1     24
     D                                     DIM(3)
     D  SCUS01                 1      8
     D  SCUS02                 9     16
     D  SCUS03                17     24
     D                 DS
     D  EX                     1    180
     D                                     DIM(3)
     D  SCEX01                 1     60
     D  SCEX02                61    120
     D  SCEX03               121    180
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS
     D  SU                     1     90  2
     D                                     DIM(10)
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
     D OPTNM           C                   CONST('Reserve History')
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** Beg Add ***  11/25/13  ******************************************************
     D charLGRH        s             10                                         LEGAL RESERVE HEAD
     D CltId           s              3
      *
     d RtvCltID        pr                  ExtPgm('RTVCLTID')
     d cltID                          3    const
      *** End Add ***  11/25/13  ***********************************************HEADING

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  RESERVE HISTORY FILE
     C*
     C     RHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEFDT
     C                   KFLD                    XXTIME
     C*
     C     RHKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     RHKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEFDT
     C*
     C*  PAYMENT DETAIL FILE
     C*
     C     PAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE RESERVE TYPE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
      *** Beg Add ***  11/25/13  ******************************************************
     C*   Claim/DARTS Control Definition File
     C*
     C     keyWMcxl      KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** End Add ***  11/25/13  ******************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQSS#             9
     C                   PARM                    QQYR
     C                   PARM                    QQCASE
     C                   PARM                    QQEFDT
     C                   PARM                    QQRTCD            1
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C*
     C     *LIKE         DEFINE    RHEFDT        XXEFDT
     C     *LIKE         DEFINE    RHTIME        XXTIME
     C     *LIKE         DEFINE    PAAMT         XXAMT
     C     *LIKE         DEFINE    PACO#         XXCO#
     C     *LIKE         DEFINE    PAFND         XXFND
     C     *LIKE         DEFINE    PAFYR         XXFYR
     C     *LIKE         DEFINE    PAFYR         QQYR
     C     *LIKE         DEFINE    QQSS#         XXSS#
     C     *LIKE         DEFINE    PACASE        XXCASE
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    PACO#         QQCO#
     C     *LIKE         DEFINE    PAFND         QQFND
     C     *LIKE         DEFINE    PACASE        QQCASE
     C     *LIKE         DEFINE    SCEFDT        QQEFDT
     C*** BEGIN DLT *** 02/16/98 ***********************************************
     C********** *LIKE     DEFN PATRDT    SVEFDT
     C*** END DLT ***** 02/16/98 ***********************************************
     C*
     C*** Beg Add ***  11/25/13  ******************************************************
      /Free
       RtvCltID(CltId);
      /End-Free
     C*** End Add ***  11/25/13  ******************************************************
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQYR          XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQSS#         XXSS#
     C                   MOVE      QQEFDT        SCEFDT
     C*
     C                   Z-ADD     0             SCDTB1
     C                   Z-ADD     0             SCDTB2
     C                   Z-ADD     0             SCDTB3
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      XXCO#         ##CO#
     C                   MOVE      XXFND         ##FND
     C                   MOVE      XXFYR         ##FYR
     C                   MOVE      XXCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C*  USE CLAIM HEADER API
     C*
     C                   CALL      'WA006R'
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    XXFYR
     C                   PARM                    XXCASE
     C                   PARM                    LN1
     C                   PARM                    LN2
     C*
     C                   MOVEL     LN1B          SCFND
     C                   MOVEL     LN1C          SCFNAM
     C                   MOVEL     LN2BB         SCSS#
     C                   MOVEL     LN2C          SCCNAM
     C                   MOVEL     LN2CS         SCYRCS
     C*
     C*  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
     C*
     C     CMKY          CHAIN     PCM01                              60
     C*
     C     CMCVT#        IFNE      *BLANK
     C                   MOVEL     CMCVT#        SCCVT
     C                   MOVE      *ON           *IN64
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   MOVE      CMEMP#        XXEMP#
     C                   MOVE      CMDIV         XXDIV
     C*
     C*  USE EMP/DIV/NAME API
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   MOVEL     EMNAME        XXNAME
     C*
     C                   CALL      'WA005R'
     C                   PARM                    XXEMP#
     C                   PARM                    XXDIV
     C                   PARM                    XXNAME           40
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
     C*
     C*  GET THE COMPANY AND FUND RECORDS
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C*** BEGIN DLT *** 02/16/98 ***********************************************
     C******************** Z-ADD*ZERO     SVEFDT
     C******************** Z-ADD*ZERO     SVTIME  60
     C******************** Z-ADD*ZERO     XXCNT   30
     C*** END DLT ***** 02/16/98 ***********************************************
     C*** Beg Add ***  11/25/13  ******************************************************
      /Free
       charLGRH = *Blanks;
       If CltId = '129';
          Chain keyWMcxl pcx01;
          If %Found(wmcxl);
             charLGRH = CXlgrh;
             If charLGRH <> *Blanks;
                scLEGL = charLGRH;
             Else;
                scLEGL = 'LEGAL';
             EndIf;
          Endif;
       Else;
          scLEGL = 'LEGAL';
       EndIf;
      /End-Free
     C*** End Add ***  11/25/13  ******************************************************
     C*
     C*   LOAD SEARCH SCREEN
     C*
     C                   GOTO      REPOS
     C     STRT          TAG
     C*
     C     XXUP          IFEQ      '1'
     C***  BEGIN CHG  ***  05/08/98  *********************************
     C**********           MOVE *BLANKS   SCUP
     C                   MOVEL     RR(2)         SCUP
     C***  END   CHG  ***  05/08/98  *********************************
     C                   ELSE
     C                   MOVEL     RR(1)         SCUP
     C                   END
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
     C                   END
     C                   EXFMT     SCR01
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR01T
     C*
     C*   ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR01T
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
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  INVALID ROLL REQUEST
     C*
     C   18XXUP          IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C   19XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   Z-ADD     KEYD(2)       XXEFDT
     C                   Z-ADD     KEYT(2)       XXTIME
     C     RHKY          CHAIN     PRH02                              60
     C     RHKY2         READE     PRH02                                  60
     C                   EXSR      LOAD
     C                   EXSR      SRDATC
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYD(1)       XXEFDT
     C                   Z-ADD     KEYT(1)       XXTIME
     C     RHKY          CHAIN     PRH02                              60
     C                   READP     PRH02                                  60
     C                   EXSR      LOADP
     C                   EXSR      SRDATC
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCEFDT        IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C                   Z-ADD     0             KEYD
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   Z-ADD     0             SCEFDT
     C                   Z-ADD     0             XXTIME
     C*
     C     XXEFDT        IFEQ      0
     C*
     C                   Z-ADD     9999999       XXEFDT
     C*
     C                   END
     C*
     C     RHKY3         SETLL     PRH02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     RHKY2         READE     PRH02                                  60
     C*
     C                   ELSE
     C*
     C     RHKY2         READPE    PRH02                                  60
     C*
     C                   ENDIF
     C*
     C                   EXSR      LOAD
     C                   EXSR      SRDATC
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C                   MOVE      '*'           QQRTCD
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       DT
     C                   MOVE      *BLANKS       CC
     C                   MOVE      *BLANKS       MC
     C                   MOVE      *BLANKS       LC
     C                   MOVE      *BLANKS       EC
     C                   MOVE      *BLANKS       CR
     C                   MOVE      *BLANKS       MR
     C                   MOVE      *BLANKS       LR
     C                   MOVE      *BLANKS       ER
     C                   MOVE      *BLANKS       CI
     C                   MOVE      *BLANKS       MI
     C                   MOVE      *BLANKS       LI
     C                   MOVE      *BLANKS       EI
     C                   MOVE      *BLANKS       TI
     C                   MOVE      *BLANKS       US
     C                   MOVE      *BLANKS       EX
     C                   MOVE      *BLANKS       TM
     C                   Z-ADD     *ZERO         I1                2 0
     C                   MOVEA     '000'         *IN(71)
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
      *
      *  THIS WILL ALLOW RECORDS WITH THE SAME KEY TO BE DISPLAYED.
      *
     C*** BEGIN DLT *** 02/16/98 ***********************************************
     C********** RHEFDT    IFEQ SVEFDT
     C********** RHTIME    ANDEQSVTIME
     C**********           ADD  1         XXCNT
     C**********           SUB  XXCNT     RHTIME
     C**********           ELSE
     C**********           Z-ADDRHEFDT    SVEFDT
     C**********           Z-ADDRHTIME    SVTIME
     C**********           END
     C*** END DLT ***** 02/16/98 ***********************************************
      *
     C                   Z-ADD     RHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         DT(I1)
     C*
     C                   MOVEL     RHUSER        US(I1)
     C                   MOVEL     RHEXPL        EX(I1)
     C                   Z-ADD     RHTIME        TM(I1)
     C                   EXSR      SRCHG
     C                   EXSR      SROUTS
     C                   Z-ADD     XXCCHG        CC(I1)
     C                   Z-ADD     XXMCHG        MC(I1)
     C                   Z-ADD     XXLCHG        LC(I1)
     C                   Z-ADD     XXECHG        EC(I1)
     C                   Z-ADD     XXCRSV        CR(I1)
     C                   Z-ADD     XXMRSV        MR(I1)
     C                   Z-ADD     XXLRSV        LR(I1)
     C                   Z-ADD     XXERSV        ER(I1)
     C                   Z-ADD     XXCINC        CI(I1)
     C                   Z-ADD     XXMINC        MI(I1)
     C                   Z-ADD     XXLINC        LI(I1)
     C                   Z-ADD     XXEINC        EI(I1)
     C                   Z-ADD     XXTOTI        TI(I1)
     C     I1            ADD       70            Y                 2 0
     C                   MOVE      '1'           *IN(Y)
     C                   ENDSR
     C*
     C*  LOAD FORWARD SUBROUTINE
     C*
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C     OVER          TAG
     C*
     C*  END OF FILE
     C*
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     RHKY2         READE     PRH02                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      3                                    61
     C   61              GOTO      OVER
     C     DONE          TAG
     C*
     C                   Z-ADD     DT(1)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KEYD(1)
     C*
     C                   Z-ADD     DT(3)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KEYD(2)
     C*
     C                   Z-ADD     TM(1)         KEYT(1)
     C                   Z-ADD     TM(3)         KEYT(2)
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     4             I1
     C     OVERP         TAG
     C*
     C*  BEGINNING OF FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   EXSR      LOADB
     C                   GOTO      DONEP
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   SUB       1             I1
     C                   EXSR      FILL
     C                   READP     PRH02                                  60
     C  N60RHCO#         COMP      XXCO#                              6060
     C  N60RHFND         COMP      XXFND                              6060
     C  N60RHFYR         COMP      XXFYR                              6060
     C  N60RHCASE        COMP      XXCASE                             6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C*
     C                   Z-ADD     DT(1)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KEYD(1)
     C*
     C                   Z-ADD     DT(3)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KEYD(2)
     C*
     C                   Z-ADD     TM(1)         KEYT(1)
     C                   Z-ADD     TM(3)         KEYT(2)
     C                   ENDSR
     C*
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C     RHKY2         CHAIN     PRH02                              60
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     RHKY2         READE     PRH02                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      3                                    61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
     C*
     C*****************************************************************
     C*  SUBROUTINE TO CALCULATE RESERVE CHANGES
     C*****************************************************************
     C     SRCHG         BEGSR
     C*
     C                   Z-ADD     0             XXCCHG            9 2
     C                   Z-ADD     0             XXMCHG            9 2
     C                   Z-ADD     0             XXLCHG            9 2
     C                   Z-ADD     0             XXECHG            9 2
     C*
     C                   DO        10            Z                 2 0
     C*
     C     RT(Z)         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     RT(Z)         XXKEY
     C     TBKEY         CHAIN     PRT01                              60
     C*
     C     RTTYPE        IFEQ      'C'
     C                   ADD       SU(Z)         XXCCHG
     C                   END
     C     RTTYPE        IFEQ      'M'
     C                   ADD       SU(Z)         XXMCHG
     C                   END
     C     RTTYPE        IFEQ      'L'
     C                   ADD       SU(Z)         XXLCHG
     C                   END
     C     RTTYPE        IFEQ      'E'
     C                   ADD       SU(Z)         XXECHG
     C                   END
     C*
     C                   END
     C                   END
     C*
     C                   ENDSR
     C*****************************************************************
     C*  SUBROUTINE TO CALCULATE OUTSTANDING RESERVES
     C*****************************************************************
     C     SROUTS        BEGSR
     C*
     C                   Z-ADD     0             XXCRSV            9 2
     C                   Z-ADD     0             XXMRSV            9 2
     C                   Z-ADD     0             XXLRSV            9 2
     C                   Z-ADD     0             XXERSV            9 2
     C                   Z-ADD     0             XXCINC            9 2
     C                   Z-ADD     0             XXMINC            9 2
     C                   Z-ADD     0             XXLINC            9 2
     C                   Z-ADD     0             XXEINC            9 2
     C                   Z-ADD     0             XXTOTI           10 2
     C                   Z-ADD     RHEFDT        XXEFDT
     C                   Z-ADD     RHTIME        XXTIME
     C*
     C*  GET THE SUM OF ALL RESERVE CHANGES UP TO THE DATE AND TIME OF
     C*  THE RECORD BEING DISPLAYED.
     C*
     C     RHKY2         SETLL     PRH01
     C     RHKY2         READE     PRH01                                  50
     C     *IN50         DOWEQ     '0'
     C     RHEFDT        COMP      XXEFDT                             41  40
     C   40RHTIME        COMP      XXTIME                             41
     C     *IN41         IFEQ      '0'
     C*
     C                   DO        10            Z
     C*
     C     RT(Z)         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     RT(Z)         XXKEY
     C     TBKEY         CHAIN     PRT01                              60
     C*
     C     RTTYPE        IFEQ      'C'
     C                   ADD       SU(Z)         XXCRSV
     C                   ADD       SU(Z)         XXCINC
     C                   END
     C     RTTYPE        IFEQ      'M'
     C                   ADD       SU(Z)         XXMRSV
     C                   ADD       SU(Z)         XXMINC
     C                   END
     C     RTTYPE        IFEQ      'L'
     C                   ADD       SU(Z)         XXLRSV
     C                   ADD       SU(Z)         XXLINC
     C                   END
     C     RTTYPE        IFEQ      'E'
     C                   ADD       SU(Z)         XXERSV
     C                   ADD       SU(Z)         XXEINC
     C                   END
     C                   ADD       SU(Z)         XXTOTI
     C                   END
     C                   END
     C                   END
     C*
     C     RHKY2         READE     PRH01                                  50
     C                   END
     C*
     C*  SUBTRACT ALL PAYMENTS UP TO THE DATE AND TIME OF THE RECORD
     C*  BEING DISPLAYED.
     C*
     C     PAKEY         CHAIN     PPA01                              50
     C     *IN50         DOWEQ     '0'
     C***  BEGIN CHG  ***  05/08/98  *********************************
     C********** PATRDT    COMP XXEFDT               41  40
     C***40***** PATIME    COMP XXTIME               41
     C     PATRDT        COMP      XXEFDT                             41  41
     C***  END   CHG  ***  05/08/98  *********************************
     C     *IN41         IFEQ      '0'
     C*
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'A'
     C     PATYPE        OREQ      'Z'
     C                   Z-ADD     PAAMT         XXAMT
     C                   ELSE
     C                   Z-SUB     PAAMT         XXAMT
     C                   END
     C*
     C     PARETP        IFEQ      'C'
     C                   SUB       XXAMT         XXCRSV
     C                   END
     C     PARETP        IFEQ      'M'
     C                   SUB       XXAMT         XXMRSV
     C                   END
     C     PARETP        IFEQ      'L'
     C                   SUB       XXAMT         XXLRSV
     C                   END
     C     PARETP        IFEQ      'E'
     C                   SUB       XXAMT         XXERSV
     C                   END
     C                   END
     C*
     C     PAKEY         READE     PPA01                                  50
     C                   END
     C*
     C*
     C                   ENDSR
     C*****************************************************************
     C*  SUBROUTINE TO CHECK DT ARRAY FOR DATES THAT ARE ZERO
     C*****************************************************************
     C     SRDATC        BEGSR
     C*
     C                   Z-ADD     1             J1                1 0
     C*
     C     J1            DOWLE     3
     C*
     C     DT(J1)        IFEQ      *ZEROS
     C*
     C     J1            ADD       74            IN                2 0
     C*
     C                   MOVE      *ON           *IN(IN)
     C*
     C                   ENDIF                                                  DT5,I1 EQ 0
     C*
     C                   ADD       1             J1
     C*
     C                   ENDDO                                                  J1 WLE 3
     C*
     C                   ENDSR                                                  SRDATC
     C*****************************************************************
**
  More...
   Bottom
