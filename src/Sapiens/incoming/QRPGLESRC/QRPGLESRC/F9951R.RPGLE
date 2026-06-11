     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H*****************************************************************
     H/TITLE F9951R - DISPLAY CHARGE LISTING PROMPT
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program sets up and submits request for Detail     **
     H**     Charge Listing.                                         **
     H**   CALLED FROM:                                              **
     H**     Menu                                                    **
     H**     F0350R - Work with Provider Bills (Opt 6)               **
     H**   CALLS TO:                                                 **
     H**     F0032R - V3 Provider Master Maint                       **
     H**     F0055R - Work With Subscribers                          **
     H**     F0060R - Work With WCMC Groups                          **
     H**     F0269R - Work with Medical Cases: Case Selection        **
     H**     F0579R - Multi Cases/Same SS# & ACDT Srch               **
     H**     F9950C - Detail Charge Listing Override                 **
     H**     SRCENTER - Subprogram to center a field                 **
     H**     SRCVTDAT - Convert date from one format to another      **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   01/11/99      PROJECT: MC152                   **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*                                                                 *
     H*  06/09/20  C:212 Project : SY420                                *
     H*                  Mask SSN                                       *
     H*****************************************************************
     H*
     H*  10/07/03 C:062 PROJECT FS267
     H*                 CHANGE EDIT ON CHARGE TYPE
     H*
     H*  05/24/01 C:062 PROJECT: FSMNT #192  BASE ONLY
     H*                 -CHANGE TITLE
     H*                 -CHANGE F3/F12 DISPLAY PROCESSING.
     H*
     H*  07/19/99 C:062 PROJECT: MC208
     H*                 POSITION CURSOR AT "CHARGE TYPE" IF COMING
     H*                 FROM WW PROVIDER BILLS.
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      20      MCR0508 - Invalid Fund                         **
     H**      21      MCR0502 - SS# required                         **
     H**      22      MCR0500 - Invalid accident                     **
     H**      23      MCR0719 - SS#/ACDT not found in Fund setup     **
     H**      24      MCR0505 - Record not found (invalid SS#/ACDT)  **
     H**      25      MCR0546 - Invalid payee                        **
     H**      27      MCR0301 - Invalid subscriber                   **
     H**      28      MCR0324 - Invalid charge type entered          **
     H**      30      Position cursor at subscriber                  **
     H**      31      Position cursor at group                       **
     H**      32      Position cursor at social security #           **
     H**      33      Position cursor at accident date               **
     H**      34      Position cursor at provider                    **
     H**      35      MC40004 - Invalid search field                 **
     H**      36      Position cursor at provider modifier           **
     H**      37      Position cursor at charge type                 **
     H**      60      General Chain                                  **
     H**      90      Redisplay screen                               **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      SMCOL    (PCO01): System company file (CO#)            **
     H**      SMUPL    (PUP01): User Parameters File (USR)           **
     H**      FMFNL    (PFN01): Fund master File (CO#/SUB#/FND)      **
     H**      FMFNP    (PFN02): Fund master File                     **
     H**      FMSML    (PSM01): Subscriber master file (CO#/SUB#)    **
     H**      FMSMP    (PSM02): Subscriber master file               **
     H**      FMCMLA   (PCM01): Claim file                           **
     H**                        (SS#/ACDT/CO#/SUB#/FND/FYR/CASE)     **
     H**      FMCML1   (PCM02): Claim file (CO#/SUB#/FND/FYR/CASE)   **
     H**      FMPDL    (PPD01): WCMC Provider Master (FEIN/MOD)      **
     H*****************************************************************
     H*
     FF9951F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     FFMSML     IF   E           K DISK
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
     FFMCMLA    IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FFMPDL     IF   E           K DISK

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D*
     D* ---------------------------------------------------------------
     D* External system information data structures
     D* ---------------------------------------------------------------
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     D* ---------------------------------------------------------------
     D* System date and time
     D* ---------------------------------------------------------------
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*** BEGIN ADD *** 05/24/01 **************************************
     D*****************************************************************
     D* Program constants.
     D*****************************************************************
     D*
     D OPTNM           C                   CONST('Detail Charge List-
     D                                     ing')

     I*** END   ADD *** 05/24/01 **************************************
     I* ---------------------------------------------------------------
     I* Parm lists
     I* ---------------------------------------------------------------

     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    KKSUB#
     C                   PARM                    KKFND
     C                   PARM                    KKSS#
     C                   PARM                    KKACDT            7 0
     C                   PARM                    KKFEIN
     C                   PARM                    KKMOD
     C*** BEGIN ADD *** 05/24/01 **************************************
     C                   PARM                    QQMODE            1            MENU/CALLED
     C*** END   ADD *** 05/24/01 **************************************
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
     C* ---------------------------------------------------------------
     C*        KEY LISTS
     C* ---------------------------------------------------------------
     C*          PCO01 - System company file
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*          PUP01 - User Parameters File
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*          PFN01 - Fund master File
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*
     C*          PSM01 - Subscriber master file
     C     SMKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C*
     C*          PCM01 - Claim file
     C     CMKY1         KLIST
     C                   KFLD                    SCSS#
     C                   KFLD                    XXACDT
     C*
     C*          PCM02 - Claim file
     C     CMKY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*          PPD01 - WCMC Provider Master
     C     PDKY          KLIST
     C                   KFLD                    SCFEIN
     C                   KFLD                    XXMOD
     C*
     C* ---------------------------------------------------------------
     C*        Defines
     C* ---------------------------------------------------------------
     C     *LIKE         DEFINE    CMDT          XXACDT
     C     *LIKE         DEFINE    Q2RECF        HLRECF
     C     *LIKE         DEFINE    PDMOD         XXMOD
     C     *LIKE         DEFINE    SCSUB#        KKSUB#
     C     *LIKE         DEFINE    SCFND         KKFND
     C     *LIKE         DEFINE    SCSS#         KKSS#
     C     *LIKE         DEFINE    SCFEIN        KKFEIN
     C     *LIKE         DEFINE    SCMOD         KKMOD
     C*
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*          M A I N L I N E
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*
     C*          Initialization
     C*          ------------------------------------------------------
     C                   EXSR      SRINIT
     C*
     C*          Process until F3
     C*          ------------------------------------------------------
     C     *INKC         DOWEQ     *OFF
     C*** BEGIN ADD *** 05/24/01 **************************************
     C     *INKL         ANDEQ     *OFF
     C*** END   ADD *** 05/24/01 **************************************
     C     *INLR         ANDEQ     *OFF
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       scSS# = MaskData('SSN':'SCSS#':SCSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   EXFMT     SCR01
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       scSS# = UnMaskData('SSN':'SCSS#':SCSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C     *INKC         IFEQ      *OFF
     C*** BEGIN ADD *** 05/24/01 **************************************
     C     *INKL         ANDEQ     *OFF
     C*** END   ADD *** 05/24/01 **************************************
     C                   EXSR      SRF10                                        Searches
     C  N90              EXSR      SREDIT                                       Edits
     C  N90              EXSR      SRCALL                                       Submit
     C                   ENDIF                                                  *INKC = Off
     C                   ENDDO                                                  *INKC = Off
     C*
     C*          End of Program
     C*          ------------------------------------------------------
     C                   SETON                                        LR
     C*
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*          S U B R O U T I N E S
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*****************************************************************
     C*          SRINIT - INITIALIZATION
     C*****************************************************************
     C     SRINIT        BEGSR
     C                   MOVE      *ZEROS        XXFYR             3 0
     C                   MOVE      *ZEROS        XXCASE            7 0
     C                   MOVE      *ZEROS        XXACDT
     C                   MOVE      *BLANKS       YYCO#             3
     C                   MOVE      *BLANKS       YYSUB#            5
     C                   MOVE      *BLANKS       YYFND             3
     C                   MOVE      *BLANKS       YYFYR             3
     C                   MOVE      *BLANKS       YYCASE            7
     C                   MOVE      *BLANKS       YYSS#             9
     C                   MOVE      *BLANKS       YYACDT            8
     C                   MOVE      *BLANKS       YYFEIN            9
     C                   MOVE      *BLANKS       YYMOD             3
     C                   MOVE      *BLANKS       YYTYPE            1
     C*
     C*          Get user parameter file
     C*          ------------------------------------------------------
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   END
     C*
     C*          Get company record
     C*          ------------------------------------------------------
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*          Initialize heading
     C*          ------------------------------------------------------
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*** BEGIN CHG *** 05/24/01 **************************************
     C**********           MOVELQQPRDS    SRDFLD    P
     C                   MOVEL(P)  OPTNM         SRDFLD
     C*** END   CHG *** 05/24/01 **************************************
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
     C                   MOVE      UPSUB#        SCSUB#
     C                   MOVE      UPFND         SCFND
     C                   SETON                                        32
     C*
     C*          If parms passed: use them
     C*          ------------------------------------------------------
     C     Q1PRMS        IFGE      3
     C                   MOVE      KKSUB#        SCSUB#
     C                   MOVE      KKFND         SCFND
     C                   MOVE      KKSS#         SCSS#
     C                   Z-ADD     KKACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C                   MOVE      KKFEIN        SCFEIN
     C                   MOVE      KKMOD         SCMOD
     C*** BEGIN ADD *** 07/19/99 **************************************
     C                   SETON                                        37
     C                   SETOFF                                       32
     C*** END   ADD *** 07/19/99 **************************************
     C                   ENDIF                                                  PRMS >= 3
     C*** BEGIN ADD *** 05/24/01 **************************************
     C     Q1PRMS        IFGT      8
     C     QQMODE        IFEQ      'Y'
     C                   SETON                                        16
     C                   ELSE
     C                   SETOFF                                       16
     C                   ENDIF
     C                   ELSE
     C                   SETOFF                                       16
     C                   ENDIF
     C*** END   ADD *** 05/24/01 **************************************
     C                   ENDSR
     C*
     C*****************************************************************
     C*          SRF10  - SEARCH
     C*****************************************************************
     C     SRF10         BEGSR
     C                   SETOFF                                       90
     C     *INKJ         IFEQ      '1'
     C                   DO
     C*
     C*          Subscriber search
     C*          ------------------------------------------------------
     C     HFLD          IFEQ      'SCSUB#'
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
     C*
     C     *INKC         IFEQ      *ON
     C                   SETON                                        LR
     C                   LEAVE
     C                   ENDIF                                                  *INKC = On
     C*
     C     ##RRN         CHAIN     PSM02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB#
     C                   SETON                                        3190
     C                   ELSE                                                   *IN60 = Off
     C                   SETON                                        3090
     C                   ENDIF                                                  *IN60 = Off
     C                   LEAVE
     C                   ENDIF                                                  HFLD = SCSUB#
     C*
     C*          Group search
     C*          ------------------------------------------------------
     C     HFLD          IFEQ      'SCFND'
     C                   CALL      'F0060R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
     C     *INKC         IFEQ      *ON
     C                   SETON                                        LR
     C                   LEAVE
     C                   ENDIF                                                  *INKC = On
     C*
     C     ##RRN         CHAIN     PFN02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      FNFND         SCFND
     C                   SETON                                        3290
     C                   ELSE                                                   *IN60 = Off
     C                   SETON                                        3190
     C                   ENDIF                                                  *IN60 = Off
     C                   LEAVE
     C                   ENDIF                                                  HFLD = SCFND
     C*
     C*          Social Security # & Accident date search
     C*          ------------------------------------------------------
     C     HFLD          IFEQ      'SCSS#'
     C     HFLD          OREQ      'SCACDT'
     C                   CALL      'F0269R'
     C                   PARM      '2'           QQCALL
     C                   PARM                    QQSUB#            5 0
     C                   PARM                    QQFND             3 0
     C                   PARM                    QQFYR             3 0
     C                   PARM      0             QQCASE            7 0
     C                   PARM                    QQSS#             9
     C                   PARM                    QQACDT            8 0
     C                   PARM                    QQRTCD            1
     C     QQRTCD        IFEQ      '*'                                          RTCD = '*'
     C                   SETON                                        3490
     C                   MOVE      QQSS#         SCSS#             9
     C                   MOVE      QQACDT        SCACDT            8 0
     C                   MOVE      QQSUB#        SCSUB#
     C                   MOVE      QQFND         SCFND
     C                   ELSE                                                   RTCD = '*'
     C                   SETON                                        3290
     C                   ENDIF                                                  RTCD = '*'
     C                   LEAVE
     C                   ENDIF                                                  HFLD = SCSS#
     C*
     C*          Provider search
     C*          ------------------------------------------------------
     C     HFLD          IFEQ      'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
     C                   CALL      'F0032R'
     C                   PARM      '2'           QQCALL
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQTYPE            1
      *
     C     QQRTCD        IFEQ      'A'
     C                   SETON                                        LR
     C                   LEAVE
     C                   ENDIF                                                  RTCD = 'A'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   SETON                                        3790
     C                   MOVE      QQFEIN        SCFEIN
     C                   MOVE      QQMOD         SCMOD
     C                   LEAVE
     C                   ELSE                                                   RTCD = '*'
     C                   SETON                                        3490
     C                   ENDIF                                                  RTCD = '*'
     C                   LEAVE
     C                   ENDIF                                                  HLFD = SCFEIN
     C*
     C*          Search not available
     C*          ------------------------------------------------------
     C                   SETON                                        3590
     C                   LEAVE
     C                   ENDDO                                                  DO Loop
     C                   ENDIF                                                  *INKJ = On
     C                   ENDSR
     C*
     C*****************************************************************
     C*          SREDIT - EDIT
     C*****************************************************************
     C     SREDIT        BEGSR
     C                   DO
     C                   SETON                                        90
     C*
     C*          Validate Subscriber
     C*          ------------------------------------------------------
     C     SMKY          CHAIN     PSM01                              27        INVALID SUB#
     C     *IN27         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF                                                  *IN27 = On
     C*
     C*          Validate Fund number
     C*          ------------------------------------------------------
     C     FNKY          CHAIN     PFN01                              20        INVALID FUND
     C     *IN20         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF                                                  *IN20 = On
     C*
     C*          Validate SS#
     C*          ------------------------------------------------------
     C     SCSS#         IFEQ      *BLANK
     C                   SETON                                        21
     C                   LEAVE
     C                   ENDIF                                                  SS# = blank
     C*
     C*          Validate Accident
     C*          ------------------------------------------------------
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCACDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        22
     C                   LEAVE
     C                   ENDIF                                                  ERR = '1'
     C*
     C*          Validate Claim (does it exist?)
     C*          ------------------------------------------------------
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C*
     C*             Check for multiple cases under the same SS#/ACDT
     C*             ---------------------------------------------------
     C                   Z-ADD     0             CNTCAS            3 0
     C     CMKY1         CHAIN     PCM01                              24
     C     *IN24         DOWEQ     *OFF
     C                   ADD       1             CNTCAS
     C     CMKY1         READE     PCM01                                  24
     C                   ENDDO                                                  *IN24 = Off
     C     CNTCAS        IFEQ      0
     C                   LEAVE                                                  CNTCAS = 0
     C                   ELSE                                                   CNTCAS = 0
     C                   SETOFF                                       24
     C                   ENDIF                                                  CNTCAS = 0
     C*
     C*             More than 1 case:  display "Multi Case" Search
     C*             ---------------------------------------------------
     C     CNTCAS        IFGT      1
     C                   MOVE      SCSS#         QTSS#
     C                   MOVE      SCACDT        QTACDT
     C                   MOVE      *BLANKS       QTFND
     C                   MOVE      *BLANKS       QTFYR
     C                   MOVE      *BLANKS       QTCASE
     C                   CALL      'F0579R'
     C                   PARM                    QTPRDS           40
     C                   PARM                    QTCALL            1
     C                   PARM                    QTSS#             9
     C                   PARM                    QTACDT            8
     C                   PARM                    QTFND             3
     C                   PARM                    QTFYR             3
     C                   PARM                    QTCASE            7
     C                   PARM                    QTRTCD            1
     C*
     C                   WRITE     SCR99
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   WRITE     SCR01
     C     QTRTCD        IFEQ      'A'
     C     QTRTCD        OREQ      'B'
     C     QTFND         OREQ      *BLANKS
     C                   SETON                                        24
     C                   LEAVE
     C                   ENDIF                                                  RTCD = 'A'
     C*
     C     QTRTCD        IFEQ      '*'
     C                   MOVE      QTFND         SCFND
     C                   MOVE      QTFYR         XXFYR
     C                   MOVE      QTCASE        XXCASE
     C     CMKY2         CHAIN     PCM02                              24
     C     *IN24         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF                                                  *IN24 = On
     C                   ENDIF                                                  RTCD = '*'
     C                   ENDIF                                                  CNTCAS > 1
     C*
     C*          Validate Claim for the Co/Fnd setup
     C*          ------------------------------------------------------
     C     CMCO#         IFNE      UPCO#
     C     CMSUB#        ORNE      SCSUB#
     C     CMFND         ORNE      SCFND
     C                   SETON                                        23
     C                   LEAVE
     C                   ENDIF                                                  CO# <> CO#
     C*
     C*          Validate provider
     C*          ------------------------------------------------------
     C     SCFEIN        IFNE      0
     C                   MOVE      SCMOD         XXMOD
     C     PDKY          CHAIN     PPD01                              25
     C     *IN25         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF                                                  *IN25 = On
     C                   ENDIF                                                  FEIN = 0
     C*
     C*          Validate type
     C*          ------------------------------------------------------
     C     SCTYPE        IFNE      'A'
     C*** BEGIN DEL *** 10/07/03 **************************************
     C********** SCTYPE    ANDNE'C'
     C*** END   DEL *** 10/07/03 **************************************
     C     SCTYPE        ANDNE     'P'
     C     SCTYPE        ANDNE     'H'
     C                   SETON                                        28
     C                   LEAVE
     C                   END                                                    END SCTYPE
     C*
     C*          No errors detected
     C*          ------------------------------------------------------
     C                   SETOFF                                       90
     C                   ENDDO                                                  DO Loop
     C                   ENDSR
     C*
     C*****************************************************************
     C*          SRCALL - CALL
     C*****************************************************************
     C     SRCALL        BEGSR
     C                   MOVE      CMCO#         YYCO#
     C                   MOVE      CMSUB#        YYSUB#
     C                   MOVE      CMFND         YYFND
     C                   MOVE      CMFYR         YYFYR
     C                   MOVE      CMCASE        YYCASE
     C                   MOVE      CMSS#         YYSS#
     C                   MOVE      XXACDT        YYACDT
     C                   MOVE      SCFEIN        YYFEIN
     C                   MOVE      SCMOD         YYMOD
     C                   MOVE      SCTYPE        YYTYPE
     C*
     C*          Call F9950C to run the list
     C*          ------------------------------------------------------
     C                   CALL      'F9950C'
     C                   PARM                    YYCO#
     C                   PARM                    YYSUB#
     C                   PARM                    YYFND
     C                   PARM                    YYFYR
     C                   PARM                    YYCASE
     C                   PARM                    YYSS#
     C                   PARM                    YYACDT
     C                   PARM                    YYFEIN
     C                   PARM                    YYMOD
     C                   PARM                    YYTYPE
     C                   PARM                    UPOUTQ
     C*** BEGIN CHG *** 05/24/01 **************************************
     C**********           PARM 'D'       QQMODE  1
     C                   PARM      'D'           YYMODE            1
     C*** END   CHG *** 05/24/01 **************************************
     C                   MOVE      *BLANK        SCTYPE
     C                   WRITE     SCR01
     C                   SETON                                        LR
     C                   ENDSR
