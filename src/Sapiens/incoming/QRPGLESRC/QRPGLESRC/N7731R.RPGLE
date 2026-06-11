     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N7731R - SALES JOURNAL - PROMPT SCREEN PROGRAM
      *****************************************************************
      ** REVISIONS:                                                  **
     H**                                                             **
     H*  12/18/24 C:342 Project : MC447 Phase 2                      **
     H*                 1.F10 Search disabled on the Group Field     **
     H*                 DSPF Changes:                                **
     H*                 1.Changed Existing Group field into Typeahead**
     H*                                                              **
     H*  04/28/98 C:90  Project: MC181                               **
     H*             SCR01 - CHANGED TO MEET VERSION 4 STANDARDS      **
     H*             COMBINED ALL SEARCHES INTO F10 SEARCH            **
     H*                                                              **
     H*  12/04/97 C:86  Project: MC145
     H*             CHANGED CODE TO FIND OUT IF THE TO DATE WAS LESS
     H*             THAN THE FROM DATE AND REMOVED ALL CALLS TO SCREEN
     H*             99
     H*
      ** 07/05/95 C:35     - REQ/PROJ: NW008                         **
      **                     MOD TO ALLOW SYSTEM TYPE M              **
      *****************************************************************
      ** INDICATORS:                                                 **
      **  20          INVALID PRODUCT/SYSTEM CODE                    **
      **  21          INVALID SUBSCRIBER #                           **
      **  22          INVALID GROUP #                                **
      **  23          INVALID FROM DATE                              **
      **  24          INVALID TO DATE                                **
      **  25          DATES MUST BE IN ASCENDING ORDER               **
      **  26          INVALID REPORT TYPE                            **
      **  27          INVALID CASE                                   **
      **  28          POSITION CURSOR @ GROUP AFTER SUB SRCH         **
      **  29          POSITION CURSOR @ YR/CASE AFTER GROUP SRCH     **
      **  30          POSITION CURSOR @ FRDT AFTER CASE SRCH         **
      *****************************************************************
     FN7731F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
      **** BEGIN ADD **** 04/28/98 ************************************
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
      **** END   ADD **** 04/28/98 ************************************

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
     C*   KEY LIST FOR THE SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*
     C*  CASE MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C*
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
     C*   INITIALIZE WORK FIELDS
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
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
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
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
     C*****************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C**********************************
     C*   F3 = EXIT
     C**********************************
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END                                                    END *INKC
      **** BEGIN CHG **** 04/28/98 ************************************
      *
     C**********************************
     C*   F10 - SEARCH                 *
     C**********************************
     C     *INKJ         IFEQ      '1'
     C**********************************
     C*     PRODUCT SEARCH             *
     C**********************************
      *
     C     HFLD          IFEQ      'SCSYS'
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRNP            9 0
     C                   PARM      'SYS'         ##TBID           10
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         CABEQ     *ON           STRT                     31
     C     HFLD          IFEQ      *BLANKS
     C                   MOVE      *ON           *IN31
     C                   ENDIF                                                  IFEQ *BLANKS
      *
     C     ##RRNP        CHAIN     PGT02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCSYS
     C                   MOVE      *ON           *IN32
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   ENDIF                                                  HFLD SCSYS
      *
     C**********************************
     C**********   F6 - SUBSCRIBER SREACH       *
     C**********************************
     C********** *INKF     IFEQ '1'
     C******************** CALL 'F0545R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           SCSUB#
     C******************** PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C*
     C********** QQRTC     IFEQ 'B'
     C********** SCSUB#    OREQ *ZERO
     C****************     WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C******************** END                             END EQ B/0
     C*
     C********** QQRTC     IFEQ '*'
     C****************     WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** MOVE '1'       *IN28            POS CURSOR @ GRP
     C******************** GOTO SCR01T
     C******************** END                             END EQ *
     C*
     C******************** END                             END *INKF
      *
     C**********************************
     C*   SUBSCRIBER SEARCH            *
     C**********************************
      *
     C     HFLD          IFEQ      'SCSUB#'
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRNS            9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         CABEQ     *ON           STRT                     32
      *
     C     ##RRNS        CHAIN     PSM02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVE      *ON           *IN28
     C                   ELSE
     C                   MOVE      *ON           *IN32
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   ENDIF                                                  HFLD SCSUB#
     C**********************************
     C**********   COMMAND  7 - GROUP SEARCH    *
     C**********************************
     C********** *INKG     IFEQ '1'
     C******************** CALL 'F0508R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           SCSUB#
     C******************** PARM           SCFND
     C******************** PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C*
     C********** QQRTC     IFEQ 'B'
     C********** SCFND     OREQ *ZERO
     C*****************    WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C******************** END                             END EQ B/0
     C*
     C********** QQRTC     IFEQ '*'
     C*****************    WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** MOVE '1'       *IN29            POS CURSOR @ YR
     C******************** GOTO SCR01T
     C******************** END                             END EQ *
     C*
     C******************** END                             END *INKG
      *
     C**********************************
     C*        GROUP SEARCH            *
     C**********************************
      *
     C******** Beg Del ***** 12/18/24 ***************************
     C*****HFLD          IFEQ      'SCFND'
     C*****              MOVE      ##RRNS        ##RRNF
     C*****              CALL      'F0060R'
     C*****              PARM      '2'           ##CALL            1
     C******INKC         PARM                    ##KC              1
     C******INKL         PARM                    ##KL              1
     C*****              PARM                    ##RRNF            9 0
      *****
     C******INKC         CABEQ     *ON           TERM
      *****
     C******INKL         CABEQ     *ON           STRT                     28
      *****
     C*****##RRNF        CHAIN     PFN02                              60
     C******IN60         IFEQ      *OFF
     C*****              MOVE      FNFND         SCFND
     C*****              MOVE      *ON           *IN29
     C*****              ELSE
     C*****              MOVE      *ON           *IN28
     C*****              ENDIF                                                  IFEQ *IN60
      *****
     C*****              ENDIF                                                  HFLD SCFND
     C******** End Del ***** 12/18/24 ***************************
     C**********************************
     C***********   COMMAND  9 - CASE  SEARCH    *
     C**********************************
     C********** *INKI     IFEQ '1'
     C******************** CALL 'F0583R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           SCSUB#
     C******************** PARM           SCFND
     C******************** PARM           SCFYR
     C******************** PARM           SCCASE
     C******************** PARM           XXSS#   9
     C******************** PARM           QQRTCD  1
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C********** QQRTC     IFEQ 'B'
     C********** SCFYR     OREQ *ZERO
     C*****************    WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C******************** END                             END EQ B/0
     C*
     C********** QQRTC     IFEQ '*'
     C*****************    WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** Z-ADDSCFYR     XXFYR
     C******************** MOVE '1'       *IN30            POS CURSOR @ FRDT
     C******************** GOTO SCR01T
     C******************** END                             END EQ *
     C******************** END                             END *INKI
      *
     C**********************************
     C*   YEAR/CASE SEARCH             *
     C**********************************
      *
     C     HFLD          IFEQ      'SCFYR'
     C     HFLD          OREQ      'SCCASE'
     C                   MOVE      SCSUB#        ##SUB#
     C                   MOVE      SCFND         ##FND
     C                   CALL      'F0269R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FND             3 0
     C                   PARM      0             ##FYR             3 0
     C                   PARM      0             ##CASE            7 0
     C                   PARM                    ##SS#             9
     C                   PARM                    ##ACDT            8 0
     C                   PARM                    ##RTCD            1
      *
     C     ##FYR         IFEQ      *ZEROS
     C     HFLD          CABEQ     'SCFYR'       STRT                     29    PC AT SCFYR
     C                   ENDIF                                                  IFEQ QQFYR
     C     ##CASE        IFEQ      *ZEROS
     C     HFLD          CABEQ     'SCCASE'      STRT                     33    PC AT SCCASE
     C                   ENDIF                                                  IFEQ QQCASE
      *
     C     ##RTCD        IFEQ      '*'
     C                   MOVE      *ON           *IN30
     C                   MOVE      ##SUB#        SCSUB#
     C                   MOVE      ##FND         SCFND
     C                   MOVE      ##FYR         SCFYR
     C                   MOVE      ##CASE        SCCASE
     C                   ENDIF                                                  IFEQ '*'
      *
     C                   ENDIF                                                  HFLD SCFYR
      *
     C*                    MOVE *ON       *IN90
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  IFEQ INKJ
     C**********************************
     C************   COMMAND 13 - CHG USER PARMS  *
     C**********************************
     C********** *INKM     IFEQ '1'
     C*
     C******************** CALL 'F0006R'
     C*****************    WRITESCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C******************** MOVE *ZERO     UPCO#
     C******************** MOVE *ZERO     UPSUB#
     C******************** END
     C******************** GOTO STRT01
     C******************** END                             END *INKM
      **** END   CHG **** 04/28/98 ************************************
     C*
     C                   END                                                    END CMDKEYS
     C*****************************************************************
     C*   PROCESS REQUEST SCREEN
     C*****************************
     C     SCR01T        TAG
     C*****************************
     C* VALIDATE SELECTIONS:      *
     C*****************************
     C*
     C*   IF ENTERED, PRODUCT/SYSTEM MUST BE VALID
     C*
     C     SCSYS         IFNE      *BLANKS
     C     SCSYS         IFNE      'P'                                          PBR
     C     SCSYS         ANDNE     'H'                                          HBA
     C     SCSYS         ANDNE     'N'                                          NET
     C     SCSYS         ANDNE     'U'                                          NET
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C     SCSYS         ANDNE     'M'                                          MED MGMT
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C     SCSYS         ANDNE     'A'                                          OTHER
     C                   MOVE      '1'           *IN20                          INVALID SYSTEM
     C                   GOTO      STRT
     C                   END                                                    ENDNE P/H/N/A
     C                   END                                                    ENDNE *BLANKS
     C*
     C*   IF ENTERED, SUBSCRIBER MUST BE VALID
     C*
     C     SCSUB#        IFNE      *ZEROS
     C     SMKEY         CHAIN     PSM01                              21        INVALID SUBSCRIBER
     C     *IN21         CABEQ     '1'           STRT
     C                   END                                                    ENDNE *ZEROS
     C*
     C*   IF ENTERED, GROUP MUST BE VALID
     C*
     C     SCFND         IFNE      *ZEROS
     C     FNKEY         CHAIN     PFN01                              22        INVALID GROUP
     C     *IN22         CABEQ     '1'           STRT
     C                   END                                                    ENDNE *ZEROS
     C*
     C*   IF ENTERED, CASE MUST BE VALID
     C*
     C     SCCASE        IFNE      *ZEROS
     C                   MOVE      SCFYR         XXFYR
     C     CMKEY         CHAIN     PCM01                              27        INVALID CASE
     C     *IN27         CABEQ     '1'           STRT
     C                   END                                                    ENDNE *ZEROS
     C*
     C*   IF ENTERED, VALIDATE DATES & ASCENDING ORDER
     C*
     C     SCFRDT        IFNE      *ZEROS
     C     SCTODT        ORNE      *ZEROS
     C*
     C*   IF ENTERED, FROM DATE MUST BE VALID
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     23    INVALID FROM DATE
     C*
     C*   IF ENTERED, TO DATE MUST BE VALID
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         CABEQ     '1'           STRT                     24    INVALID TO DATE
     C*
     C*   IF ENTERED, DATES MUST BE IN ASCENDING ORDER
     C*
     C*********** BEGIN CHANGE *************** 12/04/97 ***************
     C**************       MOVE SCFRDT    SRFRDC  6
     C**************       MOVE SCTODT    SRTODC  6
     C**************
     C**************       CALL 'SRDDIF'
     C**************       PARM           SRFRDC
     C**************       PARM           SRTODC
     C**************       PARM           SRDAYS  50
     C**************       PARM ' '       SRERR   1
     C**************       PARM ' '       SRTERM  1
     C**************
     C********** SRERR     CABEQ'1'       STRT           25INVALID ORDER
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SRFRDC            7 0
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SRTODC            7 0
     C     SRFRDC        IFGT      SRTODC
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   ENDIF
     C*********** END   CHANGE *************** 12/04/97 ***************
     C                   END                                                    ENDNE *ZEROS
     C*
     C*   REPORT TYPE MUST BE D, S OR B
     C*
     C     SCRTYP        IFNE      'D'                                          DETAIL
     C     SCRTYP        ANDNE     'S'                                          SUMMARY
     C     SCRTYP        ANDNE     'B'                                          BOTH
     C                   MOVE      '1'           *IN26                          INVALID REPORT TYPE
     C                   GOTO      STRT
     C                   END                                                    ENDNE D/S/B
     C*
     C****************************************
     C*   CALL THE CLP TO SUBMIT THE LIST.   *
     C****************************************
     C                   MOVE      SCSYS         QQSYS             1
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCSUB#        QQSUB#            5
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCCASE        QQCASE            7
     C******* BEGIN CHANGE ******** 12/04/97 **************************
     C****************     MOVE SCFRDT    QQFRDT  6
     C****************     MOVE SCTODT    QQTODT  6
     C                   MOVE      SCFRDT        QQFRDT            8
     C                   MOVE      SCTODT        QQTODT            8
     C******* END   CHANGE ******** 12/04/97 **************************
     C                   MOVE      SCRTYP        QQRTYP            1
     C                   CALL      'N7731C'
     C                   PARM                    QQSYS
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    QQRTYP
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C*****************************
     C*  END THE PROGRAM          *
     C*****************************
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
