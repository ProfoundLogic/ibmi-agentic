     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE S0020R - Client Control File Maintenance
     H*****************************************************************
     H** DESCRIPTION:                                                 *
     H**   This pgm allows the user to enter control file information *
     H**   for a WCMS Fund or WCMC Subscriber and Group.              *
     H**   The field usage is defined using file SXHPF (Client Control*
     H**   Heading File).                                             *
     H**                                                              *
     H** CALLED FROM:                                                 *
     H**   Menu                                                       *
     H**                                                              *
     H** CALLS TO:                                                    *
     H**   W0510R - WCMS Fund Search                                  *
     H**   F0545R - Subscriber Search                                 *
     H**   F0508R - Group Search                                      *
     H**   F0006R - WCMC User Parms                                   *
     H**                                                              *
     H** CREATED:  03/04/96   C:30   Project:  SY009                  *
     H**                                                              *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/15/03 C:53  project: WC577                                 *
     H*                Add current searches to pgm.                   *
     H*                                                               *
     H* 02/19/03 C:53  SUPPORT 129WAT                                 *
     H*                Add screen 6 for new fields.                   *
     H*                                                               *
     H* 04/12/01 C:61  SUPPORT WC099 27110                            *
     H*                Correct so does not CHAIN to wrong record      *
     H*                when WCMS only installed.                      *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS USED:                                              *
     H*                                                               *
     H*    21 - 40  Edit Field Indicators                             *
     H*    50       SXCPF record found/not found                      *
     H*    51  *ON  Access SXCPF using SXCPL                          *
     H*    51  *OFF Access SXCPF using SXCPL1                         *
     H*    52       LOKUP table SCD (compile time array)              *
     H*             Valid Security Type Codes                         *
     H*    53  *ON  WCMS installed                                    *
     H*    54  *ON  WCMC installed                                    *
     H*                                                               *
     H*****************************************************************
     H** FILE USAGE:                                                  *
     H**                                                              *
     H**    SMCOL  (PCO01): CO#                                       *
     H**    SMUPL  (PUP01): USR                                       *
     H**    WMUPL  (PUP02): USR                                       *
     H**    SXHPL  (SHP01): CO#                                       *
     H**    SXCPL  (SCP01): CO#/FND                                   *
     H**    SXCPL1 (SCP02): CO#/SUB#/GRP                              *
     H**    FMFNL  (PFN02): CO#/SUB#/GRP                              *
     H**    FMFNL1 (PFN03): CO#/FND                                   *
     H**    WMFNL  (PFN01): CO#/FND                                   *
     H**    FMSML  (PSM01): CO#/SUB#                                  *
     H**    STCTL1 (PCT01): RMCD                                      *
     H**    SSYLF  (SSY01): SYKEY                                     *
     H**                                                              *
     H*****************************************************************
     FS0020F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameter File
     FSMUPL     IF   E           K DISK    USROPN
     FWMUPL     IF   E           K DISK    USROPN
     F                                     RENAME(PUP01:PUP02)
     F*
     F* Client Control Heading File
     FSXHPL     IF   E           K DISK
     F*
     F* Client Control File (WCMS Fund)
     FSXCPL     UF A E           K DISK
     F*
     F* Client Control File (WCMC Subscriber/Group)
     FSXCPL1    UF A E           K DISK
     F                                     RENAME(SCP01:SCP02)
     F*
     F* WCMC Group Master File
     FFMFNL     IF   E           K DISK    USROPN
     F                                     RENAME(PFN01:PFN02)
     FFMFNL1    IF   E           K DISK    USROPN
     F                                     RENAME(PFN01:PFN03)
      **** BEG ADD *** 04/15/03 ***************************
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN04)
      **** END ADD *** 04/15/03 ***************************
     F*
     F* WCMS Fund Master File
     FWMFNL     IF   E           K DISK    USROPN
     F*
     F* WCMC Subscriber File
     FFMSML     IF   E           K DISK    USROPN
      **** BEG ADD *** 04/15/03 ***************************
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
      **** END ADD *** 04/15/03 ***************************
     F*
     F* DARTS Remark Code File
     FSTCTL1    IF   E           K DISK
     F*
     F* Application System Control File
     FSSYLF     IF   E           K DISK
      *****************************************************************

     D SCD             S              1    DIM(36) CTDATA PERRCD(36) ASCEND     Security Codes
     D DT              S              8  0 DIM(10)                              Dates
      *
     D                 DS
     D  STY                    1     20
     D                                     DIM(20)                              Security Type
     D  SCST01                 1      1
     D  SCST02                 2      2
     D  SCST03                 3      3
     D  SCST04                 4      4
     D  SCST05                 5      5
     D  SCST06                 6      6
     D  SCST07                 7      7
     D  SCST08                 8      8
     D  SCST09                 9      9
     D  SCST10                10     10
     D  SCST11                11     11
     D  SCST12                12     12
     D  SCST13                13     13
     D  SCST14                14     14
     D  SCST15                15     15
     D  SCST16                16     16
     D  SCST17                17     17
     D  SCST18                18     18
     D  SCST19                19     19
     D  SCST20                20     20
      *
     D                 DS
     D  REM                    1    100
     D                                     DIM(20)                              DARTS Remark CD
     D  SCRM01                 1      5
     D  SCRM02                 6     10
     D  SCRM03                11     15
     D  SCRM04                16     20
     D  SCRM05                21     25
     D  SCRM06                26     30
     D  SCRM07                31     35
     D  SCRM08                36     40
     D  SCRM09                41     45
     D  SCRM10                46     50
     D  SCRM11                51     55
     D  SCRM12                56     60
     D  SCRM13                61     65
     D  SCRM14                66     70
     D  SCRM15                71     75
     D  SCRM16                76     80
     D  SCRM17                81     85
     D  SCRM18                86     90
     D  SCRM19                91     95
     D  SCRM20                96    100
      *
     D                 DS
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      *
      *  Work Field for Field Cursor Position (DARTS Remark Type)
     D                 DS
     D  WRKCSR                 1     10
     D  WRKPOS                 5      6  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     *LIKE         DEFINE    Q1USER        XXUSER
     C     *LIKE         DEFINE    XCCO#         XXCO#
     C     *LIKE         DEFINE    XCFND         XXFND
     C     *LIKE         DEFINE    XCSUB#        XXSUB#
     C     *LIKE         DEFINE    XCGRP         XXGRP
     C     *LIKE         DEFINE    CTRMCD        XXRMCD
     C     *LIKE         DEFINE    SYKEY         XXSYKY
      *
      *****************************************************************
      *
      *   KEY LIST FOR THE COMPANY FILE
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      *  KEY LIST FOR USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    XXUSER
      *
      *  Key list for Client Control File
      *
     C     CPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C     CPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXGRP
      *
      *  Key list for Subscriber File
      *
     C     SMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
      *
      *  Key list for WCMS Fund File
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  Key list for WCMC Group File
      *
     C     FNKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXGRP
      *
      *  Key list for DARTS Remark Code File
      *
     C     CTKY          KLIST
     C                   KFLD                    XXRMCD
      *
      *  Key list for Application System Control File
      *
     C     SYKY          KLIST
     C                   KFLD                    XXSYKY
      *
      *****************************************************************
      *
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
      *
      *****************************************************************
      *
      *  Get the Application System Control File SSY01 record
      *
     C                   MOVEL     'SY'          XXSYKY
     C     SYKY          CHAIN     SSY01                              60
     C     *IN60         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     SYWCIF        IFEQ      'Y'
      *                    W C M S     i n s t a l l e d
     C                   OPEN      WMFNL
     C                   OPEN      WMUPL
     C                   MOVE      *ON           *IN53
     C                   ENDIF
      *
     C     SYFBIF        IFEQ      'Y'
      *                    W C M C     i n s t a l l e d
     C                   OPEN      FMFNL
     C                   OPEN      FMFNL1
     C                   OPEN      FMSML
     C                   MOVE      *ON           *IN54
     C                   ENDIF
      *
     C     *IN53         IFEQ      *OFF
      *                    W C M S     N O T    i n s t a l l e d
      *                    W C M C     i n s t a l l e d
     C                   OPEN      SMUPL
     C                   ENDIF
      *
      *  Get the User Parameter File
      *
     C                   MOVEL     Q1USER        XXUSER
      *
     C     *IN53         IFEQ      *ON
     C     UPKY          CHAIN     PUP02                              60
     C                   ELSE
     C     UPKY          CHAIN     PUP01                              60
     C                   ENDIF
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      UPCO#         XXCO#
     C                   ELSE
     C                   MOVE      *ZERO         XXCO#
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C     STRT01        TAG
      *
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      'N'           SCFLG1
      *
      *  Get the Company record
      *
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   Z-ADD     XXCO#         SCCO#
      *
      *   Load the record format info for the First Time
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
      *   Display ALL Formats
      *
     C     STRT          TAG
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
      *
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   ENDIF
      *
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   ENDIF
      *
     C     Q2RECF        IFEQ      'SCR04'
     C                   EXFMT     SCR04
     C                   ENDIF
      *
     C     Q2RECF        IFEQ      'SCR05'
     C                   EXFMT     SCR05
     C                   ENDIF
      *
      *** BEG ADD *** 02/19/03 ****************************
      *
     C     Q2RECF        IFEQ      'SCR06'
     C                   EXFMT     SCR06
     C                   ENDIF
      *** END ADD *** 02/19/03 ****************************
      *
      *   Process ALL Function Keys
      *
     C     *IN11         IFEQ      '1'
      *
      **********************************
      *   F3 = Exit                    *
      **********************************
      *
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC IFEQ '1'
      *
      **********************************
      *   F6 = Fund Search             *
      **********************************
      *
     C     *INKF         IFEQ      '1'
      *
     C                   SELECT
      *
     C     Q2RECF        WHENEQ    'SCR01'
      *
      **** BEG ADD *** 04/15/03 ***************************
      * F u n d     S e a r c h
      *
     C                   MOVEL     XXCO#         @@CO#
     C                   CALL      'W0010R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM      1             @@VIEW            1 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C                   WRITE     SCR01
     C*
     C     *INKL         IFEQ      *OFF
     C     @@FND         ANDNE     *ZERO
     C                   MOVE      @@FND         SCFND
     C*
     C                   ENDIF                                                  *INKL IFEQ
     C*
     C                   GOTO      STRT
      *
      **** END ADD *** 04/15/03 ***************************
      **** BEG DEL *** 04/15/03 ***************************
      **********           F u n d     S e a r c h
     C**********           CALL 'W0510R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCFND
     C**********           PARM           QQRTC   1
      **********
     C********** QQRTC     CABEQ'A'       TERM
      **********
     C**********           WRITESCR01
      **********
     C********** QQRTC     IFEQ 'B'
     C********** SCFND     OREQ *ZERO
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           ENDIF
      **********
     C********** QQRTC     IFEQ '*'
     C**********           GOTO SCR01T
     C**********           ENDIF
      **** END DEL *** 04/15/03 ***************************
      *
     C     Q2RECF        WHENEQ    'SCR03'
      *                    D A R T S    R e m a r k    S e a r c h
     C                   MOVE      HFLD          WRKCSR
     C                   Z-ADD     WRKPOS        X                 3 0
     C                   MOVE      REM(X)        QQRMCD
     C                   CALL      'SC580R'
     C                   PARM                    QQRMCD            5
     C                   PARM                    QQRTC             1
      *
     C     QQRTC         CABEQ     'A'           TERM
      *
     C                   WRITE     SCR01
      *
     C     QQRTC         IFEQ      'B'
     C     QQRMCD        OREQ      *BLANK
     C     70            ADD       X             Y1                2 0
     C                   MOVE      *ON           *IN(Y1)                        DSPATR(PC)
     C                   MOVE      *BLANK        REM(X)
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     QQRTC         IFEQ      '*'
     C                   MOVE      QQRMCD        REM(X)
     C     70            ADD       X             Y1                2 0
     C                   MOVE      *ON           *IN(Y1)                        DSPATR(PC)
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ENDSL
      *
     C                   ENDIF
      *
      **********************************
      *   F7 = Subscriber # Search     *
      **********************************
      *
     C     *INKG         IFEQ      '1'
      *
      **** BEG ADD *** 04/15/03 ***************************
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         CABEQ     *ON           STRT                     30
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PSM02                              99
     C*
     C                   Z-ADD     SMSUB#        SCSUB#
     C*
     C                   ELSE
     C*
     C*
     C                   ENDIF                                                  *IN99 IFEQ
     C*
     C                   GOTO      STRT
     C*
      **** END ADD *** 04/15/03 ***************************
      **** BEG DEL *** 04/15/03 ***************************
     C**********           CALL 'F0545R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCSUB#
     C**********           PARM           QQRTC   1
      **********
     C********** QQRTC     CABEQ'A'       TERM
      **********
     C**********           WRITESCR01
      **********
     C********** QQRTC     IFEQ 'B'
     C********** SCSUB#    OREQ *ZERO
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           ENDIF                           END EQUAL B
      **********
     C********** QQRTC     IFEQ '*'
     C**********           GOTO STRT
     C**********           ENDIF
      **** END DEL *** 04/15/03 ***************************
      *
     C                   ENDIF                                                  END *INKG
      *
      *********************************
      *   F8 = Group Search           *
      *********************************
      *
     C     *INKH         IFEQ      '1'
      *
      **** BEG ADD *** 04/15/03 ***************************
     C*
     C                   CALL      'F0060R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     31
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PFN04                              99
     C*
     C                   Z-ADD     FNFND         SCGRP
     C**********           MOVELFNNAME    SCGPNM    P
     C**********           SETON                     32
     C*
     C                   ELSE
     C*
     C**********           SETON                     31
     C*
     C                   ENDIF                                                  *IN99 IFEQ
     C*
     C                   GOTO      STRT
      **** END ADD *** 04/15/03 ***************************
      **** BEG DEL *** 04/15/03 ***************************
     C********** SCSUB#    IFEQ *ZEROS
     C**********           MOVE UPSUB#    XXSUB#
     C**********           MOVE UPSUB#    SCSUB#
     C**********           ELSE
     C**********           MOVE SCSUB#    QQSUB#
     C**********           ENDIF
      **********
     C**********           CALL 'F0508R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           XXSUB#
     C**********           PARM           SCGRP
     C**********           PARM           QQRTC   1
      **********
     C********** QQRTC     CABEQ'A'       TERM
      **********
     C**********           WRITESCR01
      **********
     C********** QQRTC     IFEQ 'B'
     C********** SCGRP     OREQ *ZERO
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           ENDIF
      **********
     C********** QQRTC     IFEQ '*'
     C**********           MOVE XXSUB#    SCSUB#
     C**********           GOTO SCR01T
     C**********           ENDIF
      **********
      **** END DEL *** 04/15/03 ***************************
      *
     C                   ENDIF
      *
      **********************************
      *   F9 = Subscriber # Search     *
      **********************************
      *
     C     *INKI         IFEQ      '1'
      *
     C                   MOVE      SCCO#         QQCO#
     C                   MOVE      *ZERO         QQFND
     C                   MOVE      *ZERO         QQSUB#
     C                   MOVE      *ZERO         QQGRP
     C                   CALL      'S0520R'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQGRP             3
     C                   PARM                    QQRTC             1
      *
     C     QQRTC         CABEQ     'A'           TERM
      *
     C                   WRITE     SCR01
      *
     C     QQRTC         IFEQ      'B'
     C     QQCO#         OREQ      *ZERO
     C                   Z-ADD     *ZERO         SCFND
     C                   Z-ADD     *ZERO         SCSUB#
     C                   Z-ADD     *ZERO         SCGRP
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  END EQUAL B
      *
     C     QQRTC         IFEQ      '*'
     C                   SELECT
      *
     C     QQFND         WHENEQ    *ZERO
     C     QQSUB#        ANDEQ     *ZERO
     C     QQGRP         ANDEQ     *ZERO
      *                    Default Company record selected
     C                   MOVE      'Y'           SCFLG1
      *
     C     *IN53         WHENEQ    *ON
      *                    WCMS Installed
     C                   MOVE      QQFND         SCFND
     C                   MOVE      QQFND         XXFND
     C                   Z-ADD     *ZERO         SCSUB#
     C                   Z-ADD     *ZERO         SCGRP
      *
     C     *IN53         WHENEQ    *OFF
      *                    WCMC Installed
     C                   MOVE      QQSUB#        SCSUB#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQGRP         SCGRP
     C                   MOVE      QQGRP         XXGRP
     C                   Z-ADD     *ZERO         SCFND
     C                   ENDSL
      *
     C                   GOTO      SCR01T
     C                   ENDIF
      *
     C                   ENDIF                                                  END *INKI
      *
      *   Process HELP Requests
      *
     C     *IN10         IFEQ      '1'
      *
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
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN10 IFEQ '1'
      *
      ***********************************
      *  F12 = Previous                 *
      ***********************************
      *
     C     *INKL         IFEQ      '1'
      *
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR02'
     C                   MOVE      *BLANK        SCFNAM
     C                   MOVE      *BLANK        SCSNAM
     C                   MOVE      *BLANK        SCGNAM
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     Q2RECF        WHENEQ    'SCR03'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C     Q2RECF        WHENEQ    'SCR04'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C     Q2RECF        WHENEQ    'SCR05'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   ENDSL
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL
      *
      ***********************************
      *  F13 = Change User Parameters   *
      ***********************************
      *
     C     *INKM         IFEQ      '1'
      *
     C     *IN53         IFEQ      *ON
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C     UPKY          CHAIN     PUP02                              60
      *
     C                   ELSE
      *
     C                   CALL      'F0006R'
     C                   WRITE     SCR99
     C     UPKY          CHAIN     PUP01                              60
     C                   ENDIF
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      UPCO#         XXCO#
     C                   ELSE
     C                   MOVE      *ZERO         XXCO#
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   GOTO      STRT01
     C                   ENDIF                                                  *INKM IFEQ '1'
      *
     C                   ENDIF                                                  *IN11 IFEQ '1'
      *
      *   GOTO the Appropriate Screen
      *
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C     Q2RECF        CABEQ     'SCR04'       SCR04T
     C     Q2RECF        CABEQ     'SCR05'       SCR05T
     C     Q2RECF        CABEQ     'SCR06'       SCR06T                         ADD 02/19/03
      *
      *********************************
      *   Process Request Screen 01   *
      *********************************
      *
     C     SCR01T        TAG
      *
      *   Verify Default Company Record Flag
      *
     C     SCFLG1        IFNE      'Y'
     C     SCFLG1        ANDNE     'N'
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
     C                   ENDIF
      *
      *   Verify Valid Data Entry based on specific Scenario
      *
     C                   SELECT
      *
      *   Scenario # 1
      *
     C     SCFLG1        WHENEQ    'Y'
      *                    D e f a u l t    C O #    R e c o r d
     C                   Z-ADD     *ZERO         SCFND
     C                   Z-ADD     *ZERO         SCSUB#
     C                   Z-ADD     *ZERO         SCGRP
     C                   Z-ADD     *ZERO         XXFND
     C                   Z-ADD     *ZERO         XXSUB#
     C                   Z-ADD     *ZERO         XXGRP
      *
      *   Scenario # 2
      *
     C     *IN53         WHENEQ    *ON
     C     *IN54         ANDEQ     *ON
      *                    W C M S   &   W C M C    I n s t a l l e d
      *
     C     SCFND         IFEQ      *ZERO
     C     SCSUB#        ANDEQ     *ZERO
     C     SCGRP         ANDEQ     *ZERO
     C                   MOVE      *ON           *IN20
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     SCFND         IFGT      *ZERO
     C     SCSUB#        IFGT      *ZERO
     C     SCGRP         ORGT      *ZERO
     C                   MOVE      *ON           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCSUB#        IFGT      *ZERO
     C     SCGRP         ORGT      *ZERO
     C     SCFND         IFGT      *ZERO
     C                   MOVE      *ON           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
      *   If Fund entered, verify valid WCMS Fund
      *
     C                   MOVE      *OFF          *IN51
     C                   MOVE      *BLANK        SCFNAM
     C                   MOVE      *BLANK        SCSNAM
     C                   MOVE      *BLANK        SCGNAM
      *
     C     SCFND         IFGT      *ZERO
     C                   MOVE      *ON           *IN51
     C                   Z-ADD     SCFND         XXFND
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF
     C                   MOVEL     FNNAME        SCFNAM
     C                   ENDIF
      *
      *   If Subscriber entered, verify valid WCMC Subscriber.
      *   Then verify valid Group #.
      *
     C     SCSUB#        IFGT      *ZERO
     C     SCGRP         ORGT      *ZERO
      *   Subscriber
     C                   Z-ADD     SCSUB#        XXSUB#
     C     SMKY          CHAIN     PSM01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF
     C                   MOVEL     SMNAME        SCSNAM
      *   Group
     C                   Z-ADD     SCGRP         XXGRP
     C     FNKY1         CHAIN     PFN02                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   ENDIF
     C                   MOVEL     FNNAME        SCGNAM
     C                   ENDIF
      *
      *   Passed WCMS & WCMC Edits
      *
     C     *IN51         IFEQ      *ON
      *  Access WCMC Group Master using WCMS Fund #
      *  to obtain Subscriber #, Group #, and Group Name
     C     FNKY          CHAIN     PFN03                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNNAME        SCGNAM
     C                   Z-ADD     FNSUB#        SCSUB#
     C                   Z-ADD     FNFND         SCGRP
      *  Access WCMC Subscriber Master to obtain Subscriber Name
     C                   Z-ADD     FNSUB#        XXSUB#
     C     SMKY          CHAIN     PSM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     SMNAME        SCSNAM
     C                   ENDIF
     C                   ENDIF
      *
     C                   ELSE
      *
      *  Access WCMS Fund Master using WCMC Fund # (cross reference)
      *  to obtain WCMS Fund Name
     C                   Z-ADD     FNWCF#        XXFND
     C                   Z-ADD     FNWCF#        SCFND
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNNAME        SCFNAM
     C                   ENDIF
     C                   ENDIF
      *
      *   Scenario # 3
      *
     C     *IN53         WHENEQ    *ON
     C     *IN54         ANDEQ     *OFF
      *                    W C M S    O n l y    I n s t a l l e d
      *
     C     SCFND         IFLE      *ZERO
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   Z-ADD     SCFND         XXFND
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   MOVEL     FNNAME        SCFNAM
      ***** BEG ADD ******* 04/12/01 **********************************
      *
     C     XXFND         IFGT      *ZERO
     C                   SETON                                        51
      *
     C                   ELSE
      *
     C                   SETOFF                                       51
     C                   ENDIF                                                  XXFND > 0
      *
      ***** END ADD ******* 04/12/01 **********************************
      *
      *   Scenario # 4
      *
     C     *IN53         WHENEQ    *OFF
     C     *IN54         ANDEQ     *ON
      *                    W C M C    I n s t a l l e d
      *
     C     SCSUB#        IFLE      *ZERO
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     SCGRP         IFLE      *ZERO
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF
      *   Subscriber
     C                   Z-ADD     SCSUB#        XXSUB#
     C     SMKY          CHAIN     PSM01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   MOVEL     SMNAME        SCSNAM
      *   Group
     C                   Z-ADD     SCGRP         XXGRP
     C     FNKY1         CHAIN     PFN02                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   MOVEL     FNNAME        SCGNAM
      *
     C                   ENDSL
      *
      *   Access Client Control Heading file
      *
     C     XXCO#         CHAIN     SHP01                              60
      *
      *   Access Client Control file
      *
     C     *IN51         IFEQ      *ON
     C     CPKY          CHAIN     SCP01                              50        Fund
     C                   ELSE
     C     CPKY1         CHAIN     SCP02                              50        Sub#/Group
     C                   ENDIF
      *
     C     *IN50         IFEQ      *ON
     C                   EXSR      CPINIT                                       Rec Not Found
     C                   ELSE
     C                   EXSR      CPASGN                                       Rec Found
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
      *
      *********************************
      *   Process Request Screen 02   *
      *********************************
      *
     C     SCR02T        TAG
      *
      *  Edit Security Type Code, if Entered
      *
     C                   Z-ADD     20            X                 3 0
     C                   Z-ADD     1             I                 3 0
      *
     C                   DO        20            I
      *
     C     STY(I)        IFNE      *BLANK
     C                   Z-ADD     1             N                 3 0
     C     STY(I)        LOOKUP    SCD(N)                                 52
     C     *IN52         IFEQ      *OFF
     C     X             ADD       I             Y                 3 0
     C                   MOVE      *ON           *IN(Y)
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDDO
      *
      *  Passed Edits
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
      *
      *********************************
      *   Process Request Screen 03   *
      *********************************
      *
     C     SCR03T        TAG
      *
      *  Edit DARTS Remark Code, if Entered
      *
     C                   Z-ADD     20            X                 3 0
     C                   Z-ADD     1             I                 3 0
      *
     C                   DO        20            I
      *
     C     REM(I)        IFNE      *BLANK
     C                   MOVEL     REM(I)        XXRMCD
     C     CTKY          CHAIN     PCT01                              52
     C     *IN52         IFEQ      *ON
     C     X             ADD       I             Y                 3 0
     C                   MOVE      *ON           *IN(Y)
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDDO
      *
      *  Passed Edits
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
      *
      *********************************
      *   Process Request Screen 04   *
      *********************************
      *
     C     SCR04T        TAG
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C                   GOTO      STRT
      *
      *********************************
      *   Process Request Screen 05   *
      *********************************
      *
     C     SCR05T        TAG
      *
      *   Edit Date fields
      *
     C                   Z-ADD     20            X                 3 0
     C                   Z-ADD     1             I                 3 0
      *
     C                   DO        10            I
      *
     C     DT(I)         IFGT      *ZERO
     C                   Z-ADD     DT(I)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         DT(I)
     C     SRERR         IFEQ      '1'
     C     X             ADD       I             Y                 3 0
     C                   MOVE      *ON           *IN(Y)
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDDO
      *
      *** BEG ADD *** 02/19/03 ****************************
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR06'       Q2RECF
     C                   GOTO      STRT
      *
      *********************************
      *   Process Request Screen 06   *
      *********************************
      *
     C     SCR06T        TAG
      *
      *** END ADD *** 02/19/03 ****************************
      *
      *   Update the Client Control file
      *
     C                   EXSR      CPUPDT
      *
     C     *IN50         IFEQ      *OFF
      *     U p d a t e    C l i e n t    C o n t r o l    F i l e
     C   51              UPDATE    SCP01                                        Fund
     C  N51              UPDATE    SCP02                                        Sub#/Group
     C                   ELSE
      *     W r i t e      C l i e n t    C o n t r o l    F i l e
     C   51              WRITE     SCP01                                        Fund
     C  N51              WRITE     SCP02                                        Sub#/Group
     C                   ENDIF
      *
      *  Initialize Screen 01 data entry fields and Re-start
      *
     C                   Z-ADD     *ZERO         SCFND
     C                   Z-ADD     *ZERO         SCSUB#
     C                   Z-ADD     *ZERO         SCGRP
     C                   MOVE      *BLANK        SCFNAM
     C                   MOVE      *BLANK        SCSNAM
     C                   MOVE      *BLANK        SCGNAM
     C                   MOVE      'N'           SCFLG1
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
      *********************
      *  End the Program  *
      *********************
      *
     C     TERM          TAG
      *
     C     *IN53         IFEQ      *ON
     C                   CLOSE     WMFNL
     C                   CLOSE     WMUPL
     C                   ENDIF
      *
     C     *IN54         IFEQ      *ON
     C                   CLOSE     FMFNL
     C                   CLOSE     FMFNL1
     C                   CLOSE     FMSML
     C                   ENDIF
      *
     C     *IN53         IFEQ      *OFF
     C                   CLOSE     SMUPL
     C                   ENDIF
      *
     C                   MOVE      '1'           *INLR
      *
      *****************************************************************
      *  CPINIT - Initialize all Client Control Screen Fields
      *****************************************************************
     C     CPINIT        BEGSR
      *
     C                   MOVE      *BLANK        SCSL01
     C                   MOVE      *BLANK        SCSL02
     C                   MOVE      *BLANK        SCSL03
     C                   MOVE      *BLANK        SCSL04
     C                   MOVE      *BLANK        SCSL05
     C                   MOVE      *BLANK        SCSL06
     C                   MOVE      *BLANK        SCSL07
     C                   MOVE      *BLANK        SCSL08
     C                   MOVE      *BLANK        SCSL09
     C                   MOVE      *BLANK        SCSL10
     C                   MOVE      *BLANK        SCSL11
     C                   MOVE      *BLANK        SCSL12
     C                   MOVE      *BLANK        SCSL13
     C                   MOVE      *BLANK        SCSL14
     C                   MOVE      *BLANK        SCSL15
     C                   MOVE      *BLANK        SCSL16
     C                   MOVE      *BLANK        SCSL17
     C                   MOVE      *BLANK        SCSL18
     C                   MOVE      *BLANK        SCSL19
     C                   MOVE      *BLANK        SCSL20
     C                   MOVE      *BLANK        SCST01
     C                   MOVE      *BLANK        SCST02
     C                   MOVE      *BLANK        SCST03
     C                   MOVE      *BLANK        SCST04
     C                   MOVE      *BLANK        SCST05
     C                   MOVE      *BLANK        SCST06
     C                   MOVE      *BLANK        SCST07
     C                   MOVE      *BLANK        SCST08
     C                   MOVE      *BLANK        SCST09
     C                   MOVE      *BLANK        SCST10
     C                   MOVE      *BLANK        SCST11
     C                   MOVE      *BLANK        SCST12
     C                   MOVE      *BLANK        SCST13
     C                   MOVE      *BLANK        SCST14
     C                   MOVE      *BLANK        SCST15
     C                   MOVE      *BLANK        SCST16
     C                   MOVE      *BLANK        SCST17
     C                   MOVE      *BLANK        SCST18
     C                   MOVE      *BLANK        SCST19
     C                   MOVE      *BLANK        SCST20
     C                   MOVE      *BLANK        SCRM01
     C                   MOVE      *BLANK        SCRM02
     C                   MOVE      *BLANK        SCRM03
     C                   MOVE      *BLANK        SCRM04
     C                   MOVE      *BLANK        SCRM05
     C                   MOVE      *BLANK        SCRM06
     C                   MOVE      *BLANK        SCRM07
     C                   MOVE      *BLANK        SCRM08
     C                   MOVE      *BLANK        SCRM09
     C                   MOVE      *BLANK        SCRM10
     C                   MOVE      *BLANK        SCRM11
     C                   MOVE      *BLANK        SCRM12
     C                   MOVE      *BLANK        SCRM13
     C                   MOVE      *BLANK        SCRM14
     C                   MOVE      *BLANK        SCRM15
     C                   MOVE      *BLANK        SCRM16
     C                   MOVE      *BLANK        SCRM17
     C                   MOVE      *BLANK        SCRM18
     C                   MOVE      *BLANK        SCRM19
     C                   MOVE      *BLANK        SCRM20
     C                   MOVE      *BLANK        SCFL01
     C                   MOVE      *BLANK        SCFL02
     C                   MOVE      *BLANK        SCFL03
     C                   MOVE      *BLANK        SCFL04
     C                   MOVE      *BLANK        SCFL05
     C                   MOVE      *BLANK        SCFL06
     C                   MOVE      *BLANK        SCFL07
     C                   MOVE      *BLANK        SCFL08
     C                   MOVE      *BLANK        SCFL09
     C                   MOVE      *BLANK        SCFL10
     C                   MOVE      *BLANK        SCFL11
     C                   MOVE      *BLANK        SCFL12
     C                   MOVE      *BLANK        SCFL13
     C                   MOVE      *BLANK        SCFL14
     C                   MOVE      *BLANK        SCFL15
     C                   MOVE      *BLANK        SCFL16
     C                   MOVE      *BLANK        SCFL17
     C                   MOVE      *BLANK        SCFL18
     C                   MOVE      *BLANK        SCFL19
     C                   MOVE      *BLANK        SCFL20
     C                   MOVE      *BLANK        SC10A1
     C                   MOVE      *BLANK        SC10A2
     C                   MOVE      *BLANK        SC10A3
     C                   MOVE      *BLANK        SC10A4
     C                   MOVE      *BLANK        SC10A5
     C                   MOVE      *BLANK        SC40A1
     C                   MOVE      *BLANK        SC40A2
     C                   MOVE      *BLANK        SC40A3
     C                   MOVE      *BLANK        SC40A4
     C                   MOVE      *BLANK        SC40A5
     C                   Z-ADD     *ZERO         SC6N01
     C                   Z-ADD     *ZERO         SC6N02
     C                   Z-ADD     *ZERO         SC6N03
     C                   Z-ADD     *ZERO         SC6N04
     C                   Z-ADD     *ZERO         SC6N05
     C                   Z-ADD     *ZERO         SC6N06
     C                   Z-ADD     *ZERO         SC6N07
     C                   Z-ADD     *ZERO         SC6N08
     C                   Z-ADD     *ZERO         SC6N09
     C                   Z-ADD     *ZERO         SC6N10
      *** BEG ADD *** 02/19/03 ****************************
     C                   Z-ADD     *ZERO         SC9N01
     C                   Z-ADD     *ZERO         SC9N02
     C                   Z-ADD     *ZERO         SC9N03
     C                   Z-ADD     *ZERO         SC9N04
     C                   Z-ADD     *ZERO         SC9N05
      *** END ADD *** 02/19/03 ****************************
      *
     C                   ENDSR
      *
      *****************************************************************
      *  CPASGN - Assign All Client Control Fields to Screen Fields
      *****************************************************************
     C     CPASGN        BEGSR
      *
     C                   MOVE      XCSL01        SCSL01
     C                   MOVE      XCSL02        SCSL02
     C                   MOVE      XCSL03        SCSL03
     C                   MOVE      XCSL04        SCSL04
     C                   MOVE      XCSL05        SCSL05
     C                   MOVE      XCSL06        SCSL06
     C                   MOVE      XCSL07        SCSL07
     C                   MOVE      XCSL08        SCSL08
     C                   MOVE      XCSL09        SCSL09
     C                   MOVE      XCSL10        SCSL10
     C                   MOVE      XCSL11        SCSL11
     C                   MOVE      XCSL12        SCSL12
     C                   MOVE      XCSL13        SCSL13
     C                   MOVE      XCSL14        SCSL14
     C                   MOVE      XCSL15        SCSL15
     C                   MOVE      XCSL16        SCSL16
     C                   MOVE      XCSL17        SCSL17
     C                   MOVE      XCSL18        SCSL18
     C                   MOVE      XCSL19        SCSL19
     C                   MOVE      XCSL20        SCSL20
     C                   MOVE      XCST01        SCST01
     C                   MOVE      XCST02        SCST02
     C                   MOVE      XCST03        SCST03
     C                   MOVE      XCST04        SCST04
     C                   MOVE      XCST05        SCST05
     C                   MOVE      XCST06        SCST06
     C                   MOVE      XCST07        SCST07
     C                   MOVE      XCST08        SCST08
     C                   MOVE      XCST09        SCST09
     C                   MOVE      XCST10        SCST10
     C                   MOVE      XCST11        SCST11
     C                   MOVE      XCST12        SCST12
     C                   MOVE      XCST13        SCST13
     C                   MOVE      XCST14        SCST14
     C                   MOVE      XCST15        SCST15
     C                   MOVE      XCST16        SCST16
     C                   MOVE      XCST17        SCST17
     C                   MOVE      XCST18        SCST18
     C                   MOVE      XCST19        SCST19
     C                   MOVE      XCST20        SCST20
     C                   MOVE      XCRM01        SCRM01
     C                   MOVE      XCRM02        SCRM02
     C                   MOVE      XCRM03        SCRM03
     C                   MOVE      XCRM04        SCRM04
     C                   MOVE      XCRM05        SCRM05
     C                   MOVE      XCRM06        SCRM06
     C                   MOVE      XCRM07        SCRM07
     C                   MOVE      XCRM08        SCRM08
     C                   MOVE      XCRM09        SCRM09
     C                   MOVE      XCRM10        SCRM10
     C                   MOVE      XCRM11        SCRM11
     C                   MOVE      XCRM12        SCRM12
     C                   MOVE      XCRM13        SCRM13
     C                   MOVE      XCRM14        SCRM14
     C                   MOVE      XCRM15        SCRM15
     C                   MOVE      XCRM16        SCRM16
     C                   MOVE      XCRM17        SCRM17
     C                   MOVE      XCRM18        SCRM18
     C                   MOVE      XCRM19        SCRM19
     C                   MOVE      XCRM20        SCRM20
     C                   MOVE      XCFL01        SCFL01
     C                   MOVE      XCFL02        SCFL02
     C                   MOVE      XCFL03        SCFL03
     C                   MOVE      XCFL04        SCFL04
     C                   MOVE      XCFL05        SCFL05
     C                   MOVE      XCFL06        SCFL06
     C                   MOVE      XCFL07        SCFL07
     C                   MOVE      XCFL08        SCFL08
     C                   MOVE      XCFL09        SCFL09
     C                   MOVE      XCFL10        SCFL10
     C                   MOVE      XCFL11        SCFL11
     C                   MOVE      XCFL12        SCFL12
     C                   MOVE      XCFL13        SCFL13
     C                   MOVE      XCFL14        SCFL14
     C                   MOVE      XCFL15        SCFL15
     C                   MOVE      XCFL16        SCFL16
     C                   MOVE      XCFL17        SCFL17
     C                   MOVE      XCFL18        SCFL18
     C                   MOVE      XCFL19        SCFL19
     C                   MOVE      XCFL20        SCFL20
     C                   MOVE      XC10A1        SC10A1
     C                   MOVE      XC10A2        SC10A2
     C                   MOVE      XC10A3        SC10A3
     C                   MOVE      XC10A4        SC10A4
     C                   MOVE      XC10A5        SC10A5
     C                   MOVE      XC40A1        SC40A1
     C                   MOVE      XC40A2        SC40A2
     C                   MOVE      XC40A3        SC40A3
     C                   MOVE      XC40A4        SC40A4
     C                   MOVE      XC40A5        SC40A5
     C                   Z-ADD     XC6N01        SC6N01
     C                   Z-ADD     XC6N02        SC6N02
     C                   Z-ADD     XC6N03        SC6N03
     C                   Z-ADD     XC6N04        SC6N04
     C                   Z-ADD     XC6N05        SC6N05
     C                   Z-ADD     XC6N06        SC6N06
     C                   Z-ADD     XC6N07        SC6N07
     C                   Z-ADD     XC6N08        SC6N08
     C                   Z-ADD     XC6N09        SC6N09
     C                   Z-ADD     XC6N10        SC6N10
      *** BEG ADD *** 02/19/03 ****************************
     C                   Z-ADD     XC9N01        SC9N01
     C                   Z-ADD     XC9N02        SC9N02
     C                   Z-ADD     XC9N03        SC9N03
     C                   Z-ADD     XC9N04        SC9N04
     C                   Z-ADD     XC9N05        SC9N05
      *** END ADD *** 02/19/03 ****************************
      *
     C                   ENDSR
      *
      *****************************************************************
      *  CPUPDT - Assign Screen Fields to Client Control Fields
      *****************************************************************
     C     CPUPDT        BEGSR
      *
     C                   Z-ADD     XXCO#         XCCO#
     C                   Z-ADD     SCFND         XCFND
     C                   Z-ADD     SCSUB#        XCSUB#
     C                   Z-ADD     SCGRP         XCGRP
     C                   MOVE      SCSL01        XCSL01
     C                   MOVE      SCSL02        XCSL02
     C                   MOVE      SCSL03        XCSL03
     C                   MOVE      SCSL04        XCSL04
     C                   MOVE      SCSL05        XCSL05
     C                   MOVE      SCSL06        XCSL06
     C                   MOVE      SCSL07        XCSL07
     C                   MOVE      SCSL08        XCSL08
     C                   MOVE      SCSL09        XCSL09
     C                   MOVE      SCSL10        XCSL10
     C                   MOVE      SCSL11        XCSL11
     C                   MOVE      SCSL12        XCSL12
     C                   MOVE      SCSL13        XCSL13
     C                   MOVE      SCSL14        XCSL14
     C                   MOVE      SCSL15        XCSL15
     C                   MOVE      SCSL16        XCSL16
     C                   MOVE      SCSL17        XCSL17
     C                   MOVE      SCSL18        XCSL18
     C                   MOVE      SCSL19        XCSL19
     C                   MOVE      SCSL20        XCSL20
     C                   MOVE      SCST01        XCST01
     C                   MOVE      SCST02        XCST02
     C                   MOVE      SCST03        XCST03
     C                   MOVE      SCST04        XCST04
     C                   MOVE      SCST05        XCST05
     C                   MOVE      SCST06        XCST06
     C                   MOVE      SCST07        XCST07
     C                   MOVE      SCST08        XCST08
     C                   MOVE      SCST09        XCST09
     C                   MOVE      SCST10        XCST10
     C                   MOVE      SCST11        XCST11
     C                   MOVE      SCST12        XCST12
     C                   MOVE      SCST13        XCST13
     C                   MOVE      SCST14        XCST14
     C                   MOVE      SCST15        XCST15
     C                   MOVE      SCST16        XCST16
     C                   MOVE      SCST17        XCST17
     C                   MOVE      SCST18        XCST18
     C                   MOVE      SCST19        XCST19
     C                   MOVE      SCST20        XCST20
     C                   MOVE      SCRM01        XCRM01
     C                   MOVE      SCRM02        XCRM02
     C                   MOVE      SCRM03        XCRM03
     C                   MOVE      SCRM04        XCRM04
     C                   MOVE      SCRM05        XCRM05
     C                   MOVE      SCRM06        XCRM06
     C                   MOVE      SCRM07        XCRM07
     C                   MOVE      SCRM08        XCRM08
     C                   MOVE      SCRM09        XCRM09
     C                   MOVE      SCRM10        XCRM10
     C                   MOVE      SCRM11        XCRM11
     C                   MOVE      SCRM12        XCRM12
     C                   MOVE      SCRM13        XCRM13
     C                   MOVE      SCRM14        XCRM14
     C                   MOVE      SCRM15        XCRM15
     C                   MOVE      SCRM16        XCRM16
     C                   MOVE      SCRM17        XCRM17
     C                   MOVE      SCRM18        XCRM18
     C                   MOVE      SCRM19        XCRM19
     C                   MOVE      SCRM20        XCRM20
     C                   MOVE      SCFL01        XCFL01
     C                   MOVE      SCFL02        XCFL02
     C                   MOVE      SCFL03        XCFL03
     C                   MOVE      SCFL04        XCFL04
     C                   MOVE      SCFL05        XCFL05
     C                   MOVE      SCFL06        XCFL06
     C                   MOVE      SCFL07        XCFL07
     C                   MOVE      SCFL08        XCFL08
     C                   MOVE      SCFL09        XCFL09
     C                   MOVE      SCFL10        XCFL10
     C                   MOVE      SCFL11        XCFL11
     C                   MOVE      SCFL12        XCFL12
     C                   MOVE      SCFL13        XCFL13
     C                   MOVE      SCFL14        XCFL14
     C                   MOVE      SCFL15        XCFL15
     C                   MOVE      SCFL16        XCFL16
     C                   MOVE      SCFL17        XCFL17
     C                   MOVE      SCFL18        XCFL18
     C                   MOVE      SCFL19        XCFL19
     C                   MOVE      SCFL20        XCFL20
     C                   MOVE      SC10A1        XC10A1
     C                   MOVE      SC10A2        XC10A2
     C                   MOVE      SC10A3        XC10A3
     C                   MOVE      SC10A4        XC10A4
     C                   MOVE      SC10A5        XC10A5
     C                   MOVE      SC40A1        XC40A1
     C                   MOVE      SC40A2        XC40A2
     C                   MOVE      SC40A3        XC40A3
     C                   MOVE      SC40A4        XC40A4
     C                   MOVE      SC40A5        XC40A5
     C                   Z-ADD     SC6N01        XC6N01
     C                   Z-ADD     SC6N02        XC6N02
     C                   Z-ADD     SC6N03        XC6N03
     C                   Z-ADD     SC6N04        XC6N04
     C                   Z-ADD     SC6N05        XC6N05
     C                   Z-ADD     SC6N06        XC6N06
     C                   Z-ADD     SC6N07        XC6N07
     C                   Z-ADD     SC6N08        XC6N08
     C                   Z-ADD     SC6N09        XC6N09
     C                   Z-ADD     SC6N10        XC6N10
     C                   MOVE      *BLANK        XCSTAT
     C                   MOVEL     Q1USER        XCUSER
     C                   MOVEL     Q1PGM         XCPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        XCTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XCDT
      *** BEG ADD *** 02/19/03 ****************************
     C                   Z-ADD     SC9N01        XC9N01
     C                   Z-ADD     SC9N02        XC9N02
     C                   Z-ADD     SC9N03        XC9N03
     C                   Z-ADD     SC9N04        XC9N04
     C                   Z-ADD     SC9N05        XC9N05
      *** END ADD *** 02/19/03 ****************************
      *
     C     *IN50         IFEQ      *ON
     C                   MOVEL     XCUSER        XCIUSR
     C                   Z-ADD     XCDT          XCIDT
     C                   ENDIF
      *
     C                   ENDSR
      *
**  TABLE SCD - Valid Security Type Codes
ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789
