     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N9909R - PROMPT FOR NETWORK INFORMATION REPORT
     H*****************************************************************
     H* INDICATOR USAGE
     H*  20    POSITION CURSOR AT SUBSCRIBER NUMBER
     H*  21    MCR0301 - INVALID SUBSCRIBER NUMBER
     H*  22    POSITION CURSOR AT GROUP NUMBER
     H*  23    MCR0508 - INVALID GROUP NUMBER
     H*  24    POSITION CURSOR AND PROVIDER
     H*  25    MCR0546 - INVALID PROVIDER
     H*  26    MCR0502 - SUBSCRIBER FIELD CANNOT BE EQUAL TO BLANKS
     H*  27    MCR0502 - GROUP FIELD CANNOT BE EQUAL TO BLANKS
     H*  28    MCR0502 - PROV FIELD CANNOT BE EQUAL TO BLANKS
     H*  29    MCR0503 - SYSTEM FIELD CANNOT BE EQUAL TO BLANKS
     H*  30    POSITION CURSOR AT SYSTEM TYPE
     H*  31    MCR0740 - INVALID SYSTEM CODE ENTERED
     H*  32    MCR0331 - INVALID SYSTEM ID FOR CURRENT GROUP
     H*  34    POSITION CURSOR AT NETWORK CODE
     H*  35    MCR0043 - INVALID NETWORK CODE ENTERED.
     H*  36    MCR0044 - FEIN MUST BE ENTERED PRIOR TO F11 SEARCH BEING AVAIL
     H*  60    General Chain
     H*****************************************************************
     H** REVISIONS:                                                  **
     H*                                                              **
     H*  12/18/24 C:342 Project : MC447 Phase 2                      **
     H*                 1.F10 Search disabled on the Group Field     **
     H*                 DSPF Changes:                                **
     H*                 1.Changed Existing Group field into Typeahead**
     H*                                                              **
     H*  04/30/98 C:90  Project: MC181                               **
     H*                 SCR01 - CHANGED TO MEET VERSION 4 STANDARDS  **
     H*                 COMBINED ALL SEARCHES INTO F10 SEARCH        **
     H*                                                              **
     H*  02/18/98 C:62  Project: MC159                               **
     H*                 1) REPLACE N0531R SEARCH TO N0037R.          **
     H*                 2) CHANGE NMPHL & NMHSL WITH FMS4L2/L3.      **
     H*                                                              **
     H*****************************************************************
     FN9909F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     F*** BEGIN CHG *** 02/18/98 **************************************
     F********** NMHSL   IF  E           K        DISK
     F********** NMPHL   IF  E           K        DISK
     FFMS4L2    IF   E           K DISK
     FFMS4L3    IF   E           K DISK
     F                                     RENAME(PS401:PS402)
     F*** END   CHG *** 02/18/98 **************************************
      **** BEGIN ADD **** 04/29/98 ************************************
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     FNMNWP     IF   E             DISK
     F                                     RENAME(PNW01:PNW02)
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
      **** END   ADD **** 04/29/98 ************************************

     D*
      **** BEGIN DEL **** 04/30/98 ************************************
     D******************** K1     1   3 78               CMD KEYS FOR SCR01
      **** END   DEL **** 04/30/98 ************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  FSY                    1     10
     D                                     DIM(10) ASCEND                       GROUP VALID SYS
     D  FNSY01                 1      1
     D  FNSY02                 2      2
     D  FNSY03                 3      3
     D  FNSY04                 4      4
     D  FNSY05                 5      5
     D  FNSY06                 6      6
     D  FNSY07                 7      7
     D  FNSY08                 8      8
     D  FNSY09                 9      9
     D  FNSY10                10     10
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  Company Master File
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  User Parameter File
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  Fund Master
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*
     C*  SUB# Master
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C*
     C*  PAYEE DEMOGRAPHIC RECORD
     C*
     C     PDKEY         KLIST
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*** BEGIN DEL *** 02/18/98 **************************************
     C*
     C*  PROVIDER PPO NETWORK MASTER FILE
     C*
     C********** PHKEY     KLIST
     C**********           KFLD           SCFEIN
     C**********           KFLD           SCMOD
     C**********           KFLD           SCNWCD
     C*
     C*  HOSPITAL PPO NETWORK MASTER FILE
     C*
     C********** HSKEY     KLIST
     C**********           KFLD           SCFEIN
     C**********           KFLD           SCMOD
     C**********           KFLD           SCNWCD
     C*** END   DEL *** 02/18/98 **************************************
     C*** BEGIN ADD *** 02/18/98 **************************************
     C*
     C*  PROVIDER NETWORK CONTRACT FILE.
     C*
     C     S4KEY         KLIST
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C                   KFLD                    SCNWCD
     C*** END   ADD *** 02/18/98 **************************************
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
     C     *LIKE         DEFINE    Q2RECF        HLRECF
     C*
     C                   MOVE      *BLANKS       YYCO#             3
     C                   MOVE      *BLANKS       YYSUB#            5
     C                   MOVE      *BLANKS       YYFND             3
     C                   MOVE      *BLANKS       YYSYS             1
     C                   MOVE      *BLANKS       YYFEIN            9
     C                   MOVE      *BLANKS       YYMOD             3
     C                   MOVE      *BLANKS       YYNWCD            5
     C                   MOVE      *ZEROS        QQFEIN            9
     C                   MOVE      *ZEROS        QQMOD             3
     C*
     C*  GET THE USER PARAMETER FILE
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
      **** BEGIN DEL **** 04/30/98 ************************************
     C*   Initialize the Command Keys Display Variable
     C*
     C******************** Z-ADD1         CK
     C******************** MOVELCK1,CK    SCCMD1
      **** BEGIN DEL **** 04/30/98 ************************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      UPSUB#        SCSUB#
     C                   MOVE      UPFND         SCFND
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN20
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C**************************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C***********************************
     C*   F3 = EXIT                     *
     C***********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
      **** BEGIN CHG **** 04/30/98 ************************************
      *
     C***********************************
     C*       F10 = SEARCH              *
     C***********************************
      *
     C     *INKJ         IFEQ      '1'
      *
     C***********************************
     C*       SUBSCRIBER SEARCH         *
     C***********************************
      *
     C     HFLD          IFEQ      'SCSUB#'
     C                   CALL      'F0055R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQRRNS            9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         CABEQ     *ON           STRT                     20
      *
     C     QQRRNS        CHAIN     PSM02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVE      *ON           *IN22
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD SCSUB#
     C***********************************
     C**********   F6 = SUBSCRIBER SEARCH        *
     C***********************************
     C********** *INKF     IFEQ '1'
     C********** Q2RECF    IFEQ 'SCR01'
     C******************** CALL 'F0545R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           SCSUB#
     C******************** PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C*
     C********** QQRTC     IFEQ 'B'
     C********** SCSUB#    OREQ *ZERO
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** MOVE '1'       *IN20
     C******************** GOTO STRT
     C******************** END                             END EQUAL B
     C*
     C********** QQRTC     IFEQ '*'
     C******************** MOVE '1'       *IN22
     C******************** GOTO STRT
     C******************** END
     C*
     C******************** END                             END SCR01
     C******************** END                             ENDIF *INKF
      *
     C************************************
     C*         GROUP SEARCH             *
     C************************************
      *
     C******** Beg Del ***** 12/18/24 ***************************
     C*****HFLD          IFEQ      'SCFND'
     C*****              MOVE      QQRRNS        QQRRNF
     C*****              CALL      'F0060R'
     C*****              PARM      '2'           QQCALL            1
     C******INKC         PARM                    QQKC              1
     C******INKL         PARM                    QQKL              1
     C*****              PARM                    QQRRNF            9 0
      *****
     C******INKC         CABEQ     *ON           TERM
      *****
     C******INKL         CABEQ     *ON           STRT                     22
      *****
     C*****QQRRNF        CHAIN     PFN02                              60
     C******IN60         IFEQ      *OFF
     C*****              MOVE      FNFND         SCFND
     C*****              MOVE      *ON           *IN30
     C*****              ELSE
     C*****              MOVE      *ON           *IN22
     C*****              ENDIF                                                  IFEQ *IN60
      *****
     C*****              GOTO      STRT
     C*****              ENDIF                                                  HFLD SCFND
     C******** End Del ***** 12/18/24 ***************************
     C************************************
     C**********   F7 = GROUP SEARCH              *
     C************************************
     C********** *INKG     IFEQ '1'
     C********** SCSUB#    IFEQ *ZEROS
     C******************** MOVE UPSUB#    SCSUB#
     C******************** END
     C******************** Z-ADDSCSUB#    X1SUB#  50
     C******************** Z-ADDSCFND     X1FND   30
     C******************** CALL 'F0508R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           X1SUB#
     C******************** PARM           X1FND
     C******************** PARM           QQRTCD  1
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** X1FND     OREQ *ZERO
     C******************** MOVE '1'       *IN22            POS CUR FUND
     C******************** GOTO STRT
     C******************** END
     C*
     C********** QQRTCD    IFEQ '*'
     C******************** MOVE '1'       *IN30            POS SS#
     C******************** MOVE X1SUB#    SCSUB#
     C******************** MOVE X1FND     SCFND
     C******************** GOTO STRT
     C******************** END
     C*
     C******************** END                             END *INKG
      *
      *******************************************
      *  PROVIDER AND PROVIDER MODIFIER SEARCH  *
      *******************************************
     C     HFLD          IFEQ      'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
     C                   CALL      'F0032R'
     C                   PARM      '2'           QQCALL
     C                   PARM      *BLANK        QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQTYPE            1
      *
     C     QQRTCD        CABEQ     'A'           TERM
      *
     C     QQRTCD        IFEQ      'B'
     C     QQFEIN        OREQ      *BLANK
     C     HFLD          CABEQ     'SCFEIN'      STRT                     24    PC AT PROV
     C     HFLD          CABEQ     'SCMOD'       STRT                     37    PC AT PROV
     C                   ENDIF                                                  ENDEQ 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN34                          PC AT CHRG TYPE
     C                   MOVE      QQFEIN        SCFEIN
     C                   MOVE      QQMOD         SCMOD
     C                   ENDIF                                                  ENDEQ '*'
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HLFD 'SCFEIN'
     C**********************************
     C*   SYSTEM TYPE SEARCH           *
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
     C     *INKL         CABEQ     *ON           STRT                     30
      *
     C     ##RRNP        CHAIN     PGT02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCSYS
     C                   MOVE      *ON           *IN24
     C                   ELSE
     C                   MOVE      *ON           *IN30
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   ENDIF                                                  HFLD SCSYS
     C***********************************
     C**********   F8 = SYSTEM SEARCH            *
     C***********************************
     C********** *INKH     IFEQ '1'
     C*
     C******************** MOVE SCSYS     QQKEY  10
     C******************** MOVE *BLANK    QQRTC   1
     C******************** CALL 'F0541R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           QQKEY
     C******************** PARM           QQRTC
     C*
     C******************** MOVE QQKEY     SCSYS
     C*
     C********** QQRTC     CABEQ'A'       TERM
     C*
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTC     IFEQ 'B'
     C********** QQKEY     OREQ *BLANKS
     C******************** MOVE '1'       *IN30            POS CUR SYS
     C******************** GOTO STRT
     C******************** END
     C*
     C********** QQRTC     IFEQ '*'
     C******************** MOVE '1'       *IN24            POS FEIN
     C******************** GOTO STRT
     C******************** END
     C*
     C******************** END                             ENDIF *INKH
      *
     C***********************************
     C**********   F9 = PROVIDER SEARCH          *
     C***********************************
     C********** *INKI     IFEQ '1'
     C*
     C******************** CALL 'F0032R'
     C******************** PARM '2'       ##CALL  1
     C******************** PARM           ##FEIN  9
     C******************** PARM           ##MOD   3
     C******************** PARM *BLANKS   ##RTCD  1
     C******************** PARM *BLANKS   ##TYPE  1
     C*
     C******************** WRITESCR99
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** ##FEIN    OREQ *ZEROS
     C******************** MOVE '1'       *IN24            POS CURSOR PROV
     C*
     C******************** ELSE
     C*
     C******************** MOVE ##FEIN    SCFEIN
     C******************** MOVE '1'       *IN34            POS CURSOR NETWORK CO
     C*
     C******************** ENDIF                           ENDEQ 'B'
     C*
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C*
     C******************** END                             ENDIF *INKI
      *
     C**********************************
     C*   NETWORK CODE SEARCH          *
     C**********************************
      *
     C     HFLD          IFEQ      'SCNWCD'
     C                   CALL      'N2325R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQRRN             9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         CABEQ     *ON           STRT                     34
      *
     C     QQRRN         CHAIN     PNW02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      NWNWCD        SCNWCD
     C                   ELSE
     C                   MOVE      *ON           *IN34
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HLFD SCNWCD
      *
     C                   MOVE      *ON           *IN90
     C                   GOTO      STRT
     C                   ENDIF                                                  IFEQ *INKJ
      *
     C***********************************
     C***********   F11 = NETWORK CODE SEARCH     *
     C***********************************
     C********** *INKK     IFEQ '1'
     C*** BEGIN DEL *** 02/18/98 **************************************
     C********** SCFEIN    IFNE *ZEROS
     C**********           MOVE *ZEROS    QQFEIN
     C**********           MOVE *ZEROS    QQMOD
     C**********           MOVE SCFEIN    QQFEIN
     C**********           MOVE SCMOD     QQMOD
     C**********           CALL 'N0531R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQFEIN
     C**********           PARM           QQMOD
     C**********           PARM           SCNWCD
     C**********           PARM           QQRTCD
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C**********           WRITESCR99
     C********** QQRTCD    IFEQ 'B'
     C********** SCFEIN    OREQ *ZEROS
     C**********           MOVE '1'       *IN34            POS CURSOR PROV
     C**********           END                             ENDEQ 'B'
     C*
     C********** QQRTCD    IFEQ '*'
     C**********           MOVE '1'       *IN20            POS CURSOR SUB#
     C**********           END                             ENDEQ '*'
     C**********           ELSE                            ELSE SCFEIN NE *ZEROS
     C**********           MOVE '1'       *IN36
     C**********           END                             END SCFEIN NE *ZEROS
     C*
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C*** END   DEL *** 02/18/98 **************************************
     C*** BEGIN ADD *** 02/18/98 **************************************
     C*
     C******************** MOVE SCFEIN    ##FEIN
     C******************** MOVE SCMOD     ##MOD
     C*
     C******************** CALL 'N0037R'
     C******************** PARM '2'       ##CALL  1        Blank = Maint
     C********** *INKC     PARM '0'       ##KC    1
     C********** *INKL     PARM '0'       ##KL    1
     C******************** PARM           ##FEIN  9
     C******************** PARM           ##MOD   3
     C******************** PARM *BLANKS   ##NWCD  5
     C******************** PARM *BLANKS   ##CNT# 15
     C*
     C********** ##NWCD    IFEQ *BLANKS
     C*
     C******************** SETON                     34
     C*
     C******************** ELSE
     C*
     C******************** MOVEL##NWCD    SCNWCD
     C*
     C******************** ENDIF
     C*
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C*
     C*** END   ADD *** 02/18/98 **************************************
     C******************** END                             ENDIF *INKK
     C***********************************
     C********* F13 = CHG USER PARMS          *
     C***********************************
     C********** *INKM     IFEQ '1'
     C*
     C******************** CALL 'F0006R'
     C******************** WRITESCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C******************** MOVE *ZERO     UPCO#
     C******************** END
     C******************** GOTO STRT01
     C******************** END                             END *INKM
     C***********************************
     C**********   F24 = MORE KEYS               *
     C***********************************
     C********** *INKY     IFEQ '1'
     C******************** MOVELQ2RECF    HLRECF
     C********** CK        ADD  1         CK      50
     C*
     C********** HLRECF    IFEQ 'SCR01'
     C********** CK        IFGT 3                          ADD 03/10/93
     C******************** Z-ADD1         CK
     C******************** END                             END CK GT 3
     C******************** MOVELCK1,CK    SCCMD1
     C******************** END                             END SCR01
     C*
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVELHLRECF    Q2RECF
     C******************** GOTO STRT
     C******************** END                             END *INKY
      **** END   CHG **** 04/30/98 ************************************
     C*
     C                   END                                                    END CMDKEYS
     C**************************************************************************
     C*
     C*   PROCESS REQUEST SCREEN
     C     SCR01T        TAG
     C*
     C* VALIDATE SUB#
     C     SMKEY         CHAIN     PSM01                              21
     C     *IN21         CABEQ     '1'           STRT
     C*
     C* Validate Fund Number
     C     FNKEY         CHAIN     PFN01                              23        INVALID FUND
     C     *IN23         CABEQ     '1'           STRT
     C*
     C* VALIDATE THE SYSTEM CODE ENTERED
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCSYS         QQKEY            10
     C                   MOVE      'V'           QQRTC             1
     C                   CALL      'F0541R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C* VALIDATE SYSTEM CODE FOR THIS GROUP
     C*
     C                   Z-ADD     1             X                 5 0          ##
     C     SCSYS         LOOKUP    FSY(X)                                 60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C* PROVIDER MUST BE VALID
     C*
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   ELSE
     C     PDPARN        IFNE      'P'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* THE NETWORK DISCOUNT CODE MUST BE VALID
     C*
     C     PDTYPE        IFEQ      '1'
     C*** BEGIN CHG *** 02/18/98 **************************************
     C     S4KEY         CHAIN     PS402                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END                                                    END *IN60 EQ '1'
     C                   ELSE                                                   ELSE PDTYPE EQ '1'
     C     PDTYPE        IFEQ      '4'
     C     S4KEY         CHAIN     PS401                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END                                                    END *IN60 EQ '1'
     C                   ELSE                                                   ELSE PDTYPE EQ '4'
     C     S4KEY         CHAIN     PS402                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END                                                    END *IN60 EQ '1'
     C                   END                                                    END PDTYPE EQ '4'
     C                   END                                                    END PDTYPE EQ '1'
     C*** END   CHG *** 02/18/98 **************************************
     C*
     C                   MOVE      UPCO#         YYCO#
     C                   MOVE      SCSUB#        YYSUB#
     C                   MOVE      SCFND         YYFND
     C                   MOVE      SCSYS         YYSYS
     C                   MOVE      SCFEIN        YYFEIN
     C                   MOVE      SCMOD         YYMOD
     C                   MOVE      SCNWCD        YYNWCD
     C*
     C*   CALL THE CLP TO RUN THE LIST.
     C                   CALL      'N9909C'
     C                   PARM                    YYCO#
     C                   PARM                    YYSUB#
     C                   PARM                    YYFND
     C                   PARM                    YYSYS
     C                   PARM                    YYFEIN
     C                   PARM                    YYMOD
     C                   PARM                    YYNWCD
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C*
     C*  END THE PROGRAM
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     **** BEGIN DEL **** 04/30/98 *************************************
      *====================================================================*
*************  CK1 - COMMAND KEYS for SCR01
*********       F6=Subscriber Search         F7=Group Search   F24=More keys
********* Search  F9=Provider Search    F11=Prov/Ntwk Search   F24=More keys
*********ser Parms                                             F24=More keys
********* END   DEL **** 04/30/98 *************************************
