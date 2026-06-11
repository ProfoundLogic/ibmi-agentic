     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE F9901R - PROMPT FOR DETAIL CHARGE LIST BY FND/SS#/ACDT
     H*****************************************************************
     H* INDICATOR USAGE
     H*  20    MCR0508 - Invalid Fund
     H*  21    MCR0502 - SS# required
     H*  22    MCR0500 - Invalid accident
     H*  23    MCR0719 - SS#/ACDT not found in Fund setup
     H*  24    MCR0505 - Record not found (invalid SS#/ACDT)
     H*  25    MCR0546 - INVALID PAYYEE
     H*  28    MCR0324 - INVALID CHARGE TYPE ENTERED
     H*  30    POSITION CURSOR AT SUBSCRIBER
     H*  31    POSITION CURSOR AT GROUP
     H*  32    POSITION CURSOR AT SOCIAL SECURITY #
     H*  33    POSITION CURSOR AT ACCIDENT DATE
     H*  34    POSITION CURSOR AT PROVIDER
     H*  35    MC40004 - INVALID SEARCH FIELD
     H*  36    POSITION CURSOR AT PROVIDER MODIFIER
     H*  37    POSITION CURSOR AT CHARGE TYPE
     H*  60    General Chain
     H*****************************************************************
     H* REVISIONS:
     H*                                                              **
     H* 04/22/98  C:90  PROJ:MC181                                   **
     H*                 SCR01 - CHANGED TO MEET VERSION 4 STANDARDS  **
     H*                 COMBINED ALL SEARCHES INTO F10 SEARCH        **
     H*                                                              **
     H* 06/29/94  C:05  REQ/PROJ: 141/MC110 (FIX)                    **
     H*                 1. SCR01 - chg use of F0006R1 to F0006R2     **
     H*                    so that Sub# is properly updated when     **
     H*                    same Grp# is used by more than 1 Sub      **
     H*                                                              **
     H* 05/19/94  C:20     REQ/PROJ: 141/MC110
     H*                    1. CHANGE F9 ALPHA SEARCH TO CALL NEW F0578R1/F1
     H*                       IF A RECORD IS SELECTED FROM THIS SEARCH
     H*                       PGM WILL AUTOMATICALLY CALL F0006R1 TO UPDATE
     H*                       THE USER PARAMETER RECORD TO THE SAME SUB/GRP
     H*                       AS THE RECORD THAT HAS BEEN SELECTED.
     H*                                                              **
     H*****************************************************************
     H*****************************************************************
     FF9901F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
      **** BEGIN CHG **** 04/21/98 ************************************
     FFMFNL     IF   E           K DISK
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     FFMSML     IF   E           K DISK
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
      **** BEGIN DEL **** 04/21/98 ************************************
      ********** FFMCML9  IF  E           K        DISK
      ********** F            PCM01                     KRENAMEPCM02
      **** END   DEL **** 04/21/98 ************************************
      **** END   CHG **** 04/21/98 ************************************
     FFMCMLA    IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM03)
     FFMPDL     IF   E           K DISK

     D*
      **** BEGIN CHG **** 04/17/98 ************************************
     D******************* CK1     1   3 78               CMD KEYS FOR SCR01
      **** END   CHG **** 04/17/98 ************************************
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
     C*  Claim Master File
     C*
     C     CMKY1         KLIST
     C                   KFLD                    SCSS#
     C                   KFLD                    XXACDT
     C*
     C*  Claim Master File
     C*
     C     CMKY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSS#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     CMKY3         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  PAYEE DEMOGRAPHIC RECORD
     C*
     C     PDKEY         KLIST
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*
     C     *LIKE         DEFINE    CMDT          XXACDT
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
     C                   MOVE      *ZEROS        XXFYR             3 0
     C                   MOVE      *ZEROS        XXCASE            7 0
     C                   MOVE      *ZEROS        XXACDT
     C*
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
      **** BEGIN ADD **** 04/23/98 ************************************
     C                   MOVEL(P)  QQPRDS        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
      **** END   ADD **** 04/23/98 ************************************
     C*
     C*   Initialize the Command Keys Display Variable
     C*
      **** BEGIN CHG **** 04/22/98 ************************************
     C******************** Z-ADD1         CK
     C******************** MOVELCK1,CK    SCCMD1
      **** END   CHG **** 04/22/98 ************************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      UPSUB#        SCSUB#
     C                   MOVE      UPFND         SCFND
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      **** BEGIN CHG **** 04/17/98 ************************************
     C******************** MOVE '1'       *IN32
     C                   MOVE      '1'           *IN30
      **** END   CHG **** 04/17/98 ************************************
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
      **** BEGIN DEL **** 04/20/98 ************************************
     C***********************************
     C***********F6 = SS#/ACDT SEARCH          *
     C***********************************
     C********** *INKF     IFEQ '1'
     C******************** CALL 'F0573R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           SCSS#
     C******************** PARM           SCACDT
     C******************** PARM           QQRTCD  1
     C*
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** SCSS#     OREQ *BLANK
     C******************** MOVE '1'       *IN32            PC AT SS#
     C******************** GOTO STRT
     C******************** END                             ENDEQ 'B'
     C*
     C********** QQRTCD    IFEQ '*'
     C******************** MOVE '1'       *IN34            PC AT PROV.
     C******************** GOTO STRT
     C******************** END                             ENDEQ '*'
     C******************** END                             ENDIF *INKF
     C************************************
     C***********F7 = CASE # SEARCH             *
     C************************************
     C********** *INKG     IFEQ '1'
     C********** SCFND     IFEQ *ZEROS
     C******************** MOVE UPFND     SCFND
     C******************** END
     C******************** Z-ADDSCFND     QQFND   30
     C******************** MOVE *BLANKS   XXFYR
     C******************** MOVE *BLANKS   XXCASE
     C******************** CALL 'F0570R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           XXFYR
     C******************** PARM           XXCASE
     C******************** PARM           SCSS#
     C******************** PARM           QQRTCD  1
     C******************** PARM           QQFND
     C******************** MOVE QQFND     SCFND
     C*
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** SCSS#     OREQ *BLANK
     C******************** MOVE '1'       *IN32            PC AT SS#
     C******************** GOTO STRT
     C******************** END
     C*
     C********** QQRTCD    IFEQ '*'
     C********** CMKY2     CHAINPCM02                60
     C********** *IN60     IFEQ '0'
     C******************** MOVE CMSS#     SCSS#
     C*
     C******************** Z-ADDCMACDT    SRDTI
     C******************** CALL 'SRCVTDAT'DTMDYY
     C******************** MOVELSRDTO     SCACDT
     C*
     C******************** Z-ADDCMFND     SCFND
     C******************** MOVE '1'       *IN34            PC AT PROV.
     C******************** GOTO STRT
     C******************** END                             END 0 *IN60
     C******************** END                             END EQ '*'
     C******************** END                             END *INKG
     C***********************************
     C********** F8 = SS#/CASE SEARCH          *
     C***********************************
     C********** *INKH     IFEQ '1'
     C********** SRTAG     TAG
     C******************** MOVE *BLANKS   XXFYR
     C******************** MOVE *BLANKS   XXCASE
     C******************** CALL 'F0571R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           SCSS#
     C******************** PARM           XXFYR
     C******************** PARM           XXCASE
     C******************** PARM           SCFND
     C******************** PARM           QQRTCD  1
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** SCSS#     OREQ *BLANK
     C******************** MOVE '1'       *IN32            PC AT SS#
     C******************** GOTO STRT
     C******************** END                             ENDEQ 'B'
     C*
     C********** QQRTCD    IFEQ '*'
     C********** CMKY2     CHAINPCM02                60
     C********** *IN60     IFEQ '0'
     C******************** MOVE CMSS#     SCSS#
     C*
     C******************** Z-ADDCMACDT    SRDTI
     C******************** CALL 'SRCVTDAT'DTMDYY
     C******************** MOVELSRDTO     SCACDT
     C*
     C******************** Z-ADDCMFND     SCFND
     C******************** MOVE '1'       *IN34            PC AT PROV.
     C******************** GOTO STRT
     C******************** END                             END 0 *IN60
     C******************** END                             ENDEQ '*'
     C******************** END                             ENDIF *INKH
     C***********************************
     C********** F9 = ALPHA SEARCH             *
     C***********************************
     C********** *INKI     IFEQ '1'
     C******************** CALL 'F0578R1'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           SCSS#
     C******************** PARM           SCACDT
     C******************** PARM           SCSUB#
     C******************** PARM           SCFND
     C******************** PARM           QQFYR   30
     C******************** PARM           QQCASE  70
     C******************** PARM           QQRTCD  1
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** SCSS#     OREQ *BLANK
     C******************** MOVE '1'       *IN32            PC AT SS#
     C******************** GOTO STRT
     C******************** END                             ENDEQ 'B'
     C*
     C********** QQRTCD    IFEQ '*'
     C******************** MOVE UPCO#     QQCO#   3
     C******************** MOVE SCSUB#    Q1SUB#  5        ADD 06/29/94
     C******************** MOVE SCFND     Q1FND   3
     C******************** CALL 'F0006R2'                  CHG 06/29/94
     C******************** PARM           QQCO#
     C******************** PARM           Q1SUB#           ADD 06/29/94
     C******************** PARM           Q1FND
     C******************** PARM           QQRTCD
     C******************** MOVE '1'       *IN34            PC AT PROV.
     C******************** GOTO STRT
     C******************** END
     C******************** END                             ENDIF *INKI
      **** END   DEL **** 04/20/98 ************************************
     C***********************************
     C*   F10 = SEARCH                  *
     C***********************************
      *
      ***********************************
      *  SUBSCRIBER SEARCH              *
      ***********************************
     C     *INKJ         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
      **** BEGIN CHG **** 04/16/98 ************************************
     C     HFLD          IFEQ      'SCSUB#'
      **** END   CHG **** 04/16/98 ************************************
      *
      **** BEGIN CHG **** 04/21/98 ************************************
     C******************** CALL 'F0545R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           SCSUB#
     C******************** PARM           QQSUB#  50
     C******************** PARM           QQRTC   1
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         CABEQ     *ON           STRT                     30
      *
     C     ##RRN         CHAIN     PSM02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVE      *ON           *IN31
     C                   ELSE
     C                   MOVE      *ON           *IN30
     C                   ENDIF                                                  *IN60 IFEQ
     C                   GOTO      STRT
     C********** QQRTC     CABEQ'A'       TERM
     C*
     C********** QQRTC     IFEQ 'B'
     C********** SCSUB#    OREQ *ZERO
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** MOVE '1'       *IN30
     C******************** GOTO STRT
     C******************** END                             END EQUAL B
     C*
     C********** QQRTC     IFEQ '*'
     C******************** MOVE '1'       *IN31
     C******************** GOTO STRT
     C******************** END                             END EQUAL *
     C*
     C                   ENDIF                                                  HFLD 'SCSUB#'
      *
     C*********************END                             END SCR01
     C*********************END                             ENDIF *INKJ
      *
     C***********************************
     C*  GROUP SEARCH                   *
     C***********************************
      *
     C************INKK     IFEQ '1'
     C     HFLD          IFEQ      'SCFND'
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
     C******************** PARM           QQRTCD
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** X1FND     OREQ *ZERO
     C******************** MOVE '1'       *IN31            POS CUR FUND
     C******************** GOTO STRT
     C******************** END                             END EQUAL B
     C*
     C********** QQRTCD    IFEQ '*'
     C******************** MOVE '1'       *IN32            POS SS#
     C******************** MOVE X1SUB#    SCSUB#
     C******************** MOVE X1FND     SCFND
     C******************** GOTO STRT
     C******************** END                             END EQUAL *
     C*
     C                   CALL      'F0060R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
      *
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     31
      *
     C     ##RRN         CHAIN     PFN02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      FNFND         SCFND
     C                   MOVE      *ON           *IN32
     C                   ELSE
     C                   MOVE      *ON           *IN31
     C                   ENDIF                                                  *IN60 IFEQ
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD 'SCFND'
     C*********************END                             ENDIF *INKK
      **** END   CHG **** 04/21/98 ************************************
      *
      **** BEGIN CHG **** 04/20/98 ************************************
      *
      **********************************************
      *  SOCIAL SECURITY # & ACCIDENT DATE SEARCH  *
      **********************************************
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
     C*
     C     QQRTCD        IFEQ      *BLANK
     C     HFLD          CABEQ     'SCSS#'       STRT                     32    PC AT SS#
     C     HFLD          CABEQ     'SCACDT'      STRT                     33    PC AT ACDT
     C                   ENDIF                                                  ENDIF QQRTCD
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN34                          PC AT PROV.
     C                   MOVE      QQSS#         SCSS#             9
     C                   MOVE      QQACDT        SCACDT            8 0
     C                   MOVE      QQSUB#        SCSUB#
     C                   MOVE      QQFND         SCFND
     C                   GOTO      STRT
     C                   ENDIF                                                  ENDEQ '*'
     C                   ENDIF                                                  HFLD  'SCSS#'
      *
      *******************************************
      *  PROVIDER AND PROVIDER MODIFIER SEARCH  *
      *******************************************
     C     HFLD          IFEQ      'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
     C                   CALL      'F0032R'
     C                   PARM      '2'           QQCALL
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQTYPE            1
      *
     C     QQRTCD        CABEQ     'A'           TERM
      *
     C     QQRTCD        IFEQ      'B'
     C     HFLD          CABEQ     'SCFEIN'      STRT                     34    PC AT PROV
     C     HFLD          CABEQ     'SCMOD'       STRT                     36    PC AT PROV
     C                   END                                                    ENDEQ 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN37                          PC AT CHRG TYPE
     C                   MOVE      QQFEIN        SCFEIN
     C                   MOVE      QQMOD         SCMOD
     C                   GOTO      STRT
     C                   END                                                    ENDEQ '*'
     C                   ENDIF                                                  HLFD 'SCFEIN'
     C                   MOVE      *ON           *IN35
     C                   GOTO      STRT
     C                   ENDIF                                                  SCR01
     C                   ENDIF                                                  *INKJ
      **** END   CHG **** 04/20/98 ************************************
     C***********************************
     C*   *IN10 = PROCESS HELP REQUESTS
     C***********************************
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
     C                   END                                                    END *IN10
      **** BEGIN DEL **** 04/23/98 ************************************
     C***********************************
     C***********   F13 = CHG USER PARMS          *
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
      **** END   DEL **** 04/23/98 ************************************
      *
      **** BEGIN DEL **** 04/22/98 ************************************
     C***********************************
     C********** F24 = MORE KEYS               *
     C***********************************
     C********** *INKY     IFEQ '1'
     C******************** MOVELQ2RECF    HLRECF
     C********** CK        ADD  1         CK      50
     C*
     C********** HLRECF    IFEQ 'SCR01'
     C********** CK        IFGT 3
     C******************** Z-ADD1         CK
     C******************** END                             END CK GT 2
     C******************** MOVELCK1,CK    SCCMD1
     C******************** END                             END SCR01
     C*
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVELHLRECF    Q2RECF
     C******************** GOTO STRT
     C******************** END                             END *INKY
      **** END   DEL **** 04/22/98 ************************************
     C*
     C                   END                                                    END CMDKEYS
     C**************************************************************************
     C*
     C*   PROCESS REQUEST SCREEN
     C     SCR01T        TAG
     C*
     C* VALIDATE SUB#
     C     SMKEY         CHAIN     PSM01                              27        INVALID SUB#
     C     *IN27         CABEQ     '1'           STRT
     C*
     C* Validate Fund Number
     C     FNKEY         CHAIN     PFN01                              20        INVALID FUND
     C     *IN20         CABEQ     '1'           STRT
     C*
     C* SS# required
     C     SCSS#         CABEQ     *BLANK        STRT                     21
     C*
     C* Validate Accident
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCACDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     22
     C*
     C* Validate Claim (does it exist?)
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C*
     C*** Check for multiple cases under the same SS#/ACDT
     C                   Z-ADD     *ZEROS        CNTCAS            3 0
     C     CMKY1         CHAIN     PCM01                              24         SS#/ACDT KEY
     C     *IN24         DOWEQ     '0'
     C                   ADD       1             CNTCAS
     C     CMKY1         READE     PCM01                                  24
     C                   END
     C*
     C*** If no hit, this is an invalid SS#/ACDT - Send Error Message (*IN24)
     C*** Else, setof *IN24 so message is not displayed
     C     CNTCAS        IFEQ      *ZEROS
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      '0'           *IN24
     C                   END
     C*
     C*** If there is more than 1 case, then display "Multi Case" Search F0579F
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
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   WRITE     SCR01
     C*
     C     QTRTCD        IFEQ      'A'
     C     QTRTCD        OREQ      'B'
     C     QTFND         OREQ      *BLANKS
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END                                                    END EQ A/B/*BLANKS
     C*
     C     QTRTCD        IFEQ      '*'
     C                   MOVE      QTFND         SCFND
     C                   MOVE      QTFYR         XXFYR
     C                   MOVE      QTCASE        XXCASE
     C     CMKY3         CHAIN     PCM03                              24
     C     *IN24         CABEQ     '1'           STRT
     C                   END                                                    END EQ *
     C*
     C                   END                                                    END GT 1
     C*
     C*
     C* Validate Claim for the Co/Fnd setup
     C     CMCO#         CABNE     UPCO#         STRT                 2323
     C     CMSUB#        CABNE     SCSUB#        STRT                 2323
     C     CMFND         CABNE     SCFND         STRT                 2323
     C*
     C* IF PROVIDER ENTERED MUST BE VALID
     C*
     C     SCFEIN        IFNE      *ZEROS
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C                   END
     C     SCTYPE        IFNE      'A'
     C     SCTYPE        ANDNE     'C'
     C     SCTYPE        ANDNE     'P'
     C     SCTYPE        ANDNE     'H'
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END                                                    END SCTYPE
     C*
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
     C*   CALL THE CLP TO RUN THE LIST.
     C                   CALL      'F9900C'
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
     C                   PARM      'D'           QQMODE            1
     C*
     C*  END THE PROGRAM
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
