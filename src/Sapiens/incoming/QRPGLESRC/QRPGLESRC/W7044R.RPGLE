    HCOPYRIGHT('(C) Copyright Fiserv, Inc., 2006')
    H*********************************************************************
    H/TITLE Transpac Download Prompt
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DEBUG(*YES)
    H*********************************************************************
    H*  CALLED FROM: MENU                                                *
    H*                                                                   *
    H*  CALLS TO:  W7044C                                                *
    H*                                                                   *
    H*  CREATED: 04/14/07      C:183     PROJECT: 135WIB                 *
    H*********************************************************************
    H*  REVISIONS                                                        *
    H*                                                                   *
    H*********************************************************************
      *
    H*  REVISIONS                                                        *
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW7044F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     D*
     D                 DS
     D  ZZDT                   1      8  0
     D  ZZMM                   1      2  0
     D  ZZDD                   3      4  0
     D  ZZYY                   5      8  0
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D OPTNM           C                   CONST('TransPac Claims Data-
     D                                      Extract Process')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C*_______________________________________________________________
     C*
     C     *LIKE         DEFINE    @@PRDS        XXPRDS
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C*_______________________________________________________________
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
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@PRDS           40
     C****                 PARM           @@CALL  1
     C*_______________________________________________________________
     C*
     C                   TIME                    DSDTTM
     C*
     C     Q1PRMS        IFGE      1
     C                   MOVEL(P)  @@PRDS        XXPRDS
     C                   ENDIF                                                  Q1PRMS>=1
     C*
     C     XXPRDS        IFEQ      *BLANKS
     C                   MOVEL(P)  OPTNM         SRDFLD
     C                   ELSE
     C                   MOVEL(P)  XXPRDS        SRDFLD
     C                   ENDIF                                                  XXPRDS=*BLANKS
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C                   Z-ADD     DSDATE        ZZDT
     C                   SUB       1             ZZDD
     C***                  Z-ADD1         ZZDD
     C***                  SUB  1         ZZMM
     C***
     C***        ZZMM      IFEQ *ZERO
     C***                  Z-ADD12        ZZMM
     C***                  SUB  1         ZZYY
     C***                  ENDIF                           ZZMM=*ZERO
     C*
     C*
     C     ZZDD          IFEQ      *ZERO
     C                   SUB       1             ZZMM
     C*
     C     ZZMM          IFEQ      *ZERO
     C                   Z-ADD     12            ZZMM
     C                   SUB       1             ZZYY
     C                   ENDIF                                                  ZZMM=*ZERO
     C*
     C                   SELECT
     C     ZZMM          WHENEQ    4
     C     ZZMM          OREQ      6
     C     ZZMM          OREQ      9
     C     ZZMM          OREQ      11
     C                   Z-ADD     30            ZZDD
     C     ZZMM          WHENEQ    2
     C                   Z-ADD     28            ZZDD
     C                   OTHER
     C                   Z-ADD     31            ZZDD
     C                   ENDSL
     C                   ENDIF
     C*
     C                   Z-ADD     ZZDT          SCFRDT
     C                   Z-ADD     ZZDT          SCTODT
     C*_______________________________________________________________
     C*
     C     TOP           TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C*
     C   60              Z-ADD     *ZERO         UPCO#
     C*
     C                   Z-ADD     UPCO#         XXCO#
     C*
     C*  GET COMPANY NAME
     C*
     C     XXCO#         CHAIN     PCO01                              60
     C*
     C   60              MOVEL     *BLANKS       CONAME
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  Get the Requested Record, Load Information
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C*  Display Screen Formats
     C*
     C     STRT          TAG
     C*
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR01'
     C                   EXFMT     SCR01
     C                   ENDSL
     C*_______________________________________________________________
     C*
     C*  Process All Command Keys
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*  F3=EXIT
     C*
     C     *INKC         CABEQ     *ON           TERM                     LR
     C*
     C*  F10=SEARCH
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFFND'
     C                   MOVEL     UPCO#         @@CO#
     C                   CALL      'W0010R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@CO#             3
     C                   PARM      *BLANKS       @@FND             3
     C                   PARM      *BLANKS       @@FYR             3
     C                   PARM      1             @@VIEW            1 0
     C*
     C                   MOVE      @@FND         ##FND             3 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      'B'
     C     ##FND         OREQ      *ZERO
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C*
     C                   ELSE
     C                   Z-ADD     ##FND         SCFFND
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C     HFLD          WHENEQ    'SCTFND'
     C                   MOVEL     UPCO#         @@CO#
     C                   CALL      'W0010R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@CO#             3
     C                   PARM      *BLANKS       @@FND             3
     C                   PARM      *BLANKS       @@FYR             3
     C                   PARM      1             @@VIEW            1 0
     C*
     C                   MOVE      @@FND         ##FND             3 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      'B'
     C     ##FND         OREQ      *ZERO
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C*
     C                   ELSE
     C                   Z-ADD     ##FND         SCTFND
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *IN11=*ON
     C*_______________________________________________________________
     C*
     C     SCR01T        TAG
     C*
     C*  Validate the Entered "FROM fund"  *IN71
     C*
     C     SCFFND        IFNE      *ZEROS
     C                   MOVE      SCFFND        XXFND
     C     FNKY          CHAIN     PFN01                              90
     C     *IN90         CABEQ     *ON           STRT                     71
     C                   ENDIF
     C*
     C*  Validate the Entered "TO fund"  *IN72
     C*
     C     SCTFND        IFNE      *ZEROS
     C                   MOVE      SCTFND        XXFND
     C     FNKY          CHAIN     PFN01                              90
     C     *IN90         CABEQ     *ON           STRT                     72
     C                   ENDIF
     C*
     C*  Validate FROM fund < "TO fund"  *IN73
     C*
     C     SCFFND        CABGT     SCTFND        STRT                 73
     C*
     C*  Validate the Entered "FROM DATE"
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         CABEQ     *ON           STRT                     20
     C*
     C*  Validate the Entered "TO DATE"
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         CABEQ     *ON           STRT                     21
     C*
     C*  Validate the Entered Dates to Verify Ascending Order
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7FRDT            7 0
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7TODT            7 0
     C*
     C     X7FRDT        CABGT     X7TODT        STRT                 22
     C*
     C*_______________________________________________________________
     C*
     C*  Call the CLP to Submit the Program for creating tape workfiles
     C*
     C                   MOVE      XXCO#         ##CO#
     C                   MOVE      SCFRDT        ##FRDT
     C                   MOVE      SCTODT        ##TODT
     C                   MOVE      SCFFND        ##FFND
     C                   MOVE      SCTFND        ##TFND
      *
     C                   IF        SCFFND = 0
     C                   MOVE      1             ##FFND
     C                   ENDIF
      *
     C                   IF        SCTFND = 0
     C                   MOVE      999           ##TFND
     C                   ENDIF
      *
     C                   CALL      'W7044C'                             99
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FFND            3
     C                   PARM                    ##TFND            3
     C                   PARM                    ##FRDT            8
     C                   PARM                    ##TODT            8
     C*_______________________________________________________________
     C*
     C*  End the Program
     C*
     C     TERM          TAG
     C*
     C                   MOVE      *ON           *INLR
