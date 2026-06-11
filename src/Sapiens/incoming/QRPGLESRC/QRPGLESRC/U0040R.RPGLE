     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE U0040R - PRE-CERTIFICATION LETTER REQUEST PROGRAM
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    PROGRAM TO PROCESS THE REQUEST OF PRE-CERTIFICATION LETTERS*
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    PRE-CERTIFICATION MASTER MAINTENANCE                       *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    SCO10R         DARTS                                       *
     H*                                                               *
     H*  PROGRAMMER: KEN RUTHVEN        PROJECT: 99AAA                *
     H*  CREATED: 09/03/92                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  11/02/09  C:232 PROJECT: 139WMR                              *
     H*                  OFAC Reporting-Add Pending Status Indicator. *
     H*                                                               *
     H*  10/07/98  C:94  PROJECT: 87MFX                               *
     H*                  CHANGE F12=PREVIOUS TO F12=CANCEL            *
     H*                                                               *
     H*  10/14/93   C:20    REQ/PROJ: URMNT                           *
     H*                     1. ADD F12 TO OTHER INPUT SCREEN          *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H* SCR01:                                                        *
     H*   12           EXTENSION NUMBER IS PRESENT                    *
     H*   21-59        INVALID SELECTION FIELD                        *
     H*   60           GENERAL CHAIN                                  *
     H*   61-96        INVALID SELECTION FIELD                        *
     H*****************************************************************
     H*
     FU0040F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FUMPCL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FFMPDL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FUMLCL     IF   E           K DISK
     FUALCP     O    E           K DISK    USROPN
     F                                     RENAME(PLC01:PLC02)
     FSMUPL     IF   E           K DISK
     FSTABL     IF   E           K DISK
     F*

     D CK1A            S             78    DIM(1) CTDATA PERRCD(1)              CMD KEYS FOR SCR01
     D QCA             S             56    DIM(3) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCSUB#                 4      8  0
     D  QCFND                  9     11  0
     D  QCCERT                12     26
     D                 DS
     D  XXLTR                  1     10
     D  XXINT                  1      1
     D  XXLTR#                 2      3
     D  XXJOB#                 4      7  0
     D  XXSTM                  8     10  0
     D                 DS
     D  @@CMD                  1     56
     D  @@MBR                 38     47
     D                 DS
     D  @XCMD                  1     56
     D  @XMBR                 24     33
     D                 DS
     D  LT1                    1      6
     D                                     DIM(6)                               LTR 1 FIELDS
     D  SCPR01                 1      1
     D  SCCM01                 2      2
     D  SCAT01                 3      3
     D  SCSB01                 4      4
     D  SCEM01                 5      5
     D  SCOT01                 6      6
     D                 DS
     D  LT2                    1      6
     D                                     DIM(6)                               LTR 2 FIELDS
     D  SCPR02                 1      1
     D  SCCM02                 2      2
     D  SCAT02                 3      3
     D  SCSB02                 4      4
     D  SCEM02                 5      5
     D  SCOT02                 6      6
     D                 DS
     D  LT3                    1      6
     D                                     DIM(6)                               LTR 3 FIELDS
     D  SCPR03                 1      1
     D  SCCM03                 2      2
     D  SCAT03                 3      3
     D  SCSB03                 4      4
     D  SCEM03                 5      5
     D  SCOT03                 6      6
     D                 DS
     D  LT4                    1      6
     D                                     DIM(6)                               LTR 4 FIELDS
     D  SCPR04                 1      1
     D  SCCM04                 2      2
     D  SCAT04                 3      3
     D  SCSB04                 4      4
     D  SCEM04                 5      5
     D  SCOT04                 6      6
     D                 DS
     D  LT5                    1      6
     D                                     DIM(6)                               LTR 5 FIELDS
     D  SCPR05                 1      1
     D  SCCM05                 2      2
     D  SCAT05                 3      3
     D  SCSB05                 4      4
     D  SCEM05                 5      5
     D  SCOT05                 6      6
     D                 DS
     D  LT6                    1      6
     D                                     DIM(6)                               LTR 6 FIELDS
     D  SCPR06                 1      1
     D  SCCM06                 2      2
     D  SCAT06                 3      3
     D  SCSB06                 4      4
     D  SCEM06                 5      5
     D  SCOT06                 6      6
     D                 DS
     D  LT7                    1      6
     D                                     DIM(6)                               LTR 7 FIELDS
     D  SCPR07                 1      1
     D  SCCM07                 2      2
     D  SCAT07                 3      3
     D  SCSB07                 4      4
     D  SCEM07                 5      5
     D  SCOT07                 6      6
     D                 DS
     D  LT8                    1      6
     D                                     DIM(6)                               LTR 8 FIELDS
     D  SCPR08                 1      1
     D  SCCM08                 2      2
     D  SCAT08                 3      3
     D  SCSB08                 4      4
     D  SCEM08                 5      5
     D  SCOT08                 6      6
     D                 DS
     D  LT9                    1      6
     D                                     DIM(6)                               LTR 9 FIELDS
     D  SCPR09                 1      1
     D  SCCM09                 2      2
     D  SCAT09                 3      3
     D  SCSB09                 4      4
     D  SCEM09                 5      5
     D  SCOT09                 6      6
     D                 DS
     D  LT10                   1      6
     D                                     DIM(6)                               LTR 10 FIELDS
     D  SCPR10                 1      1
     D  SCCM10                 2      2
     D  SCAT10                 3      3
     D  SCSB10                 4      4
     D  SCEM10                 5      5
     D  SCOT10                 6      6
     D                 DS
     D  LT11                   1      6
     D                                     DIM(6)                               LTR 11 FIELDS
     D  SCPR11                 1      1
     D  SCCM11                 2      2
     D  SCAT11                 3      3
     D  SCSB11                 4      4
     D  SCEM11                 5      5
     D  SCOT11                 6      6
     D                 DS
     D  LT12                   1      6
     D                                     DIM(6)                               LTR 12 FIELDS
     D  SCPR12                 1      1
     D  SCCM12                 2      2
     D  SCAT12                 3      3
     D  SCSB12                 4      4
     D  SCEM12                 5      5
     D  SCOT12                 6      6
     D*
     D*
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     IPLC01
     I              LCSRC10                     LCSR10
     I              LCSRC11                     LCSR11
     I              LCSRC12                     LCSR12
     I              LCLT10                      XCLT10
     I              LCLT11                      XCLT11
     I              LCLT12                      XCLT12

     C*
     C*   KEY LIST FOR THE PRE-CERTIFICATION MASTER FILE
     C*
     C     PCKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCERT
     C*
     C     PCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCERT
     C                   KFLD                    XXEXT
     C*
     C*   KEY LIST FOR THE CARRIER FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  PAYEE DEMOGRAPHIC RECORD
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*   KEY LIST FOR THE SUBSCRIBER
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*   KEY LIST FOR CLAIMANT DEMOGRAPHICE FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C*   KEY LIST FOR THE LETTER FILE
     C*
     C     LCKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  STATE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    STKEY
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQCERT           15
     C                   PARM                    QQEXT             3
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
     C* INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    CMFND         XXFND                          3.0N
     C     *LIKE         DEFINE    CMFYR         XXFYR                          3.0N
     C     *LIKE         DEFINE    CMCASE        XXCASE                         7.0N
     C     *LIKE         DEFINE    CMSUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    PDFEIN        XXFEIN                         9.0N
     C     *LIKE         DEFINE    PDMOD         XXMOD                          3.0N
     C     *LIKE         DEFINE    CMSS#         XXSS#                          9.0N
     C     *LIKE         DEFINE    PCCERT        XXCERT                         15A
     C     *LIKE         DEFINE    PCEXT         XXEXT                          3.0N
     C*
     C                   MOVE      *BLANKS       XXLT01            1
     C                   MOVE      *BLANKS       XXLT02            1
     C                   MOVE      *BLANKS       XXLT03            1
     C                   MOVE      *BLANKS       XXLT04            1
     C                   MOVE      *BLANKS       XXLT05            1
     C                   MOVE      *BLANKS       XXLT06            1
     C                   MOVE      *BLANKS       XXLT07            1
     C                   MOVE      *BLANKS       XXLT08            1
     C                   MOVE      *BLANKS       XXLT09            1
     C                   MOVE      *BLANKS       XXLT10            1
     C                   MOVE      *BLANKS       XXLT11            1
     C                   MOVE      *BLANKS       XXLT12            1
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQCERT        SCCERT
     C                   MOVE      QQCERT        XXCERT
     C                   MOVE      QQEXT         SCEXT
     C*
     C     SCEXT         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN12
     C                   ELSE                                                   SCEXT <> 0
     C                   MOVE      '0'           *IN12
     C                   MOVE      QQEXT         XXEXT
     C                   END                                                    END SCEXT
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE SUBSCRIBER RECORD
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C                   MOVE      XXSUB#        SCSUB#
     C                   MOVEL     SMNAME        SCSBNM
     C*
     C*  GET THE FUND RECORD
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      XXFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C* GET THE CERTIFICATION RECORD
     C*
     C     *IN12         IFEQ      '1'
     C     PCKEY         CHAIN     PPC01                              60        *IN12 = 0
     C                   ELSE
     C     PCKY1         CHAIN     PPC01                              60
     C                   END                                                    END *IN12
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     PCCASE        XXCASE
     C                   Z-ADD     PCFYR         XXFYR
     C                   Z-ADD     PCFEIN        XXFEIN
     C                   Z-ADD     PCMOD         XXMOD
     C                   END                                                    END *IN60
     C*
     C* GET CLAIMANT NAME
     C*
     C     CMKY1         CHAIN     PCM02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMSS#         XXSS#
     C                   MOVE      CMSS#         SCSS#
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST           56
     C     CDFSN         IFNE      *BLANK
     C                   MOVEL     CDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        SCCNAM
     C                   Z-ADD     XXCASE        SCCASE
     C                   Z-ADD     XXFYR         SCYR
     C                   END                                                    END CDFSN
     C                   END                                                    END *IN60
     C                   END                                                    END *IN60
     C*
     C* GET PROVIDER NAME
     C*
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*** BEG CHG *** 11/02/09 **************************************
     C****       PDSTAT    ANDNE'I'
     C     PDSTAT        ANDEQ     'A'
     C     *IN60         OREQ      '0'
     C     PDSTAT        ANDEQ     *BLANKS
     C*** END CHG *** 11/02/09 **************************************
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        SCPRNM
     C                   ELSE
     C                   MOVEL     PDNAME        SCPRNM
     C                   END                                                    END CDFSN
     C                   ELSE                                                   *IN60 = 1
     C                   MOVE      *BLANKS       SCPRNM
     C                   END                                                    END *IN60
     C                   Z-ADD     XXFEIN        SCFEIN
     C                   Z-ADD     XXMOD         SCMOD
     C*
     C* FILL SCREEN WITH LETTER INFORMATION
     C*
     C     LCKEY         CHAIN     PLC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     LCNM01        SCNM01
     C                   MOVEL     LCNM02        SCNM02
     C                   MOVEL     LCNM03        SCNM03
     C                   MOVEL     LCNM04        SCNM04
     C                   MOVEL     LCNM05        SCNM05
     C                   MOVEL     LCNM06        SCNM06
     C                   MOVEL     LCNM07        SCNM07
     C                   MOVEL     LCNM08        SCNM08
     C                   MOVEL     LCNM09        SCNM09
     C                   MOVEL     LCNM10        SCNM10
     C                   MOVEL     LCNM11        SCNM11
     C                   MOVEL     LCNM12        SCNM12
     C     LCSRC1        IFNE      *BLANKS
     C                   MOVE      'Y'           SCAI01
     C                   END
     C     LCSRC2        IFNE      *BLANKS
     C                   MOVE      'Y'           SCAI02
     C                   END
     C     LCSRC3        IFNE      *BLANKS
     C                   MOVE      'Y'           SCAI03
     C                   END
     C     LCSRC4        IFNE      *BLANKS
     C                   MOVE      'Y'           SCAI04
     C                   END
     C     LCSRC5        IFNE      *BLANKS
     C                   MOVE      'Y'           SCAI05
     C                   END
     C     LCSRC6        IFNE      *BLANKS
     C                   MOVE      'Y'           SCAI06
     C                   END
     C     LCSRC7        IFNE      *BLANKS
     C                   MOVE      'Y'           SCAI07
     C                   END
     C     LCSRC8        IFNE      *BLANKS
     C                   MOVE      'Y'           SCAI08
     C                   END
     C     LCSRC9        IFNE      *BLANKS
     C                   MOVE      'Y'           SCAI09
     C                   END
     C     LCSR10        IFNE      *BLANKS
     C                   MOVE      'Y'           SCAI10
     C                   END
     C     LCSR11        IFNE      *BLANKS
     C                   MOVE      'Y'           SCAI11
     C                   END
     C     LCSR12        IFNE      *BLANKS
     C                   MOVE      'Y'           SCAI12
     C                   END
     C*
     C                   END                                                    END *IN60
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     1             CK                5 0
     C                   MOVEL     CK1A(CK)      SCCMD1
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END                                                    END SCR01
     C*
     C*****************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C***********************************
     C*
     C***********************************
     C*   F3 = EXIT                     *
     C***********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C***********************************
     C*  F12 = PREVIOUS                 *
     C***********************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END SCR02
     C                   END                                                    ENDIF *INKL
     C***************************************
     C*  F14 = DARTS                        *
     C***************************************
     C     *INKN         IFEQ      '1'
     C***********************************
     C*  SCR01                          *
     C***********************************
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C* RETAIN SCREEN NUMBER
     C                   MOVE      Q2RECF        HLRECF           10
     C*
     C* ESTABLISH KEY
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      SCSUB#        QCSUB#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCCERT        QCCERT
     C*
     C*** CALL DARTS ENTRY PROGRAM
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM                    Q1PGM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVE      HLRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END SCR01
     C                   END                                                    END *INKN
     C*
     C                   END                                                    ENDIF *IN11
     C*****************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C**********************************************
     C*   PROCESS REQUEST SCREEN - SCR01           *
     C**********************************************
     C     SCR01T        TAG
     C*
     C* VALIDATE SELECTION FIELD TO BE BLANK OR '1' ON ALL LETTERS
     C*
     C                   DO        6             Y                 2 0
     C     Y             ADD       20            Y1                2 0
     C     LT1(Y)        IFNE      *BLANK
     C     LT1(Y)        ANDNE     '1'
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   DO        6             Y                 2 0
     C     Y             ADD       26            Y1                2 0
     C     LT2(Y)        IFNE      *BLANK
     C     LT2(Y)        ANDNE     '1'
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   DO        6             Y                 2 0
     C     Y             ADD       32            Y1                2 0
     C     LT3(Y)        IFNE      *BLANK
     C     LT3(Y)        ANDNE     '1'
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   DO        6             Y                 2 0
     C     Y             ADD       41            Y1                2 0
     C     LT4(Y)        IFNE      *BLANK
     C     LT4(Y)        ANDNE     '1'
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   DO        6             Y                 2 0
     C     Y             ADD       47            Y1                2 0
     C     LT5(Y)        IFNE      *BLANK
     C     LT5(Y)        ANDNE     '1'
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   DO        6             Y                 2 0
     C     Y             ADD       53            Y1                2 0
     C     LT6(Y)        IFNE      *BLANK
     C     LT6(Y)        ANDNE     '1'
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   DO        6             Y                 2 0
     C     Y             ADD       60            Y1                2 0
     C     LT7(Y)        IFNE      *BLANK
     C     LT7(Y)        ANDNE     '1'
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   DO        6             Y                 2 0
     C     Y             ADD       66            Y1                2 0
     C     LT8(Y)        IFNE      *BLANK
     C     LT8(Y)        ANDNE     '1'
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   DO        6             Y                 2 0
     C     Y             ADD       72            Y1                2 0
     C     LT9(Y)        IFNE      *BLANK
     C     LT9(Y)        ANDNE     '1'
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   DO        6             Y                 2 0
     C     Y             ADD       78            Y1                2 0
     C     LT10(Y)       IFNE      *BLANK
     C     LT10(Y)       ANDNE     '1'
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   DO        6             Y                 2 0
     C     Y             ADD       84            Y1                2 0
     C     LT11(Y)       IFNE      *BLANK
     C     LT11(Y)       ANDNE     '1'
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   DO        6             Y                 2 0
     C     Y             ADD       90            Y1                2 0
     C     LT12(Y)       IFNE      *BLANK
     C     LT12(Y)       ANDNE     '1'
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* CHECK FOR SELECTED LETTER REQUESTS - LETTER # 1
     C*
     C     SCPR01        IFEQ      '1'
     C     SCCM01        OREQ      '1'
     C     SCAT01        OREQ      '1'
     C     SCSB01        OREQ      '1'
     C     SCEM01        OREQ      '1'
     C     SCOT01        OREQ      '1'
     C                   MOVE      '01'          XXLTR#
     C                   MOVE      'Y'           XXLT01
     C     SCOT01        IFEQ      '1'
     C                   MOVE      *BLANKS       SCLTR#
     C                   MOVEL     'SCRL01'      SCLTR#
     C                   EXSR      WDENT
     C                   END                                                    END SCOT01
     C                   EXSR      CRTMBR
     C                   EXSR      WRTMBR
     C                   MOVE      *BLANKS       XXLT01
     C                   MOVE      *BLANKS       QQMBR            10
     C                   MOVEL     XXLTR         QQMBR
     C                   CALL      'U0040C'
     C                   PARM                    QQMBR
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C                   END
     C*
     C* CHECK FOR SELECTED LETTER REQUESTS - LETTER # 2
     C*
     C     SCPR02        IFEQ      '1'
     C     SCCM02        OREQ      '1'
     C     SCAT02        OREQ      '1'
     C     SCSB02        OREQ      '1'
     C     SCEM02        OREQ      '1'
     C     SCOT02        OREQ      '1'
     C                   MOVE      '02'          XXLTR#
     C                   MOVE      'Y'           XXLT02
     C     SCOT02        IFEQ      '1'
     C                   MOVE      *BLANKS       SCLTR#
     C                   MOVEL     'SCRL02'      SCLTR#
     C                   EXSR      WDENT
     C                   END                                                    END SCOT01
     C                   EXSR      CRTMBR
     C                   EXSR      WRTMBR
     C                   MOVE      *BLANKS       XXLT02
     C                   MOVE      *BLANKS       QQMBR
     C                   MOVEL     XXLTR         QQMBR
     C                   CALL      'U0040C'
     C                   PARM                    QQMBR
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C                   END
     C*
     C* CHECK FOR SELECTED LETTER REQUESTS - LETTER # 3
     C*
     C     SCPR03        IFEQ      '1'
     C     SCCM03        OREQ      '1'
     C     SCAT03        OREQ      '1'
     C     SCSB03        OREQ      '1'
     C     SCEM03        OREQ      '1'
     C     SCOT03        OREQ      '1'
     C                   MOVE      '03'          XXLTR#
     C                   MOVE      'Y'           XXLT03
     C     SCOT03        IFEQ      '1'
     C                   MOVE      *BLANKS       SCLTR#
     C                   MOVEL     'SCRL03'      SCLTR#
     C                   EXSR      WDENT
     C                   END                                                    END SCOT01
     C                   EXSR      CRTMBR
     C                   EXSR      WRTMBR
     C                   MOVE      *BLANKS       XXLT03
     C                   MOVE      *BLANKS       QQMBR
     C                   MOVEL     XXLTR         QQMBR
     C                   CALL      'U0040C'
     C                   PARM                    QQMBR
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C                   END
     C*
     C* CHECK FOR SELECTED LETTER REQUESTS - LETTER # 4
     C*
     C     SCPR04        IFEQ      '1'
     C     SCCM04        OREQ      '1'
     C     SCAT04        OREQ      '1'
     C     SCSB04        OREQ      '1'
     C     SCEM04        OREQ      '1'
     C     SCOT04        OREQ      '1'
     C                   MOVE      '04'          XXLTR#
     C                   MOVE      'Y'           XXLT04
     C     SCOT04        IFEQ      '1'
     C                   MOVE      *BLANKS       SCLTR#
     C                   MOVEL     'SCRL04'      SCLTR#
     C                   EXSR      WDENT
     C                   END                                                    END SCOT01
     C                   EXSR      CRTMBR
     C                   EXSR      WRTMBR
     C                   MOVE      *BLANKS       XXLT04
     C                   MOVE      *BLANKS       QQMBR
     C                   MOVEL     XXLTR         QQMBR
     C                   CALL      'U0040C'
     C                   PARM                    QQMBR
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C                   END
     C*
     C* CHECK FOR SELECTED LETTER REQUESTS - LETTER # 5
     C*
     C     SCPR05        IFEQ      '1'
     C     SCCM05        OREQ      '1'
     C     SCAT05        OREQ      '1'
     C     SCSB05        OREQ      '1'
     C     SCEM05        OREQ      '1'
     C     SCOT05        OREQ      '1'
     C                   MOVE      '05'          XXLTR#
     C                   MOVE      'Y'           XXLT05
     C     SCOT05        IFEQ      '1'
     C                   MOVE      *BLANKS       SCLTR#
     C                   MOVEL     'SCRL05'      SCLTR#
     C                   EXSR      WDENT
     C                   END                                                    END SCOT01
     C                   EXSR      CRTMBR
     C                   EXSR      WRTMBR
     C                   MOVE      *BLANKS       XXLT05
     C                   MOVE      *BLANKS       QQMBR
     C                   MOVEL     XXLTR         QQMBR
     C                   CALL      'U0040C'
     C                   PARM                    QQMBR
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C                   END
     C*
     C* CHECK FOR SELECTED LETTER REQUESTS - LETTER # 6
     C*
     C     SCPR06        IFEQ      '1'
     C     SCCM06        OREQ      '1'
     C     SCAT06        OREQ      '1'
     C     SCSB06        OREQ      '1'
     C     SCEM06        OREQ      '1'
     C     SCOT06        OREQ      '1'
     C                   MOVE      '06'          XXLTR#
     C                   MOVE      'Y'           XXLT06
     C     SCOT06        IFEQ      '1'
     C                   MOVE      *BLANKS       SCLTR#
     C                   MOVEL     'SCRL06'      SCLTR#
     C                   EXSR      WDENT
     C                   END                                                    END SCOT01
     C                   EXSR      CRTMBR
     C                   EXSR      WRTMBR
     C                   MOVE      *BLANKS       XXLT06
     C                   MOVE      *BLANKS       QQMBR
     C                   MOVEL     XXLTR         QQMBR
     C                   CALL      'U0040C'
     C                   PARM                    QQMBR
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C                   END
     C*
     C* CHECK FOR SELECTED LETTER REQUESTS - LETTER # 7
     C*
     C     SCPR07        IFEQ      '1'
     C     SCCM07        OREQ      '1'
     C     SCAT07        OREQ      '1'
     C     SCSB07        OREQ      '1'
     C     SCEM07        OREQ      '1'
     C     SCOT07        OREQ      '1'
     C                   MOVE      '07'          XXLTR#
     C                   MOVE      'Y'           XXLT07
     C     SCOT07        IFEQ      '1'
     C                   MOVE      *BLANKS       SCLTR#
     C                   MOVEL     'SCRL07'      SCLTR#
     C                   EXSR      WDENT
     C                   END                                                    END SCOT01
     C                   EXSR      CRTMBR
     C                   EXSR      WRTMBR
     C                   MOVE      *BLANKS       XXLT07
     C                   MOVE      *BLANKS       QQMBR
     C                   MOVEL     XXLTR         QQMBR
     C                   CALL      'U0040C'
     C                   PARM                    QQMBR
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C                   END
     C*
     C* CHECK FOR SELECTED LETTER REQUESTS - LETTER # 8
     C*
     C     SCPR08        IFEQ      '1'
     C     SCCM08        OREQ      '1'
     C     SCAT08        OREQ      '1'
     C     SCSB08        OREQ      '1'
     C     SCEM08        OREQ      '1'
     C     SCOT08        OREQ      '1'
     C                   MOVE      '08'          XXLTR#
     C                   MOVE      'Y'           XXLT08
     C     SCOT08        IFEQ      '1'
     C                   MOVE      *BLANKS       SCLTR#
     C                   MOVEL     'SCRL08'      SCLTR#
     C                   EXSR      WDENT
     C                   END                                                    END SCOT01
     C                   EXSR      CRTMBR
     C                   EXSR      WRTMBR
     C                   MOVE      *BLANKS       XXLT08
     C                   MOVE      *BLANKS       QQMBR
     C                   MOVEL     XXLTR         QQMBR
     C                   CALL      'U0040C'
     C                   PARM                    QQMBR
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C                   END
     C*
     C* CHECK FOR SELECTED LETTER REQUESTS - LETTER # 9
     C*
     C     SCPR09        IFEQ      '1'
     C     SCCM09        OREQ      '1'
     C     SCAT09        OREQ      '1'
     C     SCSB09        OREQ      '1'
     C     SCEM09        OREQ      '1'
     C     SCOT09        OREQ      '1'
     C                   MOVE      '09'          XXLTR#
     C                   MOVE      'Y'           XXLT09
     C     SCOT09        IFEQ      '1'
     C                   MOVE      *BLANKS       SCLTR#
     C                   MOVEL     'SCRL09'      SCLTR#
     C                   EXSR      WDENT
     C                   END                                                    END SCOT01
     C                   EXSR      CRTMBR
     C                   MOVEL     LCSRC9        Q2RECF
     C                   EXSR      WDADD
     C                   EXSR      WRTMBR
     C                   MOVE      *BLANKS       XXLT09
     C                   MOVE      *BLANKS       QQMBR
     C                   MOVEL     XXLTR         QQMBR
     C                   CALL      'U0040C'
     C                   PARM                    QQMBR
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C                   END
     C*
     C* CHECK FOR SELECTED LETTER REQUESTS - LETTER # 10
     C*
     C     SCPR10        IFEQ      '1'
     C     SCCM10        OREQ      '1'
     C     SCAT10        OREQ      '1'
     C     SCSB10        OREQ      '1'
     C     SCEM10        OREQ      '1'
     C     SCOT10        OREQ      '1'
     C                   MOVE      '10'          XXLTR#
     C                   MOVE      'Y'           XXLT10
     C     SCOT10        IFEQ      '1'
     C                   MOVE      *BLANKS       SCLTR#
     C                   MOVEL     'SCRL10'      SCLTR#
     C                   EXSR      WDENT
     C                   END                                                    END SCOT01
     C                   EXSR      CRTMBR
     C                   EXSR      WRTMBR
     C                   MOVE      *BLANKS       XXLT10
     C                   MOVE      *BLANKS       QQMBR
     C                   MOVEL     XXLTR         QQMBR
     C                   CALL      'U0040C'
     C                   PARM                    QQMBR
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C                   END
     C*
     C* CHECK FOR SELECTED LETTER REQUESTS - LETTER # 11
     C*
     C     SCPR11        IFEQ      '1'
     C     SCCM11        OREQ      '1'
     C     SCAT11        OREQ      '1'
     C     SCSB11        OREQ      '1'
     C     SCEM11        OREQ      '1'
     C     SCOT11        OREQ      '1'
     C                   MOVE      '11'          XXLTR#
     C                   MOVE      'Y'           XXLT11
     C     SCOT11        IFEQ      '1'
     C                   MOVE      *BLANKS       SCLTR#
     C                   MOVEL     'SCRL11'      SCLTR#
     C                   EXSR      WDENT
     C                   END                                                    END SCOT01
     C                   EXSR      CRTMBR
     C                   EXSR      WRTMBR
     C                   MOVE      *BLANKS       XXLT11
     C                   MOVE      *BLANKS       QQMBR
     C                   MOVEL     XXLTR         QQMBR
     C                   CALL      'U0040C'
     C                   PARM                    QQMBR
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C                   END
     C*
     C* CHECK FOR SELECTED LETTER REQUESTS - LETTER # 12
     C*
     C     SCPR12        IFEQ      '1'
     C     SCCM12        OREQ      '1'
     C     SCAT12        OREQ      '1'
     C     SCSM12        OREQ      '1'
     C     SCEM12        OREQ      '1'
     C     SCOT12        OREQ      '1'
     C                   MOVE      '12'          XXLTR#
     C                   MOVE      'Y'           XXLT12
     C     SCOT12        IFEQ      '1'
     C                   MOVE      *BLANKS       SCLTR#
     C                   MOVEL     'SCRL12'      SCLTR#
     C                   EXSR      WDENT
     C                   END                                                    END SCOT01
     C                   EXSR      CRTMBR
     C                   EXSR      WRTMBR
     C                   MOVE      *BLANKS       XXLT12
     C                   MOVE      *BLANKS       QQMBR
     C                   MOVEL     XXLTR         QQMBR
     C                   CALL      'U0040C'
     C                   PARM                    QQMBR
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C                   END
     C*********************************************
     C*  END THE PROGRAM                          *
     C*********************************************
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*******************************************************************
     C* CTRMBR SUBROUTINE - CREATES MEMBER IN UALCP AND OVERIDES TO IT  *
     C*******************************************************************
     C     CRTMBR        BEGSR
     C*
     C* SETUP MEMBER NAME
     C*
     C                   MOVE      'L'           XXINT
     C                   MOVE      Q1JOBN        XXJOB#
     C                   Z-ADD     DSTIME        XXSTM
     C*
     C*** CLOSE FILE IF OPENED
     C*
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     UALCP
     C                   MOVE      ' '           OPNFLG            1
     C                   END                                                    END OPNFLG=1
     C*
     C*** DELETE OVERRIDE
     C*
     C     OVRFLG        IFEQ      '1'
     C*
     C*** DELETE OVERRIDE TO UALCP
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG            1
     C                   END                                                    END OVRFLG=1
     C*
     C     OVRFLG        IFEQ      ' '
     C*
     C*** ADD MEMBER TO UALCP
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(2)        @XCMD
     C                   MOVEL     XXLTR         @XMBR
     C                   CALL      'QCMDEXC'
     C                   PARM                    @XCMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG            1
     C*
     C*** OVERRIDE TO UALCP/MBR
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(3)        @@CMD
     C                   MOVEL     XXLTR         @@MBR
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG            1
     C                   END                                                    END OVRFLG=0
     C*
     C*** OPEN FILE FOR PROCESSING
     C*
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      UALCP
     C                   MOVE      '1'           OPNFLG
     C                   END                                                    END OPNFLG= ' '
     C*
     C                   ENDSR
     C*******************************************************************
     C* SCCLR SUBROUTINE - CLEAR DATA ENTRY WINDOW                      *
     C*******************************************************************
     C     SCCLR         BEGSR
     C                   MOVE      *BLANKS       SCLSN
     C                   MOVE      *BLANKS       SCFSN
     C                   MOVE      *BLANKS       SCADR1
     C                   MOVE      *BLANKS       SCADR2
     C                   MOVE      *BLANKS       SCCTY
     C                   MOVE      *BLANKS       SCST
     C                   MOVE      *BLANKS       SCZIP1
     C                   MOVE      *BLANKS       SCZIP2
     C                   ENDSR
     C*******************************************************************
     C* WDADD SUBROUTINE - DATA ENTRY SUBROUTINE FOR ADDT INPUT WINDOW  *
     C*******************************************************************
     C     WDADD         BEGSR
     C     ERROR1        TAG
     C     Q2RECF        IFEQ      'SCR09'
     C                   MOVE      *BLANKS       SCLTR#
     C                   MOVEL     'SCRL09'      SCLTR#
     C                   MOVE      *BLANKS       SC1A1
     C                   MOVE      *BLANKS       SC1A2
     C                   MOVE      *BLANKS       SC1A3
     C                   MOVE      *BLANKS       SC60A1
     C                   EXFMT     SCR09
     C                   END
     C     SC1A1         IFEQ      *BLANKS
     C     SC1A2         ANDEQ     *BLANKS
     C     SC1A3         ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN38
     C                   GOTO      ERROR1
     C                   END
     C     SC1A1         IFNE      *BLANKS
     C     SC1A1         ANDNE     '1'
     C                   MOVE      '1'           *IN40
     C                   GOTO      ERROR1
     C                   END
     C     SC1A2         IFNE      *BLANKS
     C     SC1A2         ANDNE     '1'
     C                   MOVE      '1'           *IN41
     C                   GOTO      ERROR1
     C                   END
     C     SC1A3         IFNE      *BLANKS
     C     SC1A3         ANDNE     '1'
     C                   MOVE      '1'           *IN42
     C                   GOTO      ERROR1
     C                   END
     C     SC1A3         IFNE      *BLANKS
     C     SC60A1        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN39
     C                   GOTO      ERROR1
     C                   END
     C                   END
     C     SC60A1        IFNE      *BLANKS
     C     SC1A3         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN43
     C                   GOTO      ERROR1
     C                   END
     C                   END
     C*
     C                   ENDSR
     C*******************************************************************
     C* WDENT SUBROUTINE - DATA ENTRY SUBROUTINE FOR WINDOW             *
     C*******************************************************************
     C     WDENT         BEGSR
     C                   EXSR      SCCLR
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'WDWOTH'      Q2RECF
     C     ERROR         TAG
     C                   EXFMT     WDWOTH
     C     *INKL         IFEQ      '1'
     C*
     C     XXLTR#        IFEQ      '02'
     C                   MOVE      *BLANKS       LT1
     C                   ELSE
     C     XXLTR#        IFEQ      '03'
     C                   MOVE      *BLANKS       LT1
     C                   MOVE      *BLANKS       LT2
     C                   ELSE
     C     XXLTR#        IFEQ      '04'
     C                   MOVE      *BLANKS       LT1
     C                   MOVE      *BLANKS       LT2
     C                   MOVE      *BLANKS       LT3
     C                   ELSE
     C     XXLTR#        IFEQ      '05'
     C                   MOVE      *BLANKS       LT1
     C                   MOVE      *BLANKS       LT2
     C                   MOVE      *BLANKS       LT3
     C                   MOVE      *BLANKS       LT4
     C                   ELSE
     C     XXLTR#        IFEQ      '06'
     C                   MOVE      *BLANKS       LT1
     C                   MOVE      *BLANKS       LT2
     C                   MOVE      *BLANKS       LT3
     C                   MOVE      *BLANKS       LT4
     C                   MOVE      *BLANKS       LT5
     C                   ELSE
     C     XXLTR#        IFEQ      '07'
     C                   MOVE      *BLANKS       LT1
     C                   MOVE      *BLANKS       LT2
     C                   MOVE      *BLANKS       LT3
     C                   MOVE      *BLANKS       LT4
     C                   MOVE      *BLANKS       LT5
     C                   MOVE      *BLANKS       LT6
     C                   ELSE
     C     XXLTR#        IFEQ      '08'
     C                   MOVE      *BLANKS       LT1
     C                   MOVE      *BLANKS       LT2
     C                   MOVE      *BLANKS       LT3
     C                   MOVE      *BLANKS       LT4
     C                   MOVE      *BLANKS       LT5
     C                   MOVE      *BLANKS       LT6
     C                   MOVE      *BLANKS       LT7
     C                   ELSE
     C     XXLTR#        IFEQ      '09'
     C                   MOVE      *BLANKS       LT1
     C                   MOVE      *BLANKS       LT2
     C                   MOVE      *BLANKS       LT3
     C                   MOVE      *BLANKS       LT4
     C                   MOVE      *BLANKS       LT5
     C                   MOVE      *BLANKS       LT6
     C                   MOVE      *BLANKS       LT7
     C                   MOVE      *BLANKS       LT8
     C                   ELSE
     C     XXLTR#        IFEQ      '10'
     C                   MOVE      *BLANKS       LT1
     C                   MOVE      *BLANKS       LT2
     C                   MOVE      *BLANKS       LT3
     C                   MOVE      *BLANKS       LT4
     C                   MOVE      *BLANKS       LT5
     C                   MOVE      *BLANKS       LT6
     C                   MOVE      *BLANKS       LT7
     C                   MOVE      *BLANKS       LT8
     C                   MOVE      *BLANKS       LT9
     C                   ELSE
     C     XXLTR#        IFEQ      '11'
     C                   MOVE      *BLANKS       LT1
     C                   MOVE      *BLANKS       LT2
     C                   MOVE      *BLANKS       LT3
     C                   MOVE      *BLANKS       LT4
     C                   MOVE      *BLANKS       LT5
     C                   MOVE      *BLANKS       LT6
     C                   MOVE      *BLANKS       LT7
     C                   MOVE      *BLANKS       LT8
     C                   MOVE      *BLANKS       LT9
     C                   MOVE      *BLANKS       LT10
     C                   ELSE
     C     XXLTR#        IFEQ      '12'
     C                   MOVE      *BLANKS       LT1
     C                   MOVE      *BLANKS       LT2
     C                   MOVE      *BLANKS       LT3
     C                   MOVE      *BLANKS       LT4
     C                   MOVE      *BLANKS       LT5
     C                   MOVE      *BLANKS       LT6
     C                   MOVE      *BLANKS       LT7
     C                   MOVE      *BLANKS       LT8
     C                   MOVE      *BLANKS       LT9
     C                   MOVE      *BLANKS       LT10
     C                   MOVE      *BLANKS       LT11
     C                   END                                                    END XXLTR# EQ 12
     C                   END                                                    END XXLTR# EQ 11
     C                   END                                                    END XXLTR# EQ 10
     C                   END                                                    END XXLTR# EQ 9
     C                   END                                                    END XXLTR# EQ 8
     C                   END                                                    END XXLTR# EQ 7
     C                   END                                                    END XXLTR# EQ 6
     C                   END                                                    END XXLTR# EQ 5
     C                   END                                                    END XXLTR# EQ 4
     C                   END                                                    END XXLTR# EQ 3
     C                   END                                                    END XXLTR# EQ 2
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C******************************************
     C*   F6 = SEARCH LIST CALL                *
     C******************************************
     C     *INKF         IFEQ      '1'
     C                   CALL      'SY600R'
     C                   PARM                    SCST
     C                   PARM                    QQRTC             1
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'WDWOTH'      Q2RECF
     C     QQRTC         IFEQ      'B'
     C     SCST          OREQ      *BLANKS
     C                   GOTO      ERROR
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   GOTO      ERROR
     C                   END
     C                   END
     C*
     C     SCLSN         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN33
     C                   GOTO      ERROR
     C                   END
     C*
     C     SCADR1        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN34
     C                   GOTO      ERROR
     C                   END
     C*
     C     SCCTY         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN35
     C                   GOTO      ERROR
     C                   END
     C*
     C     SCZIP1        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN36
     C                   GOTO      ERROR
     C                   END
     C*
     C                   MOVE      *BLANKS       STKEY
     C                   MOVEL     SCST          STKEY
     C     TBKEY         CHAIN     PST01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN37
     C                   GOTO      ERROR
     C                   END
     C*
     C                   ENDSR
     C*******************************************************************
     C* WTRMBR SUBROUTINE - WRITES RECORD TO NEWLY CREATED MEMBER       *
     C*******************************************************************
     C     WRTMBR        BEGSR
     C*
     C                   Z-ADD     PCCO#         LCCO#
     C                   Z-ADD     PCSUB#        LCSUB#
     C                   Z-ADD     PCFND         LCFND
     C                   Z-ADD     PCFYR         LCFYR
     C                   Z-ADD     PCCASE        LCCASE
     C                   MOVE      PCCERT        LCCERT
     C                   Z-ADD     PCEXT         LCEXT
     C                   MOVE      XXLT01        LCLT1
     C                   MOVE      XXLT02        LCLT2
     C                   MOVE      XXLT03        LCLT3
     C                   MOVE      XXLT04        LCLT4
     C                   MOVE      XXLT05        LCLT5
     C                   MOVE      XXLT06        LCLT6
     C                   MOVE      XXLT07        LCLT7
     C                   MOVE      XXLT08        LCLT8
     C                   MOVE      XXLT09        LCLT9
     C                   MOVE      XXLT10        LCLT10
     C                   MOVE      XXLT11        LCLT11
     C                   MOVE      XXLT12        LCLT12
     C                   MOVE      SC1A1         LC1A1
     C                   MOVE      SC1A2         LC1A2
     C                   MOVE      SC1A3         LC1A3
     C                   MOVEL     SC60A1        LC60A1
     C                   MOVE      Q1USER        LCIUSR
     C*
     C     XXLT01        IFEQ      'Y'
     C     SCPR01        IFEQ      '1'
     C                   MOVE      'Y'           LCPRFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCPRFL
     C                   END
     C     SCCM01        IFEQ      '1'
     C                   MOVE      'Y'           LCCSFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCCSFL
     C                   END
     C     SCAT01        IFEQ      '1'
     C                   MOVE      'Y'           LCATFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCATFL
     C                   END
     C     SCSB01        IFEQ      '1'
     C                   MOVE      'Y'           LCSUFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCSUFL
     C                   END
     C     SCEM01        IFEQ      '1'
     C                   MOVE      'Y'           LCEMFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCEMFL
     C                   END
     C     SCOT01        IFEQ      '1'
     C                   MOVE      'Y'           LCOTFL
     C                   MOVEL     SCLSN         LCONME
     C                   MOVEL     SCFSN         LCOFSN
     C                   MOVEL     SCADR1        LCOAD1
     C                   MOVEL     SCADR2        LCOAD2
     C                   MOVEL     SCCTY         LCOCTY
     C                   MOVEL     SCST          LCOST
     C                   MOVEL     SCZIP1        LCOZIP
     C                   MOVE      SCZIP2        LCOZIP
     C                   ELSE
     C                   MOVE      *BLANKS       LCOTFL
     C                   END
     C                   END
     C*
     C     XXLT02        IFEQ      'Y'
     C     SCPR02        IFEQ      '1'
     C                   MOVE      'Y'           LCPRFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCPRFL
     C                   END
     C     SCCM02        IFEQ      '1'
     C                   MOVE      'Y'           LCCSFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCCSFL
     C                   END
     C     SCAT02        IFEQ      '1'
     C                   MOVE      'Y'           LCATFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCATFL
     C                   END
     C     SCSB02        IFEQ      '1'
     C                   MOVE      'Y'           LCSUFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCSUFL
     C                   END
     C     SCEM02        IFEQ      '1'
     C                   MOVE      'Y'           LCEMFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCEMFL
     C                   END
     C     SCOT02        IFEQ      '1'
     C                   MOVE      'Y'           LCOTFL
     C                   MOVEL     SCLSN         LCONME
     C                   MOVEL     SCFSN         LCOFSN
     C                   MOVEL     SCADR1        LCOAD1
     C                   MOVEL     SCADR2        LCOAD2
     C                   MOVEL     SCCTY         LCOCTY
     C                   MOVEL     SCST          LCOST
     C                   MOVEL     SCZIP1        LCOZIP
     C                   MOVE      SCZIP2        LCOZIP
     C                   ELSE
     C                   MOVE      *BLANKS       LCOTFL
     C                   END
     C                   END
     C*
     C     XXLT03        IFEQ      'Y'
     C     SCPR03        IFEQ      '1'
     C                   MOVE      'Y'           LCPRFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCPRFL
     C                   END
     C     SCCM03        IFEQ      '1'
     C                   MOVE      'Y'           LCCSFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCCSFL
     C                   END
     C     SCAT03        IFEQ      '1'
     C                   MOVE      'Y'           LCATFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCATFL
     C                   END
     C     SCSB03        IFEQ      '1'
     C                   MOVE      'Y'           LCSUFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCSUFL
     C                   END
     C     SCEM03        IFEQ      '1'
     C                   MOVE      'Y'           LCEMFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCEMFL
     C                   END
     C     SCOT03        IFEQ      '1'
     C                   MOVE      'Y'           LCOTFL
     C                   MOVEL     SCLSN         LCONME
     C                   MOVEL     SCFSN         LCOFSN
     C                   MOVEL     SCADR1        LCOAD1
     C                   MOVEL     SCADR2        LCOAD2
     C                   MOVEL     SCCTY         LCOCTY
     C                   MOVEL     SCST          LCOST
     C                   MOVEL     SCZIP1        LCOZIP
     C                   MOVE      SCZIP2        LCOZIP
     C                   ELSE
     C                   MOVE      *BLANKS       LCOTFL
     C                   END
     C                   END
     C*
     C     XXLT04        IFEQ      'Y'
     C     SCPR04        IFEQ      '1'
     C                   MOVE      'Y'           LCPRFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCPRFL
     C                   END
     C     SCCM04        IFEQ      '1'
     C                   MOVE      'Y'           LCCSFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCCSFL
     C                   END
     C     SCAT04        IFEQ      '1'
     C                   MOVE      'Y'           LCATFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCATFL
     C                   END
     C     SCSB04        IFEQ      '1'
     C                   MOVE      'Y'           LCSUFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCSUFL
     C                   END
     C     SCEM04        IFEQ      '1'
     C                   MOVE      'Y'           LCEMFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCEMFL
     C                   END
     C     SCOT04        IFEQ      '1'
     C                   MOVE      'Y'           LCOTFL
     C                   MOVEL     SCLSN         LCONME
     C                   MOVEL     SCFSN         LCOFSN
     C                   MOVEL     SCADR1        LCOAD1
     C                   MOVEL     SCADR2        LCOAD2
     C                   MOVEL     SCCTY         LCOCTY
     C                   MOVEL     SCST          LCOST
     C                   MOVEL     SCZIP1        LCOZIP
     C                   MOVE      SCZIP2        LCOZIP
     C                   ELSE
     C                   MOVE      *BLANKS       LCOTFL
     C                   END
     C                   END
     C*
     C     XXLT05        IFEQ      'Y'
     C     SCPR05        IFEQ      '1'
     C                   MOVE      'Y'           LCPRFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCPRFL
     C                   END
     C     SCCM05        IFEQ      '1'
     C                   MOVE      'Y'           LCCSFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCCSFL
     C                   END
     C     SCAT05        IFEQ      '1'
     C                   MOVE      'Y'           LCATFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCATFL
     C                   END
     C     SCSB05        IFEQ      '1'
     C                   MOVE      'Y'           LCSUFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCSUFL
     C                   END
     C     SCEM05        IFEQ      '1'
     C                   MOVE      'Y'           LCEMFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCEMFL
     C                   END
     C     SCOT05        IFEQ      '1'
     C                   MOVE      'Y'           LCOTFL
     C                   MOVEL     SCLSN         LCONME
     C                   MOVEL     SCFSN         LCOFSN
     C                   MOVEL     SCADR1        LCOAD1
     C                   MOVEL     SCADR2        LCOAD2
     C                   MOVEL     SCCTY         LCOCTY
     C                   MOVEL     SCST          LCOST
     C                   MOVEL     SCZIP1        LCOZIP
     C                   MOVE      SCZIP2        LCOZIP
     C                   ELSE
     C                   MOVE      *BLANKS       LCOTFL
     C                   END
     C                   END
     C*
     C     XXLT06        IFEQ      'Y'
     C     SCPR06        IFEQ      '1'
     C                   MOVE      'Y'           LCPRFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCPRFL
     C                   END
     C     SCCM06        IFEQ      '1'
     C                   MOVE      'Y'           LCCSFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCCSFL
     C                   END
     C     SCAT06        IFEQ      '1'
     C                   MOVE      'Y'           LCATFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCATFL
     C                   END
     C     SCSB06        IFEQ      '1'
     C                   MOVE      'Y'           LCSUFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCSUFL
     C                   END
     C     SCEM06        IFEQ      '1'
     C                   MOVE      'Y'           LCEMFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCEMFL
     C                   END
     C     SCOT06        IFEQ      '1'
     C                   MOVE      'Y'           LCOTFL
     C                   MOVEL     SCLSN         LCONME
     C                   MOVEL     SCFSN         LCOFSN
     C                   MOVEL     SCADR1        LCOAD1
     C                   MOVEL     SCADR2        LCOAD2
     C                   MOVEL     SCCTY         LCOCTY
     C                   MOVEL     SCST          LCOST
     C                   MOVEL     SCZIP1        LCOZIP
     C                   MOVE      SCZIP2        LCOZIP
     C                   ELSE
     C                   MOVE      *BLANKS       LCOTFL
     C                   END
     C                   END
     C*
     C     XXLT07        IFEQ      'Y'
     C     SCPR07        IFEQ      '1'
     C                   MOVE      'Y'           LCPRFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCPRFL
     C                   END
     C     SCCM07        IFEQ      '1'
     C                   MOVE      'Y'           LCCSFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCCSFL
     C                   END
     C     SCAT07        IFEQ      '1'
     C                   MOVE      'Y'           LCATFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCATFL
     C                   END
     C     SCSB07        IFEQ      '1'
     C                   MOVE      'Y'           LCSUFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCSUFL
     C                   END
     C     SCEM07        IFEQ      '1'
     C                   MOVE      'Y'           LCEMFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCEMFL
     C                   END
     C     SCOT07        IFEQ      '1'
     C                   MOVE      'Y'           LCOTFL
     C                   MOVEL     SCLSN         LCONME
     C                   MOVEL     SCFSN         LCOFSN
     C                   MOVEL     SCADR1        LCOAD1
     C                   MOVEL     SCADR2        LCOAD2
     C                   MOVEL     SCCTY         LCOCTY
     C                   MOVEL     SCST          LCOST
     C                   MOVEL     SCZIP1        LCOZIP
     C                   MOVE      SCZIP2        LCOZIP
     C                   ELSE
     C                   MOVE      *BLANKS       LCOTFL
     C                   END
     C                   END
     C*
     C     XXLT08        IFEQ      'Y'
     C     SCPR08        IFEQ      '1'
     C                   MOVE      'Y'           LCPRFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCPRFL
     C                   END
     C     SCCM08        IFEQ      '1'
     C                   MOVE      'Y'           LCCSFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCCSFL
     C                   END
     C     SCAT08        IFEQ      '1'
     C                   MOVE      'Y'           LCATFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCATFL
     C                   END
     C     SCSB08        IFEQ      '1'
     C                   MOVE      'Y'           LCSUFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCSUFL
     C                   END
     C     SCEM08        IFEQ      '1'
     C                   MOVE      'Y'           LCEMFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCEMFL
     C                   END
     C     SCOT08        IFEQ      '1'
     C                   MOVE      'Y'           LCOTFL
     C                   MOVEL     SCLSN         LCONME
     C                   MOVEL     SCFSN         LCOFSN
     C                   MOVEL     SCADR1        LCOAD1
     C                   MOVEL     SCADR2        LCOAD2
     C                   MOVEL     SCCTY         LCOCTY
     C                   MOVEL     SCST          LCOST
     C                   MOVEL     SCZIP1        LCOZIP
     C                   MOVE      SCZIP2        LCOZIP
     C                   ELSE
     C                   MOVE      *BLANKS       LCOTFL
     C                   END
     C                   END
     C*
     C     XXLT09        IFEQ      'Y'
     C     SCPR09        IFEQ      '1'
     C                   MOVE      'Y'           LCPRFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCPRFL
     C                   END
     C     SCCM09        IFEQ      '1'
     C                   MOVE      'Y'           LCCSFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCCSFL
     C                   END
     C     SCAT09        IFEQ      '1'
     C                   MOVE      'Y'           LCATFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCATFL
     C                   END
     C     SCSB09        IFEQ      '1'
     C                   MOVE      'Y'           LCSUFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCSUFL
     C                   END
     C     SCEM09        IFEQ      '1'
     C                   MOVE      'Y'           LCEMFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCEMFL
     C                   END
     C     SCOT09        IFEQ      '1'
     C                   MOVE      'Y'           LCOTFL
     C                   MOVEL     SCLSN         LCONME
     C                   MOVEL     SCFSN         LCOFSN
     C                   MOVEL     SCADR1        LCOAD1
     C                   MOVEL     SCADR2        LCOAD2
     C                   MOVEL     SCCTY         LCOCTY
     C                   MOVEL     SCST          LCOST
     C                   MOVEL     SCZIP1        LCOZIP
     C                   MOVE      SCZIP2        LCOZIP
     C                   ELSE
     C                   MOVE      *BLANKS       LCOTFL
     C                   END
     C                   END
     C*
     C     XXLT10        IFEQ      'Y'
     C     SCPR10        IFEQ      '1'
     C                   MOVE      'Y'           LCPRFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCPRFL
     C                   END
     C     SCCM10        IFEQ      '1'
     C                   MOVE      'Y'           LCCSFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCCSFL
     C                   END
     C     SCAT10        IFEQ      '1'
     C                   MOVE      'Y'           LCATFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCATFL
     C                   END
     C     SCSB10        IFEQ      '1'
     C                   MOVE      'Y'           LCSUFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCSUFL
     C                   END
     C     SCEM10        IFEQ      '1'
     C                   MOVE      'Y'           LCEMFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCEMFL
     C                   END
     C     SCOT10        IFEQ      '1'
     C                   MOVE      'Y'           LCOTFL
     C                   MOVEL     SCLSN         LCONME
     C                   MOVEL     SCFSN         LCOFSN
     C                   MOVEL     SCADR1        LCOAD1
     C                   MOVEL     SCADR2        LCOAD2
     C                   MOVEL     SCCTY         LCOCTY
     C                   MOVEL     SCST          LCOST
     C                   MOVEL     SCZIP1        LCOZIP
     C                   MOVE      SCZIP2        LCOZIP
     C                   ELSE
     C                   MOVE      *BLANKS       LCOTFL
     C                   END
     C                   END
     C*
     C     XXLT11        IFEQ      'Y'
     C     SCPR11        IFEQ      '1'
     C                   MOVE      'Y'           LCPRFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCPRFL
     C                   END
     C     SCCM11        IFEQ      '1'
     C                   MOVE      'Y'           LCCSFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCCSFL
     C                   END
     C     SCAT11        IFEQ      '1'
     C                   MOVE      'Y'           LCATFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCATFL
     C                   END
     C     SCSB11        IFEQ      '1'
     C                   MOVE      'Y'           LCSUFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCSUFL
     C                   END
     C     SCEM11        IFEQ      '1'
     C                   MOVE      'Y'           LCEMFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCEMFL
     C                   END
     C     SCOT11        IFEQ      '1'
     C                   MOVE      'Y'           LCOTFL
     C                   MOVEL     SCLSN         LCONME
     C                   MOVEL     SCFSN         LCOFSN
     C                   MOVEL     SCADR1        LCOAD1
     C                   MOVEL     SCADR2        LCOAD2
     C                   MOVEL     SCCTY         LCOCTY
     C                   MOVEL     SCST          LCOST
     C                   MOVEL     SCZIP1        LCOZIP
     C                   MOVE      SCZIP2        LCOZIP
     C                   ELSE
     C                   MOVE      *BLANKS       LCOTFL
     C                   END
     C                   END
     C*
     C     XXLT12        IFEQ      'Y'
     C     SCPR12        IFEQ      '1'
     C                   MOVE      'Y'           LCPRFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCPRFL
     C                   END
     C     SCCM12        IFEQ      '1'
     C                   MOVE      'Y'           LCCSFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCCSFL
     C                   END
     C     SCAT12        IFEQ      '1'
     C                   MOVE      'Y'           LCATFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCATFL
     C                   END
     C     SCSB12        IFEQ      '1'
     C                   MOVE      'Y'           LCSUFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCSUFL
     C                   END
     C     SCEM12        IFEQ      '1'
     C                   MOVE      'Y'           LCEMFL
     C                   ELSE
     C                   MOVE      *BLANKS       LCEMFL
     C                   END
     C     SCOT12        IFEQ      '1'
     C                   MOVE      'Y'           LCOTFL
     C                   MOVEL     SCLSN         LCONME
     C                   MOVEL     SCFSN         LCOFSN
     C                   MOVEL     SCADR1        LCOAD1
     C                   MOVEL     SCADR2        LCOAD2
     C                   MOVEL     SCCTY         LCOCTY
     C                   MOVEL     SCST          LCOST
     C                   MOVEL     SCZIP1        LCOZIP
     C                   MOVE      SCZIP2        LCOZIP
     C                   ELSE
     C                   MOVE      *BLANKS       LCOTFL
     C                   END
     C                   END
     C*
     C                   WRITE     PLC02
     C*
     C                   ENDSR
     C*
**  CK1A - COMMAND KEYS for SCR01
F3=Exit                            F12=Cancel
**  QCMDEXC TABLE
DLTOVR FILE(UALCP)
ADDPFM FILE(UALCP) MBR(          )
OVRDBF FILE(UALCP) TOFILE(*FILE) MBR(          )
