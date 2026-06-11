     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE U0020R - C/R REVIEW MAINTENANCE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    PROGRAM TO SET UP SPECIFICATIONS FOR REPORTS OVER DETAIL   *
     H*    CHARGE FILE.                                               *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    UTILIZATION/REVIEW MENU                                    *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    F0150R         DIAGNOSIS CODE SEARCH                       *
     H*    F0508R         GROUP NUMBER SEARCH                         *
     H*    F0520R         PROCEDURE CODE SEARCH                       *
     H*    F0527R         R & C CODE SEARCH                           *
     H*    F0532R         PROVIDER ALPHA SEARCH                       *
     H*    F0545R         SUBSCRIBER SEARCH                           *
     H*    F2141R         SPECIALTY CODE TABLE FIEL SEARCH            *
     H*    F2155R         PLACE OF SERVICE CODE SEARCH                *
     H*    SRDCK          VALIDATE DATE                               *
     H*    U0520R         EDIT CODE SEARCH                            *
     H*    U0521R         EDIT TYPE SEARCH                            *
     H*                                                               *
     H*  PROGRAMMER: KEN RUTHVEN        PROJECT: 99AAA                *
     H*  CREATED: 07/10/92                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  11/02/09 C:232 PROJECT: 139WMR                               *
     H*                 OFAC Reporting-Add Pending Status Indicator.  *
     H*                                                               *
     H*  11/05/98 C:53  PROJECT: MC145
     H*                 RE-INSTATE CALL TO U0521R FOR SEARCH
     H*                 CORRECT F24 NOT WORKING
     H*
     H*  10/07/98 C:94  PROJECT: 87MFX
     H*                 CHANGE F12=PREVIOUS TO F12=CANCEL
     H*
     H*  03/25/98 C:53  Project: MC145
     H*                 SWAP F0533R/31R TO F0032R (NEW VERSION)
     H*                                                        *
     H*  03/18/93   C:20    PROJ: 99AAA                               *
     H*                     1. ADDED CODE TO VALIDATE EDIT CODE IF    *
     H*                        EXTENSION IS ENTERED.                  *
     H*                                                               *
     H*  04/26/93  C:20   Proj:FS098                                  *
     H*                   ADDED PARM XXST TO CALL TO F0520R           *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H* SCR01:                                                        *
     H*  20        SCEDIT EQUALS BLANKS                               *
     H*  21        EDIT TYPE IS EQUAL TO BLANKS                       *
     H*  22        EXISTING EDIT CODE WITH EDIT TYPE                  *
     H*  23        INVALID EDIT CODE ENTERED                          *
     H*  25        INVALID SUBSCRIBER NUMBER                          *
     H*  26        INVALID FUND NUMBER                                *
     H*  27        ADD IS OF EXTENSION TYPE                           *
     H*  28        POSITION CURSOR EDIT SEARCH                        *
     H*  37        POSITION CURSOR @ SUB# IF NO SELECT ON SEARCH      *
     H*  38        POSITION CURSOR AFTER SUBSCRIBER SEARCH            *
     H*  39        INVALID EDIT TYPE                                  *
     H*  40        POSIT CURSOR @ SCETYP IF NO SELECTION ON SEARCH    *
     H* SCR02:                                                        *
     H*  29        SCDESC IS EQUAL TO BLANKS                          *
     H*  30        INVALID PLACE OF SERVICE CODE                      *
     H*  31        PROV SEARCH NONE SELECTED PLACE CURSOR ON PROV     *
     H*  32        INVALID LOST TIME FLAG                             *
     H*  33        POSITION CURSOR AFTER PROVIDER SEARCH              *
     H*  35        POSITION CURSOR AFTER PLACE OF SERVICE SEARCH      *
     H*  36        POSITION CURSOR @ DIAGNISIS IF NO SELECT ON SEARCH *
     H*  39        POSITION CURSOR @ POS IF NO SELECT ON SEARCH       *
     H*  41        INVALID PROVIDER                                   *
     H*  42        POSITION CURSOR @ SPEC CODE IF NO SELECT ON SEARCH *
     H*            POSITION CURSOR AFTER PROV SRCH W/NO SPEC CODE     *
     H*  43        POSITION CURSOR AFTER SPEC CODE SEARCH             *
     H*  50        INVALID SPECIALTY CODE                             *
     H*  70-74     POSITION CURSOR AFTER DIAGNOSIS CODE SEARCH        *
     H*  70        POSITION CURSOR @ DIAGNISIS IF NO SELECT ON SEARCH *
     H*  70        POSITION CURSOR AFTER SPEC CODE SEARCH             *
     H*  70        POSITION CURSOR AFTER PROV SRCH W/SPEC CODE SEL    *
     H*  86        PROTECT ALL HEADINGS EXT TYPE UPDATE               *
     H*  92        INVALID DIAGNOSIS CODE                             *
     H*  93-97     POSITION CURSOR @ DIAGNOSIS CODE THAT IS INVALID   *
     H* SCR03:                                                        *
     H*  31-38     POSITION CURSOR @ PCODE IF NO SELECT ON SEARCH     *
     H*  76        INVALID PROCEDURE CODE                             *
     H*  77-84     POSITION CURSOR ON INVALID PROCEDURE CODE          *
     H*  85        INCLUDE - EXCLUDE FLAG IS INVALID                  *
     H*  98        VALID ADJ STATE INSTALLED(R&C OR PROC SEARCH)      *
     H*****************************************************************
     H*
     FU0020F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FUMCRL     UF A E           K DISK
     FSMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMDIL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMPRL     IF   E           K DISK    USROPN
     FFMRCL1    IF   E           K DISK
     FSSYLF     IF   E           K DISK
     FFTABL     IF   E           K DISK
     F                                     RENAME(PRC01:PRC0W)
     FFMSML     IF   E           K DISK
      **** BEGIN ADD **** 05/05/98 ************************************
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
      **** END   ADD **** 05/05/98 ************************************
     F*
      **** BEGIN DEL **** 05/05/98 ************************************

     D******************* CK1A    1   2 78               CMD KEYS FOR SCR01
      **** END   DEL **** 05/05/98 ************************************
     D CK2A            S             78    DIM(3) CTDATA PERRCD(1)              CMD KEYS FOR SCR02
     D CK3A            S             78    DIM(1) CTDATA PERRCD(1)              CMD KEYS FOR SCR03
     D CK3B            S             78    DIM(1) CTDATA PERRCD(1)              CMD KEYS FOR SCR03 9
     D UA              S             10    DIM(2) CTDATA PERRCD(1)              MAINTENENCE MODE
     D PSD             S             50    DIM(14) CTDATA PERRCD(1)             PLACE OF SERVICE DES
     D PS              S              1    DIM(14) CTDATA PERRCD(14)            PLACE OF SERVICE COD
     D QCA             S             56    DIM(2) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D EDTP            S              2    DIM(21) CTDATA PERRCD(21)            EDIT TYPE CODES
     D*
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  @@CMD                  1     56
     D  @@ST                  38     39
     D*
     D                 DS
     D  DC                     1     25
     D                                     DIM(5)                               DIAG CODES - SCRN
     D  SCDIA1                 1      5
     D  SCD1                   1      1
     D  SCDIA2                 6     10
     D  SCD2                   6      6
     D  SCDIA3                11     15
     D  SCD3                  11     11
     D  SCDIA4                16     20
     D  SCD4                  16     16
     D  SCDIA5                21     25
     D  SCD5                  21     21
     D*
     D                 DS
     D  DS                     1    250
     D                                     DIM(5)                               DIAG CODES - DESC
     D  SCDGD1                 1     50
     D  SCDGD2                51    100
     D  SCDGD3               101    150
     D  SCDGD4               151    200
     D  SCDGD5               201    250
     D*
     D                 DS
     D  CP                     1     40
     D                                     DIM(8)                               PROCEDURE CODE
     D  SCCPT1                 1      5
     D  SCCPT2                 6     10
     D  SCCPT3                11     15
     D  SCCPT4                16     20
     D  SCCPT5                21     25
     D  SCCPT6                26     30
     D  SCCPT7                31     35
     D  SCCPT8                36     40
     D                 DS
     D  CDP                    1    400
     D                                     DIM(8)                               PROCEDURE CODE DESC
     D  SCDP41                 1     50
     D  SCDP42                51    100
     D  SCDP43               101    150
     D  SCDP44               151    200
     D  SCDP45               201    250
     D  SCDP46               251    300
     D  SCDP47               301    350
     D  SCDP48               351    400
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     IPRC0W
     I              RCKEY                       RCKEYW
     ISRC01
     I              RCKEY                       RCKEY1
     ISCS01
     I              CSKEY                       CSKEY1

     C*
     C*   KEY LIST FOR THE RETOSPECTIVE/CONCURRENT EDIT FILE
     C*
     C     CRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEDIT
     C*
     C     CRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEDIT
     C                   KFLD                    XXEXT
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE CARRIER FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*  DIAGNOSIS CODE FILE
     C*
     C     DIKEY         KLIST
     C                   KFLD                    XXICD9            5
     C*
     C*  PAYEE DEMOGRAPHIC RECORD
     C*
     C     PDKEY         KLIST
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*
     C*  REASONABLE AND CUSTOMARY FILE
     C*
     C     RCKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCPCD
     C*
     C*  SPECIALTY CODE TABLE FILE
     C*
     C     SPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    TABKEY           10
     C*
     C*   KEY LIST FOR THE SUBSCRIBER
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C*
     C*  FS BASE CONTROL FILE
     C*
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
     C*
     C*  PROCEDURE CODE FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXCPCD
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
     C* INITIALIZE WORK FIELDS
     C     *LIKE         DEFINE    CREDIT        XXEDIT                         5A
     C     *LIKE         DEFINE    CRSUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    CREXT         XXEXT                          3.0N
     C     *LIKE         DEFINE    PDST          XXST                           2A
     C     *LIKE         DEFINE    CRCPT1        XXCPCD                         5A
     C     *LIKE         DEFINE    CRDESC        XHDESC                         50A
     C     *LIKE         DEFINE    CRFEIN        XHFEIN                         9.0N
     C     *LIKE         DEFINE    CRMOD         XHMOD                          3.0N
     C     *LIKE         DEFINE    CRPOS         XHPOS                          1A
     C     *LIKE         DEFINE    CRLTFL        XHLTFL                         1A
     C     *LIKE         DEFINE    CRSPCD        XHSPCD                         1A
     C*
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END                                                    END UPKEY
     C                   MOVE      UPCO#         XXCO#             3 0
     C                   MOVE      UPFND         XXFND             3 0
     C                   MOVE      UPSUB#        XXSUB#
     C                   Z-ADD     UPRAL         XXRAL            11 2
     C                   Z-ADD     UPFYR         XUFYR             3 0
     C                   Z-ADD     UPFYR         XXFYR             3 0
      **** BEGIN CHG **** 05/06/98 ************************************
     C******************** MOVE '1'       *IN28
     C                   MOVE      '1'           *IN37
      **** END   CHG **** 05/06/98 ************************************
     C*
     C** WE MUST CHAIN OUT TO THE SYSTEM CONTROL FILE HERE TO SEE IF
     C** A VALID ADJUDICATION STATE IS INSTALLED.  IF WE FIND A STATE
     C** IN FBST01 WE WILL USE THAT STATE'S FILE FOR THE PROCEDURE
     C** CODE SEARCH.  IF WE DO NOT FIND A STATE IN THE CONTROL FILE
     C** WE WILL USE THE REASONABLE AND CUSTOMARY SEARCH.  VERIFICATION
     C** OF ENTERED CODES WILL BE HANDLED THE SAME WAY.
     C*
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'FB'          XSKEY
     C     FBKY          CHAIN     SFB01                              60
     C     *IN60         IFEQ      '0'
     C     FBST01        IFNE      *BLANKS
     C                   MOVE      '1'           *IN98
     C                   MOVE      FBST01        XXST
     C                   END                                                    END FBST01
     C                   END                                                    END *IN60
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
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVEL     SMNAME        SCSBNM
     C*
     C*  GET THE FUND RECORD
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      UPFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     1             CK
      **** BEGIN DEL **** 05/05/98 ************************************
     C******************** MOVELCK1A,CK   SCCMD1
      **** END   DEL **** 05/05/98 ************************************
     C                   MOVEL     CK2A(CK)      SCCMD2
     C   98              MOVEL     CK3A(CK)      SCCMD3
     C  N98              MOVEL     CK3B(CK)      SCCMD3
     C                   EXSR      INIT
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END                                                    END SCR01
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END                                                    END SCR02
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END                                                    END SCR02
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
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
      **** BEGIN CHG **** 05/05/98 ************************************
      *
     C**********************************
     C*      F10 = SEARCH              *
     C**********************************
      *
     C     *INKJ         IFEQ      '1'
      *
     C**********************************
     C***********   F6                           *
     C**********************************
     C********** *INKF     IFEQ '1'
     C**********************************
     C**********   SCR01 = EDIT SEARCH          *
     C**********************************
      *
     C     HFLD          IFEQ      'SCEDIT'
     C     HFLD          OREQ      'SCEXT'
      **** END   CHG **** 05/05/98 ************************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      *ZEROS        QQSUB#
     C                   MOVE      *ZEROS        QQFND
     C                   MOVE      *BLANKS       QQEDIT
     C                   MOVE      *ZEROS        QQEXT
     C                   CALL      'U0520R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQSUB#            5 0
     C                   PARM                    QQFND             3 0
     C                   PARM                    QQEDIT            5
     C                   PARM                    QQEXT             3 0
     C                   PARM                    QQRTCD            1
     C                   MOVE      QQSUB#        SCSUB#
     C                   MOVE      QQFND         SCFND
     C                   MOVE      QQEDIT        SCEDIT
     C                   MOVE      QQEXT         SCEXT
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      '1'           *IN28
     C                   WRITE     SCR99
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END EQUAL B
     C*
     C     QQRTCD        IFEQ      '*'
     C                   WRITE     SCR99
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *
     C                   END                                                    END SCR01
      *
      **** BEGIN ADD **** 05/05/98 ************************************
     C                   ENDIF                                                  HFLD SCEDIT
     C*** BEG ADD *** 11/05/98 ************************************
     C     HFLD          IFEQ      'SCETYP'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      *BLANK        QQEDTP            2
     C                   CALL      'U0521R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQEDTP
     C                   PARM                    QQRTCD            1
     C                   MOVEL     QQEDTP        SCETYP
     C     QQRTCD        CABEQ     'A'           TERM
     C                   WRITE     SCR99
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQEDTP        OREQ      *BLANKS
     C                   MOVE      '1'           *IN40
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    ENDEQ 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END = '*'
     C*
     C                   END                                                    END SCR01
     C                   ENDIF                                                  HFLD SCETYP
     C*** END ADD *** 11/05/98 ************************************
     C************************************
     C*      SUBSCRIBER SEARCH           *
     C************************************
      *
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
     C     *INKL         CABEQ     *ON           STRT                     37
      *
     C     QQRRNS        CHAIN     PSM02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVE      *ON           *IN38
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD SCSUB#
      *
     C************************************
     C*         GROUP SEARCH             *
     C************************************
      *
     C     HFLD          IFEQ      'SCFND'
     C                   MOVE      QQRRNS        QQRRNF
     C                   CALL      'F0060R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQRRNF            9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         CABEQ     *ON           STRT                     38
      *
     C     QQRRNF        CHAIN     PFN02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      FNFND         SCFND
     C                   MOVE      *ON           *IN28
     C                   ELSE
     C                   MOVE      *ON           *IN38
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD SCFND
      *
     C                   MOVE      *ON           *IN90
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  IFEQ *INKJ
      **** END   ADD **** 05/05/98 ************************************
     C**********************************
     C*  SCR02 = DIAGNOSTIC CODE SEARCH*
     C**********************************
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEA     '00000'       *IN(70)
     C                   DO        5             A                 5 0
     C     A             ADD       70            A1                2 0          POSIT CURSOR
     C                   MOVEL     DC(A)         XX2               2
     C     XX2           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        QQDIAG            5
     C                   CALL      'F0510R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQDIAG
     C                   PARM                    QQRTCD            1
     C                   MOVEL     QQDIAG        DC(A)
     C                   MOVE      '1'           *IN(A1)                        POSIT CURSOR
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQDIAG        OREQ      *BLANKS
     C                   MOVE      *BLANKS       DS(A)
     C     A             SUB       1             A1                2 0          POSIT CURSOR
     C                   MOVE      '1'           *IN(A1)
     C                   GOTO      STRT
     C                   END                                                    ENDEQ 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C*
     C* GET DESCRIPTION OF DIAGNOSTIC CODE
     C*
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVEL     DC(A)         XXICD9
     C     DIKEY         SETGT     PDI01
     C     NXTDC3        TAG
     C                   READP     PDI01                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     DIDSC1        DS(A)
     C                   ELSE
     C                   GOTO      NXTDC3
     C                   END                                                    END *IN60
     C*
     C                   WRITE     SCR99
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    ENDIF ?
     C                   END                                                    END DO
     C                   END                                                    ENDEQ '*'
     C                   END                                                    END SCR02
     C************************************
     C*  SCR03 - PROCEDURE CODE SEARCH   *
     C************************************
     C     Q2RECF        IFEQ      'SCR03'
     C     *IN98         IFEQ      '1'
     C*
     C*** CLOSE FILE IF OPENED
     C*
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FMPRL
     C                   MOVE      ' '           OPNFLG            1
     C                   END                                                    END OPNFLG=1
     C*
     C*** DELETE OVERRIDE
     C*
     C     OVRFLG        IFEQ      '1'
     C*
     C*** DELETE OVERRIDE TO FMPRL
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
     C*** OVERRIDE TO FMPRL
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(2)        @@CMD
     C                   MOVEL     XXST          @@ST                           02/21/91 ADD
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG            1
     C                   END                                                    END OVRFLG=0
     C*
     C*** OPEN FILE FOR PROCESSING
     C*
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMPRL
     C                   MOVE      '1'           OPNFLG
     C                   END                                                    END OPNFLG= ' '
     C*
     C                   MOVEA     '00000000'    *IN(30)
     C                   DO        8             A                 5 0
     C     A             ADD       30            A1                2 0          POSIT CURSOR
     C*
     C                   MOVEL     CP(A)         XX2               2
     C     XX2           IFEQ      '? '
     C*
     C                   MOVE      *BLANKS       QQCPT4            5
     C                   MOVE      *ZEROS        QQFDAT            8 0
     C                   CALL      'F0520R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCPT4
     C                   PARM                    QQFDAT
     C                   PARM                    XXST                           ADD 04/26/93
     C                   PARM                    QQRTCD            1
     C                   MOVEL     QQCPT4        CP(A)
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQCPT4        OREQ      *BLANKS
     C                   MOVE      '1'           *IN(A1)
     C                   MOVE      *BLANKS       CDP(A)
     C                   END                                                    ENDEQ 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C     A1            ADD       1             A1                2 0          POSIT CURSOR
     C                   MOVE      '1'           *IN(A1)
     C                   MOVE      *BLANKS       XXCPCD
     C                   MOVE      CP(A)         XXCPCD
     C     PRKEY         SETGT     PPR01
     C     NXTCP2        TAG
     C                   READP     PPR01                                  60
     C     *IN60         IFEQ      '0'
     C     PRSTAT        IFNE      'I'
     C                   MOVEL     PRDSC1        CDP(A)
     C                   ELSE                                                   DISTAT = I
     C                   GOTO      NXTCP2
     C                   END                                                    END RCSTAT
     C                   ELSE                                                   *IN60 = 1
     C                   MOVEL     *BLANK        CDP(A)
     C                   END                                                    END *IN60
     C                   END                                                    ENDEQ '*'
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    ENDIF ?
     C                   END                                                    END DO
     C*
     C                   ELSE                                                   *IN98 = 0
     C*
     C                   MOVEA     '00000000'    *IN(30)
     C                   DO        8             A                 5 0
     C     A             ADD       30            A1                2 0          POSIT CURSOR
     C*
     C                   MOVEL     CP(A)         XX2               2
     C     XX2           IFEQ      '? '
     C*
     C                   MOVE      '000'         QQZIPA            3
     C                   MOVE      *BLANKS       QQCPT4            5
     C                   MOVE      *ZEROS        QQFDAT
     C                   CALL      'F0527R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQZIPA
     C                   PARM                    QQCPT4
     C                   PARM                    QQFDAT
     C                   PARM                    QQRTCD            1
     C                   MOVEL     QQCPT4        CP(A)
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQCPT4        OREQ      *BLANKS
     C                   MOVE      '1'           *IN(A1)
     C                   MOVE      *BLANKS       CDP(A)
     C                   END                                                    ENDEQ 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C     A1            ADD       1             A1                2 0          POSIT CURSOR
     C                   MOVE      '1'           *IN(A1)
     C*
     C** GET PROCEDURE CODE DESCRIPTION IF PRESENT
     C*
     C                   MOVE      *BLANKS       XXCPCD
     C                   MOVE      CP(A)         XXCPCD
     C     RCKEY         SETGT     PRC01
     C     NXTCP3        TAG
     C                   READP     PRC01                                  60
     C     *IN60         IFEQ      '0'
     C     RCSTAT        IFNE      'I'
     C                   MOVEL     RCDSC1        CDP(A)
     C                   ELSE                                                   DISTAT = I
     C                   GOTO      NXTCP3
     C                   END                                                    END RCSTAT
     C                   ELSE                                                   *IN60 = 1
     C                   MOVEL     *BLANK        CDP(A)
     C                   END                                                    END *IN60
     C                   END                                                    ENDEQ '*'
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    ENDIF ?
     C                   END                                                    END DO
     C                   END                                                    END *IN98
     C                   END                                                    END SCR03
     C******************   GOTO STRT                       DEL 11/05/98
      **** BEGIN DEL **** 05/05/98 ************************************
     C******************** END                             ENDIF *INKF
      **** END   DEL **** 05/05/98 ************************************
      *
     C************************************
     C*   F7                             *
     C************************************
     C     *INKG         IFEQ      '1'
      **** BEGIN DEL **** 05/05/98 ************************************
     C************************************
     C**********   SCR01 = SUSCRIBER SEARCH       *
     C************************************
     C********** Q2RECF    IFEQ 'SCR01'
     C******************** CALL 'F0545R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           SCSUB#
     C******************** PARM           QQRTCD  1
     C********** QQRTCD    CABEQ'A'       TERM
     C******************** WRITESCR99
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** SCSUB#    OREQ *ZERO
     C******************** MOVE '1'       *IN37
     C******************** GOTO STRT
     C******************** END                             END EQUAL B
     C*
     C********** QQRTCD    IFEQ '*'
     C******************** MOVE '1'       *IN38
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C******************** END                             END *
     C******************** END                             END SCR01
      **** END   DEL **** 05/05/98 ************************************
     C************************************
     C*   SCR02 = POS SEARCH             *
     C************************************
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      *BLANK        QQPOS             1
     C                   CALL      'F2155R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQPOS
     C                   PARM                    QQRTCD            1
     C                   MOVEL     QQPOS         SCPOS
     C     QQRTCD        CABEQ     'A'           TERM
     C                   WRITE     SCR99
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQPOS         OREQ      *BLANKS
     C                   MOVE      *BLANKS       SCPOSD
     C                   MOVE      '1'           *IN39
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    ENDEQ 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C*
     C     SCPOS         IFNE      *BLANK
     C                   Z-ADD     1             Y                 5 0
     C     SCPOS         LOOKUP    PS(Y)                                  60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     PSD(Y)        SCPOSD
     C                   ELSE
     C                   MOVE      '1'           *IN30
     C                   END                                                    END *IN60
     C                   END                                                    END <> BLANK
     C                   END                                                    ENDEQ '*'
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END                                                    END SCR02
     C                   END                                                    ENDIF *INKG
      **** BEGIN DEL **** 05/05/98 ************************************
     C***********************************
     C**********   F8                            *
     C***********************************
     C********** *INKH     IFEQ '1'
     C************************************
     C**********   SCR01 = GROUP SEARCH           *
     C************************************
     C********** Q2RECF    IFEQ 'SCR01'
     C*
     C********** SCSUB#    IFEQ *ZEROS
     C******************** MOVE UPSUB#    QQSUB#  50
     C******************** ELSE
     C******************** MOVE SCSUB#    QQSUB#
     C******************** END
     C******************** CALL 'F0508R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           QQSUB#
     C******************** PARM           SCFND
     C******************** PARM           QQRTC   1
     C*
     C********** QQRTC     CABEQ'A'       TERM
     C*
     C********** QQRTC     IFEQ 'B'
     C********** SCFND     OREQ *ZERO
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** MOVE '1'       *IN38
     C******************** GOTO STRT
     C******************** END
     C*
     C********** QQRTC     IFEQ '*'
     C******************** MOVE QQSUB#    SCSUB#
     C******************** GOTO STRT
     C******************** END
     C*
     C******************** END                             END SCR01
     C******************** END                             END *INKH
     C***********************************
     C**********   F9                            *
     C***********************************
     C********** *INKI     IFEQ '1'
     C************************************
     C***********   SCR01 = EDIT TYPE SEARCH       *
     C************************************
     C********** Q2RECF    IFEQ 'SCR01'
     C******************** MOVE *BLANK    QQEDTP  2
     C******************** CALL 'U0521R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQEDTP
     C******************** PARM           QQRTCD  1
     C******************** MOVELQQEDTP    SCETYP
     C********** QQRTCD    CABEQ'A'       TERM
     C******************** WRITESCR99
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** QQEDTP    OREQ *BLANKS
     C******************** MOVE '1'       *IN40
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C******************** END                             ENDEQ 'B'
     C*
     C********** QQRTCD    IFEQ '*'
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C******************** END                             END = '*'
     C*
     C******************** END                             END SCR01
     C******************** END                             END *INKI
     C***********************************
     C**********  F10 = TABLE FILE SEARCH        *
     C***********************************
     C********** *INKJ     IFEQ '1'
     C************************************
     C**********  SCR01                           *
     C************************************
     C********** Q2RECF    IFEQ 'SCR01'
     C******************** WRITESCR99
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C******************** END
      **** END   DEL **** 05/05/98 ************************************
     C************************************
     C*  SCR02                           *
     C************************************
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     SCSPCD        XX2               2
     C     XX2           IFEQ      '? '
     C                   MOVE      *BLANK        SPKEY
     C                   MOVEL     SCSPCD        SPKEY
     C                   CALL      'F2141R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SPKEY
     C                   PARM                    QQRTCD            1
     C                   MOVEL     SPKEY         SCSPCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'B'
     C     SPKEY         OREQ      *BLANK
     C                   MOVE      *BLANKS       SCSPDS
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END                                                    END = B
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN70
     C                   MOVE      *BLANKS       TABKEY
     C                   MOVEL     SCSPCD        TABKEY
     C     SPKY          CHAIN     PSP01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SPDESC        SCSPDS
     C                   ELSE
     C                   MOVE      *BLANKS       SCSPDS
     C                   END                                                    END *IN60
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END = *
     C                   END                                                    END = ?
     C******************   GOTO STRT                       DEL 11/05/98
     C                   END                                                    END SCR02
      **** BEGIN DEL **** 05/05/98 ************************************
     C******************** END                             ENDIF *INKJ
      **** END   DEL **** 05/05/98 ************************************
     C***********************************
     C*   F11                           *
     C***********************************
     C     *INKK         IFEQ      '1'
     C***********************************
     C*  SCR02 = PROVIDER ALPHA SEARCH  *
     C***********************************
     C     Q2RECF        IFEQ      'SCR02'
     C*** END DEL *** 03/25/98 ***************************
     C********             CALL 'F0531R'
     C********             PARM           QQPRDS
     C********             PARM           QQCALL
     C********             PARM           SCFEIN
     C********             PARM           SCMOD
     C********             PARM           QQRTC   1
     C*** END DEL *** 03/25/98 ***************************
     C*** BEG ADD *** 03/25/98 ***************************
     C                   MOVE      SCFEIN        XXFEIN            9
     C                   MOVE      SCMOD         XXMOD             3
     C                   CALL      'F0032R'
     C                   PARM      '2'           QQCALL
     C                   PARM                    XXFEIN
     C                   PARM                    XXMOD
     C                   PARM                    QQRTCD            1
     C                   PARM      ' '           QQTYPE            1
     C                   MOVE      XXFEIN        SCFEIN
     C                   MOVE      XXMOD         SCMOD
     C*** END ADD *** 03/25/98 ***************************
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C     QQRTCD        IFEQ      'B'
     C     SCFEIN        OREQ      *ZERO
     C                   MOVE      *BLANKS       SCPVNM
     C                   MOVE      '1'           *IN31
     C                   END                                                    ENDEQ 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*** BEG CHG *** 11/02/09 **************************************
     C****       PDSTAT    ANDNE'I'
     C     PDSTAT        ANDEQ     'A'
     C     *IN60         OREQ      '0'
     C     PDSTAT        ANDEQ     *BLANKS
     C*** END CHG *** 11/02/09 **************************************
     C                   MOVEL     PDNAME        SCPVNM
     C                   MOVEL     PDSPCD        SCSPCD
     C                   END                                                    END *IN60
     C                   MOVE      *BLANKS       TABKEY
     C                   MOVEL     SCSPCD        TABKEY
     C     SPKY          CHAIN     PSP01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN70
     C                   MOVEL     SPDESC        SCSPDS
     C                   ELSE
     C                   MOVE      '1'           *IN42
     C                   MOVE      *BLANKS       SCSPDS
     C                   END                                                    END *IN60
     C                   END                                                    ENDEQ '*'
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C******************   GOTO STRT                       DEL 11/05/98
     C                   END                                                    END SCR02
     C                   END                                                    ENDIF *INKK
     C***********************************
     C*  F12 = PREVIOUS                 *
     C***********************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C     Q2RECF        IFEQ      'SCR03'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END                                                    ENDIF *INKL
     C***************************************
     C*  CMD 14                             *
     C***************************************
     C     *INKN         IFEQ      '1'
     C***********************************
     C*  SCR02                          *
     C***********************************
     C                   END                                                    END *INKN
     C***********************************
     C*  F20                            *
     C***********************************
     C     *INKU         IFEQ      '1'
     C***********************************
     C*  SCR02 = CHANGE STATUS          *
     C***********************************
     C     Q2RECF        IFEQ      'SCR02'
     C     *IN09         IFEQ      '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   END                                                    END SCSTAT
     C                   END                                                    *IN09
     C                   END                                                    END SCR02
     C                   GOTO      STRT
     C                   END                                                    ENDIF *INKU
     C****************************************
     C*      F24 = MORE COMMAND KEYS         *
     C****************************************
     C     *INKY         IFEQ      '1'
     C                   MOVEL     Q2RECF        HLRECF           10
     C     CK            ADD       1             CK                5 0
     C*
      **** BEGIN DEL **** 05/05/98 ************************************
     C********** HLRECF    IFEQ 'SCR01'
     C********** CK        IFGT 2
     C******************** Z-ADD1         CK
     C******************** END                             END CK GT 2
     C******************** MOVELCK1A,CK   SCCMD1
     C******************** END                             END SCR01
      **** END   DEL **** 05/05/98 ************************************
     C     HLRECF        IFEQ      'SCR02'
     C     CK            IFGT      3
     C                   Z-ADD     1             CK
     C                   END                                                    END CK GT 3
     C                   MOVEL     CK2A(CK)      SCCMD2
     C                   END                                                    END SCR02
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     HLRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *INKY
     C***********************************
     C*  PROCESS HELP REQUEST           *
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
     C*
     C                   GOTO      STRT
     C                   END                                                    ENDIF *IN10
     C***********************************
     C*     F13 = CHG USER PARAMETERS   *
     C***********************************
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'F0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END                                                    ENDIF *INKM
     C*
     C*
     C                   END                                                    ENDIF *IN11
     C*****************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C**********************************************
     C*   PROCESS REQUEST SCREEN - SCR01           *
     C**********************************************
     C     SCR01T        TAG
     C     SCEDIT        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20                          ERROR
     C                   GOTO      STRT
     C                   END                                                    END SCEDIT
     C                   MOVEL     SCEDIT        XXEDIT
     C                   Z-ADD     SCEXT         XXEXT
     C                   MOVE      '1'           *IN12
     C     SCSUB#        IFNE      *ZEROS
     C     SCSUB#        IFNE      XXSUB#
     C                   MOVE      *BLANKS       XXSUB#
     C                   MOVE      SCSUB#        XXSUB#
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       SCSBNM
     C                   MOVEL     SMNAME        SCSBNM
     C                   ELSE                                                   *IN60 = 1
     C                   MOVE      '1'           *IN25                          INVALID SUB#
     C                   MOVE      *BLANKS       XXSUB#
     C                   END                                                    END *IN60
     C                   END                                                    END SCSUB#
     C                   ELSE                                                   SCSUB# = 0
     C                   Z-ADD     *ZEROS        XXSUB#
     C                   MOVE      *BLANKS       SCSBNM
     C                   END                                                    END SCSUB#
     C     SCFND         IFNE      *ZEROS
     C     SCFND         IFNE      XXFND
     C                   MOVE      *BLANKS       XXFND
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       SCNAME
     C                   MOVEL     FNNAME        SCNAME
     C                   ELSE
     C                   MOVE      '1'           *IN26
     C                   MOVE      *BLANKS       XXFND
     C                   END                                                    END *IN60
     C                   END                                                    END SCFND
     C                   ELSE                                                   SCFND = 0
     C                   Z-ADD     *ZEROS        XXFND
     C                   MOVE      *BLANKS       SCNAME
     C                   END                                                    END SCFND
     C     SCEXT         IFNE      *ZEROS
     C     CRKY1         CHAIN     PCR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '0'           *IN27
     C                   MOVE      '1'           *IN86
     C                   GOTO      LOAD
     C                   ELSE
     C     CRKEY         CHAIN     PCR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN27
     C                   GOTO      LOAD
     C                   ELSE
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END                                                    END *IN60
     C                   END                                                    END CRKY1
     C                   ELSE                                                   *IN60 = 1
     C     CRKEY         CHAIN     PCR01                              60
     C                   MOVE      '0'           *IN27                          COND LOAD
     C                   MOVE      '0'           *IN86                          CIND LOAD
     C     LOAD          TAG
     C     *IN60         IFEQ      '0'
     C     SCETYP        IFNE      *BLANKS
     C                   MOVE      *BLANKS       SCETYP
     C                   END
     C                   MOVE      '0'           *IN12
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVEL     UA(2)         SCMODE
     C                   EXSR      SCXLD
     C                   GOTO      STRT
     C                   ELSE                                                   NOT EXISTING
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C     SCETYP        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN21                          ERROR
     C                   GOTO      STRT
     C                   ELSE                                                   NE *BLANKS
     C                   Z-ADD     1             Y
     C     SCETYP        LOOKUP    EDTP(Y)                                60
     C     *IN60         IFNE      '1'
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END                                                    END *IN60
     C                   END                                                    END SCETYP
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVE      'A'           SCSTAT
     C                   MOVEL     UA(1)         SCMODE
     C                   GOTO      STRT
     C                   END                                                    END *IN60
     C                   END                                                    END SCEXT
     C*
     C*********************************************
     C* PROCESS REQUEST SCREEN - SCR02            *
     C*********************************************
     C     SCR02T        TAG
     C*
     C                   EXSR      EDIT2
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C* IF THIS IS AN EXTENSION POSITION CURSOR @ THE FIRST PROCEDURE
     C* CODE ON SCREEN THREE
     C*
     C     *IN27         IFEQ      '1'
     C                   MOVE      '1'           *IN31
     C                   END                                                    END *IN27
     C                   GOTO      STRT
     C*********************************************
     C* PROCESS REQUEST SCREEN - SCR03            *
     C*********************************************
     C     SCR03T        TAG
     C                   EXSR      EDIT3
     C                   EXSR      WRITX                                        WRITE DATA
     C                   EXSR      INIT
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*********************************************
     C*  END THE PROGRAM                          *
     C*********************************************
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*******************************************************************
     C* EDIT2 SUBROUTINE - EDIT SCREEN DATA FOR SCR02                   *
     C*******************************************************************
     C     EDIT2         BEGSR
     C*
     C** VALIDATE DESCRIPTION
     C*
     C     SCDESC        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END                                                    END SCDESC
     C*
     C** VALIDATE PLACE OF SERVICE CODE
     C*
     C     SCPOS         IFNE      *BLANKS
     C                   Z-ADD     1             Y
     C     SCPOS         LOOKUP    PS(Y)                                  60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     PSD(Y)        SCPOSD
     C                   ELSE
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END                                                    END *IN60
     C                   ELSE
     C                   MOVE      *BLANKS       SCPOSD
     C                   END                                                    END SCPOS
     C*
     C** VALIDATE LOST TIME FLAG
     C*
     C     SCLTFL        IFNE      *BLANKS
     C     SCLTFL        ANDNE     'Y'
     C     SCLTFL        ANDNE     'N'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END                                                    END SCLTFL
     C*
     C** GET SPECIALTY CODE DESCRIPTION
     C*
     C     SCSPCD        IFNE      *BLANKS
     C                   MOVE      *BLANKS       TABKEY
     C                   MOVEL     SCSPCD        TABKEY
     C     SPKY          CHAIN     PSP01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SPDESC        SCSPDS
     C                   ELSE
     C                   MOVE      '1'           *IN50
     C                   GOTO      STRT
     C                   END
     C                   ELSE
     C                   MOVE      *BLANKS       SCSPDS
     C                   END
     C*
     C*** IF DIAGNOSIS CODE ENTERED MUST BE VALID
     C*
     C                   MOVE      '0'           *IN92
     C                   DO        5             Y                 5 0
     C     Y             ADD       92            Y1                2 0
     C     DC(Y)         IFNE      *BLANK
     C                   MOVEL     *BLANK        XXICD9
     C                   MOVEL     DC(Y)         XXICD9
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN92                          INVALID DIAG CODE
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   ELSE                                                   *IN60 = 0
     C     DISTAT        IFNE      'A'
     C                   MOVE      '1'           *IN92                          INVALID DIAG CODE
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   END                                                    END NE A
     C                   END                                                    END *IN60
     C                   END                                                    END DC <> BLANK
     C                   END                                                    END DO
     C*
     C** GET DIANOSTIC CODE DESCRIPTION IF PRESENT
     C*
     C                   DO        5             X
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      DC(X)         XXICD9
     C     DIKEY         SETGT     PDI01
     C     NXTDC1        TAG
     C                   READP     PDI01                                  60
     C     *IN60         IFEQ      '0'
     C     DISTAT        IFNE      'I'
     C                   MOVEL     DIDSC1        DS(X)
     C                   ELSE                                                   DISTAT = I
     C                   GOTO      NXTDC1
     C                   END                                                    END DISTAT
     C                   ELSE                                                   *IN60 = 1
     C                   MOVE      *BLANKS       DS(X)
     C                   END                                                    END *IN60
     C                   END                                                    END DO
     C*
     C** VALIDATE PROVIDER/MOD
     C*
     C     SCFEIN        IFNE      *ZEROS
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   END                                                    END *IN60
     C*** BEG CHG *** 11/02/09 **************************************
     C****       PDSTAT    IFEQ 'I'
     C     PDSTAT        IFNE      'A'
     C     PDSTAT        ANDNE     *BLANKS
     C*** END CHG *** 11/02/09 **************************************
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   ELSE                                                   END PDSTAT
     C                   MOVE      *BLANKS       SCPVNM
     C                   MOVEL     PDNAME        SCPVNM
     C                   END                                                    END *IN60
     C                   ELSE
     C                   MOVE      *BLANKS       SCPVNM
     C                   END                                                    END SCFEIN
     C                   ENDSR
     C*******************************************************************
     C* EDIT3 SUBROUTINE - EDIT SCREEN DATA FOR SCR03                   *
     C*******************************************************************
     C     EDIT3         BEGSR
     C*
     C** GET PROCEDURE CODE DESCRIPTION IF PRESENT
     C*
     C     *IN98         IFEQ      '0'
     C                   MOVE      '0'           *IN76
     C                   DO        8             Y                 5 0
     C     Y             ADD       76            Y1                2 0
     C     CP(Y)         IFNE      *BLANK
     C                   MOVE      *BLANKS       XXCPCD
     C                   MOVE      CP(Y)         XXCPCD
     C     RCKEY         CHAIN     PRC01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN76                          INVALID DIAG CODE
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   ELSE                                                   *IN60 = 0
     C     RCSTAT        IFNE      'A'
     C                   MOVE      '1'           *IN76                          INVALID PROC CODE
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   END                                                    END NE A
     C                   END                                                    END *IN60
     C                   END                                                    END CP <> BLANK
     C                   END                                                    END DO
     C*
     C                   ELSE
     C*
     C*** CLOSE FILE IF OPENED
     C*
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FMPRL
     C                   MOVE      ' '           OPNFLG            1
     C                   END                                                    END OPNFLG=1
     C*
     C*** DELETE OVERRIDE
     C*
     C     OVRFLG        IFEQ      '1'
     C*
     C*** DELETE OVERRIDE TO FMPRL
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
     C*** OVERRIDE TO FMPRL
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(2)        @@CMD
     C                   MOVEL     XXST          @@ST                           02/21/91 ADD
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG            1
     C                   END                                                    END OVRFLG=0
     C*
     C*** OPEN FILE FOR PROCESSING
     C*
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMPRL
     C                   MOVE      '1'           OPNFLG
     C                   END                                                    END OPNFLG= ' '
     C*
     C                   MOVE      '0'           *IN76
     C                   DO        8             Y                 5 0
     C     Y             ADD       76            Y1                2 0
     C     CP(Y)         IFNE      *BLANK
     C                   MOVE      *BLANKS       XXCPCD
     C                   MOVE      CP(Y)         XXCPCD
     C     PRKEY         CHAIN     PPR01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN76                          INVALID DIAG CODE
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   ELSE                                                   *IN60 = 0
     C     PRSTAT        IFNE      'A'
     C                   MOVE      '1'           *IN76                          INVALID PROC CODE
     C                   MOVE      '1'           *IN(Y1)
     C                   GOTO      STRT
     C                   END                                                    END NE A
     C                   END                                                    END *IN60
     C                   END                                                    END CP <> BLANK
     C                   END                                                    END DO
     C                   END                                                    END *IN98
     C*
     C** VALIDATE INCLUDE - EXCLUDE FLAG
     C*
     C     SCIEFL        IFNE      'I'
     C     SCIEFL        ANDNE     'E'
     C                   MOVE      '1'           *IN85
     C                   GOTO      STRT
     C                   END                                                    END SCIEFL
     C                   ENDSR
     C*******************************************************************
     C* INIT SUBROUTINE - INITALIZE ALL FIELDS ON ENTRY                 *
     C*******************************************************************
     C     INIT          BEGSR
     C                   MOVE      *BLANKS       SCDESC
     C                   Z-ADD     *ZEROS        SCWKST
     C                   Z-ADD     *ZEROS        SCVIST
     C                   MOVE      *BLANKS       DC
     C                   MOVE      *BLANKS       SCSPCD
     C                   Z-ADD     *ZEROS        SCFEIN
     C                   Z-ADD     *ZEROS        SCMOD
     C                   MOVE      *BLANKS       SCLTFL
     C                   MOVE      *BLANKS       SCSTAT
     C                   MOVE      *BLANKS       SCPOS
     C                   MOVE      *BLANKS       SCPOSD
     C                   MOVE      *BLANKS       SCPVNM
     C                   MOVE      *BLANKS       SCSPDS
     C                   MOVE      *BLANKS       DS
     C                   MOVE      *BLANKS       SCIEFL
     C                   Z-ADD     *ZEROS        SCAMT1
     C                   Z-ADD     *ZEROS        SCAGE
     C                   Z-ADD     *ZEROS        SCOCCR
     C                   MOVE      *BLANKS       CP
     C                   MOVE      *BLANKS       CDP
     C     *IN12         IFEQ      '0'
     C                   MOVE      *BLANKS       SCETYP
     C                   END                                                    END *IN12
     C                   ENDSR
     C*******************************************************************
     C* SCXLD SUBROUTINE - LOAD SCREEN DATA FOR SCR02 AND SCR03         *
     C*******************************************************************
     C     SCXLD         BEGSR
     C                   Z-ADD     CRCO#         XXCO#
     C                   Z-ADD     CRFND         XXFND
     C                   MOVEL     CRSUB#        SCSUB#
     C                   MOVEL     CREDIT        SCEDIT
     C                   MOVE      CRETYP        SCETYP
     C                   MOVEL     CRDESC        SCDESC
     C                   MOVEL     CRDESC        XHDESC
     C                   Z-ADD     CRWKST        SCWKST
     C                   Z-ADD     CRVIST        SCVIST
     C     *IN27         IFEQ      '0'
     C                   Z-ADD     CREXT         SCEXT
     C                   MOVEL     CRDIA1        SCDIA1
     C                   MOVEL     CRDIA2        SCDIA2
     C                   MOVEL     CRDIA3        SCDIA3
     C                   MOVEL     CRDIA4        SCDIA4
     C                   MOVEL     CRDIA5        SCDIA5
     C                   DO        5             X                 2 0
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      DC(X)         XXICD9
     C     DIKEY         SETGT     PDI01
     C     NXTDC2        TAG
     C                   READP     PDI01                                  60
     C     *IN60         IFEQ      '0'
     C     DISTAT        IFNE      'I'
     C                   MOVEL     DIDSC1        DS(X)
     C                   ELSE
     C                   GOTO      NXTDC2
     C                   END                                                    END DISTAT
     C                   ELSE                                                   *IN60 = 1
     C                   MOVE      *BLANK        DS(X)
     C                   END                                                    END *IN60
     C                   END                                                    END DO
     C                   ELSE                                                   *IN27 = 1
     C                   MOVE      *BLANKS       SCDIA1
     C                   MOVE      *BLANKS       SCDIA2
     C                   MOVE      *BLANKS       SCDIA3
     C                   MOVE      *BLANKS       SCDIA4
     C                   MOVE      *BLANKS       SCDIA5
     C                   MOVE      *BLANKS       DS
     C                   END                                                    END *IN27
     C                   MOVEL     CRSPCD        SCSPCD
     C                   MOVEL     CRSPCD        XHSPCD
     C     SCSPCD        IFNE      *BLANKS
     C                   MOVE      *BLANKS       TABKEY
     C                   MOVEL     SCSPCD        TABKEY
     C     SPKY          CHAIN     PSP01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SPDESC        SCSPDS
     C                   ELSE
     C                   MOVE      *BLANKS       SCSPDS
     C                   END
     C                   END                                                    END SCSPCD
     C                   Z-ADD     CRFEIN        SCFEIN
     C                   Z-ADD     CRMOD         SCMOD
     C                   Z-ADD     CRFEIN        XHFEIN
     C                   Z-ADD     CRMOD         XHMOD
     C     SCFEIN        IFNE      *ZEROS
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*** BEG CHG *** 11/02/09 **************************************
     C****       PDSTAT    ANDNE'I'
     C     PDSTAT        ANDEQ     'A'
     C     *IN60         OREQ      '0'
     C     PDSTAT        ANDEQ     *BLANKS
     C*** END CHG *** 11/02/09 **************************************
     C                   MOVE      *BLANKS       SCPVNM
     C                   MOVEL     PDNAME        SCPVNM
     C                   END                                                    END *IN60
     C                   ELSE
     C                   MOVE      *BLANKS       SCPVNM
     C                   END                                                    END SCFEIN
     C                   MOVE      CRLTFL        SCLTFL
     C                   MOVE      CRLTFL        XHLTFL
     C                   MOVE      CRSTAT        SCSTAT
     C                   MOVE      CRPOS         SCPOS
     C                   MOVE      CRPOS         XHPOS
     C     SCPOS         IFNE      *BLANKS
     C                   Z-ADD     1             Y
     C     SCPOS         LOOKUP    PS(Y)                                  60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     PSD(Y)        SCPOSD
     C                   END                                                    END *IN60
     C                   ELSE
     C                   MOVE      *BLANKS       SCPOSD
     C                   END                                                    END SCPOS
     C*
     C** MOVE IN SCR03 VALUES HERE READY TO GOTO SCR03
     C*
     C     *IN12         IFEQ      '0'
     C                   Z-ADD     CRAMT1        SCAMT1
     C                   Z-ADD     CRAGE         SCAGE
     C                   Z-ADD     CROCCR        SCOCCR
     C                   MOVE      CRIEFL        SCIEFL
     C     *IN27         IFEQ      '0'
     C                   MOVEL     CRCPT1        SCCPT1
     C                   MOVEL     CRCPT2        SCCPT2
     C                   MOVEL     CRCPT3        SCCPT3
     C                   MOVEL     CRCPT4        SCCPT4
     C                   MOVEL     CRCPT5        SCCPT5
     C                   MOVEL     CRCPT6        SCCPT6
     C                   MOVEL     CRCPT7        SCCPT7
     C                   MOVEL     CRCPT8        SCCPT8
     C*
     C** GET PROCEDURE CODE DESCRIPTION IF PRESENT
     C*
     C     *IN98         IFEQ      '0'
     C                   DO        8             X
     C                   MOVE      *BLANKS       XXCPCD
     C                   MOVE      CP(X)         XXCPCD
     C     RCKEY         SETGT     PRC01
     C     NXTCP1        TAG
     C                   READP     PRC01                                  60
     C     *IN60         IFEQ      '0'
     C     RCSTAT        IFNE      'I'
     C                   MOVEL     RCDSC1        CDP(X)
     C                   ELSE                                                   RCSTAT = I
     C                   GOTO      NXTCP1
     C                   END                                                    END RCSTAT
     C                   ELSE                                                   *IN60 = 1
     C                   MOVEL     *BLANK        CDP(X)
     C                   END                                                    END *IN60
     C                   END                                                    END DO
     C*
     C                   ELSE                                                   *IN98 = 1
     C*
     C*** CLOSE FILE IF OPENED
     C*
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FMPRL
     C                   MOVE      ' '           OPNFLG            1
     C                   END                                                    END OPNFLG=1
     C*
     C*** DELETE OVERRIDE
     C*
     C     OVRFLG        IFEQ      '1'
     C*
     C*** DELETE OVERRIDE TO FMPRL
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
     C*** OVERRIDE TO FMPRL
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(2)        @@CMD
     C                   MOVEL     XXST          @@ST                           02/21/91 ADD
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG            1
     C                   END                                                    END OVRFLG=0
     C*
     C*** OPEN FILE FOR PROCESSING
     C*
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMPRL
     C                   MOVE      '1'           OPNFLG
     C                   END                                                    END OPNFLG= ' '
     C*
     C                   DO        8             X
     C                   MOVE      *BLANKS       XXCPCD
     C                   MOVE      CP(X)         XXCPCD
     C     PRKEY         SETGT     PPR01
     C     NXTCPT        TAG
     C                   READP     PPR01                                  60
     C     *IN60         IFEQ      '0'
     C     PRSTAT        IFNE      'I'
     C                   MOVEL     PRDSC1        CDP(X)
     C                   ELSE                                                   PRSTAT = I
     C                   GOTO      NXTCPT
     C                   END                                                    END PRSTAT
     C                   ELSE                                                   *IN60 = 1
     C                   MOVEL     *BLANK        CDP(X)
     C                   END                                                    END *IN60
     C                   END                                                    END DO
     C                   END                                                    END *IN98
     C                   ELSE                                                   *IN27 = 1
     C                   MOVE      *BLANKS       SCCPT1
     C                   MOVE      *BLANKS       SCCPT2
     C                   MOVE      *BLANKS       SCCPT3
     C                   MOVE      *BLANKS       SCCPT4
     C                   MOVE      *BLANKS       SCCPT5
     C                   MOVE      *BLANKS       SCCPT6
     C                   MOVE      *BLANKS       SCCPT7
     C                   MOVE      *BLANKS       SCCPT8
     C                   MOVE      *BLANKS       CDP
     C                   END                                                    END *IN27
     C                   END                                                    END *IN12
     C                   ENDSR
     C*******************************************************************
     C* WRITX SUBROUTINE - WRITE THE DATA FOR SCR02 AND SCR03 TO DISK   *
     C*******************************************************************
     C     WRITX         BEGSR
     C     SCEXT         IFEQ      *ZEROS
     C     CRKEY         CHAIN     PCR01                              60
     C                   ELSE
     C     CRKY1         CHAIN     PCR01                              60
     C                   END                                                    END SCEXT
     C                   Z-ADD     XXCO#         CRCO#
     C                   Z-ADD     XXSUB#        CRSUB#
     C                   MOVEL     SCFND         CRFND
     C                   MOVEL     SCSTAT        CRSTAT
     C                   Z-ADD     SCFEIN        CRFEIN
     C                   Z-ADD     SCMOD         CRMOD
     C                   MOVEL     SCEDIT        CREDIT
     C                   Z-ADD     SCEXT         CREXT
     C                   MOVE      SCETYP        CRETYP
     C                   MOVEL     SCDESC        CRDESC
     C                   Z-ADD     SCAMT1        CRAMT1
     C                   Z-ADD     SCOCCR        CROCCR
     C                   Z-ADD     SCWKST        CRWKST
     C                   Z-ADD     SCAGE         CRAGE
     C                   Z-ADD     SCVIST        CRVIST
     C                   MOVEL     SCSPCD        CRSPCD
     C                   MOVEL     SCLTFL        CRLTFL
     C                   MOVE      SCIEFL        CRIEFL
     C                   MOVEL     SCDIA1        CRDIA1
     C                   MOVEL     SCDIA2        CRDIA2
     C                   MOVEL     SCDIA3        CRDIA3
     C                   MOVEL     SCDIA4        CRDIA4
     C                   MOVEL     SCDIA5        CRDIA5
     C                   MOVE      Q1USER        CRUSER
     C                   MOVE      Q1PGM         CRPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRDT
     C*
     C                   Z-ADD     DSTIME        CRTIME
     C                   MOVE      SCPOS         CRPOS
     C                   MOVEL     SCCPT1        CRCPT1
     C                   MOVEL     SCCPT2        CRCPT2
     C                   MOVEL     SCCPT3        CRCPT3
     C                   MOVEL     SCCPT4        CRCPT4
     C                   MOVEL     SCCPT5        CRCPT5
     C                   MOVEL     SCCPT6        CRCPT6
     C                   MOVEL     SCCPT7        CRCPT7
     C                   MOVEL     SCCPT8        CRCPT8
     C     *IN12         IFEQ      '1'
     C                   MOVEL     Q1USER        CRIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRIDT
     C*
     C                   END                                                    END *IN12
     C     *IN60         IFEQ      '0'
     C     *IN27         ANDEQ     '0'
     C                   UPDATE    PCR01
     C                   ELSE                                                   *IN60 = 1
     C                   WRITE     PCR01
     C                   END                                                    END *IN60
     C*
     C     *IN12         IFEQ      '0'
     C     SCDESC        IFNE      XHDESC
     C     SCFEIN        ORNE      XHFEIN
     C     SCMOD         ORNE      XHMOD
     C     SCLTFL        ORNE      XHLTFL
     C     SCPOS         ORNE      XHPOS
     C     SCSPCD        ORNE      XHSPCD
     C     CRKEY         CHAIN     PCR01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVEL     SCDESC        CRDESC
     C                   Z-ADD     SCFEIN        CRFEIN
     C                   Z-ADD     SCMOD         CRMOD
     C                   MOVE      SCLTFL        CRLTFL
     C                   MOVE      SCPOS         CRPOS
     C                   MOVEL     SCSPCD        CRSPCD
     C                   UPDATE    PCR01
     C     CRKEY         READE     PCR01                                  60
     C                   END                                                    END *IN60
     C                   END                                                    END SCDESC
     C                   END                                                    END *IN12
     C                   ENDSR
     C*
     C*
     **** BEGIN DEL **** 05/06/98 *************************************
********  CK1A - COMMAND KEYS for SCR01
********F3=Exit F6=Edit Search  F7=Subscriber Search  F24=More          keys
********F8=Group Search F9=Edit Type Search F13=Chg User Parms F24=More keys
********* END   DEL **** 05/06/98 *************************************
**  CK2A - COMMAND KEYS for SCR02
F3=Exit    F6=Diag Code Search   F7=Place of Service Search    F24=More keys
F10=Table File Search      F11=Provider Alpha Search           F24=More keys
F12=Cancel        F20=Change Status                            F24=More keys
**  CK3A - COMMAND KEYS for SCR03 (*IN98 = 1)
F3=Exit           F6=Procedure Code Search             F12=Cancel
**  CK3B - COMMAND KEYS for SCR03 (*IN98 = 0)
F3=Exit           F6=R & C Code Search                 F12=Cancel
**  MODE TABLE
-- ADD --
- UPDATE -
**  PLACE OF SERVICE DESCRIPTION
 INPATIENT HOSPITAL
 OUTPATIENT HOSPITAL
 DOCTOR'S OFFICE
 PATIENTS HOME
 DAY CARE FACILITY(PSY)
 NIGHT CARE FACILITY(PSY)
 NURSING HOME
 SKILLED NURSING FACILITY
 AMBULANCE
 OTHER LOCATION
 INDEPENDENT LABRATORY
 OTHER MEDICAL/SURGICAL FACILITY
 RESIDENTIAL TREATMENT CENTER
 SPECIALIZED TREATMENT FACILITY
**  (PS) PLACE OF SERVICE CODES
1234567890ABCD
**  QCMDEXC TABLE
DLTOVR FILE(FMPRL)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
**  EDTP EDIT TYPE CODES
A1A2A3A4A5A6A7A8A9AAABACADAEAFAGAHAIAJAKAL
