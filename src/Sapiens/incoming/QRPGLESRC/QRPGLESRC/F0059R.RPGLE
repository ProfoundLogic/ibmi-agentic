     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H*
     H*  Program: F0059R - Convert Medispan NDC'S Drug Code File
     H*
     H*****************************************************************
     H*  Created on 02/20/2004 by C:62 for Project 25MNQ
     H*****************************************************************
     H*  Revisions:
     H*
     H* 05/02/18 C:180 PROJECT: MC420
     H*                Recompile only.
     H*
     H* 03/22/10 C:251 PROJECT C71MKI
     H*          Read new record R01 to use Medispan MDDB V2.5
     H*
     H*****************************************************************
     F*================================================================
     F* Files
     F*================================================================
     FFMS0L     IF   E           K DISK
     FFMRXL     UF A E           K DISK

     D*================================================================
     D* Arrays
     D*================================================================
     D CDI             S              4    DIM(94) CTDATA PERRCD(1)             Column Headings
     D CDO             S              3    DIM(94) ALT(CDI)
     D*================================================================
     D* Data Structures
     D*================================================================
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS
     D  A1                     1     85
     D  OTC                   52     52
     D  DOSE                  56     56
     D  BRAND                 75     75
     D*
     D                 DS
     D  C1                     1     85
     D  ONDC                   2     12
     D*
     D                 DS
     D  E1                     1     85
     D  NAME                   2     61
     D*
     D                 DS
     D  J1                     1     85
     D  PDSC                  59     66
     D*
     D                 DS
     D  L1                     1     85
     D  DFORM                 25     28
     D  PSIZE                 29     36  3
     D  PSIZEU                37     38
     D  PQTY                  39     43  0
     D*
     D                 DS
     D  R1                     1     85
     D  AWP                    9     17
     D  EFDT                  18     23  0
     D  PAWP                  30     38
     D*** BEGIN ADD **** 03/22/10 *************************************
     D                 DS
     D  R01                    1     85
     D  R1AWP                 13     25
     D  R1EFDT                26     33  0
     D  R1PAWP                44     56
     D*** END ADD **** 03/22/10 ***************************************
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*================================================================
     D* Constants
     D*================================================================
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')

     C*================================================================
     C* Parameter Lists
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
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
     C     DTCYM2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YMD'        SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*================================================================
     C* Key Lists
     C*================================================================
     C     RXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKNDC
     C*
     C     S0KY          KLIST
     C                   KFLD                    KKNDC
     C*================================================================
     C* Like defines
     C*================================================================
     C     *LIKE         DEFINE    RXCO#         KKCO#
     C     *LIKE         DEFINE    RXNDC         KKNDC
     C*================================================================
     C* Main Processing
     C*================================================================
     C                   MOVE      @@CO#         KKCO#
     C*
     C                   READ      PS001                                  99
     C     *IN99         DOWEQ     *OFF
     C                   EXSR      WUPRXP
     C                   READ      PS001                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C***************************************************************
     C* WUPRXP - Write/Update FMRXP
     C***************************************************************
     C     WUPRXP        BEGSR
     C*
     C                   CLEAR                   PRX01
     C                   MOVEL(P)  S0NDC         KKNDC
     C     RXKY          CHAIN     PRX01                              99
     C*
     C                   Z-ADD     KKCO#         RXCO#
     C                   MOVEL(P)  S0NDC         RXNDC
     C*
     C     *IN98         DOUEQ     *ON
     C*
     C                   SELECT
     C     S0RTYP        WHENEQ    'A1'
     C                   MOVEL     S0DATA        A1
     C*
     C                   SELECT
     C     OTC           WHENEQ    'O'
     C     BRAND         ANDEQ     'T'
     C     OTC           OREQ      'P'
     C     BRAND         ANDEQ     'T'
     C                   MOVEL(P)  '07'          RXPTYP
     C*
     C     OTC           WHENEQ    'O'
     C     BRAND         ANDEQ     'B'
     C     OTC           OREQ      'P'
     C     BRAND         ANDEQ     'B'
     C                   MOVEL(P)  '00'          RXPTYP
     C*
     C     OTC           WHENEQ    'O'
     C     BRAND         ANDEQ     'G'
     C     OTC           OREQ      'P'
     C     BRAND         ANDEQ     'G'
     C                   MOVEL(P)  '08'          RXPTYP
     C*
     C     OTC           WHENEQ    'R'
     C     BRAND         ANDEQ     'T'
     C     OTC           OREQ      'S'
     C     BRAND         ANDEQ     'T'
     C     OTC           OREQ      'R'
     C     BRAND         ANDEQ     'B'
     C     OTC           OREQ      'S'
     C     BRAND         ANDEQ     'B'
     C                   MOVEL(P)  '01'          RXPTYP
     C*
     C     OTC           WHENEQ    'R'
     C     BRAND         ANDEQ     'G'
     C     OTC           OREQ      'S'
     C     BRAND         ANDEQ     'G'
     C                   MOVEL(P)  '02'          RXPTYP
     C                   ENDSL
     C*
     C                   SELECT
     C     DOSE          WHENEQ    'X'
     C     DOSE          OREQ      'U'
     C                   MOVEL(P)  DOSE          RXUNTD
     C                   OTHER
     C                   MOVEL(P)  'S'           RXUNTD
     C                   ENDSL
     C*
     C     S0RTYP        WHENEQ    'C1'
     C                   MOVEL     S0DATA        C1
     C                   MOVEL(P)  ONDC          RXONDC
     C*
     C     S0RTYP        WHENEQ    'E1'
     C                   MOVEL     S0DATA        E1
     C                   MOVEL(P)  NAME          RXNAME
     C*
     C     S0RTYP        WHENEQ    'J1'
     C                   MOVEL     S0DATA        J1
     C                   MOVEL(P)  PDSC          RXSTRG
     C*
     C     S0RTYP        WHENEQ    'L1'
     C                   MOVEL     S0DATA        L1
     C     PSIZE         MULT      PQTY          RXPSZE
     C                   MOVEL(P)  PSIZEU        RXMEAS
     C                   Z-ADD     1             X                 3 0
     C     DFORM         LOOKUP    CDI(X)                                 98
     C     *IN98         IFEQ      *ON
     C                   MOVEL(P)  CDO(X)        RXFORM
     C                   ELSE
     C                   MOVEL(P)  'OTH'         RXFORM
     C                   ENDIF
     C*
     C     S0RTYP        WHENEQ    'R1'
     C                   MOVEL     S0DATA        R1
     C                   MOVE      AWP           RXAWP
     C                   MOVE      PAWP          RX1AWP
     C                   Z-ADD     EFDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         RXAWPD
     C*
     C*** BEGIN ADD **** 03/22/10 *************************************
     C*** Record R01 type used for MDDB V2.5
     C*
     C     S0RTYP        WHENEQ    'R01'
     C                   MOVEL     S0DATA        R01
     C                   MOVE      R1AWP         RXAWP
     C                   MOVE      R1PAWP        RX1AWP
     C                   Z-ADD     R1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         RXAWPD
     C*** END ADD **** 03/22/10 ***************************************
     C*
     C                   ENDSL
     C*
     C     S0KY          READE     PS001                                  98
     C                   ENDDO
     C*
     C     S0KY          SETGT     PS001
     C*
     C***** MAINTENANCE FIELDS ****************************************
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RXTIME
     C                   MOVEL(P)  Q1USER        RXUSER
     C                   MOVEL(P)  Q1PGM         RXPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RXDT
     C*
     C     *IN99         IFEQ      *OFF
     C                   UPDATE    PRX01
     C                   ELSE
     C                   MOVEL     'A'           RXSTAT
     C                   MOVEL(P)  Q1USER        RXIUSR
     C                   Z-ADD     RXDT          RXIDT
     C                   WRITE     PRX01
     C                   ENDIF
     C*
     C                   ENDSR
** FORM
AEROAER
AEFBAPB
AERBARB
AERPARP
AERSARS
BAR BAR
BEADBEA
CAPSCAP
CPCRCCR
CPDRCDR
CPECCEC
CPEPCEP
CPSPCSP
CP12C12
CP24C24
CONCCNT
CREACRE
CRYSCRY
DEVIDEV
TESTTES
DPRHDIA
DISKDSK
ELIXELI
EMULEMU
ENEMNMA
FILMFIL
FLAKFLA
EXTREXT
FOAMFOA
GAS GAS
GEL GEL
SOLGGFS
GRANGRA
GREFGEF
GUM GUM
IMPLIMP
INHAINL
INJ INJ
INSTICR
IUD IUD
KIT KIT
LEAVLEA
LIQDLIQ
LQCRLCR
LPOPLOL
LOTNLOT
LOZGLOZ
MISCMIS
NEBUNEB
OIL OIL
OINTOIN
PACKPAC
PADSPAD
PSTEPAS
PTCHPAT
PT24P24
PT72P72
PTTWPBI
PTWKPWE
PLLTPEL
POWDPOW
PDEFPEF
PUDGPUD
RINGRIN
SHAMSHA
SHEESHT
SOLNSOL
SOLRSOR
SPRTSPT
STCKSTI
STRPSTR
SUPPSUP
SUSPSUS
SUSRSUR
SWABSWA
SYRPSYR
TABSTAB
CHEWCHW
TBCRTCR
TBDRTDR
TBDPTDP
TBEFTEF
TBECTCP
TBSOTSO
TB12T12
TB24T24
SUBLTSU
TAMPTAM
TAPETAP
TAR TAR
TINCTIN
TROCTRO
WAFRWAF
WAX WAX
