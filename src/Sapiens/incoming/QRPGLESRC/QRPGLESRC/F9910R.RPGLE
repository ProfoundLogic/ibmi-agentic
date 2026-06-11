     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE F9910R - PROMPT - CM nurse unbilled time
     H*****************************************************************
     H*                                                               *
     H* CALLED FROM: MENU                                             *
     H*                                                               *
     H* CALLS TO:    F9910C - SUBMIT - CM nurse unbilled time         *
     H*                                                               *
     H* CREATED  09/27/99  C:61                                       *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  12/18/24 C:342 Project : MC447 Phase 2                       *
     H*                 1.F10 Search disabled on the Group Field      *
     H*                 DSPF Changes:                                 *
     H*                 1.Changed Existing Group field into Typeahead *
     H*                                                               *
     H*  05/16/01  C:136 PROJECT: WC577                               *
     H*                  Display location of output file.             *
     H*                                                               *
     H* XX/XX/XX C:XX  PROJECT XXXXX                                  *
     H*                XXX.......................................XXX  *
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
     H*                                                               *
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* GLOBAL                                                        *
     H*  60    General Chain                                          *
     H*  99    General Chain                                          *
     H*                                                               *
     H* SCR01                                                         *
     H*  21    MCR0787 - Invalid Subscriber                           *
     H*  22    MCR0508 - Invalid Group                                *
     H*  23    MCR0538 - Invalid Employer/Division                    *
     H*  24    MCR0513 - Invalid table value (case nurse)             *
     H*  25    MCR0500 - Invalid From Date                            *
     H*  26    MCR0500 - Invalid To Date                              *
     H*  27    MCR0529 - Dates must be Ascending                      *
     H*  28    MCR0327 - Invalid Sort Order                           *
     H*  29    MCR0513 - Invalid Table value                          *
     H*  30    POSITION CURSOR AT SUBSCRIBER                          *
     H*  31    POSITION CURSOR AT GROUP                               *
     H*  32    POSITION CURSOR AT EMPLOYER                            *
     H*  33    POSITION CURSOR AT CASE NURSE                          *
     H*  34    POSITION CURSOR AT DATE RANGE                          *
     H*  35    POSITION CURSOR AT SORT ORDER                          *
     H*  36    POSITION CURSOR AT TYPE OF OUTPUT                      *
     H*  72    MCR0676 - Invalid field for search                     *
     H*  73    MCR0676 - Invalid field for search                     *
     H*  75    MCR0513 - Invalid table value                          *
     H*  76    POSITION CURSOR AT STAF CODE                           *
     H*  77    MCR0513 - Invalid table value                          *
     H*                                                               *
     H*****************************************************************
      *
      *
     FF9910F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      *
      * System company
     FSMCOL     IF   E           K DISK
      *
      * System user parm
     FSMUPL     IF   E           K DISK
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)
      *
      * Fund master
     FFMFNL     IF   E           K DISK
     F                                     INFDS(FNFDS)
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
      *
      * Subscriber master
     FFMSML     IF   E           K DISK
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
      *
      * Employer master
     FFMEML     IF   E           K DISK
     FFMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
      *
      * Generic table
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
      *
      *

     D OPTNM           C                   CONST('CM Nurse Unbilled T-
     D                                     ime Report')
     D***  BEGIN ADD  ***  05/16/01  *********************************
     D OPTSEL          C                   CONST(', if selected.')
     D***  END   ADD  ***  05/16/01  *********************************
     D*
      *
     D FNFDS           DS
     D  FNRRN                397    400B 0
      *
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      *
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *  Company Master File
      *

     C     COKY          KLIST
     C                   KFLD                    UPCO#
      *
      *  SUB# Master
      *
     C     SMKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
      *
      *  Fund Master
      *
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
      *
      *  Employer Master File
      *
     C     EMKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      *
      *  Generic Table Value File
      *
     C     GTKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
      *
      *  User Parameter File
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
     C     UPKY1         KLIST
     C                   KFLD                    KKSTAF
      *
      *  *LIKE Defines
      *
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    Q2RECF        HLRECF
     C     *LIKE         DEFINE    SMDT          XXFRDT
     C     *LIKE         DEFINE    SMDT          XXTODT
     C     *LIKE         DEFINE    UPSTAF        KKSTAF
      *
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   END
      *
      *  GET THE COMPANY RECORD
      *
     C     STRT01        TAG
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVEL(P)  OPTNM         SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS           40
      *
      *   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
     C                   MOVE      UPSUB#        SCSUB#
      *
     C     SCSUB#        IFNE      *ZERO
     C     SMKY          CHAIN     PSM01                              21
     C  N21              MOVEL(P)  SMNAME        SCSBNM
     C   21              MOVEL(P)  *BLANKS       SCSBNM
     C                   ENDIF                                                  SCSUB#
      *
     C                   MOVE      UPFND         SCFND
      *
     C     SCSUB#        IFNE      *ZERO
     C     SCFND         ANDNE     *ZERO
     C     FNKY          CHAIN     PFN01                              22
     C  N22              MOVEL(P)  FNNAME        SCGPNM
     C   22              MOVEL(P)  *BLANKS       SCGPNM
     C                   ENDIF                                                  SCFND
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
     C*** BEG ADD ******** 05/16/01 ********************************************
     C*
     C*   DISPLAY LOCATION OF OUTPUT FILE
     C*
     C                   CALL      'SA030R'
     C                   PARM                    QQSUFX            3
     C     QQSUFX        IFEQ      *BLANKS
     C     'PDSRPT'      CAT(P)    OPTSEL        XXRPTL           23
     C                   ELSE
     C     'PDSRPT'      CAT(P)    QQSUFX        XXRPTL
     C                   CAT(P)    OPTSEL:0      XXRPTL
     C                   ENDIF
     C                   MOVE      XXRPTL        SCRPTL
     C*
     C*** END ADD ******** 05/16/01 ********************************************
     C*
      *   DISPLAY ALL FORMATS
      *
     C     STRT          TAG
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
      *
      *
      **************************************************************************
      *   PROCESS ALL COMMAND KEYS
      *****************************************************************
     C     *IN11         IFEQ      '1'
      *
      ***********************************
      *   F3 = EXIT                     *
      ***********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  END *INKC
      *
      *
      *
      ***********************************
      *   F10 = SEARCH                  *
      ***********************************
     C     *INKJ         IFEQ      '1'
      *
     C                   SELECT
      *
      *  SYSTEM
      *
     C     HFLD          WHENEQ    'SCSYS'
      *
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'SYS'         ##TBID
      *
     C     ##RRN         CABEQ     *ZERO         STRT                     74
      *
     C     ##RRN         CHAIN     PGT02                              99
     C                   MOVEL(P)  GTKEY         SCSYS
     C                   MOVEL(P)  GTDESC        SCSYNM
     C                   MOVE      *ON           *IN30
      *
      *  SUBSCRIBER SEARCH
      *
     C     HFLD          WHENEQ    'SCSUB#'
      *
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
      *
     C     ##RRN         CABEQ     *ZERO         STRT                     30
      *
     C     ##RRN         CHAIN     PSM02                              99
     C                   Z-ADD     SMSUB#        SCSUB#
     C                   MOVEL(P)  SMNAME        SCSBNM
     C                   SETON                                        31
      *
      * GROUP SEARCH
      *
     C******** Beg Del ***** 12/18/24 ***************************
     C*****HFLD          WHENEQ    'SCFND'
      *****
     C*****              CALL      'F0060R'
     C*****              PARM      '2'           ##CALL            1
     C*****              PARM                    ##KC              1
     C*****              PARM                    ##KL              1
     C*****              PARM                    ##RRN             9 0
      *****
     C*****##RRN         CABEQ     *ZEROS        STRT                     31
      *****
     C*****##RRN         CHAIN     PFN02                              99
     C*****              Z-ADD     FNFND         SCFND
     C*****              MOVEL(P)  FNNAME        SCGPNM
     C*****              SETON                                        32
     C******** End Del ***** 12/18/24 ***************************
      *
      * EMPLOYER SEARCH
      *
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
      *
     C     FNKY          CHAIN     PFN01                              22
     C   22              GOTO      STRT
      *
     C                   CALL      'F0140R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      FNRRN         ##RRN2            9 0          FMFNP
      *
     C     ##RRN         CABEQ     *ZERO         STRT                     32
      *
     C     ##RRN         CHAIN     PEM02                              99
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     C                   MOVEL(P)  EMNAME        SCEMNM
     C                   SETON                                        33
      *
      * CASE NURSE
      *
     C     HFLD          WHENEQ    'SCCMNU'
     C                   CALL      'S0005R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      3             ##VIEW            1 0
      *
     C     ##RRN         CABEQ     *ZERO         STRT                     33
      *
     C     ##RRN         CHAIN     PUP03                              60
     C                   MOVEL(P)  UPSTAF        SCCMNU
     C                   MOVEL(P)  UPUSNM        SCNUNM
     C                   SETON                                        76
      *
      * STAF CODE
      *
     C     HFLD          WHENEQ    'SCSTAF'
     C                   CALL      'S0005R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      3             ##VIEW            1 0
      *
     C     ##RRN         CABEQ     *ZERO         STRT                     76
      *
     C     ##RRN         CHAIN     PUP03                              60
     C                   MOVEL(P)  UPSTAF        SCSTAF
     C                   MOVEL(P)  UPUSNM        SCSTNM
     C                   SETON                                        34
      *
      * FROM DATE
      *
     C     HFLD          WHENEQ    'SCFRDT'
     C                   SETON                                        72
      *
      * TO DATE
      *
     C     HFLD          WHENEQ    'SCTODT'
     C                   SETON                                        73
      *
      * SORT ORDER
      *
     C     HFLD          WHENEQ    'SCSORT'
      *
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'CSSO'        ##TBID
      *
     C     ##RRN         CABEQ     *ZERO         STRT                     35
      *
     C     ##RRN         CHAIN     PGT02                              99
     C                   MOVEL(P)  GTKEY         SCSORT
     C                   MOVEL(P)  GTDESC        SCSDSC
     C                   MOVE      *ON           *IN36
      *
      * TYPE OF OUTPUT
      *
     C     HFLD          WHENEQ    'SCTYPE'
      *
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'TOUT'        ##TBID
      *
     C     ##RRN         CABEQ     *ZERO         STRT                     36
      *
     C     ##RRN         CHAIN     PGT02                              99
     C                   MOVEL(P)  GTKEY         SCTYPE
     C                   MOVEL(P)  GTDESC        SCODSC
     C                   SETON                                        36
      *
     C                   ENDSL                                                  HFLD 'SCFND'
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKJ
      *
      *
      *
      ***********************************
      *   F13 = CHANGE USER PARMS       *
      ***********************************
     C     *INKM         IFEQ      '1'
     C                   CALL      'F0006R'
     C                   WRITE     SCR99
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKY          CHAIN     PUP01                              99
      *
     C     *IN99         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
      *
     C                   GOTO      STRT01
     C                   ENDIF                                                  END *INKM
      *
      *
      *
      ***********************************
      *   *IN10 = PROCESS HELP REQUESTS *
      ***********************************
     C     *IN10         IFEQ      '1'
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
      *
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
      *
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  END *IN10
      *
     C                   ENDIF                                                  END CMDKEYS
      *
      *
      *
      **************************************************************************
      *
      *   PROCESS REQUEST SCREEN
      *
     C     SCR01T        TAG
      *
      * Validate System
      *
     C     SCSYS         IFNE      *BLANK
     C                   MOVEL(P)  'SYS'         KKTBID
     C                   MOVEL(P)  SCSYS         KKKEY
      *
     C     GTKY          CHAIN     PGT01                              75
     C  N75              MOVEL(P)  GTDESC        SCSYNM
     C   75              MOVEL(P)  *BLANKS       SCSYNM
     C     *IN75         CABEQ     '1'           STRT
      *
     C                   ELSE
      *
     C                   MOVEL(P)  *BLANKS       SCSYNM
     C                   ENDIF                                                  SCSYS
      *
      * Validate Subscriber Number
      *
     C     SCSUB#        IFNE      *ZEROS
     C     SMKY          CHAIN     PSM01                              21
     C  N21              MOVEL(P)  SMNAME        SCSBNM
     C   21              MOVEL(P)  *BLANKS       SCSBNM
     C     *IN21         CABEQ     '1'           STRT
      *
     C                   ELSE
      *
     C                   MOVEL(P)  *BLANKS       SCSBNM
     C                   ENDIF
      *
      * Validate Fund Number
      *
     C     SCFND         IFNE      *ZEROS
      *
     C     SCSUB#        IFNE      *ZEROS
     C     FNKY          CHAIN     PFN01                              22
     C  N22              MOVEL(P)  FNNAME        SCGPNM
     C   22              MOVEL(P)  *BLANKS       SCGPNM
     C     *IN22         CABEQ     '1'           STRT
      *
     C                   ELSE
      *
     C                   SETON                                        41
     C                   ENDIF
      *
     C                   ELSE
      *
     C                   MOVEL(P)  *BLANKS       SCGPNM
     C                   ENDIF
      *
      * Validate Employer Number
      *
     C     SCEMP#        IFNE      *ZEROS
     C     SCDIV         ORNE      *ZEROS
      *
     C     SCSUB#        IFNE      *ZEROS
     C     SCFND         ANDNE     *ZEROS
     C     EMKY          CHAIN     PEM01                              23
     C  N23              MOVEL(P)  EMNAME        SCEMNM
     C   23              MOVEL(P)  *BLANKS       SCEMNM
     C     *IN23         CABEQ     '1'           STRT
      *
     C                   ELSE
      *
     C                   SETON                                        42
     C                   ENDIF
      *
     C                   ELSE
      *
     C                   MOVEL(P)  *BLANKS       SCEMNM
     C                   ENDIF
      *
      * Validate Case Nurse Code
      *
     C     SCCMNU        IFNE      *BLANKS
     C                   MOVEL(P)  SCCMNU        KKSTAF
     C     UPKY1         CHAIN     PUP02                              60
     C  N60              MOVEL(P)  UPUSNM        SCNUNM
     C   60              MOVEL(P)  *BLANKS       SCNUNM
     C   60              SETON                                        24
     C   60              GOTO      STRT
      *
     C                   ELSE
      *
     C                   MOVEL(P)  *BLANKS       SCNUNM
     C                   ENDIF
      *
      * Validate Staff code
      *
     C     SCSTAF        IFNE      *BLANKS
     C                   MOVEL(P)  SCSTAF        KKSTAF
     C     UPKY1         CHAIN     PUP02                              60
     C  N60              MOVEL(P)  UPUSNM        SCSTNM
     C   60              MOVEL(P)  *BLANKS       SCSTNM
     C   60              SETON                                        77
     C   60              GOTO      STRT
      *
     C                   ELSE
      *
     C                   MOVEL(P)  *BLANKS       SCSTNM
     C                   ENDIF
      *
      * Validate FROM date
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
      *
     C     SRERR         CABEQ     '1'           STRT                     25
      *
      * Validate TO date
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
      *
     C     SRERR         CABEQ     '1'           STRT                     26
      *
      * Dates Must be Ascending
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
      *
     C     XXTODT        IFNE      *ZEROS
     C     XXTODT        ANDLT     XXFRDT
     C                   SETON                                        27
     C                   GOTO      STRT
     C                   ENDIF
      *
      * Validate Sort Order
      *
     C                   MOVEL(P)  'CSSO'        KKTBID
     C                   MOVEL(P)  SCSORT        KKKEY
      *
     C     GTKY          CHAIN     PGT01                              28
     C  N28              MOVEL(P)  GTDESC        SCSDSC
     C   28              MOVEL(P)  *BLANKS       SCSDSC
     C     *IN28         CABEQ     '1'           STRT
      *
      * Validate Type of Output
      *
     C                   MOVEL(P)  'TOUT'        KKTBID
     C                   MOVEL(P)  SCTYPE        KKKEY
      *
     C     GTKY          CHAIN     PGT01                              29
     C  N29              MOVEL(P)  GTDESC        SCODSC
     C   29              MOVEL(P)  *BLANKS       SCODSC
     C     *IN29         CABEQ     '1'           STRT
      *
      *   CALL THE CLP TO RUN THE LIST.
      *
     C                   MOVE      UPCO#         ##CO#             3
     C                   MOVE      SCSYS         ##SYS             1
     C                   MOVE      SCSUB#        ##SUB#            5
     C                   MOVE      SCFND         ##FND             3
     C                   MOVE      SCEMP#        ##EMP#            9
     C                   MOVE      SCDIV         ##DIV             5
     C                   MOVE      SCCMNU        ##CMNU            3
     C                   MOVE      SCSTAF        ##STAF            3
     C                   MOVE      XXFRDT        ##FRDT            7
     C                   MOVE      XXTODT        ##TODT            7
     C                   MOVE      SCSORT        ##SORT            1
     C                   MOVE      SCTYPE        ##TYPE            1
     C                   MOVE      'D'           ##MODE            1
      *
     C                   CALL      'F9910C'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##SYS             1
     C                   PARM                    ##SUB#            5
     C                   PARM                    ##FND             3
     C                   PARM                    ##EMP#            9
     C                   PARM                    ##DIV             5
     C                   PARM                    ##CMNU            3
     C                   PARM                    ##STAF            3
     C                   PARM                    ##FRDT            7
     C                   PARM                    ##TODT            7
     C                   PARM                    ##SORT            1
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##MODE            1
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *
