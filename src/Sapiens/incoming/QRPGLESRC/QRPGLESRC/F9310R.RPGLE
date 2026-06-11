     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE F9310R - CM SERVICES REPORT
     H*
     H* CREATED 11/02/99 C:62 PROJ:MC175
     H*****************************************************************
     H* INDICATOR USAGE
     H*  21    MCR0787 - Invalid Subscriber
     H*  22    MCR0508 - Invalid Group
     H*  23    MCR0538 - Invalid Employer/Division
     H*  24    MCR2424 - Invalid Staff Code
     H*  25    MCR0513 - Invalid Table value
     H*  26    MCR0327 - Invalid Sort Order
     H*  27    MCR0513 - Invalid Table value
     H*  30    POSITION CURSOR AT SUBSCRIBER
     H*  31    POSITION CURSOR AT GROUP
     H*  32    POSITION CURSOR AT EMPLOYER
     H*  33    POSITION CURSOR AT Staff Code
     H*  34    POSITION CURSOR AT Case Load
     H*  35    POSITION CURSOR AT SORT ORDER
     H*  36    POSITION CURSOR AT TYPE OF OUTPUT
     H*  99    General Chain
     H*****************************************************************
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
     H*****************************************************************
     FF9310F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     FFMSML     IF   E           K DISK
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
     FFMEML     IF   E           K DISK
     FFMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)

     D*
     D***  BEGIN ADD  ***  05/16/01  *********************************
     D OPTSEL          C                   CONST(', if selected.')
     D***  END   ADD  ***  05/16/01  *********************************
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  Company Master File
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*  Employer Master File
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*  Fund Master
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*
     C*  Generic Table Value File
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C*  SUB# Master
     C*
     C     SMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCSUB#
     C*
     C*  User Parameter File
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     UPKY1         KLIST
     C                   KFLD                    SCSTAF
     C*
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    Q2RECF        HLRECF
     C     *LIKE         DEFINE    UPCO#         KKCO#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@PRDS           40
     C                   PARM                    @@CALL            1
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
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         KKCO#
     C                   MOVE      *ZERO         UPSUB#
     C*
     C                   ELSE
     C*
     C                   Z-ADD     UPCO#         KKCO#
     C*
     C                   ENDIF
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVEL(P)  @@PRDS        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C*   Initialize the Command Keys Display Variable
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      UPSUB#        SCSUB#
     C                   MOVE      UPFND         SCFND
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   MOVEA     '00000'       *IN(30)
     C                   MOVEA     '00000'       *IN(36)
     C                   MOVE      '1'           *IN32
     C*
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
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   EXFMT     SCR01
     C*
     C                   MOVEA     '00000'       *IN(30)
     C                   MOVEA     '00000'       *IN(36)
     C*
     C                   ENDIF
     C**************************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*****************************************************************
     C     *IN11         IFEQ      '1'
     C*
     C***********************************
     C*   F3 = EXIT                     *
     C***********************************
     C     *INKC         IFEQ      '1'
     C*
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C*
     C                   ENDIF                                                  END *INKC
     C***********************************
     C*   F10 = SEARCH                  *
     C***********************************
     C     *INKJ         IFEQ      '1'
     C*
     C                   SELECT
     C*
     C*  SUBSCRIBER SEARCH
     C*
     C     HFLD          WHENEQ    'SCSUB#'
     C*
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
     C                   MOVEL(P)  SMNAME        SCSBNM
     C                   SETON                                        31
     C*
     C                   ELSE
     C*
     C                   SETON                                        30
     C*
     C                   ENDIF                                                  *IN99 IFEQ
     C*
     C                   GOTO      STRT
     C*
     C* GROUP SEARCH
     C*
     C******** Beg Del ***** 12/18/24 ***************************
     C*****HFLD          WHENEQ    'SCFND'
     C*****
     C*****              CALL      'F0060R'
     C*****              PARM      '2'           ##CALL            1
     C******INKC         PARM                    ##KC              1
     C******INKL         PARM                    ##KL              1
     C*****              PARM                    ##RRN             9 0
     C*****
     C******INKC         CABEQ     *ON           TERM
     C******INKL         CABEQ     *ON           STRT                     31
     C*****
     C*****##RRN         IFNE      *ZEROS
     C*****
     C*****##RRN         CHAIN     PFN02                              99
     C*****
     C*****              Z-ADD     FNFND         SCFND
     C*****              MOVEL(P)  FNNAME        SCGPNM
     C*****              SETON                                        32
     C*****
     C*****              ELSE
     C*****
     C*****              SETON                                        31
     C*****
     C*****              ENDIF                                                  *IN99 IFEQ
     C*****
     C*****              GOTO      STRT
     C******** End Del ***** 12/18/24 ***************************
     C*
     C* EMPLOYER SEARCH
     C*
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'F0140R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      DBRRN         ##RRN2            9 0          FMFNP
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     32
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PEM02                              99
     C*
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     C                   MOVEL(P)  EMNAME        SCEMNM
     C                   SETON                                        33
     C*
     C                   ELSE
     C*
     C                   SETON                                        32
     C*
     C                   ENDIF                                                  *IN99 IFEQ
     C*
     C                   ELSE
     C*
     C                   SETON                                        22
     C*
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C*
     C* STAFF CODE SEARCH.
     C*
     C     HFLD          WHENEQ    'SCSTAF'
     C*
     C                   CALL      'S0005R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      3             ##VIEW            1 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C*
     C                   SETON                                          33
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PUP03                              99
     C*
     C                   MOVEL(P)  UPSTAF        SCSTAF
     C                   MOVEL(P)  UPUSNM        SCSTFN
     C                   SETON                                          34
     C*
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C*
     C* CASE LOAD
     C*
     C     HFLD          WHENEQ    'SCCSLD'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'CSLD'        ##TBID
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     34
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  GTKEY         SCCSLD
     C                   MOVEL(P)  GTDESC        SCCLDS
     C                   MOVE      *ON           *IN35
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN34
     C*
     C                   ENDIF                                                  *IN99 IFEQ
     C*
     C                   GOTO      STRT
     C*
     C* PRIMARY SORT ORDER
     C*
     C     HFLD          WHENEQ    'SC1SRT'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'CSSP'        ##TBID
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     35
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  GTKEY         SC1SRT
     C                   MOVEL(P)  GTDESC        SC1SRD
     C                   MOVE      *ON           *IN36
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN35
     C*
     C                   ENDIF                                                  *IN99 IFEQ
     C*
     C                   GOTO      STRT
     C*
     C* SECONDARY SORT ORDER
     C*
     C     HFLD          WHENEQ    'SC2SRT'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'CSSO'        ##TBID
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     36
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  GTKEY         SC2SRT
     C                   MOVEL(P)  GTDESC        SC2SRD
     C                   MOVE      *ON           *IN37
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN36
     C*
     C                   ENDIF                                                  *IN99 IFEQ
     C*
     C                   GOTO      STRT
     C*
     C* TYPE OF OUTPUTH
     C*
     C     HFLD          WHENEQ    'SCTYPE'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'TOUT'        ##TBID
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     37
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  GTKEY         SCTYPE
     C                   MOVEL(P)  GTDESC        SCODSC
     C*
     C                   ELSE
     C*
     C                   SETON                                        37
     C*
     C                   ENDIF                                                  *IN99 IFEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDSL                                                  HFLD 'SCFND'
     C*
     C                   ENDIF
     C***********************************
     C*   F13 = CHANGE USER PARMS       *
     C***********************************
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'F0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      *ZERO         UPCO#
     C*
     C                   ELSE
     C*
     C                   Z-ADD     UPCO#         KKCO#
     C*
     C                   ENDIF
     C*
     C                   GOTO      STRT01
     C*
     C                   ENDIF                                                  END *INKM
     C***********************************
     C*   *IN10 = PROCESS HELP REQUESTS *
     C***********************************
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C*
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C*
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  END *IN10
     C*
     C                   ENDIF                                                  END CMDKEYS
     C**************************************************************************
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C* Validate Subscriber Number
     C*
     C     SCSUB#        IFNE      *ZEROS
     C*
     C     SMKY          CHAIN     PSM01                              21
     C     *IN21         CABEQ     '1'           STRT
     C*
     C                   ENDIF
     C*
     C* Validate Fund Number
     C*
     C     SCFND         IFNE      *ZEROS
     C*
     C     SCSUB#        IFNE      *ZEROS
     C*
     C     FNKY          CHAIN     PFN01                              22
     C     *IN22         CABEQ     '1'           STRT
     C*
     C                   ELSE
     C*
     C                   SETON                                        41
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* Validate Employer Number
     C*
     C     SCEMP#        IFNE      *ZEROS
     C     SCDIV         ORNE      *ZEROS
     C*
     C     SCSUB#        IFNE      *ZEROS
     C     SCFND         ANDNE     *ZEROS
     C*
     C     EMKY          CHAIN     PEM01                              23
     C     *IN23         CABEQ     '1'           STRT
     C*
     C                   ELSE
     C*
     C                   SETON                                        42
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* Validate Staff code
     C*
     C     SCSTAF        IFNE      *BLANKS
     C*
     C     UPKY1         CHAIN     PUP02                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   SETON                                        24
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* Validate Case Load
     C*
     C                   MOVEL(P)  'CSLD'        KKTBID
     C                   MOVEL(P)  SCCSLD        KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              25
     C     *IN25         CABEQ     '1'           STRT
     C*
     C* Validate Primary Sort Order
     C*
     C                   MOVEL(P)  'CSSP'        KKTBID
     C                   MOVEL(P)  SC1SRT        KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              26
     C     *IN26         CABEQ     '1'           STRT
     C*
     C* Validate Secondary Sort Order
     C*
     C                   MOVEL(P)  'CSSO'        KKTBID
     C                   MOVEL(P)  SC2SRT        KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              27
     C     *IN27         CABEQ     '1'           STRT
     C*
     C* Validate Type of Output
     C*
     C                   MOVEL(P)  'TOUT'        KKTBID
     C                   MOVEL(P)  SCTYPE        KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              28
     C     *IN28         CABEQ     '1'           STRT
     C*
     C*   CALL THE CLP TO RUN THE LIST.
     C*
     C                   CALL      'F9310C'
     C                   PARM      KKCO#         ##CO#             3 0
     C                   PARM      SCSUB#        ##SUB#            5 0
     C                   PARM      SCFND         ##FND             3 0
     C                   PARM      SCEMP#        ##EMP#            9 0
     C                   PARM      SCDIV         ##DIV             5 0
     C                   PARM      SCSTAF        ##STAF            3
     C                   PARM      SCCSLD        ##CSLD            1
     C                   PARM      SC1SRT        ##1SRT            2
     C                   PARM      SC2SRT        ##2SRT            1
     C                   PARM      SCTYPE        ##TYPE            1
     C                   PARM      'D'           ##MODE            1
     C*
     C*  END THE PROGRAM
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
