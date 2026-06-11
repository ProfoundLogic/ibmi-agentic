     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE F0394R - Adjudicate/Pend selection
     H*****************************************************************
     H*                                                               *
     H*  CALLED BY: F0353R                                            *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED:                                                     *
     H*                                                               *
     H*  06/12/98 C:61 PROJECT MC152                                  *
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  XX/XX/XX C:XX PROJECT XXXXX                                  *
     H*                XXX.......................................XXX  *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H* SCR01                                                         *
     H*  71       'Adjudicate' must be 'Y' or *blank                  *
     H*  72       'Pend' must be 'Y' or *blank                        *
     H*  73       'Adjudicate' or 'Pend' must be 'Y'                  *
     H*  74       'Pend type' required if 'Pend' equal 'Y'            *
     H*  75 - 76  Position cursor                                     *
     H*  77       'Pend' required if 'Pend type' not blank            *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*****************************************************************
     F*
     F*
     F*
     FF0394F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFAEOP     UF A E             DISK
     F                                     RENAME(PEO01:DBRCDF)
     F*
     F*  Bill Status Table File
     F*****FTBSL   IF  E           K        DISK
     F*****FTBSP   IF  E                    DISK
     F*****       PBS01                             KRENAMEPBS02
     F*
     F*  WCMC EOMB Transaction File
     F*****FAESL   UF  E           K        DISK
     F*
     D***PY SCOPYMT01I
      /COPY SCOPYMT01D
     D*
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Adjudicate/Pend sele-
     D                                     ction')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

     I*
     I*
     I*================================================================
     I*  PLIST definitions
     I*================================================================

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FAEOP
     C*****                PARM           @@APDF  1
     C*****                PARM           @@APPN  3
     C*****                PARM           @@CANC  1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C* EOMB Status Transaction
     C*
     C*****      ESKY      KLIST
     C*****                KFLD           KKCO#
     C*****                KFLD           KKSUB#
     C*****                KFLD           KKFND
     C*****                KFLD           KKFYR
     C*****                KFLD           KKCASE
     C*****                KFLD           KKEOB#
     C*
     C* Bill Status Table
     C*
     C********** BSKY      KLIST
     C**********           KFLD           ##CO#
     C**********           KFLD           KKTYPE
     C**********           KFLD           KKCODE
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C*****      *LIKE     DEFN ESCO#     KKCO#
     C*****      *LIKE     DEFN ESSUB#    KKSUB#
     C*****      *LIKE     DEFN ESFND     KKFND
     C*****      *LIKE     DEFN ESFYR     KKFYR
     C*****      *LIKE     DEFN ESCASE    KKCASE
     C*****      *LIKE     DEFN ESEOB#    KKEOB#
     C*
     C********** *LIKE     DEFN BSTYPE    KKTYPE
     C********** *LIKE     DEFN BSCODE    KKCODE
     C*
     C     *LIKE         DEFINE    EOEOB#        ##EOB#
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C* Adjudicate must be 'Y' or *blank
     C*
     C**N90***** SCADJ     IFNE 'Y'
     C********** SCADJ     ANDNE*BLANK
     C**********           SETON                     9071
     C**********           ENDIF                           SCADJ
     C*
     C* Cancel  must be 'Y' or *blank
     C*
     C**N90***** SCCANC    IFNE 'Y'
     C********** SCCANC    ANDNE*BLANK
     C**********           SETON                     9079
     C**********           ENDIF                           SCCANC
     C**********
     C* Pend must be 'Y' or *blank
     C**********
     C**N90***** SCPEND    IFNE 'Y'
     C********** SCPEND    ANDNE*BLANK
     C**********           SETON                     9072
     C**********           ENDIF                           SCPEND
     C*
     C********** @@CANC    IFNE 'Y'
     C*
     C* Adjudicate OR Pend must be 'Y'
     C*
     C**N90***** SCADJ     IFNE 'Y'
     C********** SCPEND    ANDNE'Y'
     C**********           SETON                     9073
     C**********           ENDIF                           SCADJ SCPEND
     C**N90***** SCADJ     IFEQ 'Y'
     C********** SCPEND    ANDEQ'Y'
     C**********           SETON                     9073
     C**********           ENDIF                           SCADJ SCPEND
     C*
     C**********           ENDIF                           @@CANC NE 'Y'
     C*
     C********** @@CANC    IFEQ 'Y'
     C*
     C* Adjudicate OR Pend OR Cancel must be 'Y'
     C*
     C**N90***** SCADJ     IFNE 'Y'
     C********** SCPEND    ANDNE'Y'
     C********** SCCANC    ANDNE'Y'
     C**********           SETON                     9080
     C**********           ENDIF                           SCADJ SCPEND
     C**N90***** SCADJ     IFEQ 'Y'
     C********** SCPEND    ANDEQ'Y'
     C********** SCADJ     OREQ 'Y'
     C********** SCCANC    ANDEQ'Y'
     C********** SCPEND    OREQ 'Y'
     C********** SCCANC    ANDEQ'Y'
     C**********           SETON                     9081
     C**********           ENDIF                           SCADJ SCPEND
     C*
     C**********           ENDIF                           @@CANC = 'Y'
     C*
     C* Pend type required if Pend = 'Y'
     C*
     C**N90***** SCPEND    IFEQ 'Y'
     C********** SCPTYP    ANDEQ*BLANKS
     C**********           SETON                     9074
     C**********           ENDIF                           SCPEND SCPTYP
     C*
     C* 'Pend' must equal 'Y' if 'Pend Type' entered
     C*
     C**N90***** SCPTYP    IFNE *BLANKS
     C********** SCPEND    ANDEQ*BLANKS
     C**********           SETON                     9077
     C**********           ENDIF                           SCPTYP SCPEND
     C*
     C* Pend type must be valid if entered
     C*
     C**N90***** SCPTYP    IFNE *BLANKS
     C**********           MOVEL'P'       KKTYPE    P      PEND
     C**********           MOVELSCPTYP    KKCODE    P
     C********** BSKY      CHAINPBS01                60
     C***60*****           SETON                     9078
     C**********           ENDIF                           SCPTYP VALIDP
     C*
     C* If reason = *blank and Pend = 'Y' then load reason with
     C*  Pend Type Description
     C*
     C**N90***** SCREAS    IFEQ *BLANKS
     C********** SCPEND    ANDEQ'Y'
     C**********           MOVEL'P'       KKTYPE    P      PEND
     C**********           MOVELSCPTYP    KKCODE    P
     C********** BSKY      CHAINPBS01                60
     C**N60*****           MOVELBSDESC    SCREAS    P
     C***60*****           MOVEL*BLANKS   SCREAS    P
     C**********           ENDIF                           SCREAS SCPEND
     C*
     C*
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KJDO - F10 processing
     C*================================================================
     C     KJDO          BEGSR
     C*
     C**********           MOVE '1'       *IN90
     C*
     C**********           SELEC
     C*
     C*  Search Pend types
     C*
     C********** HFLD      WHEQ 'SCPTYP'
     C*
     C**********           CALL 'F2435R'
     C**********           PARM '2'       ##CALL  1
     C**********           PARM *ZERO     ##KC    1
     C**********           PARM *ZERO     ##KL    1
     C**********           PARM *ZERO     ##RRN   90
     C**********           PARM 'P'       ##TYPE  1
     C*
     C********** ##RRN     IFEQ *ZEROS
     C**********           SETON                     75
     C**********           ELSE
     C*
     C********** ##RRN     CHAINPBS02                60
     C**N60*****           MOVELBSCODE    SCPTYP    P
     C**N60*****           MOVELBSDESC    SCPTDS    P
     C**********           SETON                     76
     C**********           ENDIF
     C*
     C*
     C*
     C**********           ENDSL
     C*
     C                   ENDSR
     C*
     C*
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*  Default Adjudicate and Pend Flags
     C*
     C**********           SELEC
     C*
     C********** @@APDF    WHEQ 'A'
     C**********           MOVEL'Y'       SCADJ     P
     C*
     C********** @@APDF    WHEQ 'P'
     C**********           MOVEL'Y'       SCPEND    P
     C*
     C**********           ENDSL
     C*
     C*  Default Pend Type
     C*
     C**********           MOVE @@APPN    SCPTYP
     C*
     C********** SCPTYP    IFNE *BLANKS
     C**********           MOVEL'P'       KKTYPE    P      PEND
     C**********           MOVELSCPTYP    KKCODE    P
     C********** BSKY      CHAINPBS01                60
     C**N60*****           MOVELBSDESC    SCPTDS    P
     C***60*****           MOVEL*BLANKS   SCPTDS    P
     C**********           ENDIF                           SCPTYP
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C* If Pend = 'Y'
     C*
     C**********           MOVE ' '       HLSTAT  1
     C********** SCPEND    IFEQ 'Y'
     C**********           MOVE 'P'       HLSTAT
     C**********           ENDIF
     C*
     C********** SCCANC    IFEQ 'Y'
     C**********           MOVE 'C'       HLSTAT
     C**********           ENDIF
     C*
     C********** HLSTAT    IFNE ' '
     C**********           MOVE HLSTAT    EOBLST
     C**********           TIME           DSDTTM
     C*
     C*
     C**********           MOVELQ1USER    EOUSER    P
     C**********           MOVELQ1PGM     EOPGM     P
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     EODT
     C**********           MOVELDSTIME    EOTIME
     C*
     C*
     C*****                MOVEL'P'       ESBLST    P
     C*****                MOVELSCPTYP    ESSTCD    P
     C*****                MOVELSCREAS    ESREAS    P
     C*
     C*****                Z-ADD@@RRN     ##RRN
     C*****                MOVEL'P'       ##BLST  1 P
     C**********           MOVELSCPTYP    ##STCD  30P
     C**********           MOVELSCREAS    ##REAS 60 P
     C*
     C*****                MOVELQ1USER    ESUSER    P
     C*****                MOVELQ1PGM     ESPGM     P
     C*****                Z-ADDDSDATE    SRDTI
     C*****                CALL 'SRCVTDAT'DTCYMD
     C*****                MOVELSRDTO     ESDT
     C*****                MOVELDSTIME    ESTIME
     C*
     C**********           ENDIF                           HLSTAT NE ' '
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*  Process F10 search
     C*
     C********** *INKJ     CASEQ'1'       KJDO
     C**********           ENDCS
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C* CALL ADJUDICATION PROGRAM IF NECESSARY
     C*
     C********** SCADJ     IFEQ 'Y'
     C                   Z-ADD     EOFYR         ##FYR
     C                   Z-ADD     EOCASE        ##CASE
     C                   MOVE      *BLANKS       ##FDTS
     C                   MOVE      *BLANKS       ##FDT2
     C                   MOVE      *BLANKS       ##SEQS
     C                   MOVE      *BLANKS       ##ER#S
     C                   MOVE      '*'           ##RTCD
     C                   MOVEL     EOSS#         ##SS#
     C*
     C                   Z-ADD     EOACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ##ACDT
     C*
     C                   MOVEL     EOSPCD        ##SPCD
     C                   Z-ADD     EOSFEI        ##FEIN
     C                   Z-ADD     EOSMOD        ##MOD
     C**********           MOVE EOEOB#    ##DCS#
     C                   Z-ADD     EOEOB#        ##EOB#
     C                   MOVEL     EOAST         ##AST
     C*
     C                   CALL      'F4000RX'
     C                   PARM                    ##FYR             3 0
     C                   PARM                    ##CASE            7 0
     C                   PARM                    ##FDTS          150
     C                   PARM                    ##FDT2          150
     C                   PARM                    ##SEQS          150
     C                   PARM                    ##ER#S           60
     C                   PARM                    ##RTCD            1
     C                   PARM                    ##SS#             9 0
     C                   PARM                    ##ACDT            6 0
     C                   PARM                    ##SPCD            3
     C                   PARM                    ##FEIN            9 0
     C                   PARM                    ##MOD             3 0
     C                   PARM                    ##DCS#            5
     C                   PARM                    ##AST             2
     C                   PARM                    ##EOB#
     C*
     C*****                ELSE
     C*****                MOVE '1'       @@KC
     C*****                ENDIF
     C*
     C*
     C                   SETON                                        LR
     C                   ENDSR
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C* Set Change Mode
     C*
     C                   MOVEL(P)  'C'           @@MODE
     C*
     C* Get FAEOP record
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             60
     C*
     C* Get FAESP record
     C*
     C*****                Z-ADDEOCO#     KKCO#
     C*****                Z-ADDEOSUB#    KKSUB#
     C*****                Z-ADDEOFND     KKFND
     C*****                Z-ADDEOFYR     KKFYR
     C*****                Z-ADDEOCASE    KKCASE
     C*****                Z-ADDEOEOB#    KKEOB#
     C*****      ESKY      CHAINPES01               N60
     C*
     C*
     C********** @@CANC    IFEQ 'Y'                        allow cancel
     C**********           SETOF                     30
     C**********           ELSE
     C**********           SETON                     30    disallow cancel
     C**********           ENDIF
     C*
     C                   MOVE      '1'           *INKC
     C                   ENDSR
     C*
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*****      @@RRN     CHAINDBRCDF               99
     C*****      *IN99     IFEQ *OFF
     C*
     C*****      ESKY      CHAINPES01                60
     C*****      *IN60     IFEQ *OFF
     C*
     C*****                EXSR MVSCDB
     C*
     C*****                UPDATDBRCDF
     C*****                UPDATPES01
     C*
     C*****                ENDIF
     C*****                ENDIF                           *IN99 = *OFF
     C*
     C* CALL F0395R01 TO WRITE BILL STATUS HISTORY RECORD ONLY IF
     C* STATUS HAS CHANGED.  (STATUS DOES NOT CHANGE WHEN ADJUDICATION
     C* IS SELECTED)
     C*
     C********** HLSTAT    IFNE ' '
     C**********           Z-ADD0         ##RRN
     C**********           Z-ADD@@RRN     ##RRN2  90       faeop
     C**********           MOVE EOBLST    ##BLST  1
     C**********           MOVELSCPTYP    ##STCD    P
     C**********           MOVELSCREAS    ##REAS    P
     C**********           CALL 'F0395R01'
     C**********           PARM ' '       ##KC
     C**********           PARM ' '       ##KL
     C**********           PARM 'H'       ##MODE  1
     C**********           PARM           ##RRN
     C**********           PARM           ##RRN2
     C**********           PARM           ##BLST
     C**********           PARM           ##STCD
     C**********           PARM           ##REAS
     C**********           ENDIF
     C                   ENDSR
     C*
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C**********           CLEARDBRCDF
     C**********           EXSR MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FAEOP
     C                   ENDSR
