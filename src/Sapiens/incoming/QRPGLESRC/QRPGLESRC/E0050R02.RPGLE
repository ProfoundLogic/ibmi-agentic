     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE E0050R02 - EXCESS/NON-EXCESS CLAIMS
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* MAINTENANCE PROGRAM FOR EXCESS/NON-EXCESS CLAIMS              *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CREATED: 08/28/01                                             *
     H*                                                               *
     H* 08/28/01 C:136 PROJECT WC577                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  71       ECR0505 - SCMULT (NO RECORD FOUND)                  *
     H*  72       ECR0500 - SCACDT (INVALID DATE)                     *
     H*  73       ECR0512 - SCACDT (DATE OUTSIDE OF POLICY RANGE)     *
     H*  74       ECR0502 - SCACDT (DATE CANNOT BE ZERO)              *
     H*  75       ECR0502 - SCCASE (CASE # CANNOT BE BLANK)           *
     H*  76       ECR0547 - SCAMT1 (VALUE CANNOT BE LESS THAN 0)      *
     H*  77       ECR0511 - SCSTAT (STAT MUST BE O OR F)              *
     H*  78       ECR0547 - SCTLCA (VALUE CANNOT BE LESS THAN 0)      *
     H*  79       ECR0547 - SCAMT2 (VALUE CANNOT BE LESS THAN 0)      *
     H*  80       ECR0501 - SPTYPE (INVALID TRANSACTION CODE)
     H*  91       CONDITION SCREEN FOR EXCESS CLAIMS                  *
     H*  92       CONDITION SCREEN FOR NON-EXCESS CLAIMS              *
     H*                                                               *
     H*****************************************************************
     FE0050F02  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FEMEEP     UF A E             DISK
     F                                     RENAME(PEE01:DBRCDF)
      *
      * Employer Batch Number Master File
     FEMEBP     IF   E             DISK
      *
      * Employer Master File
     FEMEML     IF   E           K DISK
      *
      * Fund Master File
     FEMFNL     IF   E           K DISK
      *
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Excess/Non-Excess Cl-
     D                                     aims')
     D COMAPI          C                   CONST('SA005R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##CONM           40
      *
     C*================================================================
     C*  KLIST definitions
     C*================================================================
      *
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
      *
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    EEFND         KKFND
     C     *LIKE         DEFINE    EEEMP#        KKEMP#
      *
     C/COPY SCOPYMT01C
     C*================================================================
     C* ADDWDW - Add window prompt
     C*================================================================
     C     ADDWDW        BEGSR
      *
     C     *IN90         DOUEQ     *OFF
      /Free
       spTYPE = 'N';
      /End-Free
     C                   EXFMT     WDW01
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
     C                   MOVEA     '00'          *IN(91)
      *
     C     *INKL         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C  N90SPTYPE        IFNE      'Y'
     C     SPTYPE        ANDNE     'N'
     C                   SETON                                        80  90    ECR0501(INVALID TRAN
     C                   ENDIF                                                  SPTYPE<>Y/N
     C                   ELSE
     C                   RETURN
E03  C                   ENDIF                                                  *INKC=*OFF=*INKL
E02  C                   ENDDO                                                  *IN90=*OFF
      *
     C                   SELECT
     C     SPTYPE        WHENEQ    'Y'
     C                   MOVE      *ON           *IN91
     C     SPTYPE        WHENEQ    'N'
     C                   MOVE      *ON           *IN92
     C                   ENDSL
      *
     C                   ENDSR                                                  ADDWDW
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
      *
     C     *IN90         IFEQ      *OFF
     C     SCMULT        ANDNE     'Y'
     C     SCMULT        ANDNE     'N'
     C     SCMULT        ANDNE     '6'
     C                   SETON                                        71  90
     C                   ENDIF
      *
     C     *IN91         IFEQ      *ON
     C     *IN90         IFEQ      *OFF
     C     SCACDT        ANDNE     *ZERO
      *
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCACDT
      *
     C     SRERR         IFEQ      '1'
     C                   SETON                                        72  90    INVALID DATE
     C                   ENDIF                                                  IF SRERR = 1
      *
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT            7 0
      *
     C     XXACDT        IFGT      EBTODT
     C     XXACDT        ORLT      EBFRDT
     C                   SETON                                        73  90    ACDT IN POLIPOL
     C                   ENDIF                                                  CY YEAR
      *
     C                   ENDIF                                                  IF 90 OFF
      *
     C     *IN90         IFEQ      *OFF
     C     SCACDT        ANDEQ     *ZEROS
     C                   SETON                                        74  90
     C                   ENDIF
      *
     C     *IN90         IFEQ      *OFF
     C     SCCASE        ANDEQ     *BLANKS
     C                   SETON                                        75  90
     C                   ENDIF                                                  IF 90 OFF
      *
     C     *IN90         IFEQ      *OFF
     C     SCAMT1        ANDLT     *ZERO
     C                   SETON                                        76  90
     C                   ENDIF                                                  IF 90 OFF
      *
     C     *IN90         IFEQ      *OFF
     C     SCSTAT        ANDNE     'O'
     C     SCSTAT        ANDNE     'F'
     C                   SETON                                        77  90
     C                   ENDIF
     C                   ENDIF                                                  IF 91 ON
      *
     C     *IN92         IFEQ      *ON
     C     *IN90         IFEQ      *OFF
     C     SCTLCA        ANDLT     *ZERO
     C                   SETON                                        78  90
     C                   ENDIF
      *
     C     *IN90         IFEQ      *OFF
     C     SCAMT2        ANDLT     *ZERO
     C                   SETON                                        79  90
     C                   ENDIF                                                  IF 90 OFF
     C                   ENDIF                                                  IF 92 ON
      *
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *  EEMULT is common to both excess and non-excess claims
     C                   MOVE      EEMULT        SCMULT
      *  Prepare display for excess or nonexcess in change or display mode
     C     @@MODE        IFEQ      'C'
     C     @@MODE        OREQ      'D'
     C                   SETOFF                                       9192
     C     EECASE        IFNE      *BLANKS
     C                   SETON                                        91        Excess claims
     C                   ELSE
     C                   SETON                                        92        Non-excess claims
     C                   ENDIF                                                  EECASE<>*BLANKS
     C                   ENDIF                                                  @@MODE='C'OR'D'
      *  Excess claims
     C     *IN91         IFEQ      *ON
     C                   Z-ADD     EEACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCACDT
     C                   MOVEL(P)  EECASE        SCCASE
     C                   Z-ADD     EEAMT         SCAMT1
     C                   MOVE(P)   EESTAT        SCSTAT
     C                   ENDIF
      *  Non-excess claims
     C     *IN92         IFEQ      *ON
     C                   Z-ADD     EETLCA        SCTLCA
     C                   Z-ADD     EEAMT         SCAMT2
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
      *
     C                   Z-ADD     ##CO#         EECO#
     C                   Z-ADD     SCFND         EEFND
     C                   Z-ADD     SCEMP#        EEEMP#
     C                   Z-ADD     SCYEAR        EEYEAR
     C                   MOVE      SCMULT        EEMULT
      *
     C     *IN91         IFEQ      *ON
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         EEACDT
     C                   MOVEL(P)  SCCASE        EECASE
     C                   Z-ADD     SCAMT1        EEAMT
     C                   MOVE(P)   SCSTAT        EESTAT
     C                   Z-ADD     0             EETLCA
     C                   ENDIF
      *
     C     *IN92         IFEQ      *ON
     C                   Z-ADD     SCTLCA        EETLCA
     C                   Z-ADD     SCAMT2        EEAMT
     C                   Z-ADD     0             EEACDT
     C                   MOVE(P)   *BLANKS       EECASE
     C                   MOVE(P)   *BLANKS       EESTAT
     C                   ENDIF
      *
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
      *
     C     @@MODE        IFEQ      'A'
     C                   SETON                                        40
     C                   EXSR      ADDWDW
     C                   ENDIF
      * Get info from record sent in from w/w batch years
     C                   CLEAR                   PEB01
     C     @@RRN2        CHAIN     PEB01                              60
     C                   Z-ADD     EBFND         SCFND
     C                   Z-ADD     EBEMP#        SCEMP#
     C                   Z-ADD     EBYEAR        SCYEAR
     C                   Z-ADD     EBFND         KKFND
     C                   Z-ADD     EBEMP#        KKEMP#
      * Get company and fund name
     C                   CLEAR                   PEM01
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVEL(P)  EMNAME        SCEMNM
     C                   CLEAR                   PFN01
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL(P)  FNNAME        SCFNNM
      *
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        EEUSER
     C                   MOVEL     Q1PGM         EEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EEDT
     C                   MOVEL     DSTIME        EETIME
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        EEUSER
     C                   MOVEL     Q1USER        EEIUSR
     C                   MOVEL     Q1PGM         EEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EEDT
     C                   MOVEL     SRDTO         EEIDT
     C                   MOVEL     DSTIME        EETIME
     C                   WRITE     DBRCDF
     C                   FEOD      EMEEP
     C                   RETURN
     C                   ENDSR
