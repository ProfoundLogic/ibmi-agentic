     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W1070R - WORK WITH CITY/DEMOGRAPHIC CROSS REFERENCE
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO ACCESS       *
     H*                A CROSS REFERENCE FOR UPDATE, DISPLAY OR       *
     H*                SELECTION DEPENDING ON THE CALLING PROGRAM.    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*    W0050R - WORK WITH CITY/DEMOGRAPHIC CROSS REFERENCE TABLE  *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 01/26/99  PROGRAMMER: C:113 PROJECT: 102AO          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  12/10/13  C:267  PROJECT: Redmine 1509 for FMIC              *
     H*            Allow upper/lower case entry for City name.        *
     H*            Also, updated UI such that the city name field     *
     H*            definition is changed to CAPITALIZE from UPPER,    *
     H*            and the entry field for city name is also          *
     H*            changed to CAPITALIZE from UPPER.                  *
     H*                                                               *
     H*  03/09/00  C:90   PROJECT: SUPP:WA102/22120                   *
     H*            ONLY DO CITY AND STATE EDIT IN ADD MODE. ONLY      *
     H*            ALLOW CHANGE OF CITY AND STATE IN ADD MODE.        *
     H*                                                               *
     H* ------------------------------------------------------------- *
     H*  NOTE: PLEASE ADD COMMENTS TO TOP OF THE REVISIONS.           *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  60 - CHAINING - GENERAL
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  99 - CHAINING - ERRCK
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  71 - (WCR0503) State code must be entered                    *
     H*  72 - (WCR0690) State code invalid                            *
     H*  73 - (WCR0503) City name must be entered                     *
     H*  74 - (WCR0505) County code invalid                           *
     H*  75 - (WCR0690) County code inactive                          *
     H*  76 - (WCR0505) Region code invalid                           *
     H*  77 - (WCR0690) Region code inactive                          *
     H*  78 - (WCR0505) Chapter code invalid                          *
     H*  79 - (WCR0690) Chapter code inactive                         *
     H*  80 - (WCR0505) Loss Control Rep Staff code invalid           *
     H*  81 - (WC30000) Updated since entry                           *
     H*                                                               *
     H*  POSITION CURSOR:                                             *
     H*  30 - State (SCST)                                            *
     H*  31 - City name (SCCTY)                                       *
     H*  32 - County (SCCNTY)                                         *
     H*  33 - Region (SCRGN)                                          *
     H*  34 - Chapter (SCCHAP)                                        *
     H*  35 - Loss control rep (SCLREP)                               *
     H*                                                               *
     H*****************************************************************
     FW1070F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)
     FSTABL     IF   E           K DISK
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PST02)
     FWMCPL     IF   E           K DISK
     FWMCYL1    IF   E           K DISK
     F                                     RENAME(PCY01:PCY02)
     F*
     F*  Physical file definition (for maintaining record)
     FWMCYP     UF A E             DISK
     F                                     RENAME(PCY01:DBRCDF)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS1)
     FWMREL     IF   E           K DISK
     FWMREP     IF   E             DISK
     F                                     RENAME(PRE01:PRE02)
     FWMUPL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FWTCNL1    IF   E           K DISK
     FWTCNP     IF   E             DISK
     F                                     RENAME(PCN01:PCN02)
      /COPY SCOPYMT01D
      **** BEGIN ADD **** 12/10/13 ************************************
      /copy sPrString
      **** END   ADD **** 12/10/13 ************************************
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('State/City Demogra-
     D                                     phic Information')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  State Table search parameter list
     C*
     C     STSPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C*  County Table search parameter list
     C*
     C     COSPL         PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM                    XXKEY
     C                   PARM      ZZST          ##ST              2
     C                   PARM      2             ##VIEW            1 0
     C*
     C*  Region Table search parameter list
     C*
     C     RGSPL         PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      DBRRN1        ##RRN2            9 0
     C*
     C*  Chapter Table search parameter list
     C*
     C     CHSPL         PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      DBRRN1        ##RRN2
     C                   PARM                    ##CHAP
     C*
     C*  Staff Code search parameter list
     C*
     C     LCSPL         PLIST
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      3             ##VIEW
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  Fund Master file
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND             3 0
     C*
     C*  System-wide State Code Table file
     C*
     C     STKY          KLIST
     C                   KFLD                    ##KY2            10
     C*
     C*  County Code Table file
     C*
     C     CNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXKEY
     C*
     C*  Region Master file
     C*
     C     REKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    XXRGN
     C*
     C*  Chapter Master file
     C*
     C     CPKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    XXCHAP
     C*
     C*  City/State Demographic Cross Reference file
     C*
     C     CYKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCST
     C                   KFLD                    SCCTY
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CYRGN         XXRGN
     C     *LIKE         DEFINE    CYCHAP        XXCHAP
     C     *LIKE         DEFINE    CYCHAP        ##CHAP
     C     *LIKE         DEFINE    CNKEY         XXKEY
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C*  CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C     CHGSTS        BEGSR
     C                   ENDSR                                                  CHGSTS
     C*================================================================
     C*  DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C*  DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C*  ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*  Only check edits if NOT in display mode
     C*
     C     *IN09         IFEQ      *OFF
     C*
     C*  State Code must be entered
     C*
     C     *IN90         IFEQ      *OFF
     C     SCST          ANDEQ     *BLANKS
     C                   SETON                                        71  90
     C                   ELSE                                                   SCST
     C*
     C*  The State must exist in STSTP
     C*
     C                   MOVEL(P)  SCST          ##KY2
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        72  90
     C                   ENDIF                                                  *IN99=*ON
     C*
     C                   ENDIF                                                  *IN90=*ON
     C*
     C*  City name must be entered
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCTY         ANDEQ     *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCCTY
     C*
     C*  City/State cannot already exist in WMCYP
     C*
     C     *IN90         IFEQ      *OFF
      **** BEGIN ADD **** 03/09/00 ************************************
     C     @@MODE        ANDEQ     'A'
      **** END   ADD **** 03/09/00 ************************************
     C*
     C     CYKY          CHAIN     PCY02                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        82  90
     C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C*  County code must be valid if entered
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCNTY        ANDNE     *BLANKS
     C*
     C     SCST          CAT(P)    SCCNTY:0      ZZKEY             4
     C                   MOVEL     ZZKEY         XXKEY
     C*
     C     CNKY          CHAIN     PCN01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        74  90
     C                   ELSE
     C*
     C     CNSTAT        IFEQ      'I'
     C                   SETON                                        75  90
     C                   ENDIF                                                  CNSTAT=I
     C*
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  *IN90=*ON
     C*
     C*  Region code must be valid if entered
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRGN         ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCRGN         XXRGN
     C*
     C     REKY          CHAIN     WMREL                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        76  90
     C                   ELSE
     C*
     C     RESTAT        IFEQ      'I'
     C                   SETON                                        77  90
     C                   ENDIF                                                  RESTAT='I'
     C*
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  *IN90=*ON
     C*
     C*  Chapter Code must be valid if entered
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCHAP        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCCHAP        XXCHAP
     C*
     C     CPKY          CHAIN     WMCPL                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        78  90
     C                   ELSE
     C*
     C     CPSTAT        IFEQ      'I'
     C                   SETON                                        79  90
     C                   ENDIF                                                  *CPSTAT='I'
     C*
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  *IN90=*ON
     C*
     C*  Loss Control Rep Staff Code must be valid if entered
     C*
     C     *IN90         IFEQ      *OFF
     C     SCLREP        ANDNE     *BLANKS
     C*
     C     SCLREP        CHAIN     PUP02                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        80  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  *IN90=*ON
     C*
     C*  Check to be sure screen has not been updated since entry
     C*
     C     *IN90         IFEQ      *OFF
     C     CYDT          ANDGT     SAVEDT
     C     CYDT          OREQ      SAVEDT
     C     CYTIME        ANDGT     SAVETM
     C                   SETON                                        81  90
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN09=*OFF
     C*
     C*  We have passed all edits, goto the next screen
     C*
     C                   ENDSR
     C*================================================================
     C*  ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C*  ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C*  KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C*  State Table file search
     C*
     C     HFLD          WHENEQ    'SCST'
     C*
     C                   CALL      'SY601R'      STSPL
     C*
     C     *INKL         IFNE      *ON
     C     ##RRN         ORNE      *ZEROS
     C*
     C                   SETON                                        31  90     PC NEXT
     C     ##RRN         CHAIN     PST02                              99
     C                   MOVEL     STKEY         SCST
     C*
     C                   ELSE
     C*
     C                   SETON                                        30  90     PC THIS
     C*
     C                   ENDIF                                                  ##RRN<>*ZERO
     C*
     C*  County Table file search
     C*
     C     HFLD          WHENEQ    'SCCNTY'
     C*
     C                   MOVEL     SCST          ZZST              2
     C*
     C                   CALL      'W2215R'      COSPL
     C*
     C     *INKL         IFNE      *ON
     C     ##RRN         ORNE      *ZEROS
     C*
     C                   SETON                                        33  90     PC NEXT
     C*
     C     ##RRN         CHAIN     WTCNP                              99
     C     *IN99         IFEQ      *OFF
     C     2             SUBST     CNKEY:3       SCCNTY
     C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   ELSE
     C*
     C                   SETON                                        32  90     PC THIS
     C*
     C                   ENDIF                                                  ##RRN<>*ZER OS
     C*
     C*  Region Table file search
     C*
     C     HFLD          WHENEQ    'SCRGN'
     C*
     C                   CALL      'W0520R'      RGSPL
     C*
     C     *INKL         IFNE      *ON
     C     ##RRN         ORNE      *ZEROS
     C*
     C                   SETON                                        34  90     PC NEXT
     C*
     C     ##RRN         CHAIN     PRE02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RERGN#        SCRGN
     C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   ELSE
     C*
     C                   SETON                                        33  90     PC THIS
     C*
     C                   ENDIF                                                  ##RRN<>*ZEROS
     C*
     C*  Chapter Table file search
     C*
     C     HFLD          WHENEQ    'SCCHAP'
     C*
     C                   CALL      'W0720R'      CHSPL
     C*
     C     *INKL         IFNE      *ON
     C     ##CHAP        ORNE      *ZEROS
     C*
     C                   Z-ADD     ##CHAP        SCCHAP
     C                   SETON                                        35  90     PC NEXT
     C*
     C                   ELSE
     C*
     C                   SETON                                        34  90     PC THIS
     C*
     C                   ENDIF                                                  ##CHAP<>*ZEROS
     C*
     C*  Loss Control Rep Staff Code search
     C*
     C     HFLD          WHENEQ    'SCLREP'
     C*
     C                   CALL      'S0005R'      LCSPL
     C*
     C     *INKL         IFNE      *ON
     C     ##RRN         ORNE      *ZEROS
     C*
     C                   SETON                                        30  90     PC TOP
     C*
     C     ##RRN         CHAIN     PUP03                              99
     C                   MOVEL     UPSTAF        SCLREP
     C*
     C                   ELSE
     C*
     C                   SETON                                        35  90     PC THIS
     C*
     C                   ENDIF                                                   ##RRN<>0
     C*
     C                   ENDSL                                                   HFLD
     C*
     C                   ENDSR
     C*================================================================
     C*  MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     CYST          SCST
     C                   MOVEL     CYCTY         SCCTY
     C                   MOVEL     CYCNTY        SCCNTY
     C                   MOVEL     CYRGN         SCRGN
     C                   MOVEL     CYCHAP        SCCHAP
     C                   MOVEL     CYLREP        SCLREP
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCST
     C                   MOVE      *BLANKS       SCCTY
     C                   MOVE      *BLANKS       SCCNTY
     C                   MOVE      *BLANKS       SCRGN
     C                   MOVE      *BLANKS       SCCHAP
     C                   MOVE      *BLANKS       SCLREP
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C*  MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         CYCO#
     C                   MOVEL     SCST          CYST
     C                   MOVEL     SCCTY         CYCTY
     C                   MOVEL     SCCNTY        CYCNTY
     C                   MOVEL     SCRGN         CYRGN
     C                   MOVEL     SCCHAP        CYCHAP
     C                   MOVEL     SCLREP        CYLREP
     C*
     C                   ENDSR
     C*================================================================
     C*  OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C*  OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C*  PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C*  PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C                   Z-ADD     UPCO#         ##CO#
     C                   Z-ADD     UPFND         ##FND
     C     FNKY          CHAIN     PFN01                              60
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C*  PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C*  UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
      **** BEGIN ADD **** 12/10/13 ************************************
     C                   EXSR      TitCase
      **** End   ADD **** 12/10/13 ************************************
     C*
     C                   MOVEL     Q1USER        CYUSER
     C                   MOVEL     Q1PGM         CYPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CYDT
     C*
     C                   MOVEL     DSTIME        CYTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C*  WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
      **** BEGIN ADD **** 12/10/13 ************************************
     C                   EXSR      TitCase
      **** End   ADD **** 12/10/13 ************************************
     C*
     C                   MOVEL     Q1USER        CYUSER
     C                   MOVEL     Q1PGM         CYPGM
     C                   MOVEL     Q1USER        CYIUSR
     C                   MOVEL     Q1PGM         CYIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CYDT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CYIDT
     C*
     C                   MOVEL     DSTIME        CYTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMCYP
     C*
     C                   ENDSR
      **** BEGIN ADD **** 12/10/13 ************************************
     C*================================================================
     C*  TitCase - Convert city name to title case
     C*================================================================
     C     TitCase       BEGSR
      /Free
             CYCTY = TitleCase(CYCTY);
      /End-free
     C                   ENDSR
      **** END   ADD **** 12/10/13 ************************************
