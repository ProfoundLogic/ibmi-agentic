     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*===============================================================*
     H*===============================================================*
     H/TITLE SR17304M - LOUISIANA PROFESSIONAL EMPLOYER INFORMATION
     H*===============================================================*
     H* DESCRIPTION:                                                  *
     H*   This program will allow a user to change, view or add       *
     H*   records in the Professional Employer file.                  *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   SR17304  - ENDORSEMENT ATTACH/PRINT PROGRAM                 *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H*                                                               *
     H* CREATED   : 11/22/06                                          *
     H* PROGRAMMER: C:184                                             *
     H* PROJECT   : WC1505                                            *
     H*                                                               *
     H*===============================================================*
     H* REVISIONS:                                                    *
     H* ------------------------------------------------------------- *
     H*  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
     H* ------------------------------------------------------------- *
     H*===============================================================*
     H*                                                               *
     H*  NOTE:  @@RRN WILL NEED TO BE PASSED IN AS THE RRN OF WLPEP.  *
     H*         @@RRN2 WILL NEED TO BE PASSED IN AS THE RRN OF WDELP. *
     H*                                                               *
     H*===============================================================*
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  92 - GENERAL F KEY PRESSED                                   *
     H*  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  20 - (WCR0502) Certificate Holder number cannot be <= zero   *
     H*  21 - (WC30005) Certificate Holder number previously used     *
     H*  22 - (WCR0503) Name must be entered                          *
     H*  23 - (WCR0503) Address 1 must be entered                     *
     H*  24 - (WCR0503) City must be entered                          *
     H*  25 - (WCR0504) State not valid                               *
     H*  26 - (WCR0502) Zip 1 must be >= zero                         *
     H*  27 - (WCR0504) State not valid                               *
     H*  28 - Condition the '-' in zip code                           *
     H*  POSITION CURSOR:                                             *
     H*  29 - SCNAM1                                                  *
     H*                                                               *
     H*===============================================================*
     FSR17304G  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FSTABL     IF   E           K DISK
     F*
     F*  Physical file definition (for maintaining record)
     FWLPEP     UF A E             DISK
     F                                     RENAME(PLP01:DBRCDF)
     FWLPEL     IF   E           K DISK
     F                                     INFDS(XACTDS)
     FWDELP     IF   E             DISK
     FWDF2L     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWTXCL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*================================================================
     D*  Table & Array Definitions
     D*================================================================
     D XXBL            S             50    DIM(5)                               Before line
     D XXAL            S             50    DIM(5)                               After line
     D*================================================================
     D*  Data Structure & Constant Definitions
     D*================================================================
     D*
     D***PY SCOPYMT01I
     D*
     D XACTDS          DS
     D  XXRRN                397    400B 0
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D*  Effective date array
     D*
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EFF.DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D*
     D*  Cancle date array
     D*
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D*  Midterm Endorsment data area
     D*
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('LA Professional Empl-
     D                                     oyer Organization')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D AUDIT           C                   CONST('Audited')
     D EST             C                   CONST('Estimated')
     D DELETD          C                   CONST('*Deleted')
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
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      ELFND         ##FND
     C                   PARM      ELEMP#        ##EMP#            9 0
     C                   PARM      ELDIV         ##DIV             5 0
     C                   PARM      ELFYR         ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL2        PLIST
     C                   PARM                    EMCO#
     C                   PARM                    EMFND
     C                   PARM                    EMEMP#
     C                   PARM                    EMDIV
     C                   PARM      LDFYR         ##FYR             3 0
     C                   PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  Alternate Employer file
     C*
     C*
     C     LEKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  Employer Master file
     C*
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  Group Master file
     C*
     C     FNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C*
     C*  Group Auxiliary file
     C*
     C     F2KY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C*
     C*  Rate Set Code file
     C*
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C*
     C*  System Table file
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C*  Cancellation Maintenance file
     C*
     C     XCKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELPFDT
     C*
     C     XCKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
     C*
     C*  Endors/DARTS Control Definition file
     C*
     C     DXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C*  Endorsement Request workfile
     C*
     C     ENKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    LDFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    FNEXDT        SYSDT
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    STKEY         XXKEY
     C     *LIKE         DEFINE    LPNAM1        S#NAME
     C     *LIKE         DEFINE    LPADR1        S#ADR1
     C     *LIKE         DEFINE    LPADR2        S#ADR2
     C     *LIKE         DEFINE    LPCTY         S#CTY
     C     *LIKE         DEFINE    LPST          S#ST
     C     *LIKE         DEFINE    LPZIP         S#ZIP
     C     *LIKE         DEFINE    LPPROJ        S#PROJ
     C     *LIKE         DEFINE    LPEMST        S#EMST
     C     *LIKE         DEFINE    ELPFDT        XXENDT
     C*
     C/COPY SCOPYMT01C
     C*
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
     C*
     C*  Alternate Employer # cannot have been used previously.
     C*
     C*  The name cannot be blank.
     C*
     C  N90SCNAM1        IFEQ      *BLANK
     C                   SETON                                        22  90
     C                   ENDIF                                                  SCNAM1=*BLANK
     C*
     C*  The first address line cannot be blank.
     C*
     C  N90SCADR1        IFEQ      *BLANK
     C                   SETON                                        23  90
     C                   ENDIF                                                  SCADR1=*BLANK
     C*
     C*  The city cannot be blank.
     C*
     C  N90SCCTY         IFEQ      *BLANK
     C                   SETON                                        24  90
     C                   ENDIF                                                  SCCTY=*BLANK
     C*
     C*  The state must be valid and cannot be blank.
     C*
     C                   MOVEL(P)  SCST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        25  90
     C*
     C*  The zip code cannot be zero.
     C*
     C  N90SCZIP         IFLE      *ZERO
     C                   SETON                                        26  90
     C                   ENDIF                                                  SCZIP<=*ZEROO
     C*
     C*  The state of employment must be valid and cannot be blank.
     C*
     C                   MOVEL(P)  SCAEST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        27  90
     C*
     C*  Save the old database values
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C     *IN99         IFEQ      *ON
     C                   CLEAR                   S#NAME
     C                   CLEAR                   S#ADR1
     C                   CLEAR                   S#ADR2
     C                   CLEAR                   S#CTY
     C                   CLEAR                   S#ST
     C                   CLEAR                   S#ZIP
     C                   CLEAR                   S#PROJ
     C                   CLEAR                   S#EMST
     C*
     C                   ELSE
     C*
     C                   MOVEL     LPNAM1        S#NAME
     C                   MOVEL     LPADR1        S#ADR1
     C                   MOVEL     LPADR2        S#ADR2
     C                   MOVEL     LPCTY         S#CTY
     C                   MOVE      LPST          S#ST
     C                   MOVE      LPZIP         S#ZIP
     C                   MOVEL     LPPROJ        S#PROJ
     C                   MOVE      LPEMST        S#EMST
     C*
     C                   ENDIF
     C*
     C*  Check to be sure the screen has not been updated since entry.
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90LPDT          IFGT      SAVEDT
     C     LPDT          OREQ      SAVEDT
     C     LPTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  LPDT>SAVEDT
     C*
     C*  We have passed all edits, continue to the next screen.
     C*
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
     C                   MOVE      *OFF          *IN30
     C*
     C                   MOVEL     LPNAM1        SCNAM1
     C                   MOVEL     LPADR1        SCADR1
     C                   MOVEL     LPADR2        SCADR2
     C                   MOVEL     LPCTY         SCCTY
     C                   MOVEL     LPST          SCST
     C                   MOVEL     LPZIP         SCZIP
     C                   MOVE      LPZIP         SCZIP2
     C*
     C     SCZIP2        IFNE      *ZEROS
     C                   MOVE      *OFF          *IN28
     C                   ENDIF
     C*
     C                   MOVEL     LPEMST        SCAEST
     C                   MOVEL     LPPROJ        SCPROJ
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVEL     SCNAM1        LPNAM1
     C                   MOVEL     SCADR1        LPADR1
     C                   MOVEL     SCADR2        LPADR2
     C                   MOVEL     SCCTY         LPCTY
     C                   MOVEL     SCST          LPST
     C                   MOVEL     SCZIP         LPZIP
     C                   MOVE      SCZIP2        LPZIP
     C                   MOVEL     SCAEST        LPEMST
     C                   MOVEL     SCPROJ        LPPROJ
     C*
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C*
     C                   Z-ADD     LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LPEFDT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     ELPFDT        LPEFDT
     C*
     C                   ENDIF                                                  LDENDA != 0
     C*
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
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   Z-ADD     0             I1                3 0
     C     @@MODE        IFEQ      '1'                                          INQUIRY
     C     @@MODE        OREQ      '2'
     C     @@MODE        OREQ      '3'
     C     @@MODE        OREQ      'D'
     C                   MOVE      '1'           *IN09
     C                   ENDIF
     C*
     C     @@MODE        IFEQ      'A'
     C                   MOVE      '1'           *IN40
     C                   ENDIF
     C*
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
     C*
     C*  Get Employer Detail record
     C*
     C     @@RRN2        CHAIN     WDELP                              60
     C*
     C                   Z-ADD     ELFYR         SCFYR
     C*
     C     ELFAF         IFEQ      'A'
     C                   MOVEL(P)  AUDIT         SCPYST
     C                   ELSE
     C                   MOVEL(P)  EST           SCPYST
     C                   ENDIF                                                   ELFAF=A
     C*
     C*  Get Employer Master record
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     C*
     C*  Compress EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
     C*
     C     LEKY1         CHAIN     WLPEL                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      'C'           @@MODE
     C                   MOVE      XXRRN         @@RRN
     C                   CLEAR                   PLP01
     C                   ELSE
     C                   MOVE      'A'           @@MODE
     C                   ENDIF
     C*
     C*  Get most recent cancel date
     C*
     C                   MOVE      *OFF          *IN41
     C                   Z-ADD     0             SCCXDT
     C                   Z-ADD     1             I1
     C*
     C     *ZERO         LOOKUP    EF(I1)                                 61
     C*
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
     C*
     C     I1            IFNE      0
     C*
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   MOVE      *ON           *IN41
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
     C*
     C                   ENDIF                                                  I1 IFNE 0
     C*
     C*  Get Group information
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C     F2KY          CHAIN     PF201                              60
     C*
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C*
     C     FNEXDT        IFNE      *ZEROS
     C     *IN09         ANDNE     *ON
     C     SYSDT         COMP      FNEXDT                             09  09
     C                   ENDIF                                                  FNEXDT<>*ZEROS
     C*
     C*  Get Agent/Producer information
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
     C*  Chain to Rate Set Code file
     C*
     C     RSKY          CHAIN     PRS01                              30
     C     *IN30         IFEQ      *OFF
     C*
     C     XCKY          SETGT     PXC01
     C*
     C     XCKY1         READPE    PXC01                                  31
     C*
     C     *IN31         IFEQ      *OFF
     C     XCDREM        ANDNE     *BLANKS
     C                   MOVE      *ON           *IN32
     C                   ELSE
     C                   MOVE      *OFF          *IN32
     C                   ENDIF                                                  XCDREM IFNE *BLANKS
     C*
     C                   ENDIF                                                  *IN30 = *OFF
     C*
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
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C*
     C     LPNAM1        IFNE      S#NAME
     C     LPADR1        ORNE      S#ADR1
     C     LPADR2        ORNE      S#ADR2
     C     LPCTY         ORNE      S#CTY
     C     LPST          ORNE      S#ST
     C     LPZIP         ORNE      S#ZIP
     C     LPPROJ        ORNE      S#PROJ
     C     LPEMST        ORNE      S#EMST
     C*
     C                   MOVEL     Q1USER        LPUSER
     C                   MOVEL     Q1PGM         LPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LPDT
     C                   MOVEL     DSTIME        LPTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  WV changed
     C*
     C                   ELSE
     C*
     C                   MOVEL     Q1USER        LPUSER
     C                   MOVEL     Q1PGM         LPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LPDT
     C                   MOVEL     DSTIME        LPTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN99=*OFF
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C*
     C                   Z-ADD     ELCO#         LPCO#
     C                   Z-ADD     ELFND         LPFND
     C                   Z-ADD     ELEMP#        LPEMP#
     C                   Z-ADD     ELDIV         LPDIV
     C                   Z-ADD     ELFYR         LPFYR
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        LPIUSR
     C                   MOVEL     Q1USER        LPUSER
     C                   MOVEL     Q1PGM         LPIPGM
     C                   MOVEL     Q1PGM         LPPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL     DSTIME        LPITM
     C                   MOVEL     DSTIME        LPTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LPIDT
     C                   MOVEL     SRDTO         LPDT
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      WLPEP
     C*
     C                   MOVE      '1'           *INKC
     C                   ENDSR
