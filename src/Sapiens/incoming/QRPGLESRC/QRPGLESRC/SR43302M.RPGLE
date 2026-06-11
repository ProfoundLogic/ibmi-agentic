     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*===============================================================*
     H*===============================================================*
     H/TITLE SR43302M - EMPLOYEE LEASING ENDORSEMENT
     H*===============================================================*
     H* DESCRIPTION:                                                  *
     H*   This program will allow a user to change, view or add       *
     H*   records in the Professional Employer file.                  *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   SR43302  - WORK WITH EMPLOYEE LEASING                       *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H*                                                               *
     H* CREATED   : 12/13/06                                          *
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
     H*  NOTE:  @@RRN WILL NEED TO BE PASSED IN AS THE RRN OF WNIEP.  *
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
     FSR43302G  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FSTABL     IF   E           K DISK
     F*
     F*  Physical file definition (for maintaining record)
     FWNIEP     UF A E             DISK
     F                                     RENAME(PIE01:DBRCDF)
     FWNIEL     IF   E           K DISK
     F                                     INFDS(XACTDS)
     FWDELP     IF   E             DISK
     FWDF2L     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWTXCL     IF   E           K DISK
     F** Employer Attached Endorsement File
     FWMENL3    IF A E           K DISK
     F*
     F*
     F** Midterm Endorsement Table File
     FWTMYP     IF   E             DISK
     F** Endorsement/DARTS Information File
     FWMDXL     IF   E           K DISK
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
     D OPTNM           C                   CONST('UTAH Employee Leasin-
     D                                     g')
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
     C     *LIKE         DEFINE    IECNAM        S#NAME
     C     *LIKE         DEFINE    IEADR1        S#ADR1
     C     *LIKE         DEFINE    IEADR2        S#ADR2
     C     *LIKE         DEFINE    IECTY         S#CTY
     C     *LIKE         DEFINE    IEST          S#ST
     C     *LIKE         DEFINE    IEZIP         S#ZIP
     C     *LIKE         DEFINE    IEFEIN        S#FEIN
     C     *LIKE         DEFINE    ELPFDT        XXENDT
     C*
     C/COPY SCOPYMT01C
     C*
     C*=========================================================================
     C*  CKCHGS - SUBROUTINE TO CREATE MID-TERM ENDORSEMENT RECORDS
     C*=========================================================================
     C     CKCHGS        BEGSR
     C*
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C*
     C     IECNAM        IFNE      S#NAME
     C     IEADR1        ORNE      S#ADR1
     C     IEADR2        ORNE      S#ADR2
     C     IECTY         ORNE      S#CTY
     C     IEST          ORNE      S#ST
     C     IEZIP         ORNE      S#ZIP
     C     IEFEIN        ORNE      S#FEIN
     C*
     C*  Get Endors/DARTS information
     C*
     C     DXKY          CHAIN     PDX01                              60
     C*
     C     @@RRN2        CHAIN     WDELP                              60
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   CALL      AGTAPI        AGTPL2
     C     ENKY          SETGT     PEN01
     C     ENKY          READPE    PEN01                                  60
     C   60              Z-ADD     *ZERO         ENREQ#
     C*
     C                   MOVE      EMCO#         ENCO#
     C                   MOVE      EMFND         ENFND
     C                   MOVE      EMEMP#        ENEMP#
     C                   MOVE      EMDIV         ENDIV
     C                   MOVE      LDFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#
     C*
     C                   Z-ADD     LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENEEDT
     C*
     C                   MOVEL     EMNAME        ENNAM1
     C                   MOVEL     EMFDNM        ENFDN1
     C*
     C                   MOVEL     EMNAME        ENENM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVE      EMNAM2        ENENM2
     C*
     C                   MOVEL     EMFDNM        ENEFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      EMFDN2        ENEFN2
     C*
     C                   MOVEL     EMCONT        ENCON1
     C                   MOVEL     EMATTN        ENATT1
     C                   MOVEL     EMCNT3        ENCN31
     C                   MOVE      EMFEIN        ENFEN1
     C                   MOVEL     EMTPOB        ENTOB1
     C                   MOVEL     EMMAD1        ENMA11
     C                   MOVEL     EMMAD2        ENMA21
     C                   MOVEL     EMMCTY        ENMCT1
     C                   MOVEL     EMMST         ENMST1
     C                   MOVEL     EMMZIP        ENMZP1
     C                   MOVEL     EMPAD1        ENPA11
     C                   MOVEL     EMPAD2        ENPA21
     C                   MOVEL     EMPCTY        ENPCT1
     C                   MOVEL     EMPST         ENPST1
     C                   MOVEL     EMPZIP        ENPZP1
     C*
     C                   MOVEL     S#NAME        ENOEN1                         NAME
     C                   MOVEL     S#ADR1        ENOM11                         ADR1
     C                   MOVEL     S#ADR2        ENOM21                         ADR2
     C                   MOVEL     S#CTY         ENOMC1                         CTY
     C                   MOVEL     S#ST          ENOMS1                         ST
     C                   MOVE      S#ZIP         ENOMZ1                         ZIP
     C*
     C                   MOVEL     IECNAM        ENOEN2                         NAME
     C                   MOVEL     IEADR1        ENOM12                         ADR1
     C                   MOVEL     IEADR2        ENOM22                         ADR2
     C                   MOVEL     IECTY         ENOMC2                         CTY
     C                   MOVEL     IEST          ENOMS2                         ST
     C                   MOVE      IEZIP         ENOMZ2                         ZIP
     C*
     C                   MOVE      *BLANKS       ENNCFL
     C                   MOVE      *BLANKS       ENADDF
     C                   MOVE      *BLANKS       ENDLDF
     C                   MOVE      *BLANKS       ENFIFL
     C                   MOVE      *BLANKS       ENCNFL
     C                   MOVE      *BLANKS       ENATFL
     C                   MOVE      *BLANKS       ENC3FL
     C                   MOVE      *BLANKS       ENTBFL
     C                   MOVE      *BLANKS       ENRIDF
     C                   MOVE      *BLANKS       ENNOBF
     C                   MOVE      *BLANKS       ENNOFL
     C*
     C                   MOVE      'A'           ENMSFL
     C*
     C                   MOVEL(P)  DXMISC        ENRMCD
     C*
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL     ##ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
     C*
     C     LDMYRR        CHAIN     PMY01                              60
     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3
     C*
     C*  Build up BEFORE line
     C*
     C                   MOVE      *BLANKS       XXBL
     C                   Z-ADD     1             I                 1 0
     C                   CAT       S#NAME:1      XXBL(I)
     C                   ADD       1             I
     C     S#ADR1        IFNE      *BLANKS
     C                   MOVEL     S#ADR1        XXBL(I)
     C                   ADD       1             I
     C                   ENDIF
     C     S#ADR2        IFNE      *BLANKS
     C                   MOVEL     S#ADR2        XXBL(I)
     C                   ADD       1             I
     C                   ENDIF
     C     S#CTY         IFNE      *BLANKS
     C     S#ST          ORNE      *BLANKS
     C     S#ZIP         ORNE      *ZEROS
     C                   MOVEL     S#ZIP         XXZIP5            5
     C                   MOVE      S#ZIP         XXZIP4            4
     C     S#CTY         CAT       ',':0         XXBL(I)
     C                   CAT       S#ST:1        XXBL(I)
     C                   CAT       XXZIP5:1      XXBL(I)
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXBL(I)
     C                   CAT       XXZIP4:0      XXBL(I)
     C                   ENDIF
     C                   ENDIF
     C*  Build up AFTER line
     C*
     C                   MOVE      *BLANKS       XXAL
     C                   Z-ADD     1             I
     C                   CAT       IECNAM:1      XXAL(I)
     C                   ADD       1             I
     C     IEADR1        IFNE      *BLANKS
     C                   MOVEL     IEADR1        XXAL(I)
     C                   ADD       1             I
     C                   ENDIF
     C     IEADR2        IFNE      *BLANKS
     C                   MOVEL     IEADR2        XXAL(I)
     C                   ADD       1             I
     C                   ENDIF
     C     IECTY         IFNE      *BLANKS
     C     IEST          ORNE      *BLANKS
     C     IEZIP         ORNE      *ZEROS
     C                   MOVEL     IEZIP         XXZIP5            5
     C                   MOVE      IEZIP         XXZIP4            4
     C     IECTY         CAT       ',':0         XXAL(I)
     C                   CAT       IEST:1        XXAL(I)
     C                   CAT       XXZIP5:1      XXAL(I)
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXAL(I)
     C                   CAT       XXZIP4:0      XXAL(I)
     C                   ENDIF
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
     C*
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
     C                   MOVE      F2CAR#        ENCAR1
     C                   Z-ADD     ELPFDT        ENPFD1
     C*
     C*  Write the records
     C*
     C                   Z-ADD     1             I
     C     I             DOWLE     5
     C*
     C     XXBL(I)       IFEQ      *BLANKS
     C     XXAL(I)       ANDEQ     *BLANKS
     C                   LEAVE
     C                   ENDIF
     C*
     C                   MOVEL     XXBL(I)       ENBLIN
     C                   MOVEL     XXAL(I)       ENALIN
     C*
     C                   WRITE     PEN01
     C*
     C*  Bump up necessary numeric fields for next write if required
     C*
     C                   ADD       1             I
     C                   ADD       1             ENTIME
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
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
     C*  FEIN cannot cannot be blank.
     C*
     C  N90SCFEIN        IFEQ      *ZEROS
     C                   SETON                                        27  90
     C                   ENDIF                                                  SCFEIN=*ZEROS
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
     C                   CLEAR                   S#FEIN
     C*
     C                   ELSE
     C*
     C                   MOVEL     IECNAM        S#NAME
     C                   MOVEL     IEADR1        S#ADR1
     C                   MOVEL     IEADR2        S#ADR2
     C                   MOVEL     IECTY         S#CTY
     C                   MOVE      IEST          S#ST
     C                   MOVE      IEZIP         S#ZIP
     C                   MOVE      IEFEIN        S#FEIN
     C*
     C                   ENDIF
     C*
     C*  Check to be sure the screen has not been updated since entry.
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90IEDT          IFGT      SAVEDT
     C     IEDT          OREQ      SAVEDT
     C     IETIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  IEDT>SAVEDT
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
     C                   MOVEL     IECNAM        SCNAM1
     C                   MOVEL     IEADR1        SCADR1
     C                   MOVEL     IEADR2        SCADR2
     C                   MOVEL     IECTY         SCCTY
     C                   MOVEL     IEST          SCST
     C                   MOVEL     IEZIP         SCZIP
     C                   MOVE      IEZIP         SCZIP2
     C                   MOVEL     IEFEIN        SCFEIN
     C*
     C     SCZIP2        IFNE      *ZEROS
     C                   MOVE      *OFF          *IN28
     C                   ENDIF
     C*
     C                   MOVEL     IEFEIN        SCFEIN
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVEL     SCNAM1        IECNAM
     C                   MOVEL     SCADR1        IEADR1
     C                   MOVEL     SCADR2        IEADR2
     C                   MOVEL     SCCTY         IECTY
     C                   MOVEL     SCST          IEST
     C                   MOVEL     SCZIP         IEZIP
     C                   MOVE      SCZIP2        IEZIP
     C                   MOVEL     SCFEIN        IEFEIN
     C*
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C*
     C                   Z-ADD     LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IEEFDT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     ELPFDT        IEEFDT
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
     C                   MOVE      'A'           @@MODE
     C     LEKY1         CHAIN     WNIEL                              60
     C     *IN60         DOWEQ     '0'
     C     IEFEIN        IFNE      *ZEROS
     C                   MOVE      'C'           @@MODE
     C                   MOVE      XXRRN         @@RRN
     C                   CLEAR                   PIE01
     C                   ENDIF
     C     LEKY1         READE     WNIEL                                  60
     C                   ENDDO
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
     C     IECNAM        IFNE      S#NAME
     C     IEADR1        ORNE      S#ADR1
     C     IEADR2        ORNE      S#ADR2
     C     IECTY         ORNE      S#CTY
     C     IEST          ORNE      S#ST
     C     IEZIP         ORNE      S#ZIP
     C     IEFEIN        ORNE      S#FEIN
     C*
     C                   MOVEL     Q1USER        IEUSER
     C                   MOVEL     Q1PGM         IEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IEDT
     C                   MOVEL     DSTIME        IETIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  WV changed
     C*
     C                   ELSE
     C*
     C                   MOVEL     Q1USER        IEUSER
     C                   MOVEL     Q1PGM         IEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IEDT
     C                   MOVEL     DSTIME        IETIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF
     C*
     C*  Create Mid-Term Endorsement records
     C*
     C                   EXSR      CKCHGS
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
     C                   Z-ADD     ELCO#         IECO#
     C                   Z-ADD     ELFND         IEFND
     C                   Z-ADD     ELEMP#        IEEMP#
     C                   Z-ADD     ELDIV         IEDIV
     C                   Z-ADD     ELFYR         IEFYR
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        IEIUSR
     C                   MOVEL     Q1USER        IEUSER
     C                   MOVEL     Q1PGM         IEIPGM
     C                   MOVEL     Q1PGM         IEPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL     DSTIME        IEITM
     C                   MOVEL     DSTIME        IETIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IEIDT
     C                   MOVEL     SRDTO         IEDT
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      WNIEP
     C*
     C*  Create Mid-Term Endorsement records
     C*
     C                   EXSR      CKCHGS
     C                   MOVE      '1'           *INKC
     C                   ENDSR
