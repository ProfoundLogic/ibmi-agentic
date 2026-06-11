     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*================================================================
     H/TITLE W0817R02 - WORK WITH EXCLUDED WORKPLACE INFORMATION
     H*================================================================
     H* DESCRIPTION:  This program will allow a user to change, view  *
     H*               or add records in the Excluded Workplace file.  *
     H*                                                               *
     H* CALLED FROM: W0817R - Work With Excluded Workplaces           *
     H* CALLS TO   : None                                             *
     H*                                                               *
     H* CREATED    : 07/03/00                                         *
     H* PROGRAMMER : C:90                                             *
     H* PROJECT    : WA096/21925                                      *
     H*===============================================================*
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/07/05 C:65  PROJECT: WC1202                                *
     H*                ADD PROCESSING TO POPULATE EXTENDED EMPLR NAMES*
     H*                                                               *
     H* ------------------------------------------------------------- *
     H*  NOTE: Please add comments to the top of Revisions            *
     H* ------------------------------------------------------------- *
     H*===============================================================*
     H* INDICATOR USAGE:                                              *
     H*  Global                                                       *
     H*  09 - Inquiry mode                                            *
     H* N09 - Update mode                                             *
     H*  31 - Midterm mode                                            *
     H*  40 - Add mode                                                *
     H*  90 - General error                                           *
     H*  93 - Screen was updated while user was in it                 *
     H*                                                               *
     H*  SCR01:                                                       *
     H*   Error:                                                      *
     H*                                                               *
     H*===============================================================*
     F** Display File
     FW0817F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F** Employer Detail File
     FWDELP     IF   E             DISK
     F*
     F** Group Auxiliary File
     FWDF2L     IF   E           K DISK
     F*
     F** Endorsement/DARTS Information File
     FWMDXL     IF   E           K DISK
     F*
     F** Employer Master File
     FWMEML     IF   E           K DISK
     F*
     F** Employer Attached Endorsement File
     FWMENL3    IF A E           K DISK
     F*
     F** Group Master File
     FWMFNL     IF   E           K DISK
     F*
     F** Excluded Workplace File
     FWMEWL1    IF   E           K DISK
     F*
     F** Excluded Workplace physical file
     FWMEWP     UF A E             DISK
     F                                     RENAME(PEW01:DBRCDF)
     F                                     INFDS(DBFDS)
     F*
     F** Midterm Endorsement Table File
     FWTMYP     IF   E             DISK
      /COPY SCOPYMT01D
     D*=========================================================================
     D*  Array & Table definitions
     D*=========================================================================
     D XXBL            S             50    DIM(5)                               Before line
     D XXAL            S             50    DIM(5)                               After line
     D*=========================================================================
     D*  Data Structures & constants definitions
     D*=========================================================================
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D*  Effective dates
     D*
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               Eff. date
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D*
     D*  Cancellation dates
     D*
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)                               Cancel date
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D*
     D*  Midterm Endorsment data area
     D*
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     D*=========================================================================
     D*  /COPY required constants
     D*=========================================================================
     D OPTNM           C                   CONST('Excluded Workplace I-
     D                                     nformation')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D AUDIT           C                   CONST('Audited')
     D EST             C                   CONST('Estimated')
     D #OFSCR          C                   CONST(1)
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*=========================================================================
     C*  PLIST Definitions
     C*=========================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
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
     C*
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
     C*=========================================================================
     C*  KLIST Definitions
     C*=========================================================================
     C*
     C*  Endors/DARTS Control Definition file
     C*
     C     DXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C*  Employer Master file
     C*
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
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
     C*  Excluded Workplace Information file
     C*
     C     EWKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    SCFYR
     C                   KFLD                    SCWPL#
     C*
     C     EWKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*=========================================================================
     C*  *LIKE DEFN definitions
     C*=========================================================================
     C     *LIKE         DEFINE    FNEXDT        SYSDT
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    EWWPL#        XXWPL#
     C     *LIKE         DEFINE    EMEFD1        XXEFDT
     C     *LIKE         DEFINE    EWWPL#        S#WPL#
     C     *LIKE         DEFINE    EWWRK1        S#WRK1
     C     *LIKE         DEFINE    EWWRK2        S#WRK2
     C     *LIKE         DEFINE    EWWRK3        S#WRK3
     C     *LIKE         DEFINE    EWWRK4        S#WRK4
     C     *LIKE         DEFINE    EWWRK5        S#WRK5
     C     *LIKE         DEFINE    ELPFDT        XXENDT
     C                   Z-ADD     0             I1                3 0
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
     C     EWWPL#        IFNE      S#WPL#
     C     EWWRK1        ORNE      S#WRK1
     C     EWWRK2        ORNE      S#WRK2
     C     EWWRK3        ORNE      S#WRK3
     C     EWWRK4        ORNE      S#WRK4
     C     EWWRK5        ORNE      S#WRK5
     C*
     C*  Get Endors/DARTS information
     C*
     C     DXKY          CHAIN     PDX01                              60
     C*
     C     @@RRN2        CHAIN     WDELP                              60
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   CALL      AGTAPI        AGTPL2
     C*
     C*  Setup for next request number
     C*
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
     C*** BEGIN ADD ***  03/07/05  ************************************
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
     C*** END ADD *****  03/07/05  ************************************
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
     C                   MOVE      'S'           ENMSFL
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
     C*  Write the records
     C*
     C                   Z-ADD     1             I                 1 0
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
     C                   ADD       1             ENSEQ#
     C                   ENDDO
     C*
     C                   ENDIF                                                  WVWAV# <> S#WAV#
     C*
     C                   ENDIF                                                  LDENDA <>0,' '
     C*
     C                   ENDSR
     C*=========================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*=========================================================================
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     EWSTAT        IFEQ      'I'
     C                   MOVE      'A'           EWSTAT
     C                   ELSE
     C                   MOVE      'I'           EWSTAT
     C                   ENDIF                                                  WVSTAT=I
     C*
     C                   MOVEL     Q1USER        EWUSER
     C                   MOVEL     Q1PGM         EWPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EWDT
     C*
     C                   MOVEL     DSTIME        EWTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  CHGSTS
     C*=========================================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*=========================================================================
     C     DLTPRE        BEGSR
     C*
     C*
     C*  Save the old database values
     C*
     C                   MOVE      EWWPL#        S#WPL#
     C                   MOVE      EWWRK1        S#WRK1
     C                   MOVE      EWWRK2        S#WRK2
     C                   MOVE      EWWRK3        S#WRK3
     C                   MOVE      EWWRK4        S#WRK4
     C                   MOVE      EWWRK5        S#WRK5
     C*
     C                   CLEAR                   EWWPL#
     C                   CLEAR                   EWWRK1
     C                   CLEAR                   EWWRK2
     C                   CLEAR                   EWWRK3
     C                   CLEAR                   EWWRK4
     C                   CLEAR                   EWWRK5
     C*
     C*  Create Mid-Term Endorsement records
     C*
     C                   EXSR      CKCHGS
     C*
     C                   ENDSR                                                  DLTPRE
     C*=========================================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*=========================================================================
     C     DSPLYR        BEGSR
     C*
     C*
     C                   ENDSR                                                  DSPLYR
     C*=========================================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*=========================================================================
     C     ERRCK1        BEGSR
     C*
     C*  Waiver number cannot be <= zero
     C*
     C  N90EWWPL#        IFLE      *ZERO
     C                   SETON                                        20  90
     C                   ENDIF                                                  WVWAV#<=*ZERO
     C*
     C*  Waiver effective date must be entered
     C*
     C  N90SCEFDT        IFEQ      *ZERO
     C                   SETON                                        22  90
     C                   ENDIF                                                  SCEFDT = *ZERO
     C*
     C*  Waiver effective date must be valid
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        23  90
     C                   ELSE
     C                   MOVEL     SRDTO         SCEFDT
     C                   ENDIF                                                  SRERR = *ON
     C*
     C                   ENDIF
     C*
     C*  Waiver effective date must be between employer effective dates
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   SETON                                        24  90
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXENDT
     C*
     C     XXENDT        IFGE      ELPFDT
     C     XXENDT        ANDLT     ELPTDT
     C                   SETOFF                                       24  90
     C                   ENDIF                                                  XXENDT GE ELPFDT
     C*
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  Check to see if waiver # was used previously
     C*
     C  N90@@MODE        IFEQ      'A'
     C*
     C     EWKY          CHAIN     PEW01                              60
     C*
     C  N60              SETON                                        21  90
     C*
     C                   ENDIF                                                  @@MODE=A
     C*
     C* Excluded workplace line 1 cannot be blank
     C*
     C  N90SCWRK1        IFEQ      *BLANKS
     C                   SETON                                        25  90
     C                   ENDIF
     C*
     C*  Check to be sure the screen has not been updated since entry
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90EWDT          IFGT      SAVEDT
     C     EWDT          OREQ      SAVEDT
     C     EWTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  WVDT>SAVEDT
     C*
     C*  We have passed all edits, goto the next screen
     C*
     C                   ENDSR                                                  ERRCK1
     C*=========================================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*=========================================================================
     C     ERRCK2        BEGSR
     C*
     C*
     C                   ENDSR                                                  ERRCK2
     C*=========================================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*=========================================================================
     C     ERRCK3        BEGSR
     C*
     C*
     C                   ENDSR                                                  ERRCK3
     C*=========================================================================
     C* KJDO - SUBROUTINE FOR F10
     C*=========================================================================
     C*
     C*  F10=TABLE SEARCH
     C*
     C     KJDO          BEGSR
     C                   ENDSR                                                  KJDO
     C*=========================================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*=========================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      *OFF          *IN80
     C     ELWVTP        IFEQ      'S'
     C                   MOVE      *ON           *IN80
     C                   ENDIF                                                   ELWVTP = 'S
     C*
     C*  Check for midterm processing to determine waiver date
     C*
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C                   SETON                                        31        Midterm
     C                   ENDIF
     C*
     C                   SELECT
     C     *IN40         WHENEQ    *ON
     C*
     C                   MOVEL     'A'           EWSTAT
     C                   Z-ADD     XXWPL#        EWWPL#
     C*
     C     *IN31         IFEQ      *ON
     C                   Z-ADD     LDEEDT        SCEFDT
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   ENDIF                                                  Midterm
     C*
     C     @@MODE        WHENEQ    'C'
     C*
     C     *IN31         IFEQ      *ON
     C                   Z-ADD     LDEEDT        SCEFDT
     C                   ELSE
     C                   Z-ADD     EWEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SCEFDT        IFEQ      *ZEROS
     C                   SETOFF                                       31
     C                   ENDIF                                                  Midterm
     C*
     C                   ENDIF                                                  Midterm
     C*
     C     *IN09         WHENEQ    *ON
     C                   Z-ADD     EWEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   SETON                                        31
     C*
     C                   ENDSL
     C*
     C                   Z-ADD     EWWPL#        SCWPL#
     C                   MOVEL     EWWRK1        SCWRK1
     C                   MOVEL     EWWRK2        SCWRK2
     C                   MOVEL     EWWRK3        SCWRK3
     C                   MOVEL     EWWRK4        SCWRK4
     C                   MOVEL     EWWRK5        SCWRK5
     C*
     C                   ADD       1             XXWPL#
     C*
     C                   ENDSR                                                  MVDBSC
     C*=========================================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*=========================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCWRK1        EWWRK1
     C                   MOVEL     SCWRK2        EWWRK2
     C                   MOVEL     SCWRK3        EWWRK3
     C                   MOVEL     SCWRK4        EWWRK4
     C                   MOVEL     SCWRK5        EWWRK5
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EWEFDT
     C*
     C                   ENDSR                                                  MVSCDB
     C*=========================================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*=========================================================================
     C     OTHERR        BEGSR
     C                   ENDSR                                                   OTHERR
     C*=========================================================================
     C* OTHFKS - Subroutine for processing other function keys
     C*=========================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*=========================================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*=========================================================================
     C     PGMEND        BEGSR
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*=========================================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*=========================================================================
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
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
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
     C*  Get the most recent cancel date, if any
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
     C*  Default to latest # in ADD mode
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   MOVE      *ON           *IN60
     C*
     C     EWKY1         SETGT     PEW01
     C     EWKY1         READPE    PEW01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     EWWPL#        ADD       1             XXWPL#
     C                   ELSE
     C                   Z-ADD     1             XXWPL#
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  @@MODE=A
     C*
     C*  Save the old database values
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C     *IN99         IFEQ      *ON
     C                   CLEAR                   S#WPL#
     C                   CLEAR                   S#WRK1
     C                   CLEAR                   S#WRK2
     C                   CLEAR                   S#WRK3
     C                   CLEAR                   S#WRK4
     C                   CLEAR                   S#WRK5
     C*
     C                   ELSE
     C*
     C                   MOVE      EWWPL#        S#WPL#
     C                   MOVE      EWWRK1        S#WRK1
     C                   MOVE      EWWRK2        S#WRK2
     C                   MOVE      EWWRK3        S#WRK3
     C                   MOVE      EWWRK4        S#WRK4
     C                   MOVE      EWWRK5        S#WRK5
     C*
     C                   ENDIF
     C*
     C*
     C                   ENDSR                                                  PGMSTR
     C*=========================================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*=========================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*=========================================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*=========================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C*
     C     EWWPL#        IFNE      S#WPL#
     C     EWWRK1        ORNE      S#WRK1
     C     EWWRK2        ORNE      S#WRK2
     C     EWWRK3        ORNE      S#WRK3
     C     EWWRK4        ORNE      S#WRK4
     C     EWWRK5        ORNE      S#WRK5
     C*
     C                   MOVEL     Q1USER        EWUSER
     C                   MOVEL     Q1PGM         EWPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EWDT
     C                   MOVEL     DSTIME        EWTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  WV changed
     C*
     C                   ELSE
     C*
     C                   MOVEL     Q1USER        EWUSER
     C                   MOVEL     Q1PGM         EWPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EWDT
     C                   MOVEL     DSTIME        EWTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  Midterm
     C*
     C*  Create Mid-Term Endorsement records
     C*
     C                   EXSR      CKCHGS
     C*
     C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*=========================================================================
     C* WRITER - SUBROUTINE FOR WRITING DATA BASE RECORDS
     C*=========================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   Z-ADD     ELCO#         EWCO#
     C                   Z-ADD     ELFND         EWFND
     C                   Z-ADD     ELEMP#        EWEMP#
     C                   Z-ADD     ELDIV         EWDIV
     C                   Z-ADD     ELFYR         EWFYR
     C                   Z-ADD     SCWPL#        EWWPL#
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        EWIUSR
     C                   MOVEL     Q1USER        EWUSER
     C                   MOVEL     Q1PGM         EWIPGM
     C                   MOVEL     Q1PGM         EWPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL     DSTIME        EWITM
     C                   MOVEL     DSTIME        EWTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EWIDT
     C                   MOVEL     SRDTO         EWDT
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMEWP
     C*
     C*  Create Mid-Term Endorsement records
     C*
     C                   EXSR      CKCHGS
     C*
     C                   ENDSR                                                  WRITER
