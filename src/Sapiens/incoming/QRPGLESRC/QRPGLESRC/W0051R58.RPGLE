     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0051R58 - WAIVER INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:  This program will allow a user to change the   *
     H*                Employer Waiver Information.                   *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0051R - WORK WITH EMPLOYERS POLICY YEARS                  *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 05/12/99  PROGRAMMER: C:113 PROJECT: CM045          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/15/05 C:136 PROJECT: 133WBS                                *
     H*                ADD MAXIMUM WAIVER SURCHARGE FIELD.            *
     H*                                                               *
     H* 07/05/00 C:09  PROJECT WC577/WA107/SEQ# 22877                 *
     H*                Automatically change default stock discount    *
     H*                info and default waiver info based on rate     *
     H*                set code assigned.                             *
     H*                                                               *
     H* 05/05/00 C:113 PROJECT: SUPPORT #22814                        *
     H*                Fixed to default mode, if mode passed in is    *
     H*                not A,C or D.                                  *
     H*                                                               *
     H* 03/28/00 C:113 PROJECT: SUPPORT #21942                        *
     H*                Removed edit to allow a percentage or a flat   *
     H*                charge if waiver type is specific per KK.      *
     H*                                                               *
     H* 03/07/00 C:113 PROJECT: SUPPORT #22106                        *
     H*                Fixed processing in add mode to update         *
     H*                existing WDELP record instead of writing a new *
     H*                one.                                           *
     H*                                                               *
     H* 02/16/00 C:113 PROJECT: CM045                                 *
     H*                Fixed to allow processing in Delete mode.      *
     H*                                                               *
     H* 10/11/99 C:113 PROJECT: CM045                                 *
     H*                Changed to allow a blank premium level flag if *
     H*                the waiver type is blank.                      *
     H*                                                               *
     H* 09/29/99 C:113 PROJECT: 48WPK                                 *
     H*                Allow program to be called in add, change or   *
     H*                display mode from Employer Detail.             *
     H*                                                               *
     H* ------------------------------------------------------------- *
     H*  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
     H* ------------------------------------------------------------- *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  92 - GENERAL F KEY PRESSED                                   *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  20 - (WCR3142) Waiver type must be 'S', 'B' or blank.        *
     H*  21 - (WCR0547) Waiver percentage < 0.                        *
     H*  22 - (WCR0547) Waiver charge < 0.                            *
     H*  23 - (WCR0590) Waiver % and flat charge mutually exclusive.  *
     H*  24 - (WCR3143) Waiver type = S && waiver% != 0.              *
     H*  25 - (WCR3143) Waiver type = S && flat charge != 0.          *
     H*  26 - (WCR3144) Waiver type = B && waiver% and flat charge =0.*
     H*                                                               *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*****************************************************************
     FW0051F58  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMEMP     IF   E             DISK
     FWMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
     FWDELP     UF   E             DISK    USROPN
     F                                     RENAME(PEL01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMFNL     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*================================================================
     D*  Array and Table Definitions
     D*================================================================
     D*================================================================
     D*  Data Structure and Constant Definitions
     D*================================================================
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D*
     D OPTNM           C                   CONST('Waiver Information')
     D #OFSCR          C                   CONST(1)
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EFFECTIVE DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D*
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)                               CANCEL DATE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0

     C*================================================================
     C*  PLIST Definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
      *** BEGIN ADD *** 07/05/00 ***********************************************
     C                   PARM                    @@RCCG            1
      *** END ADD ***** 07/05/00 ***********************************************
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
     C                   PARM      EMFND         ##FND
     C                   PARM      EMEMP#        ##EMP#            9 0
     C                   PARM      EMDIV         ##DIV             5 0
     C                   PARM                    ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*================================================================
     C*  KLIST Definitions
     C*================================================================
     C*
     C*  Employer Master
     C*
     C     EMKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C*
     C*  Group Master file
     C*
     C     FNKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C*
     C*  Statistical Reporting/Rating file
     C*
     C     RRKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    XXST              2
     C                   KFLD                    ELPFDT
     C*
     C     RRKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    XXST
     C*
     C*  Rate Set Code file
     C*
     C     RSKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    ELRTCD
     C*================================================================
     C*  *LIKE DEFN Definitions
     C*================================================================
     C*
     C     *LIKE         DEFINE    EMCO#         KKCO#
     C     *LIKE         DEFINE    EMFND         KKFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    ELWVTP        XXWVTP
     C     *LIKE         DEFINE    ELWVPC        XXWVPC
     C     *LIKE         DEFINE    ELFCHG        XXFCHG
     C     *LIKE         DEFINE    RRWDMN        XXWMIN
     C     *LIKE         DEFINE    RRWVFL        XXWPL
     C*
     C/COPY SCOPYMT01C
     C*
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C*  The waiver type must be 'B', 'S' or blank.
     C*
     C  N90SCWVTP        IFNE      'B'
     C     SCWVTP        ANDNE     'S'
     C     SCWVTP        ANDNE     ' '
     C                   SETON                                        20  90
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  Waiver percentage cannot be less than zero.
     C*
     C  N90SCWVPC        IFLT      0
     C                   SETON                                        21  90
     C                   ENDIF                                                  SCWVPC < 0
     C*
     C*  Flat waiver charge cannot be less than zero.
     C*
     C  N90SCFCHG        IFLT      0
     C                   SETON                                        22  90
     C                   ENDIF                                                  SCFCHG < 0
     C*
     C*  Waiver percentage and flat charge are mutually exclusive.
     C*
     C  N90SCWVPC        IFGT      0
     C     SCFCHG        ANDGT     0
     C                   SETON                                        23  90
     C                   ENDIF                                                  SCWVPC > 0
     C*** BEGIN DEL *** 03/28/00 ***********************************************
     C**********
     C***If*the*waiver type is not blanket 'B', cannot have a
     C***percentage or flat charge.
     C**********
     C**N90***** SCWVTP    IFNE 'B'
     C********** SCWVPC    ANDGT0
     C**********           SETON                     24  90
     C**********           ENDIF                           SCWVTP = S
     C**********
     C**N90***** SCWVTP    IFNE 'B'
     C********** SCFCHG    ANDGT0
     C**********           SETON                     25  90
     C**********           ENDIF                           SCWVTP = S
     C*** END   DEL *** 03/28/00 ***********************************************
     C*
     C*  If the waiver type is blanket 'B', percentage and flat charge
     C*  cannot both be blank.
     C*
     C  N90SCWVTP        IFEQ      'B'
     C     SCWVPC        ANDEQ     0
     C     SCFCHG        ANDEQ     0
     C                   SETON                                        26  90
     C                   ENDIF                                                  SCWVTP = B
     C*
     C*  Premium Level flag must be blank if the Waiver Type is blank.
     C*
     C  N90SCWVTP        IFNE      *BLANK
      *
     C     SCWPL         IFNE      'J'
     C     SCWPL         ANDNE     'E'
     C     SCWPL         ANDNE     'S'
     C     SCWPL         ANDNE     'D'
     C     SCWPL         ANDNE     'M'
     C                   SETON                                        27  90
     C                   ENDIF                                                  SCWPL != JESDM
      *
     C                   ELSE
     C     SCWPL         IFNE      *BLANK
     C                   SETON                                        28  90
      *
     C                   ENDIF                                                  SCWPL != BLANK
      *
     C                   ENDIF                                                  SCWVTP != BLANK
     C*
     C*  Check to be sure the screen has not been updated since entry.
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90ELDT          IFGT      SAVEDT
     C     ELDT          OREQ      SAVEDT
     C     ELTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  ELDT>SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK3
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=SEARCH
     C*
     C     KJDO          BEGSR
     C                   ENDSR                                                  KJDO
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C*                    MOVE ELFYR     SCFYR
     C*
     C   40              MOVE      XXWVTP        SCWVTP
     C   40              Z-ADD     XXWVPC        SCWVPC
     C   40              Z-ADD     XXFCHG        SCFCHG
     C   40              Z-ADD     XXWMIN        SCWMIN
     C   40              MOVE      XXWPL         SCWPL
     C*
     C  N40              MOVE      ELWVTP        SCWVTP
     C  N40              Z-ADD     ELWVPC        SCWVPC
     C  N40              Z-ADD     ELFCHG        SCFCHG
     C  N40              MOVE      ELWMIN        SCWMIN
     C  N40              MOVE      ELWVFL        SCWPL
      *** BEG ADD ***** 07/15/05 ***********************************************
     C                   MOVE      ELWMAX        SCWMAX
      *** END ADD ***** 07/15/05 ***********************************************
     C*
      *** BEGIN ADD *** 07/05/00 ***********************************************
     C     Q1PRMS        IFGT      5
     C     @@RCCG        IFEQ      'Y'
     C*
     C*  IF RATE SET CODE WAS CHANGED, RELOAD THE DEFAULTS
     C*
     C                   MOVE      XXWVTP        SCWVTP
     C                   Z-ADD     XXWVPC        SCWVPC
     C                   Z-ADD     XXFCHG        SCFCHG
     C                   Z-ADD     XXWMIN        SCWMIN
     C                   MOVE      XXWPL         SCWPL
     C                   END
     C                   END
      *** END ADD ***** 07/05/00 ***********************************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   MOVE      SCWVTP        ELWVTP
     C                   Z-ADD     SCWVPC        ELWVPC
     C                   Z-ADD     SCFCHG        ELFCHG
     C                   MOVE      SCWMIN        ELWMIN
     C                   MOVE      SCWPL         ELWVFL
      *** BEG ADD ***** 07/15/05 ***********************************************
     C                   MOVE      SCWMAX        ELWMAX
      *** END ADD ***** 07/15/05 ***********************************************
     C*
     C                   ENDSR                                                  MVSCDB
     C*
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C*
     C     *INKC         CASEQ     *ON           FINISH
     C                   ENDCS
     C*
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C*
     C                   CLOSE     WDELP
     C*
     C                   ENDSR                                                  PGMEND
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C     PGMSTR        BEGSR
     C*
      *** BEGIN ADD *** 09/29/99 ***********************************************
     C                   OPEN      WDELP
      *** END   ADD *** 09/29/99 ***********************************************
     C*
     C     @@MODE        COMP      'A'                                    40    ADD
      *** BEGIN ADD *** 09/29/99 ***********************************************
     C     @@MODE        COMP      'D'                                    09    INQUIRY
      *** END   ADD *** 09/29/99 ***********************************************
      *** BEGIN DEL *** 02/16/00 ***********************************************
     C**********
     C********** *IN09     IFNE *ON
     C********** *IN40     ANDNE*ON
     C**********           MOVE 'C'       @@MODE
     C**********           ENDIF
      *** END   DEL *** 02/16/00 ***********************************************
      *** BEGIN ADD *** 05/05/00 ***********************************************
     C     *IN09         IFNE      *ON
     C     *IN40         ANDNE     *ON
     C     @@MODE        ANDNE     'E'
     C                   MOVE      'C'           @@MODE
     C                   ENDIF
      *** END   ADD *** 05/05/00 ***********************************************
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C*   Get Number of Employer Divisions to display in Screen Header
     C*
     C     @@RRN2        CHAIN     PEM01                              60
     C                   Z-ADD     EMCO#         KKCO#
     C                   Z-ADD     EMFND         KKFND
     C                   Z-ADD     EMEMP#        KKEMP#
     C*
     C     EMKY2         CHAIN     PEM02                              60
     C                   Z-ADD     0             SC#DIV
     C     *IN60         DOWEQ     *OFF
     C                   ADD       1             SC#DIV
     C     EMKY2         READE     PEM02                                  60
     C                   ENDDO
     C*
     C     @@RRN2        CHAIN     WMEMP                              99
     C*
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C*
     C*  Compress EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C*  Get the most recent cancel date, if any
     C*
     C                   MOVEL     XXEDN         SCEDN
     C                   MOVE      *OFF          *IN41
     C                   Z-ADD     0             SCCXDT
     C                   Z-ADD     1             I1                1 0
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
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*  Get Agent/Producer information
     C*
     C                   Z-ADD     *ZEROS        ##FYR
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
     C                   MOVE      *OFF          *IN42
     C*
     C     SCANAM        IFNE      *BLANKS
     C                   MOVE      *ON           *IN42
     C                   ENDIF
     C*
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C                   MOVE      ELFYR         SCFYR
     C*
     C     RSKY          CHAIN     PRS01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     RSJRST        XXST
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RRWDTP        XXWVTP
     C                   Z-ADD     RRWDPC        XXWVPC
     C                   Z-ADD     RRWDFC        XXFCHG
     C                   Z-ADD     RRWDMN        XXWMIN
     C                   MOVE      RRWVFL        XXWPL
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDSR                                                  PGMSTR
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
     C     PREOTH        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   MOVEL     DSTIME        ELTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
      *** BEGIN DEL *** 03/07/00 ***********************************************
      *** BEGIN ADD *** 09/29/99 ***********************************************
     C**********           CLEARDBRCDF
     C**********
     C**********           EXSR MVSCDB
     C**********
     C**********           MOVELQ1USER    ELUSER
     C**********           MOVELQ1PGM     ELPGM
     C**********           TIME           DSDTTM
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     ELDT
     C**********           MOVELDSTIME    ELTIME
     C**********
     C**********           WRITEDBRCDF
     C**********
     C**********           FEOD WDELP
      *** END   ADD *** 09/29/99 ***********************************************
      *** END   DEL *** 03/07/00 ***********************************************
      *
      *** BEGIN DEL *** 09/29/99 ***********************************************
     C**********           EXSR UPDATR
      *** END   DEL *** 09/29/99 ***********************************************
      *** BEGIN ADD *** 03/07/00 ***********************************************
     C                   EXSR      UPDATR
      *** END   ADD *** 03/07/00 ***********************************************
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  WRITER
