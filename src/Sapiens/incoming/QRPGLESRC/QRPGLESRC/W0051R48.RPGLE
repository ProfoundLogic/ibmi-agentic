     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0051R48 - RETRO INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:  This program will allow a user to change the   *
     H*                RETROSPECTIVE RATING INFORMATION.              *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0051R - WORK WITH EMPLOYERS POLICY YEARS                  *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 06/24/04  PROGRAMMER: C:65  PROJECT: 114WHO         *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/23/10 C:255 PROJECT:114WVZ                                 *
     H*                Changed the Heading for "Retro" to for both    *
     H*                "Retro/Retention".                             *
     H*                                                               *
     H* 01/24/06 C:65  PROJECT:  114WHO                               *
     H*                PROTECT SETTLEMENT AMOUNT AND DATE FIELDS AS   *
     H*                THEY SHOULD NOT BE OPEN TO EDIT.               *
     H*                                                               *
     H*****************************************************************
     FW0051F48  CF   E             WORKSTN
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
     FWMEXL     IF   E           K DISK
     F********BEG ADD******11/23/10*********************************************
     FWTABL     IF   E           K DISK
     F********END ADD******11/23/10*********************************************
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
     D OPTNM           C                   CONST('Retrospective Rati-
     D                                     ng Information')
     D********BEG CHG******11/23/10*********************************************
     D OPTNM1          C                   CONST('Retro/Retention Ra-
     D                                     ting Information')
     D********END CHG******11/23/10*********************************************
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
     C********BEG ADD******11/23/10*********************************************
     C*  Billing Schedule Code File
     C     BSKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    XXKEY            10
     C********END ADD******11/23/10*********************************************
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
     C     *LIKE         DEFINE    EXSRTN        XXSRTN
     C     *LIKE         DEFINE    EXELP         XXELP
     C     *LIKE         DEFINE    EXELPF        XXELPF
     C     *LIKE         DEFINE    EXRDF1        XXRDF1
     C     *LIKE         DEFINE    EXRDF2        XXRDF2
     C     *LIKE         DEFINE    EXRDF3        XXRDF3
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
     C*  MINIMUM FACTOR CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCMNFC        IFLT      0
     C                   SETON                                        21  90
     C                   ENDIF                                                  SCMNFC < 0
     C*
     C*  MAXIMUM FACTOR CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCMXFC        IFLT      0
     C                   SETON                                        22  90
     C                   ENDIF                                                  SCMXFC < 0
     C*
     C*  BASIC PREMIUM FACTOR CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCBSFC        IFLT      0
     C                   SETON                                        23  90
     C                   ENDIF                                                  SCBSFC < 0
     C*
     C*  TAX MULTIPLIER CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCTAX         IFLT      0
     C                   SETON                                        24  90
     C                   ENDIF                                                  SCTAX  < 0
     C*
     C*  TAX MULTIPLIER F-CLASSES CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCTAXF        IFLT      0
     C                   SETON                                        25  90
     C                   ENDIF                                                  SCTAXF < 0
     C*
     C*  LOSS CONVERSION FACTOR CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCLCVF        IFLT      0
     C                   SETON                                        26  90
     C                   ENDIF                                                  SCLCVF < 0
     C*
     C*  LOSS LIMITATION CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCRETN        IFLT      0
     C                   SETON                                        27  90
     C                   ENDIF                                                  SCRETN < 0
     C*
     C*  RETRO DEVELOPMENT FACTOR #1 CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCRDF1        IFLT      0
     C                   SETON                                        28  90
     C                   ENDIF                                                  SCRDF1 < 0
     C*
     C*  RETRO DEVELOPMENT FACTOR #2 CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCRDF2        IFLT      0
     C                   SETON                                        29  90
     C                   ENDIF                                                  SCRDF2 < 0
     C*
     C*  RETRO DEVELOPMENT FACTOR #3 CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCRDF3        IFLT      0
     C                   SETON                                        30  90
     C                   ENDIF                                                  SCRDF3 < 0
     C*
     C*  EXCESS LOSS PREMIUM FACTOR CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCELP         IFLT      0
     C                   SETON                                        31  90
     C                   ENDIF                                                  SCELP  < 0
     C*
     C*  EXCESS LOSS PREMIUM FACTOR F-CLASSES CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCELPF        IFLT      0
     C                   SETON                                        32  90
     C                   ENDIF                                                  SCELPF < 0
     C*
     C*  VERIFY SETTLEMENT DATE IF ENTERED.
     C*
     C*** BEGIN DEL ***  01/24/06  ************************************
     C**N90**    SCSTDT    IFNE *ZERO
     C*
     C*******              Z-ADDSCSTDT    SRDTI
     C*******              CALL 'SRCVTDAT'DATCHK
     C*******              MOVELSRDTO     SCSTDT
     C*
     C**N90**    SRERR     IFEQ *ON
     C*******              SETON                     33  90
     C*******              ENDIF                           SRERR=*ON
     C*
     C*******              ENDIF                           SCSTDT IFNE
     C*** END DEL *****  01/24/06  ************************************
     C*
     C*  FINAL SETTLEMENT FLAG MUST BE 'Y' OR BLANK.
     C*
     C  N90SCFSET        IFNE      'Y'
     C     SCFSET        ANDNE     *BLANK
     C                   SETON                                        34  90
     C                   ENDIF                                                  SCFSET IFNE
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
     C   40              Z-ADD     XXSRTN        SCRETN
     C   40              Z-ADD     XXELP         SCELP
     C   40              Z-ADD     XXELPF        SCELPF
     C   40              Z-ADD     XXRDF1        SCRDF1
     C   40              Z-ADD     XXRDF2        SCRDF2
     C   40              Z-ADD     XXRDF3        SCRDF3
     C*
     C  N40              Z-ADD     ELRETN        SCRETN
     C  N40              Z-ADD     ELELP         SCELP
     C  N40              Z-ADD     ELELPF        SCELPF
     C  N40              Z-ADD     ELRDF1        SCRDF1
     C  N40              Z-ADD     ELRDF2        SCRDF2
     C  N40              Z-ADD     ELRDF3        SCRDF3
     C*
     C                   Z-ADD     ELLCVF        SCLCVF
     C                   Z-ADD     ELTAX         SCTAX
     C                   Z-ADD     ELTAXF        SCTAXF
     C                   Z-ADD     ELBSFC        SCBSFC
     C                   Z-ADD     ELMNFC        SCMNFC
     C                   Z-ADD     ELMXFC        SCMXFC
     C                   Z-ADD     ELSTAM        SCSTAM
     C                   MOVE      ELFSET        SCFSET
     C                   Z-ADD     ELSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSTDT
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCRETN        ELRETN
     C                   Z-ADD     SCELP         ELELP
     C                   Z-ADD     SCELPF        ELELPF
     C                   Z-ADD     SCRDF1        ELRDF1
     C                   Z-ADD     SCRDF2        ELRDF2
     C                   Z-ADD     SCRDF3        ELRDF3
     C                   Z-ADD     SCLCVF        ELLCVF
     C                   Z-ADD     SCTAX         ELTAX
     C                   Z-ADD     SCTAXF        ELTAXF
     C                   Z-ADD     SCBSFC        ELBSFC
     C                   Z-ADD     SCMNFC        ELMNFC
     C                   Z-ADD     SCMXFC        ELMXFC
     C                   Z-ADD     SCSTAM        ELSTAM
     C                   MOVE      SCFSET        ELFSET
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELSTDT
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
     C                   OPEN      WDELP
     C*
     C     @@MODE        COMP      'A'                                    40    ADD
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C*
     C     *IN09         IFNE      *ON
     C     *IN40         ANDNE     *ON
     C     @@MODE        ANDNE     'E'
     C                   MOVE      'C'           @@MODE
     C                   ENDIF
     C*
     C********BEG DEL******11/23/10*********************************************
     C***********          MOVELOPTNM     ##DATA    P
     C*
     C***********          CALL 'SRCENTER'
     C***********          PARM           ##DATA 80
     C***********          PARM 40        ##LENG  30
     C***********          MOVEL##DATA    SCPRDS
     C***********          MOVE *ON       DSPTYP
     C********END DEL******11/23/10*********************************************
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
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EXSRTN        XXSRTN
     C                   Z-ADD     EXELP         XXELP
     C                   Z-ADD     EXELPF        XXELPF
     C                   Z-ADD     EXRDF1        XXRDF1
     C                   Z-ADD     EXRDF2        XXRDF2
     C                   Z-ADD     EXRDF3        XXRDF3
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C********BEG ADD******11/23/10*********************************************
     C                   MOVEL     ELBSCD        XXKEY
     C     BSKY          CHAIN     PBS01                              60
     C     BSLPGM        IFEQ      'R'
     C                   MOVE      *ON           *IN93
     C                   MOVEL(P)  OPTNM1        ##DATA
     C                   ELSE
     C                   MOVE      *OFF          *IN93
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C********END ADD******11/23/10*********************************************
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
     C                   EXSR      UPDATR
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  WRITER
