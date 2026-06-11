     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W2157R02 - BILLING SCHEDULE CODE AUXILIARY INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE BILLING SCH AUX FILE.    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W2157R - WORK WITH BILLING SCHEDULE CODES AUXILIARY        *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 05/13/97  PROGRAMMER: C:33  PROJECT: WC577          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/26/17 C:33  Support: 49595                                 *
     H*                Remove DEFINE(WEB)                             *
     H*                                                               *
     H* 08/26/03 C:61  SUPPORT WA099 37545                            *
     H*                Do not allow or display "F10=Search" for       *
     H*                Billing schedule code in change mode.          *
     H*                                                               *
     H* 04/11/03 C:136 PROJECT: WC1116                                *
     H*                ALLOW 'T - TWICE A MONTH' AS A VALID BILLING   *
     H*                AUX TYPE IF ACH PROCESSING IS ENABLED.         *
     H*                                                               *
     H* 02/18/03 C:136 PROJECT: WC1115                                *
     H*                ALLOW 'W - WEEKLY' AS A VALID BILLING AUX TYPE *
     H*                IF VENDOR PROCESSING IS ENABLED.               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  20 - (WCR0502) Code must be > zero {ADD mode only}           *
     H*  21 - (WCR0513) Code must be valid {ADD mode only}            *
     H*  22 - (WC30005) Code cannot be used more than once            *
     H*  23 - (WCR0852) Invalid Month/Day                             *
     H*  24 - (WCR0852) Invalid Type                                  *
     H*  POSITION CURSOR:                                             *
     H*  50 - (SCBSCD) Billing Schedule Code                          *
     H*  51 - (SCEFDT) Effective Date                                 *
     H*                                                               *
     H*  SCR02:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR03:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*****************************************************************
     FW2157F02  CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     INFDS(FIDS01)
     FWMBXP     UF A E             DISK
     F                                     RENAME(PBX01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMBXL1    IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWTBSP     IF   E             DISK
     F                                     RENAME(PBS01:PBS02)
     FWMFNP     IF   E             DISK
      **** BEG   ADD **** 02/18/03 *********************************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
      **** END   ADD **** 02/18/03 *********************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D OPTNM           C                   CONST('Billing Schedule Cod-
     D                                     e Aux Information')
     D #OFSCR          C                   CONST(1)
     D COMAPI          C                   CONST('WA001R')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C*
     C*   KEY LIST FOR THE BILLING SCHEDULE CODE FILE
     C*
     C     BSKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    XXKEY
     C*
      **** BEG   ADD **** 02/18/03 *********************************************
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      **** END   ADD **** 02/18/03 *********************************************
     C*
     C*   KEY LIST FOR THE BILLING SCHEDULE CODE AUXILIARY FILE
     C*
     C     BXKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C                   KFLD                    XXKEY
     C*
     C     *LIKE         DEFINE    BSKEY         XXKEY
     C*
     C/COPY SCOPYMT01C
     C*
     C*===============================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*===============================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     BXSTAT        IFEQ      'I'
     C                   MOVE      'A'           BXSTAT
     C                   ELSE
     C                   MOVE      'I'           BXSTAT
     C                   ENDIF                                                  BXSTAT=I
     C*
     C                   MOVEL     Q1USER        BXUSER
     C                   MOVEL     Q1PGM         BXPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BXDT
     C*
     C                   MOVEL     DSTIME        BXTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  CHGSTS
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
     C*  CODE MUST BE GREATER THAN ZERO (IF IN ADD MODE)
     C*
     C   40
B02  CANN90SCBSCD        IFLE      *ZERO
     C                   SETON                                        20  90
E02  C                   ENDIF                                                  SCBSCD<=*ZERO
     C*
     C*  CODE MUST BE VALID (IF IN ADD MODE)
     C*
     C                   MOVEL(P)  SCBSCD        XXKEY
     C                   MOVEL     *BLANKS       SCDESC
     C     BSKY          CHAIN     PBS01                              60
     C   40
     CANN90
     CANN60              MOVEL     BSDESC        SCDESC
     C   40
     CANN90*IN60         IFEQ      *ON
     C     BSSTAT        ORNE      'A'
     C                   SETON                                        21  90
     C                   ENDIF                                                  *IN60=*ON/BSSTAT<>A
     C*
     C*  CHECK TO BE SURE CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C   40
B02  CANN90BXKY          CHAIN     PBX01                              60
     C   40
     CANN90
     CANN60              SETON                                        22  90
     C*
     C*  THE DATE (MMDD) MUST BE VALID
     C*
     C                   Z-ADD     0             XXMTH             2 0
     C                   Z-ADD     0             XXDAY             2 0
     C                   MOVEL     SCEFDT        XXMTH
     C                   MOVE      SCEFDT        XXDAY
     C*
     C                   SELECT
     C*
     C     XXMTH         WHENEQ    4
     C     XXMTH         OREQ      6
     C     XXMTH         OREQ      9
     C     XXMTH         OREQ      11
     C                   Z-ADD     30            XXADJ
     C*
     C     XXMTH         WHENEQ    2
     C                   Z-ADD     29            XXADJ
     C*
     C                   OTHER
     C                   Z-ADD     31            XXADJ             2 0
     C*
     C                   ENDSL
     C*
     C  N90XXMTH         IFLT      1
     C     XXMTH         ORGT      12
     C     XXDAY         ORLT      1
     C     XXDAY         ORGT      XXADJ
     C                   SETON                                        23  90
     C                   ENDIF                                                  1>XXMTH>12
     C*
     C*  THE INSTALLMENT TYPE MUST BE VALID
     C*
      *** BEG CHG **** 04/11/03 ******************************************
     C**N90******SCTYPE    IFNE 'M'
     C  N90
     CANN41
     CANN42SCTYPE        IFNE      'M'
      *** END CHG **** 04/11/03 ******************************************
     C     SCTYPE        ANDNE     'Q'
     C     SCTYPE        ANDNE     'A'
     C     SCTYPE        ANDNE     'O'
     C                   SETON                                        24  90
     C                   ENDIF                                                  SCTYPE<>M/Q/A/O
     C*
      *** BEG ADD **** 02/18/03 ******************************************
     C  N90
     CANN42
     CAN 41SCTYPE        IFNE      'M'
     C     SCTYPE        ANDNE     'Q'
     C     SCTYPE        ANDNE     'A'
     C     SCTYPE        ANDNE     'O'
     C     SCTYPE        ANDNE     'W'
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCTYPE<>M/Q/A/O
      *** END ADD **** 02/18/03 ******************************************
      *** BEG ADD **** 04/11/03 ******************************************
     C  N90
     CAN 42SCTYPE        IFNE      'M'
     C     SCTYPE        ANDNE     'Q'
     C     SCTYPE        ANDNE     'A'
     C     SCTYPE        ANDNE     'O'
     C     SCTYPE        ANDNE     'W'
     C     SCTYPE        ANDNE     'T'
     C                   SETON                                        72  90
     C                   ENDIF                                                  SCTYPE<>M/Q/A/O
      *** END ADD **** 02/18/03 ******************************************
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90BXDT          IFGT      SAVEDT
     C     BXDT          OREQ      SAVEDT
     C     BXTIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  BXDT>SAVEDT
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
      *
      *
      *
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=TABLE SEARCH
     C*
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCBSCD'
      ***** BEG ADD ******* 08/26/03 **********************************
     C     *IN40         ANDEQ     *ON
      ***** END ADD ******* 08/26/03 **********************************
     C*
     C                   CALL      'W2155R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
      ***** BEG CHG ******* 08/26/03 **********************************
     C*********            PARM           ##RRN   90
     C                   PARM      *ZERO         ##RRN             9 0
      ***** END CHG ******* 08/26/03 **********************************
     C*
     C     ##RRN         CHAIN     WTBSP                              60
     C  N60              MOVEL(P)  BSKEY         SCBSCD
     C  N60              MOVEL(P)  BSDESC        SCDESC
     C  N60              MOVE      *ON           *IN51
     C   60              MOVE      *ON           *IN50
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
      *
      *
      *
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C     BXSTAT        IFEQ      *BLANK
     C     *IN40         OREQ      *ON
     C                   MOVEL     'A'           BXSTAT
     C                   ENDIF                                                  BXSTAT=*BLANK
     C*
     C                   MOVEL     BXBSCD        SCBSCD
     C*
     C                   MOVEL     *BLANKS       SCDESC
     C                   MOVEL(P)  BXBSCD        XXKEY
     C     BSKY          CHAIN     PBS01                              60
     C  N60              MOVEL(P)  BSDESC        SCDESC
     C*
     C                   MOVEL     BXSTAT        SCSTAT
     C                   MOVEL     BXTYPE        SCTYPE
     C                   Z-ADD     BXEFDT        SCEFDT
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     FNCO#         BXCO#
     C                   Z-ADD     FNFND         BXFND
     C                   MOVEL(P)  SCBSCD        BXBSCD
     C                   MOVEL     SCTYPE        BXTYPE
     C                   Z-ADD     SCEFDT        BXEFDT
     C                   MOVEL     SCSTAT        BXSTAT
     C*
     C                   ENDSR                                                  MVSCDB
     C*
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
      *** BEG ADD ***** 02/18/03 *************************************
     C                   SETON                                        LR
      *** END ADD ***** 02/18/03 *************************************
     C                   ENDSR                                                  PGMEND
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
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
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C*  GET GROUP INFORMATION
     C*
     C     @@RRN2        CHAIN     PFN01                              60
     C*
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
      *** BEG ADD **** 02/18/03 **************************************
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL23        IFEQ      'Y'
     C                   SETON                                        41
     C                   ELSE
     C                   SETOFF                                       41
     C                   ENDIF                                                  WCFL23 IFEQ
      *** END ADD **** 02/18/03 **************************************
     C*
      *** BEG ADD **** 04/11/03 **************************************
     C     WCFL24        IFEQ      'Y'
     C                   SETON                                        42
     C                   ELSE
     C                   SETOFF                                       42
     C                   ENDIF                                                  WCFL24 IFEQ
      *** END ADD **** 04/11/03 **************************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
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
     C                   ENDSR                                                  PREOTH
     C*
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B02  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        BXUSER
     C                   MOVEL     Q1PGM         BXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BXDT
     C                   MOVEL     DSTIME        BXTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E02  C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        BXIUSR
     C                   MOVEL     Q1USER        BXUSER
     C                   MOVEL     Q1PGM         BXIPGM
     C                   MOVEL     Q1PGM         BXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BXIDT
     C                   MOVEL     SRDTO         BXDT
     C                   MOVEL     DSTIME        BXITM
     C                   MOVEL     DSTIME        BXTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMBXP
     C*
     C                   ENDSR                                                  WRITER
