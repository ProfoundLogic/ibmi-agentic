     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0024R02 - SHORT RATE DEFINITION SCREENS
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE SHORT RATE FILE.         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0024R - SHORT RATE DETAIL MAINTENANCE                     *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 04/27/97  PROGRAMMER: C:65  PROJECT: WC687          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H* INDICATORS USED:                                              *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD                                                     *
     H* N40 - UPDATE/INQUIRY                                          *
     H*  92 - GENERAL F KEY PRESSED                                   *
     H*  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  30 - (WCR0502) DAYS MUST BE GREATER THAN 0.                  *
     H*  31 - (WCR0502) PERCENT MUST BE GREATER THAN 0                *
     H*  32 - (WC30005) RECORD ALREADY EXISTS.                        *
     H*                                                               *
     H* SCR02:                                                        *
     H*                                                               *
     H* SCR03:                                                        *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FW0024F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMSTP     IF   E             DISK
     F                                     RENAME(PST01:PST02)
     FWDSTP     UF A E             DISK
     F                                     RENAME(PST01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWDSTL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     FWMFNL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D OPTNM           C                   CONST('Short Rate Detail D-
     D                                     efinition')
     D #OFSCR          C                   CONST(1)
     D COMAPI          C                   CONST('WA001R')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*
     C     *LIKE         DEFINE    STFND         XXFND
     C     *LIKE         DEFINE    STCO#         XXCO#
     C     *LIKE         DEFINE    STST          XXST
     C     *LIKE         DEFINE    STEFDT        XXEFDT
     C     *LIKE         DEFINE    STDAYS        XXDAYS
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  SHORT RATE MASTER FILE
     C*
     C     STKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
     C*
     C*  SHORT RATE DETAIL FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
     C                   KFLD                    XXDAYS
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @MRRN             9 0
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
     C*  CHECK IF OTHER FUNCTION KEYS HAVE BEEN PRESSED
     C*
     C                   EXSR      OTHFKS
     C*
     C*  IF ANY OTHER FUNCTION KEYS HAVE BEEN PROCESSED, DO NOT
     C*  CHECK FOR ERRORS
     C*
     C     *IN92         IFEQ      *OFF
     C*
     C*  VERIFY STATE AND EFFECTIVE DATE IF IN ADD MODE.
     C*
     C     *IN40         IFEQ      *ON
     C*
     C*  DAYS MUST BE GREATER THAN 0
     C*
     C     SCDAYS        IFLE      0
     C                   MOVE      *ON           *IN30
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCDAYS LE 0
     C*
     C*  RECORD MUST NOT ALREADY EXISTS FOR ST/EFDT
     C*
     C                   Z-ADD     SCDAYS        XXDAYS
     C     STKY          CHAIN     PST01                              60
     C  N90*IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN32
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN60 EQ 0
     C*
     C                   ENDIF                                                  *IN40 EQ 1
     C*
     C*
     C*  PERCENTAGE MUST BE GREATER THAN 0
     C*
     C     SCPERC        IFLE      0
     C                   MOVE      *ON           *IN31
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCPERC LE 0
     C*
     C                   ENDIF                                                  *IN92=*OFF
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
     C*  CHECK IF OTHER FUNCTION KEYS HAVE BEEN PRESSED
     C*
     C                   EXSR      OTHFKS
     C*
     C*  IF ANY OTHER FUNCTION KEYS HAVE BEEN PROCESSED, DO NOT
     C*  CHECK FOR ERRORS
     C*
     C     *IN92         IFEQ      *OFF
     C*
     C                   ENDIF                                                  *IN92=*OFF
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
     C*
     C*  CHECK IF OTHER FUNCTION KEYS HAVE BEEN PRESSED
     C*
     C                   EXSR      OTHFKS
     C*
     C*  IF ANY OTHER FUNCTION KEYS HAVE BEEN PROCESSED, DO NOT
     C*  CHECK FOR ERRORS
     C*
     C     *IN92         IFEQ      *OFF
     C*
     C                   ENDIF                                                  *IN92=*OFF
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK3
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     STDAYS        SCDAYS
     C                   Z-ADD     STPERC        SCPERC
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     XXCO#         STCO#
     C                   Z-ADD     XXFND         STFND
     C                   MOVE      SCST          STST
     C                   Z-ADD     SCDAYS        STDAYS
     C                   Z-ADD     SCPERC        STPERC
     C                   Z-ADD     XXEFDT        STEFDT
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
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR
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
     C*
     C                   MOVE      *OFF          *IN92
     C                   MOVE      *OFF          *IN40
     C*
     C     @@MODE        IFEQ      'A'
     C                   MOVE      *ON           *IN40
     C                   ENDIF                                                  @@MODE EQ A
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C     @MRRN         CHAIN     PST02                              60
     C                   Z-ADD     STCO#         XXCO#
     C                   Z-ADD     STFND         XXFND
     C                   Z-ADD     STFND         SCFND
     C                   MOVE      STST          XXST
     C                   MOVE      STST          SCST
     C                   Z-ADD     STEFDT        XXEFDT
     C                   Z-ADD     STEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     STDAYS        XXDAYS
     C                   Z-ADD     STDAYS        SCDAYS
     C                   Z-ADD     STPERC        SCPERC
     C                   ELSE
     C                   Z-ADD     0             XXDAYS
     C                   Z-ADD     0             SCDAYS
     C                   Z-ADD     0             SCPERC
     C                   ENDIF                                                  *IN60 EQ 0
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNNAME        SCFNAM
     C                   ENDIF                                                  *IN60 EQ 0
     C*
     C                   ENDSR                                                  PGMSTR
     C*
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
B02  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        STUSER
     C                   MOVEL     Q1PGM         STPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         STDT
     C                   MOVEL     DSTIME        STTIME
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
     C                   MOVEL     Q1PGM         STPGM
     C                   MOVEL     Q1PGM         STIPGM
     C                   MOVEL     Q1USER        STUSER
     C                   MOVEL     Q1USER        STIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         STDT
     C                   MOVEL     STDT          STIDT
     C                   MOVEL     DSTIME        STTIME
     C                   MOVEL     STTIME        STITIM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   Z-ADD     0             SCDAYS
     C                   Z-ADD     0             SCPERC
     C*
     C*****                FEOD WDSTP
     C*
     C*****                MOVEL'C'       @@MODE
     C*
     C*
     C                   ENDSR                                                  WRITER
