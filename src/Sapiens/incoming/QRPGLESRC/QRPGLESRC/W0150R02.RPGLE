     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0150R02 - STOCK DISCOUNT DEFINITION
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE STOCK DISCOUNT FILE.     *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0150R - STOCK DISCOUNT DEFINITION MAINTENANCE             *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 05/29/97  PROGRAMMER: C:65  PROJECT: WC691          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/09/98 C:85 PROJECT: WC577                                 *
     H*                ADDED ERROR CHECKING FOR DISCOUNT CODE,        *
     H*                HIGH LIMIT AND EQUATION TYPE.                  *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
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
     H*  30 - (WCR0504) The state code was not found in list.         *
     H*  31 - (WCR0500) Invalid date.                                 *
     H*  32 - (WC30005) Record already exists.                        *
     H*  33 - (WCR0502) CANNOT BE LESS THAN OR EQUAL TO ZERO.         *
     H*  34 - (WCR0624) VALID VALUES ARE 'A' AND 'B'.                 *
     H*  35 - (WCR0547) CANNOT BE LESS THAN ZERO.                     *
     H*                                                               *
     H* SCR02:                                                        *
     H*                                                               *
     H* SCR03:                                                        *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H*****************************************************************
     FW0150F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMDSP     UF A E             DISK
     F                                     RENAME(PDS01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMDSL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     FWMFNL     IF   E           K DISK
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D OPTNM           C                   CONST('Stock Discount De-
     D                                     finition')
     D #OFSCR          C                   CONST(1)
     D COMAPI          C                   CONST('WA001R')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*
     C     *LIKE         DEFINE    DSCO#         XXCO#
     C     *LIKE         DEFINE    DSFND         XXFND
     C     *LIKE         DEFINE    DSST          XXST
     C     *LIKE         DEFINE    DSCODE        XXCODE
     C     *LIKE         DEFINE    DSEFDT        XXEFDT
     C     *LIKE         DEFINE    DSTYPE        XXTYPE
     C     *LIKE         DEFINE    DSLMT         XXLMT
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  KEY LIST FOR THE SYSTEM-WIDE STATE CODE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY            10
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE STOCK DISCOUNT CODE FILE
     C*
     C     DSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXCODE
     C                   KFLD                    XXTYPE
     C                   KFLD                    XXEFDT
     C                   KFLD                    XXLMT
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
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
     C*  STATE MUST BE VALID.
     C*
     C                   MOVEL     SCST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        9030
     C*
     C*  EFFECTIVE DATE MUST BE VALID
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
B02  C  N90SRERR         IFEQ      '1'
     C                   SETON                                        9031
E02  C                   ENDIF                                                  SRERR=1
     C*
     C*** BEGIN ADD *** 01/09/98 **************************************
     C*
     C*  THE DISCOUNT CODE MUST NOT BE BLANK.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCODE        ANDEQ     *BLANK
     C                   MOVE      *ON           *IN36
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN90=OFF
     C*
     C*** END   ADD *** 01/09/98 **************************************
     C*
     C*
     C*  THE HIGH LIMIT MUST BE GREATER THAN 0.
     C*
     C  N90SCLMT         IFLE      *ZERO
     C                   MOVE      '1'           *IN33
     C                   MOVE      '1'           *IN90
     C                   END
     C*
     C*  THE EQUATION TYPE MUST BE 'A' OR 'B'
     C*
     C  N90SCTYPE        IFNE      'A'
     C     SCTYPE        ANDNE     'B'
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN90
     C                   END
     C*
     C*  RECORD MUST NOT ALREADY EXISTS FOR ST/CODE/EFDT
     C*
     C                   MOVE      SCST          XXST
     C                   MOVE      SCCODE        XXCODE
     C                   MOVE      SCLMT         XXLMT
     C                   MOVE      SCTYPE        XXTYPE
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C     DSKY          CHAIN     PDS01                              60
     C  N90*IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN32
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN60 EQ 0
     C*
     C                   ENDIF                                                  *IN40 EQ 1
     C*
     C*  THE PERCENTAGE MUST BE GREATER THAN OR EQUAL TO ZERO.
     C*
     C  N90SCPCT         IFLT      *ZERO
     C                   MOVE      '1'           *IN35
     C                   MOVE      '1'           *IN90
     C                   END
     C*
     C                   ENDIF                                                  *IN92=*OFF
     C*
     C*** BEGIN ADD *** 01/09/98 **************************************
     C*
     C*  CHANGE MODE ERROR CHECKING
     C*
     C     *IN09         IFEQ      *OFF
     C     *IN40         ANDEQ     *OFF
     C*
     C*  THE HIGH LIMIT MUST BE GREATER THAN ZERO
     C*
     C     *IN90         IFEQ      *OFF
     C     SCLMT         ANDLE     *ZERO
     C                   MOVE      *ON           *IN33
     C                   MOVE      *ON           *IN90
     C                   END
     C*
     C*  THE EQUATION TYPE MUST BE 'A' OR 'B'
     C*
     C     *IN90         IFEQ      *OFF
     C     SCTYPE        ANDNE     'A'
     C     SCTYPE        ANDNE     'B'
     C                   MOVE      *ON           *IN34
     C                   MOVE      *ON           *IN90
     C                   END
     C*
     C                   ENDIF                                                  *IN09,40=OFF
     C*
     C*** END   ADD *** 01/09/98 **************************************
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
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      DSST          SCST
     C                   MOVE      DSTYPE        SCTYPE
     C                   Z-ADD     DSPCT         SCPCT
     C                   Z-ADD     DSLMT         SCLMT
     C                   MOVEL     DSCODE        SCCODE
     C                   Z-ADD     DSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     XXCO#         DSCO#
     C                   Z-ADD     XXFND         DSFND
     C                   MOVE      SCST          DSST
     C                   MOVE      SCTYPE        DSTYPE
     C                   Z-ADD     SCPCT         DSPCT
     C                   Z-ADD     SCLMT         DSLMT
     C                   MOVEL     SCCODE        DSCODE
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DSEFDT
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
     C     @@RRN         CHAIN     DBRCDF                             60
     C                   Z-ADD     @@CO#         XXCO#
     C                   Z-ADD     @@FND         XXFND
     C                   Z-ADD     @@FND         SCFND
     C                   MOVE      DSST          XXST
     C                   Z-ADD     DSEFDT        XXEFDT
     C                   MOVEL     DSCODE        XXCODE
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNNAME        SCFNAM
     C                   ENDIF                                                  *IN60 EQ 0
     C*
     C                   EXSR      MVDBSC
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
     C     PREOTH        BEGSR
     C*
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
     C                   MOVEL     Q1USER        DSUSER
     C                   MOVEL     Q1PGM         DSPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DSDT
     C                   MOVEL     DSTIME        DSTIME
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
     C                   MOVEL     Q1PGM         DSPGM
     C                   MOVEL     Q1USER        DSUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DSDT
     C                   MOVEL     DSTIME        DSTIME
     C                   MOVEL     Q1PGM         DSIPGM
     C                   MOVEL     Q1USER        DSIUSR
     C                   MOVEL     SRDTO         DSIDT
     C                   MOVEL     DSTIME        DSITIM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMDSP
     C*
     C                   MOVEL     'C'           @@MODE
     C*
     C                   ENDSR                                                  WRITER
