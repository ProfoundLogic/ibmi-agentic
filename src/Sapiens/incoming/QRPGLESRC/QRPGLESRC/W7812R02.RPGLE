     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7812R02 - ROLLOVER AGENT RATE DEFINITION
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE ROLLOVER AGENT RATE FILE *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W7812R - ROLLOVER AGENT RATE MAINTENANCE                   *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 05/14/97  PROGRAMMER: C:65  PROJECT: WC694          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  08/10/00 C:33 SEQ#: WA109/23913                              *
     H*                CHECK FOR INACTIVE RATE CODES.                 *
     H*                                                               *
     H*  10/20/99 C:33 PROJECT: WC948                                 *
     H*                THE NEW RATE SHOULD BE VALID AS OF THE SYSTEM  *
     H*                DATE.                                          *
     H*                                                               *
     H*  02/16/98 C:65 PROJECT: WC694                                 *
     H*                FIX PARMS TO CALL TO AGENT RATE SEARCH.        *
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
     H*  30 - (WCR0532) INVALID OLD RATE                              *
     H*  31 - (WC30005) Record already exists.                        *
     H*  32 - (WCR0532) INVALID NEW RATE                              *
     H*  33 - (WC30021) NEW RATE = OLD RATE                           *
     H*                                                               *
     H* SCR02:                                                        *
     H*                                                               *
     H* SCR03:                                                        *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FW7812F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWXRXP     UF A E             DISK
     F                                     RENAME(PRX01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWXRXL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     FWMARL     IF   E           K DISK
     FWMARP     IF   E             DISK
     F                                     RENAME(PAR01:PAR02)
     F                                     INFDS(DBFDS4)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS3)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
     D DBFDS4          DS
     D  DBRRN4               397    400B 0
     D*
     D OPTNM           C                   CONST('Rollover Agent Rat-
     D                                     e Definition')
     D #OFSCR          C                   CONST(1)
     D COMAPI          C                   CONST('WA001R')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*
     C     *LIKE         DEFINE    RXOLDR        XXRATE
     C     *LIKE         DEFINE    AREFDT        XXEFDT
     C     *LIKE         DEFINE    RXOLDR        XXOLDR
     C***  BEGIN ADD  ***  10/20/99  *********************************
     C     *LIKE         DEFINE    AREFDT        SYSDT7
     C***  END   ADD  ***  10/20/99  *********************************
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
     C                   KFLD                    SCFND
     C*
     C*  ROLLOVER AGENT RATE FILE
     C*
     C     RXKY          KLIST
     C                   KFLD                    QQTIME
     C                   KFLD                    XXOLDR
     C*
     C*  AGENT RATE NUMBER FILE
     C*
     C     ARKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXRATE
     C***  BEGIN ADD  ***  10/20/99  *********************************
     C**********           KFLD           XXEFDT
     C                   KFLD                    SYSDT7
     C***  END   ADD  ***  10/20/99  *********************************
     C*
     C     ARKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXRATE
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    QQTIME            6 0
     C                   PARM                    SCCONM
     C                   PARM                    SCPRDS
     C                   PARM                    XXCO#             3 0
     C                   PARM                    SCFND
     C                   PARM                    SCFNAM
     C                   PARM                    QQEFDT            8
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
     C*  VERIFY OLD RATE IF IN ADD MODE
     C*
     C     *IN40         IFEQ      *ON
     C*
     C*  OLD RATE MUST BE VALID.
     C*
     C                   Z-ADD     SCOLDR        XXRATE
     C     ARKY          SETGT     PAR01
     C     ARKY2         READPE    PAR01                                  60
     C*
     C***  BEGIN ADD  ***  08/10/00  *********************************
     C     *IN60         DOWEQ     *OFF
     C     ARSTAT        ANDEQ     'I'
     C     ARKY2         READPE    PAR01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C***  END   ADD  ***  08/10/00  *********************************
     C*
     C  N90
     CAN 60              SETON                                        9030
     C*
     C*  RECORD MUST NOT ALREADY EXISTS FOR OLD RATE
     C*
     C                   Z-ADD     SCOLDR        XXOLDR
     C     RXKY          CHAIN     PRX01                              60
     C  N90*IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN31
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN60 EQ 0
     C*
     C                   ENDIF                                                  *IN40 EQ 1
     C*
     C*  NEW RATE MUST BE VALID.
     C*
     C                   Z-ADD     SCNEWR        XXRATE
     C     ARKY          SETGT     PAR01
     C     ARKY2         READPE    PAR01                                  60
     C*
     C***  BEGIN ADD  ***  08/10/00  *********************************
     C     *IN60         DOWEQ     *OFF
     C     ARSTAT        ANDEQ     'I'
     C     ARKY2         READPE    PAR01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C***  END   ADD  ***  08/10/00  *********************************
     C*
     C  N90
     CAN 60              SETON                                        9032
     C*
     C*  NEW RATE CANNOT BE SAME AS OLD RATE
     C*
     C  N90SCNEWR        IFEQ      SCOLDR
     C                   SETON                                        9033
     C                   ENDIF                                                  SCNEWR IFEQ
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
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=SEARCH
     C*
     C     KJDO          BEGSR
     C*
     C                   CALL      'W0640R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           @#CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      DBRRN3        ##RRN2            9 0
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PAR02                              60
     C*
     C     HFLD          IFEQ      'SCOLDR'
     C*
     C                   MOVE      ARRT#         SCOLDR
     C                   MOVE      *ON           *IN34
     C**                   MOVE *ON       *IN91
     C**                   MOVE *ON       *IN92
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C     HFLD          IFEQ      'SCNEWR'
     C*
     C                   MOVE      ARRT#         SCNEWR
     C                   MOVE      *ON           *IN35
     C**                   MOVE *ON       *IN91
     C**                   MOVE *ON       *IN92
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C                   ENDIF                                                  ##RRN IFNE
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     RXOLDR        SCOLDR
     C                   Z-ADD     RXNEWR        SCNEWR
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCOLDR        RXOLDR
     C                   Z-ADD     SCNEWR        RXNEWR
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
     C*
     C     *IN60         IFEQ      *OFF
     C                   EXSR      MVDBSC
     C                   ELSE
     C                   Z-ADD     0             SCOLDR
     C                   Z-ADD     0             SCNEWR
     C                   ENDIF                                                  *IN60 EQ 0
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C                   MOVE      QQEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C***  BEGIN ADD  ***  10/20/99  *********************************
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT7
     C***  END   ADD  ***  10/20/99  *********************************
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
     C                   Z-ADD     QQTIME        RXTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WXRXP
     C*
     C                   MOVEL     'C'           @@MODE
     C*
     C                   ENDSR                                                  WRITER
