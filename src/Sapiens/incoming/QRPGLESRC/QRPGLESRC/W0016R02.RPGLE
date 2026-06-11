     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0016R02 - INSTALLMENT FEE DATA
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE EXP CONST FILE.          *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0016R - INSTALLMENT FEE MAINTENANCE                       *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 03/04/02  PROGRAMMER: C:65  PROJECT: WC1064         *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 11/05/08 C:136 PROJECT: 146WBM                                *
     H*                ADD S/R INSTALLMENT FEE                        *
     H*                                                               *
     H* 03/03/05 C:132 PROJECT: WC1260                                *
     H*                Add Date field for installment fee expiration  *
     H*                date, and edit the date field if exists.       *
     H*                                                               *
     H*  10/11/04 C:65  PROJECT: WC1064                               *
     H*                 ADD A/R TRANSFER CODE.                        *
     H*                                                               *
     H*  05/12/04 C:65  PROJECT: WC1064                               *
     H*                 ADD A/R TRANSFER CODE FOR FLAT CX'S.          *
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
     H*  30 - (WCR0504) The state code was not found in list.         *
     H*  31 - (WC30001) Record already exists.                        *
     H*  32 - (WCR0500) Invalid date.                                 *
     H*  33 - (WCR0503) This field cannot be blank.                   *
     H*  34 - (WC31380) Installment fee billed A/R code must be an    *
     H*                 "F" type and an "F" category.                 *
     H*  35 - (WCR0503) This field cannot be blank.                   *
     H*  36 - (WC31381) Installment fee paid A/R code must be a "Q"   *
     H*                 type and an "F" category.                     *
     H*  37 - (WCR0502) This field cannot be less than, or equal to   *
     H*                 zero.                                         *
     H*                                                               *
     H* SCR02:                                                        *
     H*                                                               *
     H* SCR03:                                                        *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FW0016F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMIFP     UF A E             DISK
     F                                     RENAME(PIF01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMIFL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     FWMFNL     IF   E           K DISK
     FSTSTL3    IF   E           K DISK
      * Table file - A/R Transaction Type Code - PTT01
     FWTABL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D OPTNM           C                   CONST('Installment Fee Info-
     D                                     rmation')
     D #OFSCR          C                   CONST(1)
     D COMAPI          C                   CONST('WA001R')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*
     C     *LIKE         DEFINE    IFCO#         XXCO#
     C     *LIKE         DEFINE    IFFND         XXFND
     C     *LIKE         DEFINE    IFST          XXST
     C     *LIKE         DEFINE    IFEFDT        XXEFDT
     C     *LIKE         DEFINE    TTCO#         KKCO#
     C     *LIKE         DEFINE    TTKEY         KKKEY
     C***** BEG ADD ***** 03/03/05 ************************************
     C     *LIKE         DEFINE    IFEFDT        XXEXPD
     C***** END ADD ***** 03/03/05 ************************************
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
     C*  INSTALLMENT FEE FILE
     C*
     C     IFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
     C*
     C* A/R Transaction Type Code
     C*
     C     TTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
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
     C                   SETON                                        9032
E02  C                   ENDIF                                                  SRERR=1
     C*
     C*  RECORD MUST NOT ALREADY EXISTS FOR ST/EFDT
     C*
     C                   MOVE      SCST          XXST
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C     IFKY          CHAIN     PIF01                              60
     C  N90*IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN31
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN60 EQ 0
     C*
     C                   ENDIF                                                  *IN40 EQ 1
     C*
     C*  Billing A/R code cannot be blank
     C*
     C     SCIFBL        IFEQ      *BLANKS
     C  N90              SETON                                        33  90
     C                   ENDIF
     C*
     C*  Billing A/R code must be:
     C*     'F' - Adjustment Type
     C*     'F' - Premium Category Type
     C*     Active status
     C*
     C     SCIFBL        IFNE      *BLANKS
     C                   MOVEL     XXCO#         KKCO#
     C                   MOVEL(P)  SCIFBL        KKKEY
     C     TTKY          CHAIN     PTT01                              60
      *
     C     *IN60         IFEQ      *ON                                          EXISTS
     C     TTSTAT        ORNE      'A'                                          ACTIVE
     C     TTTYPE        ORNE      'F'                                          BILLING
     C     TTCTYP        ORNE      'F'                                          FEE
     C  N90              SETON                                        34  90
     C                   ENDIF
      *
     C                   ENDIF                                                  SCIFBL
     C*
     C*  Paid A/R code cannot be blank
     C*
     C     SCIFPD        IFEQ      *BLANKS
     C  N90              SETON                                        35  90
     C                   ENDIF
     C*
     C*  Paid A/R code must be:
     C*     'Q' - Adjustment Type
     C*     'F' - Premium Category Type
     C*     Active status
     C*
     C     SCIFPD        IFNE      *BLANKS
     C                   MOVEL     XXCO#         KKCO#
     C                   MOVEL(P)  SCIFPD        KKKEY
     C     TTKY          CHAIN     PTT01                              60
      *
     C     *IN60         IFEQ      *ON                                          EXISTS
     C     TTSTAT        ORNE      'A'                                          ACTIVE
     C     TTTYPE        ORNE      'Q'                                          PAYMENT
     C     TTCTYP        ORNE      'F'                                          FEE
     C  N90              SETON                                        36  90
     C                   ENDIF
      *
     C                   ENDIF                                                  SCIFPD
     C*
     C*  Installment fee amount cannot be zeros
     C*
     C     SCIFEE        IFLE      *ZERO
     C  N90              SETON                                        37  90
     C                   ENDIF
     C*
      *** BEG ADD ***** 11/05/08 **************************************
     C     SCISFE        IFLE      *ZERO
     C  N90              SETON                                        71  90
     C                   ENDIF
      *** END ADD ***** 11/05/08 **************************************
     C*** BEGIN ADD ***  05/12/04  ************************************
     C*
     C*  TRANSFER A/R code cannot be blank
     C*
     C*** BEGIN DEL ***  10/11/04  ************************************
     C*********  SCIFTR    IFEQ *BLANKS
     C*********            SETON                     38  90
     C*********            ENDIF
     C*** END DEL *****  10/11/04  ************************************
     C*
     C*  TRANSFER A/R code must be:
     C*     'Q' - Adjustment Type
     C*     'F' - Premium Category Type
     C*     Active status
     C*
     C     SCIFTR        IFNE      *BLANKS
     C                   MOVEL     XXCO#         KKCO#
     C                   MOVEL(P)  SCIFTR        KKKEY
     C     TTKY          CHAIN     PTT01                              60
      *
     C     *IN60         IFEQ      *ON                                          EXISTS
     C     TTSTAT        ORNE      'A'                                          ACTIVE
     C     TTTYPE        ORNE      'Q'                                          PAYMENT
     C     TTCTYP        ORNE      'F'                                          FEE
     C*** BEGIN CHG ***  10/11/04  ************************************
     C**********           SETON                     39  90
     C  N90              SETON                                        38  90
     C*** END CHG *****  10/11/04  ************************************
     C                   ENDIF
      *
     C                   ENDIF                                                  SCIFTR
     C*** END ADD *****  05/12/04  ************************************
     C*** BEGIN ADD ***  10/11/04  ************************************
     C*
     C*  TRANSFER A/R code must be:
     C*     'Q' - Adjustment Type
     C*     'F' - Premium Category Type
     C*     Active status
     C*
     C     SCIFTC        IFNE      *BLANKS
     C                   MOVEL     XXCO#         KKCO#
     C                   MOVEL(P)  SCIFTC        KKKEY
     C     TTKY          CHAIN     PTT01                              60
      *
     C     *IN60         IFEQ      *ON                                          EXISTS
     C     TTSTAT        ORNE      'A'                                          ACTIVE
     C     TTTYPE        ORNE      'Q'                                          PAYMENT
     C     TTCTYP        ORNE      'F'                                          FEE
     C  N90              SETON                                        39  90
     C                   ENDIF
      *
     C                   ENDIF                                                  SCIFTR
     C*** END ADD *****  10/11/04  ************************************
     C*
     C***** BEG ADD ***** 03/03/05 ************************************
     C*
     C*  EFFECTIVE DATE MUST BE VALID
     C*
     C  N90SCEXPD        IFNE      *ZEROS
     C                   Z-ADD     SCEXPD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXPD
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9046
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SCEXPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXPD
     C     XXEXPD        IFLT      XXEFDT
     C                   SETON                                        9047
     C                   ENDIF                                                  XXEXPD<XXEXPD
     C                   ENDIF                                                  SRERR=1
     C*
     C                   ENDIF                                                  EXEXPD <> BLANK
     C***** END ADD ***** 03/03/05 ************************************
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
     C* KJDO - SUBROUTINE FOR F=10
     C*===============================================================
     C*
     C     KJDO          BEGSR
     C*
     C     HFLD          IFEQ      'SCIFBL'
     C     HFLD          OREQ      'SCIFPD'
     C*** BEGIN ADD ***  05/12/04  ************************************
     C     HFLD          OREQ      'SCIFTR'
     C*** END ADD *****  05/12/04  ************************************
     C*** BEGIN ADD ***  10/11/04  ************************************
     C     HFLD          OREQ      'SCIFTC'
     C*** END ADD *****  10/11/04  ************************************
      *
     C                   CALL      'W2161R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
      *
     C     ##KEY         IFEQ      *BLANKS
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCIFBL'
     C                   SETON                                        41
     C*
     C     HFLD          WHENEQ    'SCIFPD'
     C                   SETON                                        42
     C*** BEGIN ADD ***  05/12/04  ************************************
     C     HFLD          WHENEQ    'SCIFTR'
     C                   SETON                                        44
     C*** END ADD *****  05/12/04  ************************************
     C*** BEGIN ADD ***  10/11/04  ************************************
     C     HFLD          WHENEQ    'SCIFTC'
     C                   SETON                                        45
     C*** END ADD *****  10/11/04  ************************************
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCIFBL'
     C                   SETON                                        42
     C                   MOVEL(P)  ##KEY         SCIFBL
     C*
     C     HFLD          WHENEQ    'SCIFPD'
     C                   SETON                                        43
     C                   MOVEL(P)  ##KEY         SCIFPD
     C*** BEGIN ADD ***  05/12/04  ************************************
     C     HFLD          WHENEQ    'SCIFTR'
     C                   MOVEL(P)  ##KEY         SCIFTR
     C*** END ADD *****  05/12/04  ************************************
     C*** BEGIN ADD ***  10/11/04  ************************************
     C                   SETON                                        45
     C     HFLD          WHENEQ    'SCIFTC'
     C                   MOVEL(P)  ##KEY         SCIFTC
     C*** END ADD *****  10/11/04  ************************************
     C                   ENDSL
     C*
     C                   ENDIF                                                  ##KEY
     C*
     C                   ENDIF                                                  HFLD
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      IFST          SCST
     C                   Z-ADD     IFIFEE        SCIFEE
     C                   MOVE      IFIFBL        SCIFBL
     C                   MOVE      IFIFPD        SCIFPD
     C                   Z-ADD     IFEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*** BEGIN ADD ***  05/12/04  ************************************
     C                   MOVE      IFIFTR        SCIFTR
     C*** END ADD *****  05/12/04  ************************************
     C*** BEGIN ADD ***  10/11/04  ************************************
     C                   MOVE      IFIFTC        SCIFTC
     C*** END ADD *****  10/11/04  ************************************
     C*** BEG ADD ***** 11/05/08 **************************************
     C                   MOVE      IFISFE        SCISFE
     C*** END ADD ***** 11/05/08 **************************************
     C*
     C***** BEG ADD ***** 03/03/05 ************************************
     C     IFEXPD        IFGT      *ZEROS
     C                   Z-ADD     IFEXPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXPD
     C                   ELSE
     C                   Z-ADD     *ZEROS        SCEXPD
     C                   ENDIF                                                  IFEXPD = ZEROS
     C***** END ADD ***** 03/03/05 ************************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     XXCO#         IFCO#
     C                   Z-ADD     XXFND         IFFND
     C                   MOVE      SCST          IFST
     C                   MOVE      SCIFBL        IFIFBL
     C                   MOVE      SCIFPD        IFIFPD
     C                   Z-ADD     SCIFEE        IFIFEE
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IFEFDT
     C*** BEGIN ADD ***  05/12/04  ************************************
     C                   MOVE      SCIFTR        IFIFTR
     C*** END ADD *****  05/12/04  ************************************
     C*** BEGIN ADD ***  10/11/04  ************************************
     C                   MOVE      SCIFTC        IFIFTC
     C*** END ADD *****  10/11/04  ************************************
     C*** BEG ADD ***** 11/05/08 **************************************
     C                   MOVE      SCISFE        IFISFE
     C*** END ADD ***** 11/05/08 **************************************
     C*
     C***** BEG ADD ***** 03/03/05 ************************************
     C     SCEXPD        IFGT      *ZEROS
     C                   Z-ADD     SCEXPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IFEXPD
     C                   ELSE
     C                   Z-ADD     *ZEROS        IFEXPD
     C                   ENDIF                                                  IFEXPD = ZEROS
     C***** END ADD ***** 03/03/05 ************************************
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
     C                   Z-ADD     @@CO#         XXCO#
     C                   Z-ADD     @@FND         XXFND
     C                   Z-ADD     @@FND         SCFND
     C                   MOVE      IFST          XXST
     C                   Z-ADD     IFEFDT        XXEFDT
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
     C                   MOVEL     Q1USER        IFUSER
     C                   MOVEL     Q1PGM         IFPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IFDT
     C                   MOVEL     DSTIME        IFTIME
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
     C                   MOVEL     Q1PGM         IFPGM
     C                   MOVE      Q1USER        IFUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IFDT
     C                   MOVEL     DSTIME        IFTIME
     C                   MOVEL     SRDTO         IFIDT
     C                   MOVEL     DSTIME        IFITIM
     C                   MOVE      Q1USER        IFIUSR
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMIFP
     C*
     C                   MOVEL     'C'           @@MODE
     C*
     C                   ENDSR                                                  WRITER
