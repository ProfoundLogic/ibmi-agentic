     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0013R02 - EXPENSE CONSTANT DEFINITION MAINTENANCE DATA
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE EXP CONST FILE.          *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0013R - EXPENSE CONSTANT DEFINITION MAINTENANCE           *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 03/03/97  PROGRAMMER: C:65  PROJECT: WC643          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 12/23/08  C:190 PROJECT WC1754
     H*                 ADD NEW FIELDS FOR SECOND REDUCED EXPENSE
     H*                 CONSTANT PREMIUM LEVEL AND AMOUNT.
     H*
     H*  11/27/01 C:65 PROJECT: WC1059                                *
     H*                ADD FIELDS FOR STANDARD PREMIUM LEVEL AND      *
     H*                CORRESPONDING REDUCED EXPENSE CONSTANT.
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H*  08/12/99 C:65 PROJECT: WC891                                 *
     H*                ADD A FLAG FOR CHARGING FULL EXPENSE CONSTANT  *
     H*                ON SHORT TERM POLICIES.                        *
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
     H*  33 - (WCR0571) FLAG IS NOT Y OR BLANK.                       *
     H*                                                               *
     H* SCR02:                                                        *
     H*                                                               *
     H* SCR03:                                                        *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FW0013F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWDECP     UF A E             DISK
     F                                     RENAME(PEC01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWDECL     IF   E           K DISK
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
     D OPTNM           C                   CONST('Expense Constant De-
     D                                     finition')
     D #OFSCR          C                   CONST(1)
     D COMAPI          C                   CONST('WA001R')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*
     C     *LIKE         DEFINE    ECFND         XXFND
     C     *LIKE         DEFINE    ECCO#         XXCO#
     C     *LIKE         DEFINE    ECST          XXST
     C     *LIKE         DEFINE    ECEFDT        XXEFDT
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
     C*  EXPENSE CONSTANT DEFINITION FILE
     C*
     C     ECKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
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
     C     ECKY          CHAIN     PEC01                              60
     C  N90*IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN31
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN60 EQ 0
     C*
     C                   ENDIF                                                  *IN40 EQ 1
     C*
     C*** BEGIN ADD ***  11/27/01  ************************************
     C  N90SCEXCO        IFLT      0
     C                   MOVE      *ON           *IN34
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCEXCO IFLT
     C*
     C  N90SCMPEC        IFLT      0
     C                   MOVE      *ON           *IN35
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCMPEC IFLT
     C*** END ADD *****  11/27/01  ************************************
     C*** BEGIN ADD ***  08/12/99  ************************************
     C  N90SCSFUL        IFNE      'Y'
     C     SCSFUL        ANDNE     ' '
     C                   MOVE      *ON           *IN33
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCSFUL IFNE
     C*** END ADD *****  08/12/99  ************************************
     C*** BEGIN ADD ***  11/27/01  ************************************
     C  N90SCSTPR        IFLT      0
     C                   MOVE      *ON           *IN36
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCSTPR IFLT
     C*
     C  N90SCEXC2        IFLT      0
     C                   MOVE      *ON           *IN37
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCEXC2 IFLT
     C*** END ADD *****  11/27/01  ************************************
     C*** BEGIN ADD ***  12/23/08  ************************************
     C*
     C* ENSURE THAT THE FIRST LEVEL OF STANDARD PREMIUM IS POPULATED
     C* BEFORE THE SECOND LEVEL IS POPULATED AND THAT THE SECOND LEVEL
     C* OF PREMIUM IS GREATER THAN THE FIRST LEVEL.
     C*
     C  N90SCSTPR        IFEQ      0
     C     SCSTP3        ANDNE     0
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCSTP3 IFLT
     C*
     C  N90SCSTP3        IFNE      0
     C     SCSTPR        ANDGT     SCSTP3
     C                   MOVE      *ON           *IN42
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCSTPR IFLT SCSTP3
     C*
     C* EDIT TO ENSURE THAT THE SECOND LEVEL OF EXPENSE CONSTANT IS NOT A
     C* NEGATIVE NUMBER.
     C*
     C  N90SCSTP3        IFLT      0
     C                   MOVE      *ON           *IN38
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCSTP3 IFLT
     C*
     C  N90SCEXC3        IFLT      0
     C                   MOVE      *ON           *IN39
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCEXC3 IFLT
     C*** END ADD *****  12/23/08  ************************************
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
     C                   MOVE      ECST          SCST
     C                   Z-ADD     ECEXCO        SCEXCO
     C                   Z-ADD     ECMPEC        SCMPEC
     C*** BEGIN ADD ***  11/27/01  ************************************
     C                   Z-ADD     ECEXC2        SCEXC2
     C                   Z-ADD     ECSTPR        SCSTPR
     C*** END ADD *****  11/27/01  ************************************
     C*** BEGIN ADD ***  12/23/08  ************************************
     C                   Z-ADD     ECEXC3        SCEXC3
     C                   Z-ADD     ECSTP3        SCSTP3
     C*** END ADD *****  12/23/08  ************************************
     C                   Z-ADD     ECEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*** BEGIN ADD ***  08/12/99  ************************************
     C                   MOVE      ECSFUL        SCSFUL
     C*** END ADD *****  08/12/99  ************************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     XXCO#         ECCO#
     C                   Z-ADD     XXFND         ECFND
     C                   MOVE      SCST          ECST
     C                   Z-ADD     SCEXCO        ECEXCO
     C                   Z-ADD     SCMPEC        ECMPEC
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ECEFDT
     C*** BEGIN ADD ***  08/12/99  ************************************
     C                   MOVE      SCSFUL        ECSFUL
     C*** END ADD *****  08/12/99  ************************************
     C*** BEGIN ADD ***  11/27/01  ************************************
     C                   Z-ADD     SCEXC2        ECEXC2
     C                   Z-ADD     SCSTPR        ECSTPR
     C*** END ADD *****  11/27/01  ************************************
     C*** BEGIN ADD ***  12/23/08  ************************************
     C                   Z-ADD     SCEXC3        ECEXC3
     C                   Z-ADD     SCSTP3        ECSTP3
     C*** END ADD *****  12/23/08  ************************************
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
     C     @@RRN         CHAIN     DBRCDF                             60
     C                   Z-ADD     @@CO#         XXCO#
     C                   Z-ADD     @@FND         XXFND
     C                   Z-ADD     @@FND         SCFND
     C                   MOVE      ECST          XXST
     C                   Z-ADD     ECEFDT        XXEFDT
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
     C                   MOVEL     Q1USER        ECUSER
     C                   MOVEL     Q1PGM         ECPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ECDT
     C                   MOVEL     DSTIME        ECTIME
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
     C                   MOVEL     Q1PGM         ECPGM
     C                   MOVE      Q1USER        ECUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ECDT
     C                   MOVEL     DSTIME        ECTIME
     C                   MOVEL     Q1PGM         ECIPGM
     C                   MOVEL     SRDTO         ECIDT
     C                   MOVEL     DSTIME        ECITIM
     C                   MOVE      Q1USER        ECIUSR
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WDECP
     C*
     C                   MOVEL     'C'           @@MODE
     C*
     C                   ENDSR                                                  WRITER
