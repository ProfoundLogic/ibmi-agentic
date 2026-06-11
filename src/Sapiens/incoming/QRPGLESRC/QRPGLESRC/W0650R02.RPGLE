     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller)
     H/define CONVERT
     H*
     H/TITLE W0650R02 - PAYROLL DESCRIPTION EXTENSION
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE PAYROLL DESC EXT FILE.   *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0650R - PAYROLL DESCRIPTION EXTENSION MAINTENANCE         *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 08/11/98  PROGRAMMER: C:65  PROJECT: WC843          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 02/14/18 C:TXO Project: WC2583                                *
     H*                Add Expiration date to the subfile grid        *
     H*                                                               *
     H* 01/10/18 C:267 Support: WC2583                                *
     H*                Add Effective Date to Payroll Extention screen *
     H*                                                               *
     H* 03/31/14 C:189 Support: 85226                                 *
     H*                Changed the decription field not to convert    *
     H*                the description to upper case                  *
     H*                                                               *
     H* 08/29/03 C:61  SUPPORT WA099 38724                            *
     H*              - Do not allow F5=Refresh in ADD mode.           *
     H*              - Update SCR01 to standards.                     *
     H*              - Add State search.                              *
     H*                                                               *
     H* 11/14/02 C:65  PROJECT: WC1099                                *
     H*                ADD FIELD FOR TERRITORY DIFF STAT CODE.        *
     H*                                                               *
     H* 04/27/00 C:53  SUPPORT: WC065/19768                           *
     H*                SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY. *
     H*                                                               *
     H* 09/17/99 C:90 PROJECT: WC945                                  *
     H*          ADDED OPTIONAL STATE ENTRY TO THE SCREEN. STATE MUST *
     H*          VALID AND A STATE AND *BLANK MAY NOT BOTH BE ENTERED *
     H*          FOR THE SAME EXT.                                    *
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
     H*  30 - (WCR0503) EXT CANNOT BE BLANK.                          *
     H*  31 - (WC30005) Record already exists.                        *
     H*  32 - (WCR0503) DESCRIPTION CANNOT BE BLANK.                  *
     H*  33 - POSITION AT SIC CODE                                    *
     H*  34 - (WCR0513) SIC CODE NOT VALID.                           *
     H*  35 - (WCR0504) THE STATE CODE WAS NOT FOUND IN A VALID LIST  *
     H*  36 - (WCR0503) THIS FIELD CANNOT BE BLANK.                   *
     H*  37 - (WCR0450) THIS FIELD MUST BE BLANK.                     *
     H*  38 - (WCR0547) THIS FIELD CANNOT BE LESS THAN ZERO.          *
     H*  39 - (WCR0500) INVALID DATE                   ADDED 01/10/18 *
     H*  71 - (WC30005) RECORD ALREADY EXISTS          ADDED 01/10/18 *
     H*  75 - (WCR0907) DATE RANGE SPECIFIED OVERLAPS DATE PREVIOUSLY *         ADDED 01/10/18 *
     H*                 USED                           ADDED 02/14/18 *
     H*  76 - (WCR2367) Expiration date can not be less than the      *         ADDED 01/10/18 *
     H*                 Effective date                 ADDED 02/14/18 *
     H* SCR02:                                                        *
     H*                                                               *
     H* SCR03:                                                        *
     H*                                                               *
     H*****************************************************************
     FW0650F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMPXP     UF A E             DISK
     F                                     RENAME(PPX01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMPXL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F*** BEG ADD *** 02/14/18 **********************************
     F*** BEG ADD *** 01/10/18 **********************************
     F***WMPXL1    IF   E           K DISK    RENAME(PPX01:PPXL1)
     F***                                     PREFIX(L1)
     F*** END ADD *** 01/10/18 **********************************
     FWMPXL1    UF   E           K DISK    RENAME(PPX01:PPXL1)
     F                                     PREFIX(L1)
     F*** END ADD *** 02/14/18 **********************************
     FWTGTL     IF   E           K DISK
      **** BEGIN ADD **** 09/17/99 ************************************
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
      **** END   ADD **** 09/17/99 ************************************
      ***** BEG ADD ******* 08/29/03 **********************************
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PST02)
      ***** END ADD ******* 08/29/03 **********************************
      *
      *
      *
     D***PY SCOPYMT01I
      /COPY SCOPYMT01D
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D OPTNM           C                   CONST('Payroll Class Descri-
     D                                     ption Extension')
     D #OFSCR          C                   CONST(1)
     D COMAPI          C                   CONST('WA001R')
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *
      *
      *

     C     *LIKE         DEFINE    PXCO#         XXCO#
     C     *LIKE         DEFINE    PXCLAS        XXCLAS
     C     *LIKE         DEFINE    PXEXT         XXEXT
     C     *LIKE         DEFINE    GTTBID        XZTBID
     C*** BEG ADD *****  01/10/18  ************************************
     C     *LIKE         DEFINE    PXEFDT        XXEFDT
     C     *LIKE         DEFINE    PXST          XXST
     C*** END ADD *****  01/10/18  ************************************
     C**** BEGIN ADD **** 02/14/18 ************************************
     C     *LIKE         DEFINE    PXEXDT        XXEXDT
     C**** END   ADD **** 02/14/18 ************************************
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*   KEY LIST FOR THE PCEF
     C*
     C     PXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXEXT
      **** BEG ADD ****** 01/10/18 ************************************
     C                   KFLD                    XXEFDT
      **** END ADD ****** 01/10/18 ************************************
     C*
      **** BEG ADD ****** 01/10/18 ************************************
     C     PXKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXEXT
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
      **** END ADD ****** 01/10/18 ************************************
     C*
      **** BEG ADD ****** 02/14/18 ************************************
     C     PXKY2         KLIST
     C                   KFLD                    PXCO#
     C                   KFLD                    PXCLAS
     C                   KFLD                    PXEXT
     C                   KFLD                    PXST
      *
     C     PXKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXEXT
     C                   KFLD                    XXST
      **** END ADD ****** 02/14/18 ************************************
     C*
     C*  KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XZTBID
     C                   KFLD                    XXKEY            10
      **** BEGIN ADD **** 09/17/99 ************************************
      *
      *  SYSTEM TABLE FILE
      *
     C     STKY          KLIST
     C                   KFLD                    XXKEY
      **** END   ADD **** 09/17/99 ************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@CLAS            5 0
      ***** BEG ADD ******* 08/29/03 **********************************
     C                   PARM                    @@ST              2
      ***** END ADD ******* 08/29/03 **********************************
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
     C     PXSTAT        IFEQ      'I'
     C                   MOVE      'A'           PXSTAT
     C                   ELSE
     C                   MOVE      'I'           PXSTAT
     C                   ENDIF                                                  PXSTAT=I
     C*
     C                   MOVEL     Q1USER        PXUSER
     C                   MOVEL     Q1PGM         PXPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PXDT
     C*
     C                   MOVEL     DSTIME        PXTIME
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
     C*  CHECK IF OTHER FUNCTION KEYS HAVE BEEN PRESSED
     C*
     C                   EXSR      OTHFKS
     C*
     C*  IF ANY OTHER FUNCTION KEYS HAVE BEEN PROCESSED, DO NOT
     C*  CHECK FOR ERRORS
     C*
     C     *IN92         IFEQ      *OFF
     C*
     C*  VERIFY EXTENSION IF IN ADD MODE.
     C*
     C     *IN40         IFEQ      *ON
     C*
     C     SCEXT         IFEQ      *BLANKS
     C                   SETON                                        3090
     C                   ENDIF
     C*
     C*  RECORD MUST NOT ALREADY EXISTS FOR EXTENSION.
     C*
     C                   MOVE      SCEXT         XXEXT
     C*
      **** BEG ADD ****** 01/10/18 ************************************
     C                   IF        SCEFDT   =    0
     C                   SETON                                        3990
     C                   ELSE
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C                   ENDIF
      **** END ADD ****** 01/10/18 ************************************
     C*
      **** BEGIN DEL **** 09/17/99 ************************************
     C********** PXKY      CHAINPPX01                60
     C**N90      *IN60     IFEQ *OFF
     C**********           MOVE *ON       *IN31
     C**********           MOVE *ON       *IN90
     C**********           ENDIF                           *IN60 EQ 0
      **** END   DEL **** 09/17/99 ************************************
      **** BEGIN ADD **** 09/17/99 ************************************
      *
      *  RECORD MUST NOT ALREADY EXISTS FOR DESCRITPION, IF NO STATE.
      *
      **** BEG CHG ****** 01/10/18 ************************************
     C***  PXKY          CHAIN     PPX01                              60
     C     PXKY          SETLL     PPX01
     C     PXKY          READE     PPX01                                  60
      **** END CHG ****** 01/10/18 ************************************
     C  N90*IN60         DOWEQ     *OFF
     C                   SELECT
     C     PXST          WHENNE    *BLANK
     C     SCST          ANDEQ     *BLANK
     C                   SETON                                        36  90
      *
     C     PXST          WHENEQ    *BLANK
     C     SCST          ANDNE     *BLANK
     C                   SETON                                        37  90
      *
     C     PXST          WHENEQ    SCST
     C                   SETON                                        31  90
      *
     C                   ENDSL                                                              K
      *
     C     PXKY          READE     PPX01                                  60
     C                   ENDDO
      *
      **** END   ADD **** 09/17/99 ************************************
     C*
     C                   ENDIF                                                  *IN40 EQ 1
     C*
     C*** BEGIN ADD ***  01/10/18  ************************************
      * EFFECTIVE DATE MUST BE VALID - CHECK ONLY IN ADD MODE
     C*
     C                   IF        *IN40   =  *ON AND
     C                             *IN90   =   *OFF
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C     SRERR         IFEQ      '1'

     C                   SETON                                        39  90

     C                   ENDIF
     C                   ENDIF
     C*** END   ADD ***  01/10/18  ************************************
     C*** BEGIN ADD ***  02/14/18  ************************************
      * EXPIRATION DATE MUST BE VALID - CHECK ONLY IN CHANGE  MODE
     C*
     C                   IF        *IN41   =  *ON AND
     C                             *IN90   =   *OFF AND
     C                             SCEXDT  <>  0
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXDT
     C     SRERR         IFEQ      '1'

     C                   SETON                                        42  90

     C                   ENDIF
     C                   ENDIF
     C*** END   ADD ***  02/14/18  ************************************
     C*
     C* DESCRIPTION CANNOT BE BLANK.
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C*
     C* SIC CODE MUST BE VALID.
     C*
     C  N90SCSIC         IFNE      *BLANK
     C*
     C                   MOVEL(P)  'SIC'         XZTBID
     C                   MOVEL(P)  SCSIC         XXKEY
     C*
     C                   MOVEL     *BLANKS       SCSICD
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCSICD
     C*
     C  N90*IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        34  90
     C                   ENDIF                                                  *IN60=*ON/GTSTAT<>A
     C*
     C                   ENDIF                                                  SCSIC IFNE
     C*

     C*** BEGIN ADD ***  11/14/02  ************************************
     C*
     C* STAT CODE CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCCODE        IFLT      *ZERO
     C                   MOVE      '1'           *IN38
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C*
     C*** END ADD *****  11/14/02  ************************************
      **** BEGIN ADD **** 09/17/99 ************************************
      *
      *  IF ENTERED, STATE MUST BE VALID
      *
     C     SCST          IFNE      *BLANK
      *
     C                   MOVEL(P)  SCST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        35  90
      *
     C                   ENDIF                                                  SCST NE BLANK
      **** END   ADD **** 09/17/99 ************************************
     C                   ENDIF                                                  *IN92=*OFF
     C*
      ***** BEG ADD ******* 01/10/18 **********************************
      *
      *  RECORD MUST NOT ALREADY EXISTS FOR DESCRITPION WITH SAME EFF DATE
      *
     C                   MOVE      SCST          XXST
      *
     C                   IF        *IN40    =    *ON  AND
     C                             *IN90    =    *OFF
     C*
     C                   IF        SCEFDT   =    0
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXEFDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        71  90
     C                   ENDIF
     C                   ENDIF

     C                   ENDIF

     C                   IF        *IN40     =   *ON  AND
     C                             *IN90     =   *OFF
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C     PXKY1         SETGT     PPXL1                              60
     C     PXKY1         READPE    PPXL1                                  60
     C     *IN60         DOWEQ     *OFF
     C     PXDESC        IFEQ      SCDESC
     C     PXSIC         ANDEQ     SCSIC
     C     PXCODE        ANDEQ     SCCODE
     C                   SETON                                        49  90
     C                   ENDIF
     C     PXKY1         READPE    PPXL1                                  60
     C                   ENDDO

     C                   ENDIF

      ***** END ADD ******* 01/10/18 **********************************
     C**** BEGIN ADD **** 02/14/18 ************************************
     C                   IF        *IN40   =  *ON AND
     C                             *IN90   =   *OFF
     c                   EVAL      XXST   = SCST
     C     PXKY3         SETGT     PPXL1
     C     PXKY3         READPE    PPXL1                                  60
     C     *IN60         DOWEQ     *OFF
     C                   IF        XXEFDT  < L1PXEXDT
     C                   SETON                                        75  90
     C                   ENDIF
     C                   IF        SCEXDT  <> 0
     C                   IF        SCEXDT  < XXEFDT
     C                   SETON                                        76  90
     C                   ENDIF
     C                   ENDIF
     C     PXKY3         READPE    PPXL1                                  60
     C                   ENDDO
     C                   ENDIF
     C**** END   ADD **** 02/14/18 ************************************
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
     C                   MOVE      *ON           *IN39
     C*
     C                   SELECT
     C*
     C*  SIC CODE
     C*
     C     HFLD          WHENEQ    'SCSIC'
     C                   MOVEL(P)  'SIC'         XZTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C*
     C                   CALL      'W2493R'
     C                   PARM                    ##DATA
     C                   PARM                    @@MODE
     C                   PARM                    XZTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C*
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL(P)  XXKEY         SCSIC
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL     GTDESC        SCSICD
      ***** BEG ADD ******* 08/29/03 **********************************
     C                   MOVE      *ON           *IN74
      ***** END ADD ******* 08/29/03 **********************************
     C                   ELSE
     C                   MOVE      *ON           *IN33
     C                   ENDIF                                                  XXKEY<>*BLANKS
      ***** BEG ADD ******* 08/29/03 **********************************
      *
      *  State
      *
     C     HFLD          WHENEQ    'SCST'
     C                   CALL      'SY601R'
     C                   PARM      '2'           #1CALL            1
     C                   PARM      *BLANK        #1KC              1
     C                   PARM      *BLANK        #1KL              1
     C                   PARM      *ZEROS        #1RRN             9 0
     C*
     C     #1RRN         IFEQ      *ZEROS
     C                   SETON                                        72  90
     C*
     C                   ELSE
      *
     C     #1RRN         CHAIN     PST02                              99
     C                   MOVEL     STKEY         SCST
     C                   SETON                                        73  90
     C                   ENDIF
      ***** END ADD ******* 08/29/03 **********************************
      *
      ***** END ADD ******* 09/10/03 **********************************
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      PXEXT         SCEXT
     C                   MOVEL     PXDESC        SCDESC
      **** BEGIN ADD **** 01/10/18 ************************************
     C                   Z-ADD     PXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT

     C                   Z-ADD     PXCLAS        SCCLAS
      **** END   ADD **** 01/10/18 ************************************
      **** BEGIN ADD **** 02/14/18 ************************************
     C                   IF        PXEXDT   <> 0
     C                   Z-ADD     PXEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
     C                   ELSE
     C                   MOVEL     *BLANKS       SCEXDT
     C                   ENDIF
     c     @@MODE        IFEQ      'C'
     c                   MOVE      *ON           *IN41
     c                   ENDIF
     c     @@MODE        IFEQ      'A'
     c                   MOVE      *ON           *IN41
     c                   ENDIF
     c
      **** END   ADD **** 02/14/18 ************************************

      **** BEGIN ADD **** 09/17/99 ************************************
     C                   MOVE      PXST          SCST
      **** END   ADD **** 09/17/99 ************************************
      ***** BEG ADD ******* 08/29/03 **********************************
      *
     C     *IN40         IFEQ      *ON
     C     Q1PRMS        ANDGE     7
     C     XX1ST         ANDEQ     'Y'
     C                   MOVEL     @@ST          SCST
      ***** BEG ADD ******* 01/10/18 **********************************
     C                   Z-ADD     @@CLAS        SCCLAS
      ***** END ADD ******* 01/10/18 **********************************
     C                   MOVE      'N'           XX1ST             1
     C                   ENDIF                                                  @@MODE = A
      *
      ***** END ADD ******* 08/29/03 **********************************
     C*** BEGIN ADD ***  11/14/02  ************************************
     C                   Z-ADD     PXCODE        SCCODE
     C*** END ADD *****  11/14/02  ************************************
     C*
     C                   MOVE      PXSIC         SCSIC
     C                   CLEAR                   SCSICD
     C                   MOVEL(P)  'SIC'         XZTBID
     C                   MOVEL(P)  SCSIC         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        SCSICD
     C*
     C                   MOVE      PXSTAT        SCSTAT
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     XXCO#         PXCO#
     C                   Z-ADD     XXCLAS        PXCLAS
     C                   MOVE      SCEXT         PXEXT
     C                   MOVEL     SCDESC        PXDESC
     C                   MOVE      SCSTAT        PXSTAT
     C                   MOVE      SCSIC         PXSIC
     C*** BEGIN ADD ***  01/10/18  ************************************
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PXEXDT
     C*** END ADD *****  01/10/18  ************************************
     C*** BEGIN ADD ***  02/14/18  ************************************
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PXEFDT
     C*** END ADD *****  02/10/18  ************************************
     C*** BEGIN ADD ***  11/14/02  ************************************
     C                   Z-ADD     SCCODE        PXCODE
     C*** END ADD *****  11/14/02  ************************************
      **** BEGIN ADD **** 09/17/99 ************************************
     C                   MOVE      SCST          PXST
      **** END   ADD **** 09/17/99 ************************************
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
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
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
     C**** BEGIN ADD **** 02/14/18 ************************************
     C                   MOVE      *OFF          *IN41
     C**** END   ADD **** 02/14/18 ************************************
     C*
     C     @@MODE        IFEQ      'A'
     C                   MOVE      *ON           *IN40
     C                   ENDIF                                                  @@MODE EQ A
      ***** BEG ADD ******* 08/29/03 **********************************
     C                   MOVE      'Y'           XX1ST             1
      ***** END ADD ******* 08/29/03 **********************************
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
     C                   Z-ADD     @@CLAS        XXCLAS
     C                   MOVE      PXEXT         XXEXT
      ***** BEG ADD ******* 01/10/18 **********************************
     C                   Z-ADD     @@CLAS        SCCLAS
      ***** END ADD ******* 01/10/18 **********************************
     C*
      ***** BEG DEL ******* 08/29/03 **********************************
     C*********            EXSR MVDBSC
      ***** END DEL ******* 08/29/03 **********************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
     C     PREOTH        BEGSR
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C*
     C                   ENDSL
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
     C                   MOVEL     Q1USER        PXUSER
     C                   MOVEL     Q1PGM         PXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PXDT
     C                   MOVEL     DSTIME        PXTIME
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
     C                   MOVE      'A'           PXSTAT
     C                   MOVEL     Q1PGM         PXPGM
     C                   MOVEL     Q1USER        PXUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PXDT
     C                   MOVEL     DSTIME        PXTIME
     C                   MOVEL     Q1PGM         PXIPGM
     C                   MOVEL     Q1USER        PXIUSR
     C                   MOVEL     SRDTO         PXIDT
     C*
     C                   WRITE     DBRCDF
     C*
     C**** BEGIN ADD **** 02/14/18 ************************************
     C     PXKY1         SETGT     PPXL1
     C     PXKY1         READPE    PPXL1                                  60
     C     *IN60         IFEQ      *OFF
     c                   EVAL      XXEXDT = L1PXEFDT
     C                   ENDIF
     C     PXKY2         READPE    PPXL1                                  60
     C     *IN60         DOWEQ     *OFF
     C                   IF        L1PXEXDT = 0
     C                   MOVE      L1PXEFDT      XXEFDT
     C     PXKY1         CHAIN     PPXL1                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVE      XXEXDT        L1PXEXDT
     C                   UPDATE    PPXL1
     C                   EVAL      XXEXDT   = 0
     C                   ENDIF
     C                   ENDIF
     C     PXKY2         READPE    PPXL1                                  60
     C                   ENDDO
     C**** END   ADD **** 02/14/18 ************************************
     C                   FEOD      WMPXP
     C*
     C                   MOVEL     'C'           @@MODE
     C*
     C                   ENDSR                                                  WRITER
