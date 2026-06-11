     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0022R02 - STATE COMPENSATION BENEFIT DATA
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE STATE COMP BENEFIT FILE. *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0022R - STATE COMPENSATION BENEFITS MAINT                 *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 06/03/97  PROGRAMMER: C:65  PROJECT: WC692          *
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  02/15/05 C:53  SUPPORT: WC082/45710
     H*                 LOAD RBREPC FROM SCCMPR FOR CLIENTS STILL USING
     H*                 THE OLD FIELD IN MODS.
     H*
     H*  06/02/04 C:53  PROJECT: WC1180
     H*                 EXPAND FROM 2 DEC POS TO 4
     H*
     H*  07/10/00 C:53  PROJECT WC999
     H*                 ADD USL&H FIELDS TO PGM & SCREEN
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
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
     H*  20 - (WCR0502) VALUE CANNOT BE LESS THAN OR EQUAL TO ZERO.   *
     H*  21 - (WCR0547) VALUE CANNOT BE LESS THAN ZERO.               *
     H*  22 - (WCR0502) VALUE CANNOT BE LESS THAN OR EQUAL TO ZERO.   *
     H*  23 - (WCR0547) VALUE CANNOT BE LESS THAN ZERO.               *
     H*  24 - (WCR0547) VALUE CANNOT BE LESS THAN ZERO.               *
     H*  25 - (WCR0547) VALUE CANNOT BE LESS THAN ZERO.               *
     H*  26 - (WC30023) CHECK CODE IS INVALID.                        *
     H*  41 - (WC30005) Record already exists.                        *
     H*  42 - (WCR0504) The state code was not found in list.         *
     H*  43 - (WCR0500) Invalid date.                                 *
     H*                                                               *
     H* SCR02:                                                        *
     H*                                                               *
     H* SCR03:                                                        *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FW0022F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWDRBP     UF A E             DISK
     F                                     RENAME(PRB01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWDRBL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     FWMFNL     IF   E           K DISK
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
      *_____________________________________________________________________
      *  Bank Info file       key=CO#/APPL/GRP/CKTP/EFDT
     F*****WMBIL2  IF  E           K        DISK
      *_____________________________________________________________________
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D OPTNM           C                   CONST('State Compensation-
     D                                      Benefit Definition')
     D #OFSCR          C                   CONST(1)
     D COMAPI          C                   CONST('WA001R')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*
     C     *LIKE         DEFINE    RBCO#         XXCO#
     C     *LIKE         DEFINE    RBFND         XXFND
     C     *LIKE         DEFINE    RBST          XXST
     C     *LIKE         DEFINE    RBEFDT        XXEFDT
     C     *LIKE         DEFINE    RBVCHR        XXCHK
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
     C*   KEY LIST FOR THE STATE COMP FILE
     C*
     C     RBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
     C*
     C****KEY LIST FOR THE BANK INFO FILE
     C*****
     C*****      BIKY      KLIST
     C*****                KFLD           XXCO#
     C*****                KFLD           XXAPPL  2
     C*****                KFLD           XXFND
     C*****                KFLD           XXCHK
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
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*===============================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     RBSTAT        IFEQ      'I'
     C                   MOVE      'A'           RBSTAT
     C                   ELSE
     C                   MOVE      'I'           RBSTAT
     C                   ENDIF                                                  DPSTAT=I
     C*
     C                   MOVEL     Q1USER        RBUSER
     C                   MOVEL     Q1PGM         RBPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RBDT
     C*
     C                   MOVEL     DSTIME        RBTIME
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
     C*  VERIFY STATE AND EFFECTIVE DATE IF IN ADD MODE.
     C*
     C     *IN40         IFEQ      *ON
     C*
     C*  STATE MUST BE VALID.
     C*
     C                   MOVEL     SCST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        9042
     C*
     C*  EFFECTIVE DATE MUST BE VALID
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
B02  C  N90SRERR         IFEQ      '1'
     C                   SETON                                        9043
E02  C                   ENDIF                                                  SRERR=1
     C*
     C*  RECORD MUST NOT ALREADY EXISTS FOR ST/EFDT
     C*
     C                   MOVE      SCST          XXST
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C     RBKY          CHAIN     PRB01                              60
     C  N90*IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN60 EQ 0
     C*
     C                   ENDIF                                                  *IN40 EQ 1
     C*
     C*  REIMBUSREMENT PERCENTAGE CANNOT BE <= ZERO.
     C*
     C*** BEG CHG *** 06/02/04 ****************************
     C**N90***** SCREPC    IFLE *ZERO
     C  N90SCCMPR        IFLE      *ZERO
     C*** END CHG *** 06/02/04 ****************************
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCCMPR IFLE
     C*
     C*  MINIMUM COMPENSATION CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCMNRT        IFLT      *ZERO
     C                   MOVE      *ON           *IN21
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCACC IFLT
     C*
     C*  PAYMENT CAP CANNOT BE LESS THAN OR EQUAL TO ZERO.
     C*
     C  N90SCPACP        IFLE      *ZERO
     C                   MOVE      *ON           *IN22
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCPACP IFLE
     C*
     C*  INITIAL COMPENSATION PAYMENT DAYS CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCICPD        IFLT      *ZERO
     C                   MOVE      *ON           *IN23
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCICPD IFLT
     C*
     C*  WAITING PERIOD CANNOT BE LESS THAN ZERO.
     C*
     C     SCWKS         IFLT      *ZERO
     C                   MOVE      *ON           *IN24
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCWKS IFEQ
     C*
     C*  FULL SALARY CONTINUANCE CANNOT BE LESS THAN ZERO.
     C*
     C     SCDYS         IFLT      *ZERO
     C                   MOVE      *ON           *IN25
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCDYS IFEQ
     C*
     C***VOUCHER CHECK CODE MUST BE VALID.
     C*****
     C**N90      SCVCHR    IFNE *BLANKS
     C*****
     C*****                MOVE 'WC'      XXAPPL
     C*****                MOVE SCVCHR    XXCHK
     C*****      BIKY      CHAINPBI01                60
     C*****      *IN60     IFEQ *ON
     C*****                MOVE *ON       *IN26
     C*****                MOVE *ON       *IN90
     C*****                ENDIF                           *IN60 IFEQ
     C*****
     C*****                ENDIF                           SCVCHR IFNE
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
     C                   MOVE      RBST          SCST
     C                   Z-ADD     RBEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   MOVE      RBSTAT        SCSTAT
     C*** BEG CHG *** 06/02/04 ****************************
     C**********           Z-ADDRBREPC    SCREPC
     C                   Z-ADD     RBCMPR        SCCMPR
     C*** END CHG *** 06/02/04 ****************************
     C                   Z-ADD     RBMNRT        SCMNRT
     C                   Z-ADD     RBPACP        SCPACP
     C                   Z-ADD     RBICPD        SCICPD
     C                   Z-ADD     RBWKS         SCWKS
     C                   Z-ADD     RBDYS         SCDYS
     C*** BEG ADD *** 07/10/00 ***************************
     C                   Z-ADD     RBLHMN        SCLHMN
     C                   Z-ADD     RBLHMX        SCLHMX
     C                   Z-ADD     RBLHWP        SCLHWP
     C*** END ADD *** 07/10/00 ***************************
     C*****                MOVE RBVCHR    SCVCHR
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     XXCO#         RBCO#
     C                   Z-ADD     XXFND         RBFND
     C                   MOVE      SCST          RBST
     C                   MOVE      SCSTAT        RBSTAT
     C*** BEG CHG *** 06/02/04 ****************************
     C**********           Z-ADDSCREPC    RBREPC
     C                   Z-ADD     SCCMPR        RBCMPR
     C*** END CHG *** 06/02/04 ****************************
     C*** BEG ADD *** 02/15/05 ****************************
     C                   Z-ADD     SCCMPR        RBREPC
     C*** END ADD *** 02/15/05 ****************************
     C                   Z-ADD     SCMNRT        RBMNRT
     C                   Z-ADD     SCPACP        RBPACP
     C                   Z-ADD     SCICPD        RBICPD
     C                   Z-ADD     SCWKS         RBWKS
     C                   Z-ADD     SCDYS         RBDYS
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RBEFDT
     C*** BEG ADD *** 07/10/00 ***************************
     C                   Z-ADD     SCLHMN        RBLHMN
     C                   Z-ADD     SCLHMX        RBLHMX
     C                   Z-ADD     SCLHWP        RBLHWP
     C*** END ADD *** 07/10/00 ***************************
     C*****                MOVE SCVCHR    RBVCHR
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
     C                   MOVE      RBST          XXST
     C                   Z-ADD     RBEFDT        XXEFDT
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNNAME        SCNAME
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
     C                   MOVEL     Q1USER        RBUSER
     C                   MOVEL     Q1PGM         RBPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RBDT
     C                   MOVEL     DSTIME        RBTIME
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
     C                   MOVE      'A'           RBSTAT
     C                   MOVEL     Q1PGM         RBPGM
     C                   MOVEL     Q1USER        RBUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RBDT
     C                   MOVEL     DSTIME        RBTIME
     C                   MOVEL     Q1PGM         RBIPGM
     C                   MOVEL     Q1USER        RBIUSR
     C                   MOVEL     SRDTO         RBIDT
     C                   MOVEL     DSTIME        RBITIM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WDRBP
     C*
     C                   MOVEL     'C'           @@MODE
     C*
     C                   ENDSR                                                  WRITER
