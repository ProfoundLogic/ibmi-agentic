     H* (c) 2014 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
      *
     H/TITLE W2185R02 - INCREASED LIMIT DEFINITION MAINTENANCE DATA
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE INCREASED LIMITS FILE.   *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W2185R - INCREASED LIMITS DEFINITION MAINTENANCE           *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 05/21/97  PROGRAMMER: C:65  PROJECT: WC689          *
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 08/16/11 C:061 PROJECT 114WZW
     H*                Use expanded Minimum Prem field (if available).
     H*                Convert to RPGIV.
     H*
     H*  08/15/01 C:90  PROJECT: WC1041                               *
     H*                 Expanded percentage from 3.2 to 5.3           *
     H*                                                               *
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H* 09/20/99 C:61  PROJECT WC949                                  *
     H*                Add field for 'Auto renew (N/ )'.              *
     H*                                                               *
     H*  10/01/98 C:78 PROJECT: WC869                                 *
     H*                1.- CLASS CODE SHOULD BE REQUIRED IF REPORTING *
     H*                TO NCCI (WCFL03='Y').                          *
     H*                2.- CHANGE 'CLASS CODE' DESCRIPTION TO BE      *
     H*                'STATISTICAL REPORTING CODE' ON THE SCREEN.    *
     H*                                                               *
     H*  01/12/98 C:85 PROJECT: WC577                                 *
     H*                ADDED ERROR CHECKING FOR CLASS CODE (SCCLAS),  *
     H*                IF IN ADD MODE.                                *
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
     H*  30 - ON IF WCFL03='Y' (REPORTING NCCI).                      *
     H*  41 - (WC30005) Record already exists.                        *
     H*  42 - (WCR0504) The state code was not found in list.         *
     H*  43 - (WCR0500) Invalid date.                                 *
     H*  44 - (WCR0502) INVALID DISC CODE.                            *
     H*                                                               *
     H* SCR02:                                                        *
     H*                                                               *
     H* SCR03:                                                        *
     H*                                                               *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW2185F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWTLMP     UF A E             DISK
     F                                     RENAME(PLM01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWTLML1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     FWMFNL     IF   E           K DISK
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     F*
     F*** BEGIN ADD *** 01/12/98 **************************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     F*** END   ADD *** 01/12/98 **************************************
     F*
     D*
      ***** BEG DEL ******* 08/16/11 **********************************
     D******/COPY SCOPYMT01I
      ***** END DEL ******* 08/16/11 **********************************
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D OPTNM           C                   CONST('Increased Limits D-
     D                                     efinition')
     D #OFSCR          C                   CONST(1)
     D COMAPI          C                   CONST('WA001R')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      ***** BEG ADD ******* 08/16/11 **********************************

     d usrdat          ds            20
     d  usrID                        10
     d  runDate                      10

     C/COPY SCOPYMT01C
      ***** END ADD ******* 08/16/11 **********************************
     C*
     C     *LIKE         DEFINE    LMCO#         XXCO#
     C     *LIKE         DEFINE    LMFND         XXFND
     C     *LIKE         DEFINE    LMST          XXST
     C     *LIKE         DEFINE    LMKEY         XXCODE
     C     *LIKE         DEFINE    LMEFDT        XXEFDT
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*  TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
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
     C*   KEY LIST FOR THE DEDUCTIBLE DISCOUNT CODE FILE
     C*
     C     LMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXCODE
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
      ***** BEG DEL ******* 08/16/11 **********************************
     C******/COPY SCOPYMT01C
      ***** END DEL ******* 08/16/11 **********************************
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
     C     LMSTAT        IFEQ      'I'
     C                   MOVE      'A'           LMSTAT
     C                   ELSE
     C                   MOVE      'I'           LMSTAT
     C                   ENDIF                                                  DPSTAT=I
     C*
     C                   MOVEL     Q1USER        LMUSER
     C                   MOVEL     Q1PGM         LMPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LMDT
     C*
     C                   MOVEL     DSTIME        LMTIME
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
     C*  RECORD MUST NOT ALREADY EXISTS FOR ST/CODE/EFDT
     C*
     C                   MOVE      SCST          XXST
     C                   MOVE      SCCODE        XXCODE
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C     LMKY          CHAIN     PLM01                              60
     C  N90*IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN60 EQ 0
     C*
     C                   ENDIF                                                  *IN40 EQ 1
     C*
      *** BEGIN CHG *** 10/01/98 **************************************
     C*** BEGIN ADD *** 01/12/98 **************************************
     C*
     C*  THE CLASS CODE MUST NOT BE ZEROS.
     C*
     C********** *IN90     IFEQ *OFF
     C********** SCCLAS    ANDEQ*ZEROS
     C*
     C*  GET ELECTRONIC SUBMISSION FLAG
     C*
     C**********           MOVEL'WC'      XXSYK
     C********** SYKY      CHAINSWC01                60
     C*
     C********** WCFL03    IFEQ 'Y'
     C**********           MOVE *ON       *IN26
     C**********           MOVE *ON       *IN90
     C**********           ENDIF
     C*
     C**********           ENDIF                           *IN90=OFF
     C*
     C*** END   ADD *** 01/12/98 **************************************
     C*
     C*  THE CLASS CODE IS REQUIRED IF REPORTING NCCI (WCFL03='Y')
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCLAS        ANDEQ     *ZEROS
     C     *IN30         ANDEQ     *ON
     C                   MOVE      *ON           *IN26
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *** END   CHG *** 10/01/98 **************************************
     C*
     C*  THE PART CODE CANNOT BE BLANK
     C*
     C     SCDESC        IFEQ      *BLANK
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCDESC IFEQ
     C*
     C*  INJURY BY ACCIDENT CANNOT BE LESS THAN ZERO.
     C*
     C     SCACC         IFLT      *ZERO
     C                   MOVE      *ON           *IN21
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCACC IFEQ
     C*
     C*  INJURY BY DISEASE CANNOT BE LESS THAN ZERO.
     C*
     C     SCDIS         IFLT      *ZERO
     C                   MOVE      *ON           *IN22
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCDIS IFEQ
     C*
     C*  INJURY BY DISEASE AGGR CANNOT BE LESS THAN ZERO.
     C*
     C     SCAGG         IFLT      *ZERO
     C                   MOVE      *ON           *IN23
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCAGG IFEQ
     C*
     C*  MINIMUM PREMIUM CANNOT BE LESS THAN ZERO.
     C*
      ***** BEG ADD ******* 08/16/11 **********************************
     c                   if        *in44 = *off

      ***** END ADD ******* 08/16/11 **********************************
     C     SCMIN         IFLT      *ZERO
     C                   MOVE      *ON           *IN24
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCMIN IFEQ
      ***** BEG ADD ******* 08/16/11 **********************************

     c                   else

     c     SCMINe        IFLT      *ZERO
     c                   MOVE      *ON           *IN24
     c                   MOVE      *ON           *IN90
     c                   ENDIF                                                  SCMIN IFEQ

     c                   endif
      ***** END ADD ******* 08/16/11 **********************************
     C*
     C*  PERCENTAGE CANNOT BE LESS THAN ZERO.
     C*
     C     SCPCT         IFLT      *ZERO
     C                   MOVE      *ON           *IN25
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCPCT IFEQ
     C*
     C                   ENDIF                                                  *IN92=*OFF
      ***** BEG ADD ******* 09/20/99 **********************************
      *
      *  Auto Renew must be 'N', if entered.
      *
     C     SCRNW         IFNE      *BLANKS
     C     SCRNW         ANDNE     'N'
     C                   SETON                                        9071
     C                   ENDIF                                                  SCRNW
      *
      ***** END ADD ******* 09/20/99 **********************************
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
     C                   MOVE      LMST          SCST
     C                   MOVEL     LMKEY         SCCODE
     C                   Z-ADD     LMEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   MOVE      LMSTAT        SCSTAT
     C                   MOVEL     LMDESC        SCDESC
     C                   Z-ADD     LMACC         SCACC
     C                   Z-ADD     LMDIS         SCDIS
     C                   Z-ADD     LMAGG         SCAGG
     C                   Z-ADD     LMMIN         SCMIN
      ***** BEG ADD ******* 08/16/11 **********************************
     C                   Z-ADD     LMMINe        SCMINe
      ***** END ADD ******* 08/16/11 **********************************
     C                   Z-ADD     LMPCT         SCPCT
     C                   MOVE      LMCLAS        SCCLAS
      ***** BEG ADD ******* 09/20/99 **********************************
     C                   MOVE      LMRNW         SCRNW
      ***** END ADD ******* 09/20/99 **********************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     XXCO#         LMCO#
     C                   Z-ADD     XXFND         LMFND
     C                   MOVE      SCST          LMST
     C                   MOVE      SCSTAT        LMSTAT
     C                   MOVE      *BLANKS       LMDESC
     C                   MOVEL     SCDESC        LMDESC
     C                   MOVE      SCCLAS        LMCLAS
     C                   Z-ADD     SCPCT         LMPCT
     C                   Z-ADD     SCACC         LMACC
     C                   Z-ADD     SCDIS         LMDIS
     C                   Z-ADD     SCMIN         LMMIN
      ***** BEG ADD ******* 08/16/11 **********************************
     C                   Z-ADD     SCMINe        LMMINe
      ***** END ADD ******* 08/16/11 **********************************
     C                   Z-ADD     SCAGG         LMAGG
     C                   MOVEL     SCCODE        LMKEY
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LMEFDT
      ***** BEG ADD ******* 09/20/99 **********************************
     C                   MOVE      SCRNW         LMRNW
      ***** END ADD ******* 09/20/99 **********************************
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
     C                   MOVE      LMST          XXST
     C                   Z-ADD     LMEFDT        XXEFDT
     C                   MOVEL     LMKEY         XXCODE
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNNAME        SCNAME
     C                   ENDIF                                                  *IN60 EQ 0
     C*
      ********* BEGIN ADD ******** 10/01/98 ***************************
     C*
     C*  GET ELECTRONIC SUBMISSION FLAG
     C*  IF FLAG = 'Y' THEN STATISTICAL REPORTING SHOULD BE REQUIRED.
     C*
     C                   MOVE      'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C*
     C     WCFL03        IFEQ      'Y'
     C                   SETON                                        30
     C                   ENDIF
      ********* END   ADD ******** 10/01/98 ***************************
      ***** BEG ADD ******* 08/16/11 **********************************

     c                   exsr      srMinPrm
      ***** END ADD ******* 08/16/11 **********************************
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
      ***** BEG ADD ******* 08/16/11 **********************************


      *===============================================================
      * SRMINPRM - Determine if expanded Minimum Premium Field used
      *===============================================================
     C     srMinPrm      begsr


      * Use expanded Minimum Premium if available

     c     *dtaara       define    minprm        usrdat
     c                   in        usrdat

     c                   if        usrid <> *blank
     c                   eval      *in44 = *on
     c                   else
     c                   eval      *in44 = *off
     c                   endif

     C                   endsr


      ***** END ADD ******* 08/16/11 **********************************
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
     C                   MOVEL     Q1USER        LMUSER
     C                   MOVEL     Q1PGM         LMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LMDT
     C                   MOVEL     DSTIME        LMTIME
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
     C                   MOVE      'A'           LMSTAT
     C                   MOVEL     Q1PGM         LMPGM
     C                   MOVEL     Q1USER        LMUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LMDT
     C                   MOVEL     DSTIME        LMTIME
     C                   MOVEL     Q1PGM         LMIPGM
     C                   MOVEL     Q1USER        LMIUSR
     C                   MOVEL     SRDTO         LMIDT
     C                   MOVEL     DSTIME        LMITIM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WTLMP
     C*
     C                   MOVEL     'C'           @@MODE
     C*
     C                   ENDSR                                                  WRITER
