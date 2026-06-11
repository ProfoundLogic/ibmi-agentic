     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H******************************************************************
     H* REVISIONS:                                                     *
     H*                                                                *
     H* 06/25/25 C:65  PROJECT: WC2952/PHW#119268 (PRESCIENT)          *
     H*                ADD DATE FOR ADOPTION OF 3 DIGIT RATES.         *
     H*                                                                *
     H*  03/25/13 C:279 Redmine: 2575                                  *
     H*                 Modified to initialize checkbox 'Consent to    *
     H*                 Rate' with 'N' on add.                         *
     H*                                                                *
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                                *
     H* 12/19/97  C:35  PROJECT:WC577                                  *
     H*                 CHANGED CODE TO ALLOW ONLY ONE RECORD FOR A    *
     H*                 GIVEN RATE SET CODE (RATHER THAN ONE PER CODE  *
     H*                 PER STATE).                                    *
     H*                                                                *
     H******************************************************************
     FW2471F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMRSP     UF A E             DISK
     F                                     RENAME(PRS01:DBRCDF)
     FWMRSL     IF   E           K DISK
     FSTSTL3    IF   E           K DISK
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PST02)
     FWTGTL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Rate Set Code -
     D                                     Master Information')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
     C*================================================================
     C*  Procedure definitions
     C*================================================================
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  STATE PARM LIST
     C*
     C     STSPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     RSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##KEY
     C*
     C*  KEY LIST FOR THE SYSTEM-WIDE STATE CODE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    ##KY2            10
     C*
     C*  KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##TBID
     C                   KFLD                    ##KY2
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    RSCO#         ##CO#
     C     *LIKE         DEFINE    RSRTCD        ##KEY
     C
     C     *LIKE         DEFINE    RSRTCD        @@KEY
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     RSSTAT        IFEQ      'I'
     C                   MOVE      'A'           RSSTAT
     C                   ELSE
     C                   MOVE      'I'           RSSTAT
     C                   ENDIF                                                  RSSTAT
     C*
     C                   MOVEL     Q1USER        RSUSER
     C                   MOVEL     Q1PGM         RSPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RSDT
     C*
     C                   MOVEL     DSTIME        RSTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C* RATESET  CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCKEY         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C*  THE JURISDICTION STATE MUST EXIST IN STSTP OR MUST BE A
     C*  VALID TABLE ENTRY.
     C*
     C                   MOVEL(P)  SCJRST        ##KY2
     C*
     C  N90
     CAN 40STKY          CHAIN     PST01                              99
     C*
     C  N90
     CAN 40*IN99         IFEQ      '1'
     C*
     C                   MOVEL     'JURS'        ##TBID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      '1'
     C*
     C                   SETON                                        9074
     C*
     C                   ENDIF                                                  *IN99 GTKY
     C*
     C                   ENDIF                                                  *IN99 STKY
     C*
     C* CHECK TO BE SURE RATE SET CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCKEY         ##KEY
     C*
     C   40
     CANN90RSKY          CHAIN     PRS01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* BR-2 CODE DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C*  LOST TIME MUST BE 'Y' OR 'N'.
     C*
     C  N90SCCRFL        IFNE      'Y'
     C     SCCRFL        ANDNE     'N'
     C*
     C                   SETON                                        7990
     C*
     C                   ENDIF
     C*** begin add ***  06/25/25  **********************************************
     C                   Clear                   ms3Date
     C                   Clear                   er3Date
     C*
     C  N90sc3Date       IFNE      *zero
     C*
     C                   Z-ADD     SC3Date       SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SC3Date
     C                   ELSE
     C                   SETON                                        90
     C                   Movel(p)  'WCR0500'     ms3Date
     C                   Move      *on           er3Date
     C                   ENDIF                                                  SCERR=*OFF
     C*
     C                   ENDIF                                                  sc3Date IFNE 0
     C*** end add *****  06/25/25  **********************************************
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90RSDT          IFGT      SAVEDT
     C     RSDT          OREQ      SAVEDT
     C     RSTIME        ANDGT     SAVETM
     C                   SETON                                        9078
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH SUBSET SCREEN.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCJRST'
     C*
     C                   CALL      'SY601R'      STSPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        8090
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PST02                              99
     C*
     C                   MOVEL     STKEY         SCJRST
     C                   SETON                                        8190
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     RSRTCD        SCKEY
     C                   MOVEL     RSDESC        SCDESC
     C                   MOVEL     RSCRFL        SCCRFL
     C                   MOVEL     RSJRST        SCJRST
     C                   MOVEL     RSSTAT        SCSTAT
     C*** begin add ***  06/25/25  **************************************
     C                   Z-ADD     RS3Date       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SC3Date
     C*** end add *****  06/25/25  **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCKEY
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      'N'           SCCRFL
     C                   MOVE      *BLANKS       SCJRST
     C                   MOVEL     'A'           SCSTAT
     C*** begin add ***  06/25/25  **************************************
     C                   Clear                   sc3Date
     C*** end add *****  06/25/25  **************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         RSCO#
     C                   MOVEL     SCKEY         RSRTCD
     C                   MOVEL     SCDESC        RSDESC
     C                   MOVEL     SCCRFL        RSCRFL
     C                   MOVEL     SCJRST        RSJRST
     C                   MOVEL     SCSTAT        RSSTAT
     C*** begin add ***  06/25/25  **************************************
     C                   Z-ADD     SC3Date       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RS3Date
     C*** end add *****  06/25/25  **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
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
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        RSUSER
     C                   MOVEL     Q1PGM         RSPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RSDT
     C*
     C                   MOVEL     DSTIME        RSTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        RSUSER
     C                   MOVEL     Q1PGM         RSPGM
     C                   MOVEL     Q1USER        RSIUSR
     C                   MOVEL     Q1PGM         RSIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RSDT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RSIDT
     C*
     C                   MOVEL     DSTIME        RSTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMRSP
     C*
     C                   ENDSR
