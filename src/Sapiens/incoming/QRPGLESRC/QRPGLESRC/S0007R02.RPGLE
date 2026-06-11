     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE S0007R02 - SYSTEM FOLDER CONTROL MAINTENANCE.
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE SYSTEM FOLDER CONTROL.   *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    S0007R - SYSTEM FOLDER CONTROL MAINTENANCE                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 11/04/97  PROGRAMMER: C:65  PROJECT: 68AWX          *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  09/22/99 C:65 PROJECT: SUPPORT                               *
     H*                REMOVE CO# FROM THE KEY FOR THE FOLDER FILES.  *
     H*                                                               *
     H* 09/16/99 C:61  PROJECT WC577                                  *
     H*              - Remove F10=Search from screen.                 *
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS USED:                                              *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD                                                     *
     H* N40 - UPDATE/INQUIRY                                          *
     H*  92 - GENERAL F KEY PRESSED                                   *
     H*  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FS0007F02  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FSMFCP     UF A E             DISK
     F                                     RENAME(PFC01:DBRCDF)
     FSMFCL     IF   E           K DISK
     FSMFCL2    UF   E           K DISK
     F                                     RENAME(PFC01:PFC03)
     FSMFCL1    IF   E           K DISK
     F                                     RENAME(PFC01:PFC02)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('System Folder Contro-
     D                                     l')
     D COMAPI          C                   CONST('SA005R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  KLIST definitions
     C*================================================================
      *
      *  KEY LIST FOR SYSTEM FOLDER CONTROL MASTER FILE
      *
     C     FCKY          KLIST
     C*** BEGIN DEL ***  09/22/99  ************************************
     C**********           KFLD           XXCO#
     C*** END DEL *****  09/22/99  ************************************
     C                   KFLD                    XXPRE
     C                   KFLD                    XXSTAT
      *
     C     FCKY2         KLIST
     C*** BEGIN DEL ***  09/22/99  ************************************
     C************         KFLD           XXCO#
     C*** END DEL *****  09/22/99  ************************************
     C                   KFLD                    XXFLRS
      *
     C     FCKY3         KLIST
     C*** BEGIN DEL ***  09/22/99  ************************************
     C************         KFLD           XXCO#
     C*** END DEL *****  09/22/99  ************************************
     C                   KFLD                    XXPRE
     C                   KFLD                    X2FLRS
     C                   KFLD                    XXSTAT
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
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##CONM           40
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C*** BEGIN DEL ***  09/22/99  ************************************
     C*******    *LIKE     DEFN FCCO#     XXCO#
     C*** END DEL *****  09/22/99  ************************************
     C     *LIKE         DEFINE    FCPRE         XXPRE
     C     *LIKE         DEFINE    FCFLRS        XXFLRS
     C     *LIKE         DEFINE    FCFLRS        X2FLRS
     C     *LIKE         DEFINE    FCSTAT        XXSTAT
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - Change status of record
     C*================================================================
     C     CHGSTS        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     FCSTAT        IFEQ      'I'
     C                   MOVE      'A'           FCSTAT
     C                   ELSE
     C                   MOVE      'I'           FCSTAT
     C                   ENDIF                                                  FCSTAT IFEQ
      *
     C                   MOVEL     Q1USER        FCUSER
     C                   MOVEL     Q1PGM         FCPGM
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FCDT
      *
     C                   MOVEL     DSTIME        FCTIME
      *
     C                   UPDATE    DBRCDF
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDSR                                                  CHGSTS
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
      *
     C     *IN40         IFEQ      *ON
      *
      * DOCUMENT PREFIX MUST NOT CONTAIN BLANKS
      *
     C     ' '           SCAN      SCPRE                                  71
     C     ' '           SCAN      SCPRE                                  90
      *
     C                   ENDIF                                                  *IN40 IFEQ
      *
      * FOLDER STRING MUST NOT CONTAIN BLANKS
      *
     C  N90' '           SCAN      SCFLRS                                 73
     C  N90' '           SCAN      SCFLRS                                 90
      *
      * FOLDER STRING MUST NOT ALREADY EXIST IN THE FILE
      *
     C     *IN90         IFEQ      *OFF
     C*** BEGIN ADD *** 10/30/98 **************************************
     C     SCFLRS        ANDNE     SVFLRS
     C*** END   ADD *** 10/30/98 **************************************
      *
     C*** BEGIN DEL ***  09/22/99  ************************************
     C*********            MOVE ##CO#     XXCO#
     C*** END DEL *****  09/22/99  ************************************
     C                   MOVE      SCFLRS        XXFLRS
      *
     C     FCKY2         CHAIN     PFC02                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      *ON           *IN74
     C                   MOVE      *ON           *IN90
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  *IN90 IFEQ
      *
      * FOLDER DESCRIPTION MUST NOT BE BLANK
      *
     C  N90SCDESC        COMP      *BLANKS                                75
     C  N90SCDESC        COMP      *BLANKS                                90
      *
      * DISPLAY WARNING MESSAGE IF DOCUMENT PREFIX ALREADY HAS AN
      * ACTIVE RECORD IN THE FILE.
      *
     C     SCFLRS        IFNE      SVFLRS
      *
     C     *IN61         IFEQ      *OFF
      *
     C     *IN90         IFEQ      *OFF
      *
     C*** BEGIN DEL ***  09/22/99  ************************************
     C************         MOVE ##CO#     XXCO#
     C*** END DEL *****  09/22/99  ************************************
     C                   MOVE      SCPRE         XXPRE
     C                   MOVE      'A'           XXSTAT
      *
     C     FCKY          CHAIN     PFC01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      *ON           *IN72
     C                   MOVE      *ON           *IN61
     C                   MOVE      *ON           *IN90
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  *IN90 IFEQ
      *
     C                   ELSE
      *
     C*** BEGIN DEL ***  09/22/99  ************************************
     C***********          MOVE ##CO#     XXCO#
     C*** END DEL *****  09/22/99  ************************************
     C                   MOVE      SCPRE         XXPRE
     C                   MOVE      'A'           XXSTAT
      *
     C     FCKY          CHAIN     PFC01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
      * INACTIVATE OLD RECORD AFTER WRITING NEW RECORD.
      *
     C                   MOVEL(P)  SCFLRS        X2FLRS
     C                   MOVE      *ON           *IN62
     C                   MOVE      'A'           @@MODE
     C                   ELSE
     C                   MOVE      *OFF          *IN62
     C                   ENDIF
     C*
     C                   MOVE      *OFF          *IN61
      *
     C                   ENDIF                                                  *IN61 IFEQ
      *
     C                   ENDIF                                                  SCFLRS IFNE
      *
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVE      FCPRE         SCPRE
     C                   MOVE      FCFLRS        SCFLRS
     C                   MOVE      FCFLRS        SVFLRS            5
     C                   MOVE      FCDESC        SCDESC
      *
     C     *IN40         IFEQ      *ON
      *
     C                   MOVE      'A'           SCSTAT
      *
     C                   ELSE
      *
     C                   MOVE      FCSTAT        SCSTAT
      *
     C                   ENDIF                                                  *IN40 IFEQ
      *
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
      *
     C                   MOVE      SCPRE         FCPRE
     C                   MOVE      SCFLRS        FCFLRS
     C                   MOVE      SCDESC        FCDESC
     C                   MOVE      SCSTAT        FCSTAT
      *
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
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
      *
     C                   MOVE      *OFF          *IN61
     C                   MOVE      *OFF          *IN62
      *
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
      *
     C     @@MODE        COMP      'A'                                    40
     C     @@MODE        COMP      'D'                                    09
      *
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
      *
      * IF MODE IS 'S', THIS PROGRAM HAS BEEN CALLED TO CHANGE THE
      * STATUS OF A RECORD.  DO NO OTHER PROCESSING
      *
     C     @@MODE        IFEQ      'S'
      *
     C                   EXSR      CHGSTS
      *
     C                   RETURN
      *
     C                   ENDIF                                                  @@MODE OFEQ
      *
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C     *IN99         IFEQ      *OFF
      *
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        FCUSER
     C                   MOVEL     Q1PGM         FCPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C                   MOVEL     SRDTO         FCDT
     C                   MOVEL     DSTIME        FCTIME
      *
     C                   UPDATE    DBRCDF
      *
     C                   ENDIF                                                  *IN99 IFEQ
      *
     C                   ENDSR                                                  UPDATR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
      *
     C                   CLEAR                   DBRCDF
      *
     C                   EXSR      MVSCDB
      *
     C*** BEGIN DEL ***  09/22/99  ************************************
     C***********          MOVE ##CO#     FCCO#
     C*** END DEL *****  09/22/99  ************************************
     C                   MOVEL     Q1USER        FCUSER
     C                   MOVEL     Q1USER        FCIUSR
     C                   MOVEL     Q1PGM         FCPGM
     C                   MOVEL     Q1PGM         FCIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C                   MOVEL     SRDTO         FCDT
     C                   MOVEL     SRDTO         FCIDT
     C                   MOVEL     DSTIME        FCTIME
      *
      *
     C                   WRITE     DBRCDF
      *
     C                   FEOD      SMFCP
      *
      * INACTIVATE OLD RECORD HERE IF ADDING SAME PREFIX.
      *
     C     *IN62         IFEQ      *ON
      *
     C*** BEGIN DEL ***  09/22/99  ************************************
     C***********          MOVE ##CO#     XXCO#
     C*** END DEL *****  09/22/99  ************************************
     C                   MOVE      SCPRE         XXPRE
     C                   MOVE      SVFLRS        X2FLRS
     C                   MOVE      'A'           XXSTAT
      *
     C     FCKY3         CHAIN     PFC03                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      'I'           FCSTAT
     C                   MOVEL     Q1USER        FCUSER
     C                   MOVEL     Q1PGM         FCPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FCDT
     C                   MOVEL     DSTIME        FCTIME
      *
     C                   UPDATE    PFC03
      *
     C                   MOVE      *ON           OUTFLG
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  *IN62 IFEQ
      *
     C                   ENDSR                                                  WRITER
