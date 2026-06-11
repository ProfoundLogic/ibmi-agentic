     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W2522R01 - ASC Regions/Groups
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* MAINTENANCE PROGRAM FOR ASC REGIONS/GROUPS                    *
     H*                                                               *
     H* CALLED BY: F2522R                                              *
     H*                                                               *
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H* CREATED: 06/22/2001                                           *
     H*                                                               *
     H* 06/22/01 C:136 PROJECT 114MCG                                 *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H* 09/28/04 C:79 PROJ: TX026                                     *
     H*               USE OVERRIDE FILES                              *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  71       SCRGN CANNOT BE BLANK                               *
     H*  72       SCGRP CANNOT BE BLANK                               *
     H*  73       SCEFDT CANNOT BE BLANK                              *
     H*  74       SCEFDT MUST BE VALID                                *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FF2522F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     F*** BEG CHG *** 09/28/04 ****************************************
     F**** FAAGPPA UF  E                    DISK                      A
     FFAAGP     UF A E             DISK
     F*** END CHG *** 09/28/04 ****************************************
     F                                     RENAME(PAG01:DBRCDF)
     FSTABL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('ASC Regions/Groups')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ST              2
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     STKY          KLIST
     C                   KFLD                    KKST             10
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
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
     C     AGSTAT        IFEQ      'I'
     C                   MOVE      'A'           AGSTAT
     C                   ELSE
     C                   MOVE      'I'           AGSTAT
     C                   ENDIF                                                  AGSTAT
     C*
     C                   MOVEL     Q1USER        AGUSER
     C                   MOVEL     Q1PGM         AGPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AGDT
     C*
     C                   MOVEL     DSTIME        AGTIME
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
     C     *IN90         IFEQ      *OFF
     C     SCRGN         ANDEQ     *ZEROS
     C                   SETON                                        71  90
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCGRP         ANDEQ     *ZEROS
     C                   SETON                                        72  90
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCEFDT        ANDEQ     *ZEROS
     C                   SETON                                        73  90
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCEFDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        74  90
     C                   ENDIF                                                  IF SRERR 1
     C                   ENDIF                                                  IF 90 OFF
     C*
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
     C* MKSTHD - Creates the state header
     C*================================================================
     C     MKSTHD        BEGSR
      *
      * DISPLAY STATE CODE AND STATE NAME IN HEADER
      *
     C                   MOVEL     @@ST          KKST
     C                   MOVEL     @@ST          SCST
      *
     C     STKY          CHAIN     PST01                              99
      *
     C                   MOVEL(P)  STNAME        SCSTNM
      *
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     AGRGN         SCRGN
     C                   Z-ADD     AGGRP         SCGRP
     C                   Z-ADD     AGALOW        SCALOW
     C*
     C                   Z-ADD     AGEFDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     @@MODE        IFEQ      'A'                                          ADD MODE
     C                   MOVE(P)   'A'           SCSTAT                         ACTIVE STATUS
     C                   ELSE
     C                   MOVEL(P)  AGSTAT        SCSTAT
     C                   ENDIF                                                  @@MODE = A
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCRGN         AGRGN
     C                   Z-ADD     SCGRP         AGGRP
     C                   Z-ADD     SCALOW        AGALOW
     C                   MOVEL(P)  SCSTAT        AGSTAT
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AGEFDT
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
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'A'                                    40
     C*
     C* Executing the sub-routine to make the state header.
     C*
     C                   EXSR      MKSTHD
     C*
     C                   ENDSR
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
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        AGUSER
     C                   MOVEL     Q1PGM         AGPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AGDT
     C                   MOVEL     DSTIME        AGTIME
      *
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        AGIUSR
     C                   MOVEL     Q1USER        AGUSER
     C                   MOVEL     Q1PGM         AGIPGM
     C                   MOVEL     Q1PGM         AGPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AGIDT
     C                   MOVEL     SRDTO         AGDT
     C                   MOVEL     DSTIME        AGITIM
     C                   MOVEL     DSTIME        AGTIME
      *
     C                   WRITE     DBRCDF
     C                   FEOD      FAAGP
     C                   ENDSR
