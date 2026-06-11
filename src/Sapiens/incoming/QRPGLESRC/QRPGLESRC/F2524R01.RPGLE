     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE F2524R01 - HOME HEALTH AGENCY TABLE
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* MAINTENANCE PROGRAM FOR HOME HEATLH AGENCY TABLE              *
     H*                                                               *
     H* CALLED BY: F2524R                                              *
     H*                                                               *
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H* CREATED: 06/26/2001                                           *
     H*                                                               *
     H* 06/26/01 C:136 PROJECT 114MCG
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/08/25 C:342 Support 120196  SY458                          *
     H*                DSPF:                                          *
     H*                1.Added read only indicator to 6 fields        *
     H*                  which are below MSA field on scr01 screen    *
     H*                                                               *
     H* 11/18/08 C:198 Project PA046                                  *
     H*                Added NPI#.                                    *
     H*                                                               *
     H*****************************************************************
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  71       SCMED# CANNOT BE BLANK                              *
     H*  72       SCEFDT CAN'T BE BLANK                               *
     H*  73       INVALID DATE                                        *
     H*  74       MSA CAN'T BE BLANK                                  *
     H*                                                               *
     H*****************************************************************
     FF2524F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFAHHPPA   UF A E             DISK
     F                                     RENAME(PHH01:DBRCDF)
     FSTABL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Home Health Agency T-
     D                                     able')
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
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST             10
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    HHEFDT        S1EFDT
     C*
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
     C     HHSTAT        IFEQ      'I'
     C                   MOVE      'A'           HHSTAT
     C                   ELSE
     C                   MOVE      'I'           HHSTAT
     C                   ENDIF                                                  HHSTAT
     C*
     C                   MOVEL     Q1USER        HHUSER
     C                   MOVEL     Q1PGM         HHPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C*
     C                   MOVEL     DSTIME        HHTIME
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
     C     SCMED#        ANDEQ     *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCEFDT        ANDEQ     *ZEROS
     C                   SETON                                        72  90
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
     C                   SETON                                        73  90
     C                   ENDIF                                                  IF SRERR 1
     C                   ENDIF                                                  IF 90 OFF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMSA         ANDEQ     *ZEROS
     C                   SETON                                        74  90
     C                   ENDIF
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
     C                   MOVEL     *BLANKS       SCMED#
     C                   MOVEL     HHMED#        SCMED#
     C*** BEG ADD *** 11/18/08 ****************************************
     C                   MOVEL     HHNPI#        SCNPI#
     C*** END ADD *** 11/18/08 ****************************************
     C*
     C                   Z-ADD     HHEFDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   Z-ADD     HHMSA         SCMSA
     C                   Z-ADD     HHSKNR        SCSKNR
     C                   Z-ADD     HHPTR         SCPTR
     C                   Z-ADD     HHSPTR        SCSPTR
     C                   Z-ADD     HHOCCR        SCOCCR
     C                   Z-ADD     HHSSR         SCSSR
     C                   Z-ADD     HHHAA         SCHAA
     C*
     C     @@MODE        IFEQ      'A'                                          ADD MODE
     C                   MOVE(P)   'A'           SCSTAT                         ACTIVE STATUS
     C                   ELSE
     C                   MOVEL(P)  HHSTAT        SCSTAT
     C                   ENDIF                                                  @@MODE = A
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCMED#        HHMED#
     C*** BEG ADD *** 11/18/08 ****************************************
     C                   MOVEL     SCNPI#        HHNPI#
     C*** END ADD *** 11/18/08 ****************************************
     C                   Z-ADD     SCMSA         HHMSA
     C                   MOVEL     SCMED#        HHMED#
     C                   Z-ADD     SCSKNR        HHSKNR
     C                   Z-ADD     SCPTR         HHPTR
     C                   Z-ADD     SCSPTR        HHSPTR
     C                   Z-ADD     SCOCCR        HHOCCR
     C                   Z-ADD     SCSSR         HHSSR
     C                   Z-ADD     SCHAA         HHHAA
     C                   MOVE      SCSTAT        HHSTAT
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHEFDT
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
     C                   MOVEL     Q1USER        HHUSER
     C                   MOVEL     Q1PGM         HHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C                   MOVEL     DSTIME        HHTIME
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
     C                   MOVEL     Q1USER        HHIUSR
     C                   MOVEL     Q1USER        HHUSER
     C                   MOVEL     Q1PGM         HHIPGM
     C                   MOVEL     Q1PGM         HHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHIDT
     C                   MOVEL     SRDTO         HHDT
     C                   MOVEL     DSTIME        HHITIM
     C                   MOVEL     DSTIME        HHTIME
      *
     C                   WRITE     DBRCDF
     C                   FEOD      FAHHPPA
     C                   ENDSR
