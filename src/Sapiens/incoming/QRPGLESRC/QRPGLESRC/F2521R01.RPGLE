     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W2521R01 - DRG FEDERAL TABLE LISTING
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* MAINTENANCE PROGRAM FOR DRG FEDERAL TABLE LISTING             *
     H*                                                               *
     H* CALLED BY: F2521R                                              *
     H*                                                               *
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H* CREATED: 06/04/2001                                           *
     H*                                                               *
     H* 06/04/01 C:136 PROJECT 114MCG                                 *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H* 09/25/02 C: 79 PROJECT: CA001                                 *
     H*                USE GENERIC FILE FOR SUBFILE NOT STATE SPECIFIC*
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  71       DRG# CANNOT BE BLANK                                *
     H*  72       DRG# MUST BE UNIQUE                                 *
     H*  73       DESCRIPTION CANNOT BE BLANK                         *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     FF2521F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*** BEG DEL *** 09/25/02 ****************************************
     F*  Physical file definition (for maintaining record)
     FFTFEP     UF A E             DISK
     F                                     RENAME(PFE01:DBRCDF)
     FFTFEL     IF   E           K DISK
     F*** END DEL *** 09/25/02 ****************************************
     F*
     F*** BEG DEL *** 09/25/02 ****************************************
     F********
     F***Physical file definition (for maintaining record)
     F***FTFEPPA*UF  E                    DISK                      A
     F********    PFE01                             KRENAMEDBRCDF
     F***FTFELPA*IF  E           K        DISK
     F*** END DEL *** 09/25/02 ****************************************
     FSTABL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('DRG Federal Register-
     D                                      Table')
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
     C     KEY1          KLIST
     C                   KFLD                    KKDRG#
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST             10
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    FEDRG#        KKDRG#
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
     C     FESTAT        IFEQ      'I'
     C                   MOVE      'A'           FESTAT
     C                   ELSE
     C                   MOVE      'I'           FESTAT
     C                   ENDIF                                                  FTSTAT
     C*
     C                   MOVEL     Q1USER        FEUSER
     C                   MOVEL     Q1PGM         FEPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FEDT
     C*
     C                   MOVEL     DSTIME        FETIME
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
     C     SCDRG#        ANDEQ     *ZEROS
     C                   SETON                                        71  90
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     @@MODE        ANDEQ     'A'
     C                   MOVEL     SCDRG#        KKDRG#
     C     KEY1          CHAIN     PFE01                              60
     C  N60              SETON                                        72  90
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDSC1        ANDEQ     *BLANKS
     C                   SETON                                        73  90
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
     C                   Z-ADD     FEDRG#        SCDRG#
     C                   MOVEL     FEDESC        SCDSC1
     C                   MOVE      FEDESC        SCDSC2
     C                   Z-ADD     FERLWT        SCRLWT
     C                   Z-ADD     FEGLOS        SCGLOS
     C                   Z-ADD     FEALOS        SCALOS
     C                   Z-ADD     FEOUTL        SCOUTL
     C*
     C     @@MODE        IFEQ      'A'                                          ADD MODE
     C                   MOVE(P)   'A'           SCSTAT                         ACTIVE STATUS
     C                   ELSE
     C                   MOVEL(P)  FESTAT        SCSTAT
     C                   ENDIF                                                  @@MODE = A
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCDRG#        FEDRG#
     C                   MOVEL     SCDSC1        FEDESC
     C                   MOVE      SCDSC2        FEDESC
     C                   Z-ADD     SCRLWT        FERLWT
     C                   Z-ADD     SCGLOS        FEGLOS
     C                   Z-ADD     SCALOS        FEALOS
     C                   Z-ADD     SCOUTL        FEOUTL
     C                   MOVEL(P)  SCSTAT        FESTAT
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
     C                   MOVEL     Q1USER        FEUSER
     C                   MOVEL     Q1PGM         FEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FEDT
     C                   MOVEL     DSTIME        FETIME
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
     C                   MOVEL     Q1USER        FEIUSR
     C                   MOVEL     Q1USER        FEUSER
     C                   MOVEL     Q1PGM         FEIPGM
     C                   MOVEL     Q1PGM         FEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FEIDT
     C                   MOVEL     SRDTO         FEDT
     C                   MOVEL     DSTIME        FEITIM
     C                   MOVEL     DSTIME        FETIME
      *
     C                   WRITE     DBRCDF
     C                   FEOD      FTFEP
     C                   ENDSR
