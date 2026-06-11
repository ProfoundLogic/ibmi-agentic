     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE F2528R01 - RCC by Revenue Code
     H*****************************************************************
     H* PROGRAM DESCRIPTION: RCC by Revenue Code Information          *
     H*                                                               *
     H* CALLED BY: F2528R                                             **
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H*****************************************************************
     H* Created 08/02/2001 by C:62 for Project:114MCG                 *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  99       GENERAL READ/CHAIN                                  *
     H*  71       SCMED# CANNOT BE BLANK                              *
     H*  72       SCRVCD CANNOT BE ZERO                               *
     H*  73       DUPLICATE                                           *
     H*  74       SCRCC CANNOT BE ZERO                                *
     H*                                                               *
     H*****************************************************************
     FF2528F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFARRPPA   UF A E             DISK
     F                                     RENAME(PRR01:DBRCDF)
     FFARRLPA   IF   E           K DISK
     F                                     INFDS(DBFDS)
     FSTABL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('RCC by Revenue Co-
     D                                     de')
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
     C     RRKY          KLIST
     C                   KFLD                    KKMED#
     C                   KFLD                    KKRVCD
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST             10
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    RRMED#        KKMED#
     C     *LIKE         DEFINE    RRRVCD        KKRVCD
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
     C     RRSTAT        IFEQ      'I'
     C                   MOVE      'A'           RRSTAT
     C                   ELSE
     C                   MOVE      'I'           RRSTAT
     C                   ENDIF                                                  RRSTAT
     C*
     C                   MOVEL     Q1USER        RRUSER
     C                   MOVEL     Q1PGM         RRPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RRDT
     C*
     C                   MOVEL     DSTIME        RRTIME
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
     C* MEDICARE NUMBER CAN'T BE BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C     SCMED#        ANDEQ     *BLANKS
     C                   SETON                                        7190
     C                   ENDIF
     C*
     C* REV CODE CAN'T BE ZERO
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRVCD        IFEQ      *ZEROS
     C                   SETON                                        7290
     C                   ENDIF                                                  IF SCRVCD
     C                   ENDIF                                                  IF 90 OFF
     C*
     C* NO DUPLICATES ALLOWED
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  SCMED#        KKMED#
     C                   Z-ADD     SCRVCD        KKRVCD
     C*
     C     RRKY          CHAIN     PRR01                              99
     C     *IN99         DOWEQ     *OFF
     C     @@RRN         IFNE      DBRRN
     C                   SETON                                        7390
     C                   LEAVE
     C                   ENDIF
     C     @@MODE        IFEQ      'P'
     C                   SETON                                        7390
     C                   LEAVE
     C                   ENDIF
     C     RRKY          READE     PRR01                                  99
     C                   ENDDO
     C                   ENDIF
     C*
     C* RCC CAN'T BE ZERO
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRCC         IFEQ      *ZEROS
     C                   SETON                                        7490
     C                   ENDIF                                                  IF SCRCC 0
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
     C*
     C* DISPLAY STATE CODE AND STATE NAME IN HEADER
     C*
     C                   MOVEL     @@ST          KKST
     C                   MOVEL     @@ST          SCST
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C                   MOVEL(P)  STNAME        SCSTNM
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   MOVEL(P)  *BLANKS       SCMED#
     C                   Z-ADD     *ZEROS        SCRVCD
     C                   Z-ADD     *ZEROS        SCRCC
     C                   MOVEL(P)  'A'           SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  RRMED#        SCMED#
     C                   Z-ADD     RRRVCD        SCRVCD
     C                   Z-ADD     RRRCC         SCRCC
     C*
     C     @@MODE        IFEQ      'P'
     C                   MOVEL(P)  'A'           SCSTAT
     C                   ELSE
     C                   MOVEL(P)  RRSTAT        SCSTAT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL(P)  SCMED#        RRMED#
     C                   Z-ADD     SCRVCD        RRRVCD
     C                   Z-ADD     SCRCC         RRRCC
     C                   MOVEL(P)  SCSTAT        RRSTAT
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
     C                   MOVEL     Q1USER        RRUSER
     C                   MOVEL     Q1PGM         RRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RRDT
     C                   MOVEL     DSTIME        RRTIME
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
     C                   MOVEL     Q1USER        RRIUSR
     C                   MOVEL     Q1USER        RRUSER
     C                   MOVEL     Q1PGM         RRIPGM
     C                   MOVEL     Q1PGM         RRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RRIDT
     C                   MOVEL     SRDTO         RRDT
     C                   MOVEL     DSTIME        RRITIM
     C                   MOVEL     DSTIME        RRTIME
      *
     C                   WRITE     DBRCDF
     C                   FEOD      FARRPPA
     C                   ENDSR
