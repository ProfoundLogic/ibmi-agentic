     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE F2527R01 - ASC by MSA and Region
     H*****************************************************************
     H* PROGRAM DESCRIPTION: ASC by MSA and Region Information        *
     H*                                                               *
     H* CALLED BY: F2527R                                             **
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H*****************************************************************
     H* Created 08/01/2001 by C:62 for Project:114MCG                 *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  99       GENERAL READ/CHAIN                                  *
     H*  71       SCMED# CANNOT BE BLANK                              *
     H*  72       SCMSA CANNOT BE ZERO                                *
     H*  73       DUPLICATE                                           *
     H*                                                               *
     H*****************************************************************
     FF2527F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFAMRPPA   UF A E             DISK
     F                                     RENAME(PMR01:DBRCDF)
     FFAMRLPA   IF   E           K DISK
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
     D OPTNM           C                   CONST('ASC by MSA and Re-
     D                                     gion')
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
     C     MRKY          KLIST
     C                   KFLD                    KKMED#
     C                   KFLD                    KKMSA
     C                   KFLD                    KKRGN
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST             10
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    MRMED#        KKMED#
     C     *LIKE         DEFINE    MRMSA         KKMSA
     C     *LIKE         DEFINE    MRRGN         KKRGN
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
     C     MRSTAT        IFEQ      'I'
     C                   MOVE      'A'           MRSTAT
     C                   ELSE
     C                   MOVE      'I'           MRSTAT
     C                   ENDIF                                                  MRSTAT
     C*
     C                   MOVEL     Q1USER        MRUSER
     C                   MOVEL     Q1PGM         MRPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MRDT
     C*
     C                   MOVEL     DSTIME        MRTIME
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
     C* MSA CAN'T BE ZERO
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMSA         IFEQ      *ZEROS
     C                   SETON                                        7290
     C                   ENDIF                                                  IF SCMSA 0
     C                   ENDIF                                                  IF 90 OFF
     C*
     C* NO DUPLICATES ALLOWED
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  SCMED#        KKMED#
     C                   Z-ADD     SCMSA         KKMSA
     C                   Z-ADD     SCRGN         KKRGN
     C*
     C     MRKY          CHAIN     PMR01                              99
     C     *IN99         DOWEQ     *OFF
     C     @@RRN         IFNE      DBRRN
     C                   SETON                                        7390
     C                   LEAVE
     C                   ENDIF
     C     @@MODE        IFEQ      'P'
     C                   SETON                                        7390
     C                   LEAVE
     C                   ENDIF
     C     MRKY          READE     PMR01                                  99
     C                   ENDDO
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
     C                   Z-ADD     *ZEROS        SCMSA
     C                   Z-ADD     *ZEROS        SCRGN
     C                   MOVEL(P)  'A'           SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  MRMED#        SCMED#
     C                   Z-ADD     MRMSA         SCMSA
     C                   Z-ADD     MRRGN         SCRGN
     C*
     C     @@MODE        IFEQ      'P'
     C                   MOVEL(P)  'A'           SCSTAT
     C                   ELSE
     C                   MOVEL(P)  MRSTAT        SCSTAT
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
     C                   MOVEL(P)  SCMED#        MRMED#
     C                   Z-ADD     SCMSA         MRMSA
     C                   Z-ADD     SCRGN         MRRGN
     C                   MOVEL(P)  SCSTAT        MRSTAT
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
     C                   MOVEL     Q1USER        MRUSER
     C                   MOVEL     Q1PGM         MRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MRDT
     C                   MOVEL     DSTIME        MRTIME
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
     C                   MOVEL     Q1USER        MRIUSR
     C                   MOVEL     Q1USER        MRUSER
     C                   MOVEL     Q1PGM         MRIPGM
     C                   MOVEL     Q1PGM         MRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MRIDT
     C                   MOVEL     SRDTO         MRDT
     C                   MOVEL     DSTIME        MRITIM
     C                   MOVEL     DSTIME        MRTIME
      *
     C                   WRITE     DBRCDF
     C                   FEOD      FAMRPPA
     C                   ENDSR
