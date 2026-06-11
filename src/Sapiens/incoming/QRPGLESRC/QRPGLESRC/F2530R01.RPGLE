     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE F2530R01 - CORF Table
     H*****************************************************************
     H* PROGRAM DESCRIPTION: CORF Table Information                   *
     H*                                                               *
     H* CALLED BY: F2530R                                             **
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H*****************************************************************
     H* Created 08/08/2001 by C:62 for Project:114MCG                 *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  99       GENERAL READ/CHAIN                                  *
     H*  71       SCMED# CANNOT BE BLANK                              *
     H*  72       SCEFD MUST BE VALID                                 *
     H*  73       DUPLICATE                                           *
     H*  74       SCETD MUST BE VALID                                 *
     H*  75       SCETD MUST BE >= SCEFD                              *
     H*                                                               *
     H*****************************************************************
     FF2530F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFACRPPA   UF A E             DISK
     F                                     RENAME(PCR01:DBRCDF)
     FFACRLPA   IF   E           K DISK
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
     D OPTNM           C                   CONST('CORF Table')
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
     C     CRKY          KLIST
     C                   KFLD                    KKMED#
     C                   KFLD                    KKEFD
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST             10
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CRMED#        KKMED#
     C     *LIKE         DEFINE    CREFD         KKEFD
     C     *LIKE         DEFINE    CREFD         XXEFD
     C     *LIKE         DEFINE    CRETD         XXETD
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
     C     CRSTAT        IFEQ      'I'
     C                   MOVE      'A'           CRSTAT
     C                   ELSE
     C                   MOVE      'I'           CRSTAT
     C                   ENDIF                                                  CRSTAT
     C*
     C                   MOVEL     Q1USER        CRUSER
     C                   MOVEL     Q1PGM         CRPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRDT
     C*
     C                   MOVEL     DSTIME        CRTIME
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
     C* EFFECTIVE FROM DATE MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCEFD         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFD
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7290
     C                   ENDIF
     C                   ENDIF
     C*
     C* NO DUPLICATES ALLOWED
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  SCMED#        KKMED#
     C                   Z-ADD     SCEFD         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFD
     C     CRKY          CHAIN     PCR01                              99
     C     *IN99         DOWEQ     *OFF
     C     @@RRN         IFNE      DBRRN
     C                   SETON                                        7390
     C                   LEAVE
     C                   ENDIF
     C     @@MODE        IFEQ      'P'
     C                   SETON                                        7390
     C                   LEAVE
     C                   ENDIF
     C     CRKY          READE     PCR01                                  99
     C                   ENDDO
     C                   ENDIF
     C*
     C* EFFECTIVE THRU DATE MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCETD         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCETD
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7490
     C                   ENDIF
     C                   ENDIF
     C*
     C* EFFECTIVE THRU DATE MUST BE GREATER THAN OR EQUAL TO FROM DATE
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCEFD         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFD
     C                   Z-ADD     SCETD         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXETD
     C     XXEFD         IFGT      XXETD
     C                   SETON                                        7590
     C                   ENDIF
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
     C                   Z-ADD     *ZEROS        SCEFD
     C                   Z-ADD     *ZEROS        SCETD
     C                   Z-ADD     *ZEROS        SCP42X
     C                   Z-ADD     *ZEROS        SCP43X
     C                   Z-ADD     *ZEROS        SCP44X
     C                   Z-ADD     *ZEROS        SCP55X
     C                   Z-ADD     *ZEROS        SCP56X
     C                   Z-ADD     *ZEROS        SCP91X
     C                   Z-ADD     *ZEROS        SCC42X
     C                   Z-ADD     *ZEROS        SCC43X
     C                   Z-ADD     *ZEROS        SCC44X
     C                   Z-ADD     *ZEROS        SCC55X
     C                   Z-ADD     *ZEROS        SCC56X
     C                   Z-ADD     *ZEROS        SCC91X
     C                   MOVEL(P)  'A'           SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  CRMED#        SCMED#
     C                   Z-ADD     CREFD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFD
     C                   Z-ADD     CRETD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCETD
     C                   Z-ADD     CRP42X        SCP42X
     C                   Z-ADD     CRP43X        SCP43X
     C                   Z-ADD     CRP44X        SCP44X
     C                   Z-ADD     CRP55X        SCP55X
     C                   Z-ADD     CRP56X        SCP56X
     C                   Z-ADD     CRP91X        SCP91X
     C                   Z-ADD     CRC42X        SCC42X
     C                   Z-ADD     CRC43X        SCC43X
     C                   Z-ADD     CRC44X        SCC44X
     C                   Z-ADD     CRC55X        SCC55X
     C                   Z-ADD     CRC56X        SCC56X
     C                   Z-ADD     CRC91X        SCC91X
     C*
     C     @@MODE        IFEQ      'P'
     C                   MOVEL(P)  'A'           SCSTAT
     C                   ELSE
     C                   MOVEL(P)  CRSTAT        SCSTAT
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
     C                   MOVEL(P)  SCMED#        CRMED#
     C                   Z-ADD     SCEFD         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CREFD
     C                   Z-ADD     SCETD         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRETD
     C                   Z-ADD     SCP42X        CRP42X
     C                   Z-ADD     SCP43X        CRP43X
     C                   Z-ADD     SCP44X        CRP44X
     C                   Z-ADD     SCP55X        CRP55X
     C                   Z-ADD     SCP56X        CRP56X
     C                   Z-ADD     SCP91X        CRP91X
     C                   Z-ADD     SCC42X        CRC42X
     C                   Z-ADD     SCC43X        CRC43X
     C                   Z-ADD     SCC44X        CRC44X
     C                   Z-ADD     SCC55X        CRC55X
     C                   Z-ADD     SCC56X        CRC56X
     C                   Z-ADD     SCC91X        CRC91X
     C                   MOVEL(P)  SCSTAT        CRSTAT
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
     C                   MOVEL     Q1USER        CRUSER
     C                   MOVEL     Q1PGM         CRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRDT
     C                   MOVEL     DSTIME        CRTIME
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
     C                   MOVEL     Q1USER        CRIUSR
     C                   MOVEL     Q1USER        CRUSER
     C                   MOVEL     Q1PGM         CRIPGM
     C                   MOVEL     Q1PGM         CRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRIDT
     C                   MOVEL     SRDTO         CRDT
     C                   MOVEL     DSTIME        CRITIM
     C                   MOVEL     DSTIME        CRTIME
      *
     C                   WRITE     DBRCDF
     C                   FEOD      FACRPPA
     C                   ENDSR
