     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE F2535R01 - DRG Factors and CCR Information
     H*
     H*****************************************************************
     H* Created on 09/27/2002 by c:62 for Project CA001
     H*****************************************************************
     H* Program Description: DRG Factors and CCR Information          *
     H*                                                               *
     H* Called by: F2535R                                             *
     H* Calls to : None                                               *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     FF2535F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFADFP     UF A E             DISK
     F                                     RENAME(PDF01:DBRCDF)
     FFADFL     IF   E           K DISK
     F*
     FSTABL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('DRG Factors and CCRs')
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
     C     DFKY          KLIST
     C                   KFLD                    KKMED#
     C                   KFLD                    KKEFDT
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST             10
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    DFMED#        KKMED#
     C     *LIKE         DEFINE    DFEFDT        KKEFDT
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
     C     DFSTAT        IFEQ      'I'
     C                   MOVE      'A'           DFSTAT
     C                   ELSE
     C                   MOVE      'I'           DFSTAT
     C                   ENDIF                                                  DFSTAT
     C*
     C                   MOVEL     Q1USER        DFUSER
     C                   MOVEL     Q1PGM         DFPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DFDT
     C*
     C                   MOVEL     DSTIME        DFTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
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
     C*
     C     *IN40         IFEQ      *ON
     C*
     C* MEDICARE # CAN'T BE BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMED#        ANDEQ     *BLANKS
     C                   SETON                                        7190
     C                   ENDIF
     C*
     C* NAME CAN'T BE BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C     SCNAME        ANDEQ     *BLANKS
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     *IN40         IFEQ      *ON
     C     *IN41         OREQ      *ON
     C*
     C* EFFECTIVE DATE MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7390
     C                   ENDIF
     C                   ENDIF
     C*
     C* DUPLICATES ARE NOT ALLOWED
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL(P)  SCMED#        KKMED#
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C     DFKY          CHAIN     PDF01                              99
     C     *IN99         IFEQ      *OFF
     C     *IN40         IFEQ      *ON
     C                   SETON                                        7490
     C                   ELSE
     C                   SETON                                        7590
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
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
     C     *IN40         IFEQ      *ON
     C*
     C                   CLEAR                   SCMED#
     C                   CLEAR                   SCNAME
     C                   CLEAR                   SCEFDT
     C                   CLEAR                   SCCOMP
     C                   CLEAR                   SCCCCR
     C                   CLEAR                   SCOCCR
     C                   CLEAR                   SCTCCR
     C                   CLEAR                   SCOUTL
     C                   MOVEL     'A'           SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  DFMED#        SCMED#
     C                   MOVEL(P)  DFNAME        SCNAME
     C*
     C                   Z-ADD     DFEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   Z-ADD     DFCOMP        SCCOMP
     C                   Z-ADD     DFCCCR        SCCCCR
     C                   Z-ADD     DFOCCR        SCOCCR
     C                   Z-ADD     DFTCCR        SCTCCR
     C                   Z-ADD     DFOUTL        SCOUTL
     C                   MOVEL(P)  DFSTAT        SCSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL(P)  SCMED#        DFMED#
     C                   MOVEL(P)  SCNAME        DFNAME
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DFEFDT
     C*
     C                   Z-ADD     SCCOMP        DFCOMP
     C                   Z-ADD     SCCCCR        DFCCCR
     C                   Z-ADD     SCOCCR        DFOCCR
     C                   Z-ADD     SCTCCR        DFTCCR
     C                   Z-ADD     SCOUTL        DFOUTL
     C                   MOVEL     SCSTAT        DFSTAT
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
     C                   SETON                                        LR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'A'                                    40
     C     @@MODE        COMP      'P'                                    41
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
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        DFUSER
     C                   MOVEL     Q1PGM         DFPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DFDT
     C                   MOVEL     DSTIME        DFTIME
     C*
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        DFIUSR
     C                   MOVEL     Q1USER        DFUSER
     C                   MOVEL     Q1PGM         DFIPGM
     C                   MOVEL     Q1PGM         DFPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DFIDT
     C                   MOVEL     SRDTO         DFDT
     C                   MOVEL     DSTIME        DFITIM
     C                   MOVEL     DSTIME        DFTIME
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      FADFP
     C*
     C                   ENDSR
