     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* PROGRAM: F2532R01 - VALID STATE CODES INFORMATION - TEXAS
     H*
     H*****************************************************************
     H* CREATE ON 08/27/01 BY C:062 FOR PROJECT:TX015
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS:                                                  *
     H*                                                               *
     H*****************************************************************
     FF2532F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMVCPTX   UF A E             DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PVC01:DBRCDF)
     FFMVCLTX   IF   E           K DISK
     FSTABL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Valid State code-
     D                                      Information')
     D COMAPI          C                   CONST('FA007R')
     D SLMSGF          C                   CONST('FSMESG')
     D #OFSCR          C                   CONST(1)
      *================================================================
      *  PLIST definitions
      *================================================================

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ST              2
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      * PARM LIST FOR CALL TO STATE SPECIFIC PROGRAMS
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      @@ST          ##ST              2
      *
      *================================================================
      *  KLIST definitions
      *================================================================
      *
     C     VCKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCPT4
     C                   KFLD                    KKEFDT
      *
     C     STKY          KLIST
     C                   KFLD                    KKST
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
      *
     C     *LIKE         DEFINE    VCCO#         ##CO#
     C     *LIKE         DEFINE    VCCPT4        KKCPT4
     C     *LIKE         DEFINE    VCEFDT        KKEFDT
     C     *LIKE         DEFINE    STKEY         KKST
      *
     C/COPY SCOPYMT01C
      *================================================================
      * CHGSTS - SUBROUTINE FOR CHANGING STATUS
      *================================================================
     C     CHGSTS        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C     *IN99         IFEQ      *OFF
      *
     C     VCSTAT        IFEQ      'I'
     C                   MOVE      'A'           VCSTAT
     C                   ELSE
     C                   MOVE      'I'           VCSTAT
     C                   ENDIF                                                  VCSTAT
      *
     C                   MOVEL     Q1USER        VCUSER
     C                   MOVEL     Q1PGM         VCPGM
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         VCDT
      *
     C                   MOVEL     DSTIME        VCTIME
      *
     C                   UPDATE    DBRCDF
      *
     C                   ENDIF                                                  *IN99
      *
     C                   ENDSR                                                  CHGSTS
      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
      *===============================================================
      * DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
      *===============================================================
     C     DSPLYR        BEGSR
      *
      *
     C                   ENDSR                                                  DSPLYR
      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR
      *
     C                   SETOFF                                       52
      *
      * CPT-4 CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
      *
     C   40
     CANN90SCCPT4        IFEQ      *BLANKS
     C                   SETON                                        7190
     C                   ENDIF                                                  SCKEY
      *
      * EFFECTIVE DATE MUST BE VALID FOR COPY/ADD
      *
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C     *IN90         OREQ      *OFF
     C     *IN30         ANDEQ     *ON
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
      *
     C     SRERR         IFEQ      '1'
      *
     C                   SETON                                        7290
      *
     C                   ENDIF                                                  SRERR
      *
     C                   ENDIF                                                  *IN90/*IN40
      *
      * BE SURE CPT-4 CODE HAS NOT BEEN USED PREVIOUSLY FOR COPY/ADD
      *
     C                   MOVEL     SCCPT4        KKCPT4
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
      *
     C  N90*IN30         IFEQ      *ON
     C     *IN40         OREQ      *ON
      *
     C     VCKY          CHAIN     PVC01                              99
      *
     C  N99              SETON                                        7390
      *
     C                   ENDIF                                                  *IN30=OFF
      *
      * CPT-4 CODE DESCRIPTION CANNOT BE BLANK
      *
     C  N90SCDSC1        IFEQ      *BLANKS
     C                   SETON                                        7490
     C                   ENDIF                                                  SCDSC1
      *
      * CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
      *
     C  N90VCDT          IFGT      SAVEDT
     C     VCDT          OREQ      SAVEDT
     C     VCTIME        ANDGT     SAVETM
     C                   SETON                                        8990
     C                   ENDIF
      *
      * WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   ENDSR
      *================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR02)
      *================================================================
     C     ERRCK2        BEGSR
      *
      * WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   ENDSR
      *================================================================
      * ERRCK3 - Error checking for srceen 3 (SCR03)
      *================================================================
     C     ERRCK3        BEGSR
      *
      * WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   ENDSR
      *================================================================
      * MKSTHD - Creates the state header
      *================================================================
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
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C     *IN40         IFEQ      *OFF
      *
     C                   MOVEL     VCCPT4        SCCPT4
      *
     C                   Z-ADD     VCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
      *
     C                   MOVEL(P)  VCDSC1        SCDSC1
     C                   MOVEL(P)  VCDSC2        SCDSC2
     C                   MOVEL     VCSTAT        SCSTAT
     C*
     C                   ELSE
      *
     C                   MOVEL     *BLANKS       SCCPT4
     C                   MOVE      *ZEROS        SCEFDT
     C                   MOVE      *BLANK        SCDSC1
     C                   MOVE      *BLANK        SCDSC2
     C                   MOVEL     'A'           SCSTAT
      *
     C                   ENDIF
      *
     C                   ENDSR
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR
      *
     C                   Z-ADD     ##CO#         VCCO#
     C                   MOVEL     SCCPT4        VCCPT4
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         VCEFDT
      *
     C                   MOVE      SCDSC1        VCDSC1
     C                   MOVE      SCDSC2        VCDSC2
     C                   MOVEL     SCSTAT        VCSTAT
      *
     C                   ENDSR
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     OTHFKS        BEGSR
      *
      *
     C                   ENDSR
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
      *
     C                   MOVE      *ON           *INLR
      *
     C                   ENDSR
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C     @@MODE        COMP      'P'                                    30    COPY
      *
     C     *IN09         IFEQ      *ON
      *
     C                   MOVEL(P)  MODED         SCMODE
      *
     C                   ENDIF
      *
     C                   MOVEL(P)  OPTNM         ##DATA
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
      *
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
      *
      * Executing the sub-routine to make the state header.
      *
     C                   EXSR      MKSTHD
      *
     C                   ENDSR                                                  PGMSTR
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR
      *
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
      *
     C                   ENDSR
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     UPDATR        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
B01  C     *IN99         IFEQ      *OFF
      *
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        VCUSER
     C                   MOVEL     Q1PGM         VCPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         VCDT
      *
     C                   MOVEL     DSTIME        VCTIME
      *
     C                   UPDATE    DBRCDF
      *
E01  C                   ENDIF
      *
     C                   ENDSR
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR
      *
     C                   CLEAR                   DBRCDF
      *
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        VCUSER
     C                   MOVEL     Q1USER        VCIUSR
     C                   MOVEL     Q1PGM         VCPGM
     C                   MOVEL     Q1PGM         VCIPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         VCDT
     C                   MOVEL     SRDTO         VCIDT
      *
     C                   MOVEL     DSTIME        VCTIME
     C                   MOVEL     DSTIME        VCITIM
      *
     C                   WRITE     DBRCDF
      *
     C                   FEOD      FMVCPTX
      *
     C                   ENDSR
