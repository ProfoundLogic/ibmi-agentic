     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) ACTGRP(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H* Medicare Rate Maintenance                                     *
     H*                                                               *
     H*****************************************************************
     H* Created on 02/26/2007 by c:62 for Project 114MMZ              *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     FF2545F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMMCP     UF A E             DISK
     F                                     RENAME(PMC01:DBRCDF)
     FFMMCL1    IF   E           K DISK
     FSTABL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Medicare Rate Infor-
     D                                     mation')
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
     C     MCKY          KLIST
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKEFDT
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    MCPRCD        KKPRCD
     C     *LIKE         DEFINE    MCEFDT        KKEFDT
     C     *LIKE         DEFINE    STKEY         KKST
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
     C     *IN99         IFEQ      *OFF
     C     MCSTAT        IFEQ      'I'
     C                   MOVE      'A'           MCSTAT
     C                   ELSE
     C                   MOVE      'I'           MCSTAT
     C                   ENDIF                                                  MCSTAT
     C*
     C                   MOVEL     Q1USER        MCUSER
     C                   MOVEL     Q1PGM         MCPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MCDT
     C*
     C                   MOVEL     DSTIME        MCTIME
     C*
     C                   UPDATE    DBRCDF
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
     C     *IN30         IFEQ      *ON
     C     *IN40         OREQ      *ON
     C*
     C* RATE CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C     *IN90         IFEQ      *OFF
     C     SCPRCD        ANDEQ     *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCPRCD
     C*
     C*  MUST HAVE VALID EFFECTIVE DATE
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7390
     C                   ENDIF                                                  SRERR=1
     C                   ENDIF                                                  *IN99=*OFF
     C*
     C* CHECK TO BE SURE RATE CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL     SCPRCD        KKPRCD
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C     MCKY          CHAIN     PMC01                              99
     C  N99              SETON                                        72  90
     C                   ENDIF                                                  *IN90
     C                   ENDIF                                                  *IN30
     C*
     C* RATE CODE DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDSC1        IFEQ      *BLANKS
     C                   SETON                                        74  90
     C                   ENDIF                                                  SCDSC1
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90MCDT          IFGT      SAVEDT
     C     MCDT          OREQ      SAVEDT
     C     MCTIME        ANDGT     SAVETM
     C                   SETON                                        7990
     C                   ENDIF                                                  MCDT=SAVEDT
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
     C* KJDO - F10=SEARCHES
     C*================================================================
     C     KJDO          BEGSR
     C*
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
     C                   MOVEL(P)  MCPRCD        SCPRCD
     C                   Z-ADD     MCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   MOVEL(P)  MCDSC1        SCDSC1
     C                   MOVEL(P)  MCDSC2        SCDSC2
     C                   MOVEL(P)  MCSTAT        SCSTAT
     C*
     C                   Z-ADD     MCFA01        SCFA01
     C                   Z-ADD     MCNF01        SCNF01
     C                   Z-ADD     MCF501        SCF501
     C                   Z-ADD     MCN501        SCN501
     C                   Z-ADD     MC2601        SC2601
     C                   Z-ADD     MCTC01        SCTC01
     C*
     C                   Z-ADD     MCFA99        SCFA99
     C                   Z-ADD     MCNF99        SCNF99
     C                   Z-ADD     MCF599        SCF599
     C                   Z-ADD     MCN599        SCN599
     C                   Z-ADD     MC2699        SC2699
     C                   Z-ADD     MCTC99        SCTC99
     C   30              MOVEL     'A'           SCSTAT
     C*
     C                   ELSE
     C                   MOVEL     *BLANKS       SCPRCD
     C                   Z-ADD     *ZEROS        SCEFDT
     C                   MOVEL(P)  *BLANKS       SCDSC1
     C                   MOVEL(P)  *BLANKS       SCDSC2
     C*
     C                   Z-ADD     *ZEROS        SCFA01
     C                   Z-ADD     *ZEROS        SCNF01
     C                   Z-ADD     *ZEROS        SCF501
     C                   Z-ADD     *ZEROS        SCN501
     C                   Z-ADD     *ZEROS        SC2601
     C                   Z-ADD     *ZEROS        SCTC01
     C*
     C                   Z-ADD     *ZEROS        SCFA99
     C                   Z-ADD     *ZEROS        SCNF99
     C                   Z-ADD     *ZEROS        SCF599
     C                   Z-ADD     *ZEROS        SCN599
     C                   Z-ADD     *ZEROS        SC2699
     C                   Z-ADD     *ZEROS        SCTC99
     C                   MOVEL     'A'           SCSTAT
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL(P)  SCPRCD        MCPRCD
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MCEFDT
     C                   MOVEL(P)  SCDSC1        MCDSC1
     C                   MOVEL(P)  SCDSC2        MCDSC2
     C                   MOVEL(P)  SCSTAT        MCSTAT
     C*
     C                   Z-ADD     SCFA01        MCFA01
     C                   Z-ADD     SCNF01        MCNF01
     C                   Z-ADD     SCF501        MCF501
     C                   Z-ADD     SCN501        MCN501
     C                   Z-ADD     SC2601        MC2601
     C                   Z-ADD     SCTC01        MCTC01
     C*
     C                   Z-ADD     SCFA99        MCFA99
     C                   Z-ADD     SCNF99        MCNF99
     C                   Z-ADD     SCF599        MCF599
     C                   Z-ADD     SCN599        MCN599
     C                   Z-ADD     SC2699        MC2699
     C                   Z-ADD     SCTC99        MCTC99
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
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'P'                                    30    COPY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C                   MOVEL     @@ST          KKST
     C                   MOVEL     @@ST          SCST
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  STNAME        SCSTNM
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       SCSTNM
     C                   ENDIF
     C*
     C                   ENDSR                                                  PGMSTR
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
     C                   MOVEL     Q1USER        MCUSER
     C                   MOVEL     Q1PGM         MCPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MCDT
     C*
     C                   MOVEL     DSTIME        MCTIME
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
     C                   MOVEL     Q1USER        MCUSER
     C                   MOVEL     Q1USER        MCIUSR
     C                   MOVEL     Q1PGM         MCPGM
     C                   MOVEL     Q1PGM         MCIPGM
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MCDT
     C                   MOVEL     SRDTO         MCIDT
     C                   MOVEL     DSTIME        MCTIME
     C                   MOVEL     DSTIME        MCITIM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FMMCP
     C*
     C                   ENDSR
