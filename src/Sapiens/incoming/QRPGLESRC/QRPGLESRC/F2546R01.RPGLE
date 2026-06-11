     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* Home Health Maintenance                                       *
     H*                                                               *
     H*****************************************************************
     H* Created on 04/04/2008 by c:62 for Project TX040               *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     FF2546F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMHHP     UF A E             DISK
     F                                     RENAME(PHH01:DBRCDF)
     FFMHHL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Home Health Procedur-
     D                                     e Information')
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
     C     HHKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKFAGE
     C                   KFLD                    KKTAGE
     C                   KFLD                    KKMOD1
     C                   KFLD                    KKMOD2
     C                   KFLD                    KKTOS
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    HHPRCD        KKPRCD
     C     *LIKE         DEFINE    HHEFDT        KKEFDT
     C     *LIKE         DEFINE    HHFAGE        KKFAGE
     C     *LIKE         DEFINE    HHTAGE        KKTAGE
     C     *LIKE         DEFINE    HHMOD1        KKMOD1
     C     *LIKE         DEFINE    HHMOD2        KKMOD2
     C     *LIKE         DEFINE    HHTOS         KKTOS
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
     C     HHKY          CHAIN     PHH01                              99
     C  N99              SETON                                        72  90
     C                   ENDIF                                                  *IN90
     C                   ENDIF                                                  *IN30
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90HHDT          IFGT      SAVEDT
     C     HHDT          OREQ      SAVEDT
     C     HHTIME        ANDGT     SAVETM
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
     C                   MOVEL(P)  HHPRCD        SCPRCD
     C                   Z-ADD     HHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   MOVEL(P)  HHDESC        SCDSC1
     C                   MOVE(P)   HHDESC        SCDSC2
     C                   Z-ADD     HHFAGE        SCFAGE
     C                   Z-ADD     HHTAGE        SCTAGE
     C                   Z-ADD     HHALOW        SCALOW
     C                   MOVEL(P)  HHMOD1        SCMOD1
     C                   MOVEL(P)  HHMD1D        SCMD1D
     C                   MOVEL(P)  HHMOD2        SCMOD2
     C                   MOVEL(P)  HHMD2D        SCMD2D
     C                   MOVEL(P)  HHTOS         SCTOS
     C                   MOVEL(P)  HHTOSD        SCTOSD
     C                   MOVEL(P)  HHRPT         SCRPT
     C                   MOVEL(P)  HHSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCPRCD
     C                   Z-ADD     *ZEROS        SCEFDT
     C                   MOVEL(P)  *BLANKS       SCDSC1
     C                   MOVEL(P)  *BLANKS       SCDSC2
     C                   Z-ADD     0             SCFAGE
     C                   Z-ADD     0             SCTAGE
     C                   Z-ADD     0             SCALOW
     C                   MOVEL(P)  *BLANKS       SCMOD1
     C                   MOVEL(P)  *BLANKS       SCMD1D
     C                   MOVEL(P)  *BLANKS       SCMOD2
     C                   MOVEL(P)  *BLANKS       SCMD2D
     C                   MOVEL(P)  *BLANKS       SCTOS
     C                   MOVEL(P)  *BLANKS       SCTOSD
     C                   MOVEL(P)  *BLANKS       SCRPT
     C                   MOVEL(P)  'A'           SCSTAT
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         HHCO#
     C                   MOVEL(P)  SCPRCD        HHPRCD
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHEFDT
     C                   MOVEL(P)  SCDSC1        HHDESC
     C                   MOVE      SCDSC2        HHDESC
     C                   Z-ADD     SCFAGE        HHFAGE
     C                   Z-ADD     SCTAGE        HHTAGE
     C                   Z-ADD     SCALOW        HHALOW
     C                   MOVEL(P)  SCMOD1        HHMOD1
     C                   MOVEL(P)  SCMD1D        HHMD1D
     C                   MOVEL(P)  SCMOD2        HHMOD2
     C                   MOVEL(P)  SCMD2D        HHMD2D
     C                   MOVEL(P)  SCTOS         HHTOS
     C                   MOVEL(P)  SCTOSD        HHTOSD
     C                   MOVEL(P)  SCRPT         HHRPT
     C                   MOVEL(P)  SCSTAT        HHSTAT
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
     C     @@MODE        COMP      'A'                                    40    ADD
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
     C                   MOVEL     Q1USER        HHUSER
     C                   MOVEL     Q1PGM         HHPGM
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
     C                   MOVEL     Q1USER        HHUSER
     C                   MOVEL     Q1USER        HHIUSR
     C                   MOVEL     Q1PGM         HHPGM
     C                   MOVEL     Q1PGM         HHIPGM
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C                   MOVEL     SRDTO         HHIDT
     C                   MOVEL     DSTIME        HHTIME
     C                   MOVEL     DSTIME        HHITIM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FMHHP
     C*
     C                   ENDSR
