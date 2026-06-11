     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H**********************************************************
     H* PROGRAM: F2172301
     H* DESCRIPTION: WORK PATTERN TABLE FILE MAINTENANCE
     H**********************************************************
     H* INDICATORS:
     H**********************************************************
     H* REVISIONS:
     H* 02/18/97 C:56  87MAZ
     H*                CREATED
     H**********************************************************
     FF2173F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FFTWPP     UF A E             DISK
     F                                     RENAME(PWP01:DBRCDF)
     FFTWPL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D OPTNM           C                   CONST('Work with Work Patt-
     D                                     ern')
     D COMAPI          C                   CONST('FA007R')

     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CO#             3 0
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     TBLKEY        KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*
     C     *LIKE         DEFINE    WPKEY         KKKEY
     C                   Z-ADD     1             #OFSCR            1 0
     C*
     C/COPY SCOPYMT01C
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
B01  C     SCKEY         IFEQ      *BLANKS
     C                   SETON                                        9089
E01  C                   ENDIF
B01  C     SCDESC        IFEQ      *BLANKS
     C                   SETON                                        9087
E01  C                   ENDIF
     C     *IN90         IFNE      *ON
     C     @@MODE        IFEQ      'A'
     C                   MOVEL     SCKEY         KKKEY
     C     TBLKEY        CHAIN     PWP01                              99
     C  N99              SETON                                        9086
     C                   ENDIF
     C                   ENDIF
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
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVEL     WPKEY         SCKEY
     C                   MOVEL     WPDESC        SCDESC
     C                   MOVEL     WPSTAT        SCSTAT
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   Z-ADD     @@CO#         WPCO#
     C                   MOVEL     SCKEY         WPKEY
     C                   MOVEL     SCDESC        WPDESC
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other Functions
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C     @@MODE        IFEQ      'S'
     C                   EXSR      UPDATR
     C                   RETURN
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C     @@MODE        IFNE      'S'
     C                   EXSR      MVSCDB
     C                   ELSE
     C     WPSTAT        IFEQ      'A'
     C                   MOVE      'I'           WPSTAT
     C                   ELSE
     C                   MOVE      'A'           WPSTAT
     C                   ENDIF
     C                   ENDIF
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        WPUSER
     C                   MOVEL     Q1PGM         WPPGM
     C                   MOVEL     DSTIME        WPTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WPDT
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL     'A'           WPSTAT
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        WPIUSR
     C                   MOVEL     Q1PGM         WPIPGM
     C                   MOVEL     DSTIME        WPITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WPIDT
     C                   WRITE     DBRCDF
     C                   ENDSR
