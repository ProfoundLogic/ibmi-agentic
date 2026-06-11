     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H**********************************************************
     H* PROGRAM: F2172R01
     H* DESCRIPTION: LEVEL OF DUTY TABLE FILE MAINTENANCE
     H**********************************************************
     H* INDICATORS:
     H**********************************************************
     H* REVISIONS:
     H*
     H* 08/07/98 C:94  PROJECT: MCMNT
     H*                DEFAULTED STATUS TO 'A' IF STATUS WAS BLANK
     H*
     H* 02/07/97 C:53  87MAZ
     H*                CREATED
     H**********************************************************
     FF2172F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FFTDLP     UF A E             DISK
     F                                     RENAME(PDL01:DBRCDF)
     FFTDLL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D OPTNM           C                   CONST('Work with Level of -
     D                                     Duty')
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
     C     *LIKE         DEFINE    DLKEY         KKKEY
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
     C     TBLKEY        CHAIN     PDL01                              60
     C  N60              SETON                                        9086
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
     C                   MOVEL     DLKEY         SCKEY
     C                   MOVEL     DLDESC        SCDESC
     C*** BEG ADD *** 08/07/98 ***************************************
     C     DLSTAT        IFEQ      *BLANK
     C                   MOVEL     'A'           SCSTAT
     C                   ELSE
     C                   MOVEL     DLSTAT        SCSTAT
     C                   ENDIF
     C*** END ADD *** 08/07/98 ***************************************
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   Z-ADD     @@CO#         DLCO#
     C                   MOVEL     SCKEY         DLKEY
     C                   MOVEL     SCDESC        DLDESC
     C*** BEG ADD *** 08/07/98 **************************************
     C                   MOVEL     SCSTAT        DLSTAT
     C*** END ADD *** 08/07/98 **************************************
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions
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
     C     DLSTAT        IFEQ      'A'
     C                   MOVE      'I'           DLSTAT
     C                   ELSE
     C                   MOVE      'A'           DLSTAT
     C                   ENDIF
     C                   ENDIF
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        DLUSER
     C                   MOVEL     Q1PGM         DLPGM
     C                   MOVEL     DSTIME        DLTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DLDT
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
     C                   MOVEL     'A'           DLSTAT
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        DLIUSR
     C                   MOVEL     Q1PGM         DLIPGM
     C                   MOVEL     DSTIME        DLITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DLIDT
     C                   WRITE     DBRCDF
     C                   ENDSR
