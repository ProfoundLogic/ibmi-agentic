     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     FS0008F02  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     FSTABL     IF   E           K DISK
     FSMUPP     IF   E             DISK
     FSMALL     IF   E           K DISK
     F*  Physical file definition (for maintaining record)
     FSMALP     UF A E             DISK
     F                                     RENAME(PAL01:DBRCDF)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Adjuster Licenses')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@UPRR            9 0
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
     C     LICKEY        KLIST
     C                   KFLD                    SCUSR
     C                   KFLD                    SCST
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
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
     C*
     C     SCST          IFEQ      *BLANKS
     C                   SETON                                        7190
     C                   ELSE
     C*
     C*  Check for state license already exists
     C*
     C     SCST          IFNE      HHST
     C     LICKEY        CHAIN     SMALL                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C*  Check for valid state
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  SCST          STKEY
     C     STKEY         CHAIN     STABL                              90
     C     *IN90         IFEQ      *ON
     C                   SETON                                        74
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCLIC         ANDEQ     *BLANKS
     C                   SETON                                        7390
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
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVEL     ALST          HHST
     C                   MOVEL     ALST          SCST
     C                   MOVEL     ALLIC         SCLIC
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVEL     SCUSR         ALUSR
     C                   MOVEL     SCST          ALST
     C                   MOVEL     SCLIC         ALLIC
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
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   MOVEL     *BLANKS       SCUSR
     C                   MOVEL     *BLANKS       SCUSNM
     C     @@UPRR        CHAIN     SMUPP                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     UPUSR         SCUSR
     C                   MOVEL     UPUSNM        SCUSNM
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1PGM         ALPGM
     C                   MOVEL     Q1USER        ALUSER
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ALDT
     C                   MOVEL     DSTIME        ALTIME
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1PGM         ALIPGM
     C                   MOVEL     Q1USER        ALIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ALIDT
     C                   MOVEL     DSTIME        ALITIM
     C                   MOVEL     'A'           ALSTAT
     C                   WRITE     DBRCDF
     C                   FEOD      SMALP
     C                   ENDSR
