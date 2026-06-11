     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*  AUTO CMS LETTERS
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: W0973R02                                          *
     H*    CREATED: 02/02/11  C:235 PROJ: 114WXO                      *
     H*                                                               *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FW0973F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWAMLP     UF A E             DISK
     F                                     RENAME(PML01:DBRCDF)
     FWMDOL     IF   E           K DISK
     FWMFNP     IF   E             DISK
     FWMDEL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('AUTO CMS LETTERS')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
     D PTL             C                   CONST('DEPARTMENT/DOC ID SE-
     D                                     ARCH')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0          FUND
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
     C     DOKY          KLIST
     C                   KFLD                    MLCO#
     C                   KFLD                    KKDEPT
     C                   KFLD                    MLDPCD
     C                   KFLD                    MLFLDR
     C                   KFLD                    MLDCID
     C*
     C     DOKEY         KLIST
     C                   KFLD                    MLCO#
     C                   KFLD                    KKDEPT
     C                   KFLD                    SCDPCD
     C                   KFLD                    SCFLDR
     C                   KFLD                    SCDCID
     C*
     C*  DEPT CODE TABLE FILE
     C*
     C     DEKEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKDEPT
     C                   KFLD                    SCDPCD
      *
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    DODEPT        KKDEPT
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
     C*** DEPT CODE
     C*
     C     SCDPCD        IFEQ      *BLANKS
     C                   SETON                                        707390
     C                   GOTO      ENDCK1
     C                   ENDIF
     C*
     C*  THE DEPT CODE MUST BE VALID
     C*
     C     DEKEY         CHAIN     PDE01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        7390
     C                   GOTO      ENDCK1
     C                   END
     C*
     C*** FOLDER
     C*
     C     SCFLDR        IFEQ      *BLANKS
     C                   SETON                                        707590
     C                   GOTO      ENDCK1
     C                   ENDIF
     C*
     C                   MOVE      ' '           QQRTCD            1
     C                   CALL      'SVALFLR'
     C                   PARM                    SCFLDR
     C                   PARM                    QQRTCD
     C*
     C*  FOLDER MUST EXIST
     C*
     C     QQRTCD        IFEQ      '1'
     C                   SETON                                        7590
     C                   GOTO      ENDCK1
     C                   ENDIF                                                  END EQ '1'
     C*
     C*  USER MUST HAVE AUTHORITY TO THE FOLDER
     C*
     C     QQRTCD        IFEQ      '2'
     C                   SETON                                        8090
     C                   GOTO      ENDCK1
     C                   ENDIF                                                  END EQ '2'
     C*
     C*** DOCUMENT ID
     C*
     C     SCDCID        IFEQ      *BLANKS
     C                   SETON                                        707490
     C                   GOTO      ENDCK1
     C                   ENDIF
     C*
     C*  THE DOCUMENT ID MUST BE VALID
     C*
     C                   CALL      'SVALDOC'
     C                   PARM                    SCDCID
     C                   PARM                    SCFLDR
     C                   PARM      *BLANKS       QQRTCD
     C*
     C     QQRTCD        IFEQ      '1'
     C                   SETON                                        7490
     C                   GOTO      ENDCK1
     C                   ENDIF
     C*
     C*** # OF COPIES
     C*
     C     SC#CPY        IFEQ      *ZEROS
     C                   SETON                                        707690
     C                   GOTO      ENDCK1
     C                   ENDIF
     C*
     C*** OUTQ
     C*
     C     SCOUTQ        IFEQ      *BLANKS
     C                   SETON                                        707790
     C                   GOTO      ENDCK1
     C                   ENDIF
     C*
     C*
     C*  DOCUMENT OUTPUT QUEUE MUST BE VALID ON SYSTEM.
     C*
     C                   CALL      'S0003C'
     C                   PARM                    SCOUTQ
     C                   PARM      ' '           XFOUND            1
     C     XFOUND        IFEQ      '1'
     C                   SETON                                        7790
     C                   GOTO      ENDCK1
     C                   END
     C*
     C     ENDCK1        ENDSR
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
     C*
     C     MLFND         IFNE      *ZEROS
     C                   MOVE      MLFND         SCFND
     C                   ELSE
     C                   MOVE      FNFND         SCFND
     C                   ENDIF
     C                   MOVE      MLEMTH        SCEMTH
     C                   MOVE      MLRMTH        SCRMTH
     C                   MOVE      MLDPCD        SCDPCD
     C                   MOVEL     MLDCID        SCDCID
     C                   MOVEL     MLFLDR        SCFLDR
     C     DOKY          CHAIN     PDO01                              99
     C  N99              MOVEL     DODESC        SCDESC
     C                   MOVE      ML#CPY        SC#CPY
     C                   MOVE      MLOUTQ        SCOUTQ
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVE      ##CO#         MLCO#
     C                   MOVE      SCFND         MLFND
     C                   MOVE      SCEMTH        MLEMTH
     C                   MOVE      SCRMTH        MLRMTH
     C                   MOVE      SCDPCD        MLDPCD
     C                   MOVEL     SCDCID        MLDCID
     C                   MOVEL     SCFLDR        MLFLDR
     C                   MOVE      SC#CPY        ML#CPY
     C                   MOVE      SCOUTQ        MLOUTQ
     C*
     C                   MOVEL     Q1USER        MLUSER
     C                   MOVEL     Q1PGM         MLPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MLDT
     C                   MOVEL     DSTIME        MLTIME
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
     C* F10 = SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCDPCD'
     C     HFLD          OREQ      'SCDCID'
     C     HFLD          OREQ      'SCFLDR'
     C*
     C                   MOVEL(P)  PTL           QQPRDS
     C                   MOVE      *BLANKS       QQCALL
     C                   CALL      'W2321R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    KKDEPT
     C                   PARM                    SCDPCD
     C                   PARM                    SCFLDR
     C                   PARM                    SCDCID
     C                   PARM                    QQRTCD            1
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCDPCD'
     C                   Z-ADD     32            IN                2 0
     C     HFLD          WHENEQ    'SCDCID'
     C                   Z-ADD     33            IN
     C     HFLD          WHENEQ    'SCFLDR'
     C                   Z-ADD     34            IN
     C                   ENDSL
     C*
     C                   SELECT
     C     QQRTCD        WHENEQ    'A'
     C                   EXSR      PGMEND
     C                   RETURN
     C*
     C     QQRTCD        WHENEQ    '*'
     C*
     C     DOKEY         CHAIN     PDO01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     DODESC        SCDESC
     C                   ENDIF
     C                   ADD       1             IN
     C*
     C                   OTHER
     C*
     C                   ENDSL
     C*
     C                   MOVE      *ON           *IN(IN)
     C*
     C                   OTHER
     C                   ENDSL
     C                   ENDIF
     C*
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
     C                   MOVE      'CS'          KKDEPT
     C     @@RRN2        CHAIN     WMFNP                              60
     C                   MOVE      FNFND         SCFND
     C                   MOVEL(P)  FNNAME        SCFNAM
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
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        MLIUSR
     C                   MOVEL     Q1PGM         MLIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MLIDT
     C                   MOVEL     DSTIME        MLITM
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      WAMLP
     C                   ENDSR
