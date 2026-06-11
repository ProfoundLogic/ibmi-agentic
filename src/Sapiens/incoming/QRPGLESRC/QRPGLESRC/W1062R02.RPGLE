     H* (C) Copyright Progressive Data Solutions, Inc., 2002
     H*
     H* W1062R02 - Group Rate Set Code Maintenance
     H*****************************************************************
     H*                                                               *
     H* CALLED FROM: W1062R   - WW Group Rate Set Code                *
     H*                                                               *
     H* CALLS TO: W2471R - Search Rate Set Code                       *
     H*                                                               *
     H* Created  03/05/02  C:61  Project: 114WCY                      *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  08/24/06 C:180 PROJECT: 114WMF  - New pgm                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
      *
      *
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW1062F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
      *  Physical file definition (for maintaining record)
     FWMGRP     UF A E             DISK
     F                                     RENAME(PGR01:DBRCDF)
      *
      * Group Rate Set Code
     FWMGRL     IF   E           K DISK
     F                                     INFDS(GRFDS)
      *
      * Rate Set Code Table
     FWMRSL     IF   E           K DISK
      *
      * Group Master
     FWMFNP     IF   E             DISK
      *
      *
      *
     D GRFDS           DS
     D  GRRRN                397    400B 0
      *
      *
      *
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('Group Rate Set-
     D                                     Code Information')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
     C/COPY SCOPYMT01C
      *
      *
      *
      *================================================================
      *  PLIST definitions
      *================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          WMGRP
     C                   PARM                    @@RRN1            9 0          WMFNP
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================
      *
      *  Rate set code
      *
     C     RSKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    SCRTCD
      *
      *  Group Rate Set Code
      *
     C     GRKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C                   KFLD                    SCRTCD
      *
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
      *
      *
      *
      *
      *
      *
      *================================================================
      * CHGSTS - SUBROUTINE FOR CHANGING STATUS
      *================================================================
      *
     C     CHGSTS        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C     *IN99         IFEQ      *OFF
      *
     C     GRSTAT        IFEQ      'I'
     C                   MOVE      'A'           GRSTAT
     C                   ELSE
     C                   MOVE      'I'           GRSTAT
     C                   ENDIF                                                  GRSTAT
      *
     C                   MOVEL     Q1USER        GRUSER
     C                   MOVEL     Q1PGM         GRPGM
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GRDT
      *
     C                   MOVEL     DSTIME        GRTIME
      *
     C                   UPDATE    DBRCDF
      *
     C                   ENDIF                                                  *IN99
      *
     C                   ENDSR                                                  CHGSTS
      *
      *
      *
      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
      *
      *
      *
      *================================================================
      * DSPLYR - Processing after displaying information
      *================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
      *
      *
      *
      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR
      *
      * Group Rate Set Code must not be Blank
      *
     C  N90SCRTCD        IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCRTCD
      *
      * Group Rate Set Code must be valid and have active status.
      *
     C     *IN90         IFEQ      *OFF
     C     RSKY          CHAIN     PRS01                              60
      *
     C     *IN60         IFEQ      *ON
     C     RSSTAT        OREQ      'I'
     C                   SETON                                        72  90
     C                   ENDIF                                                  *IN60
      *
     C                   ENDIF                                                  SCRTCD
      *
      * Group Rate Set Code must be unique for Group
      *    co#/fnd/rtcd
      *
     C     *IN90         IFEQ      *OFF
      *
     C     @@MODE        IFEQ      'A'
     C     GRKY          CHAIN     PGR01                              60
     C  N60              SETON                                        90  73
     C                   ENDIF                                                  @@MODE = A
      *
     C     @@MODE        IFEQ      'C'
     C     GRKY          CHAIN     PGR01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     GRRRN         IFNE      @@RRN
     C                   SETON                                        90  73
     C                   LEAVE
     C                   ENDIF
      *
     C     GRKY          READE     PGR01                                  60
     C                   ENDDO
      *
     C                   ENDIF                                                  @@MODE = C
      *
     C                   ENDIF                                                  *IN90 = *OFF
      *
      * CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
      *
     C  N90GRDT          IFGT      SAVEDT
     C     GRDT          OREQ      SAVEDT
     C     GRTIME        ANDGT     SAVETM
     C                   SETON                                        9074
     C                   ENDIF
      *
     C                   ENDSR                                                  ERRCK1
      *
      *
      *
      *================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR02)
      *================================================================
     C     ERRCK2        BEGSR
      *
      * WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   ENDSR                                                  ERRCK2
      *
      *
      *
      *================================================================
      * ERRCK3 - Error checking for srceen 3 (SCR03)
      *================================================================
     C     ERRCK3        BEGSR
      *
      * WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   ENDSR                                                  ERRCK3
      *
      *
      *
      *================================================================
      * KJDO - F10=SEARCH SUBSET SCREEN.
      *================================================================
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCRTCD'
      *
     C                   CALL      'W2471R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      *BLANKS       ##RTCD            3
      *
     C     ##RTCD        IFNE      *BLANKS
     C                   MOVEL(P)  ##RTCD        SCRTCD
     C     RSKY          CHAIN     PRS01                              60
     C  N60              MOVEL(P)  RSDESC        SCDESC
     C   60              MOVEL(P)  *BLANKS       SCDESC
     C                   ENDIF
      *
     C                   ENDSL
      *
     C                   SETON                                        90
      *
     C                   ENDSR                                                  KJDO
      *
      *
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     GRRTCD        SCRTCD
     C     RSKY          CHAIN     PRS01                              60
     C  N60              MOVEL(P)  RSDESC        SCDESC
     C   60              MOVEL(P)  *BLANKS       SCDESC
     C                   MOVEL     GRSTAT        SCSTAT
     C   40              MOVE      'A'           SCSTAT
      *
     C                   ENDSR                                                  MVDBSC
      *
      *
      *
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR
      *
     C                   Z-ADD     FNCO#         GRCO#
     C                   Z-ADD     FNFND         GRFND
     C                   MOVEL(P)  SCRTCD        GRRTCD
     C                   MOVE      SCSTAT        GRSTAT
      *
     C                   ENDSR                                                  MVSCDB
      *
      *
      *
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     OTHERR        BEGSR
     C                   ENDSR                                                  OTHERR
      *
      *
      *
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     OTHFKS        BEGSR
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHFKS
      *
      *
      *
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
      *
     C                   ENDSR                                                  PGMEND
      *
      *
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
      * Get Group Info
      *
     C     @@RRN1        CHAIN     PFN01                              60
     C   60              SETON                                        LR
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL(P)  FNNAME        SCNAME
      *
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR
      *
     C                   SELECT
      *
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  PREOTH
      *
      *
      *
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     UPDATR        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C     *IN99         IFEQ      *OFF
      *
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        GRUSER
     C                   MOVEL     Q1PGM         GRPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GRDT
      *
     C                   MOVEL     DSTIME        GRTIME
      *
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
      *
     C                   ENDSR                                                  UPDATR
      *
      *
      *
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
      *
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        GRUSER
     C                   MOVEL     Q1USER        GRIUSR
     C                   MOVEL     Q1PGM         GRPGM
     C                   MOVEL     Q1PGM         GRIPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GRDT
     C                   MOVEL     SRDTO         GRIDT
      *
     C                   Z-ADD     DSTIME        GRTIME
     C                   Z-ADD     DSTIME        GRITIM
      *
     C                   WRITE     DBRCDF
     C                   FEOD      WMGRP
      *
     C                   ENDSR                                                  WRITER
      *
      *
      *
