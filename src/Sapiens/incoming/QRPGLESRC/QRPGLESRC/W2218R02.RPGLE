     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H* REVISIONS
     H*
     H* 12/13/99  C:118  SUPPORT: WC048/20956
     H*                  Add mode not updating database or screen.
     H*                  Change mode not updating.
     H*                  User information not updating.
     H*                  Added change status code.
     H*                  Made SCCODE, Desc and coverage required on
     H*                    display screen.
     H*                  Error checking added.
     H*
     H*****************************************************************
     FW2218F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWTECP     UF A E             DISK
     F                                     RENAME(PEC01:DBRCDF)
     F***** BEGIN ADD **** 12/13/99 ***********************************
     FWTECL     IF   E           K DISK
     F***** END   ADD **** 12/13/99 ***********************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Coverage Codes')
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
     C***** BEGIN ADD **** 12/13/99 ***********************************
     C* KEY LIST FOR COVERAGE CODE
     C*
     C     ECKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXKEY
     C***** END   ADD **** 12/13/99 ***********************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C***** BEGIN ADD **** 12/13/99 ***********************************
     C     *LIKE         DEFINE    ECKEY         XXKEY
     C***** END   ADD **** 12/13/99 ***********************************
     C*
     C/COPY SCOPYMT01C
     C*
     C***** BEGIN ADD **** 12/13/99 ***********************************
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C     *IN60         IFEQ      *OFF
     C*
     C     ECSTAT        IFEQ      'I'
     C                   MOVE      'A'           ECSTAT
     C                   ELSE
     C                   MOVE      'I'           ECSTAT
     C                   ENDIF                                                  ECSTAT=I
     C*
     C                   MOVEL     Q1USER        ECUSER
     C                   MOVEL     Q1PGM         ECPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ECDT
     C*
     C                   MOVEL     DSTIME        ECTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDSR                                                  CHGSTS
     C***** END   ADD **** 12/13/99 ***********************************
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
     C***** BEGIN ADD **** 12/13/99 ***********************************
     C* COVERAGE CODE MUST BE ENTERED IN ADD MODE
     C*
     C   40
     CANN90SCCODE        IFEQ      *BLANKS
     C                   SETON                                        21  90
     C                   ENDIF                                                  SCCODE=BLKS
     C*
     C* CHECK TO BE SURE COVERAGE CODE HAS NOT BEEN USED BEFORE
     C*
     C                   MOVEL     SCCODE        XXKEY
     C*
     C   40
     CANN90ECKY          CHAIN     PEC01                              60
     C   40
     CANN90
     CANN60              SETON                                        22  90
     C*
     C* DESCRIPTION CAN NOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        23  90
     C                   ENDIF                                                  SCDESC=BLKS
     C*
     C* INTEGRATED COVERAGE CODE MUST BE Y/N
     C*
     C  N90SC24HR        IFNE      'Y'
     C     SC24HR        ANDNE     'N'
     C                   SETON                                        24  90
     C                   ENDIF                                                  SC24HR<>Y
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90ECDT          IFGT      SAVEDT
     C     ECDT          OREQ      SAVEDT
     C     ECTIME        ANDGT     SAVETM
     C                   SETON                                          9093
     C                   ENDIF                                                  CNDT>SAVEDT
     C*
     C***** END   ADD **** 12/13/99 ***********************************
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
     C*
     C***** BEGIN ADD **** 12/13/99 ***********************************
     C   40              MOVEL     'A'           ECSTAT
     C***** END   ADD **** 12/13/99 ***********************************
     C*
     C                   MOVEL(P)  ECKEY         SCCODE
     C                   MOVEL(P)  ECDESC        SCDESC
     C                   MOVEL     EC24HR        SC24HR
     C                   MOVEL     ECSTAT        SCSTAT
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C***** BEGIN ADD **** 12/13/99 ***********************************
     C                   Z-ADD     ##CO#         ECCO#
     C                   MOVEL(P)  SCCODE        ECKEY
     C                   MOVEL(P)  SCDESC        ECDESC
     C                   MOVEL     SC24HR        EC24HR
     C                   MOVEL     SCSTAT        ECSTAT
     C***** END   ADD **** 12/13/99 ***********************************
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
     C*
     C***** BEGIN ADD **** 12/13/99 ***********************************
     C     @@MODE        COMP      'A'                                    40
     C***** END   ADD **** 12/13/99 ***********************************
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C***** BEGIN ADD **** 12/13/99 ***********************************
     C     @@MODE        IFEQ      'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDIF                                                  @@MODE=S
     C***** END   ADD **** 12/13/99 ***********************************
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C***** BEGIN ADD **** 12/13/99 ***********************************
     C*
     C                   MOVEL     Q1USER        ECUSER
     C                   MOVEL     Q1PGM         ECPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ECDT
     C                   MOVEL     DSTIME        ECTIME
     C*
     C***** END   ADD **** 12/13/99 ***********************************
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C***** BEGIN ADD **** 12/13/99 ***********************************
     C*
     C                   MOVEL     Q1USER        ECIUSR
     C                   MOVEL     Q1USER        ECUSER
     C                   MOVEL     Q1PGM         ECIPGM
     C                   MOVEL     Q1PGM         ECPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ECIDT
     C                   MOVEL     SRDTO         ECDT
     C                   MOVEL     DSTIME        ECITM
     C                   MOVEL     DSTIME        ECTIME
     C***** END   ADD **** 12/13/99 ***********************************
     C                   WRITE     DBRCDF
     C                   FEOD      WTECP
     C                   ENDSR
