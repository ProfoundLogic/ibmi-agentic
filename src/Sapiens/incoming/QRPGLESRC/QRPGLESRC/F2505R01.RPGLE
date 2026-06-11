     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*      F2505R01 - EDI 148/A49 ERROR MESSAGE MAINTENANCE
     H*
     H*================================================================
     H*
     FF2505F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFTERP     UF A E             DISK
     F                                     RENAME(PER01:DBRCDF)
     F*
     F*  Logical file definition (for verifying record)
     FFTERL     IF   E           K DISK
     F*
     D***PY SCOPYMT01I
      /COPY SCOPYMT01D
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('EDI Error Code-
     D                                      Information')
     D COMAPI          C                   CONST('FA013R')
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
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     ERKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKMSG#
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    ERMSG#        KKMSG#
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
     C     ERSTAT        IFEQ      'I'
     C                   MOVE      'A'           ERSTAT
     C                   ELSE
     C                   MOVE      'I'           ERSTAT
     C                   ENDIF                                                  ERSTAT
     C*
     C                   MOVEL     Q1USER        ERUSER
     C                   MOVEL     Q1PGM         ERPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ERDT
     C*
     C                   MOVEL     DSTIME        ERTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
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
     C* MESSAGE ID CAN'T BE ZERO
     C*
     C   40*IN90         IFEQ      *OFF
     C     SCMSG#        ANDEQ     *ZEROS
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C* MESSAGE ID CAN'T BE DUPLICATE
     C*
     C   40*IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     SCMSG#        KKMSG#
     C*
     C     ERKY          CHAIN     PER01                              99
     C*
     C  N99              SETON                                        7290
     C*
     C                   ENDIF
     C*
     C* MESSAGE ID CAN'T BE ZERO
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDESC        ANDEQ     *BLANKS
     C*
     C                   SETON                                        7390
     C*
     C                   ENDIF
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90ERDT          IFGT      SAVEDT
     C     ERDT          OREQ      SAVEDT
     C     ERTIME        ANDGT     SAVETM
     C                   SETON                                        9078
     C                   ENDIF
     C*
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
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   Z-ADD     ERMSG#        SCMSG#
     C                   MOVEL     ERDESC        SCDESC
     C                   MOVEL     ERSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        SCMSG#
     C                   MOVEL     *BLANKS       SCDESC
     C                   MOVEL     'A'           SCSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         ERCO#
     C                   Z-ADD     SCMSG#        ERMSG#
     C                   MOVEL     SCDESC        ERDESC
     C                   MOVEL     SCSTAT        ERSTAT
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
     C*
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        ERUSER
     C                   MOVEL     Q1PGM         ERPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ERDT
     C*
     C                   MOVEL     DSTIME        ERTIME
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
     C                   MOVEL(P)  Q1USER        ERUSER
     C                   MOVEL(P)  Q1PGM         ERPGM
     C                   MOVEL(P)  Q1USER        ERIUSR
     C                   MOVEL(P)  Q1PGM         ERIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ERDT
     C                   MOVEL     SRDTO         ERIDT
     C*
     C                   MOVEL     DSTIME        ERTIME
     C                   MOVEL     DSTIME        ERITIM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FTERP
     C*
     C                   ENDSR
