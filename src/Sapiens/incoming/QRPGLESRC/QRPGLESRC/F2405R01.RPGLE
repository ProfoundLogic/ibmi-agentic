     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE F2405R01 - ZIP AREA INFORMATION
     H*
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               * *
     H*  08/08/02 C:62 PROJECT FS244                                  * *
     H*                EFFECTIVE DATE PROCESSING                      * *
     H*                                                               *
     H*  03/04/02 C:79 SUPPORT:MC048 31508                            *
     H*                LIFT EDIT ON DUPLICATE ZIP CODES               *
     H*                                                               *
     H*  05/05/00 C:62 PROJECT:FSMNT                                  *
     H*                CHANGE OPTNM.                                  *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF2405F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMGZP     UF A E             DISK
     F                                     RENAME(PGZ01:DBRCDF)
     F*** BEGIN CHG *** 08/08/02 **************************************
     F**********FMGZL1  IF  E           K        DISK
     FFMGZL3    IF   E           K DISK
     F*** END   CHG *** 08/08/02 **************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*** BEGIN CHG *** 05/05/00 **************************************
     D**********    'Zip Area Mainten-    C         OPTNM
     D**********    'ance'
     D OPTNM           C                   CONST('Zip Area Informa-
     D                                     tion')
     D*** END   CHG *** 05/05/00 **************************************
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
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     GZKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKZIPA
     C*** BEGIN ADD *** 08/08/02 **************************************
     C                   KFLD                    KKEFDT
     C*** END   ADD *** 08/08/02 **************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    GZZIPA        KKZIPA
     C*** BEGIN ADD *** 08/08/02 **************************************
     C     *LIKE         DEFINE    GZEFDT        KKEFDT
     C*** END   ADD *** 08/08/02 **************************************
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
     C     GZSTAT        IFEQ      'I'
     C                   MOVE      'A'           GZSTAT
     C                   ELSE
     C                   MOVE      'I'           GZSTAT
     C                   ENDIF                                                  GZSTAT
     C*
     C                   MOVEL     Q1USER        GZUSER
     C                   MOVEL     Q1PGM         GZPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GZDT
     C*
     C                   MOVEL     DSTIME        GZTIME
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
     C* ZIP AREA MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCZIPA        IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCZIPA
     C*** BEGIN ADD *** 08/08/02 **************************************
     C*
     C* EFFECTIVE DATE MUST BE VALID
     C*
     C     *IN40         IFEQ      *ON
     C     *IN90         ANDEQ     *OFF
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C     SRERR         IFEQ      *ON
     C                   SETON                                        7690
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 08/08/02 **************************************
     C*
     C* CHECK TO BE SURE ZIP AREA HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCZIPA        KKZIPA
     C*
     C   40
     CANN90GZKY          CHAIN     PGZ01                              99
     C*** BEG DEL *** 03/04/02 ****************************************
     C***40N90N99********  SETON                     72  90
     C*** END DEL *** 03/04/02 ****************************************
     C*
     C* ZIP AREA DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C* FROM ZIP MUST NOT BE BLANK
     C*
     C  N90SCFRZP        IFEQ      *BLANKS
     C                   SETON                                        74  90
     C                   ENDIF                                                  SCFRZP
     C*
     C* TO ZIP MUST NOT BE BLANK.
     C*
     C  N90SCTOZP        IFEQ      *BLANKS
     C                   SETON                                        75  90
     C                   ENDIF                                                  SCTOZP
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90GZDT          IFGT      SAVEDT
     C     GZDT          OREQ      SAVEDT
     C     GZTIME        ANDGT     SAVETM
     C                   SETON                                        9078
     C                   ENDIF
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
     C                   MOVEL     GZZIPA        SCZIPA
     C*** BEGIN ADD *** 08/08/02 **************************************
     C                   Z-ADD     GZEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*** END   ADD *** 08/08/02 **************************************
     C                   MOVEL     GZDESC        SCDESC
     C                   MOVEL     GZFRZP        SCFRZP
     C                   MOVEL     GZTOZP        SCTOZP
     C                   MOVEL     GZSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCZIPA
     C*** BEGIN ADD *** 08/08/02 **************************************
     C                   Z-ADD     *ZEROS        SCEFDT
     C*** END   ADD *** 08/08/02 **************************************
     C                   MOVEL     *BLANKS       SCDESC
     C                   MOVEL     *BLANKS       SCFRZP
     C                   MOVEL     *BLANKS       SCTOZP
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
     C                   MOVE      ##CO#         GZCO#
     C                   MOVEL     SCZIPA        GZZIPA
     C*** BEGIN ADD *** 08/08/02 **************************************
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GZEFDT
     C*** END   ADD *** 08/08/02 **************************************
     C                   MOVEL     SCDESC        GZDESC
     C                   MOVEL     SCFRZP        GZFRZP
     C                   MOVEL     SCTOZP        GZTOZP
     C                   MOVEL     SCSTAT        GZSTAT
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
     C                   MOVE      *ON           *INLR
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
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
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
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        GZUSER
     C                   MOVEL     Q1PGM         GZPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GZDT
     C*
     C                   MOVEL     DSTIME        GZTIME
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
     C                   MOVEL     Q1USER        GZUSER
     C                   MOVEL     Q1PGM         GZPGM
     C                   MOVEL     Q1USER        GZIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GZDT
     C*
     C                   MOVEL     SRDTO         GZIDT
     C*
     C                   MOVEL     DSTIME        GZTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FMGZP
     C*
     C                   ENDSR
