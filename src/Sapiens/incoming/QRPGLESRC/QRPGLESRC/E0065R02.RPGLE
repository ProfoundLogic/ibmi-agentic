     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE E0065R02  Ballast Values Update/Display
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* MAINTENANCE PROGRAM FOR WEIGHTING VALUES                      *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CREATED: 08/09/01                                             *
     H*                                                               *
     H* 08/09/01 C:136 PROJECT WC577                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  71       SSEFDT - INVALID STATE                              *
     H*  72       SSEFDT - INVALID DATE                               *
     H*  73       SCLOSS CANNOT BE LESS THAN ZERO                     *
     H*  74       SCWEIG CANNOT BE LESS THAN ZERO                     *
     H*  75       SCST CANNOT BE BLANK                                *
     H*  76       SCEFDT CANNOT BE BLANK                              *
     H*                                                               *
     H*****************************************************************
     FE0065F02  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FEMWEP     UF A E             DISK
     F                                     RENAME(PWE01:DBRCDF)
      *
      *  State table file
     FSTABL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Weighting Value Main-
     D                                     tenance')
     D COMAPI          C                   CONST('SA005R')
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
     C                   PARM                    ##CONM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
      *
     C     STKY          KLIST
     C                   KFLD                    KKST             10
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C     WESTAT        IFEQ      'I'
     C                   MOVE      'A'           WESTAT
     C                   ELSE
     C                   MOVE      'I'           WESTAT
     C                   ENDIF                                                  WESTAT
     C*
     C                   MOVEL     Q1USER        WEUSER
     C                   MOVEL     Q1PGM         WEPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WEDT
     C*
     C                   MOVEL     DSTIME        WETIME
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
      *
     C     *IN90         IFEQ      *OFF
     C     SCST          ANDNE     *BLANK
     C                   MOVEL     SCST          KKST
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        71  90
     C                   ELSE
     C                   SETON                                        75  90
     C                   ENDIF
      *
     C     *IN90         IFEQ      *OFF
     C     SCEFDT        ANDNE     *ZEROS
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
      *
     C     SRERR         IFEQ      '1'
     C                   SETON                                        72  90
     C                   ENDIF                                                  IF SRERR = 1
     C                   ELSE
     C                   SETON                                        76  90
     C                   ENDIF                                                  IF 90 OFF
      *
     C     *IN90         IFEQ      *OFF
     C     SCLOSS        ANDLT     0
     C                   SETON                                        73  90
     C                   ENDIF
      *
     C     *IN90         IFEQ      *OFF
     C     SCWEIG        ANDLT     0
     C                   SETON                                        74  90
     C                   ENDIF
      *
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
      *
     C                   MOVE      WEST          SCST
      *
     C                   Z-ADD     WEEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
      *
     C                   Z-ADD     WELOSS        SCLOSS
     C                   Z-ADD     WEWEIG        SCWEIG
     C                   MOVE      WESTAT        SCSTAT
      *
     C     *IN40         IFEQ      *ON
      *
     C     SVST          IFNE      *BLANKS
     C                   SETON                                        80
     C                   ENDIF
      *
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      SVST          SCST
     C                   Z-ADD     SVEFDT        SCEFDT
     C                   ENDIF
      *
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
      *
     C                   MOVE      ##CO#         WECO#
     C                   MOVE      SCST          WEST
     C                   MOVE      SCST          SVST              2
      *
     C                   Z-ADD     SCEFDT        SVEFDT            8 0
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WEEFDT
      *
     C                   Z-ADD     SCLOSS        WELOSS
     C                   Z-ADD     SCWEIG        WEWEIG
     C                   MOVE      SCSTAT        WESTAT
      *
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
      *
     C     @@MODE        COMP      'A'                                    40
      *
     C                   ENDSR
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
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        WEUSER
     C                   MOVEL     Q1PGM         WEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WEDT
     C                   MOVEL     DSTIME        WETIME
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        WEUSER
     C                   MOVEL     Q1USER        WEIUSR
     C                   MOVEL     Q1PGM         WEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WEDT
     C                   MOVEL     SRDTO         WEIDT
     C                   MOVEL     DSTIME        WETIME
     C                   WRITE     DBRCDF
     C                   FEOD      EMWEP
     C                   ENDSR
