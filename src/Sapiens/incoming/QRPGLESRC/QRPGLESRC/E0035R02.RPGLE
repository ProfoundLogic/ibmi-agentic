     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE E0035R02  Manual Rate Update/Display
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* MAINTENANCE PROGRAM FOR MANUAL RATE MAINTENANCE               *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CREATED: 08/01/2001                                           *
     H*                                                               *
     H* 08/01/01 C:136 PROJECT WC577                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H* 06/19/24 C:291 Project: WC2952                                *
     H*                Changed SCRate field in display file to 8,3.   *
     H*                and added Edit Mask.                           *
     H*                Recompile due to changes in display file.      *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  71       SSEFDT - INVALID STATE                              *
     H*  72       SSEFDT - INVALID DATE                               *
     H*  73       SCLOSS CANNOT BE LESS THAN ZERO                     *
     H*  74       SCBALL CANNOT BE LESS THAN ZERO                     *
     H*  75       SCST CANNOT BE BLANK                                *
     H*  76       SCEFDT CANNOT BE BLANK                              *
     H*                                                               *
     H*****************************************************************
     FE0035F02  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FEMMRP     UF A E             DISK
     F                                     RENAME(PMR01:DBRCDF)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Manual Rate Maintena-
     D                                     nce')
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
     C     MRSTAT        IFEQ      'I'
     C                   MOVE      'A'           MRSTAT
     C                   ELSE
     C                   MOVE      'I'           MRSTAT
     C                   ENDIF                                                  MRSTAT
     C*
     C                   MOVEL     Q1USER        MRUSER
     C                   MOVEL     Q1PGM         MRPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MRDT
     C*
     C                   MOVEL     DSTIME        MRTIME
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
     C     SCDRSC        ANDEQ     *BLANK
     C                   SETON                                        71  90
     C                   ENDIF
      *
     C     *IN90         IFEQ      *OFF
     C     SCCLAS        ANDLT     0
     C                   SETON                                        72  90
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
     C                   SETON                                        73  90
     C                   ENDIF                                                  IF SRERR = 1
     C                   ELSE
     C                   SETON                                        74  90
     C                   ENDIF                                                  IF 90 OFF
      *
     C     *IN90         IFEQ      *OFF
     C     SCRATE        ANDLT     0
     C                   SETON                                        75  90
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
     C                   MOVE      MRDRSC        SCDRSC
     C                   Z-ADD     MRCLAS        SCCLAS
      *
     C                   Z-ADD     MREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
      *
     C                   Z-ADD     MRRATE        SCRATE
     C                   MOVE      MRSTAT        SCSTAT
      *
     C     *IN40         IFEQ      *ON
     C                   MOVE      'A'           SCSTAT
     C                   ENDIF
      *
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
      *
     C                   MOVE      ##CO#         MRCO#
     C                   MOVEL     SCDRSC        MRDRSC
     C                   Z-ADD     SCCLAS        MRCLAS
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MREFDT
      *
     C                   Z-ADD     SCRATE        MRRATE
     C                   MOVE      SCSTAT        MRSTAT
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
     C     @@MODE        COMP      'A'                                    40
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
      *
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
      *
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        MRUSER
     C                   MOVEL     Q1PGM         MRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MRDT
     C                   MOVEL     DSTIME        MRTIME
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        MRUSER
     C                   MOVEL     Q1USER        MRIUSR
     C                   MOVEL     Q1PGM         MRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MRDT
     C                   MOVEL     SRDTO         MRIDT
     C                   MOVEL     DSTIME        MRTIME
      *
     C                   WRITE     DBRCDF
     C                   FEOD      EMMRP
     C                   ENDSR
