     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE E0025R - EXPECTED LOSS & DISCOUNT RATIO UPDATE/DISPLAY
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* MAINTENANCE PROGRAM FOR EXPECTED LOSS & DISCOUNT RATIO        *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CREATED: 06/14/2001                                           *
     H*                                                               *
     H* 06/14/01 C:136 PROJECT WC577                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H* 06/19/24 C:291 Project: WC2952                                *
     H*                Changed SCLsRt field in display file to 8,3.   *
     H*                and added Edit Mask.                           *
     H*                Recompile due to changes in display file.      *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  71       INVALID STATE(EMESGG/ECR0504)                       *
     H*  72       INVALID DATE(EMESGG/ECR0500)                        *
     H*  73       SCCLAS CANNOT BE LESS THAN 0(EMESGG/ECR0547)        *
     H*  74       SCLSRT CANNOT BE LESS THAN 0(EMESGG/ECR0547)        *
     H*  75       SCDSRT CANNOT BE LESS THAN 0(EMESGG/ECR0547)        *
     H*                                                               *
     H*****************************************************************
     FE0025F02  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FEMEXP     UF A E             DISK
     F                                     RENAME(PEX01:DBRCDF)
      *  State table file
     FSTABL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Expected Losses/Disc-
     D                                     ount Ratio')
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
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     EXSTAT        IFEQ      'I'
     C                   MOVE      'A'           EXSTAT
     C                   ELSE
     C                   MOVE      'I'           EXSTAT
     C                   ENDIF                                                  EXSTAT
     C*
     C                   MOVEL     Q1USER        EXUSER
     C                   MOVEL     Q1PGM         EXPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EXDT
     C*
     C                   MOVEL     DSTIME        EXTIME
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
     C                   SETON                                        77  90
     C                   ENDIF
      *                                                    IF 90 OFF
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
     C     SCCLAS        ANDLT     0
     C                   SETON                                        73  90
     C                   ENDIF
      *
     C     *IN90         IFEQ      *OFF
     C     SCLSRT        ANDLT     0
     C                   SETON                                        74  90
     C                   ENDIF
      *
     C     *IN90         IFEQ      *OFF
     C     SCDSRT        ANDLT     0
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
     C                   MOVE      EXST          SCST
     C                   Z-ADD     EXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   Z-ADD     EXCLAS        SCCLAS
     C                   Z-ADD     EXLSRT        SCLSRT
     C                   Z-ADD     EXDSRT        SCDSRT
     C                   MOVE      EXSTAT        SCSTAT
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
     C                   Z-ADD     ##CO#         EXCO#
     C                   MOVE      SCST          EXST
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EXEFDT
     C                   Z-ADD     SCCLAS        EXCLAS
     C                   Z-ADD     SCLSRT        EXLSRT
     C                   Z-ADD     SCDSRT        EXDSRT
     C                   MOVE      SCSTAT        EXSTAT
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
     C*
     C                   MOVE      *ON           *INLR
     C*
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
     C                   MOVEL     Q1USER        EXUSER
     C                   MOVEL     Q1PGM         EXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EXDT
     C                   MOVEL     DSTIME        EXTIME
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        EXUSER
     C                   MOVEL     Q1USER        EXIUSR
     C                   MOVEL     Q1PGM         EXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EXDT
     C                   MOVEL     SRDTO         EXIDT
     C                   MOVEL     DSTIME        EXTIME
     C                   WRITE     DBRCDF
     C                   FEOD      EMEXP
     C                   ENDSR
