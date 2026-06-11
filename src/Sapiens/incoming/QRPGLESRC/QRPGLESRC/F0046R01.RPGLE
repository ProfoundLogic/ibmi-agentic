     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F0046R01 - GROUP T/M RATES MASTER INFORMATION C:62 04/10/98  *
     H*                                                               *
     H*  CALLED FROM: F0046R                                          *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF0046F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMSRP     UF A E             DISK
     F                                     RENAME(PSR01:DBRCDF)
     FFMSRL     IF   E           K DISK
     FFMSMP     IF   E             DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Subscriber T/M Rates')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FMSRP
     C                   PARM                    @@RRN2            9 0          FMSMP
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
     C     SRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKEFDT
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    SMCO#         KKCO#
     C     *LIKE         DEFINE    SMSUB#        KKSUB#
     C     *LIKE         DEFINE    SREFDT        KKEFDT
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
     C* EFFECTIVE DATE MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* DUPLICATE RECORDS MUST NOT EXIST.
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C     SRKY          CHAIN     PSR01                              99
     C*
     C  N99              SETON                                        7290
     C*
     C                   ENDIF
     C*
     C* PROFESSIONAL RATE IS REQUIRED.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCPRT1        ANDEQ     *ZEROS
     C*
     C                   SETON                                        7390
     C*
     C                   ENDIF
     C*
     C* NON-PROFESSIONAL RATE IS REQUIRED.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCNRT1        ANDEQ     *ZEROS
     C*
     C                   SETON                                        7490
     C*
     C                   ENDIF
     C*
     C* MILEAGEL RATE IS REQUIRED.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMRT1        ANDEQ     *ZEROS
     C*
     C                   SETON                                        7590
     C*
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
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   Z-ADD     SREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   Z-ADD     SRPRT1        SCPRT1
     C                   Z-ADD     SRNRT1        SCNRT1
     C                   Z-ADD     SRMRT1        SCMRT1
     C*
     C                   MOVEL     SRSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        SCEFDT
     C                   Z-ADD     *ZEROS        SCPRT1
     C                   Z-ADD     *ZEROS        SCNRT1
     C                   Z-ADD     *ZEROS        SCMRT1
     C*
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
     C                   Z-ADD     SMCO#         SRCO#
     C                   Z-ADD     SMSUB#        SRSUB#
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SREFDT
     C*
     C                   Z-ADD     SCPRT1        SRPRT1
     C                   Z-ADD     SCNRT1        SRNRT1
     C                   Z-ADD     SCMRT1        SRMRT1
     C*
     C                   MOVEL     SCSTAT        SRSTAT
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
     C     @@MODE        COMP      'A'                                    40
     C     @@MODE        COMP      'D'                                    09
     C*
     C     @@RRN2        CHAIN     PSM01                              99
     C*
     C                   Z-ADD     SMSUB#        SCSUB#
     C                   MOVEL     SMNAME        SCSNAM
     C                   Z-ADD     SMCO#         KKCO#
     C                   Z-ADD     SMSUB#        KKSUB#
     C*
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
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        SRUSER
     C                   MOVEL     Q1PGM         SRPGM
     C                   MOVEL     DSTIME        SRTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SRDT
     C*
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
     C                   MOVEL     Q1USER        SRUSER
     C                   MOVEL     Q1USER        SRIUSR
     C                   MOVEL     Q1PGM         SRPGM
     C                   MOVEL     Q1PGM         SRIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SRDT
     C*
     C                   MOVEL     SRDTO         SRIDT
     C*
     C                   MOVEL     DSTIME        SRTIME
     C                   MOVEL     DSTIME        SRITIM
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      FMSRP
     C*
     C                   MOVEL     *ON           OUTFLG
     C*
     C                   ENDSR
