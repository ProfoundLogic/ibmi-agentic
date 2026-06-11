     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE E0015R02 - MAINTEANCE PROGRAM FOR WORK WITH GROUPS
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* MAINTENANCE PROGRAM FOR WORK WITH GROUPS                      *
     H*                                                               *
     H* CALLED BY: E0015R                                             *
     H*                                                               *
     H* CALLS TO: NONE                                                *
     H*                                                               *
     H* CREATED: 06/08/2001                                           *
     H*                                                               *
     H* 06/08/01 C:136 PROJECT WC577                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  40 INDICATE ADD MODE                                         *
     H*  71 GROUP NUMBER MUST BE UNIQUE                               *
     H*  72 GROUP NUMBER CANNOT BE BLANK                              *
     H*  73 GROUP NAME CANNOT BE BLANK                                *
     H*  74 GROUP SHORT NAME CANNOT BE BLANK                          *
     H*  75 CITY NAME CANNOT BE BLANK                                 *
     H*  76 STATE MUST BE VALID                                       *
     H*  77 ZIP CODE CANNOT BE BLANK                                  *
     H*  78 RISK CODE CANNOT BE BLANK                                 *
     H*  79 EFFECTIVE DATE MUST BE VALID                              *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*                                                               *
     H*****************************************************************
     FE0015F02  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FEMFNP     UF A E             DISK
     F                                     RENAME(PFN01:DBRCDF)
     F*
     F*  Logical file for validating group and group name
     FEMFNL     IF   E           K DISK
     F*
     F*  Logical file for validating state
     FSTSTL3    IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Group Master Mainten-
     D                                     ance')
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
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    FNFND         KKFND
     C     *LIKE         DEFINE    STKEY         KKKEY
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
     C* Group number must be unique for add mode
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C                   MOVEL     SCFND         KKFND
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        71  90
     C                   ENDIF
     C     SCFND         IFEQ      *ZEROS
     C                   SETON                                        72  90
     C                   ENDIF
     C                   ENDIF
     C*
     C* Group name cannot be equal to blanks.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCNAME        ANDEQ     *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF
     C*
     C* Group short name cannot be equal to blanks.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSHNM        ANDEQ     *BLANKS
     C                   SETON                                        74  90
     C                   ENDIF
     C*
     C* City cannot be equal to blanks.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMCTY        ANDEQ     *BLANKS
     C                   SETON                                        75  90
     C                   ENDIF
     C*
     C* Validate state
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  SCMST         KKKEY
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        76  90
     C                   ENDIF
     C*
     C* Zipcode cannot be equal to blanks.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMZP1        ANDEQ     *ZEROS
     C                   SETON                                        77  90
     C                   ENDIF
     C*
     C* Risk Code cannot be equal to blanks.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDRSC        ANDEQ     *BLANKS
     C                   SETON                                        78  90
     C                   ENDIF
     C*
     C* Effective date must be valid.
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C                   ENDIF
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        79  90
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
     C                   MOVEL     FNFND         SCFND
     C                   MOVEL(P)  FNNAME        SCNAME
     C                   MOVEL     FNSHNM        SCSHNM
     C                   MOVEL(P)  FNMAD1        SCMAD1
     C                   MOVEL(P)  FNMAD2        SCMAD2
     C                   MOVEL(P)  FNMCTY        SCMCTY
     C                   MOVEL     FNMST         SCMST
     C                   MOVEL(P)  FNMZIP        SCMZP1
     C                   MOVE      FNMZIP        SCMZP2
     C                   MOVE      FNDRSC        SCDRSC
     C*
     C                   Z-ADD     FNEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         FNCO#
     C                   Z-ADD     SCFND         FNFND
     C                   MOVEL     SCNAME        FNNAME
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FNEFDT
     C*
     C                   MOVEL     SCSHNM        FNSHNM
     C                   MOVEL     SCMAD1        FNMAD1
     C                   MOVEL     SCMCTY        FNMCTY
     C                   MOVEL     SCMST         FNMST
     C                   MOVEL     SCMZP1        FNMZIP
     C                   MOVE      SCMZP2        FNMZIP
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FNDT
     C*
     C                   MOVE      SCDRSC        FNDRSC
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
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
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
     C                   MOVEL     Q1USER        FNIUSR
     C                   MOVEL     Q1USER        FNUSER
     C                   MOVEL     Q1PGM         FNPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FNDT
     C                   MOVEL     SRDTO         FNIDT
     C                   MOVEL     DSTIME        FNTIME
      *
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
     C                   MOVEL     Q1USER        FNIUSR
     C                   MOVEL     Q1USER        FNUSER
     C                   MOVEL     Q1PGM         FNPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FNDT
     C                   MOVEL     SRDTO         FNIDT
     C                   MOVEL     DSTIME        FNTIME
      *
     C                   WRITE     DBRCDF
     C                   FEOD      EMFNP
     C                   ENDSR
