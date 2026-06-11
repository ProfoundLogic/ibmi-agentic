     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*
     H/TITLE E0010R02 - MAINTEANCE PROGRAM FOR WORK WITH COMPANY AUXILIARY MAINT
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* MAINTENANCE PROGRAM FOR WORK WITH COMPANY AUXILIARY MAINT     *
     H*                                                               *
     H* CALLED BY: E0010R                                             *
     H*                                                               *
     H* CALLS TO: NONE                                                *
     H*                                                               *
     H* CREATED: 06/08/2001                                           *
     H*                                                               *
     H* 06/08/01 C:136 PROJECT WC577                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H* 11/05/03 C:58  PROJECT WC1156                                 *
     H*            Add state field, validation, and search.           *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  40 INDICATE ADD MODE                                         *
     H*  30 - (WCR0504) The state code was not found in list.  SCR01  *
     H*  73 GROUP NUMBER MUST BE VALID                                *
     H*  74 GROUP NUMBER CANNOT BE BLANK                              *
     H*  75 EXCESS LOSS AMOUNT (SCEXLS) CANNOT BE LESS THAN ZERO      *
     H*  76 PRIMARY LOSS AMT (SCPRLS) CANNOT BE LESS THAN ZERO        *
     H*  77 MULTIPLES CLAIM FACTOR (SCMCF) CANNOT BE LESS THAN ZERO   *
     H*  78 MINIMUM PREMIUM LAST TWO YEARS (SCPR2Y) CANNOT BE < ZERO  *
     H*  79 MINIMUM PERMIUM AVERAGE (SCPRAV) CANNOT BE LESS THAN ZERO *
     H*  80 RECORD ALREADY EXISTS                                     *
     H*  82 POSITION CURSOR TO SCEXLS AFTER FND SEARCH                *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*                                                               *
     H*****************************************************************
     FE0010F02  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FEMACP     UF A E             DISK
     F                                     RENAME(PAC01:DBRCDF)
     F*
     FEMACL1    IF   E           K DISK
     F*
     F* Fund Master File
     FEMFNL     IF   E           K DISK
     F*
     F*** BEG ADD *** 11/05/03 **********************************
     FSTSTL3    IF   E           K DISK
     F*** END ADD *** 11/05/03 **********************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Company Auxiliary Ma-
     D                                     intenance')
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
     C     ACKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C*
     C*** BEG ADD *** 11/05/03 **********************************
     C*  KEY LIST FOR THE SYSTEM-WIDE STATE CODE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY            10
     C*** END ADD *** 11/05/03 **********************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    ACFND         KKFND
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
     C* Group number must be valid for add mode
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C                   MOVEL     SCFND         KKFND
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        73  90
     C                   ENDIF
     C     SCFND         IFEQ      *ZEROS
     C                   SETON                                        74  90
     C                   ENDIF                                                  SCFFND = 0
     C                   ENDIF                                                  *IN90 AND *IN40
     C*
     C* Add mode permitted for only new company - groups
     C*
     C     @@MODE        IFEQ      'A'
     C     ACKY          CHAIN     PAC01                              60
     C  N60              SETON                                        80  90
     C                   ENDIF
     C*
     C* Excess Loss Amount cannot be less than Zero.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCEXLS        ANDLE     0
     C                   SETON                                        75  90
     C                   END
     C*
     C* Primary Loss Amount cannot be less than Zero.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCPRLS        ANDLE     0
     C                   SETON                                        76  90
     C                   END
     C*
     C* Multiple Claim Factor cannot be less than Zero.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMCF         ANDLE     0
     C                   SETON                                        77  90
     C                   END
     C*
     C* Minimum Premium Last Two Years cannot be less than Zero.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCPR2Y        ANDLE     0
     C                   SETON                                        78  90
     C                   END
     C*
     C* Minimum Premium Average cannot be less than Zero.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCPRAV        ANDLE     0
     C                   SETON                                        79  90
     C                   END
     C*
     C*** BEG ADD *** 11/05/03 **********************************
     C*  STATE MUST BE VALID.
     C     *IN90         IFEQ      *OFF
     C                   MOVEL     SCST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN30
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 11/05/03 **********************************
     C*
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
      *===============================================================
      * KJDO - F10 SEARCHES
      *===============================================================
     C     KJDO          BEGSR
      *
     C                   SELECT
      *  Group Master
     C     HFLD          WHENEQ    'SCFND'
      *
     C                   CALL      'E0015R'
     C                   PARM      '2'           QQCALL            1
     C                   PARM                    QQKC              1
     C                   PARM                    QQKL              1
     C                   PARM                    QQFND             3
      *
     C                   MOVEL     QQFND         SCFND                82
      *
     C                   MOVE      SCFND         KKFND
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SCFNNM
     C   60              MOVEL(P)  *BLANKS       SCFNNM
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  KJDO
      *
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   Z-ADD     ACFND         SCFND
     C                   Z-ADD     ACFND         KKFND
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  FNNAME        SCFNNM
     C                   ELSE
     C                   MOVE      *BLANKS       SCFNNM
     C                   ENDIF
     C                   MOVEL     ACEXLS        SCEXLS
     C                   MOVEL     ACPRLS        SCPRLS
     C                   MOVEL     ACMCF         SCMCF
     C                   MOVEL     ACPR2Y        SCPR2Y
     C                   MOVEL     ACPRAV        SCPRAV
     C                   MOVEL     ACCNAM        SCCNAM
     C*** BEG ADD *** 11/05/03 **********************************
     C                   MOVEL     ACST          SCST
     C*** END ADD *** 11/05/03 **********************************
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   Z-ADD     ##CO#         ACCO#
     C                   Z-ADD     SCFND         ACFND
     C                   MOVEL     SCEXLS        ACEXLS
     C                   MOVEL     SCPRLS        ACPRLS
     C                   MOVEL     SCMCF         ACMCF
     C                   MOVEL     SCPR2Y        ACPR2Y
     C                   MOVEL     SCPRAV        ACPRAV
     C                   MOVEL     SCCNAM        ACCNAM
     C*** BEG ADD *** 11/05/03 **********************************
     C                   MOVEL     SCST          ACST
     C*** END ADD *** 11/05/03 **********************************
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
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
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
     C     @@MODE        COMP      'A'                                    40
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
     C                   MOVEL     Q1USER        ACUSER
     C                   MOVEL     Q1PGM         ACPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACDT
     C                   MOVEL     DSTIME        ACTIME
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
     C                   MOVEL     Q1USER        ACUSER
     C                   MOVEL     Q1USER        ACIUSR
     C                   MOVEL     Q1PGM         ACPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ACDT
     C                   MOVEL     DSTIME        ACTIME
     C                   MOVEL     DSTIME        ACIDT
      *
     C                   WRITE     DBRCDF
     C                   FEOD      EMACP
     C                   ENDSR
