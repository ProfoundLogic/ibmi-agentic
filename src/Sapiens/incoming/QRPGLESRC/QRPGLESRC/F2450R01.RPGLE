     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) ACTGRP(*CALLER)
     H/define CONVERT
     H*
     H/TITLE F2450R01 - MT Outcome/Savings Code
     H*****************************************************************
     H*                                                               *
     H*  CALLED BY: F2450R - WW Outcome/Savings Code                  *
     H*                                                               *
     H*  CALLS TO:  F2380R - WW Generic Table values - search         *
     H*                                                               *
     H*  CREATED:                                                     *
     H*                                                               *
     H*     05/04/98 C:61 PROJECT 98MAC                               *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/02/01 C:62 PROJECT MCMNT    BASE ONLY                     *
     H*                COMPILE ONLY.                                  *
     H*                                                               *
     H*  XX/XX/XX C:XX PROJECT XXXXX                                  *
     H*                XXX.......................................XXX  *
     H*                XXX.......................................XXX  *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09       Display mode                                        *
     H*  40       Add mode                                            *
     H*  41       Change mode                                         *
     H*  60       GENERAL READ/CHAIN                                  *
     H*                                                               *
     H* SCR01                                                         *
     H*  71       Outcome/Savings code not entered.                   *
     H*  72       Description must be entered.                        *
     H*  73       Rate value must be entered.                         *
     H*  74       Rate value must be valid.                           *
     H*  75       Duplicate records not allowed                       *
     H*  76 - 77  Position cursor                                     *
     H*  78       Record updated since data entry.                    *
     H*                                                               *
     H*****************************************************************
      *
      *
      *
      *
     FF2450F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
      *  Physical file definition (for maintaining record)
     FFTOTP     UF A E             DISK
     F                                     RENAME(POT01:DBRCDF)
      *
      *  Outcome/Savings File
     FFTOTL     IF   E           K DISK
      *
      *  Generic Table File
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
      *
     D***PY SCOPYMT01I
      *
      *================================================================
      *  /COPY required constants
      *================================================================
      /COPY SCOPYMT01D
     D OPTNM           C                   CONST('Outcome/Savings Code-
     D                                     s')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
      *
      *================================================================
      *  PLIST definitions
      *================================================================

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FTOTP
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *  Service code search
      *
     C     SRRTVL        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      *ZERO         ##RRN             9 0          FTGTP
     C                   PARM      'RTVL'        ##TBID
      *
      *================================================================
      *  KLIST definitions
      *================================================================
      *
      * Outcome/Savings Table File Key
      *
     C     OTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCODE
      *
      * Generic Table File Key
      *
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
      *
     C     *LIKE         DEFINE    OTCO#         KKCO#
     C     *LIKE         DEFINE    OTCODE        KKCODE
      *
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
      *
      *
     C/COPY SCOPYMT01C
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
     C     OTSTAT        IFEQ      'I'
     C                   MOVE      'A'           OTSTAT
     C                   ELSE
     C                   MOVE      'I'           OTSTAT
     C                   ENDIF                                                  OTSTAT
      *
     C                   MOVEL     Q1USER        OTUSER
     C                   MOVEL     Q1PGM         OTPGM
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OTDT
      *
     C                   MOVEL     DSTIME        OTTIME
      *
     C                   UPDATE    DBRCDF
      *
     C                   ENDIF                                                  *IN99
      *
     C                   ENDSR                                                  CHGSTS
      *
      *
      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
      *================================================================
      * DSPLYR - Processing after displaying information
      *================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
      *
      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR
      *
     C     @@MODE        IFEQ      'A'
      *
      * Outcome/Savings Code must be Entered
      *
     C     *IN90         IFEQ      '0'
     C     SCCODE        ANDEQ     *BLANKS
     C                   SETON                                        9071
     C                   ENDIF                                                  SCCODE
      *
      *  Duplicate records not allowed
      *
     C     *IN90         IFEQ      '0'
     C                   MOVEL(P)  SCCODE        KKCODE
     C     OTKY          CHAIN     POT01                              60
     C  N60              SETON                                        9075
     C                   ENDIF                                                  DUPLICATE REC
      *
     C                   ENDIF                                                  ADD MODE
      *
      * Outcome/Savings Description must be Entered
      *
     C     *IN90         IFEQ      '0'
     C     SCDESC        ANDEQ     *BLANKS
     C                   SETON                                        9072
     C                   ENDIF                                                  SCDESC
      *
      * Savings Rate Value must be entered
      *
     C     *IN90         IFEQ      '0'
     C     SCRTVL        ANDEQ     *BLANKS
     C                   SETON                                        9073
     C                   ENDIF                                                  SCRTVL ENTERED
      *
      * Savings Rate Value must be valid
      *
     C     *IN90         IFEQ      '0'
     C     SCRTVL        ANDNE     *BLANKS
     C                   MOVEL(P)  SCRTVL        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCRVDS
     C   60              MOVEL(P)  *BLANKS       SCRVDS
     C   60              SETON                                        9074
     C                   ENDIF                                                  SCRTVL VALID
      *
      * CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
      *
     C     @@MODE        IFEQ      'C'
     C     *IN90         ANDEQ     '0'
      *
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C  N90OTDT          IFGT      SAVEDT
     C     OTDT          OREQ      SAVEDT
     C     OTTIME        ANDGT     SAVETM
     C                   SETON                                        9078
     C                   ENDIF
      *
     C                   ENDIF                                                  @@MODE = 'C'
      *
     C                   ENDSR
      *
      *================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR02)
      *================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
      *
      *
      *===============================================================
      * KJDO -    F10=Search
      *===============================================================
      *
     C     KJDO          BEGSR
      *
      *
     C                   SELECT
      *
      *  Savings rate value search
      *  ZZZ
     C     HFLD          WHENEQ    'SCRTVL'
     C                   CALL      'F2380R'      SRRTVL
     C     ##RRN         IFNE      *ZERO
     C     ##RRN         CHAIN     PGT02                              60
     C                   MOVEL(P)  GTKEY         SCRTVL
     C                   MOVEL(P)  GTDESC        SCRVDS
     C                   SETON                                        77
     C                   ELSE
     C                   SETON                                        76
     C                   ENDIF
      *
      *
     C                   ENDSL
      *
     C                   SETON                                        90
      *
     C                   ENDSR                                                  KJDO
      *
      *================================================================
      * ERRCK3 - Error checking for srceen 3 (SCR03)
      *================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVEL(P)  OTCODE        SCCODE
     C                   MOVEL(P)  OTDESC        SCDESC
      * ZZZ
     C                   MOVEL(P)  OTRTVL        SCRTVL
     C                   MOVEL(P)  OTRTVL        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCRVDS
     C   60              MOVEL(P)  *BLANKS       SCRVDS
      *
     C                   Z-ADD     OTSVRT        SCSVRT
     C                   Z-ADD     OTPERC        SCPERC
     C                   MOVEL(P)  OTSTAT        SCSTAT
      *
     C                   ENDSR
      *
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR
      *
     C                   Z-ADD     ##CO#         OTCO#
      *
     C                   MOVEL(P)  SCCODE        OTCODE
     C                   MOVEL(P)  SCDESC        OTDESC
      *
     C                   MOVEL(P)  SCRTVL        OTRTVL
     C                   Z-ADD     SCSVRT        OTSVRT
     C                   Z-ADD     SCPERC        OTPERC
      *
     C                   MOVEL(P)  SCSTAT        OTSTAT
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVEL(P)  Q1USER        OTUSER
     C                   MOVEL(P)  Q1PGM         OTPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OTDT
     C                   MOVEL     DSTIME        OTTIME
      *
     C     @@MODE        IFEQ      'A'
      *
     C                   MOVEL(P)  'A'           OTSTAT
      *
     C                   MOVEL(P)  Q1USER        OTIUSR
     C                   MOVEL(P)  Q1PGM         OTIPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OTIDT
     C                   MOVEL     DSTIME        OTITIM
      *
     C                   ENDIF                                                  @@MODE = A
      *
     C                   ENDSR
      *
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     OTHFKS        BEGSR
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90      F10=Search
     C                   ENDCS
      *
     C                   ENDSR
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
      *  SET DISPLAY MODE INDICATORS
      *
     C                   SETOFF                                       094041
     C                   SELECT
      *
     C     @@MODE        WHENEQ    'D'                                          DISPLAY MODE
     C                   SETON                                        09
      *
     C     @@MODE        WHENEQ    'A'                                          ADD MODE
     C                   SETON                                        40
      *
     C     @@MODE        WHENEQ    'C'                                          CHG MODE
     C                   SETON                                        41
      *
     C                   ENDSL
      *
      * Initialize
      *
     C                   Z-ADD     ##CO#         KKCO#
     C                   MOVEL     'RTVL'        KKTBID
      *
     C                   ENDSR
      *
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR
      *
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
      *
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
      *
     C                   ENDSR
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FTOTP
     C                   ENDSR
