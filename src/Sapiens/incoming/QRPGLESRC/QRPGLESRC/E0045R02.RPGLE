     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE E0045R02 - EMPLOYER PAYROLL MAINTENANCE
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* MAINTENANCE PROGRAM FOR EMPLOYER PAYROLL                      *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CREATED: 08/17/01                                             *
     H*                                                               *
     H* 08/17/01 C:136 PROJECT WC577                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/05/02 C:136 SUPPORT: WA093 32854                           *
     H*                Correct the error check routine.               *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  80       POSITION CURSOR TO SCCLAS (PAYROLL CLASS)           *
     H*                                                               *
     H*****************************************************************
     FE0045F02  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
      *
      *  Physical file definition (for maintaining record)
     FEMEPP     UF A E             DISK
     F                                     RENAME(PEP01:DBRCDF)
      *
      * Employer Batch Number Master File
     FEMEBP     IF   E             DISK
      *
      * Employer Master File
     FEMEML     IF   E           K DISK
      *
      * Fund Master File
     FEMFNL     IF   E           K DISK
      *
     F* Manual Rate Logical File
     FEMMRL2    IF   E           K DISK
     F*
     D***PY SCOPYMT01I
      *
      /COPY SCOPYMT01D
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Employer Payroll')
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
     C                   PARM                    @@RRN2            9 0
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##CONM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
      *
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
      *
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
      *
     C     MRKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKDRSC
     C*
     C     MRKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKDRSC
     C                   KFLD                    KKCLAS
     C*
     C     MRKY3         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKDRSC
     C                   KFLD                    KKCLAS
     C                   KFLD                    KKRDAT
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
      *
     C     *LIKE         DEFINE    EPFND         KKFND
     C     *LIKE         DEFINE    EPEMP#        KKEMP#
     C     *LIKE         DEFINE    EPDRSC        KKDRSC
     C     *LIKE         DEFINE    EPCLAS        KKCLAS
     C     *LIKE         DEFINE    EPRDAT        KKRDAT
      *
     C/COPY SCOPYMT01C
      *
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
      *
     C     CHGSTS        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C     EPSTAT        IFEQ      'I'
     C                   MOVE      'A'           EPSTAT
     C                   ELSE
     C                   MOVE      'I'           EPSTAT
     C                   ENDIF                                                  EPSTAT
      *
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPPGM
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
      *
     C                   MOVEL     DSTIME        EPTIME
      *
     C                   UPDATE    DBRCDF
      *
     C                   ENDIF                                                  *IN99
      *
     C                   ENDSR                                                  CHGSTS
      *
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
      *  Rate set code must be valid
     C     *IN90         IFEQ      *OFF
     C     SCDRSC        ANDNE     *BLANK
     C                   MOVEL(P)  SCDRSC        KKDRSC
     C     MRKY1         CHAIN     PMR01                              60
     C   60              SETON                                        71  90
     C                   ENDIF
      *  Rate set code must not be blank
     C     *IN90         IFEQ      *OFF
     C     SCDRSC        ANDEQ     *BLANK
     C                   SETON                                        72  90
     C                   ENDIF
      *  Payroll class must be valid
     C     *IN90         IFEQ      *OFF
     C     SCCLAS        ANDNE     *ZEROS
     C                   Z-ADD     SCCLAS        KKCLAS
     C     MRKY2         CHAIN     PMR01                              60
     C   60              SETON                                        73  90
     C                   ENDIF
      *  Payroll class must not be blank
     C     *IN90         IFEQ      *OFF
     C     SCCLAS        ANDEQ     *ZEROS
     C                   SETON                                        74  90
     C                   ENDIF
      *  Rate date must be in manual rate table
     C     *IN90         IFEQ      *OFF
     C     SCRDAT        ANDNE     *ZEROS
     C                   Z-ADD     SCRDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         KKRDAT
      *** BEG CHG **** 06/05/02 ******************************************
     C********** MRKY3     CHAINPMR01                60
     C     MRKY3         SETGT     PMR01
     C     MRKY2         READPE    PMR01                                  60
      *** END CHG **** 06/05/02 ******************************************
     C   60              SETON                                        75  90
     C                   ENDIF
      *  Rate date must be in manual rate table
     C     *IN90         IFEQ      *OFF
      *** BEG CHG **** 06/05/02 ******************************************
     C********** SCRDAT    ANDNE*ZEROS
     C     SCRDAT        ANDEQ     *ZEROS
      *** END CHG **** 06/05/02 ******************************************
     C                   SETON                                        76  90
     C                   ENDIF
      *  Payroll amt cannot be less than zero
     C     *IN90         IFEQ      *OFF
     C     SCAMT         ANDLT     *ZERO
     C                   SETON                                        77  90
     C                   ENDIF
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
      *
     C     HFLD          WHENEQ    'SCDRSC'
     C                   MOVE      '2'           #1CALL            1
     C                   CALL      'E0035R'
     C                   PARM                    #1CALL
     C                   PARM                    #1DRSC            3
     C                   PARM                    #1CLAS            5 0
     C                   PARM                    #1RDAT            7 0
     C                   MOVEL     #1DRSC        SCDRSC
     C                   Z-ADD     #1CLAS        SCCLAS
     C                   Z-ADD     #1RDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRDAT
     C                   SETON                                        50        GO TO SCAMT
      *
     C     HFLD          WHENEQ    'SCCLAS'
     C                   MOVE      '2'           #1CALL
     C                   MOVE      SCDRSC        #1DRSC
     C                   CALL      'E0035R'
     C                   PARM                    #1CALL
     C                   PARM                    #1DRSC
     C                   PARM                    #1CLAS
     C                   PARM                    #1RDAT
     C                   MOVEL     #1DRSC        SCDRSC
     C                   Z-ADD     #1CLAS        SCCLAS
     C                   Z-ADD     #1RDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRDAT
     C                   SETON                                        50        GO TO SCAMT
      *
     C     HFLD          WHENEQ    'SCRDAT'
     C                   MOVE      '2'           #1CALL
     C                   MOVE      SCDRSC        #1DRSC
     C                   MOVE      SCCLAS        #1CLAS
     C                   CALL      'E0035R'
     C                   PARM                    #1CALL
     C                   PARM                    #1DRSC
     C                   PARM                    #1CLAS
     C                   PARM                    #1RDAT
     C                   MOVEL     #1DRSC        SCDRSC
     C                   Z-ADD     #1CLAS        SCCLAS
     C                   Z-ADD     #1RDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRDAT
     C                   SETON                                        50        GO TO SCAMT
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *
     C                   SETOFF                                       50        KJDO SETS ON 50
     C                   MOVEL     EPDRSC        SCDRSC
     C                   Z-ADD     EPCLAS        SCCLAS
     C                   MOVEL     EPSTAT        SCSTAT
     C                   Z-ADD     EPAMT         SCAMT
      *
     C                   Z-ADD     EPRDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRDAT
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
     C                   Z-ADD     ##CO#         EPCO#
     C                   Z-ADD     SCFND         EPFND
     C                   Z-ADD     SCYEAR        EPYEAR
     C                   Z-ADD     SCEMP#        EPEMP#
     C                   MOVEL     SCDRSC        EPDRSC
     C                   Z-ADD     SCCLAS        EPCLAS
     C                   MOVEL     SCSTAT        EPSTAT
     C                   Z-ADD     SCAMT         EPAMT
      *
     C                   Z-ADD     SCRDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPRDAT
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
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
      *
     C     @@MODE        COMP      'A'                                    40
      *
     C                   CLEAR                   PEB01
     C     @@RRN2        CHAIN     PEB01                              60
     C                   Z-ADD     EBFND         SCFND
     C                   Z-ADD     EBEMP#        SCEMP#
     C                   Z-ADD     EBYEAR        SCYEAR
     C                   Z-ADD     EBFND         KKFND
     C                   Z-ADD     EBEMP#        KKEMP#
      *
     C                   CLEAR                   PEM01
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVEL(P)  EMNAME        SCEMNM
      *
     C                   CLEAR                   PFN01
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL(P)  FNNAME        SCFNNM
      *
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
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C                   MOVEL     DSTIME        EPTIME
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1USER        EPIUSR
     C                   MOVEL     Q1PGM         EPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C                   MOVEL     SRDTO         EPIDT
     C                   MOVEL     DSTIME        EPTIME
     C                   WRITE     DBRCDF
     C                   FEOD      EMEPP
     C                   ENDSR
