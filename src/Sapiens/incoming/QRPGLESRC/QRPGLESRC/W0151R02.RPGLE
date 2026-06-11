     H*  (C) Copyright Progressive Data Solutions, Inc., 2003
     H*****************************************************************
     H/TITLE W0151R02 - INTERNAL ACCT COMMISSION RATES INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL UPDATE/DISPLAY INTERNAL ACCT *
     H*                COMMISSION RATE FILE RECORDS.                  *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0151R - WORK WITH INTERNAL ACCT REP COMM RATES            *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 01/15/03  PROGRAMMER: C:136 PROJECT:114WEY          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW0151F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMICP     UF A E             DISK
     F                                     RENAME(PIC01:DBRCDF)
     FWMFNP     IF   E             DISK
     FWMICL     IF   E           K DISK
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Internal Acct Rep Co-
     D                                     mm Rates')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
     C/COPY SCOPYMT01C
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@GRRN            9 0
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
      *
     C     ICKY          KLIST
     C                   KFLD                    KKCO#             3 0
     C                   KFLD                    KKGRP             3 0
     C                   KFLD                    KKEFDT            7 0
     C                   KFLD                    KKNWRW            1
     C                   KFLD                    KKPLVL           11 2
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     ICSTAT        IFEQ      'I'
     C                   MOVE      'A'           ICSTAT
     C                   ELSE
     C                   MOVE      'I'           ICSTAT
     C                   ENDIF                                                  ICSTAT
     C*
     C                   MOVEL     Q1USER        ICUSER
     C                   MOVEL     Q1PGM         ICPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ICDT
     C*
     C                   MOVEL     DSTIME        ICTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN60
     C*
     C                   ENDSR                                                  CHGSTS
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
     C     *IN40         IFEQ      *ON
     C     *IN90         ANDEQ     *OFF
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        71  90
     C                   ENDIF                                                  SRERR '1'
     C*
     C                   ENDIF                                                  *IN40 *ON
     C*
     C* NEW/RENEW FLAG VALID
     C*
     C  N90*IN40         IFEQ      *ON
     C*
     C     SCNWRW        IFNE      'R'
     C     SCNWRW        ANDNE     'N'
     C                   SETON                                        72  90
     C                   ENDIF                                                  SCNWRW = 'R'
     C*
     C                   ENDIF                                                  *IN40 *ON
     C*
     C* PREMIUM LEVEL NOT LESS THAN OR EQUAL TO ZERO
     C*
     C  N90*IN40         IFEQ      *ON
     C     SCPLVL        ANDLE     *ZERO
     C     SCNWRW        ANDEQ     'N'
     C                   SETON                                        73  90
     C                   ENDIF                                                  *IN40 *ON
     C*
     C* RATE NOT LESS THAN OR EQUAL TO ZERO
     C*
     C  N90SCRATE        IFLE      *ZERO
     C                   SETON                                        74  90
     C                   ENDIF                                                  *IN40 *ON
     C*
     C* NO PRE-EXISTING RECORD WITH SAME KEY INFORMATION
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
      *
     C                   Z-ADD     ##CO#         KKCO#
     C                   Z-ADD     SCGRP         KKGRP
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C                   MOVE      SCNWRW        KKNWRW
     C                   Z-ADD     SCPLVL        KKPLVL
     C     ICKY          CHAIN     PIC01                              60
     C  N60              SETON                                        75  90
      *
     C                   ENDIF                                                  *IN90 *OFF
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
     C                   Z-ADD     ICEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   MOVE      ICNWRW        SCNWRW
     C                   Z-ADD     ICPLVL        SCPLVL
     C                   Z-ADD     ICRATE        SCRATE
     C   40              MOVEL     'A'           ICSTAT
     C                   MOVE      ICSTAT        SCSTAT
      *
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
      *
     C                   Z-ADD     ##CO#         ICCO#
     C                   Z-ADD     SCGRP         ICGRP
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ICEFDT
     C                   MOVE      SCNWRW        ICNWRW
     C                   Z-ADD     SCPLVL        ICPLVL
     C                   Z-ADD     SCRATE        ICRATE
     C                   MOVE      SCSTAT        ICSTAT
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
     C*
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C     @@GRRN        CHAIN     WMFNP                              60
     C*
     C                   Z-ADD     FNFND         SCGRP
     C                   MOVEL     FNNAME        SCGRNM
     C*
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
      *
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        ICUSER
     C                   MOVEL     Q1PGM         ICPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ICDT
     C                   MOVEL     DSTIME        ICTIME
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
      *
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
      *
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        ICUSER
     C                   MOVEL     Q1USER        ICIUSR
     C                   MOVEL     Q1PGM         ICPGM
     C                   MOVEL     Q1PGM         ICIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ICDT
     C                   MOVEL     SRDTO         ICIDT
     C                   MOVEL     DSTIME        ICTIME
     C                   MOVEL     DSTIME        ICITIM
     C                   WRITE     DBRCDF
     C                   FEOD      WMICP
      *
     C                   ENDSR
