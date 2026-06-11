     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W1250R02 - DIRECT DEBIT EMPLOYER MASTER FILE
     H*****************************************************************
     H* Description:  This program will display a list of employers   *
     H*   that are setup on the direct debit plan.                    *
     H*                                                               *
     H* Called from:  W0050R                                          *
     H* Calls to:     NONE
     H* Created by:  C:136    04/11/03                                *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     H* Indicators:                                                   *
     H*                                                               *
     H*****************************************************************
     FW1250F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMDBP     UF A E             DISK
     F                                     RENAME(PDB01:DBRCDF)
     FWMDBL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMEMP     IF   E             DISK
     FWMFNL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D***PY SCOPYMT01I
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Direct Debit')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0                     WMDBP
     C                   PARM                    @@RRN1            9 0                     WMEMP
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
     C     DBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    EMFND
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C/COPY SCOPYMT01C
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
     C*  Validate Checking/Savings Flag
     C*
     C     SCSTAT        IFNE      'A'
     C     SCSTAT        ANDNE     'I'
     C                   SETON                                        71  90
     C                   ENDIF
     C*
     C*  Validate Checking/Savings Flag
     C*
     C  N90SCCSFL        IFNE      'C'
     C     SCCSFL        ANDNE     'S'
     C                   SETON                                        72  90
     C                   ENDIF
     C*
     C*  Validate DFI Identification
     C*
     C  N90SCDFII        IFLE      *ZERO
     C                   SETON                                        73  90
     C                   ENDIF
     C*
     C*  Validate Transit/Routing Check Digit
     C*
     C  N90SCTRCD        IFLT      *ZERO
     C                   SETON                                        74  90
     C                   ENDIF
     C*
     C*  Validate DFI Account Number
     C*
     C  N90SCDFIA        IFEQ      *BLANKS
     C                   SETON                                        75  90
     C                   ENDIF
     C*
     C*  Validate pre-notification date
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVE      DBPRED        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPRED            8 0
     C*
     C     SCPRED        IFNE      XXPRED
     C     SCPRED        ANDNE     *ZERO
     C                   MOVE      SCPRED        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPRED
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        76  90
     C                   ENDIF                                                  SRERR
     C*
     C                   CLEAR                   XXCNT
     C                   ENDIF                                                  SCPRED
     C*
     C                   ENDIF                                                  *IN90
     C*
     C  N90SCPRED        IFEQ      *ZERO
     C     DBPRED        ANDNE     *ZERO
     C     XXCNT         ANDEQ     *ZERO
     C                   ADD       1             XXCNT             1 0
     C                   SETON                                        77  90
     C                   ENDIF                                                  SCPRED
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
     C*===============================================================
     C* KUDO - SUBROUTINE FOR F20 = CHANGE STATUS
     C*===============================================================
     C     KUDO          BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C     DBSTAT        IFEQ      'A'
     C                   MOVEL     'I'           DBSTAT
     C                   MOVEL     'I'           SCSTAT
     C                   ELSE
     C                   MOVEL     'A'           DBSTAT
     C                   MOVEL     'A'           SCSTAT
     C                   ENDIF                                                  A3STAT=A
     C*
     C                   MOVEL     Q1USER        DBUSER
     C                   MOVEL     Q1PGM         DBPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DBDT
     C                   MOVEL     DSTIME        DBTIME
     C*
     C  N60              UPDATE    DBRCDF
     C*
     C                   ENDSR                                                  KUDO
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   SETOFF                                       31
     C*
     C                   MOVE      DBCSFL        SCCSFL
     C                   Z-ADD     DBDFII        SCDFII
     C                   Z-ADD     DBTRCD        SCTRCD
     C                   MOVE      DBDFIA        SCDFIA
     C*
     C                   Z-ADD     DBPRED        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPRED
     C                   Z-ADD     DB1STD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SC1STD
     C                   Z-ADD     DBLSTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCLSTD
     C                   MOVE      DBSTAT        SCSTAT
     C*
     C     DBCXDT        IFNE      *ZERO
     C                   Z-ADD     DBCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   SETON                                        31        DIPSLAY CANC DATE
     C                   END
     C*
     C   40              MOVE      'A'           SCSTAT
     C*
     C                   CLEAR                   XXCNT
      *
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         DBCO#
     C                   Z-ADD     SCFND         DBFND
     C                   Z-ADD     EMEMP#        DBEMP#
     C                   Z-ADD     EMDIV         DBDIV
     C                   Z-ADD     SCPRED        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DBPRED
     C                   MOVE      SCCSFL        DBCSFL
     C                   Z-ADD     SCDFII        DBDFII
     C                   Z-ADD     SCTRCD        DBTRCD
     C                   MOVE      SCDFIA        DBDFIA
     C                   MOVE      SCSTAT        DBSTAT
     C                   MOVEL     Q1USER        DBUSER
     C                   MOVEL     Q1PGM         DBPGM
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DBDT
     C                   MOVEL     DSTIME        DBTIME
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
     C     *INKU         CASEQ     *ON           KUDO                     90
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   SETON                                        LR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
      *
     C     @@RRN1        CHAIN     PEM01                              60
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        ##EMP#            9 0
     C                   PARM      EMDIV         ##DIV             5 0
     C                   PARM      EMNAME        ##NAME           40
     C                   PARM                    ##EDN            80
      *
     C                   MOVEL     ##EDN         SCEDN
     C                   Z-ADD     EMFND         SCFND
      *
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
      *
     C   60              RETURN
      *
     C     DBKY          CHAIN     PDB01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     DBRRN         @@RRN
     C                   MOVE      'C'           @@MODE
     C                   SETOFF                                       40
     C                   ELSE
     C                   MOVE      'A'           @@MODE
     C                   SETON                                        40
     C                   ENDIF
      *
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
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        DBIUSR
     C                   MOVEL     Q1PGM         DBIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DBIDT
     C                   MOVEL     DSTIME        DBITM
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      WMDBP
     C*
     C                   MOVE      'C'           @@MODE
     C*
     C                   ENDSR
