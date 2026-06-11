     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE S0006R - USER PARAMETER MAINTENANCE
     H****************************************************************
     H*  DESCRIPTION: UPDATE THE USER PARAMETER FILE.                *
     H*                                                              *
     H*  CALLED FROM:                                                *
     H*    MANY                                                      *
     H*  CALLS TO:                                                   *
     H*    W0006R                                                    *
     H*    F0006R                                                    *
     H*                                                              *
     H*  CREATED: 11/19/99      C:33      PROJECT: DR079             *
     H****************************************************************
     H*  REVSIONS:                                                   *
     H*                                                              *
     H****************************************************************
     FS0006F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FSMUPP     UF A E             DISK
     F                                     RENAME(PUP01:DBRCDF)
     FSMUPL     IF   E           K DISK    INFDS(UPFDS)
     FSMCOL     IF   E           K DISK
     D*
     D***PY SCOPYMT01I
     D*
      /COPY SCOPYMT01D
     D UPFDS           DS
     D  UPRRN                397    400B 0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('User Parameter Maint-
     D                                     enance')
     D COMAPI          C                   CONST('SA005R')
     D #OFSCR          C                   CONST(1)

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C*
     C*  NOTE:  @@CALL does not need to be passed in.
     C*         If @@CALL is passed in as:
     C*          - ' ' -> This program will be used
     C*          - 'M' -> WCMC program will be used (F0006R)
     C*          - 'P' -> PCMS program will be used (C0006R)
     C*          - 'W' -> WCMS program will be used (W0006R)
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    @@CALL        XXCALL
     C     *LIKE         DEFINE    @@CALL        @@MODE
     C     *LIKE         DEFINE    UPRRN         @@RRN
     C     *LIKE         DEFINE    SCPRDS        ##PRDS
     C     *LIKE         DEFINE    @@MODE        ##CALL
     C     *LIKE         DEFINE    Q1PGM         ##PGM
     C     *LIKE         DEFINE    *IN99         ##ERR
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C*  COMPANY MUST BE VALID
     C*
     C     SCCO#         CHAIN     PCO01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C     COSTAT        OREQ      'I'
     C                   SETON                                        71  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C*  CHECK THE PROCESS DATE FOR A VALID DATE
     C*
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        72  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  CHECK THE OUTQ FOR A VALID VALUE
     C*
     C                   CALL      'S0003C'
     C                   PARM                    SCOUTQ
     C                   PARM      ' '           XXFOND            1
     C*
     C  N90XXFOND        IFEQ      '1'
     C                   SETON                                        73  90
     C                   ENDIF                                                  XXFOND=*ON
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C*
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
     C*
     C*================================================================
     C* KJDO   - Processing for F10
     C*================================================================
     C*
     C     KJDO          BEGSR
     C*
     C                   CLEAR                   ##PGM
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCCO#'
     C                   MOVEL(P)  'S0510R'      ##PGM
     C                   ENDSL
     C*
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM                    ##PGM
     C                   PARM                    ##ERR
     C*
     C     ##ERR         IFEQ      *ON
     C     ##PGM         ANDNE     *BLANKS
     C                   SETON                                        90
     C                   ENDIF                                                  ##ERR=*ON
     C*
     C  N90              SELECT
     C*_______________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCCO#'
     C*
     C                   CALL      'S0501R'
     C                   PARM      SCPRDS        ##PRDS
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##CO#
     C                   PARM      *BLANKS       ##RTC             1
     C*
     C                   SELECT
     C     ##RTC         WHENEQ    'A'
     C                   MOVE      *ON           *INKC
     C     ##RTC         WHENEQ    'B'
     C     ##CO#         OREQ      *ZERO
     C                   MOVE      *ON           *INKL
     C                   SETON                                        81  90
     C     ##RTC         WHENEQ    '*'
     C                   Z-ADD     ##CO#         SCCO#
     C                   SETON                                        82  90
     C                   ENDSL
     C*_______________________________________________________________
     C*
     C                   OTHER
     C*
     C                   SETON                                            90
     C*_______________________________________________________________
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*================================================================
     C* MAINT  - Update maintenance fields
     C*================================================================
     C*
     C     MAINT         BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      *BLANK        UPSTAT
     C                   MOVEL(P)  Q1USER        UPUSER
     C                   MOVEL(P)  Q1PGM         UPPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPDT
     C*
     C                   Z-ADD     DSTIME        UPTIME
     C*
     C     *IN40         IFEQ      *ON
     C                   MOVEL     UPPGM         UPIPGM
     C                   MOVEL     UPUSER        UPIUSR
     C                   Z-ADD     UPDT          UPIDT
     C                   ENDIF                                                  *IN40=*ON
     C*
     C                   ENDSR                                                  MAINT
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     UPCO#         SCCO#
     C*
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT
     C*
     C                   MOVEL(P)  UPOUTQ        SCOUTQ
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCCO#         UPCO#
     C*
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPPRDT
     C*
     C                   MOVEL(P)  SCOUTQ        UPOUTQ
     C*
     C                   EXSR      MAINT
     C*
     C                   ENDSR                                                  MVSCDB
     C*
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                  OTHERR
     C*
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   CLEAR                   XXCALL
     C*
     C     Q1PRMS        IFGE      1
     C                   MOVE      @@CALL        XXCALL
     C                   ENDIF                                                  Q1PRMS>=1
     C*
     C     XXCALL        CASEQ     'M'           WCMC
     C     XXCALL        CASEQ     'P'           PCMS
     C     XXCALL        CASEQ     'W'           WCMS
     C                   ENDCS
     C*
     C                   CLEAR                   UPRRN
     C     Q1USER        CHAIN     PUP01                              99
     C                   Z-ADD     UPRRN         @@RRN
     C*
     C                   SELECT
     C     XXCALL        WHENEQ    '1'
     C                   MOVE      'D'           @@MODE
     C     *IN99         WHENEQ    *ON
     C                   MOVE      'A'           @@MODE
     C     *IN99         WHENEQ    *OFF
     C                   MOVE      'C'           @@MODE
     C                   ENDSL
     C*
     C     @@MODE        COMP      'A'                                    40
     C     @@MODE        COMP      'D'                                    09
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C*
     C     PREOTH        BEGSR
     C                   ENDSR                                                  PREOTH
     C*
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*================================================================
     C* PCMS   - Process PCMS user parameter program
     C*================================================================
     C*
     C     PCMS          BEGSR
     C*
     C                   CALL      'C0006R'
     C*
     C                   RETURN
     C*
     C                   ENDSR                                                  PCMS
     C*
     C*================================================================
     C* WCMC   - Process WCMC user parameter program
     C*================================================================
     C*
     C     WCMC          BEGSR
     C*
     C                   CALL      'F0006R'
     C*
     C                   RETURN
     C*
     C                   ENDSR                                                  WCMC
     C*
     C*================================================================
     C* WCMS   - Process WCMS user parameter program
     C*================================================================
     C*
     C     WCMS          BEGSR
     C*
     C                   CALL      'W0006R'
     C*
     C                   RETURN
     C*
     C                   ENDSR                                                  WCMS
     C*
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      SMUPP
     C*
     C                   ENDSR                                                  WRITER
