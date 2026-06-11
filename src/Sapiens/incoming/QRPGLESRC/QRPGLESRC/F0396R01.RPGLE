     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      * ***************************************************************
      /TITLE F0396R01 - Recon Delete Maint Pgm
      * ***************************************************************
      *
      *  This program will display a screen to confirm a delete
      *  of the most current reconsideration for an EOMB.  A delete
      *  reason code is mandatory.  If F12 is not pressed, this pgm
      *  will call FA749R to archive & delete all of the associated
      *  records.  This pgm currently only writes a record to FAESP.
      *
      *  Called by: F0396R opt 8
      *
      *  Calls to : FA749R - Archive & Delete EOB Files
      *
      *  CREATED: 11/15/07  PROGRAMMER: C:47  PROJECT: MC309
      ****************************************************************
      *  REVISIONS:
      *
      *  mm/dd/yy  C:000  SUPPORT: MCnnn nnnnn
      *                   PROJECT: 999xxx
      * **************************************************************
     FF0396F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFAESP     UF A E             DISK
     F                                     RENAME(PES01:DBRCDF)
     F*
     FFAEAP     IF   E             DISK
     F*  Generic Table File
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F*
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Provider Bill Header-
     D                                      Amounts')
     D COMAPI          C                   CONST('SA005R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FOR FAESP
     C                   PARM                    @@RRNA            9 0          FOR FAEAP
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##CONM           40
     C*
     C     GTPLT         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID            4
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    ESCO#         KKCO#
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   Z-ADD     ESCO#         KKCO#
     C                   CLEAR                   SCRCCD
     C                   CLEAR                   SCRCDS
     C                   CLEAR                   SCRCRN
     C                   MOVEA     '00000'       *IN(75)                        *IN75 - 79
     C                   MOVEA     '0'           *IN(90)
     C*
    C* DELETE CONFIRMATION SCREEN
    C*
     C     *IN90         DOUEQ     *OFF
     C     *INKL         OREQ      *ON
     C     *INKC         OREQ      *ON
     C                   EXFMT     SCRDE
     C                   MOVEA     '00000'       *IN(75)                        *IN75 - 79
     C                   MOVEA     '0'           *IN(90)
    C*
     C*  F10 = SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C                   EXSR      KJDO
     C                   ENDIF
     C*
     C                   DO
     C*
     C*  DELETE REASON CODE IS REQUIRED
     C*
     C     SCRCCD        IFEQ      *BLANK
     C                   SETON                                        9075
     C                   LEAVE
     C                   ENDIF
     C*
     C*  VALIDATE DELETE REASON CODE
     C*
     C     SCRCCD        IFNE      *BLANK
     C                   MOVEL(P)  'RCON'        KKTBID
     C                   MOVEL(P)  SCRCCD        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9076
     C                   LEAVE
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCRCCD
     C*
     C                   ENDDO                                                  DO
     C*
     C                   ENDDO                                                  *IN90
     C*
    C* --------------------------------------------------------------
     C*
     C* LAST CHANCE TO NOT DELETE
     C*
     C     *INKL         IFEQ      *ON
     C     *INKC         OREQ      *ON
     C                   RETURN
     C                   END
    C* --------------------------------------------------------------
     C*
     C* CALL API TO SAVE ORIGINAL RECORDS TO THEIR RESPECTIVE
     C* BACK UP FILES AND THEN DELETE THEM FROM THE MAIN FILES.
     C* EX: COPY RECORD TO FAEAPBK, THEN DELETE IT FROM FAEAP.
     C*
     C     *INKL         IFEQ      *OFF
     C     *INKC         ANDEQ     *OFF
     C                   Z-ADD     EACO#         QICO#
     C                   Z-ADD     EASUB#        QISUB#
     C                   Z-ADD     EAFND         QIFND
     C                   Z-ADD     EAFYR         QIFYR
     C                   Z-ADD     EACASE        QICASE
     C                   Z-ADD     EAEOB#        QIEOB#
     C                   Z-ADD     EARCN         QIRCN
     C*
     C                   CALL      'FA749R'
     C                   PARM                    QICO#             3 0
     C                   PARM                    QISUB#            5 0
     C                   PARM                    QIFND             3 0
     C                   PARM                    QIFYR             3 0
     C                   PARM                    QICASE            7 0
     C                   PARM                    QIEOB#            9 0
     C                   PARM                    QIRCN             3 0
     C*
     C* WRITE RECORD TO FAESP
     C                   EXSR      WRITER
     C                   END                                                    *INKL = OFF
     C*
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
     C                   SELECT
     C*
     C** DELETE REASON CODE
     C*
     C     HFLD          WHENEQ    'SCRCCD'
     C*
     C                   MOVEL(P)  'RCON'        ##TBID
     C                   CALL      'F2380R'      GTPLT
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9031      pos cursor
     C                   ENDIF                                                  ##KL IFEQ *ON
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL(P)  GTKEY         SCRCCD
     C                   MOVEL(P)  GTDESC        SCRCDS
     C                   SETON                                        9032      pos cursor
     C                   ENDIF                                                  ##RRN IFNE ZEROS
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
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
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C                   SELECT
     C*
     C** DELETE REASON CODE ON SCRDE
     C*
     C     HFLD          WHENEQ    'SCRCCD'
     C*
     C                   MOVEL(P)  'RCON'        ##TBID
     C                   CALL      'F2380R'      GTPLT
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9031      pos cursor
     C                   ENDIF                                                  ##KL IFEQ *ON
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL(P)  GTKEY         SCRCCD
     C                   MOVEL(P)  GTDESC        SCRCDS
     C                   SETON                                        9032      pos cursor
     C                   ENDIF                                                  ##RRN IFNE ZEROS
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   Z-ADD     EACO#         ESCO#
     C                   Z-ADD     EASUB#        ESSUB#
     C                   Z-ADD     EAFND         ESFND
     C                   Z-ADD     EAFYR         ESFYR
     C                   Z-ADD     EACASE        ESCASE
     C                   Z-ADD     EAEOB#        ESEOB#
     C                   Z-ADD     EARCN         ESRCN
     C     EARCN         SUB       1             ESRCN
     C     ESRCN         IFLT      *ZERO
     C                   Z-ADD     *ZERO         ESRCN
     C                   END
     C                   MOVE      EASS#         ESSS#
     C                   Z-ADD     EAACDT        ESACDT
     C                   MOVEL(P)  'C'           ESBLST
     C                   MOVEL(P)  SCRCCD        ESSTCD
     C                   MOVEL(P)  SCRCRN        ESREAS
     C                   Z-ADD     *ZERO         ESRRDT
     C                   Z-ADD     *ZERO         ESRSDT
      *
     C                   MOVE      'A'           ESSTAT
     C                   MOVE      *BLANK        ESUSER
     C                   MOVE      *BLANK        ESPGM
     C                   Z-ADD     *ZERO         ESDT
     C                   Z-ADD     *ZERO         ESTIME
      *
     C                   MOVEL(P)  Q1USER        ESIUSR
     C                   MOVEL(P)  Q1PGM         ESIPGM
     C                   EXSR      GETDTM
     C                   Z-ADD     SAVEDT        ESIDT
     C                   Z-ADD     SAVETM        ESITIM
     C*
     C                   Z-ADD     *ZERO         ESDIAL
     C                   Z-ADD     *ZERO         ESDICM
     C                   Z-ADD     *ZERO         ESDIDT
     C                   MOVE      *BLANK        ESDKEY
     C                   Z-ADD     *ZERO         ESDSEQ
     C                   Z-ADD     *ZERO         ESLTR#
     C                   Z-ADD     *ZERO         ESBRDT
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
     C     *INKJ         CASEQ     *ON           KJDO                           F10=search
     C                   ENDCS
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
     C     @@RRNA        CHAIN     PEA01                              98
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
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FAESP
     C                   ENDSR
