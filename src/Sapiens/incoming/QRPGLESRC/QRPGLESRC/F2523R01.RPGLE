     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE F2523R01 - Maintenance - Skilled Nursing
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: F2523R - Work With Skilled Nursing                 *
     H*                                                               *
     H* CALLS TO:  F7052C - Submit - Skilled Nursing Facilities       *
     H*                              Table File Listing               *
     H*                                                               *
     H* CREATED:   06/28/01 C:61  PROJECT 114MCG                      *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/18/08 C:198 Project PA046                                  *
     H*                Added NPI#.                                    *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*                                                               *
     H* SCR01                                                         *
     H*  40       Add                                                 *
     H*  71       Medicare Number must be entered.                    *
     H*  72       Effective date must be valid.                       *
     H*  73       Record must be unique.                              *
     H*  89       Record updated by different user.                   *
     H*                                                               *
     H*****************************************************************
      *
     FF2523F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
      *  Physical file definition (for maintaining record)
     FFASNPPA   UF A E             DISK
     F                                     RENAME(PSN01:DBRCDF)
     FFASNLPA   IF   E           K DISK
     F                                     INFDS(DBFDS)
      *
      * System State
     FSTABL     IF   E           K DISK
      *
      *
      *
     D***PY SCOPYMT01I
      *
      /COPY SCOPYMT01D
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D DBFDS           DS
     D  DBRRN                397    400B 0
      *
      *
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('Skilled Nursi-
     D                                     ng Facilities')
     D COMAPI          C                   CONST('FA007R')
      *             'FSMESG'              C         SLMSGF
      *
      *
      *
      *================================================================
      *  PLIST definitions
      *================================================================

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ST              2
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      * PARM LIST FOR CALL TO STATE SPECIFIC PROGRAMS
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      @@ST          ##ST              2
      *
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================
      *
     C     SNKY          KLIST
     C                   KFLD                    KKMED#
     C                   KFLD                    KKEFDT
      *
     C     STKY          KLIST
     C                   KFLD                    KKST
      *
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    SNMED#        KKMED#
     C     *LIKE         DEFINE    SNEFDT        KKEFDT
     C     *LIKE         DEFINE    STKEY         KKST
      *
      *
      *
     C/COPY SCOPYMT01C
      *
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
     C     SNSTAT        IFEQ      'I'
     C                   MOVE      'A'           SNSTAT
      *
     C                   ELSE
      *
     C                   MOVE      'I'           SNSTAT
     C                   ENDIF                                                  SNSTAT
      *
     C                   MOVEL     Q1USER        SNUSER
     C                   MOVEL     Q1PGM         SNPGM
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SNDT
      *
     C                   MOVEL     DSTIME        SNTIME
      *
     C                   UPDATE    DBRCDF
     C                   ENDIF                                                  *IN99
      *
     C                   ENDSR                                                  CHGSTS
      *
      *
      *
      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
      *
      *
      *
      *===============================================================
      * DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
      *===============================================================
      *
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
      *
      *
      *
      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR
      *
      *  Medicare# must be entered
      *
     C  N90*IN30         IFEQ      *ON
     C     *IN40         OREQ      *ON
      *
     C     SCMED#        IFEQ      *BLANKS
     C                   SETON                                        7190
     C                   ENDIF
      *
     C                   ENDIF                                                  MED#
      *
      *  Effective Date must be valid
      *
     C  N90*IN30         IFEQ      *ON
     C     *IN40         OREQ      *ON
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
      *
     C     SRERR         IFEQ      '1'
     C                   SETON                                        72  90
     C                   ENDIF                                                  SRERR
      *
     C                   ENDIF                                                  EFDT
      *
      *  MED#/EFDT must be unique
      *
     C  N90*IN30         IFEQ      *ON
     C     *IN40         OREQ      *ON
     C                   MOVEL(P)  SCMED#        KKMED#
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
      *
     C     SNKY          CHAIN     PSN01                              60
     C  N60              SETON                                        73  90
     C                   ENDIF                                                  MED#/EFDT
      *
      * CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C  N90SNDT          IFGT      SAVEDT
     C     SNDT          OREQ      SAVEDT
     C     SNTIME        ANDGT     SAVETM
     C                   SETON                                          8990
     C                   ENDIF
      *
     C                   ENDSR                                                  ERRCK1
      *
      *
      *
      *================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR02)
      *================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
      *
      *
      *
      *================================================================
      * ERRCK3 - Error checking for srceen 3 (SCR03)
      *================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
      *
      *
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
      *
     C                   MOVEL(P)  SNMED#        SCMED#
      *
     C*** BEG ADD *** 11/18/08 ****************************************
     C                   MOVEL(P)  SNNPI#        SCNPI#
     C*** END ADD *** 11/18/08 ****************************************
     C                   Z-ADD     SNEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
      *
     C                   Z-ADD     SNINPT        SCINPT
     C                   Z-ADD     SNOUPT        SCOUPT
      *
     C  N40              MOVE      SNSTAT        SCSTAT
     C   40              MOVE      'A'           SCSTAT
      *
     C                   ENDSR                                                  MVDBSC
      *
      *
      *
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR
      *
     C                   MOVEL(P)  SCMED#        SNMED#
      *
     C*** BEG ADD *** 11/18/08 ****************************************
     C                   MOVEL(P)  SCNPI#        SNNPI#
     C*** END ADD *** 11/18/08 ****************************************
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SNEFDT
      *
     C                   Z-ADD     SCINPT        SNINPT
     C                   Z-ADD     SCOUPT        SNOUPT
     C                   MOVE      SCSTAT        SNSTAT
      *
     C                   ENDSR                                                  MVSCDB
      *
      *
      *
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     OTHERR        BEGSR
     C                   ENDSR                                                  OTHERR
      *
      *
      *
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR                                                  OTHFKS
      *
      *
      *
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   MOVE      *ON           *INLR
     C                   ENDSR                                                  PGMEND
      *
      *
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
     C                   Z-ADD     1             #OFSCR            1 0
      *
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C     @@MODE        COMP      'P'                                    30    COPY
      *
      *  State header.
      *
     C                   MOVEL     @@ST          KKST
     C                   MOVEL     @@ST          SCST
     C     STKY          CHAIN     PST01                              60
     C  N60              MOVEL(P)  STNAME        SCSTNM
     C   60              MOVEL(P)  *BLANKS       SCSTNM
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR
      *
     C                   SELECT
      *
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  PREOTH
      *
      *
      *
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     UPDATR        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        SNUSER
     C                   MOVEL     Q1PGM         SNPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SNDT
      *
     C                   MOVEL     DSTIME        SNTIME
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
      *
     C                   ENDSR                                                  UPDATR
      *
      *
      *
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR
      *
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        SNUSER
     C                   MOVEL     Q1USER        SNIUSR
     C                   MOVEL     Q1PGM         SNPGM
     C                   MOVEL     Q1PGM         SNIPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SNDT
     C                   MOVEL     SRDTO         SNIDT
      *
     C                   MOVEL     DSTIME        SNTIME
     C                   MOVEL     DSTIME        SNITIM
     C                   WRITE     DBRCDF
      *
     C                   FEOD      FASNPPA
     C                   ENDSR                                                  WRITER
      *
      *
      *
