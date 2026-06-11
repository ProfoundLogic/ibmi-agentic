     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  S0550R01 - GROUPS DEPARTMENT CODE INFORMATION  C:62 05/14/98 *
     H*                                                               *
     H*  CALLED FROM: S0550R                                          *
     H*  CALLS TO:    NONE
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FS0550F01  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FSMDEP     UF A E             DISK
     F                                     RENAME(PDE01:DBRCDF)
     FSMDEL     IF   E           K DISK
     F*** BEGIN ADD *** 11/30/98 **************************************
     FSTGTL     IF   E           K DISK
     FSTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     F*** END   ADD *** 11/30/98 **************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Department codes')
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
     C     DEKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKDPCD
     C*** BEGIN ADD *** 11/30/98 **************************************
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*** END   ADD *** 11/30/98 **************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    DEDPCD        KKDPCD
     C*** BEGIN ADD *** 11/30/98 **************************************
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*** END   ADD *** 11/30/98 **************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     DESTAT        IFEQ      'I'
     C                   MOVE      'A'           DESTAT
     C                   ELSE
     C                   MOVE      'I'           DESTAT
     C                   ENDIF                                                  DESTAT
     C*
     C                   MOVEL     Q1USER        DEUSER
     C                   MOVEL     Q1PGM         DEPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DEDT
     C*
     C                   MOVEL     DSTIME        DETIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
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
     C* DEPARTMENT CODE CAN'T BE BLANK.
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C     SCDPCD        ANDEQ     *BLANKS
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C* DEPARTMENT CODE CAN'T BE BLANK.
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C*
     C                   MOVEL(P)  SCDPCD        KKDPCD
     C*
     C     DEKY          CHAIN     PDE01                              99
     C*
     C  N99              SETON                                        7290
     C*
     C                   ENDIF
     C*
     C* DEPARTMENT NAME CAN'T BE BLANK.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCNAME        ANDEQ     *BLANKS
     C*
     C                   SETON                                        7390
     C*
     C                   ENDIF
     C*
     C* DEPARTMENT LETTER TYPE CAN'T BE BLANK.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL(P)  'DLTY'        KKTBID
     C                   MOVEL(P)  SCDLTY        KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C   99              SETON                                        7490
     C  N99              MOVEL(P)  GTDESC        SCLDSC
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
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*** LETTER TYPE SEARCH.
     C*
     C     HFLD          IFEQ      'SCDLTY'
     C*
     C                   CALL      'S2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'DLTY'        ##TBID            4
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        31
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C                   MOVEL(P)  GTKEY         SCDLTY
     C                   MOVEL(P)  GTDESC        SCLDSC
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL(P)  DEDPCD        SCDPCD
     C                   MOVEL(P)  DENAME        SCNAME
     C                   MOVEL     DESTAT        SCSTAT
     C*
     C                   MOVEL(P)  DEDLTY        SCDLTY
     C                   MOVEL(P)  'DLTY'        KKTBID
     C                   MOVEL(P)  DEDLTY        KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C   99              MOVEL(P)  *BLANKS       SCLDSC
     C  N99              MOVEL(P)  GTDESC        SCLDSC
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  *BLANKS       SCDPCD
     C                   MOVEL(P)  *BLANKS       SCNAME
     C                   MOVEL     'A'           SCSTAT
     C                   MOVEL(P)  *BLANKS       SCDLTY
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         DECO#
     C                   MOVEL(P)  SCDPCD        DEDPCD
     C                   MOVEL(P)  SCNAME        DENAME
     C                   MOVEL     SCSTAT        DESTAT
     C                   MOVEL(P)  SCDLTY        DEDLTY
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
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
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
     C     @@MODE        COMP      'D'                                    09
     C     @@MODE        COMP      'A'                                    40
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        DEUSER
     C                   MOVEL     Q1PGM         DEPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        DETIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DEDT
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        DEUSER
     C                   MOVEL     Q1PGM         DEPGM
     C                   MOVEL     Q1USER        DEIUSR
     C                   MOVEL     Q1PGM         DEIPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        DETIME
     C                   Z-ADD     DSTIME        DEITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DEDT
     C                   MOVEL     SRDTO         DEIDT
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      SMDEP
     C                   ENDSR
