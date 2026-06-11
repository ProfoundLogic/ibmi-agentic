     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE F2531R01 - FEIN/Med# Table
     H*****************************************************************
     H* PROGRAM DESCRIPTION: FEIN/Med# Table Information              *
     H*                                                               *
     H* CALLED BY: F2531R                                             **
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H*****************************************************************
     H* Created 08/16/2001 by C:62 for Project:114MCG                 *
     H*****************************************************************
     H* REVISIONS:
     H*                                                                    *
     H* 12/17/04 C:79 Support: MC129 45115                                 *
     H*               Edit FEIN in change mode also                        *
     H*                                                                    *
     H* 09/07/04 C:79 Project: NY009                                       *
     H*               Populate Name on FEIN search                         *
     H*                                                                    *
     H* 11/21/02 C:79 PROJECT: NY006
     H*               TEMP LIFT EDIT ON FEIN IF STATE = NY
     H*               MAKE MEDICARE# A VARIABLE HEADING
     H*
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  99       GENERAL READ/CHAIN                                  *
     H*  71       SCFEIN IS NOT VALID PROVIDER                        *
     H*  72       DUPLICATE                                           *
     H*  73       SCNAME CANNOT BE BLANK                              *
     H*  74       SCMED# CANNOT BE BLANK                              *
     H*  75       SCNAME NON-SEARCHABLE FIELD                         *
     H*  76       SCMED# NON-SEARCHABLE FIELD                         *
     H*  77       SCZIP NON-SEARCHABLE FIELD                          *
     H*                                                               *
     H*****************************************************************
     FF2531F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMFMP     UF A E             DISK
     F                                     RENAME(PFM01:DBRCDF)
     FFMFML2    IF   E           K DISK
     F                                     INFDS(DBFDS)
     FSTABL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('FEIN and Medicare-
     D                                      Number Table')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D*
     D*================================================================
     D*  Other constants
     D*================================================================
     D NYHD            C                   CONST('OpCert#  :')
     D PAHD            C                   CONST('Medicare#:')
     D STHD            C                   CONST('Medicare#:')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ST              2
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
     C     FMKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKNAME
     C                   KFLD                    KKMED#
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C*** BEG ADD *** 09/07/04 ****************************************
     C*
     C     PDKY1         KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*** END ADD *** 09/07/04 ****************************************
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST             10
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    FMFEIN        KKFEIN
     C     *LIKE         DEFINE    FMNAME        KKNAME
     C     *LIKE         DEFINE    FMMED#        KKMED#
     C*** BEG ADD *** 09/07/04 ****************************************
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C*** END ADD *** 09/07/04 ****************************************
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
     C     FMSTAT        IFEQ      'I'
     C                   MOVE      'A'           FMSTAT
     C                   ELSE
     C                   MOVE      'I'           FMSTAT
     C                   ENDIF                                                  FMSTAT
     C*
     C                   MOVEL     Q1USER        FMUSER
     C                   MOVEL     Q1PGM         FMPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FMDT
     C*
     C                   MOVEL     DSTIME        FMTIME
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
     C* FEIN MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C*** BEG DEL *** 09/07/04 ****************************************
     C*********  *IN40     ANDEQ*ON
     C*** END DEL *** 09/07/04 ****************************************
     C                   Z-ADD     SCFEIN        KKFEIN
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7190
     C                   ENDIF
     C                   ENDIF
     C*
     C* NAME CAN'T BE BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C     SCNAME        ANDEQ     *BLANKS
     C                   SETON                                        7390
     C                   ENDIF
     C*
     C* MED# CAN'T BE BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMED#        ANDEQ     *BLANKS
     C                   SETON                                        7490
     C                   ENDIF
     C*
     C* NO DUPLICATES ALLOWED
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCFEIN        KKFEIN
     C                   MOVEL(P)  SCNAME        KKNAME
     C                   MOVEL(P)  SCMED#        KKMED#
     C     FMKY          CHAIN     PFM01                              99
     C     *IN99         DOWEQ     *OFF
     C     @@RRN         IFNE      DBRRN
     C                   SETON                                        7290
     C                   LEAVE
     C                   ENDIF
     C     @@MODE        IFEQ      'P'
     C                   SETON                                        7290
     C                   LEAVE
     C                   ENDIF
     C     FMKY          READE     PFM01                                  99
     C                   ENDDO
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
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFEIN'
     C                   CALL      'F0032R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANKS       ##FEIN            9
     C                   PARM      *BLANKS       ##MOD             3
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      *BLANKS       ##TYPE            1
     C                   PARM      *BLANKS       ##1N4             1                        nly
     C                   PARM      *BLANKS       ##1PRV            1                        fein/mod
     C                   PARM      2             ##PRIV            1 0
     C*
     C     ##FEIN        IFEQ      *BLANKS
     C                   SETON                                        3190
     C                   ELSE
     C                   MOVE      ##FEIN        SCFEIN
     C*** BEG ADD *** 09/07/04 ****************************************
     C                   MOVE      ##FEIN        KKFEIN
     C                   MOVE      ##MOD         KKMOD
     C     PDKY1         CHAIN     PPD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PDNAME        SCNAME
     C                   ENDIF
     C                   SETON                                        3390
     C*** END ADD *** 09/07/04 ****************************************
     C*** BEG DEL *** 09/07/04 ****************************************
     C******************** SETON                     3290
     C*** END DEL *** 09/07/04 ****************************************
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCNAME'
     C                   SETON                                        7590
     C*
     C     HFLD          WHENEQ    'SCMED#'
     C                   SETON                                        7690
     C*
     C     HFLD          WHENEQ    'SCZIP'
     C                   SETON                                        7790
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* MKSTHD - Creates the state header
     C*================================================================
     C     MKSTHD        BEGSR
     C*
     C* DISPLAY STATE CODE AND STATE NAME IN HEADER
     C*
     C                   MOVEL     @@ST          KKST
     C                   MOVEL     @@ST          SCST
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C                   MOVEL(P)  STNAME        SCSTNM
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   Z-ADD     *ZEROS        SCFEIN
     C                   MOVEL(P)  *BLANKS       SCNAME
     C                   MOVEL(P)  *BLANKS       SCMED#
     C                   Z-ADD     *ZEROS        SCZIP
     C                   MOVEL(P)  'A'           SCSTAT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     FMFEIN        SCFEIN
     C                   MOVEL(P)  FMNAME        SCNAME
     C                   MOVEL(P)  FMMED#        SCMED#
     C                   Z-ADD     FMZIP         SCZIP
     C*
     C     @@MODE        IFEQ      'P'
     C                   MOVEL(P)  'A'           SCSTAT
     C                   ELSE
     C                   MOVEL(P)  FMSTAT        SCSTAT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*** BEG ADD *** 11/14/02 ****************************************
     C                   SELECT
     C*
     C     SCST          WHENEQ    'NY'
     C                   MOVEL(P)  NYHD          SCHMED
     C*
     C     SCST          WHENEQ    'PA'
     C                   MOVEL(P)  PAHD          SCHMED
     C*
     C                   OTHER
     C*
     C                   MOVEL(P)  STHD          SCHMED
     C*
     C                   ENDSL
     C*** END ADD *** 11/14/02 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCFEIN        FMFEIN
     C                   MOVEL(P)  SCNAME        FMNAME
     C                   MOVEL(P)  SCMED#        FMMED#
     C                   Z-ADD     SCZIP         FMZIP
     C                   MOVEL(P)  SCSTAT        FMSTAT
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
     C                   MOVEA     '00000'       *IN(31)
     C*
     C     *INKJ         CASEQ     *ON           KJDO
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
     C*
     C     @@MODE        COMP      'A'                                    40
     C*
     C* Executing the sub-routine to make the state header.
     C*
     C                   EXSR      MKSTHD
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
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        FMUSER
     C                   MOVEL     Q1PGM         FMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FMDT
     C                   MOVEL     DSTIME        FMTIME
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
      *
     C                   MOVEL     Q1USER        FMIUSR
     C                   MOVEL     Q1USER        FMUSER
     C                   MOVEL     Q1PGM         FMIPGM
     C                   MOVEL     Q1PGM         FMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FMIDT
     C                   MOVEL     SRDTO         FMDT
     C                   MOVEL     DSTIME        FMITIM
     C                   MOVEL     DSTIME        FMTIME
      *
     C                   WRITE     DBRCDF
     C                   FEOD      FMFMP
     C                   ENDSR
