     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE F2556R01 - NY Provider Table Maintenance
     H*****************************************************************
     H* PROGRAM DESCRIPTION: NY Provider Table Maintenance            *
     H*                                                               *
     H* CALLED BY: F2556R                                             *
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H*****************************************************************
     H* Created 06/23/2016 by C:212 for Project:NY038                 *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
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
     FF2556F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMFMP     UF A E             DISK
     F                                     RENAME(PFM01:DBRCDF)
     FFMFML5    IF   E           K DISK
     F                                     INFDS(DBFDS)
     FSTABL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D strZIP          S              5
     D strZI4          S              4
     D strZIPF         S             42
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('NY Provider Table')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D*
     D*================================================================
     D*  Other constants
     D*================================================================
     D NYHD            C                   CONST('OpCert#          :')
     D PAHD            C                   CONST('Medicare#        :')
     D STHD            C                   CONST('Medicare#        :')
     D OPCFULL         C                   CONST('Full Opcert #    :')

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
     C                   KFLD                    KKNAME
     C                   KFLD                    KKMED#
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKEFFDTE
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
     C     *LIKE         DEFINE    FMEFDT        KKEFFDTE
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
     C* NAME CAN'T BE BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C     SCNAME        ANDEQ     *BLANKS
     C                   SETON                                        7390
     C                   ENDIF
     C*
     C* EFFECTIVE DATE CAN'T BE BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C                   IF        %char(SCEFFDATE) = *blanks or
     C                             SCEFFDATE = *loval
     C                   Eval      erEffDate = *On
     C                   Eval      msEffDate = 'MCR0503'
     C                   ENDIF
     C                   ENDIF
     C*
     C* ADDRESS CAN'T BE BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C     SCADR1        ANDEQ     *BLANKS
     C                   Eval      erAdr1 = *On
     C                   Eval      msAdr1 = 'MCR0503'
     C                   Eval      *in90 = *On
     C                   ENDIF
     C*
     C* MED# CAN'T BE BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMED#        ANDEQ     *BLANKS
     C                   SETON                                        7490
     C                   ENDIF
     C*
     C* FULL MED# CAN'T BE BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C     SCFullMed#    ANDEQ     *BLANKS
     C                   Eval      erFullMed# = *On
     C                   Eval      msFullMed# = 'MCR0503'
     C                   Eval      *in90 = *On
     C                   ENDIF
     C*
     C* NO DUPLICATES ALLOWED
     C*
     C     *IN90         IFEQ      *OFF
     C                   EVAL      KKEFFDTE = %dec(%char(SCEffDate:*cymd0):7:0)
     C                   Z-ADD     0             KKFEIN
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
     C***                Z-ADD     *ZEROS        SCFEIN
     C                   MOVEL(P)  *BLANKS       SCNAME
     C                   EVAL      SCADR1 = *blanks
     C                   MOVEL(P)  *BLANKS       SCMED#
     C                   MOVEL(P)  *BLANKS       SCFULLMED#
     C                   EVAL      SCZIP = *blanks
     C                   EVAL      SCZIP4 = *blanks
     C                   EVAL      vsZipHyp = *off
     C                   MOVEL(P)  'A'           SCSTAT
     C*
     C                   ELSE
     C*
     C***                Z-ADD     FMFEIN        SCFEIN
     C                   MOVEL(P)  FMNAME        SCNAME
     C                   EVAL      SCADR1 = FMADR1
     C                   MOVEL(P)  FMMED#        SCMED#
     C                   MOVEL(P)  FMOPCT        SCFULLMED#
     C                   MOVEL(P)  FMCTY         SCCTY
     C                   MOVEL(P)  FMST          SCST
     C                   EVAL      SCZIP = %editc(FMZIP:'X')
     C                   If        FMZIP4 <> *zeros
     C                   EVAL      SCZIP4 = %editc(FMZIP4:'X')
     C                   EVAL      vsZipHyp = *on
     C                   Else
     C                   EVAL      vsZipHyp = *off
     C                   EndIf
     C*
     C     @@MODE        IFEQ      'P'
     C                   MOVEL(P)  'A'           SCSTAT
     C                   ELSE
     C                   MOVEL(P)  FMSTAT        SCSTAT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
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
     C*
     C                   MOVEL(P)  OPCFULL       SCFULLHMED
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     0             FMFEIN
     C                   MOVEL(P)  SCNAME        FMNAME
     C                   EVAL      FMADR1 = SCADR1
     C                   MOVEL(P)  SCMED#        FMMED#
     C                   MOVEL(P)  SCCTY         FMCTY
     C                   MOVEL(P)  SCST          FMST
     C                   MOVEL(P)  SCFULLMED#    FMOPCT
     C                   EVAL      FMEFDT = %dec(%char(SCEffDate:*cymd0):7:0)
     C                   MONITOR
     c                   EVAL      FMZIP = %dec(SCZIP:5:0)
     C                   ON-ERROR
     c                   EVAL      FMZIP = *zeros
     C                   ENDMON
     C                   MONITOR
     C                   EVAL      FMZIP4 = %dec(SCZIP4:4:0)
     C                   EVAL      vsZipHyp = *on
     C                   ON-ERROR
     c                   EVAL      FMZIP4 = *zeros
     C                   EVAL      vsZipHyp = *off
     C                   ENDMON
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
