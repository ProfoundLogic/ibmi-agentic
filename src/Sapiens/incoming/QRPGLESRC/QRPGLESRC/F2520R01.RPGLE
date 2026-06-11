     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W2520R01 - DRG PROSPECTIVE PAYMENT SYSTEM TABLE
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* MAINTENANCE PROGRAM FOR DRG PROSPECTIVE PAYMENT SYSTEM TABLE  *
     H*                                                               *
     H* CALLED BY: F2520R                                              *
     H*                                                               *
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H* CREATED: 06/11/2001                                           *
     H* 04/04/18 C:180 PROJECT PA085                                  *
     H*                Protect screen fields on Display Mode.         *
     H*                                                               *
     H* 06/11/01 C:136 PROJECT 114MCG                                 *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/18/08 C:198 Project: PA046                                 *
     H*                Added NPI# after Medicare#.                    *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  40       ADD MODE                                            *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  71       SCMED# CAN NOT BE BLANK                             *
     H*  72       SCMED# MUST BE UNIQUE                               *
     H*  73       SCSTAT MUST BE 'A','I', OR ' '.                     *
     H*  74       SCEFDT CAN NOT BE BLANK                             *
     H*  75       SCEFDT MUST BE VALID                                *
     H*                                                               *
     H*****************************************************************
     H*
     FF2520F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFAPOPPA   UF A E             DISK
     F                                     RENAME(PPO01:DBRCDF)
     FSTABL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('DRG Prospective Paym-
     D                                     ent System')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

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
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKMED#
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST             10
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    POMED#        KKMED#
     C     *LIKE         DEFINE    POEFDT        KKEFDT
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
     C     POSTAT        IFEQ      'I'
     C                   MOVE      'A'           POSTAT
     C                   ELSE
     C                   MOVE      'I'           POSTAT
     C                   ENDIF                                                  POSTAT
     C*
     C                   MOVEL     Q1USER        POUSER
     C                   MOVEL     Q1PGM         POPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PODT
     C*
     C                   MOVEL     DSTIME        POTIME
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
     C     *IN90         IFEQ      *OFF
     C     SCMED#        ANDEQ     *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSTAT        ANDNE     'A'
     C     SCSTAT        ANDNE     'I'
     C     SCSTAT        ANDNE     ' '
     C                   SETON                                        73  90
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCEFDT        ANDEQ     *ZEROS
     C                   SETON                                        74  90
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCEFDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        75  90
     C                   ENDIF
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
      *
      * DISPLAY STATE CODE AND STATE NAME IN HEADER
      *
     C                   MOVEL     @@ST          KKST
     C                   MOVEL     @@ST          SCST
      *
     C     STKY          CHAIN     PST01                              99
      *
     C                   MOVEL(P)  STNAME        SCSTNM
      *
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     POMED#        SCMED#
     C*** BEG ADD *** 11/18/08 ************************************************
     C                   MOVEL     PONPI#        SCNPI#
     C*** END ADD *** 11/18/08 ************************************************
     C*
     C                   Z-ADD     POEFDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   Z-ADD     PODRRT        SCDRRT
     C                   Z-ADD     POCPRT        SCCPRT
     C                   Z-ADD     POHHRT        SCHHRT
     C                   Z-ADD     POINAD        SCINAD
     C                   Z-ADD     PODIAD        SCDIAD
     C                   Z-ADD     PODSAD        SCDSAD
     C                   Z-ADD     POPMAD        SCPMAD
     C                   Z-ADD     POARCC        SCARCC
     C*
     C     @@MODE        IFEQ      'A'                                          ADD MODE
     C                   MOVE(P)   'A'           SCSTAT                         ACTIVE STATUS
     C                   ELSE
     C                   MOVEL(P)  POSTAT        SCSTAT
     C                   ENDIF                                                  @@MODE = A
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCMED#        POMED#
     C*** BEG ADD *** 11/18/08 ************************************************
     C                   MOVEL     SCNPI#        PONPI#
     C*** END ADD *** 11/18/08 ************************************************
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         POEFDT
     C*
     C                   Z-ADD     SCDRRT        PODRRT
     C                   Z-ADD     SCCPRT        POCPRT
     C                   Z-ADD     SCHHRT        POHHRT
     C                   Z-ADD     SCINAD        POINAD
     C                   Z-ADD     SCDIAD        PODIAD
     C                   Z-ADD     SCDSAD        PODSAD
     C                   Z-ADD     SCPMAD        POPMAD
     C                   Z-ADD     SCARCC        POARCC
     C                   MOVEL     SCSTAT        POSTAT
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
     C*
     C     @@MODE        COMP      'A'                                    40
      /Free
       //*** Begin Add 04/04/18 ***************************************
        *in09 = @@Mode = 'D';
       //*** End   Add 04/04/18 ***************************************
      /End-Free
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
      *
     C                   MOVEL     Q1USER        POUSER
     C                   MOVEL     Q1PGM         POPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PODT
     C                   MOVEL     DSTIME        POTIME
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
     C                   MOVEL     Q1USER        POIUSR
     C                   MOVEL     Q1USER        POUSER
     C                   MOVEL     Q1PGM         POIPGM
     C                   MOVEL     Q1PGM         POPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         POIDT
     C                   MOVEL     SRDTO         PODT
     C                   MOVEL     DSTIME        POITIM
     C                   MOVEL     DSTIME        POTIME
      *
     C                   WRITE     DBRCDF
     C                   FEOD      FAPOPPA
     C                   ENDSR
