     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE F2534R01 - Mutually Exclusive Procedures Information
     H*****************************************************************
     H*                                                               *
     H* Called by: F2534R - Work With Mutually Exclusive Procedures   *
     H* Calls to : None                                               *
     H*                                                               *
     H*****************************************************************
     H* Created on 01/16/02 c:62 for Project 114MAO                   *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/15/03 C:79 Project: FS258                                  *
     H*               Add expanded CCI explaination fields            *
     H*                                                               *
     H* 01/16/02 C:79 Project: 114MAO                                 *
     H*               Add parm for positioning to service date        *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     FF2534F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMMUP     UF A E             DISK
     F                                     RENAME(PMU01:DBRCDF)
     FFMMUL1    IF   E           K DISK
     FFAEOP     IF   E             DISK
     F*** BEG ADD *** 04/15/03 ****************************************
     FFTCDL     IF   E           K DISK
     F*** END ADD *** 04/15/03 ****************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     D*** BEG DEL *** 04/15/03 ****************************************
     D************
     D************DS
     D************                            1 100 XXDESC
     D************                            1  40 SCDSCA
     D************                           41  80 SCDSCB
     D************                           81 100 SCDSCC
     D*** END DEL *** 04/15/03 ****************************************
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Mutually Exclusive P-
     D                                     rocedure Information')
     D COMAPI          C                   CONST('FA007R')
     D SLMSGF          C                   CONST('FSMESG')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*** BEG ADD *** 01/16/02 ****************************************
     C                   PARM                    @@RRN2            9 0          FAEOP
     C                   PARM                    @@FDAT            7 0
     C*** END ADD *** 01/16/02 ****************************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     MUKY          KLIST
     C                   KFLD                    KKCOD1
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKCOD2
     C*** BEG ADD *** 04/15/03 ****************************************
     C     CDKY          KLIST
     C                   KFLD                    KKKEY
     C*** END ADD *** 04/15/03 ****************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    MUCOD1        KKCOD1
     C     *LIKE         DEFINE    MUCOD2        KKCOD2
     C     *LIKE         DEFINE    MUEFDT        KKEFDT
     C*** BEG ADD *** 01/16/02 ****************************************
     C     *LIKE         DEFINE    EOCO#         ##CO#
     C     *LIKE         DEFINE    EOSUB#        ##SUB#
     C     *LIKE         DEFINE    EOFND         ##FND
     C     *LIKE         DEFINE    EOFYR         ##FYR
     C     *LIKE         DEFINE    EOCASE        ##CASE
     C     *LIKE         DEFINE    EOEOB#        ##EOB#
     C     *LIKE         DEFINE    EOFRDT        ##FDAT
     C*** END ADD *** 01/16/02 ****************************************
     C*** BEG ADD *** 04/15/03 ****************************************
     C     *LIKE         DEFINE    CDKEY         KKKEY
     C*** END ADD *** 04/15/03 ****************************************
     C*
     C/COPY SCOPYMT01C
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C*
     C     MUSTAT        IFEQ      'I'
     C                   MOVE      'A'           MUSTAT
     C                   ELSE
     C                   MOVE      'I'           MUSTAT
     C                   ENDIF                                                  MUSTAT
     C*
     C                   MOVEL     Q1USER        MUUSER
     C                   MOVEL     Q1PGM         MUPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MUDT
     C                   MOVEL     DSTIME        MUTIME
     C                   UPDATE    DBRCDF
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     D*===============================================================
     D* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     D*===============================================================
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C     *IN40         IFEQ      *ON
     C*
     C*  Code 1 must be entered
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCOD1        ANDEQ     *BLANKS
     C                   SETON                                        7190
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  Effective Date must be valid
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7290
     C                   ENDIF                                                  SRERR
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  Code 2 must be entered
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCOD2        ANDEQ     *BLANKS
     C                   SETON                                        7390
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  Duplicate records are not allowed
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL(P)  SCCOD1        KKCOD1
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C                   MOVEL(P)  SCCOD2        KKCOD2
     C*
     C     MUKY          CHAIN     PMU01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        7490
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  *IN90 = *OFF
     C                   ENDIF                                                  *IN40 = *ON
     C*
     C*  Deletion Date must be valid if entered
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDLDT        ANDNE     *ZEROS
     C                   Z-ADD     SCDLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDLDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7590
     C                   ENDIF                                                  SRERR
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  Description must be entered
     C*
     C     *IN90         IFEQ      *OFF
     C*** BEG CHG *** 04/15/03 ****************************************
     C********** SCDSCA    ANDEQ*BLANKS
     C     SCDESC        ANDEQ     *BLANKS
     C*** END CHG *** 04/15/03 ****************************************
     C                   SETON                                        7690
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C                   ENDSR                                                  ERRCK1
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
     C*** BEG ADD *** 01/16/02 ****************************************
     C*================================================================
     C* KQDO - F16=History
     C*================================================================
     C     KQDO          BEGSR
     C*
     C*  Call history pgm F0391R
     C*
     C                   CALL      'F0391R'
     C                   PARM      ' '           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      EOCO#         ##CO#
     C                   PARM      EOSUB#        ##SUB#
     C                   PARM      EOFND         ##FND
     C                   PARM      EOFYR         ##FYR
     C                   PARM      EOCASE        ##CASE
     C                   PARM      EOEOB#        ##EOB#
     C                   PARM                    ##FDAT
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** END ADD *** 01/16/02 ****************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL(P)  MUCOD1        SCCOD1
     C                   MOVEL(P)  MUCOD2        SCCOD2
     C*
     C                   Z-ADD     MUEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   Z-ADD     MUDLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDLDT
     C*
     C*** BEG DEL *** 04/15/03 ****************************************
     C******************** MOVELMUDESC    XXDESC    P
     C******************** MOVELMUPRCI    SCPRCI    P
     C*** END DEL *** 04/15/03 ****************************************
     C                   MOVEL(P)  MUGBMI        SCGBMI
     C                   MOVEL(P)  MUSTAT        SCSTAT
     C*** BEG ADD *** 04/15/03 ****************************************
     C                   MOVEL(P)  MUDESC        SCDESC
     C                   MOVEL(P)  MUDESC        KKKEY
     C*
     C     CDKY          CHAIN     PCD01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CDDSC2        SCDSC2
     C                   MOVEL(P)  CDDSC3        SCDSC3
     C                   MOVEL(P)  CDDSC4        SCDSC4
     C                   MOVEL(P)  CDDSC5        SCDSC5
     C                   MOVEL(P)  CDDSC6        SCDSC6
     C*
     C                   ENDIF
     C*
     C* GLOBAL MODIFIER INDICATOR DESCRIPTIONS
     C*
     C                   MOVEL(P)  MUGBMI        KKKEY
     C*
     C     CDKY          CHAIN     PCD01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CDDSC2        SCGMD1
     C                   MOVEL(P)  CDDSC3        SCGMD2
     C*
     C                   ENDIF
     C*** END ADD *** 04/15/03 ****************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  *BLANKS       SCCOD1
     C                   MOVEL(P)  *BLANKS       SCCOD2
     C                   Z-ADD     *ZEROS        SCEFDT
     C                   Z-ADD     *ZEROS        SCDLDT
     C*** BEG CHG *** 04/15/03 ****************************************
     C******************** MOVEL*BLANKS   XXDESC    P
     C                   MOVEL(P)  *BLANKS       SCDESC
     C                   MOVEL(P)  *BLANKS       SCDSC2
     C                   MOVEL(P)  *BLANKS       SCDSC3
     C                   MOVEL(P)  *BLANKS       SCDSC4
     C                   MOVEL(P)  *BLANKS       SCDSC5
     C                   MOVEL(P)  *BLANKS       SCDSC6
     C*** END CHG *** 04/15/03 ****************************************
     C*** BEG DEL *** 04/15/03 ****************************************
     C******************** MOVEL*BLANKS   SCPRCI    P
     C*** END DEL *** 04/15/03 ****************************************
     C                   MOVEL(P)  *BLANKS       SCGBMI
     C                   MOVEL(P)  'A'           SCSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  MVDBSC
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL(P)  SCCOD1        MUCOD1
     C                   MOVEL(P)  SCCOD2        MUCOD2
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MUEFDT
     C*
     C                   Z-ADD     SCDLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MUDLDT
     C*
     C*** BEG CHG *** 04/15/03 ****************************************
     C******************** MOVELXXDESC    MUDESC    P
     C                   MOVEL(P)  SCDESC        MUDESC
     C*** END END *** 04/15/03 ****************************************
     C*** BEG DEL *** 04/15/03 ****************************************
     C******************** MOVELSCPRCI    MUPRCI    P
     C*** END DEL *** 04/15/03 ****************************************
     C                   MOVEL(P)  SCGBMI        MUGBMI
     C                   MOVEL(P)  SCSTAT        MUSTAT
     C*
     C                   ENDSR                                                  MVSCDB
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR                                                  OTHERR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*** BEG ADD *** 01/16/02 ****************************************
     C     *INKQ         CASEQ     *ON           KQDO                                       s
     C                   ENDCS
     C*** END ADD *** 01/16/02 ****************************************
     C                   ENDSR                                                  OTHFKS
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR                                                  PGMEND
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C*** BEG ADD *** 01/16/02 ****************************************
     C     Q1PRMS        IFGT      4
     C*
     C                   SETON                                        45
     C*
     C     @@RRN2        CHAIN     PEO01                              99
     C*
     C                   Z-ADD     @@FDAT        ##FDAT
     C*
     C                   ELSE
     C                   SETOFF                                       45
     C*
     C                   ENDIF
     C*** END ADD *** 01/16/02 ****************************************
     C*
     C                   ENDSR                                                  PGMSTR
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
     C                   ENDSR                                                  PREOTH
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        MUUSER
     C                   MOVEL     Q1PGM         MUPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MUDT
     C*
     C                   MOVEL     DSTIME        MUTIME
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C*
     C                   ENDSR                                                  UPDATR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        MUUSER
     C                   MOVEL     Q1USER        MUIUSR
     C                   MOVEL     Q1PGM         MUPGM
     C                   MOVEL     Q1PGM         MUIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MUDT
     C                   MOVEL     SRDTO         MUIDT
     C*
     C                   MOVEL     DSTIME        MUTIME
     C                   MOVEL     DSTIME        MUITIM
     C                   WRITE     DBRCDF
     C                   FEOD      FMMUP
     C*
     C                   ENDSR                                                  WRITER
