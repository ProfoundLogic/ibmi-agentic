     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE F2345R01 - ZIP CODE/REGION INFORMATION
     H*
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  12/01/25 C:342 Suport 120189/SY458 release testing           *
     H*                DSPF Changes:                                  *
     H*                Added read only indicator on Area field.       *
     H*                                                               *
     H*  09/25/03 C:79 PROJECT:TX020                                  *
     H*                ADD AREA TO SCREEN - RURAL/URBAN, ETC.         *
     H*                                                               *
     H*  05/05/00 C:62 PROJECT:FSMNT                                  *
     H*                CHANGE OPTNM.                                  *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF2345F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMZRP     UF A E             DISK
     F                                     RENAME(PZR01:DBRCDF)
     FFMZRL     IF   E           K DISK
     FSTABL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D                 DS
     D  ##TBID                 1      4
     D  XXST                   1      2
     D  XXRG                   3      4
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*** BEGIN CHG *** 05/05/00 **************************************
     D**********    'Zip code/Region -    C         OPTNM
     D**********    'Maintenance'
     D OPTNM           C                   CONST('Zip code/Region -
     D                                     Information')
     D*** END   CHG *** 05/05/00 **************************************
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
     C* SEARCH PARM LIST
     C*
     C     ACDPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     ZRKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFZCD
     C                   KFLD                    KKEFDT
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##TBID
     C                   KFLD                    KKGKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    ZRCO#         ##CO#
     C     *LIKE         DEFINE    ZRFZCD        KKFZCD
     C     *LIKE         DEFINE    ZREFDT        KKEFDT
     C     *LIKE         DEFINE    STKEY         KKST
     C     *LIKE         DEFINE    GTKEY         KKGKEY
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
     C     ZRSTAT        IFEQ      'I'
     C                   MOVE      'A'           ZRSTAT
     C                   ELSE
     C                   MOVE      'I'           ZRSTAT
     C                   ENDIF                                                  ZRSTAT
     C*
     C                   MOVEL     Q1USER        ZRUSER
     C                   MOVEL     Q1PGM         ZRPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZRDT
     C*
     C                   MOVEL     DSTIME        ZRTIME
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
     C*  THE FROM ZIP CODE CANNOT BE ZEROS
     C*
     C   40
     CANN90SCFZCD        IFEQ      *ZEROS
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C*  THE TO ZIP CODE CANNOT BE ZEROS
     C*
     C   40
     CANN90SCTZCD        IFEQ      *ZEROS
     C*
     C                   SETON                                        7290
     C*
     C                   ENDIF
     C*
     C* THE TO ZIP CODE HAS TO BE GREATER THAN THE FROM ZIP CODE
     C*
     C   40
     CANN90SCTZCD        IFLT      SCFZCD
     C*
     C                   SETON                                        7390
     C*
     C                   ENDIF
     C*
     C*  THE EFF. DATE MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        7490
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* CHECK TO BE SURE ZIP CODE/REGION HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCFZCD        KKFZCD
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C   40
     CANN90ZRKY          CHAIN     PZR01                              99
     C   40
     CANN90
     CANN99              SETON                                        75  90
     C*
     C*  THE REGION CODE CANNOT BE ZEROS
     C*
     C  N90SCRGN#        IFEQ      *ZEROS
     C*
     C                   SETON                                        7690
     C*
     C                   ENDIF
     C*
     C*  THE REGION CODE MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL     @@ST          XXST
     C                   MOVEL     'RG'          XXRG
     C                   MOVEL(P)  SCRGN#        KKGKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C   99              SETON                                        7790
     C*
     C                   ENDIF
     C*** BEG ADD *** 09/25/03 ****************************************
     C*
     C*  THE AREA MUST BE VALID IF ENTERED
     C*
     C     *IN90         IFEQ      *OFF
     C     SCAREA        ANDNE     *BLANKS
     C     *IN90         OREQ      *OFF
     C     *IN21         ANDEQ     *ON
     C*
     C                   MOVE      'ZRAR'        ##TBID
     C                   MOVEL(P)  SCAREA        KKGKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C   99              SETON                                        7990
     C*
     C                   ENDIF
     C*** BEG ADD *** 09/25/03 ****************************************
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90ZRDT          IFGT      SAVEDT
     C     ZRDT          OREQ      SAVEDT
     C     ZRTIME        ANDGT     SAVETM
     C                   SETON                                        9078
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C* REGION SEARCH
     C*
     C     HFLD          WHENEQ    'SCRGN#'
     C*
     C                   MOVEL     @@ST          XXST
     C                   MOVEL     'RG'          XXRG
     C*
     C                   CALL      'F2380R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        81
     C*
     C                   ELSE
     C*** BEG ADD *** 09/25/03 ****************************************
     C                   SETON                                        82
     C*** END ADD *** 09/25/03 ****************************************
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTKEY         SCRGN#
     C*
     C                   ENDIF
     C*** BEG ADD *** 09/25/03 ****************************************
     C*
     C* REGION SEARCH
     C*
     C     HFLD          WHENEQ    'SCAREA'
     C*
     C                   MOVEL     'ZRAR'        ##TBID
     C*
     C                   CALL      'F2380R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        82
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTKEY         SCAREA
     C                   MOVEL     GTDESC        SCARDS
     C*
     C                   ENDIF
     C*** END ADD *** 09/25/03 ****************************************
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
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
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVE      ZRFZCD        SCFZCD
     C                   MOVE      ZRTZCD        SCTZCD
     C*
     C                   Z-ADD     ZREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVE      ZRRGN#        SCRGN#
     C                   MOVEL     ZRSTAT        SCSTAT
     C*** BEG ADD *** 09/25/03 ****************************************
     C                   MOVE      ZRAREA        SCAREA
     C*** END ADD *** 09/25/03 ****************************************
     C*
     C                   ELSE
     C*
     C                   MOVE      *ZEROS        SCFZCD
     C                   MOVE      *ZEROS        SCTZCD
     C                   MOVE      *ZEROS        SCEFDT
     C                   MOVE      *ZEROS        SCRGN#
     C*** BEG ADD *** 09/25/03 ****************************************
     C                   MOVE      *BLANK        SCAREA
     C*** END ADD *** 09/25/03 ****************************************
     C                   MOVEL     'A'           SCSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         ZRCO#
     C                   MOVE      SCFZCD        ZRFZCD
     C                   MOVE      SCTZCD        ZRTZCD
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZREFDT
     C*
     C                   MOVE      SCRGN#        ZRRGN#
     C                   MOVEL     SCSTAT        ZRSTAT
     C*** BEG ADD *** 09/25/03 ****************************************
     C                   MOVE      SCAREA        ZRAREA
     C*** END ADD *** 09/25/03 ****************************************
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
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C* Executing the sub-routine to make the state header.
     C*
     C                   EXSR      MKSTHD
     C*** BEG ADD *** 09/25/03 ****************************************
     C                   SELECT
     C     SCST          WHENEQ    'TX'
     C                   SETON                                        21
     C                   ENDSL
     C*** END ADD *** 09/25/03 ****************************************
     C*
     C                   ENDSR                                                  PGMSTR
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
     C                   MOVEL     Q1USER        ZRUSER
     C                   MOVEL     Q1PGM         ZRPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZRDT
     C*
     C                   MOVEL     DSTIME        ZRTIME
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
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        ZRUSER
     C                   MOVEL     Q1PGM         ZRPGM
     C                   MOVEL     Q1USER        ZRIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZRDT
     C*
     C                   MOVEL     SRDTO         ZRIDT
     C*
     C                   MOVEL     DSTIME        ZRTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FMZRP
     C*
     C                   ENDSR
