     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F2420R01 - SUBSCRIBER STAFF CODE MAINTENANCE  C:62 12/29/97  *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*               F2420R - WORK WITH SUBSCRIBER STAFF CODES       *
     H*  CALLS TO:                                                    *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  11/23/99 C:62  ADD SUBSCRIBER TO SCREEN.                     *
     H*                                                               *
     H*  04/13/99 C:56  CHANGE  COMPANY API TO FA007R PER MIKE FLOWER *
     H*                 FA011R WAS A DUPLICATE OF FA007R              *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF2420F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMSFP     UF A E             DISK
     F                                     RENAME(PSF01:DBRCDF)
     FFMSFL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     F*** BEGIN ADD *** 11/23/99 **************************************
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
     F*** END   ADD *** 11/23/99 **************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Subscriber Staff-
     D                                      Code Maintenance')
     D*** BEG CHG *** 04/13/99 ****************************************
     D**********    'FA011R'              C         COMAPI
     D COMAPI          C                   CONST('FA007R')
     D*** END CHG *** 04/13/99 ****************************************
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
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     SFKY          KLIST
     C                   KFLD                    ##CO#
     C*** BEGIN CHG *** 11/23/99 **************************************
     C**********           KFLD           ##SUB#
     C                   KFLD                    KKSUB#
     C*** END   CHG *** 11/23/99 **************************************
     C                   KFLD                    KKSTAF
     C*
     C     SMKY          KLIST
     C                   KFLD                    ##CO#
     C*** BEGIN CHG *** 11/23/99 **************************************
     C**********           KFLD           ##SUB#
     C                   KFLD                    KKSUB#
     C*** END   CHG *** 11/23/99 **************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C*** BEGIN ADD *** 11/23/99 **************************************
     C     *LIKE         DEFINE    SFSUB#        KKSUB#
     C*** END   ADD *** 11/23/99 **************************************
     C     *LIKE         DEFINE    SFSTAF        KKSTAF
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
     C     SFSTAT        IFEQ      'I'
     C                   MOVE      'A'           SFSTAT
     C                   ELSE
     C                   MOVE      'I'           SFSTAT
     C                   ENDIF                                                  SFSTAT
     C*
     C                   MOVEL     Q1USER        SFUSER
     C                   MOVEL     Q1PGM         SFPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SFDT
     C*
     C                   MOVEL     DSTIME        SFTIME
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
     C*** BEGIN ADD *** 11/23/99 **************************************
     C*
     C* SUBSCRIBER MUST BE VALID (IF IN ADD MODE)
     C*
     C                   Z-ADD     SCSUB#        KKSUB#
     C*
     C   40
     CANN90SMKY          CHAIN     PSM01                              99
     C   99              SETON                                        74  90
     C  N99              MOVEL(P)  SMNAME        SCSBNM
     C*** END   ADD *** 11/23/99 **************************************
     C*
     C* STAFF CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCSTAF        IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCZIPA
     C*
     C* CHECK TO BE SURE STAFF CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCSTAF        KKSTAF
     C*
     C   40
     CANN90SFKY          CHAIN     PSF01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCNAME        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90SFDT          IFGT      SAVEDT
     C     SFDT          OREQ      SAVEDT
     C     SFTIME        ANDGT     SAVETM
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
     C*** BEGIN ADD *** 11/23/99 **************************************
     C*================================================================
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C     HFLD          IFEQ      'SCSUB#'
     C*
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      '1'           OUTFLG
     C*                    SETON                     90
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PSM02                              99
     C                   Z-ADD     SMSUB#        SCSUB#
     C                   MOVEL(P)  SMNAME        SCSBNM
     C                   SETON                                        9031
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** END   ADD *** 11/23/99 **************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*** BEGIN ADD *** 11/23/99 **************************************
     C*
     C                   Z-ADD     SFSUB#        SCSUB#
     C                   Z-ADD     SFSUB#        KKSUB#
     C*
     C     SMKY          CHAIN     PSM01                              99
     C*
     C   99              MOVEL     *BLANKS       SCSBNM
     C  N99              MOVEL(P)  SMNAME        SCSBNM
     C*** END   ADD *** 11/23/99 **************************************
     C                   MOVEL     SFSTAF        SCSTAF
     C                   MOVEL     SFNAME        SCNAME
     C                   MOVEL     SFSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C*** BEGIN ADD *** 11/23/99 **************************************
     C                   Z-ADD     *ZEROS        SCSUB#
     C                   MOVEL     *BLANKS       SCSBNM
     C*** END   ADD *** 11/23/99 **************************************
     C                   MOVEL     *BLANKS       SCSTAF
     C                   MOVEL     *BLANKS       SCNAME
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
     C                   MOVE      ##CO#         SFCO#
     C*** BEGIN CHG *** 11/23/99 **************************************
     C**********           MOVE ##SUB#    SFSUB#
     C                   MOVE      SCSUB#        SFSUB#
     C*** END   CHG *** 11/23/99 **************************************
     C                   MOVEL     SCSTAF        SFSTAF
     C                   MOVEL     SCNAME        SFNAME
     C                   MOVEL     SCSTAT        SFSTAT
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
     C*** BEGIN ADD *** 11/23/99 **************************************
     C                   SETOFF                                       31
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*** END   ADD *** 11/23/99 **************************************
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
     C*** BEGIN DEL *** 11/23/99 **************************************
     C********** SMKY      CHAINPSM01                99
     C*
     C**********           MOVELSMNAME    ##DATA    P
     C*
     C**********           CALL 'SRCENTER'
     C**********           PARM           ##DATA 80
     C**********           PARM 40        ##LENG  30
     C*
     C**********           MOVEL##DATA    SCSBNM
     C*** END   DEL *** 11/23/99 **************************************
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
     C                   MOVEL     Q1USER        SFUSER
     C                   MOVEL     Q1PGM         SFPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SFDT
     C*
     C                   MOVEL     DSTIME        SFTIME
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
     C                   MOVEL     Q1USER        SFUSER
     C                   MOVEL     Q1PGM         SFPGM
     C                   MOVEL     Q1USER        SFIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SFDT
     C*
     C                   MOVEL     SRDTO         SFIDT
     C*
     C                   MOVEL     DSTIME        SFTIME
     C                   MOVEL     DSTIME        SFITIM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FMSFP
     C*
     C                   ENDSR
