     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: 168        PROJECT: 93WFP  *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  04/27/12 C:279 SUPPORT: WA099 79746                          *
     H*                 MODIFIED TO PROTECT CONTACT TYPE IN CHANGE    *
     H*                 MODE.                                         *
     H*                                                               *
     H*  04/17/12 C:136 SUPPORT: WA099 79746                          *
     H*                 FIX SORTING                                   *
     H*                                                               *
     H*  01/13/05 C:168  PROJECT: 93WFP                               *
     H*                  Initial creation, Maintain the Agent Contact *
     H*                  type                                         *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     F*****************************************************************
     FW2287F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWTAYP     UF A E             DISK
     F                                     RENAME(PAY01:DBRCDF)
     FWTAYL1    IF   E           K DISK
     F                                     RENAME(PAY01:DBRCDF1)
     FWTAYL2    IF   E           K DISK
     F                                     RENAME(PAY01:DBRCDF2)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Agent Contact Type')
     D #OFSCR          C                   CONST(1)
     D COMAPI          C                   CONST('WA001R')
     D XXPRDS          C                   CONST('Agent Contact Type')

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
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     AYKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##TYPE
     C*
     C     AYKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##DESC
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    AYCO#         ##CO#
     C     *LIKE         DEFINE    AYTYPE        ##TYPE
     C     *LIKE         DEFINE    AYDESC        ##DESC
      *
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
     C     AYSTAT        IFEQ      'I'
     C                   MOVE      'A'           AYSTAT
     C                   ELSE
     C                   MOVE      'I'           AYSTAT
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        AYUSER
     C                   MOVEL     Q1PGM         AYPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AYDT
     C*
     C                   MOVEL     DSTIME        AYTIME
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
     C* CONTACT TYPE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCTYPE        IFEQ      *BLANKS
     C                   SETON                                        73  90
      *
     C                   ELSE
     C******** Beg Chg ****** 04/27/12 ********************************
     C********             MOVELSCDESC    ##TYPE
     C                   MOVEL     SCTYPE        ##TYPE
     C******** End Chg ****** 04/27/12 ********************************
     C     AYKY1         CHAIN     DBRCDF1                            99
     C   40
     CANN90
     CANN99              SETON                                        74  90
     C                   ENDIF
      *
     C*
     C* DESCRIPTION MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCDESC        ##DESC
     C******** Beg Chg ****** 04/27/12 ********************************
     C*  Modified to check duplicate description even in edit mode.
     C***40N90** AYKY2     CHAINDBRCDF2              99
     C***40N90N99          SETON                     72  90
     C  N90AYKY2         CHAIN     DBRCDF2                            99
     C  N90
     CANN99              SETON                                        72  90
     C******** End Chg ****** 04/27/12 ********************************
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90AYDT          IFGT      SAVEDT
     C     AYDT          OREQ      SAVEDT
     C     AYTIME        ANDGT     SAVETM
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
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     AYTYPE        SCTYPE
     C                   MOVEL     AYDESC        SCDESC
     C                   MOVEL     AYSTAT        SCSTAT
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       SCTYPE
     C                   MOVE      *BLANKS       SCDESC
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
     C                   Z-ADD     ##CO#         AYCO#
     C                   MOVEL     SCTYPE        AYTYPE
     C                   MOVEL     SCDESC        AYDESC
     C                   MOVEL     SCSTAT        AYSTAT
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
     C                   SETON                                        LR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C******** Beg Add ****** 04/27/12 ********************************
     C                   MOVE      *OFF          *IN51
     C     @@MODE        IFEQ      'C'
     C     *IN09         OREQ      *ON
     C                   MOVE      *ON           *IN51
     C                   ENDIF
     C******** End Add ****** 04/27/12 ********************************
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        AYUSER
     C                   MOVEL     Q1PGM         AYPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AYDT
     C                   MOVEL     DSTIME        AYTIME
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        AYUSER
     C                   MOVEL     Q1PGM         AYPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AYDT
     C                   MOVEL     DSTIME        AYTIME
     C                   WRITE     DBRCDF
     C                   FEOD      WTAYP
     C                   ENDSR
