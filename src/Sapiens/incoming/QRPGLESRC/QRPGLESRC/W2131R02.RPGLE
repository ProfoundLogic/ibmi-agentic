     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H/TITLE W2131R02
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/13/99  C:118  PROJECT:  WC999                              *
     H*                  Modified F10=Search in Add mode to allow     *
     H*                  search in EDI code field only.               *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  20 - Error message indicating that F10=Search is not allowed *
     H*       in this field. (WCR1058)
     H*                                                               *
     H*  81 - INDICATOR TO POSITION CURSOR IN EDI CODE FIELD.
     H*
     H*****************************************************************
     FW2131F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWTCAP     UF A E             DISK
     F                                     RENAME(PCA01:DBRCDF)
     FWTABL     IF   E           K DISK
     F*
     F*  Generic table file to validate EDI code
     FWTGTL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Cause Code Master In-
     D                                     formation')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
     D XXPRDS          C                   CONST('Cause Code EDI-
     D                                      Information')

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
     C     CAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##KEY
     C*
     C*    Generic table -- for EDI nature code
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXTKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CACO#         ##CO#
     C     *LIKE         DEFINE    CAKEY         ##KEY
     C     *LIKE         DEFINE    CAKEY         @@KEY
     C*
     C     *LIKE         DEFINE    GTKEY         XXTKEY
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         QQTKEY
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
     C     CASTAT        IFEQ      'I'
     C                   MOVE      'A'           CASTAT
     C                   ELSE
     C                   MOVE      'I'           CASTAT
     C                   ENDIF                                                  CASTAT
     C*
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C*
     C                   MOVEL     DSTIME        CATIME
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
     C* CAUSE CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCKEY         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE CAUSE CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCKEY         ##KEY
     C*
     C   40
     CANN90CAKY          CHAIN     PCA01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* CAUSE CODE DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90CADT          IFGT      SAVEDT
     C     CADT          OREQ      SAVEDT
     C     CATIME        ANDGT     SAVETM
     C                   SETON                                        9078
     C                   ENDIF
     C*
     C* EDI CODE CANNOT BE BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C     SCEDIC        ANDEQ     *BLANKS
     C                   SETON                                        74  90
     C                   ENDIF                                                  IN90/SCEDIC
     C*
     C* VALIDATE EDI CODE
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL     'CAUS'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     SCEDIC        XXTKEY
     C     GTKY          CHAIN     PGT01                              75
     C     *IN75         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN75   On
     C                   ENDIF                                                  *IN90   Off
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
     C                   MOVEL     CAKEY         SCKEY
     C                   MOVEL     CADESC        SCDESC
     C                   MOVEL     CASTAT        SCSTAT
     C                   MOVEL     CAEDIC        SCEDIC
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCKEY
     C                   MOVE      *BLANKS       SCDESC
     C***** BEGIN ADD **** 04/13/99 ********************************
     C                   MOVE      *BLANKS       SCEDIC
     C***** END ADD ****** 04/13/99 ********************************
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
     C                   Z-ADD     ##CO#         CACO#
     C                   MOVEL     SCKEY         CAKEY
     C                   MOVEL     SCDESC        CADESC
     C                   MOVEL     SCSTAT        CASTAT
     C                   MOVEL     SCEDIC        CAEDIC
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
     C     *INKJ         IFEQ      *ON
     C                   MOVE      *OFF          *IN90
     C                   MOVEA     '00000'       *IN(71)
     C*
     C***** BEGIN ADD **** 04/13/99 *******************************
     C     HFLD          IFEQ      'SCEDIC'
     C***** END ADD ****** 04/13/99 ******************************
     C                   MOVEL     XXPRDS        QQPRDS           40
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      ' '           QQCALL            1
     C                   PARM      'CAUS'        QQTBID            4
     C                   PARM                    QQTKEY
     C                   PARM      ' '           QQRTN             1
     C*
     C     QQRTN         IFEQ      '*'
     C                   MOVEL     QQTKEY        SCEDIC
     C***** BEGIN ADD **** 04/13/99 ***********************************
     C                   ELSE
     C                   MOVE      *ON           *IN81
     C***** END ADD ****** 04/13/99 ***********************************
     C                   ENDIF
     C*
     C***** BEGIN ADD **** 04/13/99 ***********************************
     C                   ELSE
     C                   MOVE      *ON           *IN20
     C***** END ADD ****** 04/13/99 ***********************************
     C                   ENDIF
     C*
     C***** BEGIN ADD **** 04/13/99 ***********************************
     C                   MOVE      *ON           *IN90
     C***** END ADD ****** 04/13/99 *******************************
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*
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
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C*
     C                   MOVEL     DSTIME        CATIME
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
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C*
     C                   MOVEL     DSTIME        CATIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WTCAP
     C*
     C                   ENDSR
