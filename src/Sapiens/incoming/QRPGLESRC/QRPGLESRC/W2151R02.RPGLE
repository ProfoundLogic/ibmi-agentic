     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 06/20/07 C:53  PROJECT: 114WMF  BACK TO BASE FOR:
     H* 06/11/01 C:15  Project: 114WBY                                *
     H*               -Added "CW uasge only" field.                   *
     H*               -Chgd edits to only require the Lost Time entry *
     H*                if the CW usage field is blank.                *
     H*****************************************************************
     H*
     FW2151F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWTB7P     UF A E             DISK
     F                                     RENAME(PB701:DBRCDF)
     FWTABL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Payment Value 2 -
     D                                     Master Information')
     D COMAPI          C                   CONST('WA001R')
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
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     B7KY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##KEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    B7CO#         ##CO#
     C     *LIKE         DEFINE    B7KEY         ##KEY
     C     *LIKE         DEFINE    B7KEY         @@KEY
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
     C     B7STAT        IFEQ      'I'
     C                   MOVE      'A'           B7STAT
     C                   ELSE
     C                   MOVE      'I'           B7STAT
     C                   ENDIF                                                  B7STAT
     C*
     C                   MOVEL     Q1USER        B7USER
     C                   MOVEL     Q1PGM         B7PGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         B7DT
     C*
     C                   MOVEL     DSTIME        B7TIME
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
     C* BR-7 CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCKEY         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE BR-7 CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCKEY         ##KEY
     C*
     C   40
     CANN90B7KY          CHAIN     PB701                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* BR-2 CODE DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C*  LOST TIME MUST BE 'Y' OR 'N'.
     C*
     C  N90SCLTLF        IFNE      'Y'
     C     SCLTLF        ANDNE     'N'
     C*
     C*** Begin add *** 06/11/01 **************************************
     C     SCLTLF        IFNE      *BLANKS
     C     SCRSTR        OREQ      *BLANKS
     C*** End add ***** 06/11/01 **************************************
     C*
     C                   SETON                                        7990
     C*
     C*** Begin add *** 06/11/01 **************************************
     C                   ENDIF
     C*** End add ***** 06/11/01 **************************************
     C*
     C                   ENDIF
     C*
     C*** Begin add *** 06/11/01 **************************************
     C  N90SCRSTR        IFNE      *BLANKS
     C     SCRSTR        ANDNE     'Y'
     C     SCRSTR        ANDNE     'N'
     C                   SETON                                        8090
     C                   ENDIF
     C*** End add ***** 06/11/01 **************************************
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90B7DT          IFGT      SAVEDT
     C     B7DT          OREQ      SAVEDT
     C     B7TIME        ANDGT     SAVETM
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
     C                   MOVEL     B7KEY         SCKEY
     C                   MOVEL     B7DESC        SCDESC
     C                   MOVEL     B7LTLF        SCLTLF
     C                   MOVEL     B7STAT        SCSTAT
     C*** Begin add *** 06/11/01 **************************************
     C                   MOVEL     B7RSTR        SCRSTR
     C*** End add ***** 06/11/01 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCKEY
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCLTLF
     C                   MOVEL     'A'           SCSTAT
     C*** Begin add *** 06/11/01 **************************************
     C                   MOVEL     *BLANKS       SCRSTR
     C*** End add ***** 06/11/01 **************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         B7CO#
     C                   MOVEL     SCKEY         B7KEY
     C                   MOVEL     SCDESC        B7DESC
     C                   MOVEL     SCLTLF        B7LTLF
     C                   MOVEL     SCSTAT        B7STAT
     C*** Begin add *** 06/11/01 **************************************
     C                   MOVEL     SCRSTR        B7RSTR
     C*** End add ***** 06/11/01 **************************************
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
     C                   MOVEL     'N'           B7STFL
     C                   MOVEL     Q1USER        B7USER
     C                   MOVEL     Q1PGM         B7PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         B7DT
     C*
     C                   MOVEL     DSTIME        B7TIME
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
     C                   MOVEL     'N'           B7STFL
     C                   MOVEL     Q1USER        B7USER
     C                   MOVEL     Q1PGM         B7PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         B7DT
     C*
     C                   MOVEL     DSTIME        B7TIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WTB7P
     C*
     C                   ENDSR
