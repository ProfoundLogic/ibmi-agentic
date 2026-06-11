     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      * ***************************************************************
      * REVISIONS:                                                    *
      *
      * 05/20/08  C:53 Project: CI054                                  *
      *                Add field for OSHA form                         *
      *
     H*
     FW2121F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWTNAP     UF A E             DISK
     F                                     RENAME(PNA01:DBRCDF)
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
     D OPTNM           C                   CONST('Nature Code Master-
     D                                      Information')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
     D XXPRDS          C                   CONST('Nature Code EDI-
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
     C     NAKY          KLIST
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
     C     *LIKE         DEFINE    NACO#         ##CO#
     C     *LIKE         DEFINE    NAKEY         ##KEY
     C     *LIKE         DEFINE    NAKEY         @@KEY
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
     C     NASTAT        IFEQ      'I'
     C                   MOVE      'A'           NASTAT
     C                   ELSE
     C                   MOVE      'I'           NASTAT
     C                   ENDIF                                                  NASTAT
     C*
     C                   MOVEL     Q1USER        NAUSER
     C                   MOVEL     Q1PGM         NAPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NADT
     C*
     C                   MOVEL     DSTIME        NATIME
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
     C* NATURE CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCKEY         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE NATURE CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCKEY         ##KEY
     C*
     C   40
     CANN90NAKY          CHAIN     PNA01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* NATURE CODE DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90NADT          IFGT      SAVEDT
     C     NADT          OREQ      SAVEDT
     C     NATIME        ANDGT     SAVETM
     C                   SETON                                        9078
     C                   ENDIF
     C*
     C* EDI CODE CANNOT BE BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C     SCEDIN        ANDEQ     *BLANKS
     C                   SETON                                        74  90
     C                   ENDIF                                                  IN90/SCEDIC
     C*
     C* VALIDATE EDI CODE
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL     'NATR'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     SCEDIN        XXTKEY
     C     GTKY          CHAIN     PGT01                              75
     C     *IN75         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN75   On
     C                   ENDIF                                                  *IN90   Off
     C*
      *** BEG ADD *** 05/20/08 ****************************
     C*
     C* VALIDATE OSHA CODE
     C*
     C     *IN90         IFEQ      *OFF
     C     SCOSHA        ANDNE     *BLANKS
     C                   MOVEL     'NATO'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     SCOSHA        XXTKEY
     C     GTKY          CHAIN     PGT01                              75
     C     *IN75         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN76   On
     C                   ENDIF                                                  *IN90   Off
     C*
      *** END ADD *** 05/20/08 ****************************
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
     C                   MOVEL     NAKEY         SCKEY
     C                   MOVEL     NADESC        SCDESC
     C                   MOVEL     NASTAT        SCSTAT
     C                   MOVEL     NAEDIN        SCEDIN
      *** BEG ADD *** 05/20/08 ****************************
     C                   MOVE      NAOSHA        SCOSHA
      *** END ADD *** 05/20/08 ****************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCKEY
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
     C                   Z-ADD     ##CO#         NACO#
     C                   MOVEL     SCKEY         NAKEY
     C                   MOVEL     SCDESC        NADESC
     C                   MOVEL     SCSTAT        NASTAT
     C                   MOVEL     SCEDIN        NAEDIN
      *** BEG ADD *** 05/20/08 ****************************
     C                   MOVE      SCOSHA        NAOSHA
      *** END ADD *** 05/20/08 ****************************
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
     C*
     C                   MOVE      *ON           *IN90
     C                   MOVEL     XXPRDS        QQPRDS           40
     C*
      *** BEG CHG *** 05/20/08 ****************************
     C*******************  MOVEA'00000'   *IN,71
     C                   MOVEA     '000000'      *IN(71)
      *** END CHG *** 05/20/08 ****************************
      *** BEG ADD *** 05/20/08 ****************************
     C                   SELECT
     C     HFLD          WHENEQ    'SCEDIN'
      *** END ADD *** 05/20/08 ****************************
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      ' '           QQCALL            1
     C                   PARM      'NATR'        QQTBID            4
     C                   PARM                    QQTKEY
     C                   PARM      ' '           QQRTN             1
     C*
     C     QQRTN         IFEQ      '*'
     C                   MOVEL     QQTKEY        SCEDIN
      *** BEG ADD *** 05/20/08 ****************************
     C                   MOVE      *ON           *IN81
     C                   ELSE
     C                   MOVE      *ON           *IN80
      *** END ADD *** 05/20/08 ****************************
     C                   ENDIF
     C*
      *** BEG ADD *** 05/20/08 ****************************
     C     HFLD          WHENEQ    'SCOSHA'
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      ' '           QQCALL            1
     C                   PARM      'NATO'        QQTBID            4
     C                   PARM                    QQTKEY
     C                   PARM      ' '           QQRTN             1
     C*
     C     QQRTN         IFEQ      '*'
     C                   MOVEL     QQTKEY        SCOSHA
     C                   ENDIF
     C*
     C                   ENDSL
      *** END ADD *** 05/20/08 ****************************
     C                   ENDIF
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
     C                   MOVEL     Q1USER        NAUSER
     C                   MOVEL     Q1PGM         NAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NADT
     C*
     C                   MOVEL     DSTIME        NATIME
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
     C                   MOVEL     Q1USER        NAUSER
     C                   MOVEL     Q1PGM         NAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NADT
     C*
     C                   MOVEL     DSTIME        NATIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WTNAP
     C*
     C                   ENDSR
