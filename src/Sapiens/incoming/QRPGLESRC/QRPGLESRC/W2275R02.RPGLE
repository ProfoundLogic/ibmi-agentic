     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H* TYPE OF BUILDING CODE MAINTENACE -                           *
     H*                                                              *
     H*      This code is located in WTTBP (new file) & was added to *
     H*      WDLDP (Entity/Location file), WAELP (Revised billing    *
     H*      Employer Detail) and WDELP (Employer Deatil f  e)       *
     H*                                                              *
     H*  10/05/04  C:132  PROJECT: 132WBF                            *
     H*                                                              *
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                               *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     FW2275F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWTTBP     UF A E             DISK
     F                                     RENAME(PTB01:DBRCDF)
     F*  Primary logical file definition (for displaying items)
     FWTTBL     IF   E           K DISK
      *
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Type of buildin-
     D                                     g code')
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
     C*
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##TBCD
     C*
     C     TBKY1         KLIST
     C                   KFLD                    ##CO#
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    TBCO#         ##CO#
     C     *LIKE         DEFINE    TBTBCD        ##KEY
     C     *LIKE         DEFINE    TBTBCD        ##TBCD
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
     C     TBSTAT        IFEQ      'I'
     C                   MOVE      'A'           TBSTAT
     C                   ELSE
     C                   MOVE      'I'           TBSTAT
     C                   ENDIF                                                  TBSTAT
     C*
     C                   MOVEL     Q1USER        TBUSER
     C                   MOVEL     Q1PGM         TBPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TBDT
     C*
     C                   MOVEL     DSTIME        TBTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
     C     *IN16         IFEQ      *ON
     C*
     C                   ENDIF                                                  *IN16
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
     C* TYPE OF BLDG CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCTBCD        IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCTBCD
     C*
     C* TYPE OF BLDG CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C* CHECK TO BE SURE TRANSACTION CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCTBCD        ##TBCD
     C*
     C   40
     CANN90TBKY          CHAIN     PTB01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90TBDT          IFGT      SAVEDT
     C     TBDT          OREQ      SAVEDT
     C     TBTIME        ANDGT     SAVETM
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
     C                   MOVEL     TBTBCD        SCTBCD
     C                   MOVEL     TBDESC        SCDESC
     C                   MOVEL     TBSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCTBCD
     C                   MOVEL     *BLANKS       SCDESC
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
     C                   Z-ADD     ##CO#         TBCO#
     C                   MOVEL     SCTBCD        TBTBCD
     C                   MOVEL     SCDESC        TBDESC
     C                   MOVEL     SCSTAT        TBSTAT
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
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
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
     C     TBSTAT        IFEQ      ' '
     C                   MOVE      'A'           TBSTAT
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        TBUSER
     C                   MOVEL     Q1PGM         TBPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TBDT
     C*
     C                   MOVEL     DSTIME        TBTIME
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
     C     TBSTAT        IFEQ      ' '
     C                   MOVE      'A'           TBSTAT
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        TBUSER
     C                   MOVEL     Q1PGM         TBPGM
     C                   MOVEL     Q1USER        TBIUSR
     C                   MOVEL     Q1PGM         TBIPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TBDT
     C                   MOVEL     SRDTO         TBIDT
     C*
     C                   MOVEL     DSTIME        TBTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WTTBP
     C*
     C                   ENDSR
     C*
     C*================================================================
