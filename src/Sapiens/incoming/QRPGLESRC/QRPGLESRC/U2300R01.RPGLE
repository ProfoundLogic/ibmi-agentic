     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               * *
     H*  09/14/00 C:62 PROJECT:URMNT                                  * *
     H*                -ADD KJDO FOR F10=SEARCH OF REGION AND CATEGORY* *
     H*                                                               *
     H*****************************************************************
     FU2300F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FUMLSP     UF A E             DISK
     F                                     RENAME(PLS01:DBRCDF)
     FUMLSL2    IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('LOS Data Maint-
     D                                     enance')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D*** BEGIN ADD *** 09/14/00 **************************************
     D PRD1            C                   CONST('LOS REGION CODE -
     D                                     SEARCH')
     D PRD2            C                   CONST('LOS CATEGORY CODE -
     D                                     SEARCH')

     C*** END   ADD *** 09/14/00 **************************************
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
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     LSKY          KLIST
     C                   KFLD                    ##RGN
     C                   KFLD                    ##LGRP
     C                   KFLD                    ##CAT
     C                   KFLD                    ##AGE
     C                   KFLD                    ##EFDT
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    LSRGN         ##RGN
     C     *LIKE         DEFINE    LSLGRP        ##LGRP
     C     *LIKE         DEFINE    LSCAT         ##CAT
     C     *LIKE         DEFINE    LSAGE         ##AGE
     C     *LIKE         DEFINE    LSEFDT        ##EFDT
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
     C     LSSTAT        IFEQ      'I'
     C                   MOVE      'A'           LSSTAT
     C                   ELSE
     C                   MOVE      'I'           LSSTAT
     C                   ENDIF                                                  LSSTAT
     C*
     C                   MOVEL     Q1USER        LSUSER
     C                   MOVEL     Q1PGM         LSPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LSDT
     C*
     C                   MOVEL     DSTIME        LSTIME
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
     C* THE REGION CODE MUST BE A VALID CODE
     C*
     C  N90
     CAN 40SCRGN         IFNE      2
     C     SCRGN         ANDNE     3
     C     SCRGN         ANDNE     4
     C     SCRGN         ANDNE     5
     C     SCRGN         ANDNE     6
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF                                                  END SCRGN
     C*
     C* THE DIAG/OPER GROUP CANNOT BE BLANKS
     C*
     C  N90
     CAN 40SCLGRP        IFEQ      *BLANKS
     C*
     C                   SETON                                        7290
     C*
     C                   ENDIF                                                  END SCLGRP
     C*
     C* THE CATEGORY CODE MUST BE A VALID CODE
     C*
     C  N90
     CAN 40SCCAT         IFNE      0
     C     SCCAT         ANDNE     1
     C     SCCAT         ANDNE     2
     C     SCCAT         ANDNE     3
     C     SCCAT         ANDNE     4
     C     SCCAT         ANDNE     5
     C     SCCAT         ANDNE     6
     C     SCCAT         ANDNE     7
     C     SCCAT         ANDNE     8
     C     SCCAT         ANDNE     9
     C*
     C                   SETON                                        7390
     C*
     C                   ENDIF                                                  END SCCAT
     C*
     C* AGE GROUP CODE CAN'T BE ZERO
     C*
     C  N90
     CAN 40SCAGE         IFEQ      *ZEROS
     C*
     C                   SETON                                        7490
     C*
     C                   ENDIF                                                  END SCAGE
     C*
     C* THE EFF. DATE MUST BE VALID
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
     C                   SETON                                        7590
     C*
     C                   ENDIF                                                  END SRERR
     C*
     C                   ENDIF                                                  END SCEFDT
     C*
     C* CHECK TO BE SURE CLAIM STATUS CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCRGN         ##RGN
     C                   MOVEL     SCLGRP        ##LGRP
     C                   MOVEL     SCCAT         ##CAT
     C                   MOVEL     SCAGE         ##AGE
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##EFDT
     C*
     C   40
     CANN90LSKY          CHAIN     PLS01                              99
     C   40
     CANN90
     CANN99              SETON                                        76  90
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90LSDT          IFGT      SAVEDT
     C     LSDT          OREQ      SAVEDT
     C     LSTIME        ANDGT     SAVETM
     C*
     C                   SETON                                        9078
     C*
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
     C*** BEGIN ADD *** 09/14/00 **************************************
     C*================================================================
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCRGN'
     C                   CALL      'U0536R'
     C                   PARM      PRD1          ##PRDS           40
     C                   PARM      *BLANKS       ##CODE            1
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##CODE        OREQ      *BLANKS
     C*
     C                   SETON                                        31
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  ##CODE        SCRGN
     C                   SETON                                        32
     C*
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCCAT'
     C                   CALL      'U0537R'
     C                   PARM      PRD2          ##PRDS           40
     C                   PARM      *BLANKS       ##CODE            1
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##CODE        OREQ      *BLANKS
     C*
     C                   SETON                                        33
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  ##CODE        SCCAT
     C                   SETON                                        34
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** END   ADD *** 09/14/00 **************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVE      LSRGN         SCRGNO
     C                   MOVEL     LSLGRP        SCLGRP
     C                   MOVE      LSCAT         SCCATO
     C                   MOVE      LSAGE         SCAGEO
     C*
     C                   Z-ADD     LSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDO
     C*
     C                   MOVE      LSTPAT        SCTPAT
     C                   MOVE      LSASTY        SCASTY
     C                   MOVE      LSVARI        SCVARI
     C                   MOVE      LS10P         SC10P
     C                   MOVE      LS10P1        SC10P1
     C                   MOVE      LS25P         SC25P
     C                   MOVE      LS25P1        SC25P1
     C                   MOVE      LS50P         SC50P
     C                   MOVE      LS50P1        SC50P1
     C                   MOVE      LS75P         SC75P
     C                   MOVE      LS75P1        SC75P1
     C                   MOVE      LS90P         SC90P
     C                   MOVE      LS90P1        SC90P1
     C                   MOVE      LS95P         SC95P
     C                   MOVE      LS95P1        SC95P1
     C                   MOVE      LS99P         SC99P
     C                   MOVE      LS99P1        SC99P1
     C                   MOVEL     LSSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVE      *ZEROS        SCRGN
     C                   MOVEL     *BLANKS       SCLGRP
     C                   MOVE      *ZEROS        SCCAT
     C                   MOVE      *ZEROS        SCAGE
     C                   MOVE      *ZEROS        SCEFDT
     C                   MOVE      *BLANKS       SCTPAT
     C                   MOVE      *BLANKS       SCASTY
     C                   MOVE      *BLANKS       SCVARI
     C                   MOVE      *BLANKS       SC10P
     C                   MOVE      *BLANKS       SC10P1
     C                   MOVE      *BLANKS       SC25P
     C                   MOVE      *BLANKS       SC25P1
     C                   MOVE      *BLANKS       SC50P
     C                   MOVE      *BLANKS       SC50P1
     C                   MOVE      *BLANKS       SC75P
     C                   MOVE      *BLANKS       SC75P1
     C                   MOVE      *BLANKS       SC90P
     C                   MOVE      *BLANKS       SC90P1
     C                   MOVE      *BLANKS       SC95P
     C                   MOVE      *BLANKS       SC95P1
     C                   MOVE      *BLANKS       SC99P
     C                   MOVE      *BLANKS       SC99P1
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
     C   40              MOVE      SCRGN         LSRGN
     C  N40              MOVE      SCRGNO        LSRGN
     C                   MOVEL     SCLGRP        LSLGRP
     C   40              MOVE      SCCAT         LSCAT
     C  N40              MOVE      SCCATO        LSCAT
     C   40              MOVE      SCAGE         LSAGE
     C  N40              MOVE      SCAGEO        LSAGE
     C*
     C   40              Z-ADD     SCEFDT        SRDTI
     C  N40              Z-ADD     SCEFDO        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LSEFDT
     C*
     C*** IF THE FIRST POS IF SCLGRP IS BLANK MOVE IN OPER FLAG
     C*
     C                   SUBST     SCLGRP:1      XXOPER            1
     C*
     C     XXOPER        IFEQ      *BLANKS
     C*
     C                   MOVE      'O'           LSLOST
     C*
     C                   ELSE
     C*
     C                   MOVE      'D'           LSLOST
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       LSBLK1
     C                   MOVE      SCTPAT        LSTPAT
     C                   MOVE      SCASTY        LSASTY
     C                   MOVE      SCVARI        LSVARI
     C                   MOVE      SC10P         LS10P
     C                   MOVE      SC10P1        LS10P1
     C                   MOVE      SC25P         LS25P
     C                   MOVE      SC25P1        LS25P1
     C                   MOVE      SC50P         LS50P
     C                   MOVE      SC50P1        LS50P1
     C                   MOVE      SC75P         LS75P
     C                   MOVE      SC75P1        LS75P1
     C                   MOVE      SC90P         LS90P
     C                   MOVE      SC90P1        LS90P1
     C                   MOVE      SC95P         LS95P
     C                   MOVE      SC95P1        LS95P1
     C                   MOVE      SC99P         LS99P
     C                   MOVE      SC99P1        LS99P1
     C                   MOVEL     LSSTAT        LSSTAT
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
     C*** BEGIN ADD *** 09/14/00 **************************************
     C                   MOVEA     '0000'        *IN(31)
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*** END   ADD *** 09/14/00 **************************************
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
     C                   MOVEL     Q1USER        LSUSER
     C                   MOVEL     Q1PGM         LSPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LSDT
     C*
     C                   MOVEL     DSTIME        LSTIME
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
     C                   MOVEL     Q1USER        LSUSER
     C                   MOVEL     Q1PGM         LSPGM
     C                   MOVEL     Q1USER        LSIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LSDT
     C*
     C                   MOVEL     SRDTO         LSIDT
     C*
     C                   MOVEL     DSTIME        LSTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      UMLSP
     C*
     C                   ENDSR
