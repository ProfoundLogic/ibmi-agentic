     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0140R - LOCATION BUILDING MAINTENANCE
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program is the Location Building Maintenance Pgm  **
     H**   CALLED FROM:                                             **
     H**     ______ - ___________________________                   **
     H**   CALLS TO:                                                **
     H**     ______ - ___________________________                   **
     H****************************************************************
     H*
     H****************************************************************
     H** REVISIONS:                                                 **
     H**  11/18/96 *** C:107 *** V2000 ***                          **
     H**           1. Version 2 format changes.                     **
     H****************************************************************
     H*
     H****************************************************************
     H** INDICATOR USAGE:                                           **
     H**   INDICATOR            FUNCTION DESCRIPTION                **
     H**      __       ___________________________                  **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H**   FILES:                                                   **
     H**   _____  (_____): __________________________ (___)         **
     H****************************************************************
     H*
     FC0140F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FCMUPL     IF   E           K DISK
     FCMLOL     UF A E           K DISK
     FCMPML     IF   E           K DISK
     FCMAIL     IF   E           K DISK
     FCTABL     IF   E           K DISK
     F*

     D*===============================================================*
     D*     EXTENSION SECTION                                         *
     D*===============================================================*
     D*
     D* DEFINE ARRAYS
     D*
     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D ST              S              2    DIM(52) CTDATA PERRCD(26)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  AI                     1     20
     D                                     DIM(10) ASCEND
     D  SCADI1                 1      2
     D  SCADI2                 3      4
     D  SCADI3                 5      6
     D  SCADI4                 7      8
     D  SCADI5                 9     10
     D  SCADI6                11     12
     D  SCADI7                13     14
     D  SCADI8                15     16
     D  SCADI9                17     18
     D  SCADI0                19     20
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  POLICY MASTER FILE
     C*
     C     PMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C*
     C*  ADDITIONAL INTEREST FILE
     C*
     C     AIKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    XXADI             2
     C*
     C*  TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  LOCATION MASTER FILE
     C*
     C     LOKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCLOC#
     C                   KFLD                    SCPRM#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQPOL#           13
     C                   PARM                    QQRTC             1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQPOL#        SCPOL#
     C                   MOVE      QQLOC#        SCLOC#
     C                   MOVE      QQPRM#        SCPRM#
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE POLICY MASTER RECORD
     C*
     C     PMKEY         CHAIN     PPM01                              60
     C*
     C*   SET INDICATOR TO DISPLAY POLICY CANCELLED IF CANCELATION
     C*   FLAG(PMCANC) IS EQUAL TO "C"
     C*
     C     PMCANC        IFEQ      'C'
     C                   MOVE      '1'           *IN03
     C                   ELSE
     C                   MOVE      '0'           *IN03
     C                   END
     C*
     C                   MOVEL     PMFIN         SCFIN
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C*   PROCESS ALL FUNCTION KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  FUNCTION 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTC
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  FUNCTION 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   MOVE      'B'           QQRTC
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  FUNCTION 03 = RESTART
     C*
     C     *INKA         IFEQ      '1'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *ZERO         SCLOC#
     C                   MOVE      *ZERO         SCPRM#
     C                   GOTO      STRT
     C                   END
     C*
     C*  FUNCTION 05 = SEARCH LIST OF ADDITIONAL INSURED
     C*
     C     *INKE         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      SCPOL#        QQPOL#           13
     C                   MOVE      SCLOC#        QQLOC#            3
     C                   MOVE      SCPRM#        QQPRM#            3
     C                   CALL      'C0640R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQLOC#
     C                   PARM                    QQPRM#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQLOC#        SCLOC#
     C                   MOVE      QQPRM#        SCPRM#
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCLOC#        OREQ      *ZERO
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 07 = CALL ADDITIONAL INSURED MAINTENANCE
     C*
     C     *INKG         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C                   MOVE      SCPOL#        QQPOL#           13
     C                   CALL      'C0130R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   Z-ADD     1             Z                 2 0
     C     *BLANK        LOOKUP    AI(Z)                                  60
     C     *IN60         IFEQ      '1'
     C     Z             ADD       69            Y                 2 0
     C                   MOVE      '1'           *IN(Y)
     C                   END
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*  FUNCTION 9 = ADDITIONAL INTEREST SEARCH - SCREEN 2
     C*
     C     *INKI         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   Z-ADD     1             Z                 2 0
     C     '? '          LOOKUP    AI(Z)                                  60
     C     *IN60         IFEQ      '1'
     C*
     C                   MOVEL     SCPOL#        QQPOL#           13
     C                   MOVE      *BLANK        QQADI             2
     C                   MOVE      ' '           QQTYPE            1
     C                   CALL      'C0630R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQADI
     C                   PARM                    QQTYPE
     C                   PARM                    QQRTC             1
     C                   MOVE      QQADI         AI(Z)
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C     QQRTC         IFEQ      'B'
     C     QQADI         OREQ      *BLANK
     C     Z             ADD       69            Y                 2 0
     C                   MOVE      '1'           *IN(Y)
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C     Z             ADD       70            Y
     C                   MOVE      '1'           *IN(Y)
     C                   GOTO      STRT
     C                   END
     C*
     C                   ELSE
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C                   END
     C*
     C*  FCN 10 - TABLE FILE SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C*   COUNTY CODE SEARCH
     C*
     C     SCCNTY        IFEQ      '?'
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVEL     SCCNTY        QQKEY
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C2221R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C                   MOVEL     QQKEY         SCCNTY
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN17
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN18
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C                   END
     C                   GOTO      STRT
     C*
     C                   END
     C*   CHANGE STATUS
     C*
     C     *INKU         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  THE LOCATION NUMBER CANNOT BE ZERO
     C*
     C     SCLOC#        IFEQ      *ZERO
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE PREMISIS NUMBER CANNOT BE ZERO
     C*
     C     SCPRM#        IFEQ      *ZERO
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*   GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     LOKEY         CHAIN     PLO01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PLO01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      LOSTAT        SCSTAT
     C                   MOVE      LOOCAS        SCOCAS
     C                   MOVE      LOADR1        SCADR1
     C                   MOVE      LOADR2        SCADR2
     C                   MOVE      LOCTY         SCCTY
     C                   MOVE      LOCNTY        SCCNTY
     C                   MOVE      LOST          SCST
     C                   MOVEL     LOZIP         SCZIP1
     C                   MOVE      LOZIP         SCZIP2
     C                   MOVE      LOINT         SCINT
     C                   MOVE      LOYRBL        SCYRBL
     C                   Z-ADD     LOPTOC        SCPTOC
     C                   MOVE      LOCONS        SCCONS
     C                   MOVE      LOPRTC        SCPRTC
     C                   MOVE      LOSPRK        SCSPRK
     C                   MOVE      LOADI1        SCADI1
     C                   MOVE      LOADI2        SCADI2
     C                   MOVE      LOADI3        SCADI3
     C                   MOVE      LOADI4        SCADI4
     C                   MOVE      LOADI5        SCADI5
     C                   MOVE      LOADI6        SCADI6
     C                   MOVE      LOADI7        SCADI7
     C                   MOVE      LOADI8        SCADI8
     C                   MOVE      LOADI9        SCADI9
     C                   MOVE      LOADI0        SCADI0
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANK        SCOCAS
     C                   MOVE      *BLANK        SCADR1
     C                   MOVE      *BLANK        SCADR2
     C                   MOVE      *BLANK        SCCTY
     C                   MOVE      *BLANK        SCCNTY
     C                   MOVE      *BLANK        SCST
     C                   MOVE      *BLANK        SCZIP1
     C                   MOVE      *BLANK        SCZIP2
     C                   MOVE      *BLANK        SCINT
     C                   MOVE      *BLANK        SCYRBL
     C                   Z-ADD     *ZERO         SCPTOC
     C                   MOVE      *BLANK        SCCONS
     C                   MOVE      *BLANK        SCPRTC
     C                   MOVE      *BLANK        SCSPRK
     C                   MOVE      *BLANK        SCADI1
     C                   MOVE      *BLANK        SCADI2
     C                   MOVE      *BLANK        SCADI3
     C                   MOVE      *BLANK        SCADI4
     C                   MOVE      *BLANK        SCADI5
     C                   MOVE      *BLANK        SCADI6
     C                   MOVE      *BLANK        SCADI7
     C                   MOVE      *BLANK        SCADI8
     C                   MOVE      *BLANK        SCADI9
     C                   MOVE      *BLANK        SCADI0
     C                   MOVE      'A'           SCSTAT
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C* BEGIN EDIT CHECKING
     C*
     C*   OCCUPIED DESCRIPTION CANNOT BE BLANK
     C*
     C     SCOCAS        CABEQ     *BLANK        STRT                     20
     C*
     C*   ADDRESS CANNOT BE BLANK
     C*
     C     SCADR1        CABEQ     *BLANK        STRT                     21
     C*
     C*   CITY CANNOT BE BLANK
     C*
     C     SCCTY         CABEQ     *BLANK        STRT                     22
     C*
     C*  THE COUNTY CODE MUST BE VALID
     C*
     C     SCCNTY        IFNE      *BLANKS
     C                   MOVEL     SCCNTY        XXKEY
     C     TBKEY         CHAIN     PCY01                              42
     C     *IN42         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C     CYSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN80
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   STATE CANNOT BE BLANK
     C*
     C     SCST          LOOKUP    ST                                     60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*   ZIP CANNOT BE ZERO
     C*
     C     SCZIP1        CABEQ     *ZERO         STRT                     24
     C*
     C*    INSURED INTEREST MUST BE O, T, L, OR I
     C*
     C     SCINT         IFNE      *BLANK
     C     SCINT         ANDNE     'O'
     C     SCINT         ANDNE     'T'
     C     SCINT         ANDNE     'L'
     C     SCINT         ANDNE     'I'
     C                   MOVE      '1'           *IN40
     C                   GOTO      STRT
     C                   END
     C*
     C*   YEAR BUILT MUST BE 19XX OR 20
     C*
     C     SCYRBL        IFNE      *BLANK
     C*
     C                   MOVEL     SCYRBL        XXCENT            2
     C                   MOVE      SCYRBL        XXYEAR            2
     C     XXCENT        IFNE      '18'
     C     XXCENT        ANDNE     '19'
     C     XXCENT        ANDNE     '20'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEL     XXYEAR        XXTEN             1
     C                   MOVE      XXYEAR        XXONE             1
     C     XXTEN         IFLT      '0'
     C     XXTEN         ORGT      '9'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C     XXONE         IFLT      '0'
     C     XXONE         ORGT      '9'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   PART OF BUILDING OCCUPIED MUST BE BETWEEN 0 AND 100
     C*
     C     SCPTOC        IFNE      *ZERO
     C     SCPTOC        IFGT      100
     C     SCPTOC        ORLE      0
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   CONSTUCTION ISO MUST BE 0, 1, 2, 3, 4, 5, 6, 7, 8, OR 9
     C*
     C     SCCONS        IFNE      *BLANK
     C     SCCONS        ANDNE     '0'
     C     SCCONS        ANDNE     '1'
     C     SCCONS        ANDNE     '2'
     C     SCCONS        ANDNE     '3'
     C     SCCONS        ANDNE     '4'
     C     SCCONS        ANDNE     '5'
     C     SCCONS        ANDNE     '6'
     C     SCCONS        ANDNE     '7'
     C     SCCONS        ANDNE     '8'
     C     SCCONS        ANDNE     '9'
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROTECTION CLASS ISO MUST BE 01, 02, 03, 04, 05, 06, 07, 08,
     C*   09, OR 10
     C*
     C     SCPRTC        IFNE      *BLANK
     C     SCPRTC        ANDNE     '01'
     C     SCPRTC        ANDNE     '02'
     C     SCPRTC        ANDNE     '03'
     C     SCPRTC        ANDNE     '04'
     C     SCPRTC        ANDNE     '05'
     C     SCPRTC        ANDNE     '06'
     C     SCPRTC        ANDNE     '07'
     C     SCPRTC        ANDNE     '08'
     C     SCPRTC        ANDNE     '09'
     C     SCPRTC        ANDNE     '10'
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C*   SPRINKLAR  SYSTEM MUST BE "Y" OR "N"
     C*
     C     SCSPRK        IFNE      *BLANK
     C     SCSPRK        ANDNE     'Y'
     C     SCSPRK        ANDNE     'N'
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE ADDITIONAL INTERESTS IF NOT BLANK
     C*
     C     SCADI1        IFNE      *BLANK
     C                   MOVE      SCADI1        XXADI
     C     AIKEY         CHAIN     PAI01                              60
     C     *IN60         CABEQ     '1'           STRT                     30
     C     AISTAT        CABEQ     'I'           STRT                     50
     C                   END
     C*
     C     SCADI2        IFNE      *BLANK
     C                   MOVE      SCADI2        XXADI
     C     AIKEY         CHAIN     PAI01                              60
     C     *IN60         CABEQ     '1'           STRT                     31
     C     AISTAT        CABEQ     'I'           STRT                     51
     C                   END
     C*
     C     SCADI3        IFNE      *BLANK
     C                   MOVE      SCADI3        XXADI
     C     AIKEY         CHAIN     PAI01                              60
     C     *IN60         CABEQ     '1'           STRT                     32
     C     AISTAT        CABEQ     'I'           STRT                     52
     C                   END
     C*
     C     SCADI4        IFNE      *BLANK
     C                   MOVE      SCADI4        XXADI
     C     AIKEY         CHAIN     PAI01                              60
     C     *IN60         CABEQ     '1'           STRT                     33
     C     AISTAT        CABEQ     'I'           STRT                     53
     C                   END
     C*
     C     SCADI5        IFNE      *BLANK
     C                   MOVE      SCADI5        XXADI
     C     AIKEY         CHAIN     PAI01                              60
     C     *IN60         CABEQ     '1'           STRT                     34
     C     AISTAT        CABEQ     'I'           STRT                     54
     C                   END
     C*
     C     SCADI6        IFNE      *BLANK
     C                   MOVE      SCADI6        XXADI
     C     AIKEY         CHAIN     PAI01                              60
     C     *IN60         CABEQ     '1'           STRT                     35
     C     AISTAT        CABEQ     'I'           STRT                     55
     C                   END
     C*
     C     SCADI7        IFNE      *BLANK
     C                   MOVE      SCADI7        XXADI
     C     AIKEY         CHAIN     PAI01                              60
     C     *IN60         CABEQ     '1'           STRT                     36
     C     AISTAT        CABEQ     'I'           STRT                     56
     C                   END
     C*
     C     SCADI8        IFNE      *BLANK
     C                   MOVE      SCADI8        XXADI
     C     AIKEY         CHAIN     PAI01                              60
     C     *IN60         CABEQ     '1'           STRT                     37
     C     AISTAT        CABEQ     'I'           STRT                     57
     C                   END
     C*
     C     SCADI9        IFNE      *BLANK
     C                   MOVE      SCADI9        XXADI
     C     AIKEY         CHAIN     PAI01                              60
     C     *IN60         CABEQ     '1'           STRT                     38
     C     AISTAT        CABEQ     'I'           STRT                     58
     C                   END
     C*
     C     SCADI0        IFNE      *BLANK
     C                   MOVE      SCADI0        XXADI
     C     AIKEY         CHAIN     PAI01                              60
     C     *IN60         CABEQ     '1'           STRT                     39
     C     AISTAT        CABEQ     'I'           STRT                     59
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     *IN09         IFEQ      '0'
     C     LOKEY         CHAIN     PLO01                              40
     C                   MOVE      UPCO#         LOCO#
     C                   MOVE      SCPOL#        LOPOL#
     C                   MOVE      SCLOC#        LOLOC#
     C                   MOVE      SCPRM#        LOPRM#
     C                   MOVE      SCOCAS        LOOCAS
     C                   MOVE      SCADR1        LOADR1
     C                   MOVE      SCADR2        LOADR2
     C                   MOVE      SCCTY         LOCTY
     C                   MOVE      SCCNTY        LOCNTY
     C                   MOVE      SCST          LOST
     C                   MOVEL     SCZIP1        LOZIP
     C                   MOVE      SCZIP2        LOZIP
     C                   MOVE      SCINT         LOINT
     C                   MOVE      SCYRBL        LOYRBL
     C                   MOVE      SCPTOC        LOPTOC
     C                   MOVE      SCCONS        LOCONS
     C                   MOVE      SCPRTC        LOPRTC
     C                   MOVE      SCSPRK        LOSPRK
     C                   MOVE      SCADI1        LOADI1
     C                   MOVE      SCADI2        LOADI2
     C                   MOVE      SCADI3        LOADI3
     C                   MOVE      SCADI4        LOADI4
     C                   MOVE      SCADI5        LOADI5
     C                   MOVE      SCADI6        LOADI6
     C                   MOVE      SCADI7        LOADI7
     C                   MOVE      SCADI8        LOADI8
     C                   MOVE      SCADI9        LOADI9
     C                   MOVE      SCADI0        LOADI0
     C                   MOVE      SCSTAT        LOSTAT
     C                   MOVE      Q1USER        LOUSER
     C                   MOVE      Q1PGM         LOPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LODT
     C                   Z-ADD     DSTIME        LOTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PLO01
     C                   ELSE
     C                   WRITE     PLO01
     C                   END
     C*
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCLOC#
     C                   MOVE      *BLANK        SCPRM#
     C                   GOTO      STRT
     C*
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPLO01     E            PLO01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE ST
ALAKAZARCACOCTDEDCFLGAHIIDILINIAKSKYLAMEMDMAMIMNMSWY
MOMTNENVNHNJNMNYNCNDOHOKORPARISCSDTNTXUTVTVAWAWVWIPR
