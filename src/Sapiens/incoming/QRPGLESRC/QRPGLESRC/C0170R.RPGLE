     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0170R - DRIVER MASTER MAINTENANCE
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program maintains the Driver Master File (CMDRP)  **
     H**   CALLED FROM:                                             **
     H**     ______ - ___________________________                   **
     H**   CALLS TO:                                                **
     H**     C0006R - Change User Parameters                        **
     H**     C0530R - Search Policy Master File By Policy Number    **
     H**     C0171R - Global Search Driver Database                 **
     H**     C0531R - Search Policy Master File By Named Insured    **
     H**     C0630R - Casuality Additional Interest                 **
     H**     C0670R - Driver Master Search                          **
     H**     C0671R - Driver Last Name Search                       **
     H**     M0006C - Help Processor                                **
     H**     SRDCK  - Check For Valid Date                          **
     H****************************************************************
     H*
     H****************************************************************
     H** REVISIONS:                                                 **
     H**  11/18/96 *** C:107 *** V2000 ***                          **
     H**           1. Version 2 format changes.                     **
     H**                                                            **
     H****************************************************************
     H*
     H****************************************************************
     H** INDICATOR USAGE:                                           **
     H**   INDICATOR            FUNCTION DESCRIPTION                **
     H**      03       ___________________________                  **
     H**      09       ___________________________                  **
     H**      10       ___________________________                  **
     H**      11       ___________________________                  **
     H**      20       ___________________________                  **
     H**      21       ___________________________                  **
     H**      22       ___________________________                  **
     H**      23       ___________________________                  **
     H**      24       ___________________________                  **
     H**      25       ___________________________                  **
     H**      27       Error - owner/driver must be 'Y' or  'N'     **
     H**      28       Error - owner/driver is 'Y' and Vehicle #    **
     H**               is blank                                     **
     H**      32       ___________________________                  **
     H**      35       Error - Item 1 not in vehicle data base      **
     H**      36       Error - Item 2 not in vehicle data base      **
     H**      37       Error - Item 3 not in vehicle data base      **
     H**      38       Error - Item 4 not in vehicle data base      **
     H**      39       Error - Item 5 not in vehicle data base      **
     H**      40       ___________________________                  **
     H**      41       Data passed from search driver database      **
     H**      50       ___________________________                  **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H**   FILES:                                                   **
     H**   SMCOL  (PCO01): System Company Master File (CO#)         **
     H**   CMUPL  (PUP01): User Parameter File (USR)                **
     H**   CMPML  (PPM01): Policy Master File (CO#,POL#)            **
     H**   CMDRL  (PDR01): Driver Master File (CO#/POL#/STI/LIC#)   **
     H**   CMVSL  (PVS01): Vehicle Schecule File (CO#/POL#/ITM#)    **
     H****************************************************************
     H*
     FC0170F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FCMUPL     IF   E           K DISK
     FCMPML     IF   E           K DISK
     FCMDRL     UF A E           K DISK
     FCMVSL     IF   E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D ST              S              2    DIM(52) CTDATA PERRCD(26)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE KEY LISTS
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  DRIVER MASTER FILE
     C*
     C     DRKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCSTI
     C                   KFLD                    SCLIC#
     C*
     C*  POLICY MASTER FILE
     C*
     C     PMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C*
     C*  VEHICLE SCHEDULE FILE
     C*
     C     VSKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    XXITEM
     C     *LIKE         DEFINE    VSITM#        XXITEM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQPOL#           13
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
     C                   MOVEL     QQPOL#        SCPOL#
     C     SCPOL#        IFNE      *BLANK
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              50
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
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
     C*
     C*   PROCESS ALL FUNCTION KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  FUNCTION 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  FUNCTION 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*  FUNCTION 05 = SEARCH POLICY MASTER - POLICY ORDER - SCREEN 1
     C*
     C     *INKE         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVE      *BLANK        QQPOL#           13
     C                   MOVE      SCPOL#        QQPOL#
     C                   CALL      'C0530R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQPOL#        SCPOL#
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQPOL#        OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C                   END
     C*
     C*  FUNCTION 07 = SEARCH POLICY MASTER BY NAME, SCR01
     C*
     C     *INKG         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVE      *BLANKS       QQPOL#
     C                   CALL      'C0531R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQPOL#        SCPOL#
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     SCPOL#        OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C                   END
     C*
     C*  FUNCTION 8 = DRIVER LAST NAME SEARCH - SCR01
     C*
     C     *INKH         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVE      *BLANKS       QQPOL#
     C                   MOVE      SCPOL#        QQPOL#
     C                   MOVE      *BLANKS       QQLNAM           30
     C                   CALL      'C0671R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQLNAM
     C                   PARM                    QQSTI
     C                   PARM                    QQLIC#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQPOL#        SCPOL#
     C                   MOVE      QQSTI         SCSTI
     C                   MOVE      QQLIC#        SCLIC#
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     SCSTI         OREQ      *BLANK
     C     SCLIC#        OREQ      *BLANK
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C*
     C                   END
     C                   END
     C*
     C*  FUNCTION 9 = DRIVER SEARCH - SCR01
     C*
     C     *INKI         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVE      *BLANKS       QQPOL#
     C                   MOVE      SCPOL#        QQPOL#
     C                   MOVEL     SCSTI         QQSTI             2
     C                   MOVEL     SCLIC#        QQLIC#           14
     C                   CALL      'C0670R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQSTI
     C                   PARM                    QQLIC#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQPOL#        SCPOL#
     C                   MOVE      QQSTI         SCSTI
     C                   MOVE      QQLIC#        SCLIC#
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     SCSTI         OREQ      *BLANK
     C     SCLIC#        OREQ      *BLANK
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C*
     C                   END
     C                   END
     C*
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
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'C0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
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
     C*   VALIDATE POLICY NUMBER
     C*
     C     PMKEY         CHAIN     PPM01                              20
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
     C     *IN20         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEL     PMFIN         SCFIN
     C*
     C*  THE STATE MUST BE VALID
     C*
     C     SCSTI         LOOKUP    ST                                     50
     C     *IN50         IFEQ      '0'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*          CHECK DRIVER MASTER FILE FOR DUPS
     C*          -----------------------------------------------------
     C                   MOVE      *OFF          *IN41
     C                   MOVEL     SCLIC#        QQLIC#
     C                   MOVE      *BLANKS       QQLSN            30
     C                   MOVE      *BLANKS       QQFSN            15
     C                   MOVE      *BLANKS       QQMID             1
     C                   MOVE      *BLANKS       QQDOB             6
     C                   CALL      'C0171R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQLIC#
     C                   PARM                    QQLSN
     C                   PARM                    QQFSN
     C                   PARM                    QQMID
     C                   PARM                    QQDOB
     C                   PARM                    QQRTC
     C*
     C     QQRTC         IFEQ      'B'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      *ON           *IN41
     C                   END
     C*
     C*  ALL EDITS PASSED, GET REQUESTED RECORD & LOAD INFORMATION
     C*
     C     DRKEY         CHAIN     PDR01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PDR01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVEL     DRLSN         SCLSN
     C                   MOVEL     DRFSN         SCFSN
     C                   MOVE      DRMID         SCMID
     C                   Z-ADD     DRDOB         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDOB
     C                   MOVE      DROWN         SCOWN
     C                   MOVE      DRITM#        SCITM#
     C                   MOVE      DRITM2        SCITM2
     C                   MOVE      DRITM3        SCITM3
     C                   MOVE      DRITM4        SCITM4
     C                   MOVE      DRITM5        SCITM5
     C                   MOVE      DRCLS         SCCLS
     C                   Z-ADD     DREMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEMDT
     C                   Z-ADD     DRVROD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCVROD
     C                   Z-ADD     DRVRRD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCVRRD
     C                   MOVEL     DRCOMM        SCCOMM
     C                   MOVE      DRSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCLSN
     C                   MOVE      *BLANKS       SCFSN
     C                   MOVE      *BLANK        SCMID
     C                   MOVE      *ZEROS        SCDOB
     C*
     C*          USE DATA PASSED FROM DRIVER SEARCH
     C*          -----------------------------------------------------
     C     *IN41         IFEQ      *ON
     C                   MOVE      QQLSN         SCLSN
     C                   MOVE      QQFSN         SCFSN
     C                   MOVE      QQMID         SCMID
     C                   MOVE      QQDOB         SCDOB
     C                   Z-ADD     SCDOB         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDOB
     C                   END
     C                   MOVE      *BLANK        SCOWN
     C                   MOVE      *BLANK        SCITM#
     C                   MOVE      *BLANK        SCITM2
     C                   MOVE      *BLANK        SCITM3
     C                   MOVE      *BLANK        SCITM4
     C                   MOVE      *BLANK        SCITM5
     C                   MOVE      *BLANK        SCCLS
     C                   MOVE      *ZEROS        SCEMDT
     C                   MOVE      *ZEROS        SCVROD
     C                   MOVE      *ZEROS        SCVRRD
     C                   MOVE      *BLANKS       SCCOMM
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
     C     *IN09         CABEQ     '1'           SCR02B
     C*
     C*  LAST NAME CANNOT BE BLANK
     C*
     C     SCLSN         IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  FIRST NAME CANNOT BE BLANK
     C*
     C     SCFSN         IFEQ      *BLANK
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  DATE OF BIRTH MUST BE A VALID DATE
     C*
     C     SCDOB         IFEQ      *ZERO
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      *BLANKS       SRDTE             6
     C                   MOVE      ' '           SRERR             1
     C                   MOVE      ' '           SRTERM            1
     C                   Z-ADD     SCDOB         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDOB
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   OWNER/DRIVER REQUIRED
     C*
     C     SCOWN         IFNE      'Y'
     C     SCOWN         ANDNE     'N'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*   VEHICLE ID REQUIRED IF OWNER/DRIVER IS YES
     C*
     C     SCOWN         IFEQ      'Y'
     C     SCITM#        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C*   ITEM 1, IF USED,  MUST BE IN VEHICLE DATA BASE
     C*
     C     SCITM#        IFNE      *BLANKS
     C     SCITM#        ANDNE     '00000'
     C                   MOVE      SCITM#        XXITEM
     C     VSKEY         CHAIN     PVS01                              50
     C     *IN50         IFEQ      *ON
     C     VSSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN35
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   ITEM 2, IF USED,  MUST BE IN VEHICLE DATA BASE
     C*
     C     SCITM2        IFNE      *BLANKS
     C     SCITM2        ANDNE     '00000'
     C                   MOVE      SCITM2        XXITEM
     C     VSKEY         CHAIN     PVS01                              50
     C     *IN50         IFEQ      *ON
     C     VSSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN36
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   ITEM 3, IF USED,  MUST BE IN VEHICLE DATA BASE
     C*
     C     SCITM3        IFNE      *BLANKS
     C     SCITM3        ANDNE     '00000'
     C                   MOVE      SCITM3        XXITEM
     C     VSKEY         CHAIN     PVS01                              50
     C     *IN50         IFEQ      *ON
     C     VSSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN37
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   ITEM 4, IF USED,  MUST BE IN VEHICLE DATA BASE
     C*
     C     SCITM4        IFNE      *BLANKS
     C     SCITM4        ANDNE     '00000'
     C                   MOVE      SCITM4        XXITEM
     C     VSKEY         CHAIN     PVS01                              50
     C     *IN50         IFEQ      *ON
     C     VSSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN38
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   ITEM 5, IF USED,  MUST BE IN VEHICLE DATA BASE
     C*
     C     SCITM5        IFNE      *BLANKS
     C     SCITM5        ANDNE     '00000'
     C                   MOVE      SCITM5        XXITEM
     C     VSKEY         CHAIN     PVS01                              50
     C     *IN50         IFEQ      *ON
     C     VSSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN39
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   LICENSE CLASS REQUIRED
     C*
     C     SCCLS         IFNE      'A'
     C     SCCLS         ANDNE     'B'
     C     SCCLS         ANDNE     'C'
     C     SCCLS         ANDNE     'D'
     C     SCCLS         ANDNE     'E'
     C     SCCLS         ANDNE     'L'
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C*  DATE HIRED MUST BE VALID
     C*
     C     SCEMDT        IFNE      *ZERO
     C                   MOVE      *BLANKS       SRDTE             6
     C                   MOVE      ' '           SRERR             1
     C                   MOVE      ' '           SRTERM            1
     C                   Z-ADD     SCEMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEMDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  DATE MVR ORDERED MUST BE VALID
     C*
     C     SCVROD        IFNE      *ZERO
     C                   MOVE      *BLANKS       SRDTE             6
     C                   MOVE      ' '           SRERR             1
     C                   MOVE      ' '           SRTERM            1
     C                   Z-ADD     SCVROD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCVROD
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  DATE MVR RECEIVED MUST BE VALID
     C*
     C     SCVRRD        IFNE      *ZERO
     C                   MOVE      *BLANKS       SRDTE             6
     C                   MOVE      ' '           SRERR             1
     C                   MOVE      ' '           SRTERM            1
     C                   Z-ADD     SCVRRD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCVRRD
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     DRKEY         CHAIN     PDR01                              40
     C                   MOVE      UPCO#         DRCO#
     C                   MOVE      SCPOL#        DRPOL#
     C                   MOVEL     SCLSN         DRLSN
     C                   MOVEL     SCFSN         DRFSN
     C                   MOVE      SCMID         DRMID
     C                   Z-ADD     SCDOB         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DRDOB
     C                   MOVE      SCOWN         DROWN
     C                   MOVE      SCITM#        DRITM#
     C                   MOVE      SCITM2        DRITM2
     C                   MOVE      SCITM3        DRITM3
     C                   MOVE      SCITM4        DRITM4
     C                   MOVE      SCITM5        DRITM5
     C                   MOVE      SCCLS         DRCLS
     C                   MOVEL     SCLIC#        DRLIC#
     C                   MOVE      SCSTI         DRSTI
     C                   Z-ADD     SCEMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DREMDT
     C                   Z-ADD     SCVROD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DRVROD
     C                   Z-ADD     SCVRRD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DRVRRD
     C                   MOVEL     SCCOMM        DRCOMM
     C                   MOVE      SCSTAT        DRSTAT
     C                   MOVE      Q1USER        DRUSER
     C                   MOVE      Q1PGM         DRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DRDT
     C                   Z-ADD     DSTIME        DRTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PDR01
     C                   ELSE
     C                   WRITE     PDR01
     C                   END
     C*
     C     SCR02B        TAG
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCSTI
     C                   MOVE      *BLANK        SCLIC#
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     OPDR01     E            PDR01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE ST
ALAKAZARCACOCTDEDCFLGAHIIDILINIAKSKYLAMEMDMAMIMNMSWY
MOMTNENVNHNJNMNYNCNDOHOKORPARISCSDTNTXUTVTVAWAWVWIPR
