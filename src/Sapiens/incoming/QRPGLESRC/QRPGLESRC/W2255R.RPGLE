     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE W2255R - DEPTS/DOC IDS MAINTENANCE
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/14/08 C:190 Project: 133WET                               *
     H*                  Allow the "FOLDER" entry to be blank and     *
     H*                  allow program ID to be entered into the      *
     H*                  the "Document ID" field.                     *
     H*                                                               *
     H*  10/11/06  C:33  Seq#: WA099/41801                            *
     H*                  Use SC305R instead of SC580R.                *
     H*                                                               *
     H*  09/01/98  C:02  PROJECT: 33WDQ                               *
     H*                 -BLANKS ARE ALLOWED FOR REMARK CODE           *
     H*                                                               *
     H*                                                               *
     H*  05/28/98  C:77  PROJECT: WC577                               *
     H*                 -ADD PROCESSING TO ALLOW OPTION TO CHANGE     *
     H*                  DEPT. LETTERS THAT ARE RESOLVED TO DARTS.    *
     H*                 -REMOVE AUTO FOLDER BUILD, AREA OF SERVICE,   *
     H*                  SUB-AREA.                                    *
     H*                                                               *
     H*  04/10/98  C:47  SUPP: DR060/11072                            *
     H*                  DO NOT CHANGE THE ADDITIONAL INPUT FLAG      *
     H*                  (WMDOP/DOAIFL).                              *
     H* ------------------------------------------------------------  *
     H*  11/28/94  C:30  PROJECT:  WC376                              *
     H*                  ALLOW F23 TO DELETE DEPARTMENT CODE IF       *
     H*                  STATUS IS 'I'.                               *
     H*                                                               *
     H*  05/08/95 C:30 PROJECT: WC376                                  *
     H*           REMOVE WTABL (REFERENCE TO FILE WTASP AND WTSAP).    *
     H*           ADD LOGICALS WTASL AND WTSAL.                        *
     H*                                                                *
     H*  08/21/95 C:11 PROJECT: WC376                                 *
     H*                REMOVE "ADDITIONAL INPUT" FLAG FROM SCREEN.    *
     H*                LOAD 'N' AS DEFAULT VALUE TO FILE. PROGRAMMER  *
     H*                WILL LOAD 'Y' AND PGM NAME THRU DFU IF NEEDED. *
     H*****************************************************************
     H*  VARIABLE  USAGE:                                             *
     H*    QQRTCD - USED TO RETURN VALUE TO W4115R                    *
     H*           A = F3 (EXIT)                                       *
     H*           B = F12 (CANCEL)                                    *
     H*           * = NEW DEPT ADDED                                  *
     H*****************************************************************
     H* INDICATOR USAGE
     H* ALL/DIFFERENT SCREENS AND FUNCTIONS:
     H*  60      GENERAL CHAIN
     H* SCR01:
     H*  20      WCR0824-INVALID DEPARTMENT CODE
     H*  21      WCR0826-INVALID FOLDER NAME
     H*  22      WCR0825-DOCUMENT ID DOES NOT EXIST IN SPECIFIED FOLDER
     H*  23      SCM0517-NOT AUTHORIZED TO DOCUMENT OR FOLDER
     H*  30      POSITION CURSOR AT DEPT CODE
     H*  31      POSITION CURSOR AT FOLDER
     H*  32      POSITION CURSOR AT DOCUMENT ID
     H*  81      SCM0525-DOCUMENT NAME MUST BE VALID TO PERFORM ADD FUNCTION
     H*  82      SCM0517-NOT AUTHORIZED TO DOCUMENT OR FOLDER
     H*  83      SCM0518-USER NOT ENROLLED IN OFFICE 400
     H*  84      SCM0531-UNABLE TO PROCESS REQUEST FOR THIS FOLDER
     H*  85      SCM0520-DOCUMENT NAME ALREADY EXISTS AS DOCUMENT OR FOLDER
     H*  86      SCM0521-FOLDER NOT FOUND
     H*  91      SCM0525-DOCUMENT NAME MUST BE VALID TO PERFORM ADD FUNCTION
     H*  92      SCM0517-NOT AUTHORIZED TO DOCUMENT OR FOLDER
     H*  93      SCM0518-USER NOT ENROLLED IN OFFICE 400
     H*  94      SCM0531-UNABLE TO PROCESS REQUEST FOR THIS FOLDER
     H*  95      SCM0520-DOCUMENT NAME ALREADY EXISTS AS DOCUMENT OR FOLDER
     H*  96      SCM0521-FOLDER NOT FOUND
     H* SCR02:
     H*  24      WCR0501-ADDITIONAL INPUT FLAG MUST BY Y OR N
     H*  25      WCR0827-INVALID REMARK CODE ENTERED
     H*  27      WCR0503-DOCUMENT DESCRIPTION REQUIRED
     H*  28      WCR0513-INVALID TABLE VALUE FOUND IN FIELD
     H*  29      WCR0513-INVALID TABLE VALUE FOUND IN FIELD
     H*  33      POSITION CURSOR AT DOCUMENT DESCRIPTION
     H*  34      POSITION CURSOR AT REMARK TYPE
     H*  35      POSITION CURSOR AT AREA OF SERVICE
     H*  36      POSITION CURSOR AT SUB-AREA
     H*  37      SCM0614-REMARK TYPE IS INACTIVE
     H*****************************************************************
     FW2255F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMDEL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FWMDOL     UF A E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D PTL             S             40    DIM(4) CTDATA PERRCD(1)                PGM TITLES
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*** BEGIN ADD *** 05/29/98 **************************************
     D                 DS
     D  SCFLDR                 1     80
     D  SCFLD1                 1     60
     D  SCFLD2                61     80
     D  SCFLD3                 1     55
     D  SCFLD4                56     80
     D*** END ADD ***** 05/29/98 **************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  DEPT CODE TABLE FILE
     C*
     C     DEKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXDEPT
     C                   KFLD                    SCDPCD
     C*
     C*  DEPTS/DOC IDS FILE
     C*
     C     DOKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXDEPT
     C                   KFLD                    SCDPCD
     C                   KFLD                    SCFLDR
     C                   KFLD                    SCDCID
     C*
     C*
     C     *LIKE         DEFINE    DEDEPT        XXDEPT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQDEPT            2
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
     C                   MOVE      '0'           *IN64
     C                   MOVE      *BLANKS       QQRTCD
     C                   MOVE      QQDEPT        XXDEPT
     C     QQCALL        COMP      '1'                                    09
     C     QQCALL        COMP      '2'                                    70
     C*
     C                   MOVE      *BLANK        SCDPCD
     C                   MOVE      *BLANK        SCFLDR
     C                   MOVE      *BLANK        SCDCID
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
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C************************************
     C*   F3 = EXIT                      *
     C************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C************************************
     C*   F12 = CANCEL                   *
     C************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C                   END                                                    END *INKL
     C************************************
     C*   F5 = DEPT/DOC ID SEARCH        *
     C************************************
     C     *INKE         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C                   MOVEL     PTL(4)        QQPRDS
     C                   MOVE      *BLANKS       QQCALL
     C                   CALL      'W2321R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQDEPT            2
     C                   PARM                    SCDPCD            5
     C                   PARM                    SCFLDR           80
     C                   PARM                    SCDCID           12
     C                   PARM                    QQRTCD            1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTCD        CABEQ     '*'           SELTAG
     C                   GOTO      STRT
     C*
     C                   END                                                    END *INKE
     C************************************
     C*   F6 = DEPT SEARCH               *
     C************************************
     C     *INKF         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C                   MOVEL     PTL(1)        QQPRDS
     C                   CALL      'W2251R'
     C                   PARM                    SCDPCD
     C                   PARM                    QQPRDS
     C                   PARM                    QQRTC             1
     C                   PARM                    XXDEPT
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTC         IFEQ      'B'
     C     SCDPCD        OREQ      *BLANK
     C                   MOVE      '1'           *IN30
     C                   END
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN31
     C                   END
     C                   GOTO      STRT
     C*
     C                   END                                                    END *INKF
     C************************************
     C*   F7 = FOLDER SEARCH             *
     C************************************
     C     *INKG         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C*
     C                   MOVE      *BLANK        QQFLDR
     C                   MOVEL     SCFLDR        QQFLDR           80
     C                   MOVEL     *BLANK        QQRTCD            1            ERR RTN CODE
     C*
     C                   CALL      'SDSPFLRS'                                   "DSPFLR" OFC API
     C                   PARM                    QQFLDR                         FOLDER
     C                   PARM                    QQRTCD                         ERR ID CODE
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN81
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN82
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN83
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN84
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '5'
     C                   MOVE      '1'           *IN85
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '6'
     C                   MOVE      '1'           *IN86
     C                   GOTO      STRT
     C                   END
     C     QQFLDR        IFEQ      *BLANK                                       VALID DOC ID
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT                                         START
     C                   ELSE
     C                   MOVEL     QQFLDR        SCFLDR                         USE IT, GOTO
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT                                         START
     C                   END
     C*
     C                   END                                                    END *INKG
     C************************************
     C*   F8 = DOC ID SEARCH             *
     C************************************
     C     *INKH         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C*
     C*  MUST HAVE FOLDER NAME TO SEARCH FOR DOCUMENTS
     C*
     C     SCFLDR        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEL     *BLANK        XXDCID                         RETURN DOC ID
     C                   MOVEL     SCFLDR        XXFLDR           80            FOLDER
     C                   MOVEL     *BLANK        QQRTCD            1            ERR RTN CODE
     C*
     C                   CALL      'SDSPFLR'                                    "DSPFLR" OFC API
     C                   PARM                    XXFLDR                         FOLDER
     C                   PARM                    XXDCID           12            RTN DOC ID SLCT
     C                   PARM                    QQRTCD                         ERR ID CODE
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN91
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN92
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN93
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN94
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '5'
     C                   MOVE      '1'           *IN95
     C                   GOTO      STRT
     C                   END
     C     QQRTCD        IFEQ      '6'
     C                   MOVE      '1'           *IN96
     C                   GOTO      STRT
     C                   END
     C     XXDCID        IFEQ      *BLANK                                       VALID DOC ID
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT                                         START
     C                   ELSE
     C                   MOVEL     XXDCID        SCDCID                         USE IT, GOTO
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT                                         START
     C                   END
     C*
     C                   END                                                    END *INKH
     C************************************
     C*   F9 = REMARK TYPE SEARCH        *
     C************************************
     C     *INKI         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C***  Begin Chg  ***  10/11/06  **************************************
     C**********           CALL 'SC580R'
     C**********           PARM           SCRMTY
     C**********           PARM           QQRTC   1
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C***  End   Chg  ***  10/11/06  **************************************
     C*
     C***  Begin Chg  ***  10/11/06  **************************************
     C********** QQRTC     CABEQ'A'       TERM
     C     *INKC         CABEQ     'A'           TERM
     C***  End   Chg  ***  10/11/06  **************************************
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C***  Begin Chg  ***  10/11/06  **************************************
     C********** QQRTC     IFEQ 'B'
     C********** SCRMTY    OREQ *BLANK
     C**********           MOVE '1'       *IN34
     C**********           GOTO STRT
     C**********           END
     C********** QQRTC     IFEQ '*'
     C**********
     C**********           GOTO STRT
     C**********           END
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN34
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCRMTY
     C                   OTHER
     C                   MOVE      *ON           *IN34
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C***  End   Chg  ***  10/11/06  **************************************
     C*
     C                   END                                                    END *INKI
     C************************************
     C*   F10 = TABLE FILE SEARCH        *
     C************************************
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C*
     C                   END                                                    END *INKJ
     C************************************
     C*   *IN10 = PROCESS HELP REQUESTS  *
     C************************************
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
     C                   END                                                    END *IN10
     C********************************************
     C*   F20 = CHANGE STATUS                    *
     C********************************************
     C     *INKU         IFEQ      '1'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   MOVE      '1'           *IN64
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      '0'           *IN64
     C                   END
     C                   GOTO      STRT
     C                   END                                                     END *INKU
     C*
     C*   F23=DELETE CODE
     C     *INKX         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C                   DELETE    PDO01
     C                   MOVE      '0'           *IN64
     C                   MOVE      *BLANKS       SCDPCD
     C                   MOVE      *BLANKS       SCFLDR
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C****
     C                   END                                                    END CMDKEYS
     C*************************************************************************
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
     C*  THE DEPT CODE MUST BE VALID
     C*
     C     DEKEY         CHAIN     PDE01                              20
     C     *IN20         CABEQ     '1'           STRT
     C*
     C*** BEG DEL ***** 01/14/08 **************************************
     C******  THE FOLDER NAME MUST BE VALID
     C******
     C******     SCFLDR    IFEQ *BLANKS
     C******               MOVE '1'       *IN21
     C******               GOTO STRT
     C******               END                             END EQ *BLANKS
     C*** END DEL ***** 01/14/08 **************************************
     C*
     C*** BEG ADD ***** 01/14/08 **************************************
     C     SCFLDR        IFNE      *BLANKS
     C** IF NOT BLANKS VALIDATE FOLDER ID
     C                   SETOFF                                       42
     C*** END ADD ***** 01/14/08 **************************************
     C                   MOVE      ' '           QQRTCD            1
     C                   CALL      'SVALFLR'
     C                   PARM                    SCFLDR
     C                   PARM                    QQRTCD
     C*
     C*  FOLDER MUST EXIST
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END                                                    END EQ '1'
     C*
     C*  USER MUST HAVE AUTHORITY TO THE FOLDER
     C*
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END                                                    END EQ '2'
     C*
     C*  THE DOCUMENT ID MUST BE VALID
     C*
     C                   CALL      'SVALDOC'
     C                   PARM                    SCDCID
     C                   PARM                    SCFLDR
     C                   PARM                    QQRTCD
     C*
     C     QQRTCD        CABEQ     '1'           STRT                     23
     C*** BEG ADD ***** 01/14/08 **************************************
     C                   ELSE                                                   SCFLDR NE ' '
     C                   SETON                                        42
     C** IF BLANKS VALIDATE PROGRAM ID
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    SCDCID
     C                   PARM      '*PGM'        QQTYPE            7
     C                   PARM      ' '           QQRTCD
     C*
     C     QQRTCD        IFEQ      *ON
     C     SCDCID        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   ENDIF                                                  XXERR=*ON
     C*
     C                   ENDIF                                                  SCFLDR NE ' '
     C*** END ADD ***** 01/14/08 **************************************
     C*
     C     SELTAG        TAG
     C*
     C     DOKEY         CHAIN     PDO01                              40
     C     *IN40         IFEQ      '0'
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVEL     DODESC        SCDESC
     C                   MOVEL     DORMCD        SCRMTY
     C*** BEGIN ADD *** 05/28/98 **************************************
     C                   MOVEL     DOALCH        SCALCH
     C*** END ADD ***** 05/28/98 **************************************
     C*** BEGIN ADD *** 05/25/06 **************************************
     C                   MOVEL     DOINTR        SCINTR
     C*** END ADD ***** 05/25/06 **************************************
     C*
     C*  RETRIEVE AUTO-FOLDER BUILD SUB-AREA CODE ASSOCIATED WITH
     C*  REMARK TYPE TO DISPLAY
     C*
     C     DORMCD        CHAIN     PCT01                              60
     C                   MOVE      DOSTAT        SCSTAT
     C     SCSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN64                          ALLOW DELETE
     C                   ELSE                                                   |
     C                   MOVE      '0'           *IN64                          NO DELETING
     C                   END                                                    |
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVEL     *BLANKS       SCDESC
     C                   MOVEL     *BLANKS       SCRMTY
     C*** BEGIN ADD *** 05/28/98 **************************************
     C                   MOVEL     *BLANKS       SCALCH
     C*** END ADD ***** 05/28/98 **************************************
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      '0'           *IN64                          NO DELETING
     C*
     C                   END
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C*  THE DOCUMENT DESCRIPTION MUST BE ENTERED
     C*
     C     SCDESC        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END                                                    END EQ *BLANKS
     C*
     C*
     C*** RMV 9/1/98 *** 33WDQ *** BLANK FOR RMTY IS VALID (NO DARTS) ***
     C**9/1/98** SCRMTY    IFEQ *BLANKS
     C********             MOVE '1'       *IN25
     C*******              GOTO STRT
     C***END DEL 9/1/98*** END                             END EQ '1'
     C*** ADD 9/1/98 *** 33WDQ *** BLANK FOR RMTY IS VALID (NO DARTS) ***
     C     SCRMTY        IFNE      *BLANKS
     C     SCRMTY        CHAIN     PCT01                              25
     C     *IN25         CABEQ     '1'           STRT
     C     CTSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN37
     C                   GOTO      STRT
     C                   END
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 05/28/98 **************************************
     C*
     C*  USER MUST ENTER 'Y' OR 'N' FOR OPTION TO ALLOW CHANGES TO
     C*  RESOLVED DOCUMENTS.
     C*
     C*
     C*** ADD 9/1/98 *** 33WDQ *** BLANK FOR RMTY IS VALID (NO DARTS) ***
     C     SCRMTY        IFNE      *BLANKS
     C     SCALCH        IFEQ      *BLANKS
     C     SCALCH        ORNE      'Y'
     C     SCALCH        ANDNE     'N'
     C                   MOVE      '1'           *IN38
     C                   GOTO      STRT
     C                   END                                                    END EQ '1'
     C                   ENDIF
     C*** END ADD ***** 05/28/98 **************************************
     C*** ADD     ***** 05/25/06 **************************************
     C     SCINTR        IFNE      'I'
     C     SCINTR        ANDNE     'B'
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   ENDIF
     C*** END ADD ***** 05/28/98 **************************************
     C*
     C     DOKEY         CHAIN     PDO01                              40
     C                   MOVE      UPCO#         DOCO#
     C                   MOVEL     SCDPCD        DODPCD
     C                   MOVEL     SCFLDR        DOFLDR
     C                   MOVEL     SCDESC        DODESC
     C                   MOVEL     SCDCID        DODCID
      *** BEG DEL *** 04/10/98 ********************************
     C***                  MOVE 'N'       DOAIFL
      *** END DEL *** 04/10/98 ********************************
     C                   MOVEL     SCRMTY        DORMCD
     C                   MOVEL     CTTYPE        DORMTY
     C                   MOVEL     XXDEPT        DODEPT
     C                   MOVE      SCSTAT        DOSTAT
     C                   MOVEL     Q1USER        DOUSER
     C                   MOVEL     Q1PGM         DOPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DODT
     C*
     C                   Z-ADD     DSTIME        DOTIME
     C*** BEGIN ADD *** 05/28/98 **************************************
     C                   MOVEL(P)  SCALCH        DOALCH
     C*** END ADD ***** 05/28/98 **************************************
     C*** BEGIN ADD *** 05/25/06 **************************************
     C                   MOVEL(P)  SCINTR        DOINTR
     C*** END ADD ***** 05/25/06 **************************************
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PDO01
     C                   ELSE
     C                   MOVEL     Q1USER        DOIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DOIDT
     C*
     C*
     C                   WRITE     PDO01
     C                   END
     C                   MOVE      *BLANKS       SCDPCD
     C                   MOVE      *BLANKS       SCFLDR
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE PTL - PGM TITLES
         DEPARTMENT CODE SEARCH
         AREA OF SERVICE SEARCH
             SUB-AREA SEARCH
        DEPARTMENT/DOC ID SEARCH
