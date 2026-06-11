     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0085R - COVERAGE MAINTENANCE - GLASS ITEM
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program is the Coverage Maintenance pgm for       **
     H**     the Glass Item.
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
     FC0085F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FCMUPL     IF   E           K DISK
     FCDGIL     UF A E           K DISK
     FCMPML     IF   E           K DISK
     FCTABL     IF   E           K DISK
     F*

     D*===============================================================*
     D*     EXTENSION SECTION                                         *
     D*===============================================================*
     D*
     D* DEFINE ARRAYS
     D*
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D KEYI            S              5  0 DIM(2)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  IT                     1     70  0
     D                                     DIM(14) ASCEND
     D  SCIT01                 1      5  0
     D  SCIT02                 6     10  0
     D  SCIT03                11     15  0
     D  SCIT04                16     20  0
     D  SCIT05                21     25  0
     D  SCIT06                26     30  0
     D  SCIT07                31     35  0
     D  SCIT08                36     40  0
     D  SCIT09                41     45  0
     D  SCIT10                46     50  0
     D  SCIT11                51     55  0
     D  SCIT12                56     60  0
     D  SCIT13                61     65  0
     D  SCIT14                66     70  0
     D                 DS
     D  NP                     1     42  0
     D                                     DIM(14) ASCEND
     D  SCNP01                 1      3  0
     D  SCNP02                 4      6  0
     D  SCNP03                 7      9  0
     D  SCNP04                10     12  0
     D  SCNP05                13     15  0
     D  SCNP06                16     18  0
     D  SCNP07                19     21  0
     D  SCNP08                22     24  0
     D  SCNP09                25     27  0
     D  SCNP10                28     30  0
     D  SCNP11                31     33  0
     D  SCNP12                34     36  0
     D  SCNP13                37     39  0
     D  SCNP14                40     42  0
     D                 DS
     D  LN                     1     42  0
     D                                     DIM(14) ASCEND
     D  SCLN01                 1      3  0
     D  SCLN02                 4      6  0
     D  SCLN03                 7      9  0
     D  SCLN04                10     12  0
     D  SCLN05                13     15  0
     D  SCLN06                16     18  0
     D  SCLN07                19     21  0
     D  SCLN08                22     24  0
     D  SCLN09                25     27  0
     D  SCLN10                28     30  0
     D  SCLN11                31     33  0
     D  SCLN12                34     36  0
     D  SCLN13                37     39  0
     D  SCLN14                40     42  0
     D                 DS
     D  WD                     1     42  0
     D                                     DIM(14) ASCEND
     D  SCWD01                 1      3  0
     D  SCWD02                 4      6  0
     D  SCWD03                 7      9  0
     D  SCWD04                10     12  0
     D  SCWD05                13     15  0
     D  SCWD06                16     18  0
     D  SCWD07                19     21  0
     D  SCWD08                22     24  0
     D  SCWD09                25     27  0
     D  SCWD10                28     30  0
     D  SCWD11                31     33  0
     D  SCWD12                34     36  0
     D  SCWD13                37     39  0
     D  SCWD14                40     42  0
     D                 DS
     D  DD                     1    560
     D                                     DIM(14) ASCEND
     D  SCDD01                 1     40
     D  SCDD02                41     80
     D  SCDD03                81    120
     D  SCDD04               121    160
     D  SCDD05               161    200
     D  SCDD06               201    240
     D  SCDD07               241    280
     D  SCDD08               281    320
     D  SCDD09               321    360
     D  SCDD10               361    400
     D  SCDD11               401    440
     D  SCDD12               441    480
     D  SCDD13               481    520
     D  SCDD14               521    560
     D                 DS
     D  DT                     1    112  0
     D                                     DIM(14) ASCEND
     D  SCDT01               001    008  0
     D  SCDT02               009    016  0
     D  SCDT03               017    024  0
     D  SCDT04               025    032  0
     D  SCDT05               033    040  0
     D  SCDT06               041    048  0
     D  SCDT07               049    056  0
     D  SCDT08               057    064  0
     D  SCDT09               065    072  0
     D  SCDT10               073    080  0
     D  SCDT11               081    088  0
     D  SCDT12               089    096  0
     D  SCDT13               097    104  0
     D  SCDT14               105    112  0
     D                 DS
     D  ST                     1     14
     D                                     DIM(14) ASCEND
     D  SCST01                 1      1
     D  SCST02                 2      2
     D  SCST03                 3      3
     D  SCST04                 4      4
     D  SCST05                 5      5
     D  SCST06                 6      6
     D  SCST07                 7      7
     D  SCST08                 8      8
     D  SCST09                 9      9
     D  SCST10                10     10
     D  SCST11                11     11
     D  SCST12                12     12
     D  SCST13                13     13
     D  SCST14                14     14
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
     C*  GLASS ITEM FILE
     C*
     C     GIKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCCC
     C                   KFLD                    SCLOC#
     C                   KFLD                    SCPRM#
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXITM#            5 0
     C*
     C     GIKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCCC
     C                   KFLD                    SCLOC#
     C                   KFLD                    SCPRM#
     C                   KFLD                    XXCLAS
     C*
     C*  TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQPOL#           13
     C                   PARM                    QQCC              2
     C                   PARM                    QQLOC#            3
     C                   PARM                    QQPRM#            3
     C                   PARM                    QQCLAS            6
     C                   PARM                    QQTRDT            6
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
     C                   MOVEL     QQCC          SCCC
     C                   MOVE      QQLOC#        SCLOC#
     C                   MOVE      QQPRM#        SCPRM#
     C                   MOVE      QQCLAS        SCCLAS
     C                   MOVE      QQCLAS        XXCLAS            6
     C                   MOVE      QQTRDT        XXTRDT            6 0
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
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*  GET THE POLICY NAME FROM POLICY MASTER FILE
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
     C*   GET SECTION CODE DESCRIPTION
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCCC          XXKEY
     C     TBKEY         CHAIN     PCV01                              60
     C                   MOVEL     CVDESC        SCCODS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C     *IN09         IFEQ      '1'
     C                   MOVE      'X'           SCUPDT
     C                   GOTO      SCR01T
     C                   ELSE
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
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
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C     XXUP          IFEQ      '1'
     C                   MOVE      *BLANKS       SCUP
     C                   ELSE
     C                   MOVEL     RR(1)         SCUP
     C                   END
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
     C                   END
     C                   EXFMT     SCR03
     C                   END
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
     C                   GOTO      STRT
     C                   END
     C     Q2RECF        IFEQ      'SCR03'
     C     *IN09         ANDEQ     '0'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  FUNCTION 12 = CHANGE ITEM STATUS
     C*
     C     *INKB         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       8             I1                3 0
     C*
     C*  VALID CURSOR POSITION - CHANGE ITEM STATUS
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     14
     C     IT(I1)        ANDNE     *ZERO
     C     ST(I1)        ANDNE     'A'
     C*
     C     I1            ADD       69            QX                5 0
     C                   MOVE      '0'           *IN(QX)                        PROTECT OFF
     C                   MOVE      IT(I1)        XXITM#
     C     GIKEY         CHAIN     PGI01                              60
     C                   Z-ADD     GIEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C                   MOVE      'A'           ST(I1)
     C                   MOVE      'A'           GISTAT
     C                   Z-ADD     *ZERO         GIIADT
     C                   UPDATE    PGI01
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      '1'           *IN16
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END
     C*
     C*   ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR03T
     C*
     C*   ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR03T
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
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  PICK ADD OR UPDATE
     C*
     C     SCUPDT        IFEQ      *BLANK
     C     SCADD         ANDEQ     *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*   GOTO APPROPIATE PROCEDURE
     C*
     C     SCUPDT        IFNE      *BLANK
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   MOVE      UA(2)         SCMODE
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C                   MOVE      *BLANK        SCITM#
     C                   GOTO      REPOS
     C                   ELSE
     C*
     C*   LOAD BLANKS ONTO SCREEN FOR ADD MODE
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       NP
     C                   MOVE      *BLANKS       LN
     C                   MOVE      *BLANKS       WD
     C                   MOVE      *BLANKS       DD
     C*
     C*   LOAD NEW ITEM NUMBERS
     C*
     C     GIKY1         SETGT     PGI01
     C                   READP     PGI01                                  60
     C     *IN60         IFEQ      *OFF
     C                   EXCEPT    PGI01E
     C     GICO#         IFNE      UPCO#
     C     GIPOL#        ORNE      SCPOL#
     C     GICC          ORNE      SCCC
     C     GILOC#        ORNE      SCLOC#
     C     GIPRM#        ORNE      SCPRM#
     C     GICLAS        ORNE      XXCLAS
     C                   MOVE      *ON           *IN60
     C                   END
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     1             XXNUM             5 0
     C                   ELSE
     C     GIITM#        ADD       1             XXNUM
     C                   END
     C*
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLE     14
     C                   Z-ADD     XXNUM         IT(X)
     C                   Z-ADD     XXTRDT        DT(X)
     C                   MOVE      'A'           ST(X)
     C                   ADD       1             XXNUM
     C     XXNUM         IFGT      99999
     C                   Z-ADD     14            X
     C                   END
     C                   ADD       1             X
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     SCR02T        TAG
     C*
     C* BEGIN EDIT CHECKING
     C*
     C                   MOVE      '0'           *IN43                          NO BLK LINE
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLE     14
     C     NP(X)         IFGT      *ZERO
     C*
     C*   FLAG BLANK LINES BETWEEN ITEMS - MAXIMIZE ITEM # ORDER
     C*
     C     *IN43         IFEQ      '1'
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   END
     C*
     C*   LENGTH CANNOT BE BLANK
     C*
     C     LN(X)         IFEQ      *ZERO
     C     X             ADD       19            A                 2 0
     C                   MOVE      '1'           *IN(A)
     C                   MOVE      '1'           *IN35
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C*   WIDTH CANNOT BE BLANK
     C*
     C     WD(X)         IFEQ      *ZERO
     C     X             ADD       19            B                 2 0
     C                   MOVE      '1'           *IN(B)
     C                   MOVE      '1'           *IN36
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C*   DESCRIPTION CANNOT BE BLANK
     C*
     C     DD(X)         IFEQ      *BLANK
     C     X             ADD       19            C                 2 0
     C                   MOVE      '1'           *IN(C)
     C                   MOVE      '1'           *IN37
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE EFFECTIVE DATE
     C*
     C                   MOVE      *BLANKS       SRDTE             6
     C                   MOVE      ' '           SRERR             1
     C                   MOVE      ' '           SRTERM            1
     C                   Z-ADD     DT(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         DT(X)
     C     SRERR         IFEQ      '1'
     C     X             ADD       19            D                 2 0
     C                   MOVE      '1'           *IN(D)
     C                   MOVE      '1'           *IN38
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C                   ELSE
     C*
     C*   CHECK IF ITEM HAS A DESCRIPTION OR ID# AND FLAG ZERO VALUE
     C*
     C     DD(X)         IFNE      *BLANK
     C     LN(X)         ORNE      *ZERO
     C     WD(X)         ORNE      *ZERO
     C     X             ADD       19            E                 2 0
     C                   MOVE      '1'           *IN(E)
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      '1'           *IN43                          IND BLK LINE
     C                   END
     C                   ADD       1             X
     C                   END
     C*
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C*   WRITE RECORD WITH VALUE
     C*
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLE     14
     C     NP(X)         ANDGT     *ZERO
     C*
     C                   MOVE      UPCO#         GICO#
     C                   MOVE      SCPOL#        GIPOL#
     C                   MOVE      SCCC          GICC
     C                   MOVE      SCLOC#        GILOC#
     C                   MOVE      SCPRM#        GIPRM#
     C                   MOVE      SCCLAS        GICLAS
     C                   MOVE      IT(X)         GIITM#
     C                   MOVE      NP(X)         GIPLTS
     C                   MOVE      LN(X)         GILEN
     C                   MOVE      WD(X)         GIWIDH
     C                   MOVEL     DD(X)         GIDESC
     C                   Z-ADD     DT(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GIEFDT
     C                   MOVE      'A'           GISTAT
     C                   MOVE      Q1USER        GIUSER
     C                   MOVE      Q1PGM         GIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GIDT
     C                   Z-ADD     DSTIME        GITIME
     C*
     C                   WRITE     PGI01
     C*
     C                   ADD       1             X
     C                   END
     C*
     C     NP(14)        IFNE      *ZERO
     C                   GOTO      SCR01T
     C                   ELSE
     C                   MOVE      *BLANK        SCADD
     C                   MOVE      *BLANK        SCUPDT
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*   SCR03T - BEGIN EDITING UPDATE SCREEN
     C*
     C     SCR03T        TAG
     C*
     C*   VALIDATE UPDATE SCREEN AND THEN PROCESS REQUEST
     C*
     C     *IN09         IFEQ      '0'
     C                   EXSR      VALSC
     C                   END
     C*
     C*  INVALID ROLL REQUEST
     C*
     C     *IN18         IFEQ      *ON
     C     XXUP          ANDNE     *BLANK
     C                   MOVE      '1'           *IN15
     C                   GOTO      STRT
     C                   END
     C*
     C     *IN19         IFEQ      *ON
     C     XXDOWN        ANDNE     *BLANK
     C                   MOVE      '1'           *IN15
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVEA     *ALL'0'       *IN(70)
     C                   MOVE      *BLANK        XXDOWN
     C                   Z-ADD     KEYI(2)       XXITM#
     C     GIKEY         CHAIN     PGI01                              60
     C     *IN60         IFEQ      *OFF
     C                   EXCEPT    PGI01E
     C                   END
     C                   EXSR      LOAD
     C                   EXSR      PROT
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVEA     *ALL'0'       *IN(70)
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYI(1)       XXITM#
     C     GIKEY         CHAIN     PGI01                              60
     C     *IN60         IFEQ      *OFF
     C                   EXCEPT    PGI01E
     C                   END
     C                   EXSR      LOADP
     C                   EXSR      PROT
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCITM#        IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVEA     *ALL'0'       *IN(70)
     C                   Z-ADD     *ZERO         KEYI
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVE      SCITM#        XXITM#
     C                   MOVE      *BLANKS       SCITM#
     C     GIKEY         SETLL     PGI01
     C     GIKY1         READE     PGI01                                  60
     C     *IN60         IFEQ      *OFF
     C                   EXCEPT    PGI01E
     C                   END
     C                   EXSR      LOAD
     C                   EXSR      PROT
     C                   GOTO      STRT
     C                   END
     C*
     C*   IF ENTER KEY PRESSED GOTO SCREEN 1
     C*
     C     *IN09         IFEQ      '0'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCUPDT
     C                   MOVE      *BLANK        SCADD
     C                   GOTO      STRT
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       IT
     C                   MOVE      *BLANKS       NP
     C                   MOVE      *BLANKS       LN
     C                   MOVE      *BLANKS       WD
     C                   MOVE      *BLANKS       DD
     C                   MOVE      *BLANKS       DT
     C                   MOVE      *BLANKS       ST
     C                   Z-ADD     *ZERO         I1                3 0
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   Z-ADD     GIITM#        IT(I1)
     C                   Z-ADD     GIPLTS        NP(I1)
     C                   Z-ADD     GILEN         LN(I1)
     C                   Z-ADD     GIWIDH        WD(I1)
     C                   MOVEL     GIDESC        DD(I1)
     C                   MOVE      GISTAT        ST(I1)
     C     GISTAT        IFEQ      'I'
     C     I1            ADD       69            QX
     C                   MOVE      '1'           *IN(QX)
     C                   Z-ADD     GIIADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C                   ELSE
     C                   Z-ADD     GIEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C                   END
     C                   ENDSR
     C*
     C*  LOAD FORWARD SUBROUTINE
     C*
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C     OVER          TAG
     C*
     C*  END OF FILE
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONE
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     GIKY1         READE     PGI01                                  60
     C     *IN60         IFEQ      *OFF
     C                   EXCEPT    PGI01E
     C                   ELSE
     C                   DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C                   END
     C     I1            COMP      14                                   61
     C     *IN61         CABEQ     *ON           OVER
     C     DONE          TAG
     C                   MOVE      IT(1)         KEYI(1)
     C                   MOVE      IT(14)        KEYI(2)
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     15            I1
     C     OVERP         TAG
     C*
     C*  BEGINNING OF FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   EXSR      LOADB
     C                   GOTO      DONEP
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   SUB       1             I1
     C                   EXSR      FILL
     C                   READP     PGI01                                  60
     C     *IN60         IFEQ      *OFF
     C                   EXCEPT    PGI01E
     C     GICO#         IFNE      UPCO#
     C     GIPOL#        ORNE      SCPOL#
     C     GICC          ORNE      SCCC
     C     GILOC#        ORNE      SCLOC#
     C     GIPRM#        ORNE      SCPRM#
     C     GICLAS        ORNE      XXCLAS
     C                   MOVE      *ON           *IN60
     C                   END
     C                   END
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C     *IN61         CABEQ     *ON           OVERP
     C     DONEP         TAG
     C                   MOVE      IT(1)         KEYI(1)
     C                   MOVE      IT(14)        KEYI(2)
     C                   ENDSR
     C*
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     *ZERO         XXITM#
     C     GIKEY         SETLL     PGI01
     C     GIKY1         READE     PGI01                                  60
     C     *IN60         IFEQ      *OFF
     C                   EXCEPT    PGI01E
     C                   ELSE
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     GIKY1         READE     PGI01                                  60
     C     *IN60         IFEQ      *OFF
     C                   EXCEPT    PGI01E
     C                   ELSE
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      14                                   61
     C     *IN61         CABEQ     *ON           OVERB
     C     DONEB         ENDSR
     C*
     C*   SUBROUTINE TO VALIDATE UPDATE SCREEN
     C*
     C     VALSC         BEGSR
     C*
     C                   Z-ADD     1             X
     C     X             DOWLE     14
     C     IT(X)         ANDNE     *ZERO
     C*
     C*   NUMBER OF PLANTE CANNOT BE LESS THAN OR EQUAL TO ZERO
     C*
     C     NP(X)         IFLE      *ZERO
     C     X             ADD       19            A                 2 0
     C                   MOVE      '1'           *IN(A)
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C*   LENGTH CANNOT BE LESS THAN OR EQUAL TO ZERO
     C*
     C     LN(X)         IFLE      *ZERO
     C     X             ADD       19            B                 2 0
     C                   MOVE      '1'           *IN(B)
     C                   MOVE      '1'           *IN35
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C*   WIDTH CANNOT BE LESS THAN OR EQUAL TO ZERO
     C*
     C     WD(X)         IFLE      *ZERO
     C     X             ADD       19            C                 2 0
     C                   MOVE      '1'           *IN(C)
     C                   MOVE      '1'           *IN36
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C*   DESCRIPTION CANNOT BE BLANK
     C*
     C     DD(X)         IFEQ      *BLANK
     C     X             ADD       19            D                 2 0
     C                   MOVE      '1'           *IN(D)
     C                   MOVE      '1'           *IN37
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE EFFECTIVE DATE
     C*
     C                   MOVE      *BLANKS       SRDTE             6
     C                   MOVE      ' '           SRERR             1
     C                   MOVE      ' '           SRTERM            1
     C                   Z-ADD     DT(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         DT(X)
     C     SRERR         IFEQ      '1'
     C     X             ADD       19            E                 2 0
     C                   MOVE      '1'           *IN(E)
     C                   MOVE      '1'           *IN38
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C*   STATUS MUST BE "A" ACTIVE OR "I" INACTIVE
     C*
     C     ST(X)         IFNE      'I'
     C     ST(X)         ANDNE     'A'
     C     X             ADD       19            F                 2 0
     C                   MOVE      '1'           *IN(F)
     C                   MOVE      '1'           *IN39
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C                   ADD       1             X
     C                   END
     C*
     C*   ALL EDIT HAS PASSED - UPDATE FILE
     C*
     C                   Z-ADD     1             X
     C     X             DOWLE     14
     C     IT(X)         ANDNE     *ZERO
     C*
     C                   MOVE      IT(X)         XXITM#
     C*
     C     GIKEY         CHAIN     PGI01                              60
     C*
     C                   MOVE      UPCO#         GICO#
     C                   MOVE      SCPOL#        GIPOL#
     C                   MOVE      SCCC          GICC
     C                   MOVE      SCLOC#        GILOC#
     C                   MOVE      SCPRM#        GIPRM#
     C                   MOVE      SCCLAS        GICLAS
     C                   MOVE      IT(X)         GIITM#
     C                   Z-ADD     NP(X)         GIPLTS
     C                   Z-ADD     LN(X)         GILEN
     C                   Z-ADD     WD(X)         GIWIDH
     C                   MOVEL     DD(X)         GIDESC
     C                   MOVE      ST(X)         GISTAT
     C     ST(X)         IFEQ      'I'
     C                   Z-ADD     DT(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GIIADT
     C                   ELSE
     C                   Z-ADD     DT(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GIEFDT
     C                   END
     C                   MOVE      Q1USER        GIUSER
     C                   MOVE      Q1PGM         GIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GIDT
     C                   Z-ADD     DSTIME        GITIME
     C*
     C                   UPDATE    PGI01
     C*
     C                   ADD       1             X
     C                   END
     C*
     C                   ENDSR
     C*
     C*   SUBROUTINE TO PROTECT EMPTY LINES FOR UPDATE MODE
     C*
     C     PROT          BEGSR
     C*
     C                   Z-ADD     1             X
     C     X             DOWLE     14
     C     IT(X)         IFEQ      *ZERO
     C     X             ADD       69            QX
     C                   MOVE      '1'           *IN(QX)
     C                   END
     C                   ADD       1             X
     C                   END
     C*
     C                   ENDSR
     C*
     C*
     OPGI01     E            PGI01E
**
ROLL UP
ROLL DOWN
**  TABLE UA
-- ADD --
- UPDATE -
