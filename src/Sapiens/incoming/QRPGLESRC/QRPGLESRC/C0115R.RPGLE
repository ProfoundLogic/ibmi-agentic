     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0115R - COVERAGE MAINTENANCE - INLAND MARINE ITEM
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program is the Coverage Maintenance Pgm for the   **
     H**     Inland Marine Item.                                    **
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
     FC0115F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FCMUPL     IF   E           K DISK
     FCDIIL     UF A E           K DISK
     FCMAIL     IF   E           K DISK
     FCMPML     IF   E           K DISK
     FCTABL     IF   E           K DISK
     F*

     D*===============================================================*
     D*     EXTENSION SECTION                                         *
     D*===============================================================*
     D*
     D* DEFINE ARRAYS
     D*
     D A1              S              2    DIM(14)
     D A2              S              2    DIM(14)
     D A3              S              2    DIM(14)
     D A4              S              2    DIM(14)
     D A5              S              2    DIM(14)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D KEYI            S              5  0 DIM(2)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  IT                     1     70  0
     D                                     DIM(14)
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
     D  DD                     1    490
     D                                     DIM(14)
     D  SCDD01                 1     35
     D  SCDD02                36     70
     D  SCDD03                71    105
     D  SCDD04               106    140
     D  SCDD05               141    175
     D  SCDD06               176    210
     D  SCDD07               211    245
     D  SCDD08               246    280
     D  SCDD09               281    315
     D  SCDD10               316    350
     D  SCDD11               351    385
     D  SCDD12               386    420
     D  SCDD13               421    455
     D  SCDD14               456    490
     D                 DS
     D  ID                     1    210
     D                                     DIM(14)
     D  SCID01                 1     15
     D  SCID02                16     30
     D  SCID03                31     45
     D  SCID04                46     60
     D  SCID05                61     75
     D  SCID06                76     90
     D  SCID07                91    105
     D  SCID08               106    120
     D  SCID09               121    135
     D  SCID10               136    150
     D  SCID11               151    165
     D  SCID12               166    180
     D  SCID13               181    195
     D  SCID14               196    210
     D                 DS
     D  VL                     1    112  0
     D                                     DIM(14)
     D  SCVL01                 1      8  0
     D  SCVL02                 9     16  0
     D  SCVL03                17     24  0
     D  SCVL04                25     32  0
     D  SCVL05                33     40  0
     D  SCVL06                41     48  0
     D  SCVL07                49     56  0
     D  SCVL08                57     64  0
     D  SCVL09                65     72  0
     D  SCVL10                73     80  0
     D  SCVL11                81     88  0
     D  SCVL12                89     96  0
     D  SCVL13                97    104  0
     D  SCVL14               105    112  0
     D                 DS
     D  DT                     1    112  0
     D                                     DIM(14)
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
     D                                     DIM(14)
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
     D  AI                     1     10
     D                                     DIM(5)
     D  SCAI01                 1      2
     D  SCAI02                 3      4
     D  SCAI03                 5      6
     D  SCAI04                 7      8
     D  SCAI05                 9     10
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
     C*  INLAND MARINE ITEM FILE
     C*
     C     IIKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCCC
     C                   KFLD                    SCLOC#
     C                   KFLD                    SCPRM#
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXITM#            5 0
     C*
     C     IIKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCCC
     C                   KFLD                    SCLOC#
     C                   KFLD                    SCPRM#
     C                   KFLD                    XXCLAS
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
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQPOL#           13
     C                   PARM                    QQCC              2
     C                   PARM                    QQLOC#            3
     C                   PARM                    QQPRM#            3
     C                   PARM                    QQCVBO            2
     C                   PARM                    QQTRDT            8
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
     C                   MOVE      QQCVBO        SCCVBO
     C                   MOVE      *BLANKS       XXCLAS            6
     C                   MOVE      QQTRDT        XXTRDT            8 0
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
     C                   MOVEL     PMFIN         SCFIN
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
     C     Q2RECF        IFEQ      'SCR04'
     C                   EXFMT     SCR04
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
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C                   MOVE      *BLANK        SCITM#
     C                   MOVEL     XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  FUNCTION 07 = CALL ADDITIONAL INSURED MAINTENANCE
     C*
     C     *INKG         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR04'
     C                   MOVE      SCPOL#        QQPOL#           13
     C                   CALL      'C0130R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*  FUNCTION 9 = ADD ADDITIONAL INTEREST SCREEN - SCREEN 2 & 3
     C*               ADDITIONAL INTEREST SEARCH - SCREEN 4
     C*
     C     *INKI         IFEQ      '1'
     C*
     C*   ADD ADDITIONAL INTEREST SCREEN
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR03'
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
     C*
     C*   FLAG ERROR IF ITEM DOES NOT HAVE A DESCRIPTION
     C*
     C     DD(I1)        IFEQ      *BLANK
     C                   MOVE      '1'           *IN99
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVE      A1(I1)        SCAI01
     C                   MOVE      A2(I1)        SCAI02
     C                   MOVE      A3(I1)        SCAI03
     C                   MOVE      A4(I1)        SCAI04
     C                   MOVE      A5(I1)        SCAI05
     C                   MOVE      DD(I1)        SCDESC
     C                   MOVE      ID(I1)        SCID#
     C                   MOVE      VL(I1)        SCVAL
     C                   MOVE      DT(I1)        SCEFDT
     C                   MOVE      IT(I1)        SCITM#
     C                   MOVE      ST(I1)        SCSTAT
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ELSE
     C                   MOVE      '1'           *IN99
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C                   END
     C*
     C*   ADDITIONAL INTEREST SEARCH - SCREEN 4
     C*
     C     Q2RECF        IFEQ      'SCR04'
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
     C                   MOVEL     'SCR04'       Q2RECF
     C     QQRTC         IFEQ      'B'
     C     QQADI         OREQ      *BLANK
     C     Z             ADD       30            Y                 2 0
     C                   MOVE      '1'           *IN(Y)
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C     Z             ADD       31            Y
     C                   MOVE      '1'           *IN(Y)
     C                   GOTO      STRT
     C                   END
     C*
     C                   ELSE
     C                   GOTO      STRT
     C                   END
     C*
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
     C     IIKEY         CHAIN     PII01                              60
     C                   Z-ADD     IIEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C                   MOVE      'A'           ST(I1)
     C                   MOVE      'A'           IISTAT
     C                   Z-ADD     *ZERO         IIIADT
     C                   UPDATE    PII01
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
     C     Q2RECF        CABEQ     'SCR04'       SCR04T
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
     C                   MOVE      *BLANKS       DD
     C                   MOVE      *BLANKS       ID
     C                   MOVE      *BLANKS       VL
     C                   MOVE      *BLANKS       A1
     C                   MOVE      *BLANKS       A2
     C                   MOVE      *BLANKS       A3
     C                   MOVE      *BLANKS       A4
     C                   MOVE      *BLANKS       A5
     C*
     C*   LOAD NEW ITEM NUMBERS
     C*
     C     IIKY1         SETGT     PII01
     C                   READP     PII01                                  60
     C     *IN60         IFEQ      *OFF
     C                   EXCEPT    PII01E
     C     IICO#         IFNE      UPCO#
     C     IIPOL#        ORNE      SCPOL#
     C     IICC          ORNE      SCCC
     C     IILOC#        ORNE      SCLOC#
     C     IIPRM#        ORNE      SCPRM#
     C     IICLAS        ORNE      XXCLAS
     C                   MOVE      *ON           *IN60
     C                   END
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     1             XXNUM             5 0
     C                   ELSE
     C     IIITM#        ADD       1             XXNUM
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
     C                   MOVE      '0'           *IN41                          NO BLK LINE
     C                   MOVE      '0'           *IN38                          NO BLK LINE
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLE     14
     C     VL(X)         IFGT      *ZERO
     C*
     C*   FLAG BLANK LINES BETWEEN ITEMS - MAXIMIZE ITEM # ORDER
     C*
     C     *IN41         IFEQ      '1'
     C                   MOVE      '1'           *IN38
     C                   GOTO      STRT
     C                   END
     C*
     C*   DESCRIPTION CANNOT BE BLANK
     C*
     C     DD(X)         IFEQ      *BLANK
     C     X             ADD       19            A                 2 0
     C                   MOVE      '1'           *IN(A)
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE EFFECTIVE DATE
     C*
     C                   Z-ADD     DT(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         DT(X)
     C     SRERR         IFEQ      '1'
     C     X             ADD       19            C                 2 0
     C                   MOVE      '1'           *IN(C)
     C                   MOVE      '1'           *IN37
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C                   ELSE
     C*
     C*   CHECK IF ITEM HAS A DESCRIPTION OR ID# AND FLAG ZERO VALUE
     C*
     C     DD(X)         IFNE      *BLANK
     C     ID(X)         ORNE      *BLANK
     C     X             ADD       19            D                 2 0
     C                   MOVE      '1'           *IN(D)
     C                   MOVE      '1'           *IN36
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      '1'           *IN41                          IND BLK LINE
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
     C     VL(X)         ANDGT     *ZERO
     C*
     C                   MOVE      UPCO#         IICO#
     C                   MOVE      SCPOL#        IIPOL#
     C                   MOVE      SCCC          IICC
     C                   MOVE      SCLOC#        IILOC#
     C                   MOVE      SCPRM#        IIPRM#
     C                   MOVE      XXCLAS        IICLAS
     C                   MOVE      IT(X)         IIITM#
     C                   MOVEL     DD(X)         IIDS35
     C                   MOVEL     ID(X)         IIID#
     C                   MOVE      VL(X)         IIVAL
     C                   Z-ADD     DT(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IIEFDT
     C                   MOVE      A1(X)         IIAI01
     C                   MOVE      A2(X)         IIAI02
     C                   MOVE      A3(X)         IIAI03
     C                   MOVE      A4(X)         IIAI04
     C                   MOVE      A5(X)         IIAI05
     C                   Z-ADD     *ZERO         IIIADT
     C                   MOVE      'A'           IISTAT
     C                   MOVE      Q1USER        IIUSER
     C                   MOVE      Q1PGM         IIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IIDT
     C                   Z-ADD     DSTIME        IITIME
     C*
     C                   WRITE     PII01
     C*
     C                   ADD       1             X
     C                   END
     C*
     C     VL(14)        IFNE      *ZERO
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
     C                   EXSR      VALSC
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
     C     IIKEY         CHAIN     PII01                              60
     C     *IN60         IFEQ      *OFF
     C                   EXCEPT    PII01E
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
     C     IIKEY         CHAIN     PII01                              60
     C     *IN60         IFEQ      *OFF
     C                   EXCEPT    PII01E
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
     C     IIKEY         SETLL     PII01
     C     IIKY1         READE     PII01                                  60
     C     *IN60         IFEQ      *OFF
     C                   EXCEPT    PII01E
     C                   END
     C                   EXSR      LOAD
     C                   EXSR      PROT
     C                   GOTO      STRT
     C                   END
     C*
     C*   IF ENTER KEY PRESSED GOTO SCREEN 1
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCUPDT
     C                   MOVE      *BLANK        SCADD
     C                   GOTO      STRT
     C*
     C*   PROCESS SCREEN 4
     C*
     C     SCR04T        TAG
     C*
     C*   VALIDATE ADDITIONAL INTERESTS
     C*
     C                   Z-ADD     1             I                 5 0
     C     I             DOWLE     5
     C     AI(I)         IFNE      *BLANK
     C*
     C                   MOVEL     AI(I)         XXADI
     C     AIKEY         CHAIN     PAI01                              60
     C     *IN60         IFEQ      '1'
     C     I             ADD       19            J                 5 0
     C                   MOVE      '1'           *IN(J)
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   ADD       1             I
     C                   END
     C*
     C*   UPDATE ARRAYS AND RETURN TO LAST SCREEN
     C*
     C                   MOVE      SCAI01        A1(I1)
     C                   MOVE      SCAI02        A2(I1)
     C                   MOVE      SCAI03        A3(I1)
     C                   MOVE      SCAI04        A4(I1)
     C                   MOVE      SCAI05        A5(I1)
     C                   MOVE      *BLANK        SCITM#
     C                   MOVEL     XXRECF        Q2RECF
     C                   GOTO      STRT
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
     C                   MOVE      *BLANKS       DD
     C                   MOVE      *BLANKS       ID
     C                   MOVE      *BLANKS       VL
     C                   MOVE      *BLANKS       DT
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       A1
     C                   MOVE      *BLANKS       A2
     C                   MOVE      *BLANKS       A3
     C                   MOVE      *BLANKS       A4
     C                   MOVE      *BLANKS       A5
     C                   Z-ADD     *ZERO         I1                3 0
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   Z-ADD     IIITM#        IT(I1)
     C                   MOVEL     IIDS35        DD(I1)
     C                   MOVEL     IIID#         ID(I1)
     C                   Z-ADD     IIVAL         VL(I1)
     C                   MOVE      IISTAT        ST(I1)
     C                   MOVE      IIAI01        A1(I1)
     C                   MOVE      IIAI02        A2(I1)
     C                   MOVE      IIAI03        A3(I1)
     C                   MOVE      IIAI04        A4(I1)
     C                   MOVE      IIAI05        A5(I1)
     C     IISTAT        IFEQ      'I'
     C     I1            ADD       69            QX
     C                   MOVE      '1'           *IN(QX)
     C                   Z-ADD     IIIADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C                   ELSE
     C                   Z-ADD     IIEFDT        SRDTI
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
     C     IIKY1         READE     PII01                                  60
     C     *IN60         IFEQ      *OFF
     C                   EXCEPT    PII01E
     C                   ELSE
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
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
     C                   READP     PII01                                  60
     C     *IN60         IFEQ      *OFF
     C                   EXCEPT    PII01E
     C     IICO#         IFNE      UPCO#
     C     IIPOL#        ORNE      SCPOL#
     C     IICC          ORNE      SCCC
     C     IILOC#        ORNE      SCLOC#
     C     IIPRM#        ORNE      SCPRM#
     C     IICLAS        ORNE      XXCLAS
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
     C     IIKEY         SETLL     PII01
     C     IIKY1         READE     PII01                                  60
     C     *IN60         IFEQ      *OFF
     C                   EXCEPT    PII01E
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
     C     IIKY1         READE     PII01                                  60
     C     *IN60         IFEQ      *OFF
     C                   EXCEPT    PII01E
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
     C*   DESCRIPTION CANNOT BE BLANK
     C*
     C     DD(X)         IFEQ      *BLANK
     C     X             ADD       19            A                 2 0
     C                   MOVE      '1'           *IN(A)
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALUE CANNOT BE LESS THAN OR EQUAL TO ZERO
     C*
     C     VL(X)         IFLE      *ZERO
     C     X             ADD       19            D                 2 0
     C                   MOVE      '1'           *IN(D)
     C                   MOVE      '1'           *IN36
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE EFFECTIVE DATE
     C*
     C                   Z-ADD     DT(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         DT(X)
     C     SRERR         IFEQ      '1'
     C     X             ADD       19            C                 2 0
     C                   MOVE      '1'           *IN(C)
     C                   MOVE      '1'           *IN37
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END
     C*
     C*   STATUS MUST BE "A" ACTIVE OR "I" INACTIVE
     C*
     C     ST(X)         IFNE      'I'
     C     ST(X)         ANDNE     'A'
     C     X             ADD       19            E                 2 0
     C                   MOVE      '1'           *IN(E)
     C                   MOVE      '1'           *IN38
     C                   MOVE      '1'           *IN39
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
     C     IIKEY         CHAIN     PII01                              60
     C*
     C                   MOVE      UPCO#         IICO#
     C                   MOVE      SCPOL#        IIPOL#
     C                   MOVE      SCCC          IICC
     C                   MOVE      SCLOC#        IILOC#
     C                   MOVE      SCPRM#        IIPRM#
     C                   MOVE      XXCLAS        IICLAS
     C                   MOVE      IT(X)         IIITM#
     C                   MOVEL     DD(X)         IIDS35
     C                   MOVEL     ID(X)         IIID#
     C                   Z-ADD     VL(X)         IIVAL
     C                   MOVE      ST(X)         IISTAT
     C                   MOVE      A1(X)         IIAI01
     C                   MOVE      A2(X)         IIAI02
     C                   MOVE      A3(X)         IIAI03
     C                   MOVE      A4(X)         IIAI04
     C                   MOVE      A5(X)         IIAI05
     C     ST(X)         IFEQ      'I'
     C                   Z-ADD     DT(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IIIADT
     C                   ELSE
     C                   Z-ADD     DT(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IIEFDT
     C                   END
     C                   MOVE      Q1USER        IIUSER
     C                   MOVE      Q1PGM         IIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IIDT
     C                   Z-ADD     DSTIME        IITIME
     C*
     C                   UPDATE    PII01
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
     OPII01     E            PII01E
**
ROLL UP
ROLL DOWN
**  TABLE UA
-- ADD --
- UPDATE -
