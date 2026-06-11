     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE F0527R - REASONABLE & CUSTOMARY FILE SEARCH PGM
     H*****************************************************************
     H*  REVISIONS:
     H*  02/03/92  LJD    REQ/PROJ:  9/W0019 FOR 24
     H*                   1.  Added new screen field SCZIPA for
     H*                       Geographic Zip Area
     H*                   2.  Chgd key to FMRCP file to include ZIPA field
     H*
     H* 09/17/92 KRR  REQ/PROJ: 10039/FS039                           *
     H*               1. ADDED SCR02 DETAIL INQUIRY                   *
     H*               2. ADDED F5(DET. INQUIRY) TO SCR01              *
     H*****************************************************************
     FF0527F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FFMRCL     IF   E           K DISK
     FSMUPL     IF   E           K DISK

     D FL              S              1    DIM(14)                              Positional Flag
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEY0            S              3    DIM(2)                               Geo Zip Area Key
     D KEY1            S              5    DIM(2)                               Proc Code Key
     D KEY2            S              8  0 DIM(2)                               Effect Dt Key
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  GZ                     1     42
     D                                     DIM(14)                              Geo Zip Area
     D  SCGZ01                 1      3
     D  SCGZ02                 4      6
     D  SCGZ03                 7      9
     D  SCGZ04                10     12
     D  SCGZ05                13     15
     D  SCGZ06                16     18
     D  SCGZ07                19     21
     D  SCGZ08                22     24
     D  SCGZ09                25     27
     D  SCGZ10                28     30
     D  SCGZ11                31     33
     D  SCGZ12                34     36
     D  SCGZ13                37     39
     D  SCGZ14                40     42
     D                 DS
     D  RC                     1     70
     D                                     DIM(14)                              R&C Proc Code
     D  SCRC01                 1      5
     D  SCRC02                 6     10
     D  SCRC03                11     15
     D  SCRC04                16     20
     D  SCRC05                21     25
     D  SCRC06                26     30
     D  SCRC07                31     35
     D  SCRC08                36     40
     D  SCRC09                41     45
     D  SCRC10                46     50
     D  SCRC11                51     55
     D  SCRC12                56     60
     D  SCRC13                61     65
     D  SCRC14                66     70
     D                 DS
     D  NM                     1    700
     D                                     DIM(14)                              Proc Code Desc
     D  SCNM01                 1     50
     D  SCNM02                51    100
     D  SCNM03               101    150
     D  SCNM04               151    200
     D  SCNM05               201    250
     D  SCNM06               251    300
     D  SCNM07               301    350
     D  SCNM08               351    400
     D  SCNM09               401    450
     D  SCNM10               451    500
     D  SCNM11               501    550
     D  SCNM12               551    600
     D  SCNM13               601    650
     D  SCNM14               651    700
     D                 DS
     D  ST                     1     14
     D                                     DIM(14)                              Status
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
     D  ED                     1    112  0
     D                                     DIM(14)                              Effective Date
     D  SCED01               001    008  0
     D  SCED02               009    016  0
     D  SCED03               017    024  0
     D  SCED04               025    032  0
     D  SCED05               033    040  0
     D  SCED06               041    048  0
     D  SCED07               049    056  0
     D  SCED08               057    064  0
     D  SCED09               065    072  0
     D  SCED10               073    080  0
     D  SCED11               081    088  0
     D  SCED12               089    096  0
     D  SCED13               097    104  0
     D  SCED14               105    112  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    RCZIPA        XXKEY0                         ADD 02/03/92
     C     *LIKE         DEFINE    RCCPT4        XXKEY1
     C     *LIKE         DEFINE    RCEFDT        XXKEY2
     C*
     C*  CPT4 CODE TABLE FILE
     C*
     C     CPKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY0                         ADD 02/03/92
     C                   KFLD                    XXKEY1
     C                   KFLD                    XXKEY2
     C*
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
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQKEY0            3            ADD 02/03/92
     C                   PARM                    QQKEY1            5
     C                   PARM                    QQKEY2            8 0
     C                   PARM                    QQRTCD            1
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
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED
     C*
     C                   MOVEL     QQKEY0        SCZIPA                         ADD 02/03/92
     C                   MOVEL     QQKEY1        SCCPT4
     C                   MOVEL     QQKEY2        SCEFDT
     C                   GOTO      REPOS
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
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
     C                   EXFMT     SCR01
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *BLANK        QQKEY0                         ADD 02/03/92
     C                   MOVE      *BLANK        QQKEY1
     C                   MOVE      *ZERO         QQKEY2
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   F5 = DETAIL INQUIRY
     C*
     C     *INKE         IFEQ      '1'
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       7             I1                3 0
     C*
     C*  VALID CURSOR POSITION - GOTO SCR02
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     14
     C                   MOVEL     GZ(I1)        XXKEY0
     C                   MOVEL     RC(I1)        XXKEY1
     C*
     C                   Z-ADD     ED(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXKEY2
     C*
     C     CPKY1         CHAIN     PRC01                              60
     C                   MOVEL     RCZIPA        SCZIPA
     C                   MOVEL     RCCPT4        SCCPT4
     C                   MOVE      RCSTAT        SCSTAT
     C                   MOVE      RCDSC1        SCDSC1
     C                   MOVE      RCDSC2        SCDSC2
     C                   MOVE      RCMRA         SCMRA
     C                   MOVE      RCFOLU        SCFOLU
     C                   MOVE      RCANAM        SCANAM
     C                   MOVE      RCPROF        SCPROF
     C                   MOVE      RCRPT         SCRPT
     C                   MOVE      RCSURG        SCSURG
     C                   MOVE      RCSUPP        SCSUPP
     C                   MOVE      RCFSR         SCFSR
     C*
     C                   Z-ADD     RCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*   COMMAND 12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *BLANK        QQKEY0                         ADD 02/03/92
     C                   MOVE      *BLANK        QQKEY1
     C                   MOVE      *ZERO         QQKEY2
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      *BLANKS       SCZIPA
     C                   MOVE      *BLANKS       SCCPT4
     C                   MOVE      *ZEROS        SCEFDT
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*   ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR01T
     C*
     C*   ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR01T
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
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  INVALID ROLL REQUEST
     C*
     C   18XXUP          IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C   19XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVEL     KEY0(2)       XXKEY0                         ADD 02/03/92
     C                   MOVEL     KEY1(2)       XXKEY1
     C*
     C                   Z-ADD     KEY2(2)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXKEY2
     C*
     C     CPKY1         CHAIN     PRC01                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   MOVEL     KEY0(1)       XXKEY0                         ADD 02/03/92
     C                   MOVEL     KEY1(1)       XXKEY1
     C*
     C                   Z-ADD     KEY2(1)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXKEY2
     C*
     C     CPKY1         CHAIN     PRC01                              60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCZIPA        IFNE      *BLANK
     C     REPOS         TAG
     C                   MOVE      *BLANKS       KEY0                           ADD 02/03/92
     C                   MOVE      *BLANK        KEY1
     C                   MOVE      *ZEROS        KEY2
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVEL     SCZIPA        XXKEY0                         ADD 02/03/92
     C                   MOVE      *BLANK        SCZIPA                         ADD 02/03/92
     C                   MOVEL     SCCPT4        XXKEY1
     C                   MOVE      *BLANK        SCCPT4
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXKEY2
     C*
     C                   MOVE      *ZEROS        SCEFDT
     C     CPKY1         SETLL     PRC01
     C     UPCO#         READE     PRC01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       7             I1                3 0
     C*
     C*  VALID CURSOR POSITION - RETURN WITH SELECTION
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     14
     C                   MOVE      '*'           QQRTCD
     C                   MOVEL     GZ(I1)        QQKEY0                         ADD 02/03/92
     C                   MOVEL     RC(I1)        QQKEY1
     C                   MOVE      ED(I1)        QQKEY2
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANK        QQKEY0                         ADD 02/03/92
     C                   MOVE      *BLANK        QQKEY1
     C                   MOVE      *ZEROS        QQKEY2
     C                   GOTO      TERM
     C*
     C*   PROCESS REQUEST SCREEN TWO
     C*
     C     SCR02T        TAG
     C*
     C                   MOVE      *BLANKS       SCZIPA
     C                   MOVE      *BLANKS       SCCPT4
     C                   MOVE      *ZEROS        SCEFDT
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       GZ                             ADD 02/03/92
     C                   MOVE      *BLANKS       RC
     C                   MOVE      *BLANKS       NM
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *ZEROS        ED
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVEL     RCZIPA        GZ(I1)                         ADD 02/03/92
     C                   MOVEL     RCCPT4        RC(I1)
     C                   MOVE      RCSTAT        ST(I1)
     C                   MOVEL     RCDSC1        NM(I1)
     C*
     C                   Z-ADD     RCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ED(I1)
     C*
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
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     UPCO#         READE     PRC01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVEL     GZ(1)         KEY0(1)                        ADD 02/03/92
     C                   MOVEL     GZ(14)        KEY0(2)                        ADD 02/03/92
     C                   MOVEL     RC(1)         KEY1(1)
     C                   MOVEL     RC(14)        KEY1(2)
     C                   MOVE      ED(1)         KEY2(1)
     C                   MOVE      ED(14)        KEY2(2)
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
     C                   READP     PRC01                                  60
     C  N60RCCO#         COMP      UPCO#                              6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   MOVEL     GZ(1)         KEY0(1)                        ADD 02/03/92
     C                   MOVEL     GZ(14)        KEY0(2)                        ADD 02/03/92
     C                   MOVEL     RC(1)         KEY1(1)
     C                   MOVEL     RC(14)        KEY1(2)
     C                   MOVE      ED(1)         KEY2(1)
     C                   MOVE      ED(14)        KEY2(2)
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
     C                   MOVE      *BLANK        XXKEY0                         ADD 02/03/92
     C                   MOVE      *BLANK        XXKEY1
     C     CPKY1         SETLL     PRC01
     C     UPCO#         READE     PRC01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     UPCO#         READE     PRC01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
**
ROLL UP
ROLL DOWN
