     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE F0530R - FEE SCHEDULE SERVICE CODE NUMERIC SEARCH
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*  07/16/90 KWK   - PER FL STATE RULE CHANGE:                   *
     H*                   ADDED EFFECTIVE DATE FIELD FOR SERVICE CODE *
     H*
     H*  03/12/92 LJD   - Programming for New FL Hospital Fee Schedule*
     H*                   1.  Chg to FMSVL1 logical keyfield          *
     H*                   2.  New "Common Service Code" made up of:   *
     H*                           Common Service Code (11A)
     H*                                                               *
     H* 09/16/92 KRR  REQ/PROJ: 10040/FS040                           *
     H*               1. ADDED SCR02 DETAIL INQUIRY                   *
     H*               2. ADDED F5(DET. INQUIRY) TO SCR01              *
     H*****************************************************************
     FF0530F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FFMSVL1    IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FSTABL     IF   E           K DISK

     D FL              S              1    DIM(14)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEYC            S             11    DIM(2)
     D KEY2            S              8  0 DIM(2)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  SV                     1    154
     D                                     DIM(14)
     D  SCSV01                 1     11
     D  SCSV02                12     22
     D  SCSV03                23     33
     D  SCSV04                34     44
     D  SCSV05                45     55
     D  SCSV06                56     66
     D  SCSV07                67     77
     D  SCSV08                78     88
     D  SCSV09                89     99
     D  SCSV10               100    110
     D  SCSV11               111    121
     D  SCSV12               122    132
     D  SCSV13               133    143
     D  SCSV14               144    154
     D                 DS
     D  ED                     1    112  0
     D                                     DIM(14)
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
     D                 DS
     D  DS                     1    560
     D                                     DIM(14)
     D  SCDS01                 1     40
     D  SCDS02                41     80
     D  SCDS03                81    120
     D  SCDS04               121    160
     D  SCDS05               161    200
     D  SCDS06               201    240
     D  SCDS07               241    280
     D  SCDS08               281    320
     D  SCDS09               321    360
     D  SCDS10               361    400
     D  SCDS11               401    440
     D  SCDS12               441    480
     D  SCDS13               481    520
     D  SCDS14               521    560
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
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR STATE FILE
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C*  SERVICE CODE FILE
     C     SVKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXCMSV
     C                   KFLD                    XXEFDT
     C*
     C     SVKEY1        KLIST
     C                   KFLD                    UPCO#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCMSV
     C                   PARM                    QQST
     C                   PARM                    QQEFDT            8 0          ADD 07/16/90
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
     C                   MOVEL     QQST          SCST
     C*
     C     *LIKE         DEFINE    SVCMSV        XXCMSV
     C     *LIKE         DEFINE    SVCMSV        QQCMSV
     C     *LIKE         DEFINE    STKEY         XXKEY
     C     *LIKE         DEFINE    SCST          QQST
     C     *LIKE         DEFINE    SVDT          XXEFDT
     C*
     C                   MOVEL     SCST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     STNAME        SCSTNM
     C                   END
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
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C                   MOVEL     QQCMSV        SCCMSV
     C                   MOVEL     QQEFDT        SCEFDT                         ADD 07/16/90
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
     C****************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*********************************
     C*  F3 = EXIT                    *
     C*********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *BLANK        QQCMSV
     C                   MOVE      *ZERO         QQEFDT                         ADD 07/16/90
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*********************************
     C*   F5 = DETAIL INQUIRY         *
     C*********************************
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
     C                   MOVEL     SV(I1)        XXCMSV
     C*
     C                   Z-ADD     ED(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C     SVKEY         CHAIN     PSV01                              60
     C*
     C                   Z-ADD     SVEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVEL     SVCMSV        SCCMSV
     C                   MOVEL     SVDESC        SCDESC
     C                   MOVEL     SVTYPE        SCTYPE
     C                   Z-ADD     SVALOW        SCALOW
     C                   Z-ADD     SVINPC        SCINPC
     C                   Z-ADD     SVOTPC        SCOTPC
     C                   MOVE      SVSTAT        SCSTAT
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*********************************
     C*  F12 = PREVIOUS               *
     C*********************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *BLANK        QQCMSV
     C                   MOVE      *ZERO         QQEFDT                         ADD 07/16/90
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      *BLANKS       SCCMSV
     C                   MOVE      *ZEROS        SCEFDT
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*********************************
     C*   ROLL UP REQUEST
     C*********************************
     C     *IN18         CABEQ     '1'           SCR01T
     C*********************************
     C*   ROLL DOWN REQUEST
     C*********************************
     C     *IN19         CABEQ     '1'           SCR01T
     C*********************************
     C*  *IN10 = HELP
     C*********************************
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
     C                   END                                                    END CMDKEYS
     C****************************************************************
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
     C                   MOVEL     KEYC(2)       XXCMSV
     C*
     C                   Z-ADD     KEY2(2)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C     SVKEY         CHAIN     PSV01                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   MOVEL     KEYC(1)       XXCMSV
     C*
     C                   Z-ADD     KEY2(1)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C     SVKEY         CHAIN     PSV01                              60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCCMSV        IFNE      *BLANKS
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEYC
     C                   MOVE      *ZEROS        KEY2                           ADD 07/16/90
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVEL     SCCMSV        XXCMSV
     C                   MOVE      *BLANK        SCCMSV
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   MOVE      *ZEROS        SCEFDT
     C     SVKEY         SETLL     PSV01
     C     SVKEY1        READE     PSV01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       7             I1                3 0
     C*
     C*  VALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     14
     C                   MOVE      '*'           QQRTCD
     C                   MOVEL     SV(I1)        QQCMSV
     C                   MOVE      ED(I1)        QQEFDT                         ADD 07/16/90
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANK        QQCMSV
     C                   MOVE      *ZEROS        QQEFDT                         ADD 07/16/90
     C                   GOTO      TERM
     C*
     C*   PROCESS REQUEST SCREEN TWO
     C*
     C     SCR02T        TAG
     C*
     C                   MOVE      *BLANKS       SCCMSV
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
     C                   MOVE      *BLANKS       SV
     C                   MOVE      *ZEROS        ED                             ADD 07/16/90
     C                   MOVE      *BLANKS       DS
     C                   MOVE      *BLANKS       ST
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVEL     SVCMSV        SV(I1)
     C*
     C                   Z-ADD     SVEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ED(I1)
     C*
     C                   MOVEL     SVDESC        DS(I1)
     C                   MOVEL     SVSTAT        ST(I1)
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
     C     SVKEY1        READE     PSV01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVEL     SV(1)         KEYC(1)
     C                   MOVEL     SV(14)        KEYC(2)
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
     C                   READP     PSV01                                  60
     C     SVCO#         IFNE      UPCO#
     C                   MOVE      '1'           *IN60
     C                   END
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   MOVEL     SV(1)         KEYC(1)
     C                   MOVEL     SV(14)        KEYC(2)
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
     C                   MOVE      *BLANKS       XXCMSV
     C     SVKEY         SETLL     PSV01
     C     SVKEY1        READE     PSV01                                  60
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
     C     SVKEY1        READE     PSV01                                  60
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
