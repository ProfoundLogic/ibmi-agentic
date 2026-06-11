     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE W2159R - BILLING SCHEDULE AUXILIARY FILE SEARCH
     H****************************************************************
     H*   PROGRAM DESCRIPTION:                                       *
     H*     THIS PROGRAM SEARCHES THE BILLING SCHEDULE AUXILIARY     *
     H*     FILE FOR ALL BILLING CODES AND DISPLAYS THE CODE,        *
     H*     DESCRIPTION, EFFECTIVE DATE, TYPE AND STATUS.            *
     H*                                                              *
     H*   CALLED FROM:                                               *
     H*     W2157R                                                   *
     H*   CALLS TO:                                                  *
     H*     NONE                                                     *
     H*   PROGRAMMER: DAVID MONCRIEF                                 *
     H*   CREATED:    04/16/91     PROJECT: AHE                      *
     H*                                                              *
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                              *
     H****************************************************************
     FW2159F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMBXL1    IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMFNL     IF   E           K DISK

     D FL              S              1    DIM(14)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEY             S              3    DIM(2)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  BS                     1     42
     D                                     DIM(14)
     D  SCBS01                 1      3
     D  SCBS02                 4      6
     D  SCBS03                 7      9
     D  SCBS04                10     12
     D  SCBS05                13     15
     D  SCBS06                16     18
     D  SCBS07                19     21
     D  SCBS08                22     24
     D  SCBS09                25     27
     D  SCBS10                28     30
     D  SCBS11                31     33
     D  SCBS12                34     36
     D  SCBS13                37     39
     D  SCBS14                40     42
     D                 DS
     D  DS                     1    420
     D                                     DIM(14)
     D  SCDS01                 1     30
     D  SCDS02                31     60
     D  SCDS03                61     90
     D  SCDS04                91    120
     D  SCDS05               121    150
     D  SCDS06               151    180
     D  SCDS07               181    210
     D  SCDS08               211    240
     D  SCDS09               241    270
     D  SCDS10               271    300
     D  SCDS11               301    330
     D  SCDS12               331    360
     D  SCDS13               361    390
     D  SCDS14               391    420
     D                 DS
     D  EF                     1     56  0
     D                                     DIM(14)
     D  SCEF01                 1      4  0
     D  SCEF02                 5      8  0
     D  SCEF03                 9     12  0
     D  SCEF04                13     16  0
     D  SCEF05                17     20  0
     D  SCEF06                21     24  0
     D  SCEF07                25     28  0
     D  SCEF08                29     32  0
     D  SCEF09                33     36  0
     D  SCEF10                37     40  0
     D  SCEF11                41     44  0
     D  SCEF12                45     48  0
     D  SCEF13                49     52  0
     D  SCEF14                53     56  0
     D                 DS
     D  TY                     1     14
     D                                     DIM(14)
     D  SCTY01                 1      1
     D  SCTY02                 2      2
     D  SCTY03                 3      3
     D  SCTY04                 4      4
     D  SCTY05                 5      5
     D  SCTY06                 6      6
     D  SCTY07                 7      7
     D  SCTY08                 8      8
     D  SCTY09                 9      9
     D  SCTY10                10     10
     D  SCTY11                11     11
     D  SCTY12                12     12
     D  SCTY13                13     13
     D  SCTY14                14     14
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*****************************************************************
     C*  LIKE DEFINES
     C*****************************************************************
     C*
     C     *LIKE         DEFINE    BXBSCD        XXBSCD
     C     *LIKE         DEFINE    FNCO#         XXCO#
     C*
     C*****************************************************************
     C*  KEY LISTING
     C*****************************************************************
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  BILLING SCHEDULE FILE
     C*
     C     BXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXBSCD
     C*
     C*  BILLING SCHEDULE  - COMPANY ONLY
     C*
     C     BXKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C*
     C*  TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQKEY            10
     C                   PARM                    XXCO#
     C                   PARM                    SCFND
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
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*  GET THE FUND MASTER RECORD
     C*
     C                   MOVEL     *BLANKS       SCNAME
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVE      FNNAME        SCNAME
     C*
     C                   MOVEL     *BLANKS       SRDFLD
     C     COKY          CHAIN     PCO01                              60
     C  N60              MOVEL(P)  CONAME        SRDFLD
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
     C                   MOVEL     QQKEY         SCCODE
     C     QQKEY         COMP      *BLANKS                                21
     C                   GOTO      REPOS
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     XXUP          IFEQ      '1'
     C                   MOVE      *BLANKS       SCUP
     C                   ELSE
     C                   MOVEL     RR(1)         SCUP
     C                   END
     C*
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
     C                   END
     C*
     C                   EXFMT     SCR01
     C*
     C*  PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*  F3=EXIT
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  F12=CANCEL
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
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
     C     *IN10         IFEQ      *ON
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
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  INVALID ROLL REQUEST
     C*
     C   18XXUP          CABNE     *BLANK        STRT                 2020
     C*
     C   19XXDOWN        CABNE     *BLANK        STRT                 2020
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      *ON
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVEL     KEY(2)        XXBSCD
     C     BXKY          CHAIN     PBX01                              60
     C     BXKY1         READE     PBX01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      *ON
     C                   MOVE      *BLANK        XXUP
     C                   MOVEL     KEY(1)        XXBSCD
     C     BXKY          CHAIN     PBX01                              60
     C                   READP     PBX01                                  60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCCODE        IFNE      *BLANKS
     C*
     C     REPOS         TAG
     C*
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   MOVE      '0'           *IN21
     C                   MOVE      *BLANK        KEY
     C                   MOVE      *BLANK        XXUP
     C                   MOVEL     SCCODE        XXBSCD
     C                   MOVE      *BLANK        SCCODE
     C     BXKY          SETLL     PBX01
     C     BXKY1         READE     PBX01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C*
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
     C                   MOVEL     BS(I1)        QQKEY
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANKS       QQKEY
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       BS
     C                   MOVE      *BLANKS       DS
     C                   MOVE      *ZEROS        EF
     C                   MOVE      *BLANKS       TY
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVEL     BXBSCD        BS(I1)
     C                   MOVE      BXEFDT        EF(I1)
     C                   MOVEL     BXTYPE        TY(I1)
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     BS(I1)        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     BSDESC        DS(I1)
     C                   ELSE
     C                   MOVE      *BLANKS       DS(I1)
     C                   END
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
     C     BXKY1         READE     PBX01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVEL     BS(1)         KEY(1)
     C                   MOVEL     BS(14)        KEY(2)
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
     C                   READP     PBX01                                  60
     C  N60BXCO#         COMP      XXCO#                              6060
     C  N60BXFND         COMP      SCFND                              6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   MOVEL     BS(1)         KEY(1)
     C                   MOVEL     BS(14)        KEY(2)
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
     C                   MOVE      *BLANKS       XXBSCD
     C     BXKY          SETLL     PBX01
     C     BXKY1         READE     PBX01                                  60
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
     C     BXKY1         READE     PBX01                                  60
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
