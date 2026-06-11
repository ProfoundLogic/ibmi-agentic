     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W2158R - BILLING AUXILIARY SEARCH
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/04/92  LPK  PROJECT WC155                                  *
     H*                DON'T DISPLAY INACTIVE B.S. CODES.             *
     H*                                                               *
     H*  02/16/95  C:24  PROJECT WC367                                *
     H*                  ADD FUND TO THE BILLING SCHEDULE AUXILIARY   *
     H*                  FILE KEY.                                    *
     H*****************************************************************
     FW2158F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWTABL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMBXL     IF   E           K DISK
     FWMFNL     IF   E           K DISK

     D FL              S              1    DIM(14)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEYC            S             10    DIM(2)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXPFDT                 1      8  0
     D  XXMMDD                 1      4  0
     D  XXYEAR                 5      8  0
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
     D  SD                     1    420
     D                                     DIM(14)
     D  SCSD01                 1     30
     D  SCSD02                31     60
     D  SCSD03                61     90
     D  SCSD04                91    120
     D  SCSD05               121    150
     D  SCSD06               151    180
     D  SCSD07               181    210
     D  SCSD08               211    240
     D  SCSD09               241    270
     D  SCSD10               271    300
     D  SCSD11               301    330
     D  SCSD12               331    360
     D  SCSD13               361    390
     D  SCSD14               391    420
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
     C*  TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY
     C*
     C*  BILLING SCHEDULE AUXILIARY FILE
     C*
     C     BXKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXTYPE
     C                   KFLD                    XXMMDD
     C                   KFLD                    XXKEY
     C*
     C     BXKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXTYPE
     C                   KFLD                    XXMMDD
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C     *LIKE         DEFINE    BXTYPE        XXTYPE
     C     *LIKE         DEFINE    BXBSCD        XXKEY
     C     *LIKE         DEFINE    BXBSCD        XXBSCD
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQPFDT            8
     C                   PARM                    QQBSCD            3
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
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE FUND MASTER RECORD
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVE      UPFND         SCFND
     C                   MOVE      FNNAME        SCNAME
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   MOVE      QQPFDT        XXPFDT
     C                   MOVE      QQTYPE        XXTYPE
     C     BXKEY2        SETGT     PBX01
     C                   READP     PBX01                                  60
     C     *IN60         IFEQ      '0'
     C     UPCO#         COMP      BXCO#                              6060
     C  N60              Z-ADD     BXEFDT        XXMMDD
     C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C     QQTYPE        IFNE      *BLANKS
     C     BXKEY2        CHAIN     PBX01                              60
     C                   Z-ADD     0             XXCNT             2 0
     C                   MOVE      *BLANKS       XXBSCD
     C     *IN60         DOWEQ     '0'
     C     XXCNT         ANDLT     2
     C                   MOVEL     BXBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              50
     C  N50BSSTAT        IFEQ      'A'
     C                   ADD       1             XXCNT
     C                   MOVEL     BXBSCD        XXBSCD
     C                   END
     C     BXKEY2        READE     PBX01                                  60
     C                   END
     C*
     C     XXCNT         IFEQ      2
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      SCR02T
     C                   ELSE
     C                   MOVE      '*'           QQRTCD
     C                   MOVEL     XXBSCD        QQBSCD
     C                   GOTO      TERM
     C                   END
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     XXUP          IFEQ      '1'
     C                   MOVE      *BLANKS       SCUP              9
     C                   ELSE
     C                   MOVEL     RR(1)         SCUP
     C                   END
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN            9
     C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *BLANKS       QQBSCD
     C                   MOVE      *BLANKS       QQTYPE
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *BLANKS       QQBSCD
     C                   MOVE      *BLANKS       QQTYPE
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C                   END
     C*
     C*   ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           ROLLT
     C*
     C*   ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           ROLLT
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
     C     Q2RECF        CABEQ     'SCR02'       ROLLT
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  ONLY ONE INSTALLMENT PLAN CAN BE SELECTED
     C*
     C                   Z-ADD     *ZEROS        XXSEL
     C     SCMNIN        IFNE      *BLANKS
     C                   ADD       1             XXSEL             3 0
     C                   MOVE      'M'           XXTYPE
     C                   END
     C     SCQRIN        IFNE      *BLANKS
     C                   ADD       1             XXSEL
     C                   MOVE      'Q'           XXTYPE
     C                   END
     C     SCANIN        IFNE      *BLANKS
     C                   ADD       1             XXSEL
     C                   MOVE      'A'           XXTYPE
     C                   END
     C     SCOTIN        IFNE      *BLANKS
     C                   ADD       1             XXSEL
     C                   MOVE      'O'           XXTYPE
     C                   END
     C     XXSEL         IFNE      1
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      QQPFDT        XXPFDT
     C     BXKEY2        SETGT     PBX01
     C                   READP     PBX01                                  60
     C     *IN60         IFEQ      '0'
     C     UPCO#         COMP      BXCO#                              6060
     C  N60              Z-ADD     BXEFDT        XXMMDD
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR02T        TAG
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C                   MOVEL     QQBSCD        SCCODE
     C     QQBSCD        COMP      *BLANKS                                21
     C                   GOTO      REPOS
     C*
     C*  INVALID ROLL REQUEST
     C*
     C     ROLLT         TAG
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
     C                   MOVEL     KEYC(2)       XXKEY
     C     BXKEY         CHAIN     PBX01                              60
     C     BXKEY2        READE     PBX01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   MOVEL     KEYC(1)       XXKEY
     C     BXKEY         CHAIN     PBX01                              60
     C                   READP     PBX01                                  60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCCODE        IFNE      *BLANKS
     C     REPOS         TAG
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   MOVE      '0'           *IN21
     C                   MOVE      *BLANK        KEYC
     C                   MOVE      *BLANK        XXUP
     C                   MOVEL     SCCODE        XXKEY
     C                   MOVE      *BLANKS       SCCODE
     C     BXKEY         SETLL     PBX01
     C     BXKEY2        READE     PBX01                                  60
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
     C                   MOVEL     BS(I1)        QQBSCD
     C                   MOVEL     XXTYPE        QQTYPE
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANKS       QQBSCD
     C                   MOVE      *BLANKS       QQTYPE
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       BS
     C                   MOVE      *BLANKS       SD
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVEL     *BLANKS       XXKEY
     C                   MOVEL     BXBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     BSKEY         BS(I1)
     C                   MOVEL     BSDESC        SD(I1)
     C                   ELSE
     C                   MOVE      *BLANKS       BS(I1)
     C                   MOVE      *BLANKS       SD(I1)
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
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C*  B.S. CODE MUST BE ACTIVE
     C*
     C                   MOVEL     BXBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              50
     C  N50BSSTAT        IFEQ      'A'
     C                   ADD       1             I1
     C                   EXSR      FILL
     C                   END
     C     BXKEY2        READE     PBX01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVEL     BS(1)         KEYC(1)
     C                   MOVEL     BS(14)        KEYC(2)
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
     C*  B.S. CODE MUST BE ACTIVE
     C*
     C                   MOVEL     BXBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              50
     C  N50BSSTAT        IFEQ      'A'
     C                   SUB       1             I1
     C                   EXSR      FILL
     C                   END
     C                   READP     PBX01                                  60
     C  N60BXCO#         COMP      UPCO#                              6060
     C  N60BXTYPE        COMP      XXTYPE                             6060
     C  N60BXEFDT        COMP      XXMMDD                             6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   MOVEL     BS(1)         KEYC(1)
     C                   MOVEL     BS(14)        KEYC(2)
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
     C                   MOVE      *BLANKS       XXKEY
     C     BXKEY         SETLL     PBX01
     C     BXKEY2        READE     PBX01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C*
     C*  B.S. CODE MUST BE ACTIVE
     C*
     C                   MOVEL     BXBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              50
     C  N50BSSTAT        IFEQ      'A'
     C                   ADD       1             I1
     C                   EXSR      FILL
     C                   END
     C     BXKEY2        READE     PBX01                                  60
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
