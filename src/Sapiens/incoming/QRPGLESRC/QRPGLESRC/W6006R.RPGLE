     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W6006R - PROCESS CHECKS MAINTENANCE: CHECK SCHEDULE SEARCH
     H*****************************************************************
     H* REVISIONS:                                                    *
     H* 02/22/93 C:11  PROJECT WC201                                  *
     H*                ADD FUND TO REPOSITION KEY.                    *
     H*                                                               *
     H* 09/08/94 C:15  PROJECT: WC361                                 *
     H*                ADD A PARM TO DIFFERENTIATE BETWEEN NORMAL AND *
     H*                GROUP CHECKS.  THIS PARM WILL CONTROL WHICH    *
     H*                FILE IS USED.                                  *
     H*****************************************************************
     FW6006F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWACPL     IF   E           K DISK    USROPN
     FWAC3L     IF   E           K DISK    USROPN
     F                                     RENAME(PCP01:PCP02)

     D FL              S              1    DIM(14)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEY             S              8  0 DIM(2)
     D KEYF            S              3  0 DIM(2)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  FD                     1     42  0
     D                                     DIM(14)
     D  SCFD01                 1      3  0
     D  SCFD02                 4      6  0
     D  SCFD03                 7      9  0
     D  SCFD04                10     12  0
     D  SCFD05                13     15  0
     D  SCFD06                16     18  0
     D  SCFD07                19     21  0
     D  SCFD08                22     24  0
     D  SCFD09                25     27  0
     D  SCFD10                28     30  0
     D  SCFD11                31     33  0
     D  SCFD12                34     36  0
     D  SCFD13                37     39  0
     D  SCFD14                40     42  0
     D                 DS
     D  PD                     1    112  0
     D                                     DIM(14)
     D  SCPD01               001    008  0
     D  SCPD02               009    016  0
     D  SCPD03               017    024  0
     D  SCPD04               025    032  0
     D  SCPD05               033    040  0
     D  SCPD06               041    048  0
     D  SCPD07               049    056  0
     D  SCPD08               057    064  0
     D  SCPD09               065    072  0
     D  SCPD10               073    080  0
     D  SCPD11               081    088  0
     D  SCPD12               089    096  0
     D  SCPD13               097    104  0
     D  SCPD14               105    112  0
     D                 DS
     D  S0                     1     14
     D                                     DIM(14)
     D  SCS001                 1      1
     D  SCS002                 2      2
     D  SCS003                 3      3
     D  SCS004                 4      4
     D  SCS005                 5      5
     D  SCS006                 6      6
     D  SCS007                 7      7
     D  SCS008                 8      8
     D  SCS009                 9      9
     D  SCS010                10     10
     D  SCS011                11     11
     D  SCS012                12     12
     D  SCS013                13     13
     D  SCS014                14     14
     D                 DS
     D  S1                     1     14
     D                                     DIM(14)
     D  SCS101                 1      1
     D  SCS102                 2      2
     D  SCS103                 3      3
     D  SCS104                 4      4
     D  SCS105                 5      5
     D  SCS106                 6      6
     D  SCS107                 7      7
     D  SCS108                 8      8
     D  SCS109                 9      9
     D  SCS110                10     10
     D  SCS111                11     11
     D  SCS112                12     12
     D  SCS113                13     13
     D  SCS114                14     14
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
     C*  PROCESS CHECKS FILE
     C*
     C     CPKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY
     C                   KFLD                    XXFND             3 0
     C*
     C*  TABLE FILE - COMPANY ONLY
     C*
     C     CPKEY2        KLIST
     C                   KFLD                    UPCO#
     C*
     C     *LIKE         DEFINE    CPDT          XXKEY
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQKEY             8 0
     C                   PARM                    QQFND             3 0
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQCKTP            1
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
     C*  OPEN THE CORRECT CHECK PROCESS FILE.
     C*    - QQCKTP = ' ',  OPEN WACPL  (NORMAL CHECKS)
     C*    - QQCKTP = 'G',  OPEN WAC3L  (GROUP CHECKS)
     C*
     C     QQCKTP        IFEQ      ' '
     C                   OPEN      WACPL
     C                   MOVE      '0'           *IN35
     C                   ELSE
     C                   OPEN      WAC3L
     C                   MOVE      '1'           *IN35
     C                   END
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
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C                   Z-ADD     QQKEY         SCPRDT
     C                   Z-ADD     QQFND         SCFND
     C*
     C     QQKEY         COMP      *ZERO                                  21
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
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
     C                   END
     C                   EXFMT     SCR01
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
     C                   MOVE      *ZERO         QQKEY
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *ZERO         QQKEY
     C                   MOVE      '1'           *INLR
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
     C*
     C                   Z-ADD     KEY(2)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXKEY
     C*
     C                   Z-ADD     KEYF(2)       XXFND
     C*
     C  N35CPKEY         CHAIN     PCP01                              60
     C  N35CPKEY2        READE     PCP01                                  60
     C   35CPKEY         CHAIN     PCP02                              60
     C   35CPKEY2        READE     PCP02                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C*
     C                   Z-ADD     KEY(1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXKEY
     C*
     C                   Z-ADD     KEYF(1)       XXFND
     C*
     C  N35CPKEY         CHAIN     PCP01                              60
     C  N35              READP     PCP01                                  60
     C   35CPKEY         CHAIN     PCP02                              60
     C   35              READP     PCP02                                  60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCPRDT        IFNE      *ZERO
     C     SCFND         ORNE      *ZERO
     C*
     C     REPOS         TAG
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   MOVE      '0'           *IN21
     C                   MOVE      *ZERO         KEY
     C                   MOVE      *BLANK        XXUP
     C*
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXKEY
     C*
     C                   MOVE      *ZERO         SCPRDT
     C                   Z-ADD     SCFND         XXFND
     C                   Z-ADD     *ZERO         SCFND
     C  N35CPKEY         SETLL     PCP01
     C  N35CPKEY2        READE     PCP01                                  60
     C   35CPKEY         SETLL     PCP02
     C   35CPKEY2        READE     PCP02                                  60
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
     C                   MOVE      FD(I1)        QQFND
     C                   MOVE      PD(I1)        QQKEY
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *ZERO         QQKEY
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
     C                   MOVE      *ZERO         FD
     C                   MOVE      *ZERO         PD
     C                   MOVE      *BLANKS       S0
     C                   MOVE      *BLANKS       S1
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVE      CPFND         FD(I1)
     C*
     C                   Z-ADD     CPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PD(I1)
     C*
     C                   MOVEL     CPSTA0        S0(I1)
     C                   MOVEL     CPSTA1        S1(I1)
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
     C  N35CPKEY2        READE     PCP01                                  60
     C   35CPKEY2        READE     PCP02                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVEL     PD(1)         KEY(1)
     C                   MOVEL     PD(14)        KEY(2)
     C                   MOVE      FD(1)         KEYF(1)
     C                   MOVE      FD(14)        KEYF(2)
     C*
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
     C  N35              READP     PCP01                                  60
     C   35              READP     PCP02                                  60
     C  N60CPCO#         COMP      UPCO#                              6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   MOVE      PD(1)         KEY(1)
     C                   MOVE      PD(14)        KEY(2)
     C                   MOVE      FD(1)         KEYF(1)
     C                   MOVE      FD(14)        KEYF(2)
     C*
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
     C                   MOVE      *ZERO         XXKEY
     C                   Z-ADD     *ZERO         XXFND
     C*
     C  N35CPKEY         SETLL     PCP01
     C  N35CPKEY2        READE     PCP01                                  60
     C   35CPKEY         SETLL     PCP02
     C   35CPKEY2        READE     PCP02                                  60
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
     C  N35CPKEY2        READE     PCP01                                  60
     C   35CPKEY2        READE     PCP02                                  60
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
