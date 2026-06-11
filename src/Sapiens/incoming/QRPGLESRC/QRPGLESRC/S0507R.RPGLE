     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE S0507R - USER PARAMETER FILE BY STAFF CODE
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PROVIDES A SEARCH FOR THE SYSTEM           **
     H**     USER PARAMETER FILE  BY STAFF CODE                      **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: Lori J. Quinn                                 **
     H**   CREATED:    04/25/91     PROJECT:                         **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** MODIFICATIONS:                                              **
     H**                                                             **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**      KC      Command 03 - EXIT                              **
     H**      KL      Command 12 - CANCEL                            **
     H**    SCR01:                                                   **
     H**      20      SCR01  EOF on Roll                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    S0507F:  Screen Display                                  **
     H**    SMUPL (PUP01): USER                                      **
     H*****************************************************************
     F*
     F* Screen File
     FS0507F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F*
     F* User Parameter Master File
     FSMUPL1    IF   E           K DISK

     D*
     D* Define Tables and Arrays
     D FL              S              1    DIM(14)                              Selection Field
     D RR              S              9    DIM(2) CTDATA PERRCD(1)              Roll Indicators
     D KEY             S              3    DIM(2)                               Keys to User File
     D*
     D* System Date and Time Data Structure
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* Screen Reference Data Structure
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D* Title Array
     D                 DS
     D  TI                     1    210
     D                                     DIM(14)                              Title
     D  SCTI01                 1     15
     D  SCTI02                16     30
     D  SCTI03                31     45
     D  SCTI04                46     60
     D  SCTI05                61     75
     D  SCTI06                76     90
     D  SCTI07                91    105
     D  SCTI08               106    120
     D  SCTI09               121    135
     D  SCTI10               136    150
     D  SCTI11               151    165
     D  SCTI12               166    180
     D  SCTI13               181    195
     D  SCTI14               196    210
     D*
     D* User Profile Array
     D                 DS
     D  US                     1    140
     D                                     DIM(14)                              User Profile
     D  SCUS01                 1     10
     D  SCUS02                11     20
     D  SCUS03                21     30
     D  SCUS04                31     40
     D  SCUS05                41     50
     D  SCUS06                51     60
     D  SCUS07                61     70
     D  SCUS08                71     80
     D  SCUS09                81     90
     D  SCUS10                91    100
     D  SCUS11               101    110
     D  SCUS12               111    120
     D  SCUS13               121    130
     D  SCUS14               131    140
     D*
     D* Staff Code Array
     D                 DS
     D  ST                     1     42
     D                                     DIM(14)                              Staff Code
     D  SCST01                 1      3
     D  SCST02                 4      6
     D  SCST03                 7      9
     D  SCST04                10     12
     D  SCST05                13     15
     D  SCST06                16     18
     D  SCST07                19     21
     D  SCST08                22     24
     D  SCST09                25     27
     D  SCST10                28     30
     D  SCST11                31     33
     D  SCST12                34     36
     D  SCST13                37     39
     D  SCST14                40     42
     D*
     D* Externally Described Data Structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Key List for User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    XXSTAF            3
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQSTAF            3
     C                   PARM                    QQRTCD            1
     C*
     C* Initialize and Set-Up
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C* Load the Record Format Information for the First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C* Load the Initial Screen from Selected Position
     C*
     C                   MOVEL     QQSTAF        SCSTAF
     C     QQSTAF        COMP      *BLANK                                 21
     C                   GOTO      REPOS
     C*
     C* Starting Position
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
     C*
     C                   EXFMT     SCR01
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = Exit
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *BLANK        QQSTAF
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 12 = PRIOR SCREEN
     C*
     C     *INKB         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *BLANK        QQSTAF
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
     C                   MOVEL     KEY(2)        XXSTAF
     C     UPKEY         CHAIN     PUP01                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   MOVEL     KEY(1)        XXSTAF
     C     UPKEY         CHAIN     PUP01                              60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCSTAF        IFNE      *BLANK
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEY
     C                   MOVE      *BLANK        XXUP
     C  N21              MOVE      *BLANK        XXDOWN
     C   21              MOVE      '1'           XXDOWN
     C                   SETOFF                                       21
     C                   MOVEL     SCSTAF        XXSTAF
     C                   MOVE      *BLANK        SCSTAF
     C     UPKEY         SETLL     PUP01
     C                   READ      PUP01                                  60
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
     C                   MOVEL     ST(I1)        QQSTAF
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANK        QQSTAF
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
     C                   MOVE      *BLANKS       US
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       TI
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVEL     UPTITL        TI(I1)
     C                   MOVEL     UPSTAF        ST(I1)
     C                   MOVEL     UPUSR         US(I1)
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
     C                   READ      PUP01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVEL     ST(1)         KEY(1)
     C                   MOVEL     ST(14)        KEY(2)
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
     C                   READP     PUP01                                  60
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   MOVEL     ST(1)         KEY(1)
     C                   MOVEL     ST(14)        KEY(2)
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
     C                   MOVEL     *BLANK        XXSTAF
     C     UPKEY         SETLL     PUP01
     C                   READ      PUP01                                  60
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
     C                   READ      PUP01                                  60
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
