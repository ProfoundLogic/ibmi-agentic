     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H**
     H/TITLE E0505R - USER PARAMETER SEARCH
     H**
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PROVIDES SEARCH CABABILITIES FOR THE       **
     H**     USER PARAMETER MASTER FILE                              **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     E0005R                                                  **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: Lori J. Quinn                                 **
     H**   CREATED:    01/21/91     PROJECT: EXP MOD                 **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** MODIFICATIONS:                                              **
     H**                                                             **
     H** 08/12/98  C:90  PROJ: WC853                                 **
     H**           MODIFIED TO VERSION 3 STANDARDS                   **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**      KC      Command 03 - EOJ                               **
     H**      KL      Command 12 - Prior Screen (Screen 2)           **
     H**  SCR01:                                                     **
     H**      20      End of File reached on roll (ECR0600)          **
     H**              ECR0600 = End of file reached on roll          **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    E0505F:  Screen Display                                  **
     H**    EMUPL (PUP01): CO/USER                                   **
     H*****************************************************************
     H*
     F*
     F* Display Screen File
     FE0505F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F*
     F* User Parameter Master File
     FEMUPL     IF   E           K DISK

     D*
     D* Roll Indicators For Screen
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D*
     D* Cursor Position Field
     D FL              S              1    DIM(14)
     D*
     D* User Parameter Field
     D*
     D* Company Number Field
     D*
     D* User Name Field
     D*
     D* Staff Code Field
     D*
     D* Key To File - for Roll Formward/Backward
     D KEY             S             10    DIM(2)
     D*
     D* Data Structure for System Date & Time
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* Data Structure for the Comapny Array
     D*
     D                 DS
     D  CO                     1     42  0
     D                                     DIM(14)
     D  SCCO01                 1      3  0
     D  SCCO02                 4      6  0
     D  SCCO03                 7      9  0
     D  SCCO04                10     12  0
     D  SCCO05                13     15  0
     D  SCCO06                16     18  0
     D  SCCO07                19     21  0
     D  SCCO08                22     24  0
     D  SCCO09                25     27  0
     D  SCCO10                28     30  0
     D  SCCO11                31     33  0
     D  SCCO12                34     36  0
     D  SCCO13                37     39  0
     D  SCCO14                40     42  0
     D*
     D* Data Structure for the Name Array
     D*
     D                 DS
     D  NM                     1    560
     D                                     DIM(14)
     D  SCNM01                 1     40
     D  SCNM02                41     80
     D  SCNM03                81    120
     D  SCNM04               121    160
     D  SCNM05               161    200
     D  SCNM06               201    240
     D  SCNM07               241    280
     D  SCNM08               281    320
     D  SCNM09               321    360
     D  SCNM10               361    400
     D  SCNM11               401    440
     D  SCNM12               441    480
     D  SCNM13               481    520
     D  SCNM14               521    560
     D*
     D* Data Structure for User Array
     D*
     D                 DS
     D  UR                     1    140
     D                                     DIM(14)
     D  SCUR01                 1     10
     D  SCUR02                11     20
     D  SCUR03                21     30
     D  SCUR04                31     40
     D  SCUR05                41     50
     D  SCUR06                51     60
     D  SCUR07                61     70
     D  SCUR08                71     80
     D  SCUR09                81     90
     D  SCUR10                91    100
     D  SCUR11               101    110
     D  SCUR12               111    120
     D  SCUR13               121    130
     D  SCUR14               131    140
     D*
     D* Data Structure for Staff Array
     D*
     D                 DS
     D  SF                     1     42
     D                                     DIM(14)
     D  SCSF01                 1      3
     D  SCSF02                 4      6
     D  SCSF03                 7      9
     D  SCSF04                10     12
     D  SCSF05                13     15
     D  SCSF06                16     18
     D  SCSF07                19     21
     D  SCSF08                22     24
     D  SCSF09                25     27
     D  SCSF10                28     30
     D  SCSF11                31     33
     D  SCSF12                34     36
     D  SCSF13                37     39
     D  SCSF14                40     42
     D*
     D* External Program Data Structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Key List For the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    XXUSER           10
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQKEY            10
     C                   PARM                    QQRTCD            1
     C*
     C* Initialize and Set-up
     C*
     C                   TIME                    DSDTTM
     C******************   MOVE Q1USER    SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C* Load the Record Format Information for the First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C* Load the Initial Screen from Selected Position
     C*
     C                   MOVEL     QQKEY         SCUSR
     C                   GOTO      REPOS
     C*
     C* Display all Screen Formats
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
     C* Process all Command Keys
     C*
     C     *IN11         IFEQ      '1'
     C*
     C* COMMAND 03 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *BLANK        QQKEY
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C* COMMAND 12 = CANCEL
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *BLANK        QQKEY
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
     C                   MOVEL     KEY(2)        XXUSER
     C     UPKEY         CHAIN     EMUPL                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   MOVEL     KEY(1)        XXUSER
     C     UPKEY         CHAIN     EMUPL                              60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCUSR         IFNE      *BLANK
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEY
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVEL     SCUSR         XXUSER
     C                   MOVE      *BLANK        SCUSR
     C     UPKEY         SETLL     EMUPL
     C                   READ      EMUPL                                  60
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
     C                   MOVEL     UR(I1)        QQKEY
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANK        QQKEY
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
     C                   MOVE      *BLANKS       CO
     C                   MOVE      *BLANKS       NM
     C                   MOVE      *BLANKS       SF
     C                   MOVE      *BLANKS       UR
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   Z-ADD     UPCO#         CO(I1)
     C                   MOVEL     UPUSNM        NM(I1)
     C                   MOVEL     UPSTAF        SF(I1)
     C                   MOVEL     UPUSR         UR(I1)
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
     C                   READ      EMUPL                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVEL     UR(1)         KEY(1)
     C                   MOVEL     UR(14)        KEY(2)
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
     C                   READP     EMUPL                                  60
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   MOVEL     UR(1)         KEY(1)
     C                   MOVEL     UR(14)        KEY(2)
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
     C                   MOVEL     *BLANK        XXUSER
     C     UPKEY         SETLL     EMUPL
     C                   READ      EMUPL                                  60
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
     C                   READ      EMUPL                                  60
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
