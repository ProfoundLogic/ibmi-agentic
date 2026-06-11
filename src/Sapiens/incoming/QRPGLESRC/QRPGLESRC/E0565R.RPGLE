     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H**
     H/TITLE E0565R - WEIGHTING VALUE SEARCH
     H**
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PROVIDES SEARCH CABABILITIES FOR THE       **
     H**     WEIGHTING VALUE FILE                                    **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     E0065R                                                  **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: T ANZALONE                                    **
     H**   CREATED:    02/03/92     PROJECT: MD054                   **
     H**   REASON:     TO SEPARATE WEIGHTING AND BALLAST VALUES      **
     H**               INTO 2 SEPARATE FILES                         **
     H**   CLIENT:     #0025                                         **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** MODIFICATIONS:                                              **
     H**                                                             **
     H**  08/12/98  C:90  PROJECT: WC853                             **
     H**            MODIFIED TO VERSION 3 STANDARDS                  **
     H**                                                             **
     H**  11/07/94  C:30  PROJECT:  WC356                            **
     H**            Add State Code.                                  **
     H**                                                             **
     H**     08-02-94 C:28 PROJ#: WC356                              **
     H**               CHANGED THE BALOSS AND WELOSS FIELDS SIZE     **
     H**               FROM 8/0 TO 11/0                              **
     H**                                                             **
     H**     07-16-92  LJQ PROJ#: WC153                              **
     H**               ADDED EFFECTIVE DATE TO FILE.  USE EFFECTIVE  **
     H**               DATE OF MOD TO DETERMINE WHICH VALUE TO USE.  **
     H**                                                             **
     H**               MOVED FROM CLIENT #25 TO BASE EXP MOD         **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**      KC      Command 03 - EXIT                              **
     H**      KL      Command 12 - CANCEL                            **
     H**  SCR01:                                                     **
     H**      20      End of File reached on roll (ECR0600)          **
     H**              ECR0600 = End of file reached on roll          **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    E0530F:  Screen Display                                  **
     H**    SMCOL (PCO01): CO                                        **
     H**    EMUPL (PUP01): USER                                      **
     H**    EMWEL3(PWE01): CO/STATE/EFF DT/LOSS                      **
     H*****************************************************************
     H*
     F*
     F* Display Screen File
     FE0565F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameter Master File
     FEMUPL     IF   E           K DISK
     F*
     F* WEIGHT   Master File
     FEMWEL3    IF   E           K DISK

     D*
     D* State Code Field
     D*
     D* Cursor Position Field
     D FL              S              1    DIM(14)
     D*
     D* Expected Losses Field
     D*
     D* Effective Date Field
     D*
     D* Weighting Value Field
     D*
     D*
     D* Roll Indicators For Screen
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D*
     D* Key Value for State
     D KEYB            S              2    DIM(2)
     D*
     D* Key Value for Losses
     D KEYL            S             10  2 DIM(2)
     D* Key Value for EFDT
     D KEYD            S              8  0 DIM(2)
     D*
     D* Data Structure for System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* Data Structure for Screen Reference
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D* Data Structure for Payroll Class Code Array
     D                 DS
     D  ST                     1     28
     D                                     DIM(14)
     D  SCST01                 1      2
     D  SCST02                 3      4
     D  SCST03                 5      6
     D  SCST04                 7      8
     D  SCST05                 9     10
     D  SCST06                11     12
     D  SCST07                13     14
     D  SCST08                15     16
     D  SCST09                17     18
     D  SCST10                19     20
     D  SCST11                21     22
     D  SCST12                23     24
     D  SCST13                25     26
     D  SCST14                27     28
     D*
     D                 DS
     D  LS                     1    182  2
     D                                     DIM(14)
     D  SCLS01                 1     13  2
     D  SCLS02                14     26  2
     D  SCLS03                27     39  2
     D  SCLS04                40     52  2
     D  SCLS05                53     65  2
     D  SCLS06                66     78  2
     D  SCLS07                79     91  2
     D  SCLS08                92    104  2
     D  SCLS09               105    117  2
     D  SCLS10               118    130  2
     D  SCLS11               131    143  2
     D  SCLS12               144    156  2
     D  SCLS13               157    169  2
     D  SCLS14               170    182  2
     D*
     D* Data Structure for Ballast value (date) Array
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
     D* Data Structure for Weighting Value Array
     D                 DS
     D  WE                     1     42  2
     D                                     DIM(14)
     D  SCWE01                 1      3  2
     D  SCWE02                 4      6  2
     D  SCWE03                 7      9  2
     D  SCWE04                10     12  2
     D  SCWE05                13     15  2
     D  SCWE06                16     18  2
     D  SCWE07                19     21  2
     D  SCWE08                22     24  2
     D  SCWE09                25     27  2
     D  SCWE10                28     30  2
     D  SCWE11                31     33  2
     D  SCWE12                34     36  2
     D  SCWE13                37     39  2
     D  SCWE14                40     42  2
     D*
     D*
     D* external Program Data Structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C* Define Work Fields
     C*
     C     *LIKE         DEFINE    WELOSS        XXLOSS
     C     *LIKE         DEFINE    WEEFDT        XXEFDT
     C     *LIKE         DEFINE    WEST          XXST
     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C* Key List for the User PArameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* Key List for the Weighting & Ballast Value Master File
     C*
     C     WEKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
     C                   KFLD                    XXLOSS
     C*
     C     WEKEY1        KLIST
     C                   KFLD                    UPCO#
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQST              2
     C                   PARM                    QQLOSS           11 0
     C                   PARM                    QQEFDT            8 0
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
     C* Initialize and Set-Up
     C*
     C                   TIME                    DSDTTM
     C******************   MOVE Q1USER    SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      QQST          SCST
     C                   MOVE      QQLOSS        SCLOSS
     C                   MOVE      QQEFDT        SCEFDT
     C*
     C* Retrieve the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C* Retrieve the Company Record
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C* Load Screen Record Format for the First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C* Load the Screen from Selected Position
     C*
     C     QQEFDT        COMP      *ZERO                                  21
     C                   GOTO      REPOS
     C*
     C* Display All Screen Formats
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
     C* PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C* COMMAND 03 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   Z-ADD     *ZERO         QQLOSS
     C                   Z-ADD     0             QQEFDT
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C* COMMAND 12 = CANCEL
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   Z-ADD     *ZERO         QQLOSS
     C                   Z-ADD     0             QQEFDT
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C* ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR01T
     C*
     C* ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR01T
     C*
     C* PROCESS HELP REQUESTS
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
     C* Goto the Appropriate Screen
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C* Process Requested Screen
     C*
     C     SCR01T        TAG
     C*
     C* INVALID ROLL REQUEST
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
     C* ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVE      KEYB(2)       XXST
     C                   Z-ADD     KEYL(2)       XXLOSS
     C*
     C                   Z-ADD     KEYD(2)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C     WEKEY         CHAIN     PWE01                              60
     C     WEKEY1        READE     PWE01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      KEYB(1)       XXST
     C                   Z-ADD     KEYL(1)       XXLOSS
     C*
     C                   Z-ADD     KEYD(1)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C     WEKEY         CHAIN     PWE01                              60
     C                   READP     PWE01                                  60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCST          IFNE      *BLANK
     C     SCLOSS        ORNE      *BLANK
     C     SCEFDT        ORNE      0
     C     REPOS         TAG
     C                   MOVE      *BLANKS       KEYB
     C                   MOVE      *BLANK        KEYL
     C                   MOVE      *BLANK        XXUP
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   SETOFF                                       21
     C                   MOVE      *BLANK        XXLOSS
     C                   MOVE      *BLANK        XXEFDT
     C                   MOVE      SCLOSS        XXLOSS
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   MOVE      *BLANKS       SCLOSS
     C                   MOVE      *BLANK        SCEFDT
     C                   MOVE      SCST          XXST
     C                   MOVE      *BLANKS       SCST
     C     WEKEY         SETLL     PWE01
     C     WEKEY1        READE     PWE01                                  60
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
     C     LS(I1)        MULT      1             QQLOSS
     C                   Z-ADD     DT(I1)        QQEFDT
     C                   MOVE      ST(I1)        QQST
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   Z-ADD     *ZERO         QQLOSS
     C                   Z-ADD     0             QQEFDT
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
     C                   MOVE      *BLANKS       LS
     C                   MOVE      *BLANKS       WE
     C                   MOVE      *BLANKS       DT
     C                   MOVE      *BLANKS       ST
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   Z-ADD     WELOSS        LS(I1)
     C     WEWEIG        MULT      .01           WE(I1)
     C*
     C                   Z-ADD     WEEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C*
     C                   MOVE      WEST          ST(I1)
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
     C     WEKEY1        READE     PWE01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVE      ST(1)         KEYB(1)
     C                   MOVE      ST(14)        KEYB(2)
     C     LS(1)         MULT      1             KEYL(1)
     C     LS(14)        MULT      1             KEYL(2)
     C                   Z-ADD     DT(1)         KEYD(1)
     C                   Z-ADD     DT(14)        KEYD(2)
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
     C                   READP     PWE01                                  60
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   MOVE      ST(1)         KEYB(1)
     C                   MOVE      ST(14)        KEYB(2)
     C     LS(1)         MULT      1             KEYL(1)
     C     LS(14)        MULT      1             KEYL(2)
     C                   Z-ADD     DT(1)         KEYD(1)
     C                   Z-ADD     DT(14)        KEYD(2)
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
     C                   Z-ADD     *ZERO         XXLOSS
     C                   Z-ADD     *ZERO         XXEFDT
     C     WEKEY         SETLL     PWE01
     C     WEKEY1        READE     PWE01                                  60
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
     C     WEKEY1        READE     PWE01                                  60
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
