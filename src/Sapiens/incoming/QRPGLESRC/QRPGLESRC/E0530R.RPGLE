     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H**
     H/TITLE E0530R - WEIGHTING & BALLAST VALUE SEARCH
     H**
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PROVIDES SEARCH CABABILITIES FOR THE       **
     H**     WEIGHTING & BALLAST VALUE FILE                          **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     E0030R                                                  **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: Lori J. Quinn                                 **
     H**   CREATED:    01/30/91     PROJECT: EXP MOD                 **
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
     H**    EMWBL (PWB01): CO/LOSS                                   **
     H*****************************************************************
     H*
     F*
     F* Display Screen File
     FE0530F    CF   E             WORKSTN
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
     F* Employer Master File
     FEMWBL     IF   E           K DISK

     D*
     D* Cursor Position Field
     D FL              S              1    DIM(14)
     D*
     D* Expected Losses Field
     D*
     D* Weighting Value Field
     D*
     D* Ballast Value Field
     D*
     D* Roll Indicators For Screen
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D*
     D* Key Value for Losses
     D KEYL            S              9  2 DIM(2)
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
     D* Data Structure for Expected Losses Array
     D                 DS
     D  LS                     1    126  2
     D                                     DIM(14)
     D  SCLS01                 1      9  2
     D  SCLS02                10     18  2
     D  SCLS03                19     27  2
     D  SCLS04                28     36  2
     D  SCLS05                37     45  2
     D  SCLS06                46     54  2
     D  SCLS07                55     63  2
     D  SCLS08                64     72  2
     D  SCLS09                73     81  2
     D  SCLS10                82     90  2
     D  SCLS11                91     99  2
     D  SCLS12               100    108  2
     D  SCLS13               109    117  2
     D  SCLS14               118    126  2
     D*
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
     D* Data Structure for Ballast Value Array
     D                 DS
     D  BA                     1     98  2
     D                                     DIM(14)
     D  SCBA01                 1      7  2
     D  SCBA02                 8     14  2
     D  SCBA03                15     21  2
     D  SCBA04                22     28  2
     D  SCBA05                29     35  2
     D  SCBA06                36     42  2
     D  SCBA07                43     49  2
     D  SCBA08                50     56  2
     D  SCBA09                57     63  2
     D  SCBA10                64     70  2
     D  SCBA11                71     77  2
     D  SCBA12                78     84  2
     D  SCBA13                85     91  2
     D  SCBA14                92     98  2
     D*
     D* external Program Data Structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C* Define Work Fields
     C*
     C     *LIKE         DEFINE    WBLOSS        XXLOSS
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
     C     WBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXLOSS
     C*
     C     WBKEY1        KLIST
     C                   KFLD                    UPCO#
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQLOSS            7 0
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
     C                   MOVE      QQLOSS        SCLOSS
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
     C     QQLOSS        COMP      *ZERO                                  21
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
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C* COMMAND 12 = CANCEL
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   Z-ADD     *ZERO         QQLOSS
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
     C                   Z-ADD     KEYL(2)       XXLOSS
     C     WBKEY         CHAIN     PWB01                              60
     C     WBKEY1        READE     PWB01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYL(1)       XXLOSS
     C     WBKEY         CHAIN     PWB01                              60
     C                   READP     PWB01                                  60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCLOSS        IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEYL
     C                   MOVE      *BLANK        XXUP
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   SETOFF                                       21
     C                   MOVE      *BLANK        XXLOSS
     C                   MOVE      SCLOSS        XXLOSS
     C                   MOVE      *BLANKS       SCLOSS
     C     WBKEY         SETLL     PWB01
     C     WBKEY1        READE     PWB01                                  60
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
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   Z-ADD     *ZERO         QQLOSS
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
     C                   MOVE      *BLANKS       BA
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   Z-ADD     WBLOSS        LS(I1)
     C     WBWEIG        MULT      .01           WE(I1)
     C                   Z-ADD     WBBALL        BA(I1)
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
     C     WBKEY1        READE     PWB01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C     LS(1)         MULT      1             KEYL(1)
     C     LS(14)        MULT      1             KEYL(2)
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
     C                   READP     PWB01                                  60
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C     LS(1)         MULT      1             KEYL(1)
     C     LS(14)        MULT      1             KEYL(2)
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
     C     WBKEY         SETLL     PWB01
     C     WBKEY1        READE     PWB01                                  60
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
     C     WBKEY1        READE     PWB01                                  60
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
