     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H**
     H/TITLE E0555R - EMPLOYER EXP MODS
     H**
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PROVIDES SEARCH CAPABILITIES FOR           **
     H**     EXPERIENCE ALREADY CALCULATED W/EFFECTIVE DATE          **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     E7055R                                                  **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: LORI J. QUINN                                 **
     H**   CREATED:    08/08/91     PROJECT: EXP MOD                 **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** MODIFICATIONS:                                              **
     H* 07/22/10  C:246  SUPPORT:73562  REROFIT THE WC1502 GERT FTR  **
     H*                                                               *
     H* 09/17/06  C:03   PROJECT: WC1502
     H*                  ADD GERT FACTOR
     H**                                                             **
     H** 08/12/98  C:90  PROJ: WC853                                 **
     H**           MODIFIED TO VERSION 3 STANDARDS                   **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**      10      Help                                           **
     H**      18      Roll Up                                        **
     H**      19      Roll Down                                      **
     H**                                                             **
     H**      KC      Command 03 - EOJ                               **
     H**      KL      Command 12 - Prior Screen (Screen 2)           **
     H**  SCR01:                                                     **
     H**      20      End of File reached on roll (ECR0600)          **
     H**              ECR0600 = End of file reached on roll          **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    E0555F:  Screen Display                                  **
     H**    SMCOL (PCO01): CO                                        **
     H**    EMUPL (PUP01): USER                                      **
     H**    EMFNL (PUP01): CO/FUND                                   **
     H**    EMEML (PEM01): CO/FND/EMP#                               **
     H**    EXXTL1(PXX01): CO/FND/EMP#/EFF DT/REC TYPE (SEL "C" REC) **
     H*****************************************************************
     H*
     F*
     F* Display Screen File
     FE0555F    CF   E             WORKSTN
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
     F* Fund Master File
     FEMFNL     IF   E           K DISK
     F*
     F* Employer Master File
     FEMEML     IF   E           K DISK
     F*
     F* Employer Experience Mod File
     FEXXTL1    IF   E           K DISK

     D*
     D* Cursor Position Field
     D FL              S              1    DIM(14)
     D*
     D* Fund Number Field
     D*
     D* Employer Number Field
     D*
     D* Employer Name Field
     D*
     D* Effective Date Field
     D*
     D* Exp Modifier Field
     D*
     D* Roll Indicators For Screen
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D*
     D* Array for Notes
     D NT              S             70    DIM(2) CTDATA PERRCD(1)              note/xtovr3 flag
     D*
     D* Key Value for Fund
     D KEYF            S              3  0 DIM(2)
     D*
     D* Key Value for Employer Number
     D KEYE            S              9  0 DIM(2)
     D*
     D* Key Value for Effective Date
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
     D* Data Structure for Fund Number Array
     D                 DS
     D  FN                     1     42  0
     D                                     DIM(14)
     D  SCFN01                 1      3  0
     D  SCFN02                 4      6  0
     D  SCFN03                 7      9  0
     D  SCFN04                10     12  0
     D  SCFN05                13     15  0
     D  SCFN06                16     18  0
     D  SCFN07                19     21  0
     D  SCFN08                22     24  0
     D  SCFN09                25     27  0
     D  SCFN10                28     30  0
     D  SCFN11                31     33  0
     D  SCFN12                34     36  0
     D  SCFN13                37     39  0
     D  SCFN14                40     42  0
     D*
     D* Data Structure for Employer Number Array
     D                 DS
     D  EM                     1    126  0
     D                                     DIM(14)
     D  SCEM01                 1      9  0
     D  SCEM02                10     18  0
     D  SCEM03                19     27  0
     D  SCEM04                28     36  0
     D  SCEM05                37     45  0
     D  SCEM06                46     54  0
     D  SCEM07                55     63  0
     D  SCEM08                64     72  0
     D  SCEM09                73     81  0
     D  SCEM10                82     90  0
     D  SCEM11                91     99  0
     D  SCEM12               100    108  0
     D  SCEM13               109    117  0
     D  SCEM14               118    126  0
     D*
     D* Data Structure for Employer Name Array
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
     D* Data Structure for Effective Date
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
     D*
     D* Data Structure for Exp Mods
     D                 DS
     D  MD                     1     42  2
     D                                     DIM(14)
     D  SCMD01                 1      3  2
     D  SCMD02                 4      6  2
     D  SCMD03                 7      9  2
     D  SCMD04                10     12  2
     D  SCMD05                13     15  2
     D  SCMD06                16     18  2
     D  SCMD07                19     21  2
     D  SCMD08                22     24  2
     D  SCMD09                25     27  2
     D  SCMD10                28     30  2
     D  SCMD11                31     33  2
     D  SCMD12                34     36  2
     D  SCMD13                37     39  2
     D  SCMD14                40     42  2
     D*
     D* External Program Data Structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C* Define Work Fields
     C*
     C     *LIKE         DEFINE    XFUND         XXFND
     C     *LIKE         DEFINE    XEMP#         XXEMP#
     C     *LIKE         DEFINE    XEFDT         XXEFDT
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
     C* Key List for the FUND Master File
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C*
     C* Key List for the Employer Excess/Non-Excess File
     C*
     C     XTKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C     XTKY1         KLIST
     C                   KFLD                    UPCO#
     C*
     C     XTKY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXEFDT
     C*
     C* Key List for the Employer Master File
     C*
     C     EMKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQRTCD            1
     C                   PARM                    UPCO#             3 0
     C                   PARM                    QQFND             3 0
     C                   PARM                    QQEMP#            9 0
     C                   PARM                    QQEFDT            8 0          mmddyy
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
     C                   MOVE      QQFND         SCFND
     C                   MOVE      QQEMP#        SCEMP#
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
     C     QQFND         COMP      *ZERO                                  21
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
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C* PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C* COMMAND 03 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   Z-ADD     *ZERO         QQFND
     C                   Z-ADD     *ZERO         QQEMP#
     C                   Z-ADD     *ZERO         QQEFDT
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C* COMMAND 12 = CANCEL
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   Z-ADD     *ZERO         QQFND
     C                   Z-ADD     *ZERO         QQEMP#
     C                   Z-ADD     *ZERO         QQEFDT
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
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     0             SCFND
     C                   Z-ADD     0             SCEMP#
     C                   Z-ADD     0             SCEFDT
     C                   GOTO      STRT
     C                   END
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
     C                   Z-ADD     KEYF(2)       XXFND
     C                   Z-ADD     KEYE(2)       XXEMP#
     C*
     C                   Z-ADD     KEYD(2)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C     XTKY          CHAIN     PXX01                              60
     C     XTKY1         READE     PXX01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYF(1)       XXFND
     C                   Z-ADD     KEYE(1)       XXEMP#
     C*
     C                   Z-ADD     KEYD(1)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C     XTKY          CHAIN     PXX01                              60
     C                   READP     PXX01                                  60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCFND         IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEYF
     C                   MOVE      *BLANK        KEYE
     C                   MOVE      *BLANKS       KEYD
     C                   MOVE      *BLANK        XXUP
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   SETOFF                                       21
     C                   MOVE      *BLANK        XXFND
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCFND         XXFND
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   MOVE      *BLANKS       SCEMP#
     C                   MOVE      *BLANKS       SCFND
     C                   MOVE      *BLANKS       SCEFDT
     C     XTKY          SETLL     PXX01
     C     XTKY1         READE     PXX01                                  60
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
     C                   Z-ADD     FN(I1)        XXFND
     C                   Z-ADD     EM(I1)        XXEMP#
     C*
     C                   Z-ADD     DT(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   Z-ADD     DT(I1)        SCEFDT
     C                   Z-ADD     EM(I1)        SCEMP#
     C                   GOTO      SCR02T
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   GOTO      TERM
     C*
     C     SCR02T        TAG
     C*
     C* GET FUND NAME
     C*
     C     FNKEY         CHAIN     PFN01                              66
     C                   MOVE      *BLANKS       SCFNAM
     C                   MOVEL     FNNAME        SCFNAM
     C*
     C* GET REQUESTED RECORD
     C     XTKY2         CHAIN     PXX01                              41
     C*
     C*
     C*  Utilize XTOVER3 flag to determine table note
     C     *IN41         IFNE      '1'
     C     XTOVR3        IFEQ      '*'
     C                   Z-ADD     1.00          SCMOD
     C                   MOVE      *BLANKS       XNOTE            70
     C                   MOVEL     NT(1)         XNOTE
     C                   ELSE
     C     XTMODA        IFNE      0
     C     XTMODA        MULT(H)   1             SCMOD
     C                   MOVE      *BLANKS       XNOTE
     C                   MOVEL     NT(2)         XNOTE
     C                   ELSE
     C     XTMOD         MULT(H)   1             SCMOD
     C                   END
     C                   END                                                     endif *XTOVR3
     C                   END                                                     endif *IN41
     C*
     C*  move file fields to screen fields
     C     XREC          IFEQ      'C'
     C                   MOVE      '0'           *IN34
     C*
     C     1             SUB       XTWVWV        SCZWVW
     C                   Z-ADD     XTPAL         SCPAL
     C                   Z-ADD     XTBALL        SCBALL
     C                   Z-ADD     XTBALL        SCBALC
     C                   Z-ADD     XTEXLS        SCEXLS
     C                   Z-ADD     XTWVWV        SCWVWV
     C                   Z-ADD     XTAELW        SCAELW
     C                   Z-ADD     XTEXEW        SCEXEW
     C                   Z-ADD     XTNUM         SCNUM
     C                   Z-ADD     XTDENM        SCDENM
     C                   Z-ADD     XTM2YR        SCM2YR
     C                   Z-ADD     XTMALY        SCMALY
     C                   MOVEL(P)  EMSEMP        SCSEMP
     C*
     C*** 09/17/06  BGN ADD *******************************
     C     XTGMOD        MULT(H)   1             SCGMOD
     C*** 09/17/06  BGN ADD *******************************
     C                   MOVEL     *BLANKS       SCNOTE
     C                   MOVEL     *BLANKS       SCFNAM
     C                   MOVEL     *BLANKS       SCENAM
     C                   MOVEL     XNOTE         SCNOTE
     C                   MOVEL     FNNAME        SCFNAM
     C                   MOVEL     EMNAME        SCENAM
     C     XTMALR        MULT(H)   1             SCMALR
     C     XTM2RT        MULT(H)   1             SCM2RT
     C* Get Employer's Name
     C                   Z-ADD     XFUND         XXFND
     C                   Z-ADD     XEMP#         XXEMP#
     C     EMKY          CHAIN     PEM01                              65
     C     *IN65         IFEQ      '0'
     C                   MOVEL     EMNAME        SCENAM
     C                   MOVEL(P)  EMSEMP        SCSEMP
     C                   END
     C*
     C                   ELSE
     C*
     C*  clear screen fields when XREC is not = 'C', there is no hit
     C*
     C                   Z-ADD     0             SCZWVW
     C                   Z-ADD     0             SCPAL
     C                   Z-ADD     0             SCBALL
     C                   Z-ADD     0             SCBALC
     C                   Z-ADD     0             SCEXLS
     C                   Z-ADD     0             SCWVWV
     C                   Z-ADD     0             SCAELW
     C                   Z-ADD     0             SCEXEW
     C                   Z-ADD     0             SCNUM
     C                   Z-ADD     0             SCDENM
     C                   Z-ADD     0             SCM2YR
     C                   Z-ADD     0             SCMALY
     C                   Z-ADD     0             SCMALR
     C                   Z-ADD     0             SCM2RT
     C                   Z-ADD     0             SCMOD
     C*** 09/17/06  BGN ADD *******************************
     C                   Z-ADD     0             SCGMOD
     C*** 09/17/06  BGN ADD *******************************
     C                   MOVEL     *BLANKS       SCNOTE
     C                   MOVEL     *BLANKS       SCFNAM
     C                   MOVEL     *BLANKS       SCENAM
     C                   MOVEL     *BLANKS       SCSEMP
     C                   MOVE      '1'           *IN34
     C*
     C                   END                                                    endif XREC
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C********************************************************
     C*  Subroutines
     C********************************************************
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       FN
     C                   MOVE      *BLANKS       EM
     C                   MOVE      *BLANKS       DT
     C                   MOVE      *BLANKS       NM
     C                   MOVE      *BLANKS       MD
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   Z-ADD     XFUND         FN(I1)
     C                   Z-ADD     XEMP#         EM(I1)
     C*
     C                   Z-ADD     XEFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C*
     C*  Utilize XTOVER3 flag to determine table note
     C     XTOVR3        IFEQ      '*'
     C                   Z-ADD     1.00          MD(I1)
     C                   ELSE
     C     XTMODA        IFNE      0
     C     XTMODA        MULT(H)   1             MD(I1)
     C                   ELSE
     C     XTMOD         MULT(H)   1             MD(I1)
     C                   END
     C                   END                                                     endif *XTOVR3
     C* Get Employer's Name
     C                   Z-ADD     XFUND         XXFND
     C                   Z-ADD     XEMP#         XXEMP#
     C                   MOVE      *BLANKS       NM(I1)
     C     EMKY          CHAIN     PEM01                              65
     C     *IN65         IFEQ      '0'
     C                   MOVEL     EMNAME        NM(I1)
     C                   ELSE
     C                   MOVE      *BLANKS       NM(I1)
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
     C     XTKY1         READE     PXX01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     FN(1)         KEYF(1)
     C                   Z-ADD     FN(14)        KEYF(2)
     C                   Z-ADD     EM(1)         KEYE(1)
     C                   Z-ADD     EM(14)        KEYE(2)
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
     C                   READP     PXX01                                  60
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     FN(1)         KEYF(1)
     C                   Z-ADD     FN(14)        KEYF(2)
     C                   Z-ADD     EM(1)         KEYE(1)
     C                   Z-ADD     EM(14)        KEYE(2)
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
     C                   Z-ADD     *ZERO         XXEMP#
     C                   Z-ADD     *ZERO         XXFND
     C                   Z-ADD     *ZERO         XXEFDT
     C     XTKY          SETLL     PXX01
     C     XTKY1         READE     PXX01                                  60
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
     C     XTKY1         READE     PXX01                                  60
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
**  TABLE NT
THIS EMPLOYER DOES NOT MEET MINIMUM PREMIUM REQUIREMENTS.
THE SPECIAL RULE TO ADJUST EXPERIENCE MODIFIERS HAS BEEN INVOKED.
