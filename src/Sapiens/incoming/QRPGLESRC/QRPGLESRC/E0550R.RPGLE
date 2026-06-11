     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H**
     H/TITLE E0550R - EMPLOYER EXCESS/NON-EXCESS FILE SEARCH
     H**
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PROVIDES SEARCH CABABILITIES FOR THE       **
     H**     EXCESS/NON-EXCESS FILE(FUND, EMP.NO., FUND YEAR ORDER)  **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     E0050R                                                  **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: LORI J. QUINN                                 **
     H**   CREATED:    03/11/91     PROJECT: EXP MOD                 **
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
     H**    E0550F:  Screen Display                                  **
     H**    SMCOL (PCO01): CO                                        **
     H**    EMUPL (PUP01): USER                                      **
     H**    EMEEL (PEB01): CO/FND/EMP#/YEAR/ACDT/CASE                **
     H*****************************************************************
     H*
     F*
     F* Display Screen File
     FE0550F    CF   E             WORKSTN
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
     F* Employer Excess/Non-Excess Master File
     FEMEEL     IF   E           K DISK

     D*
     D* Cursor Position Field
     D FL              S              1    DIM(14)
     D*
     D* Fund Number Field
     D*
     D* Employer Numer Field
     D*
     D* Batch Year Field
     D*
     D* Accident Date Field
     D*
     D* Case Number Field
     D*
     D* Case Status Field
     D*
     D* Multiple Claim Field
     D*
     D* Amount of Claim Field
     D*
     D* Number of Claims Field
     D*
     D* Roll Indicators For Screen
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D*
     D* Key Value for Fund
     D KEYF            S              3  0 DIM(2)
     D*
     D* Key Value for Employer Number
     D KEYE            S              9  0 DIM(2)
     D*
     D* Key Value for Batch Year
     D KEYY            S              3  0 DIM(2)
     D*
     D* Key Value for Accident Date
     D KEYD            S              8  0 DIM(2)
     D*
     D* Key Value for Case Number
     D KEYC            S             14    DIM(2)
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
     D* Data Structure for BATCH YEAR Array
     D                 DS
     D  YR                     1     42  0
     D                                     DIM(14)
     D  SCYR01                 1      3  0
     D  SCYR02                 4      6  0
     D  SCYR03                 7      9  0
     D  SCYR04                10     12  0
     D  SCYR05                13     15  0
     D  SCYR06                16     18  0
     D  SCYR07                19     21  0
     D  SCYR08                22     24  0
     D  SCYR09                25     27  0
     D  SCYR10                28     30  0
     D  SCYR11                31     33  0
     D  SCYR12                34     36  0
     D  SCYR13                37     39  0
     D  SCYR14                40     42  0
     D*
     D* Data Structure for Accident Date
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
     D* Data Structure for Case
     D                 DS
     D  CA                     1    196
     D                                     DIM(14)
     D  SCCA01                 1     14
     D  SCCA02                15     28
     D  SCCA03                29     42
     D  SCCA04                43     56
     D  SCCA05                57     70
     D  SCCA06                71     84
     D  SCCA07                85     98
     D  SCCA08                99    112
     D  SCCA09               113    126
     D  SCCA10               127    140
     D  SCCA11               141    154
     D  SCCA12               155    168
     D  SCCA13               169    182
     D  SCCA14               183    196
     D*
     D* Data Structure for Status
     D                 DS
     D  ST                     1     70
     D                                     DIM(14)
     D  SCST01                 1      5
     D  SCST02                 6     10
     D  SCST03                11     15
     D  SCST04                16     20
     D  SCST05                21     25
     D  SCST06                26     30
     D  SCST07                31     35
     D  SCST08                36     40
     D  SCST09                41     45
     D  SCST10                46     50
     D  SCST11                51     55
     D  SCST12                56     60
     D  SCST13                61     65
     D  SCST14                66     70
     D*
     D* Data Structure for Multiple Claim
     D                 DS
     D  MC                     1     42
     D                                     DIM(14)
     D  SCMC01                 1      3
     D  SCMC02                 4      6
     D  SCMC03                 7      9
     D  SCMC04                10     12
     D  SCMC05                13     15
     D  SCMC06                16     18
     D  SCMC07                19     21
     D  SCMC08                22     24
     D  SCMC09                25     27
     D  SCMC10                28     30
     D  SCMC11                31     33
     D  SCMC12                34     36
     D  SCMC13                37     39
     D  SCMC14                40     42
     D*
     D* Data Structure for Amount
     D                 DS
     D  AM                     1    154  2
     D                                     DIM(14)
     D  SCAM01                 1     11  2
     D  SCAM02                12     22  2
     D  SCAM03                23     33  2
     D  SCAM04                34     44  2
     D  SCAM05                45     55  2
     D  SCAM06                56     66  2
     D  SCAM07                67     77  2
     D  SCAM08                78     88  2
     D  SCAM09                89     99  2
     D  SCAM10               100    110  2
     D  SCAM11               111    121  2
     D  SCAM12               122    132  2
     D  SCAM13               133    143  2
     D  SCAM14               144    154  2
     D*
     D* Data Structure for Number of Claims
     D                 DS
     D  CL                     1     70  0
     D                                     DIM(14)
     D  SCCL01                 1      5  0
     D  SCCL02                 6     10  0
     D  SCCL03                11     15  0
     D  SCCL04                16     20  0
     D  SCCL05                21     25  0
     D  SCCL06                26     30  0
     D  SCCL07                31     35  0
     D  SCCL08                36     40  0
     D  SCCL09                41     45  0
     D  SCCL10                46     50  0
     D  SCCL11                51     55  0
     D  SCCL12                56     60  0
     D  SCCL13                61     65  0
     D  SCCL14                66     70  0
     D*
     D* External Program Data Structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C* Define Work Fields
     C*
     C     *LIKE         DEFINE    EEFND         XXFND
     C     *LIKE         DEFINE    EEEMP#        XXEMP#
     C     *LIKE         DEFINE    EEYEAR        XXYEAR
     C     *LIKE         DEFINE    EEACDT        XXACDT
     C     *LIKE         DEFINE    EECASE        XXCASE
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
     C* Key List for the Employer Excess/Non-Excess File
     C*
     C     EEKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXYEAR
     C                   KFLD                    XXACDT
     C                   KFLD                    XXCASE
     C*
     C     EEKEY1        KLIST
     C                   KFLD                    UPCO#
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFND             3 0
     C                   PARM                    QQEMP#            9 0
     C                   PARM                    QQYEAR            3 0
     C                   PARM                    QQACDT            8 0          mmddyy
     C                   PARM                    QQCASE           14
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
     C                   MOVE      QQFND         SCFND
     C                   MOVE      QQEMP#        SCEMP#
     C                   MOVE      QQYEAR        SCYEAR
     C                   MOVE      QQACDT        SCACDT
     C                   MOVE      QQCASE        SCCASE
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
     C                   Z-ADD     *ZERO         QQFND
     C                   Z-ADD     *ZERO         QQEMP#
     C                   Z-ADD     *ZERO         QQYEAR
     C                   Z-ADD     *ZERO         QQACDT
     C                   MOVE      *BLANKS       QQCASE
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
     C                   Z-ADD     *ZERO         QQYEAR
     C                   Z-ADD     *ZERO         QQACDT
     C                   MOVE      *BLANKS       QQCASE
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
     C                   Z-ADD     KEYF(2)       XXFND
     C                   Z-ADD     KEYE(2)       XXEMP#
     C                   Z-ADD     KEYY(2)       XXYEAR
     C*
     C                   Z-ADD     KEYD(2)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C*
     C                   MOVE      *BLANKS       XXCASE
     C                   MOVEL     KEYC(2)       XXCASE
     C     EEKEY         CHAIN     PEE01                              60
     C     EEKEY1        READE     PEE01                                  60
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
     C                   Z-ADD     KEYY(1)       XXYEAR
     C*
     C                   Z-ADD     KEYD(1)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C*
     C                   MOVE      *BLANKS       XXCASE
     C                   MOVEL     KEYC(1)       XXCASE
     C     EEKEY         CHAIN     PEE01                              60
     C                   READP     PEE01                                  60
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
     C                   MOVE      *BLANK        KEYY
     C                   MOVE      *BLANKS       KEYD
     C                   MOVE      *BLANKS       KEYC
     C                   MOVE      *BLANK        XXUP
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   SETOFF                                       21
     C                   MOVE      *BLANK        XXFND
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCYEAR        XXYEAR
     C                   MOVE      SCFND         XXFND
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C*
     C                   MOVEL     SCCASE        XXCASE
     C                   MOVE      *BLANKS       SCEMP#
     C                   MOVE      *BLANKS       SCFND
     C                   MOVE      *BLANKS       SCYEAR
     C                   MOVE      *BLANKS       SCACDT
     C                   MOVE      *BLANKS       SCCASE
     C     EEKEY         SETLL     PEE01
     C     EEKEY1        READE     PEE01                                  60
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
     C                   Z-ADD     FN(I1)        QQFND
     C                   Z-ADD     EM(I1)        QQEMP#
     C                   Z-ADD     YR(I1)        QQYEAR
     C                   Z-ADD     DT(I1)        QQACDT
     C                   MOVE      *BLANKS       QQCASE
     C                   MOVEL     CA(I1)        QQCASE
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   Z-ADD     *ZERO         QQFND
     C                   Z-ADD     *ZERO         QQEMP#
     C                   Z-ADD     *ZERO         QQYEAR
     C                   Z-ADD     *ZERO         QQACDT
     C                   MOVE      *BLANKS       QQCASE
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
     C                   MOVE      *BLANKS       FN
     C                   MOVE      *BLANKS       EM
     C                   MOVE      *BLANKS       YR
     C                   MOVE      *BLANKS       DT
     C                   MOVE      *BLANKS       CA
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       MC
     C                   MOVE      *BLANKS       AM
     C                   MOVE      *BLANKS       CL
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   Z-ADD     EEFND         FN(I1)
     C                   Z-ADD     EEEMP#        EM(I1)
     C                   MOVEL     EEYEAR        YR(I1)
     C*
     C                   Z-ADD     EEACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C* Excess Claim
     C     EEACDT        IFNE      0
     C     EESTAT        IFEQ      'F'
     C                   MOVEL     'FINAL'       ST(I1)
     C                   ELSE
     C                   MOVEL     'OPEN '       ST(I1)
     C                   END
     C     EEMULT        IFEQ      'Y'
     C                   MOVEL     'YES'         MC(I1)
     C                   ELSE
     C                   MOVEL     'NO '         MC(I1)
     C                   END
     C                   Z-ADD     0             CL(I1)
     C                   MOVE      *BLANKS       CA(I1)
     C                   MOVEL     EECASE        CA(I1)
     C                   ELSE
     C* Non-Excess Claim
     C                   MOVE      *BLANKS       ST(I1)
     C                   MOVE      *BLANKS       MC(I1)
     C                   Z-ADD     EETLCA        CL(I1)
     C                   MOVE      *BLANKS       CA(I1)
     C                   END
     C                   Z-ADD     EEAMT         AM(I1)
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
     C     EEKEY1        READE     PEE01                                  60
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
     C                   Z-ADD     YR(1)         KEYY(1)
     C                   Z-ADD     YR(14)        KEYY(2)
     C                   Z-ADD     DT(1)         KEYD(1)
     C                   Z-ADD     DT(14)        KEYD(2)
     C                   MOVE      *BLANKS       KEYC(1)
     C                   MOVE      *BLANKS       KEYC(2)
     C                   MOVEL     CA(1)         KEYC(1)
     C                   MOVEL     CA(14)        KEYC(2)
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
     C                   READP     PEE01                                  60
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
     C                   Z-ADD     YR(1)         KEYY(1)
     C                   Z-ADD     YR(14)        KEYY(2)
     C                   Z-ADD     DT(1)         KEYD(1)
     C                   Z-ADD     DT(14)        KEYD(2)
     C                   MOVE      *BLANKS       KEYC(1)
     C                   MOVE      *BLANKS       KEYC(2)
     C                   MOVEL     CA(1)         KEYC(1)
     C                   MOVEL     CA(14)        KEYC(2)
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
     C                   Z-ADD     *ZERO         XXYEAR
     C                   Z-ADD     *ZERO         XXACDT
     C                   MOVE      *BLANKS       XXCASE
     C     EEKEY         SETLL     PEE01
     C     EEKEY1        READE     PEE01                                  60
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
     C     EEKEY1        READE     PEE01                                  60
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
