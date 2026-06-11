     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr**** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H/TITLE F0651R - EOB WORK FILE SEARCH BY FUND YEAR/CASE
      *****************************************************************
      ** REVISIONS:                                                  **
      *
      *  06/09/20 C:212  Project : SY420                             **
      *                  Mask SSN                                    **
      *                                                              **
      ** 07/22/92 KRR    PROJ: 99AAC                                 **
      **                 ADDED SUB# TO ALL PROCESSING                **
      ** 02/04/93 C:20       PROJ:                                   **
      **                     1. ADDED CODE TO NOT INCLUDE ANY EOMB   **
      **                        IF EOFLG4 = 'Y'(PRE/RE-CERT)         **
      **                        THESE SHOULD NOT BE INCLUDED IN SRCH **
      **                        BECAUSE THEY ARE NOT AVAILABLE FOR   **
      **                        REPRINT OR DELETION                  **
      *****************************************************************
     FF0651F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FFAEOL4    IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMSML     IF   E           K DISK

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D FL              S              1    DIM(14)                              POSITIONAL FLAG
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEYY            S              3  0 DIM(2)                               KEY - FUND YEAR
     D KEYC            S              7  0 DIM(2)                               KEY - CASE
     D KEYF            S              9  0 DIM(2)                               KEY - EOMB #
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  YR                     1     42  0
     D                                     DIM(14)                              YEAR
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
     D                 DS
     D  CA                     1     98  0
     D                                     DIM(14)                              CASE
     D  SCCA01                 1      7  0
     D  SCCA02                 8     14  0
     D  SCCA03                15     21  0
     D  SCCA04                22     28  0
     D  SCCA05                29     35  0
     D  SCCA06                36     42  0
     D  SCCA07                43     49  0
     D  SCCA08                50     56  0
     D  SCCA09                57     63  0
     D  SCCA10                64     70  0
     D  SCCA11                71     77  0
     D  SCCA12                78     84  0
     D  SCCA13                85     91  0
     D  SCCA14                92     98  0
     D                 DS
     D  FS                     1    126  0
     D                                     DIM(14)                              EOMB#
     D  SCFS01                 1      9  0
     D  SCFS02                10     18  0
     D  SCFS03                19     27  0
     D  SCFS04                28     36  0
     D  SCFS05                37     45  0
     D  SCFS06                46     54  0
     D  SCFS07                55     63  0
     D  SCFS08                64     72  0
     D  SCFS09                73     81  0
     D  SCFS10                82     90  0
     D  SCFS11                91     99  0
     D  SCFS12               100    108  0
     D  SCFS13               109    117  0
     D  SCFS14               118    126  0
     D                 DS
     D  FE                     1    126  0
     D                                     DIM(14)                              PROVIDER ID
     D  SCFE01                 1      9  0
     D  SCFE02                10     18  0
     D  SCFE03                19     27  0
     D  SCFE04                28     36  0
     D  SCFE05                37     45  0
     D  SCFE06                46     54  0
     D  SCFE07                55     63  0
     D  SCFE08                64     72  0
     D  SCFE09                73     81  0
     D  SCFE10                82     90  0
     D  SCFE11                91     99  0
     D  SCFE12               100    108  0
     D  SCFE13               109    117  0
     D  SCFE14               118    126  0
     D                 DS
     D  SS                     1    126
     D                                     DIM(14)                              SOC SEC #
     D  SCSS01                 1      9
     D  SCSS02                10     18
     D  SCSS03                19     27
     D  SCSS04                28     36
     D  SCSS05                37     45
     D  SCSS06                46     54
     D  SCSS07                55     63
     D  SCSS08                64     72
     D  SCSS09                73     81
     D  SCSS10                82     90
     D  SCSS11                91     99
     D  SCSS12               100    108
     D  SCSS13               109    117
     D  SCSS14               118    126
     D                 DS
     D  AD                     1    112  0
     D                                     DIM(14)                              ACCIDENT DATE
     D  SCAD01               001    008  0
     D  SCAD02               009    016  0
     D  SCAD03               017    024  0
     D  SCAD04               025    032  0
     D  SCAD05               033    040  0
     D  SCAD06               041    048  0
     D  SCAD07               049    056  0
     D  SCAD08               057    064  0
     D  SCAD09               065    072  0
     D  SCAD10               073    080  0
     D  SCAD11               081    088  0
     D  SCAD12               089    096  0
     D  SCAD13               097    104  0
     D  SCAD14               105    112  0
     D                 DS
     D  NM                     1    280
     D                                     DIM(14)                              NAME
     D  SCNM01                 1     20
     D  SCNM02                21     40
     D  SCNM03                41     60
     D  SCNM04                61     80
     D  SCNM05                81    100
     D  SCNM06               101    120
     D  SCNM07               121    140
     D  SCNM08               141    160
     D  SCNM09               161    180
     D  SCNM10               181    200
     D  SCNM11               201    220
     D  SCNM12               221    240
     D  SCNM13               241    260
     D  SCNM14               261    280
     D                 DS
     D  ST                     1     14
     D                                     DIM(14)                              PRINT STATUS
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
     C*  KEY LIST #1 FOR THE EOB WORK FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C*  KEY LIST #2 FOR THE EOB WORK FILE
     C*
     C     EOKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C*
     C*   KEY LIST FOR THE SUBSCRIBER
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQEOB#
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
     C*** INITIALIZE WORK FIELDS ***
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    EOEOB#        QQEOB#
     C     *LIKE         DEFINE    CDSS#         XXSS#
     C     *LIKE         DEFINE    EOFND         XXFND
     C     *LIKE         DEFINE    EOFYR         XXFYR
     C     *LIKE         DEFINE    EOCASE        XXCASE
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      *ZEROS        SCFYR
     C                   MOVE      *ZEROS        SCCASE
     C                   MOVE      *ZEROS        XXFYR
     C                   MOVE      *ZEROS        XXCASE
     C                   MOVE      *ZEROS        XXEOB#
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
     C     SMKEY         CHAIN     PSM01                              60
     C                   MOVEL     SMSUB#        SCSUB#
     C                   MOVEL     SMNAME        SCSBNM
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     UPFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C     XXFYR         COMP      *ZERO                                  21
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
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   Z-ADD     *ZERO         QQEOB#
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   Z-ADD     *ZERO         QQEOB#
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
     C                   Z-ADD     KEYY(2)       XXFYR
     C                   Z-ADD     KEYC(2)       XXCASE
     C                   Z-ADD     KEYF(2)       XXEOB#
     C     EOKEY         CHAIN     PEO01                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYY(1)       XXFYR
     C                   Z-ADD     KEYC(1)       XXCASE
     C                   Z-ADD     KEYF(1)       XXEOB#
     C     EOKEY         CHAIN     PEO01                              60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCFYR         IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEYY
     C                   MOVE      *BLANK        KEYC
     C                   MOVE      *BLANK        KEYF
     C                   MOVE      *BLANK        XXUP
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   SETOFF                                       21
     C                   Z-ADD     SCFYR         XXFYR
     C                   MOVE      *BLANKS       SCFYR
     C                   Z-ADD     SCCASE        XXCASE
     C                   MOVE      *BLANKS       SCCASE
     C                   Z-ADD     *ZERO         XXEOB#
     C     EOKEY         SETLL     PEO01
     C     EOKEY2        READE     PEO01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       7             I1                3 0
     C*
     C*  VALID CURSOR POSITION - RETURN WITH SELECTION
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     14
     C                   MOVE      '*'           QQRTCD
     C                   Z-ADD     FS(I1)        QQEOB#
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   Z-ADD     *ZERO         QQEOB#
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C********************************************************************
     C*  CLEAR SCREEN ARRAYS       ** SUBROUTINE **              C L R   *
     C********************************************************************
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       YR
     C                   MOVE      *BLANKS       CA
     C                   MOVE      *BLANKS       FS
     C                   MOVE      *BLANKS       FE
     C                   MOVE      *BLANKS       SS
     C                   MOVE      *BLANKS       AD
     C                   MOVE      *BLANKS       NM
     C                   MOVE      *BLANKS       ST
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C********************************************************************
     C*  FILL SCREEN ARRAYS      ** SUBROUTINE **                F I L L *
     C********************************************************************
     C     FILL          BEGSR
     C                   Z-ADD     EOFYR         YR(I1)
     C                   Z-ADD     EOCASE        CA(I1)
     C                   Z-ADD     EOEOB#        FS(I1)
     C                   Z-ADD     EOFEIN        FE(I1)
     C*
     C*  CHECK TO SEE IF PRINTED
     C*
     C     EOSTAT        IFEQ      'P'
     C                   MOVE      'Y'           ST(I1)
     C                   ELSE
     C                   MOVE      ' '           ST(I1)
     C                   END                                                    END IF
     C*
     C*  GET SS# AND ACCIDENT DATE
     C*
     C     CMKEY         CHAIN     PCM01                              70
     C     *IN70         IFEQ      '0'
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      CMSS#         SS(I1)
      /Free
       SS(I1) = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   MOVE      CMSS#         XXSS#
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         AD(I1)
     C*
     C                   END                                                    END IF
     C*
     C*  GET CLAIMANT NAME
     C*
     C     CDKEY         CHAIN     PCD01                              80
     C     *IN80         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVEL     '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        NM(I1)
     C                   END                                                    END IF
     C*
     C     ENDFIL        ENDSR
     C********************************************************************
     C*  LOAD FORWARD           ** SUBROUTINE **                 L O A D *
     C********************************************************************
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
     C     EOFLG4        IFEQ      'Y'
     C                   GOTO      NXTRCD
     C                   END
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     NXTRCD        TAG
     C     EOKEY2        READE     PEO01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     YR(1)         KEYY(1)
     C                   Z-ADD     YR(14)        KEYY(2)
     C                   Z-ADD     CA(1)         KEYC(1)
     C                   Z-ADD     CA(14)        KEYC(2)
     C                   Z-ADD     FS(1)         KEYF(1)
     C                   Z-ADD     FS(14)        KEYF(2)
     C                   ENDSR
     C********************************************************************
     C*  LOAD BACKWARD          ** SUBROUTINE **               L O A D P *
     C********************************************************************
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
     C     EOFLG4        IFEQ      'Y'
     C                   GOTO      NXTRC1
     C                   END
     C                   SUB       1             I1
     C                   EXSR      FILL
     C     NXTRC1        TAG
     C                   READP     PEO01                                  60
     C     UPCO#         IFNE      EOCO#
     C     UPSUB#        ORNE      EOSUB#
     C     UPFND         ORNE      EOFND
     C                   MOVE      '1'           *IN60
     C                   END
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     YR(1)         KEYY(1)
     C                   Z-ADD     YR(14)        KEYY(2)
     C                   Z-ADD     CA(1)         KEYC(1)
     C                   Z-ADD     CA(14)        KEYC(2)
     C                   Z-ADD     FS(1)         KEYF(1)
     C                   Z-ADD     FS(14)        KEYF(2)
     C                   ENDSR
     C********************************************************************
     C*  LOAD BEGINNING         ** SUBROUTINE **               L O A D B *
     C********************************************************************
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     *ZERO         XXFYR
     C                   Z-ADD     *ZERO         XXCASE
     C                   Z-ADD     *ZERO         XXEOB#
     C     EOKEY         SETLL     PEO01
     C     EOKEY2        READE     PEO01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C     EOFLG4        IFEQ      'Y'
     C                   GOTO      NXTRC2
     C                   END
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     NXTRC2        TAG
     C     EOKEY2        READE     PEO01                                  60
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
