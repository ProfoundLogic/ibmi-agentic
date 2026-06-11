     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*
     H* API - FA001R - MULTI CASES/SAME SOCIAL SECURITY NUMBER & ACCIDENT
     H*                DATE SEARCH - SCREEN PROGRAM
     H*                FEE SCHEDULE VERSION
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA001R                                            *
     H*    CREATED: 11/13/96  C:45                                    *
     H*    DESCRIPTION: MULTI CASES/SAME SS# & ACCIDENT DATE SEARCH   *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/09/20  C:212 Project : SY420                              *
     H*                  Mask SSN                                     *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       18      ROLL UP                                         *
     H*       19      ROLL DOWN                                       *
     H*       60      GENERAL USAGE (CHAIN)                           *
     H*       62      GENERAL USAGE (CHAIN)                           *
     H*       61      CONDITIONING INDICATOR                          *
     H*                                                               *
     H*****************************************************************
     F*
     FFA001F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FFMCMLA    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D*
     D FL              S              1    DIM(14)                                POSITIONAL FLAG
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEYF            S              3  0 DIM(2)                                 FUND
     D KEYY            S              3  0 DIM(2)                                 FUND YEAR
     D KEYC            S              7  0 DIM(2)                                 CASE
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  FN                     1     42  0
     D                                     DIM(14)                                FUND
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
     D                 DS
     D  YR                     1     42  0
     D                                     DIM(14)                                FUND YEAR
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
     D                                     DIM(14)                                CASE
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
     D  DS                     1    798
     D                                     DIM(14)                                ACC DESCRIPTION
     D  SCDS01                 1     57
     D  SCDS02                58    114
     D  SCDS03               115    171
     D  SCDS04               172    228
     D  SCDS05               229    285
     D  SCDS06               286    342
     D  SCDS07               343    399
     D  SCDS08               400    456
     D  SCDS09               457    513
     D  SCDS10               514    570
     D  SCDS11               571    627
     D  SCDS12               628    684
     D  SCDS13               685    741
     D  SCDS14               742    798
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   COMPANY FILE KEY LIST
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE KEY LIST
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  CLAIM MASTER FILE KEY LIST
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXSS#
     C                   KFLD                    XXACDT
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     CMKY2         KLIST
     C                   KFLD                    XXSS#
     C                   KFLD                    XXACDT
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     CMKEY1        KLIST
     C                   KFLD                    XXSS#
     C                   KFLD                    XXACDT
     C                   KFLD                    UPCO#
     C*
     C     CMKEY2        KLIST
     C                   KFLD                    XXSS#
     C                   KFLD                    XXACDT
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE KEY LIST
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQIN03            1
     C                   PARM                    QQIN12            1
     C                   PARM                    QQSS#             9
     C                   PARM                    QQACDT            8            (MMDDYY ORDER)
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
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
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMACDT        XXACDT
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C*
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      QQSS#         SCSS#
      /Free
       scSS# = MaskData('SSN':'QQSS#':QQSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   MOVE      QQACDT        SCACDT
     C                   MOVE      QQSS#         XXSS#
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
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
     C                   MOVE      *ZERO         UPSUB#                         ADD 02/25/93
     C                   ENDIF
     C*
     C*  GET COMPANY NAME
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SCCONM
     C                   ELSE
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   ENDIF
     C*
     C*** CHECK FOR MULTIPLE CASES UNDER THE SAME SS#/ACDT
     C*
     C                   Z-ADD     *ZEROS        CNTCAS            3 0
     C     CMKEY2        CHAIN     PCM01                              62         SS#/ACDT KEY
     C*
     C     *IN62         IFEQ      '1'
     C                   MOVE      'X'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C     *IN62         DOWEQ     '0'
     C                   ADD       1             CNTCAS
     C     CMKEY2        READE     PCM01                                  62
     C                   ENDDO
     C*
     C*** IF THERE CNTCAS IS NOT GREATER THAN 1 MOVE 'X' TO RETURN CODE
     C*** AND END CALL
     C*
     C     CNTCAS        IFLE      1
     C                   MOVE      'X'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*  GET THE CLAIMANT NAME
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCCLNM
     C                   ELSE
     C                   MOVEL     *BLANKS       SCCLNM
     C                   ENDIF
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
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
     C                   ENDIF
     C*
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
     C                   ENDIF
     C*
     C                   EXFMT     SCR01
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C****************************
     C*   F3 = EXIT              *
     C****************************
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *BLANK        QQFYR
     C                   MOVE      *BLANK        QQCASE
     C                   MOVE      *BLANK        QQSS#
     C                   MOVE      '1'           *INLR
     C                   MOVE      '1'           QQIN03
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C****************************
     C*  F12 = PREVIOUS          *
     C****************************
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *BLANK        QQFYR
     C                   MOVE      *BLANK        QQCASE
     C                   MOVE      *BLANK        QQSS#
     C                   MOVE      '1'           *INLR
     C                   MOVE      '1'           QQIN12
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C****************************
     C*   ROLL UP REQUEST        *
     C****************************
     C*
     C     *IN18         CABEQ     '1'           SCR01T
     C*
     C****************************
     C*   ROLL DOWN REQUEST      *
     C****************************
     C*
     C     *IN19         CABEQ     '1'           SCR01T
     C*
     C****************************
     C*   *IN10 = HELP REQUESTS  *
     C****************************
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
     C                   ENDIF
     C*
     C                   ENDIF                                                  END CMDKEYS
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
     C                   ENDIF
     C*
     C   19XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   Z-ADD     KEYF(2)       XXFND
     C                   Z-ADD     KEYY(2)       XXFYR
     C                   Z-ADD     KEYC(2)       XXCASE
     C     CMKEY2        CHAIN     PCM01                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYF(1)       XXFND
     C                   Z-ADD     KEYY(1)       XXFYR
     C                   Z-ADD     KEYC(1)       XXCASE
     C     CMKEY2        CHAIN     PCM01                              60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCFND         IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEYF
     C                   MOVE      *BLANK        KEYY
     C                   MOVE      *BLANK        KEYC
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C                   Z-ADD     SCFND         XXFND
     C                   MOVE      *BLANKS       SCFND
     C                   Z-ADD     SCFYR         XXFYR
     C                   MOVE      *BLANKS       SCFYR
     C                   Z-ADD     SCCASE        XXCASE
     C                   MOVE      *BLANKS       SCCASE
     C     CMKEY2        SETLL     PCM01
     C                   READ      PCM01                                  60
     C     XXSS#         IFNE      CMSS#
     C     XXACDT        ORNE      CMACDT
     C     UPCO#         ORNE      CMCO#
     C                   SETON                                            60
     C                   ENDIF
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   ENDIF
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
     C                   MOVE      FN(I1)        QQFND
     C                   MOVE      YR(I1)        QQFYR
     C                   MOVE      CA(I1)        QQCASE
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANK        QQFND
     C                   MOVE      *BLANK        QQFYR
     C                   MOVE      *BLANK        QQCASE
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
     C*
     C                   MOVE      *BLANKS       FN
     C                   MOVE      *BLANKS       YR
     C                   MOVE      *BLANKS       CA
     C                   MOVE      *BLANKS       DS
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C*
     C                   Z-ADD     CMFND         FN(I1)
     C                   Z-ADD     CMFYR         YR(I1)
     C                   MOVE      CMCASE        CA(I1)
     C                   MOVEL     CMACDS        DS(I1)
     C     ENDFIL        ENDSR
     C*
     C*  LOAD FORWARD SUBROUTINE
     C*
     C     LOAD          BEGSR
     C*
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
     C     CMKEY1        READE     PCM01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   ENDDO
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     FN(1)         KEYF(1)
     C                   Z-ADD     FN(14)        KEYF(2)
     C                   Z-ADD     YR(1)         KEYY(1)
     C                   Z-ADD     YR(14)        KEYY(2)
     C                   Z-ADD     CA(1)         KEYC(1)
     C                   Z-ADD     CA(14)        KEYC(2)
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C*
     C                   EXSR      CLR
     C                   Z-ADD     15            I1
     C     OVERP         TAG
     C*
     C*  BEGINNING OF FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   EXSR      LOADB
     C                   GOTO      DONEP
     C                   ENDIF
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   SUB       1             I1
     C                   EXSR      FILL
     C                   READP     PCM01                                  60
     C  N60CMSS#         COMP      XXSS#                              6060
     C  N60CMACDT        COMP      XXACDT                             6060
     C  N60CMCO#         COMP      UPCO#                              6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   ENDDO
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     FN(1)         KEYF(1)
     C                   Z-ADD     FN(14)        KEYF(2)
     C                   Z-ADD     YR(1)         KEYY(1)
     C                   Z-ADD     YR(14)        KEYY(2)
     C                   Z-ADD     CA(1)         KEYC(1)
     C                   Z-ADD     CA(14)        KEYC(2)
     C                   ENDSR
     C*
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*
     C     LOADB         BEGSR
     C*
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     *ZERO         XXFND
     C                   Z-ADD     *ZERO         XXFYR
     C                   Z-ADD     *ZERO         XXCASE
     C     CMKEY2        SETLL     PCM01
     C     CMKEY1        READE     PCM01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   ENDDO
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C*
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     CMKEY1        READE     PCM01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   ENDDO
     C     I1            COMP      14                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
**
ROLL UP
ROLL DOWN
