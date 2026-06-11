     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H/TITLE F0579R - MULTI CASES/SAME SS# & ACDT - SRCH - SCRN PGM
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  06/09/20 C:212 Project : SY420
     H*                  Mask SSN
     H*
     H*  06/17/99 C:53  MC145
     H*                 QUALIFY WMCMS FILES W/NON-PDS WCMS FLAG.
     H*
     H*****************************************************************
     FF0579F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMCMLA    IF   E           K DISK    USROPN
     FFMCMLA    IF   E           K DISK    USROPN
     F                                     RENAME(PCM01:PCM02)
     FWMCDL     IF   E           K DISK    USROPN
     FFMCDL     IF   E           K DISK    USROPN
     F                                     RENAME(PCD01:PCD02)
     FSSYLF     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D FL              S              1    DIM(14)                                POSITIONAL FLAG
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEYF            S              3  0 DIM(2)                                 FUND
     D KEYY            S              3  0 DIM(2)                                 FUND YEAR
     D KEYC            S              7  0 DIM(2)                                 CASE
     D*** END   ADD *** 03/09/98 **************************************
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

     I*** BEGIN ADD *** 03/09/98 **************************************
     IPCM02
     I              CMOSCL                      MCOSCL

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
     C*  CLAIM MASTER FILE
     C     CMKEY         KLIST
     C                   KFLD                    XXSS#
     C                   KFLD                    XXACDT
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     CMKEY1        KLIST
     C                   KFLD                    XXSS#
     C                   KFLD                    XXACDT
     C                   KFLD                    UPCO#
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C*   KEY LIST FOR THE FUND AUXILLARY FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#                         ADD 02/25/93
     C                   KFLD                    UPFND
     C*
     C*  FS BASE CONTROL FILE
     C*
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
     C*
     C*  CLAIM MASTER FILE
     C     CMKY2         KLIST
     C                   KFLD                    XXSS#
     C                   KFLD                    XXACDT
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQSS#             9
     C                   PARM                    QQACDT            8            (MMDDYY ORDER)
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
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
     C                   END
     C*
     C*  GET COMPANY NAME
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SCCONM
     C                   ELSE
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   END
     C*
     C*** BEG ADD *** 06/17/99 ****************************
     C                   MOVEL(P)  'SY'          XSKEY
     C     FBKY          CHAIN     SSY01                              61
     C                   MOVEL     SYFL05        XXNPWC            1
     C*** END ADD *** 06/17/99 ****************************
     C*
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'FB'          XSKEY
     C     FBKY          CHAIN     SFB01                              60
     C     *IN60         IFEQ      '0'
     C     FBFL01        IFEQ      'Y'
     C     F1KEY         CHAIN     PF101                              50
     C     *IN50         IFEQ      '0'
     C     F1OUTA        IFEQ      'N'
     C     XXNPWC        ANDNE     'Y'                                          ADD 06/17/99
     C                   MOVE      '1'           *IN71
     C                   OPEN      WMCMLA
     C                   OPEN      WMCDL
     C                   ELSE                                                   ELSE F1OUTA
     C                   MOVE      '0'           *IN71
     C                   OPEN      FMCMLA
     C                   OPEN      FMCDL
     C                   END                                                    END F1OUTA
     C                   END                                                    END *IN50
     C                   ELSE                                                   ELSE FBFL01
     C                   MOVE      '0'           *IN71
     C                   OPEN      FMCMLA
     C                   OPEN      FMCDL
     C                   END                                                    END FBFL01
     C                   END                                                    END *IN60
     C*
     C*  GET THE CLAIMANT NAME
     C   71CDKEY         CHAIN     PCD01                              60
     C  N71CDKEY         CHAIN     PCD02                              60
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
     C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C     SCFND         COMP      *ZERO                                  21
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
     C********************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C     *IN11         IFEQ      '1'
     C****************************
     C*   F3 = EXIT              *
     C****************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *BLANK        QQFYR
     C                   MOVE      *BLANK        QQCASE
     C                   MOVE      *BLANK        QQSS#
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C****************************
     C*  F12 = PREVIOUS          *
     C****************************
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *BLANK        QQFYR
     C                   MOVE      *BLANK        QQCASE
     C                   MOVE      *BLANK        QQSS#
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C****************************
     C*   ROLL UP REQUEST        *
     C****************************
     C     *IN18         CABEQ     '1'           SCR01T
     C****************************
     C*   ROLL DOWN REQUEST      *
     C****************************
     C     *IN19         CABEQ     '1'           SCR01T
     C****************************
     C*   *IN10 = HELP REQUESTS  *
     C****************************
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
     C                   END                                                    END CMDKEYS
     C********************************************************************
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
     C                   Z-ADD     KEYF(2)       XXFND
     C                   Z-ADD     KEYY(2)       XXFYR
     C                   Z-ADD     KEYC(2)       XXCASE
     C   71CMKEY         CHAIN     PCM01                              60
     C  N71CMKY2         CHAIN     PCM02                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYF(1)       XXFND
     C                   Z-ADD     KEYY(1)       XXFYR
     C                   Z-ADD     KEYC(1)       XXCASE
     C   71CMKEY         CHAIN     PCM01                              60
     C  N71CMKY2         CHAIN     PCM02                              60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCFND         IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEYF
     C                   MOVE      *BLANK        KEYY
     C                   MOVE      *BLANK        KEYC
     C                   MOVE      *BLANK        XXUP
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   SETOFF                                       21
     C                   Z-ADD     SCFND         XXFND
     C                   MOVE      *BLANKS       SCFND
     C                   Z-ADD     SCFYR         XXFYR
     C                   MOVE      *BLANKS       SCFYR
     C                   Z-ADD     SCCASE        XXCASE
     C                   MOVE      *BLANKS       SCCASE
     C   71CMKEY         SETLL     PCM01
     C   71              READ      PCM01                                  60
     C  N71CMKY2         SETLL     PCM02
     C  N71              READ      PCM02                                  60
     C     XXSS#         IFNE      CMSS#
     C     XXACDT        ORNE      CMACDT
     C     UPCO#         ORNE      CMCO#
     C                   SETON                                            60
     C                   END
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
     C                   MOVE      FN(I1)        QQFND
     C                   MOVE      YR(I1)        QQFYR
     C                   MOVE      CA(I1)        QQCASE
     C                   GOTO      TERM
     C                   END
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
     C                   Z-ADD     CMFND         FN(I1)
     C                   Z-ADD     CMFYR         YR(I1)
     C                   MOVE      CMCASE        CA(I1)
     C                   MOVEL     CMACDS        DS(I1)
     C     ENDFIL        ENDSR
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
     C   71CMKEY1        READE     PCM01                                  60
     C  N71CMKEY1        READE     PCM02                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
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
     C   71              READP     PCM01                                  60
     C  N71              READP     PCM02                                  60
     C  N60CMSS#         COMP      XXSS#                              6060
     C  N60CMACDT        COMP      XXACDT                             6060
     C  N60CMCO#         COMP      UPCO#                              6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
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
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     *ZERO         XXFND
     C                   Z-ADD     *ZERO         XXFYR
     C                   Z-ADD     *ZERO         XXCASE
     C   71CMKEY         SETLL     PCM01
     C   71CMKEY1        READE     PCM01                                  60
     C  N71CMKY2         SETLL     PCM02
     C  N71CMKEY1        READE     PCM02                                  60
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
     C   71CMKEY1        READE     PCM01                                  60
     C  N71CMKEY1        READE     PCM02                                  60
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
