     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W4310R - LATE PAYMENT MAINTENANCE/INQUIRY
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  05/25/07 C:191 PROJECT: WC1515                               *
     H*                 Check claims authority                        *
     H*                 for no-auth, do not include claimant name     *
     H*                                                               *
     H*****************************************************************
     FW4310F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
     FWDPAL     IF   E           K DISK
     FWMLPL1    UF   E           K DISK

     D FL              S              1    DIM(6)
     D SCD             S             10    DIM(6) CTDATA PERRCD(1)
     D SQ              S              5  0 DIM(6)
     D OCD             S              2    DIM(6)
     D KEYD            S              6  0 DIM(2)
     D KEYY            S              2  0 DIM(2)
     D KEYC            S              7  0 DIM(2)
     D KEYS            S              5  0 DIM(2)
     D YY              S             15    DIM(2) CTDATA PERRCD(1)
     D MD              S              2  0 DIM(12) CTDATA PERRCD(12)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  CD                     1     12
     D                                     DIM(6)
     D  SCCD01                 1      2
     D  SCCD02                 3      4
     D  SCCD03                 5      6
     D  SCCD04                 7      8
     D  SCCD05                 9     10
     D  SCCD06                11     12
     D                 DS
     D  YR                     1     12  0
     D                                     DIM(6)
     D  SCYR01                 1      2  0
     D  SCYR02                 3      4  0
     D  SCYR03                 5      6  0
     D  SCYR04                 7      8  0
     D  SCYR05                 9     10  0
     D  SCYR06                11     12  0
     D                 DS
     D  CS                     1     42  0
     D                                     DIM(6)
     D  SCCS01                 1      7  0
     D  SCCS02                 8     14  0
     D  SCCS03                15     21  0
     D  SCCS04                22     28  0
     D  SCCS05                29     35  0
     D  SCCS06                36     42  0
     D                 DS
     D  NM                     1    138
     D                                     DIM(6)
     D  SCNM01                 1     23
     D  SCNM02                24     46
     D  SCNM03                47     69
     D  SCNM04                70     92
     D  SCNM05                93    115
     D  SCNM06               116    138
     D                 DS
     D  PC                     1     12
     D                                     DIM(6)
     D  SCPC01                 1      2
     D  SCPC02                 3      4
     D  SCPC03                 5      6
     D  SCPC04                 7      8
     D  SCPC05                 9     10
     D  SCPC06                11     12
     D                 DS
     D  TP                     1      6
     D                                     DIM(6)
     D  SCTP01                 1      1
     D  SCTP02                 2      2
     D  SCTP03                 3      3
     D  SCTP04                 4      4
     D  SCTP05                 5      5
     D  SCTP06                 6      6
     D                 DS
     D  CK                     1     42  0
     D                                     DIM(6)
     D  SCCK01                 1      7  0
     D  SCCK02                 8     14  0
     D  SCCK03                15     21  0
     D  SCCK04                22     28  0
     D  SCCK05                29     35  0
     D  SCCK06                36     42  0
     D                 DS
     D  B2                     1     12
     D                                     DIM(6)
     D  SCB201                 1      2
     D  SCB202                 3      4
     D  SCB203                 5      6
     D  SCB204                 7      8
     D  SCB205                 9     10
     D  SCB206                11     12
     D                 DS
     D  WK                     1     18  0
     D                                     DIM(6)
     D  SCWK01                 1      3  0
     D  SCWK02                 4      6  0
     D  SCWK03                 7      9  0
     D  SCWK04                10     12  0
     D  SCWK05                13     15  0
     D  SCWK06                16     18  0
     D                 DS
     D  DA                     1      6  0
     D                                     DIM(6)
     D  SCDA01                 1      1  0
     D  SCDA02                 2      2  0
     D  SCDA03                 3      3  0
     D  SCDA04                 4      4  0
     D  SCDA05                 5      5  0
     D  SCDA06                 6      6  0
     D                 DS
     D  FR                     1    048  0
     D                                     DIM(6)
     D  SCFR01               001    008  0
     D  SCFR02               009    016  0
     D  SCFR03               017    024  0
     D  SCFR04               025    032  0
     D  SCFR05               033    040  0
     D  SCFR06               041    048  0
     D                 DS
     D  TO                     1    048  0
     D                                     DIM(6)
     D  SCTO01               001    008  0
     D  SCTO02               009    016  0
     D  SCTO03               017    024  0
     D  SCTO04               025    032  0
     D  SCTO05               033    040  0
     D  SCTO06               041    048  0
     D                 DS
     D  PO                     1    048  0
     D                                     DIM(6)
     D  SCPO01               001    008  0
     D  SCPO02               009    016  0
     D  SCPO03               017    024  0
     D  SCPO04               025    032  0
     D  SCPO05               033    040  0
     D  SCPO06               041    048  0
     D                 DS
     D  TR                     1    048  0
     D                                     DIM(6)
     D  SCTR01               001    008  0
     D  SCTR02               009    016  0
     D  SCTR03               017    024  0
     D  SCTR04               025    032  0
     D  SCTR05               033    040  0
     D  SCTR06               041    048  0
     D                 DS
     D  AM                     1     54  2
     D                                     DIM(6)
     D  SCAM01                 1      9  2
     D  SCAM02                10     18  2
     D  SCAM03                19     27  2
     D  SCAM04                28     36  2
     D  SCAM05                37     45  2
     D  SCAM06                46     54  2
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** BEG ADD ****** 05/25/07 **********************************
     D XXATTX          C                   CONST('*** NO AUTHORITY ***')
      *** END ADD ****** 05/25/07 **********************************

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND             3 0
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  PAYMENT FILE KEY
     C*
     C     APKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    LPFYR
     C                   KFLD                    LPCASE
     C                   KFLD                    LPSEQ#
     C*
     C*  LATE PAYMENT FILE
     C*
     C     LPKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C     LPKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXTRDT
     C*
     C     LPKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXTRDT
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXSEQ#
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    LPFYR
     C                   KFLD                    LPCASE
     C*
     C     *LIKE         DEFINE    CMDT          XXTRDT
     C     *LIKE         DEFINE    CMDT          XXFRDT
     C     *LIKE         DEFINE    CMDT          XXTODT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C*
     C*  GET THE COMPANY AND FUND RECORDS
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      UPFND         SCFND
     C                   MOVE      FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C     XXUP          IFEQ      '1'
     C                   MOVE      *BLANKS       SCUP
     C                   ELSE
     C                   MOVEL     YY(1)         SCUP
     C                   END
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C                   MOVEL     YY(2)         SCDOWN
     C                   END
     C                   EXFMT     SCR02
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      *BLANK        XXUP
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*  CMD 10 - TABLE FILE SEARCH - LATE CODE
     C*
     C     *INKJ         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   DO        6             X                 2 0
     C                   MOVEL     CD(X)         XX2               2
     C     XX2           IFEQ      '? '
     C     HFLD          OREQ      SCD(X)
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2221R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C*
     C                   MOVEL     SCKEY         CD(X)
     C     *INKC         CABEQ     *ON           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANK
     C     X             ADD       24            X1                2 0
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C     X             ADD       65            X3                2 0
     C     *IN(X3)       IFEQ      '1'
     C     X             ADD       25            X1
     C                   ELSE
     C     X             ADD       24            X1                2 0
     C                   END
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    XX2 = '?'
     C                   END                                                    DO 6 X
     C                   END                                                    Q2RECF=SCR02
     C                   GOTO      STRT
     C                   END                                                    *INKJ = '1'
     C*
     C*   ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR02T
     C*
     C*   ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR02T
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
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C*   CMD 24 - DELETE LATE PAYMENT
     C*
     C     *INKY         IFEQ      '1'
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       8             I1
     C                   ADD       1             I1
     C                   DIV       2             I1
     C     I1            IFGE      1
     C     I1            ANDLE     6
     C*
     C                   Z-ADD     TR(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
     C*
     C                   Z-ADD     YR(I1)        XXFYR
     C                   Z-ADD     CS(I1)        XXCASE
     C                   Z-ADD     SQ(I1)        XXSEQ#
     C     LPKEY1        CHAIN     PLP01                              60
     C     *IN60         IFEQ      '0'
     C                   DELETE    PLP01
     C                   END
     C                   END
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   Z-ADD     KEYD(1)       XXTRDT
     C                   Z-ADD     KEYY(1)       XXFYR
     C                   Z-ADD     KEYC(1)       XXCASE
     C                   Z-ADD     KEYS(1)       XXSEQ#
     C     LPKY          SETLL     PLP01
     C     LPKEY         READE     PLP01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  MONTH/YEAR MUST BE VALID
     C*
     C                   MOVE      *BLANKS       XXMNTH            2 0
     C                   MOVEL     SCMMYY        XXMNTH            2 0
     C     XXMNTH        IFLT      01
     C     XXMNTH        ORGT      12
     C                   MOVE      '1'           *IN22
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET BEGINNING & ENDING DATE FOR MONTH
     C*
     C                   MOVE      SCMMYY        X1FRDT            6 0
     C                   MOVEL     '01'          X1FRDT
     C*
     C                   Z-ADD     X1FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   MOVE      SCMMYY        XXYEAR            2 0
     C                   MOVEL     SCMMYY        M1                2 0
     C                   Z-ADD     MD(M1)        XXMDAY            2 0
     C     XXYEAR        DIV       4             XXWK50            5 0
     C                   MVR                     XXWK83            8 3
     C     XXWK83        IFEQ      *ZERO
     C     M1            ANDEQ     2
     C                   Z-ADD     29            XXMDAY
     C                   END
     C*
     C                   MOVE      SCMMYY        X1TODT            6 0
     C                   MOVEL     XXMDAY        X1TODT
     C*
     C                   Z-ADD     X1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVE      '1'           *IN21
     C                   GOTO      REPOS
     C*
     C* PROCESS SCREEN 2 - PAYMENT INFORMATION
     C*
     C     SCR02T        TAG
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
     C                   Z-ADD     KEYD(2)       XXTRDT
     C                   Z-ADD     KEYY(2)       XXFYR             3 0
     C                   Z-ADD     KEYC(2)       XXCASE            7 0
     C                   Z-ADD     KEYS(2)       XXSEQ#            5 0
     C     LPKEY1        CHAIN     PLP01                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYD(1)       XXTRDT
     C                   Z-ADD     KEYY(1)       XXFYR
     C                   Z-ADD     KEYC(1)       XXCASE
     C                   Z-ADD     KEYS(1)       XXSEQ#
     C     LPKEY1        CHAIN     PLP01                              60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCTRDT        IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        KEYD
     C                   MOVE      *BLANK        KEYY
     C                   MOVE      *BLANK        KEYC
     C                   MOVE      *BLANK        KEYS
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   SETOFF                                       21
     C*
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
     C*
     C     XXTRDT        IFLT      XXFRDT
     C                   Z-ADD     XXFRDT        XXTRDT
     C                   END
     C                   MOVE      *ZEROS        SCTRDT
     C     LPKY          SETLL     PLP01
     C     LPKEY         READE     PLP01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF ENTER WAS PRESSED EXECUTE THE RITE SUBROUTINE
     C*
     C                   EXSR      RITE
     C*
     C*  GOTO FIRST SCREEN
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       CD
     C                   MOVE      *BLANKS       YR
     C                   MOVE      *BLANKS       CS
     C                   MOVE      *BLANKS       NM
     C                   MOVE      *BLANKS       PC
     C                   MOVE      *BLANKS       TP
     C                   MOVE      *BLANKS       CK
     C                   MOVE      *BLANKS       B2
     C                   MOVE      *BLANKS       WK
     C                   MOVE      *BLANKS       DA
     C                   MOVE      *BLANKS       FR
     C                   MOVE      *BLANKS       TO
     C                   MOVE      *BLANKS       PO
     C                   MOVE      *BLANKS       TR
     C                   MOVE      *BLANKS       AM
     C                   MOVE      *BLANKS       SQ
     C                   MOVE      *BLANKS       OCD
     C                   Z-ADD     *ZERO         I1                3 0
     C                   MOVEA     '000000'      *IN(65)
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C     I1            ADD       64            X2
     C                   MOVE      '1'           *IN(X2)
     C                   MOVE      LPCODE        CD(I1)
     C                   Z-ADD     LPFYR         YR(I1)
     C                   Z-ADD     LPCASE        CS(I1)
     C                   MOVE      LPSEQ#        SQ(I1)
     C*
     C*  CHAIN TO CLAIM FILE TO GET CLAIMANT NAME
     C*
     C     CMKEY         CHAIN     PCM01                              40
     C  N40CDKEY         CHAIN     PCD01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        NM(I1)
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      UPCO#         ##CO#
     C                   MOVE      UPFND         ##FND
     C                   MOVE      LPFYR         ##FYR
     C                   MOVE      LPCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *BLANKS       NM(I1)
     C                   MOVEL(P)  XXATTX        NM(I1)
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C                   END
     C*
     C*  GET PAYMENT DETAIL INFO
     C*
     C     APKEY         CHAIN     PPA01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVE      PATRCD        PC(I1)
     C                   MOVE      PATYPE        TP(I1)
     C                   MOVE      PACHK#        CK(I1)
     C                   MOVE      PABR2         B2(I1)
     C                   MOVE      PAWKS         WK(I1)
     C                   MOVE      PADAYS        DA(I1)
     C*
     C                   Z-ADD     PAFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         FR(I1)
     C*
     C                   Z-ADD     PATODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         TO(I1)
     C*
     C                   Z-ADD     PAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PO(I1)
     C*
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         TR(I1)
     C*
     C                   Z-ADD     PAAMT         AM(I1)
     C                   END
     C*
     C                   EXCEPT    PLP01E
     C                   MOVE      LPCODE        OCD(I1)
     C                   ENDSR
     C*
     C*  LOAD FORWARD SUBROUTINE
     C*
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C     OVER          TAG
     C*
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     LPTRDT        IFLE      XXTODT
     C                   ADD       1             I1
     C                   EXSR      FILL
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   END
     C  N60LPKEY         READE     PLP01                                  60
     C   60              DO
     C     I1            IFLT      6
     C                   MOVE      '1'           XXUP
     C                   END
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      6                                    61
     C   61              GOTO      OVER
     C     DONE          TAG
     C  N60              EXCEPT    PLP01E
     C     I1            IFNE      0
     C*
     C                   Z-ADD     TR(1)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KEYD(1)
     C*
     C                   Z-ADD     TR(6)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KEYD(2)
     C*
     C                   Z-ADD     YR(1)         KEYY(1)
     C                   Z-ADD     YR(6)         KEYY(2)
     C                   Z-ADD     CS(1)         KEYC(1)
     C                   Z-ADD     CS(6)         KEYC(2)
     C                   Z-ADD     SQ(1)         KEYS(1)
     C                   Z-ADD     SQ(6)         KEYS(2)
     C                   END
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     7             I1
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
     C     LPTRDT        IFLE      XXTODT
     C                   SUB       1             I1
     C                   EXSR      FILL
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   END
     C  N60              READP     PLP01                                  60
     C  N60LPCO#         COMP      UPCO#                              6060
     C  N60LPFND         COMP      UPFND                              6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C  N60              EXCEPT    PLP01E
     C*
     C                   Z-ADD     TR(1)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KEYD(1)
     C*
     C                   Z-ADD     TR(6)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KEYD(2)
     C*
     C                   Z-ADD     YR(1)         KEYY(1)
     C                   Z-ADD     YR(6)         KEYY(2)
     C                   Z-ADD     CS(1)         KEYC(1)
     C                   Z-ADD     CS(6)         KEYC(2)
     C                   Z-ADD     SQ(1)         KEYS(1)
     C                   Z-ADD     SQ(6)         KEYS(2)
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
     C                   Z-ADD     XXFRDT        XXTRDT
     C     LPKY          SETLL     PLP01
     C     LPKEY         READE     PLP01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     LPTRDT        IFLE      XXTODT
     C                   ADD       1             I1
     C                   EXSR      FILL
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   END
     C  N60LPKEY         READE     PLP01                                  60
     C   60              DO
     C     I1            IFLT      6
     C                   MOVE      '1'           XXUP
     C                   END
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      6                                    61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
     C**************************************************************
     C*     RITE SUBROUTINE - UPDATE RECORDS                       *
     C**************************************************************
     C     RITE          BEGSR
     C*
     C                   DO        6             X                 2 0
     C     X             ADD       34            X2                2 0          ERROR
     C*
     C*   COMPARE HOLD FIELDS FROM ORIGINAL SCREEN TO THE CURRENT
     C*   FIELDS. IF THE FIELDS CHANGED, EDIT & UPDATE OR WRITE.
     C*
     C     CD(X)         IFNE      OCD(X)
     C*
     C*  CODE MUST BE VALID
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     CD(X)         XXKEY
     C     TBKEY         CHAIN     PLR01                              21
     C   21              MOVE      '1'           *IN(X2)
     C     *IN21         CABEQ     '1'           STRT
     C*
     C     LRSTAT        COMP      'I'                                    21
     C   21              MOVE      '1'           *IN(X2)
     C     *IN21         CABEQ     '1'           STRT
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C                   Z-ADD     TR(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
     C*
     C                   Z-ADD     YR(X)         XXFYR
     C                   Z-ADD     CS(X)         XXCASE
     C                   Z-ADD     SQ(X)         XXSEQ#
     C     LPKEY1        CHAIN     PLP01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CD(X)         LPCODE
     C                   MOVE      Q1USER        LPUSER
     C                   MOVE      Q1PGM         LPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        LPTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LPDT
     C*
     C                   UPDATE    PLP01
     C                   END
     C*
     C*  MOVE NEW FIELDS TO OLD FIELDS
     C*
     C                   MOVE      CD(X)         OCD(X)
     C*
     C                   END                                                    OLD = NEW
     C                   END                                                    DO 6 X
     C*
     C     RITEN         ENDSR
     OPLP01     E            PLP01E
** SCD
SCCD01
SCCD02
SCCD03
SCCD04
SCCD05
SCCD06
**
ROLL UP
ROLL DOWN
**
312831303130313130313031
