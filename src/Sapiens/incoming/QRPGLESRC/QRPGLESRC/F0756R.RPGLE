     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*                                                               *
     H* PROGRAM: F0750R                                               *
     H* DESCRIPTION:  REQ LTR SRCH BY PROV - SCRN PGM                 *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H* 07/22/92 KRR     PROJ: 99AAC                                  *
     H*                  1. ADDED SUB# TO ALL PROCESSING              *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*****************************************************************
     H* FILES:                                                        *
     H*  FALTL: CO/FND/FYR/CASE/FEIN/MOD/LTR#     NO S/O              *
     H*****************************************************************
     FF0756F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FFALTL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMCDL     IF   E           K DISK

     D*
     D PTL             S             40    DIM(1) CTDATA PERRCD(1)              CALLED PGM TITLES
     D FL              S              1    DIM(14)                              POSITION FLAG
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEY1            S              9  0 DIM(2)                               PROVIDER ID
     D KEY2            S              3  0 DIM(2)                               MODIFIER
     D KEY3            S              9  0 DIM(2)                               LETTER #
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  PR                     1    126  0
     D                                     DIM(14)                              PROVIDER ID
     D  SCPR01                 1      9  0
     D  SCPR02                10     18  0
     D  SCPR03                19     27  0
     D  SCPR04                28     36  0
     D  SCPR05                37     45  0
     D  SCPR06                46     54  0
     D  SCPR07                55     63  0
     D  SCPR08                64     72  0
     D  SCPR09                73     81  0
     D  SCPR10                82     90  0
     D  SCPR11                91     99  0
     D  SCPR12               100    108  0
     D  SCPR13               109    117  0
     D  SCPR14               118    126  0
     D                 DS
     D  MD                     1     42  0
     D                                     DIM(14)                              MODIFIER
     D  SCMD01                 1      3  0
     D  SCMD02                 4      6  0
     D  SCMD03                 7      9  0
     D  SCMD04                10     12  0
     D  SCMD05                13     15  0
     D  SCMD06                16     18  0
     D  SCMD07                19     21  0
     D  SCMD08                22     24  0
     D  SCMD09                25     27  0
     D  SCMD10                28     30  0
     D  SCMD11                31     33  0
     D  SCMD12                34     36  0
     D  SCMD13                37     39  0
     D  SCMD14                40     42  0
     D                 DS
     D  LT                     1    126  0
     D                                     DIM(14)                              LETTER #
     D  SCLT01                 1      9  0
     D  SCLT02                10     18  0
     D  SCLT03                19     27  0
     D  SCLT04                28     36  0
     D  SCLT05                37     45  0
     D  SCLT06                46     54  0
     D  SCLT07                55     63  0
     D  SCLT08                64     72  0
     D  SCLT09                73     81  0
     D  SCLT10                82     90  0
     D  SCLT11                91     99  0
     D  SCLT12               100    108  0
     D  SCLT13               109    117  0
     D  SCLT14               118    126  0
     D                 DS
     D  FR                     1    112  0
     D                                     DIM(14)                              FROM SRV DATE
     D  SCFR01               001    008  0
     D  SCFR02               009    016  0
     D  SCFR03               017    024  0
     D  SCFR04               025    032  0
     D  SCFR05               033    040  0
     D  SCFR06               041    048  0
     D  SCFR07               049    056  0
     D  SCFR08               057    064  0
     D  SCFR09               065    072  0
     D  SCFR10               073    080  0
     D  SCFR11               081    088  0
     D  SCFR12               089    096  0
     D  SCFR13               097    104  0
     D  SCFR14               105    112  0
     D                 DS
     D  TO                     1    112  0
     D                                     DIM(14)                              TO SRV DATE
     D  SCTO01               001    008  0
     D  SCTO02               009    016  0
     D  SCTO03               017    024  0
     D  SCTO04               025    032  0
     D  SCTO05               033    040  0
     D  SCTO06               041    048  0
     D  SCTO07               049    056  0
     D  SCTO08               057    064  0
     D  SCTO09               065    072  0
     D  SCTO10               073    080  0
     D  SCTO11               081    088  0
     D  SCTO12               089    096  0
     D  SCTO13               097    104  0
     D  SCTO14               105    112  0
     D                 DS
     D  PD                     1    112  0
     D                                     DIM(14)                              LAST PRINT DT
     D  SCPD01               001    008  0
     D  SCPD02               009    016  0
     D  SCPD03               017    024  0
     D  SCPD04               025    032  0
     D  SCPD05               033    040  0
     D  SCPD06               041    048  0
     D  SCPD07               049    056  0
     D  SCPD08               057    064  0
     D  SCPD09               065    072  0
     D  SCPD10               073    080  0
     D  SCPD11               081    088  0
     D  SCPD12               089    096  0
     D  SCPD13               097    104  0
     D  SCPD14               105    112  0
     D                 DS
     D  DD                     1    112  0
     D                                     DIM(14)                              DIARY DATE
     D  SCDD01               001    008  0
     D  SCDD02               009    016  0
     D  SCDD03               017    024  0
     D  SCDD04               025    032  0
     D  SCDD05               033    040  0
     D  SCDD06               041    048  0
     D  SCDD07               049    056  0
     D  SCDD08               057    064  0
     D  SCDD09               065    072  0
     D  SCDD10               073    080  0
     D  SCDD11               081    088  0
     D  SCDD12               089    096  0
     D  SCDD13               097    104  0
     D  SCDD14               105    112  0
     D                 DS
     D  AM                     1    126  2
     D                                     DIM(14)                              AMOUNT
     D  SCAM01                 1      9  2
     D  SCAM02                10     18  2
     D  SCAM03                19     27  2
     D  SCAM04                28     36  2
     D  SCAM05                37     45  2
     D  SCAM06                46     54  2
     D  SCAM07                55     63  2
     D  SCAM08                64     72  2
     D  SCAM09                73     81  2
     D  SCAM10                82     90  2
     D  SCAM11                91     99  2
     D  SCAM12               100    108  2
     D  SCAM13               109    117  2
     D  SCAM14               118    126  2
     D                 DS
     D  ST                     1     14
     D                                     DIM(14)                              STATUS
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
     C*** INITIALIZE WORK FIELDS
     C     *LIKE         DEFINE    LTCO#         XXCO#
     C     *LIKE         DEFINE    LTSUB#        XXSUB#
     C     *LIKE         DEFINE    LTFND         XXFND
     C     *LIKE         DEFINE    LTFYR         XXFYR
     C     *LIKE         DEFINE    LTCASE        XXCASE
     C     *LIKE         DEFINE    LTFEIN        XXFEIN
     C     *LIKE         DEFINE    LTMOD         XXMOD
     C     *LIKE         DEFINE    LTLTR#        XXLTR#
     C*
     C*  KEY LIST FOR THE REQUEST LETTER FILE
     C*
     C     LTKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXLTR#
     C*
     C     LTKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    UPFND
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
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#             9
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQLTR#            9
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
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQSUB#        SCSUB#
     C                   MOVE      QQFND         SCFND
     C                   MOVE      QQFYR         SCFYR
     C                   MOVE      QQCASE        SCCASE
     C                   MOVE      QQFEIN        XXFEIN
     C                   MOVE      QQMOD         XXMOD
     C*
     C     QQCALL        COMP      '1'                                    09    INQUIRY
     C     QQCALL        COMP      '2'                                    08    ANOTHER PGM
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
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
     C                   END
     C*
     C*  GET CLAIMANT NAME
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMSS#         XXSS#
     C                   END                                                    ENDIF *IN60
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVEL     '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCCLNM
     C                   ELSE                                                   ELSIF *IN60
     C                   MOVE      *BLANKS       SCCLNM
     C                   END                                                    ENDIF *IN60
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C     XXFEIN        COMP      *ZERO                                  21
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
     C***************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C**************************************
     C*   COMMAND 03 = END OF JOB          *
     C**************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *ZERO         QQFEIN
     C                   MOVE      *ZERO         QQMOD
     C                   GOTO      TERM
     C                   END
     C**************************************
     C*   COMMAND 12 = PRIOR SCREEN        *
     C**************************************
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C**************************************
     C*   COMMAND 06 = PENDED CHARGES      *
     C**************************************
     C     *INKF         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      Q2RECF        XXRECF           10
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
     C*
     C                   Z-ADD     PR(I1)        XXFEIN
     C                   Z-ADD     MD(I1)        XXMOD
     C                   Z-ADD     LT(I1)        XXLTR#
     C*
     C                   MOVE      SCSUB#        QQSUB#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      PR(I1)        QQFEIN
     C                   MOVE      MD(I1)        QQMOD
     C                   MOVE      LT(I1)        QQLTR#            9
     C                   MOVE      FR(I1)        QQFRDT            8
     C                   MOVE      *BLANKS       QQPRDS
     C                   MOVEL     PTL(1)        QQPRDS
     C                   CALL      'F0760R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM                    QQLTR#
     C                   PARM                    QQFRDT
     C                   PARM                    QQRTCD
     C*
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C                   END                                                    ENDIF GE/LE
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    ENDIF SCR01
     C                   END                                                    ENDIF *INKF
     C**************************************
     C*   ROLL UP REQUEST                  *
     C**************************************
     C     *IN18         CABEQ     '1'           SCR01T
     C**************************************
     C*   ROLL DOWN REQUEST                *
     C**************************************
     C     *IN19         CABEQ     '1'           SCR01T
     C**************************************
     C*   PROCESS HELP REQUESTS            *
     C**************************************
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
     C*  VALIDATE THE DELETE FLAG ENTRIES
     C*
     C                   DO        14            V                 2 0
     C     V             ADD       30            X1                3 0
     C     FL(V)         IFNE      ' '
     C     FL(V)         ANDNE     'D'
     C                   MOVE      '1'           *IN22
     C                   MOVE      '1'           *IN(X1)
     C                   END
     C                   END
     C     *IN22         CABEQ     '1'           STRT
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   Z-ADD     KEY1(2)       XXFEIN
     C                   Z-ADD     KEY2(2)       XXMOD
     C                   Z-ADD     KEY3(2)       XXLTR#
     C     LTKEY1        CHAIN     PLT01                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEY1(1)       XXFEIN
     C                   Z-ADD     KEY2(1)       XXMOD
     C                   Z-ADD     KEY3(1)       XXLTR#
     C     LTKEY1        CHAIN     PLT01                              60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCFEIN        IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEY1
     C                   MOVE      *BLANK        KEY2
     C                   MOVE      *BLANK        KEY3
     C                   MOVE      *BLANK        XXUP
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   SETOFF                                       21
     C                   Z-ADD     SCFEIN        XXFEIN
     C                   Z-ADD     *ZERO         SCFEIN
     C                   Z-ADD     SCMOD         XXMOD
     C                   Z-ADD     *ZERO         SCMOD
     C     LTKEY1        SETLL     PLT01
     C     LTKEY2        READE     PLT01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  RETURN TO CALLING PROGRAM
     C*
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
     C                   MOVE      LT(I1)        QQLTR#
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C     I1            IFLT      1
     C     I1            ANDGT     14
     C                   MOVE      *BLANKS       QQLTR#
     C                   END
     C                   MOVE      '*'           QQRTCD
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C********************************************************************
     C*  CLEAR SCREEN ARRAYS       ** SUBROUTINE **              C L R   *
     C********************************************************************
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       PR
     C                   MOVE      *BLANKS       MD
     C                   MOVE      *BLANKS       LT
     C                   MOVE      *BLANKS       FR
     C                   MOVE      *BLANKS       TO
     C                   MOVE      *BLANKS       PD
     C                   MOVE      *BLANKS       DD
     C                   MOVE      *BLANKS       AM
     C                   MOVE      *BLANKS       ST
     C                   Z-ADD     *ZERO         I1                3 0
     C                   ENDSR
     C********************************************************************
     C*  FILL SCREEN ARRAYS      ** SUBROUTINE **                F I L L *
     C********************************************************************
     C     FILL          BEGSR
     C                   Z-ADD     LTFEIN        PR(I1)
     C                   Z-ADD     LTMOD         MD(I1)
     C                   Z-ADD     LTLTR#        LT(I1)
     C*
     C                   Z-ADD     LTFSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         FR(I1)
     C*
     C                   Z-ADD     LTTSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         TO(I1)
     C*
     C                   Z-ADD     LTPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PD(I1)
     C*
     C                   Z-ADD     LTDIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DD(I1)
     C*
     C                   Z-ADD     LTAMT         AM(I1)
     C                   MOVE      LTSTAT        ST(I1)
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
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     LTKEY2        READE     PLT01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     PR(1)         KEY1(1)
     C                   Z-ADD     PR(14)        KEY1(2)
     C                   Z-ADD     MD(1)         KEY2(1)
     C                   Z-ADD     MD(14)        KEY2(2)
     C                   Z-ADD     LT(1)         KEY3(1)
     C                   Z-ADD     LT(14)        KEY3(2)
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
     C                   SUB       1             I1
     C                   EXSR      FILL
     C                   READP     PLT01                                  60
     C     UPCO#         IFNE      LTCO#
     C     SCSUB#        ORNE      LTSUB#
     C     SCFND         ORNE      LTFND
     C     SCFYR         ORNE      LTFYR
     C     SCCASE        ORNE      LTCASE
     C                   MOVE      '1'           *IN60
     C                   END
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     PR(1)         KEY1(1)
     C                   Z-ADD     PR(14)        KEY1(2)
     C                   Z-ADD     MD(1)         KEY2(1)
     C                   Z-ADD     MD(14)        KEY2(2)
     C                   Z-ADD     LT(1)         KEY3(1)
     C                   Z-ADD     LT(14)        KEY3(2)
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
     C                   Z-ADD     *ZERO         XXFEIN
     C                   Z-ADD     *ZERO         XXMOD
     C     LTKEY1        SETLL     PLT01
     C     LTKEY2        READE     PLT01                                  60
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
     C     LTKEY2        READE     PLT01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
      *========================================================================*
**  TABLE PTL - PROGRAM TITLES
   PENDED CHARGES SEARCH FOR A LETTER
**
ROLL UP
ROLL DOWN
