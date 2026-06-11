     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0331R - PARTIAL FILING SEARCH - REINSURANCE SYSTEM
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  01/28/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  07/07/92  JEI  PROJECT: WC157                                *
     H*                 CHANGED CASE NUMBER TO 7 NUMERIC FOR 'BASE'   *
     H*                 FILES (XXCASE) - CASE NUMBER REMAINS 14 ALPHA *
     H*                 FOR ALL REINSURANCE FILES (X@CASE)            *
     H*                                                               *
     H*  04/03/92  LPK  PROJECT: MD027                                *
     H*                 CHANGED CASE NUMBER FROM 7 NUMERIC TO 14      *
     H*                 ALPHA FIELD.                                  *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     F*****************************************************************
     FW0331F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDPFL4    IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK

     D FL              S              1    DIM(14)
     D*
     D*** BEGIN CHG *** 01/28/98 **************************************
     D**********          YR         14  2 0
     D*** END   CHG *** 01/28/98 **************************************
     D*
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D*
     D*** BEGIN CHG *** 01/28/98 **************************************
     D**********          KEYY        2  2 0
     D KEYY            S              3  0 DIM(2)
     D*** END   CHG *** 01/28/98 **************************************
     D*
     D KEYD            S              8  0 DIM(2)
     D KEYC            S              7  0 DIM(2)
     D KEYM            S              7  0 DIM(2)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D*** BEGIN CHG *** 01/28/98 **************************************
     D*
     D**********  DS
     D**********                              1  28 YR
     D**********                              1   20SCYR01
     D**********                              3   40SCYR02
     D**********                              5   60SCYR03
     D**********                              7   80SCYR04
     D**********                              9  100SCYR05
     D**********                             11  120SCYR06
     D**********                             13  140SCYR07
     D**********                             15  160SCYR08
     D**********                             17  180SCYR09
     D**********                             19  200SCYR10
     D**********                             21  220SCYR11
     D**********                             23  240SCYR12
     D**********                             25  260SCYR13
     D**********                             27  280SCYR14
     D*
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
     D*** END   CHG *** 01/28/98 **************************************
     D*
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
     D                 DS
     D  CA                     1     98  0
     D                                     DIM(14)
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
     D  MC                     1     98  0
     D                                     DIM(14)
     D  SCMC01                 1      7  0
     D  SCMC02                 8     14  0
     D  SCMC03                15     21  0
     D  SCMC04                22     28  0
     D  SCMC05                29     35  0
     D  SCMC06                36     42  0
     D  SCMC07                43     49  0
     D  SCMC08                50     56  0
     D  SCMC09                57     63  0
     D  SCMC10                64     70  0
     D  SCMC11                71     77  0
     D  SCMC12                78     84  0
     D  SCMC13                85     91  0
     D  SCMC14                92     98  0
     D                 DS
     D  NM                     1    420
     D                                     DIM(14)
     D  SCNM01                 1     30
     D  SCNM02                31     60
     D  SCNM03                61     90
     D  SCNM04                91    120
     D  SCNM05               121    150
     D  SCNM06               151    180
     D  SCNM07               181    210
     D  SCNM08               211    240
     D  SCNM09               241    270
     D  SCNM10               271    300
     D  SCNM11               301    330
     D  SCNM12               331    360
     D  SCNM13               361    390
     D  SCNM14               391    420
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*                                                               *
     D*** BEGIN ADD *** 01/28/98 ***********************************************
     D OPTNM           C                   CONST('Specific Partial Fil-
     D                                     ing Maintenance')

     I*** END   ADD *** 01/28/98 ***********************************************
     I*

     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
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
     C*  PARTIAL FILING FILE
     C*
     C     PFKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    XXRNDT
     C                   KFLD                    X@CASE
     C                   KFLD                    XXMCLM            7 0
     C*
     C     PFKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    XXCASE
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    PFCASE        X@CASE
     C     *LIKE         DEFINE    PFDT          XXRNDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C*                                                               *
     C*** BEGIN CHG *** 01/28/98 ***********************************************
     C**********           PARM           QQFYR   20
     C                   PARM                    QQFYR             3 0
     C*** END   CHG *** 01/28/98 ***********************************************
     C*
     C                   PARM                    QQRNDT            8 0
     C                   PARM                    QQCASE            7 0
     C                   PARM                    QQMCLM            7 0
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
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C*
     C**********           MOVE QQPRDS    SCPRDS
     C*
     C*  CENTER PROGRAM NAME
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        SCPRDS
     C                   MOVEL(P)  ##DATA        QQPRDS
     C*
     C*** END   CHG *** 01/28/98 **************************************
     C*
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
     C     QQFYR         COMP      *ZERO                                  21
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
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   Z-ADD     *ZERO         QQFYR
     C                   Z-ADD     *ZERO         QQRNDT
     C                   MOVE      *ZEROS        QQCASE
     C                   Z-ADD     *ZERO         QQMCLM
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   Z-ADD     *ZERO         QQFYR
     C                   Z-ADD     *ZERO         QQRNDT
     C                   MOVE      *ZEROS        QQCASE
     C                   Z-ADD     *ZERO         QQMCLM
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
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
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
     C*
     C                   Z-ADD     KEYD(2)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
     C*
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      KEYC(2)       X@CASE
     C                   Z-ADD     KEYM(2)       XXMCLM
     C     PFKEY         CHAIN     PPF01                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYY(1)       XXFYR
     C*
     C                   Z-ADD     KEYD(1)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
     C*
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      KEYC(1)       X@CASE
     C                   Z-ADD     KEYM(1)       XXMCLM
     C     PFKEY         CHAIN     PPF01                              60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCFYR         IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEYY
     C                   MOVE      *BLANK        KEYD
     C                   MOVE      *BLANK        KEYC
     C                   MOVE      *BLANK        KEYM
     C                   MOVE      *BLANK        XXUP
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   SETOFF                                       21
     C                   Z-ADD     SCFYR         XXFYR
     C                   MOVE      *BLANKS       SCFYR
     C*
     C                   Z-ADD     SCRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
     C*
     C                   MOVE      *BLANKS       SCRNDT
     C                   MOVE      *ZEROS        X@CASE
     C                   Z-ADD     0             XXMCLM
     C     PFKEY         SETLL     PPF01
     C                   READ      PPF01                                  60
     C     UPCO#         IFNE      PFCO#
     C     UPFND         ORNE      PFFND
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
     C                   Z-ADD     YR(I1)        QQFYR
     C                   Z-ADD     DT(I1)        QQRNDT
     C                   MOVE      CA(I1)        QQCASE
     C                   Z-ADD     MC(I1)        QQMCLM
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   Z-ADD     *ZERO         QQFYR
     C                   Z-ADD     *ZERO         QQRNDT
     C                   MOVE      *ZERO         QQCASE
     C                   Z-ADD     *ZERO         QQMCLM
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
     C                   MOVE      *ZEROS        YR
     C                   MOVE      *ZEROS        DT
     C                   MOVE      *ZEROS        CA
     C                   MOVE      *ZEROS        MC
     C                   MOVE      *BLANKS       NM
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   Z-ADD     PFFYR         YR(I1)
     C*
     C                   Z-ADD     PFRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C*
     C                   MOVE      PFCASE        CA(I1)
     C                   MOVE      PFMCLM        MC(I1)
     C     PFCASE        IFNE      *ZEROS
     C                   Z-ADD     PFFYR         XXFYR
     C                   MOVE      PFCASE        XXCASE
     C     CMKEY         CHAIN     PCM01                              40
     C     CDKEY         CHAIN     PCD01                              40
     C   40              GOTO      ENDFIL
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        NM(I1)
     C                   END
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
     C     PFKEY1        READE     PPF01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     YR(1)         KEYY(1)
     C                   Z-ADD     YR(14)        KEYY(2)
     C                   Z-ADD     DT(1)         KEYD(1)
     C                   Z-ADD     DT(14)        KEYD(2)
     C                   Z-ADD     CA(1)         KEYC(1)
     C                   Z-ADD     CA(14)        KEYC(2)
     C                   Z-ADD     MC(1)         KEYM(1)
     C                   Z-ADD     MC(14)        KEYM(2)
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
     C                   READP     PPF01                                  60
     C  N60PFCO#         COMP      UPCO#                              6060
     C  N60PFFND         COMP      UPFND                              6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     YR(1)         KEYY(1)
     C                   Z-ADD     YR(14)        KEYY(2)
     C                   Z-ADD     DT(1)         KEYD(1)
     C                   Z-ADD     DT(14)        KEYD(2)
     C                   Z-ADD     CA(1)         KEYC(1)
     C                   Z-ADD     CA(14)        KEYC(2)
     C                   Z-ADD     MC(1)         KEYM(1)
     C                   Z-ADD     MC(14)        KEYM(2)
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
     C                   Z-ADD     *ZERO         XXFYR
     C                   Z-ADD     0             XXRNDT
     C                   MOVE      *ZEROS        XXCASE
     C                   Z-ADD     0             XXMCLM
     C     PFKEY         SETLL     PPF01
     C     PFKEY1        READE     PPF01                                  60
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
     C     PFKEY1        READE     PPF01                                  60
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
