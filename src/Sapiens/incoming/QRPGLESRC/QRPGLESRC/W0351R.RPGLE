     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0351R - REINSURANCE RECOVERY PAYMENT SEARCH
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  09/26/03 C:53 SUPPORT: WC062 #39219                          *
     H*                CORRECT ROLL ISSUE                             *
     H*                                                               *
     H*  08/30/02 C:15 Project: WC577                                 *
     H*                Chgd F23 to F04.                               *
     H*                                                               *
     H*  01/28/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  10/20/93 C:22 PROJECT: WC207                                 *
     H*                ADDED CODE TO REDUCE AMOUNT RECOVERED WHEN     *
     H*                A RECOVERY PAYMENT IS DELETED                  *
     H*                                                               *
     H*  07/07/92  JEI  PROJECT: WC157                                *
     H*                 CHANGED CASE NUMBER BACK TO 7 NUMERIC FOR     *
     H*                 'BASE' FILES (XXCASE) - CASE NUMBER REMAINS   *
     H*                 14 ALPHA FOR REINSURANCE FILES (X@CASE)       *
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
     FW0351F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWDRVL1    IF   E           K DISK
     FWDRVL2    UF   E           K DISK
     F                                     RENAME(PRV01:PRV02)
     FWMRCL     IF   E           K DISK
     FWMRQL     UF   E           K DISK

     D*
     D*** BEGIN ADD *** 01/28/98 ***********************************************
     D MD              S             10    DIM(4) CTDATA PERRCD(1)              SCMODE TABLE
     D*** END   ADD *** 01/28/98 ***********************************************
     D*
     D FL              S              1    DIM(6)
     D TM              S              6  0 DIM(6)
     D RQ              S              5  0 DIM(6)
     D KEYD            S              7  0 DIM(2)
     D KEYT            S              6  0 DIM(2)
     D YY              S             15    DIM(2) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
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
     D  CO                     1     18  0
     D                                     DIM(6)
     D  SCCO01                 1      3  0
     D  SCCO02                 4      6  0
     D  SCCO03                 7      9  0
     D  SCCO04                10     12  0
     D  SCCO05                13     15  0
     D  SCCO06                16     18  0
     D                 DS
     D  NM                     1     90
     D                                     DIM(6)
     D  SCNM01                 1     15
     D  SCNM02                16     30
     D  SCNM03                31     45
     D  SCNM04                46     60
     D  SCNM05                61     75
     D  SCNM06                76     90
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
     D  DS                     1    180
     D                                     DIM(6)
     D  SCDS01                 1     30
     D  SCDS02                31     60
     D  SCDS03                61     90
     D  SCDS04                91    120
     D  SCDS05               121    150
     D  SCDS06               151    180
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
     D*                                                               *
     D*** BEGIN ADD *** 01/28/98 ***********************************************
     D OPTNM           C                   CONST('Reinsurance Recovery-
     D                                      Received Maint.')

     I*** END   ADD *** 01/28/98 ***********************************************
     I*

     C*
     C*   KEY LITY FOR THE COMAMNY FILE
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
     C*  REIN RECOVERY RECEIVED FILE
     C*
     C     RVKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    QQASFL
     C                   KFLD                    XXFYR
     C                   KFLD                    X@CASE
     C*
     C     RVKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    QQASFL
     C                   KFLD                    XXFYR
     C                   KFLD                    X@CASE
     C                   KFLD                    XXTRDT
     C                   KFLD                    XXTIME            6 0
     C*
     C     RVKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    QQASFL
     C                   KFLD                    X@CASE
     C                   KFLD                    XXREQ#            5 0
     C                   KFLD                    SCRCO
     C                   KFLD                    SCAMT
     C                   KFLD                    XXTRAN
     C                   KFLD                    SCCHK#
     C*
     C*  REIN PAYMENT RECEIVED FILE
     C*
     C     RQKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    QQASFL
     C                   KFLD                    X@CASE
     C                   KFLD                    XXREQ#
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*  REIN COMPANY FILE
     C*
     C     RCKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    RVRCO
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    XXCASE
     C*
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    RVCASE        X@CASE
     C     *LIKE         DEFINE    RVDT          XXTRAN
     C     *LIKE         DEFINE    RVDT          XXTRDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    XXFYR             3 0
     C                   PARM                    QQASFL            1
     C                   PARM                    XXCASE
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
     C                   Z-ADD     XXFYR         SCFYR
     C                   MOVE      XXCASE        SCCASE
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      XXCASE        X@CASE
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
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C                   Z-ADD     FNFND         SCFND
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C                   MOVE      FNNAME        SCNAME
     C     XXFYR         IFEQ      *ZERO
     C                   Z-ADD     UPFYR         XXFYR
     C                   END
     C*
     C*  GET CLAIMANT NAME IF SPECIFIC
     C*
     C                   MOVE      '1'           *IN79
     C     QQASFL        IFEQ      'S'
     C                   MOVE      '0'           *IN79
     C                   MOVE      *BLANKS       SCCNAM
     C     CMKEY         CHAIN     PCM01                              60
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCCNAM
     C                   END
     C                   END
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
     C     QQCALL        COMP      '1'                                    09
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      REPOS
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
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
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
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
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
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
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
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
     C*   F23 - DELETE TRANSACTION
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C********** *INKY     IFEQ '1'
     C*** Begin dlt *** 08/30/02 **************************************
     C********** *INKX     IFEQ *ON
     C*** End dlt ***** 08/30/02 **************************************
     C*** END   CHG *** 01/28/98 **************************************
     C*** Begin add *** 08/30/02 **************************************
     C     *INKD         IFEQ      *ON
     C*** End add ***** 08/30/02 **************************************
     C     Q2RECF        ANDEQ     'SCR01'
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       9             I1
     C                   ADD       1             I1
     C                   DIV       2             I1
     C     I1            IFGE      1
     C     I1            ANDLE     6
     C*
     C                   Z-ADD     CK(I1)        SCCHK#
     C                   Z-ADD     CO(I1)        SCRCO
     C                   MOVE      NM(I1)        SCSHNM
     C                   Z-ADD     PO(I1)        SCPODT
     C                   Z-ADD     TR(I1)        SCTRAN
     C                   MOVE      DS(I1)        SCDESC
     C                   Z-ADD     AM(I1)        SCAMT
     C                   Z-ADD     RQ(I1)        XXREQ#
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C                   MOVE      MD(4)         SCMODE
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
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
     C* PROCESS SCREEN 1
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
     C                   Z-ADD     KEYD(2)       XXTRDT
     C                   Z-ADD     KEYT(2)       XXTIME
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      XXCASE        X@CASE
     C     RVKEY1        CHAIN     PRV01                              60
     C                   MOVE      *ON           *IN15                          ADD 09/26/03
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYD(1)       XXTRDT
     C                   Z-ADD     KEYT(1)       XXTIME
     C     RVKEY1        CHAIN     PRV01                              60
     C                   MOVE      *ON           *IN16                          ADD 09/26/03
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
     C                   MOVE      *BLANK        KEYT
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   SETOFF                                       21
     C*
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
     C*
     C                   MOVE      *ZEROS        SCTRDT
     C                   MOVE      *ZEROS        XXTIME
     C     RVKEY1        SETLL     PRV01
     C     RVKEY         READE     PRV01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       9             I1                3 0
     C                   ADD       1             I1
     C                   DIV       2             I1
     C*
     C*  VALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     6
     C                   MOVE      '*'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   GOTO      TERM
     C*
     C*  PROCESS SCREEN TWO - DELETE TRANSACTION
     C*
     C     SCR02T        TAG
     C*
     C*  ENTER HAS BEEN PRESSED, DELETE TRANSACTION
     C*
     C                   Z-ADD     SCTRAN        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRAN
     C*
     C     RVKEY2        CHAIN     PRV02                              60
     C  N60              DELETE    PRV02
     C*
     C     RQKEY         CHAIN     PRQ01                              60
     C     *IN60         IFEQ      '0'
     C     RQRCVD        SUB       SCAMT         RQRCVD
     C                   UPDATE    PRQ01
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     KEYD(1)       XXTRDT
     C                   Z-ADD     KEYT(1)       XXTIME
     C     RVKEY1        SETLL     PRV01
     C     RVKEY         READE     PRV01                                  60
     C                   EXSR      LOAD
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
     C                   MOVE      *BLANKS       CK
     C                   MOVE      *BLANKS       CO
     C                   MOVE      *BLANKS       NM
     C                   MOVE      *BLANKS       PO
     C                   MOVE      *BLANKS       TR
     C                   MOVE      *BLANKS       DS
     C                   MOVE      *BLANKS       AM
     C                   MOVE      *BLANKS       RQ
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVE      RVTIME        TM(I1)
     C                   MOVE      RVCHK#        CK(I1)
     C                   MOVE      RVRCO         CO(I1)
     C     RCKEY         CHAIN     PRC01                              61
     C                   MOVEL     RCSHNM        NM(I1)
     C*
     C                   Z-ADD     RVPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PO(I1)
     C*
     C                   Z-ADD     RVTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         TR(I1)
     C*
     C                   MOVEL     RVDESC        DS(I1)
     C                   Z-ADD     RVAMT         AM(I1)
     C                   Z-ADD     RVREQ#        RQ(I1)
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
     C     *IN15         IFEQ      *OFF                                         ADD 09/26/03
     C                   ADD       1             I1
     C                   EXSR      FILL
     C*** BEG ADD *** 09/26/03 ****************************
     C                   ELSE
     C                   MOVE      *OFF          *IN15
     C                   ENDIF
     C*** END ADD *** 09/26/03 ****************************
     C     RVKEY         READE     PRV01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      6                                    61
     C   61              GOTO      OVER
     C     DONE          TAG
     C*
     C                   Z-ADD     TR(1)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KEYD(1)
     C*
     C                   Z-ADD     TR(6)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KEYD(2)
     C*
     C                   Z-ADD     TM(1)         KEYT(1)
     C                   Z-ADD     TM(6)         KEYT(2)
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
     C     *IN16         IFEQ      *OFF                                         ADD 09/26/03
     C                   SUB       1             I1
     C                   EXSR      FILL
     C*** BEG ADD *** 09/26/03 ****************************
     C                   ELSE
     C                   MOVE      *OFF          *IN16
     C                   ENDIF
     C*** END ADD *** 09/26/03 ****************************
     C                   READP     PRV01                                  60
     C  N60RVCO#         COMP      UPCO#                              6060
     C  N60RVFND         COMP      UPFND                              6060
     C  N60RVFYR         COMP      XXFYR                              6060
     C  N60RVCASE        COMP      X@CASE                             6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C*
     C                   Z-ADD     TR(1)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KEYD(1)
     C*
     C                   Z-ADD     TR(6)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KEYD(2)
     C*
     C                   Z-ADD     TM(1)         KEYT(1)
     C                   Z-ADD     TM(6)         KEYT(2)
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
     C     RVKEY         CHAIN     PRV01                              60
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
     C     RVKEY         READE     PRV01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      6                                    61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C*   ADDED TABLE MD FOR SCMODE
     C*** END   ADD *** 01/28/98 **************************************
     C*
**  TABLE MD
- Display-
-- Add --
- Change -
- Delete -
**
ROLL UP
ROLL DOWN
