     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0123R1 - GROUP CHECK BATCH TRANSACTION SEARCH
     H* ***************************************************************
     H*  DESCRIPTION:  THIS PGM WILL DISPLAY ALL PENDING TRANSACTIONS.*
     H*                IF THE DAILY BATCH HAS ALREADY PROCESSED A     *
     H*                TRANSACTION, IT WILL NOT BE DISPLAYED.         *
     H*  CALLED FROM:  W0123R                                         *
     H*  CALLS TO:     NONE                                           *
     H* ***************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* ***************************************************************
     FW0123F1   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWAGCL     UF   E           K DISK
      *

     D KEYT            S              6  0 DIM(2)
     D KEYC            S              7  0 DIM(2)
     D YY              S              9    DIM(2) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  TD                     1    104  0
     D                                     DIM(13)
     D  SCTD01               001    008  0
     D  SCTD02               009    016  0
     D  SCTD03               017    024  0
     D  SCTD04               025    032  0
     D  SCTD05               033    040  0
     D  SCTD06               041    048  0
     D  SCTD07               049    056  0
     D  SCTD08               057    064  0
     D  SCTD09               065    072  0
     D  SCTD10               073    080  0
     D  SCTD11               081    088  0
     D  SCTD12               089    096  0
     D  SCTD13               097    104  0
     D                 DS
     D  CK                     1     91  0
     D                                     DIM(13)
     D  SCCK01                 1      7  0
     D  SCCK02                 8     14  0
     D  SCCK03                15     21  0
     D  SCCK04                22     28  0
     D  SCCK05                29     35  0
     D  SCCK06                36     42  0
     D  SCCK07                43     49  0
     D  SCCK08                50     56  0
     D  SCCK09                57     63  0
     D  SCCK10                64     70  0
     D  SCCK11                71     77  0
     D  SCCK12                78     84  0
     D  SCCK13                85     91  0
     D                 DS
     D  PA                     1    117  0
     D                                     DIM(13)
     D  SCPA01                 1      9  0
     D  SCPA02                10     18  0
     D  SCPA03                19     27  0
     D  SCPA04                28     36  0
     D  SCPA05                37     45  0
     D  SCPA06                46     54  0
     D  SCPA07                55     63  0
     D  SCPA08                64     72  0
     D  SCPA09                73     81  0
     D  SCPA10                82     90  0
     D  SCPA11                91     99  0
     D  SCPA12               100    108  0
     D  SCPA13               109    117  0
     D                 DS
     D  MD                     1     39  0
     D                                     DIM(13)
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
     D                 DS
     D  ST                     1     52
     D                                     DIM(13)
     D  SCST01                 1      4
     D  SCST02                 5      8
     D  SCST03                 9     12
     D  SCST04                13     16
     D  SCST05                17     20
     D  SCST06                21     24
     D  SCST07                25     28
     D  SCST08                29     32
     D  SCST09                33     36
     D  SCST10                37     40
     D  SCST11                41     44
     D  SCST12                45     48
     D  SCST13                49     52
     D                 DS
     D  TC                     1     26
     D                                     DIM(13)
     D  SCTC01                 1      2
     D  SCTC02                 3      4
     D  SCTC03                 5      6
     D  SCTC04                 7      8
     D  SCTC05                 9     10
     D  SCTC06                11     12
     D  SCTC07                13     14
     D  SCTC08                15     16
     D  SCTC09                17     18
     D  SCTC10                19     20
     D  SCTC11                21     22
     D  SCTC12                23     24
     D  SCTC13                25     26
     D                 DS
     D  TY                     1     13
     D                                     DIM(13)
     D  SCTY01                 1      1
     D  SCTY02                 2      2
     D  SCTY03                 3      3
     D  SCTY04                 4      4
     D  SCTY05                 5      5
     D  SCTY06                 6      6
     D  SCTY07                 7      7
     D  SCTY08                 8      8
     D  SCTY09                 9      9
     D  SCTY10                10     10
     D  SCTY11                11     11
     D  SCTY12                12     12
     D  SCTY13                13     13
     D                 DS
     D  AM                     1    117  2
     D                                     DIM(13)
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
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

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
     C                   KFLD                    UPFND
     C*
     C*  GROUP CHECK BATCH TRANSACTION FILE
     C*
     C     GCKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXDATE
     C                   KFLD                    XXCHK#
     C*
     C     GCKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    SCTRDT
     C                   PARM                    SCCHK#            7 0
     C                   PARM                    QQRTCD
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
     C     *LIKE         DEFINE    GCCHK#        XXCHK#
     C     *LIKE         DEFINE    GCDT          XXDATE
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTRDT
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
     C*  LOAD TRANSACTION SCREEN
     C*
     C                   MOVE      '1'           XXDOWN
     C                   Z-ADD     *ZEROS        XXDATE
     C                   Z-ADD     *ZEROS        XXCHK#
     C     GCKEY         SETLL     PGC01
     C     GCKY1         READE(N)  PGC01                                  60
     C*****   N60          EXCPTPGC01E
     C                   EXSR      LOAD
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
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
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD            1
     C                   Z-ADD     *ZERO         SCTRDT
     C                   Z-ADD     *ZERO         SCCHK#
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  F9 = DELETE REQUEST
     C*
     C     *INKI         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       7             I1                3 0
     C*
     C*  PROCESS DELETE IF VALID REQUEST
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     13
     C                   Z-ADD     TD(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C                   Z-ADD     CK(I1)        XXCHK#
     C     GCKEY         CHAIN     PGC01                              60
     C                   DELETE    PGC01
     C                   EXSR      LOADB
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*   F12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   Z-ADD     *ZERO         SCTRDT
     C                   Z-ADD     *ZERO         SCCHK#
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
     C                   Z-ADD     KEYT(2)       XXDATE
     C                   Z-ADD     KEYC(2)       XXCHK#
     C     GCKEY         CHAIN(N)  PGC01                              60
     C******  N60          EXCPTPGC01E
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYT(1)       XXDATE
     C                   Z-ADD     KEYC(1)       XXCHK#
     C     GCKEY         CHAIN(N)  PGC01                              60
     C******  N60          EXCPTPGC01E
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCTRDT        IFNE      *ZERO
     C                   MOVE      *BLANK        KEYT
     C                   MOVE      *BLANK        KEYC
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C                   Z-ADD     *ZEROS        XXCHK#
     C     GCKEY         SETLL     PGC01
     C     GCKY1         READE(N)  PGC01                                  60
     C*****   N60          EXCPTPGC01E
     C                   EXSR      LOAD
     C                   Z-ADD     0             SCTRDT
     C                   GOTO      STRT
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C************         Z-ADDTD,I1     SCTRDT   IN PROGRESS - I1 =
     C************         Z-ADDCK,I1     SCCHK#     LAST FIELD ON SCREEN,
      *                                              NOT SELECTED RECORD.
     C                   MOVE      '1'           *INLR
     C*
     C* =================================================================
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       TD
     C                   MOVE      *BLANKS       CK
     C                   MOVE      *BLANKS       PA
     C                   MOVE      *BLANKS       MD
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       TC
     C                   MOVE      *BLANKS       TY
     C                   MOVE      *BLANKS       AM
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C* =================================================================
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   Z-ADD     GCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         TD(I1)
     C                   MOVE      GCCHK#        CK(I1)
     C                   MOVE      GCFEIN        PA(I1)
     C                   MOVE      GCMOD         MD(I1)
     C                   MOVE      GCCHKS        ST(I1)
     C                   MOVE      GCTRCD        TC(I1)
     C                   MOVE      GCTRTY        TY(I1)
     C                   MOVE      GCAMT         AM(I1)
     C                   ENDSR
     C*
     C* =================================================================
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
     C     GCKY1         READE(N)  PGC01                                  60
     C******  N60          EXCPTPGC01E
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      13                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     TD(1)         KEYT(1)
     C                   Z-ADD     TD(13)        KEYT(2)
     C                   Z-ADD     CK(1)         KEYC(1)
     C                   Z-ADD     CK(13)        KEYC(2)
     C                   ENDSR
     C*
     C* =================================================================
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     13            I1
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
     C                   READP(N)  PGC01                                  60
     C*****   N60          EXCPTPGC01E
     C  N60UPCO#         COMP      GCCO#                              6060
     C  N60UPFND         COMP      GCFND                              6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     TD(1)         KEYT(1)
     C                   Z-ADD     TD(13)        KEYT(2)
     C                   Z-ADD     CK(1)         KEYC(1)
     C                   Z-ADD     CK(13)        KEYC(2)
     C                   ENDSR
     C*
     C* =================================================================
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     *ZERO         XXDATE
     C                   Z-ADD     *ZERO         XXCHK#
     C     GCKEY         SETLL     PGC01
     C     GCKY1         READE(N)  PGC01                                  60
     C******  N60          EXCPTPGC01E
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
     C     GCKY1         READE(N)  PGC01                                  60
     C******  N60          EXCPTPGC01E
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      13                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
     C*
     C*
      **** OPGC01   E                PGC01E
**


