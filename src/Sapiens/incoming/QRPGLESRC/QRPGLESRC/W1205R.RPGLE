     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W1205R - BATCH CHECK#/AMOUNT SEARCH
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*       THIS PROGRAM ALLOWS THE USER TO SEARCH AND SELECT ALL   *
     H*       EXISTING CHECKS AND CHECK AMOUNTS BY A BATCH NUMBER.    *
     H*       THE INFORMATION TO BE DISPLAYED INCLUDE: BATCH NUMBER   *
     H*       AND AMOUNT, CHECK NUMBER, TOTAL CHECK AMOUNT, AMOUNT    *
     H*       ENTERED, AND DIFFERENCE AMOUNT.                         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*       W1200R                                                  *
     H*  CALLS TO:                                                    *
     H*       NONE                                                    *
     H*  PROGRAMMER: MARIE NGUYEN                                     *
     H*  CREATED:    07/15/91                                         *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*       18      ROLL UP REQUEST                                 *
     H*       19      ROLL DOWN REQUEST                               *
     H*       20      INVALID ROLL REQUEST                            *
     H*       21      START AT BEGIN OF FILE                          *
     H*      N22      LOAD FORWARD                                    *
     H*       22      LOAD BACKWARD                                   *
     H*       60      GENERAL CHAINS & READS                          *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/21/92 KWK  PROJECT WC177                                   *
     H*               CORRECTED CODE SO THAT CHECKS POSTED AS SEVERAL *
     H*               DIFFERENT ENTRIES WILL ALWAYS DISPLAY PROPERLY  *
     H*               WHEN A ROLL FUNCTION IS USED.                   *
     H*****************************************************************
     FW1205F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDEAL     IF   E           K DISK

     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEY#            S              7  0 DIM(2)                               CHECK# KEY
     D KEYA            S              9  2 DIM(2)                               CHK AMT KEY
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  CK                     1     98  0
     D                                     DIM(14)
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
     D  SCCK14                92     98  0
     D                 DS
     D  CA                     1    126  2
     D                                     DIM(14)
     D  SCCA01                 1      9  2
     D  SCCA02                10     18  2
     D  SCCA03                19     27  2
     D  SCCA04                28     36  2
     D  SCCA05                37     45  2
     D  SCCA06                46     54  2
     D  SCCA07                55     63  2
     D  SCCA08                64     72  2
     D  SCCA09                73     81  2
     D  SCCA10                82     90  2
     D  SCCA11                91     99  2
     D  SCCA12               100    108  2
     D  SCCA13               109    117  2
     D  SCCA14               118    126  2
     D                 DS
     D  TE                     1    126  2
     D                                     DIM(14)
     D  SCTE01                 1      9  2
     D  SCTE02                10     18  2
     D  SCTE03                19     27  2
     D  SCTE04                28     36  2
     D  SCTE05                37     45  2
     D  SCTE06                46     54  2
     D  SCTE07                55     63  2
     D  SCTE08                64     72  2
     D  SCTE09                73     81  2
     D  SCTE10                82     90  2
     D  SCTE11                91     99  2
     D  SCTE12               100    108  2
     D  SCTE13               109    117  2
     D  SCTE14               118    126  2
     D                 DS
     D  DF                     1    126  2
     D                                     DIM(14)
     D  SCDF01                 1      9  2
     D  SCDF02                10     18  2
     D  SCDF03                19     27  2
     D  SCDF04                28     36  2
     D  SCDF05                37     45  2
     D  SCDF06                46     54  2
     D  SCDF07                55     63  2
     D  SCDF08                64     72  2
     D  SCDF09                73     81  2
     D  SCDF10                82     90  2
     D  SCDF11                91     99  2
     D  SCDF12               100    108  2
     D  SCDF13               109    117  2
     D  SCDF14               118    126  2
     D                 DS
     D  FL                     1     14
     D                                     DIM(14)
     D  SCFL01                 1      1
     D  SCFL02                 2      2
     D  SCFL03                 3      3
     D  SCFL04                 4      4
     D  SCFL05                 5      5
     D  SCFL06                 6      6
     D  SCFL07                 7      7
     D  SCFL08                 8      8
     D  SCFL09                 9      9
     D  SCFL10                10     10
     D  SCFL11                11     11
     D  SCFL12                12     12
     D  SCFL13                13     13
     D  SCFL14                14     14
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE EMPLOYER A/R BATCH FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSTAT
     C                   KFLD                    XXBAT#
     C                   KFLD                    XXCHK#
     C                   KFLD                    XXCKAM
     C*
     C     EAKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSTAT
     C                   KFLD                    XXBAT#
     C*
     C     EAKEY3        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSTAT
     C                   KFLD                    XXBAT#
     C                   KFLD                    XXCHK#
     C*
     C     *LIKE         DEFINE    EABAT#        XXBAT#
     C     *LIKE         DEFINE    EASTAT        XXSTAT
     C     *LIKE         DEFINE    EAAMT         XXENTR
     C     *LIKE         DEFINE    EACHK#        XXCHK#
     C     *LIKE         DEFINE    EACHK#        SVCHK#
     C     *LIKE         DEFINE    EACKAM        XXCKAM
     C     *LIKE         DEFINE    EACKAM        SVCKAM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQSTAT            1            STATUS
     C                   PARM                    QQBAT#            9            BATCH #
     C                   PARM                    QQCHK#            7            CHECK #
     C                   PARM                    QQCKAM            9            CHECK AMOUNT
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
     C                   MOVE      QQSTAT        XXSTAT
     C                   MOVE      QQBAT#        XXBAT#
     C                   Z-ADD     XXBAT#        SCBAT#
     C                   MOVE      QQCHK#        XXCHK#
     C                   MOVE      QQCKAM        XXCKAM
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
     C                   END
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C     SCBAT#        COMP      *ZEROS                                 21
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
     C                   MOVE      *BLANKS       QQCHK#
     C                   MOVE      *BLANKS       QQCKAM
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *BLANKS       QQCHK#
     C                   MOVE      *BLANKS       QQCKAM
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
     C                   Z-ADD     KEY#(2)       XXCHK#
     C                   Z-ADD     KEYA(2)       XXCKAM
     C     EAKEY3        SETGT     PEA01
     C     EAKEY2        READE     PEA01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEY#(1)       XXCHK#
     C                   Z-ADD     KEYA(1)       XXCKAM
     C     EAKEY         CHAIN     PEA01                              60
     C                   READP     PEA01                                  60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCCHK#        IFNE      *ZEROS
     C                   Z-ADD     *ZEROS        XXCKAM
     C     REPOS         TAG
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   MOVE      '0'           *IN21
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *ZERO         KEY#
     C                   MOVE      *ZERO         KEYA
     C                   Z-ADD     SCCHK#        XXCHK#
     C                   Z-ADD     *ZEROS        SCCHK#
     C     EAKEY         SETLL     PEA01
     C     EAKEY2        READE     PEA01                                  60
     C  N60              Z-ADD     EABATA        SCBATA
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
     C                   MOVE      CK(I1)        QQCHK#
     C                   MOVE      CA(I1)        QQCKAM
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANK        QQCHK#
     C                   MOVE      *BLANK        QQCKAM
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *ZEROS        CK
     C                   MOVE      *ZEROS        CA
     C                   MOVE      *ZEROS        TE
     C                   MOVE      *ZEROS        DF
     C                   MOVE      *BLANKS       FL
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*****************************************************************
     C*
     C*  FILL SCREEN SUBROUTINE
     C*
     C     FILL          BEGSR
     C                   Z-ADD     EACHK#        CK(I1)                         BATCH #
     C                   Z-ADD     EACKAM        CA(I1)                         BATCH AMOUNT
     C                   Z-ADD     *ZERO         XXENTR
     C                   Z-ADD     EACHK#        SVCHK#
     C                   Z-ADD     EACKAM        SVCKAM
     C*
     C*  FOR EACH UNIQUE CO#, BATCH#, CHECK#, CHECK AMT, AMOUNT ENTERED
     C*  IS THE TOTAL OF ALL POSTED AMOUNTS AND DIFFERENCE AMOUNT IS
     C*  TOTAL CHECK AMOUNT LESS AMOUNT ENTERED.
     C*
     C     *IN60         DOWEQ     '0'
     C     EACHK#        ANDEQ     SVCHK#
     C     EACKAM        ANDEQ     SVCKAM
     C                   ADD       EAAMT         XXENTR
     C  N22EAKEY2        READE     PEA01                                  60     LOAD FORWARD
     C   22              READP     PEA01                                  60     LOAD BACKWARD
     C   22
     CANN60EACO#         COMP      UPCO#                              6060       |
     C   22
     CANN60EASTAT        COMP      XXSTAT                             6060       |
     C   22
     CANN60EABAT#        COMP      XXBAT#                             6060       |
     C                   END
     C                   Z-ADD     XXENTR        TE(I1)                         AMT ENTERED
     C     CA(I1)        SUB       XXENTR        DF(I1)                         DIFFERENCE
     C*
     C                   ENDSR
     C*****************************************************************
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
     C                   MOVE      '0'           *IN22                          N22=LOAD FORWARD
     C                   EXSR      FILL
     C*
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     CK(1)         KEY#(1)
     C                   Z-ADD     CK(14)        KEY#(2)
     C                   Z-ADD     CA(1)         KEYA(1)
     C                   Z-ADD     CA(14)        KEYA(2)
     C                   ENDSR
     C*****************************************************************
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
     C                   MOVE      '1'           *IN22                          22=LOAD BACKWARD
     C                   EXSR      FILL
     C*
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     CK(1)         KEY#(1)
     C                   Z-ADD     CK(14)        KEY#(2)
     C                   Z-ADD     CA(1)         KEYA(1)
     C                   Z-ADD     CA(14)        KEYA(2)
     C                   ENDSR
     C*****************************************************************
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
     C                   MOVE      *ZEROS        XXCHK#
     C     EAKEY         SETLL     PEA01
     C     EAKEY2        READE     PEA01                                  60
     C                   EXSR      LOAD
     C     DONEB         ENDSR
**
ROLL UP
ROLL DOWN
