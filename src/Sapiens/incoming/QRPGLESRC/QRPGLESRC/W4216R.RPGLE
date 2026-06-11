     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W4216R - DISPLAY CLEARED TRANSACTIONS
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/13/01 C:61  PROJECT WC1020                                 *
     H*              - Return from SCR02 if called with account #.    *
     H*                                                               *
     H* 01/24/01 C:61  SUPPORT WA099  25700                           *
     H*              - Return if F3 or F12 used to exit program.      *
     H*              - F12=Cancel to SCR02. -> exit program.          *
     H*                                                               *
     H* 12/17/99 C:118 PROJECT: WC577                                 *
     H*                Remove F12=Cancel and chg pgm ref to Q1PGM.    *
     H*                                                               *
     H* 09/16/99 C:61  PROJECT WC577                                  *
     H*              - Change F10=Clear trans status --> F20.         *
     H*              - Correct default Check# after F20 select.       *
     H*                                                               *
     H*  09/04/97 C:30 Project:  WC659                                *
     H*           Version 3 changes.  Remove WMBAP.  Use WMBIP.       *
     H*                                                               *
     H*  PLEASE ADD TO TOP OF REVISIONS                               *
     H*****************************************************************
      *
      *
     FW4216F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDCRL1    UF   E           K DISK
      *** BEGIN CHG *** 09/04/97 ********************************
     F****WMBAL   IF  E           K        DISK
      *
      *  Bank Information file
     FWMBIL4    IF   E           K DISK
      *** END   CHG *** 09/04/97 ********************************

     D DA              S              7  0 DIM(11)
     D TM              S              6  0 DIM(11)
     D OCK             S              7  0 DIM(11)
     D ODS             S             30    DIM(11)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEYC            S              7  0 DIM(2)
     D KEYD            S              7  0 DIM(2)
     D KEYT            S              6  0 DIM(2)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  AT                     1     11
     D                                     DIM(11)
     D  SCAT01                 1      1
     D  SCAT02                 2      2
     D  SCAT03                 3      3
     D  SCAT04                 4      4
     D  SCAT05                 5      5
     D  SCAT06                 6      6
     D  SCAT07                 7      7
     D  SCAT08                 8      8
     D  SCAT09                 9      9
     D  SCAT10                10     10
     D  SCAT11                11     11
     D                 DS
     D  DT                     1    088  0
     D                                     DIM(11)
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
     D                 DS
     D  CK                     1     77  0
     D                                     DIM(11)
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
     D                 DS
     D  CD                     1    088  0
     D                                     DIM(11)
     D  SCCD01               001    008  0
     D  SCCD02               009    016  0
     D  SCCD03               017    024  0
     D  SCCD04               025    032  0
     D  SCCD05               033    040  0
     D  SCCD06               041    048  0
     D  SCCD07               049    056  0
     D  SCCD08               057    064  0
     D  SCCD09               065    072  0
     D  SCCD10               073    080  0
     D  SCCD11               081    088  0
     D                 DS
     D  TP                     1     11
     D                                     DIM(11)
     D  SCTP01                 1      1
     D  SCTP02                 2      2
     D  SCTP03                 3      3
     D  SCTP04                 4      4
     D  SCTP05                 5      5
     D  SCTP06                 6      6
     D  SCTP07                 7      7
     D  SCTP08                 8      8
     D  SCTP09                 9      9
     D  SCTP10                10     10
     D  SCTP11                11     11
     D                 DS
     D  AM                     1     99  2
     D                                     DIM(11)
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
     D                 DS
     D  DS                     1    330
     D                                     DIM(11)
     D  SCDS01                 1     30
     D  SCDS02                31     60
     D  SCDS03                61     90
     D  SCDS04                91    120
     D  SCDS05               121    150
     D  SCDS06               151    180
     D  SCDS07               181    210
     D  SCDS08               211    240
     D  SCDS09               241    270
     D  SCDS10               271    300
     D  SCDS11               301    330
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  CHECK RECONCILIATION DETAIL FILE
     C*
     C     CRKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCACCT
     C*
     C     CRKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCACCT
     C                   KFLD                    XXCHK#            7 0
     C                   KFLD                    XXDT              7 0
     C                   KFLD                    XXTIME            6 0
      *
      *** BEGIN CHG *** 09/04/97 ***********************************************
     C*
     C*  BANK ACCOUNT FILE
     C*
     C     BAKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCACCT
      *
      *  Bank Information file
      *
     C     BIKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXAPPL            2
     C                   KFLD                    SCACCT
     C                   KFLD                    UPFND
      *
      *** END   CHG *** 09/04/97 ***********************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQACCT           20
     C                   PARM                    QQRTCD            1
      ***** BEG ADD ******* 01/24/01 **********************************
     C                   PARM      *INKC         QQKC              1
     C                   PARM      *INKL         QQKL              1
      ***** END ADD ******* 01/24/01 **********************************
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
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
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
     C                   MOVE      *BLANKS       SCACCT
     C                   MOVE      QQACCT        SCACCT
      ***** BEG ADD ******* 04/13/01 **********************************
     C                   MOVEL(P)  QQACCT        XXACCT           20
      ***** END ADD ******* 04/13/01 **********************************
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C     QQRTCD        COMP      '2'                                    78
     C     QQRTCD        COMP      '3'                                    78
     C     QQRTCD        COMP      '3'                                    56
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     SCACCT        IFNE      *BLANKS
     C                   GOTO      SCR01T
     C                   END
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
     C     *IN78         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   END
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
      ***** BEG ADD ******* 01/24/01 **********************************
     C                   GOTO      TERM
      ***** END ADD ******* 01/24/01 **********************************
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* F10=SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
      *** BEGIN CHG *** 09/04/97 ***********************************************
     C*****                MOVE SCFND     QQFND   3
     C*****                CALL 'W4205R'
     C*****                PARM           QQPRDS
     C*****                PARM           QQCALL
     C*****                PARM           QQFND
     C*****                PARM           SCACCT
     C*****                PARM           QQRTC   1
     C*****      QQRTC     CABEQ'A'       TERM
     C*
     C                   MOVE      UPCO#         ##CO#             3
     C                   MOVE      SCFND         ##FND             3
      *
     C                   CALL      'W2420R'                                     Bank Info search
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM      *BLANKS       ##ACCT           20
     C                   PARM      *ZERO         ##EFDT            8
     C                   PARM      'WC'          ##APPL            2
     C                   PARM      *BLANKS       ##CKTP            2
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         IFEQ      *OFF
     C     ##ACCT        ANDNE     *BLANK
     C                   MOVE      ##ACCT        SCACCT
     C                   ENDIF                                                  *INKL IFEQ *OFF
      *** END   CHG *** 09/04/97 ***********************************************
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   COMMAND 20 - CLEAR TRANSACTION STATUS-SCR02
     C*
      ***** BEG CHG ******* 09/16/99 **********************************
     C*********  *INKJ     IFEQ '1'
     C     *INKU         IFEQ      '1'
      ***** END CHG ******* 09/16/99 **********************************
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C*  CHECK CURSOR POSITION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
      ***** BEG CHG ******* 09/16/99 **********************************
     C*********  LIN       SUB  9         I1      20
     C     LIN           SUB       10            I1                2 0
      ***** END CHG ******* 09/16/99 **********************************
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCACCT        QQACCT           20
     C     I1            IFLT      1
     C     I1            ORGT      11
     C     TP(I1)        ORNE      'C'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      TP(I1)        QQRTP             1
     C                   MOVE      CK(I1)        QQCK#             7
     C*
     C                   MOVE      CD(I1)        QQCKDT            8
      *
     C                   CALL      'W4217R'
     C                   PARM                    QQCO#
     C                   PARM                    QQACCT
     C                   PARM                    QQRTP
     C                   PARM                    QQCK#
     C                   PARM                    QQCKDT
     C                   PARM                    QQCALL
      *** BEGIN CHG *** 09/04/97 ***********************************************
     C*********************PARM***********QQRTCD
     C                   PARM                    QQRTC             1
      *** END   CHG *** 09/04/97 ***********************************************
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END
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
     C                   MOVE      *BLANK        SCCONM
     C                   MOVEL     *ZERO         SCFND
     C                   MOVEL     *BLANK        SCNAME
     C*
     C                   ELSE
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     UPFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE ACCOUNT NUMBER
     C*
      *** BEGIN CHG *** 09/04/97 *************************************
     C***********BAKEY     CHAINPBA01                20
      *
     C                   MOVE      'WC'          XXAPPL
      *
     C     BIKEY         CHAIN     PBI01                              20
      *
     C     *IN20         DOWEQ     *OFF
     C     BISTAT        ANDEQ     'I'
     C     BIKEY         READE     PBI01                                  20
     C                   ENDDO
      *
      *** END   CHG *** 09/04/97 *************************************
      *
     C     *IN20         IFEQ      '1'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   Z-ADD     0             SCCHK#
     C                   MOVE      '1'           *IN57
     C                   GOTO      REPOS
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C     SCR02T        TAG
     C*
     C*  INVALID ROLL REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   EXSR      RITE
     C     XXUP          IFNE      *BLANK
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     *IN19         IFEQ      '1'
     C                   EXSR      RITE
     C     XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   Z-ADD     KEYC(2)       XXCHK#
     C                   Z-ADD     KEYD(2)       XXDT
     C                   Z-ADD     KEYT(2)       XXTIME
     C     CRKY1         CHAIN     PCR01                              60
     C     CRKEY         READE     PCR01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYC(1)       XXCHK#
     C                   Z-ADD     KEYD(1)       XXDT
     C                   Z-ADD     KEYT(1)       XXTIME
     C     CRKY1         CHAIN     PCR01                              60
     C                   READP     PCR01                                  60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCCHK#        IFNE      *ZERO
     C                   EXSR      RITE
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEYC
     C                   MOVE      *BLANK        KEYD
     C                   MOVE      *BLANK        KEYT
     C                   MOVE      *BLANK        XXUP
     C   57              MOVE      '1'           XXDOWN
     C  N57              MOVE      *BLANK        XXDOWN
     C                   MOVE      '0'           *IN57
     C                   Z-ADD     SCCHK#        XXCHK#
     C                   MOVE      *BLANK        SCCHK#
     C                   Z-ADD     0             XXDT
     C                   Z-ADD     0             XXTIME
     C     CRKY1         SETLL     PCR01
     C     CRKEY         READE     PCR01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
      *
      ***** BEG ADD ******* 04/13/01 **********************************
     C     XXACCT        IFEQ      *BLANKS
      ***** END ADD ******* 04/13/01 **********************************
     C*
     C*  IF ENTER WAS PRESSED EXECUTE THE RITE SUBROUTINE
     C*
     C                   EXSR      RITE
     C*
     C*
     C*  RETURN TO SCREEN 1
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
      ***** BEG ADD ******* 04/13/01 **********************************
     C                   ENDIF                                                  XXACCT EQ BLANK
      ***** END ADD ******* 04/13/01 **********************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*  CLEAR THE SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C*             CLEAR SCREEN ARRAYS
     C                   MOVE      *BLANKS       AT
     C                   MOVE      *BLANKS       DT
     C                   MOVE      *BLANKS       CK
     C                   MOVE      *BLANKS       CD
     C                   MOVE      *BLANKS       TP
     C                   MOVE      *BLANKS       AM
     C                   MOVE      *BLANKS       DS
     C                   MOVE      *BLANKS       DA
     C                   MOVE      *BLANKS       TM
     C*             CLEAR HOLD ARRAYS
     C                   MOVE      *BLANKS       OCK
     C                   MOVE      *BLANKS       ODS
     C                   Z-ADD     *ZERO         I1                             ARRAY INDEX
     C                   MOVEA     '0000000'     *IN(34)
     C                   MOVEA     '0000'        *IN(41)
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C     I1            ADD       33            X                 2 0
     C                   MOVE      '1'           *IN(X)
     C                   MOVE      CRACTP        AT(I1)
     C*
     C                   Z-ADD     CRACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C*
     C                   Z-ADD     CRCHK#        CK(I1)
     C*
     C                   Z-ADD     CRCKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CD(I1)
     C*
     C                   MOVE      CRRTP         TP(I1)
     C                   Z-ADD     CRCKAM        AM(I1)
     C                   MOVEL     CRDESC        DS(I1)
     C                   Z-ADD     CRDT          DA(I1)
     C                   Z-ADD     CRTIME        TM(I1)
     C                   EXCEPT    PCR01E
     C*               FILL HOLD ARRAYS
     C                   MOVE      CK(I1)        OCK(I1)
     C                   MOVE      DS(I1)        ODS(I1)
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
     C     CRSTAT        IFNE      *BLANKS
     C                   ADD       1             I1
     C                   EXSR      FILL
     C                   END
     C     CRKEY         READE     PCR01                                  60
     C   60              DO
     C     I1            IFLT      11
     C                   MOVE      '1'           XXUP
     C                   END
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      11                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C  N60              EXCEPT    PCR01E
     C     I1            IFNE      0
     C                   Z-ADD     CK(1)         KEYC(1)
     C                   Z-ADD     CK(11)        KEYC(2)
     C                   Z-ADD     DA(1)         KEYD(1)
     C                   Z-ADD     DA(11)        KEYD(2)
     C                   Z-ADD     TM(1)         KEYT(1)
     C                   Z-ADD     TM(11)        KEYT(2)
     C                   END
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     12            I1
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
     C     CRSTAT        IFNE      *BLANK
     C                   SUB       1             I1
     C                   EXSR      FILL
     C                   END
     C                   READP     PCR01                                  60
     C  N60              DO
     C     CRCO#         IFNE      UPCO#
     C     CRACCT        ORNE      SCACCT
     C                   MOVE      '1'           *IN60
     C                   END
     C                   END
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C  N60              EXCEPT    PCR01E
     C                   MOVEL     CK(1)         KEYC(1)
     C                   MOVEL     CK(11)        KEYC(2)
     C                   Z-ADD     DA(1)         KEYD(1)
     C                   Z-ADD     DA(11)        KEYD(2)
     C                   Z-ADD     TM(1)         KEYT(1)
     C                   Z-ADD     TM(11)        KEYT(2)
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
     C                   Z-ADD     *ZERO         XXCHK#
     C                   Z-ADD     0             XXDT
     C                   Z-ADD     0             XXTIME
     C     CRKY1         SETLL     PCR01
     C     CRKEY         READE     PCR01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C     CRSTAT        IFNE      *BLANKS
     C                   ADD       1             I1
     C                   EXSR      FILL
     C                   END
     C     CRKEY         READE     PCR01                                  60
     C   60              DO
     C     I1            IFLT      11
     C                   MOVE      '1'           XXUP
     C                   END
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      11                                   61
     C   61              GOTO      OVERB
     C     DONEB         TAG
     C                   ENDSR
     C**************************************************************
     C*     RITE SUBROUTINE - UPDATE RECORDS                       *
     C**************************************************************
     C     RITE          BEGSR
     C*
     C                   DO        11            X                 2 0
     C     X             ADD       44            X2                2 0          ERROR
     C*
     C*   COMPARE HOLD FIELDS FROM ORIGINAL SCREEN TO THE CURRENT
     C*   FIELDS. IF THE FIELDS CHANGED, EDIT & UPDATE OR WRITE.
     C*
     C     CK(X)         IFNE      OCK(X)
     C     DS(X)         ORNE      ODS(X)
     C*
     C*   CHECK NUMBER CANNOT BE LESS THAN 0
     C*
     C     CK(X)         COMP      0                                    23
     C     *IN23         IFEQ      '1'
     C                   MOVE      '1'           *IN(X2)
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK# MUST BE ENTERED IF TYPE IS C, OR N.
     C*
     C     TP(X)         IFEQ      'C'
     C     TP(X)         OREQ      'N'
     C     CK(X)         IFEQ      0
     C                   MOVE      '1'           *IN24
     C                   MOVE      '1'           *IN(X2)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  PAYEE/DESCRIPTION CANNOT BE BLANK
     C*
     C     DS(X)         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN28
     C                   MOVE      '1'           *IN(X2)
     C                   GOTO      STRT
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C                   Z-ADD     CK(X)         XXCHK#
     C                   Z-ADD     DA(X)         XXDT
     C                   Z-ADD     TM(X)         XXTIME
     C*
     C*    CHAIN WITH THE OLD CHECK# & UPDATE
     C*
     C     CRKY1         CHAIN     PCR01                              60
     C*
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     CK(X)         CRCHK#
     C                   MOVEL     DS(X)         CRDESC
     C                   MOVE      Q1USER        CRUSER
     C                   MOVE      Q1PGM         CRPGM
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        CRTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRDT
     C*
     C                   UPDATE    PCR01
     C                   END
     C*
     C*  MOVE NEW FIELDS TO OLD FIELDS
     C*
     C                   MOVE      CK(X)         OCK(X)
     C                   MOVE      DS(X)         ODS(X)
     C                   Z-ADD     CRDT          DA(X)
     C                   Z-ADD     CRTIME        TM(X)
     C*
     C                   END                                                    OLD = NEW
     C                   END                                                    DO 11 X
     C*
     C     RITEN         ENDSR
     OPCR01     E            PCR01E
**
ROLL UP
ROLL DOWN
