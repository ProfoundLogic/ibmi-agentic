     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N0720R - CASE/INVOICE SEARCH - SCREEN PROGRAM
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/07/99  C:94  PROJECT: MC152                               *
     H*                  CHANGE LENGTH OF INV# FROM 5 TO 9            *
     H*                                                               *
     H*****************************************************************
     FN0720F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FNMARL7    IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMSML     IF   E           K DISK

     D FL              S              1    DIM(14)                              POSITION FLAG
     D*** BEGIN CHG *** 09/29/99 **************************************
     D**********          YR         14  2 0             YEAR
     D*** END   CHG *** 09/29/99 **************************************
     D*** BEG CHG *** 01/07/99 ***************************************
     D**********          IN         14  5 0             INVOICE #
     D*** END CHG *** 01/07/99 ***************************************
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEY1            S              3  0 DIM(2)                               GROUP KEY
     D*** BEGIN CHG *** 09/29/99 **************************************
     D**********          KEY2        2  2 0             YEAR KEY
     D KEY2            S              3  0 DIM(2)                               YEAR KEY
     D*** END   CHG *** 09/29/99 **************************************
     D KEY3            S              7  0 DIM(2)                               CASE KEY
     D*** BEG CHG *** XX/XX/XX ***************************************
     D KEY4            S              9  0 DIM(2)                               INV# KEY
     D**********          KEY4        2  5 0             INV# KEY
     D*** END CHG *** XX/XX/XX ***************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  GR                     1     42  0
     D                                     DIM(14)                              GROUP
     D  SCGR01                 1      3  0
     D  SCGR02                 4      6  0
     D  SCGR03                 7      9  0
     D  SCGR04                10     12  0
     D  SCGR05                13     15  0
     D  SCGR06                16     18  0
     D  SCGR07                19     21  0
     D  SCGR08                22     24  0
     D  SCGR09                25     27  0
     D  SCGR10                28     30  0
     D  SCGR11                31     33  0
     D  SCGR12                34     36  0
     D  SCGR13                37     39  0
     D  SCGR14                40     42  0
     D*** BEGIN CHG *** 09/29/99 **************************************
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
     D**********
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
     D*** END   CHG *** 09/29/99 **************************************
     D                 DS
     D  CS                     1     98  0
     D                                     DIM(14)                              CASE
     D  SCCS01                 1      7  0
     D  SCCS02                 8     14  0
     D  SCCS03                15     21  0
     D  SCCS04                22     28  0
     D  SCCS05                29     35  0
     D  SCCS06                36     42  0
     D  SCCS07                43     49  0
     D  SCCS08                50     56  0
     D  SCCS09                57     63  0
     D  SCCS10                64     70  0
     D  SCCS11                71     77  0
     D  SCCS12                78     84  0
     D  SCCS13                85     91  0
     D  SCCS14                92     98  0
     D*** BEG CHG *** 01/07/99 ***************************************
     D                 DS
     D  IN                     1    126  0
     D                                     DIM(14)                              INVOICE #
     D  SCIN01                 1      9  0
     D  SCIN02                10     18  0
     D  SCIN03                19     27  0
     D  SCIN04                28     36  0
     D  SCIN05                37     45  0
     D  SCIN06                46     54  0
     D  SCIN07                55     63  0
     D  SCIN08                64     72  0
     D  SCIN09                73     81  0
     D  SCIN10                82     90  0
     D  SCIN11                91     99  0
     D  SCIN12               100    108  0
     D  SCIN13               109    117  0
     D  SCIN14               118    126  0
     D**********  DS
     D**********                              1  70 IN
     D**********                              1   50SCIN01
     D**********                              6  100SCIN02
     D**********                             11  150SCIN03
     D**********                             16  200SCIN04
     D**********                             21  250SCIN05
     D**********                             26  300SCIN06
     D**********                             31  350SCIN07
     D**********                             36  400SCIN08
     D**********                             41  450SCIN09
     D**********                             46  500SCIN10
     D**********                             51  550SCIN11
     D**********                             56  600SCIN12
     D**********                             61  650SCIN13
     D**********                             66  700SCIN14
     D*** END CHG *** 01/07/99 ***************************************
     D                 DS
     D  TD                     1    112  0
     D                                     DIM(14)                              TRANS DATE
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
     D  SCTD14               105    112  0
     D                 DS
     D  PD                     1    112  0
     D                                     DIM(14)                              POST DATE
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
     D  IA                     1    126  2
     D                                     DIM(14)                              INVOICE AMT
     D  SCIA01                 1      9  2
     D  SCIA02                10     18  2
     D  SCIA03                19     27  2
     D  SCIA04                28     36  2
     D  SCIA05                37     45  2
     D  SCIA06                46     54  2
     D  SCIA07                55     63  2
     D  SCIA08                64     72  2
     D  SCIA09                73     81  2
     D  SCIA10                82     90  2
     D  SCIA11                91     99  2
     D  SCIA12               100    108  2
     D  SCIA13               109    117  2
     D  SCIA14               118    126  2
     D                 DS
     D  OA                     1    126  2
     D                                     DIM(14)                              OPEN AMT/BAL DUE
     D  SCOA01                 1      9  2
     D  SCOA02                10     18  2
     D  SCOA03                19     27  2
     D  SCOA04                28     36  2
     D  SCOA05                37     45  2
     D  SCOA06                46     54  2
     D  SCOA07                55     63  2
     D  SCOA08                64     72  2
     D  SCOA09                73     81  2
     D  SCOA10                82     90  2
     D  SCOA11                91     99  2
     D  SCOA12               100    108  2
     D  SCOA13               109    117  2
     D  SCOA14               118    126  2
     D                 DS
     D  SY                     1     14
     D                                     DIM(14)                              SYS CODE
     D  SCSY01                 1      1
     D  SCSY02                 2      2
     D  SCSY03                 3      3
     D  SCSY04                 4      4
     D  SCSY05                 5      5
     D  SCSY06                 6      6
     D  SCSY07                 7      7
     D  SCSY08                 8      8
     D  SCSY09                 9      9
     D  SCSY10                10     10
     D  SCSY11                11     11
     D  SCSY12                12     12
     D  SCSY13                13     13
     D  SCSY14                14     14
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE A/R FILE
     C*
     C     ARKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXINV#
     C*
     C*   KEY LIST FOR THE A/R FILE
     C*
     C     ARKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C*
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    ARTRDT        XHTRDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C*** BEG CHG *** XX/XX/XX ***************************************
     C                   PARM                    QQINV#            9
     C**********           PARM           QQINV#  5
     C*** END CHG *** XX/XX/XX ***************************************
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
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      '-'           XXHYP1            1
     C                   MOVE      '-'           XXHYP2            1
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE            7 0
     C*** BEG CHG *** 01/07/99 ***************************************
     C                   MOVE      QQINV#        XXINV#            9 0
     C**********           MOVE QQINV#    XXINV#  50
     C*** END CHG *** 01/07/99 ***************************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
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
     C* GET SUBSCRIBER NAME
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         SCSUB
     C                   MOVE      *BLANK        SCSBNM
     C                   ELSE
     C                   MOVEL     UPSUB#        SCSUB
     C                   MOVEL     SMNAME        SCSBNM
     C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C                   MOVE      QQFND         SCFND
     C                   MOVE      QQFYR         SCFYR
     C                   MOVE      QQCASE        SCCASE
     C                   MOVE      QQINV#        SCINV#
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
     C*   F3 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVEL     *BLANK        QQINV#
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   F12 = PREVIOUS
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVEL     *BLANK        QQINV#
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
     C                   MOVE      KEY1(2)       XXFND
     C                   MOVE      KEY2(2)       XXFYR
     C                   MOVE      KEY3(2)       XXCASE
     C                   MOVE      KEY4(2)       XXINV#
     C     ARKEY         CHAIN     PAR01                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      KEY1(1)       XXFND
     C                   MOVE      KEY2(1)       XXFYR
     C                   MOVE      KEY3(1)       XXCASE
     C                   MOVE      KEY4(1)       XXINV#
     C     ARKEY         CHAIN     PAR01                              60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCFND         IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *ZERO         KEY1
     C                   MOVE      *ZERO         KEY2
     C                   MOVE      *ZERO         KEY3
     C                   MOVE      *ZERO         KEY4
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVE      SCFND         XXFND
     C                   MOVE      SCFYR         XXFYR
     C                   MOVE      SCCASE        XXCASE
     C                   MOVE      SCINV#        XXINV#
     C                   MOVE      *ZEROS        SCFND
     C                   MOVE      *ZEROS        SCFYR
     C                   MOVE      *ZEROS        SCCASE
     C                   MOVE      *ZEROS        SCINV#
     C     ARKEY         SETLL     PAR01
     C     ARKEY2        READE     PAR01                                  60
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
     C                   MOVE      GR(I1)        QQFND
     C                   MOVE      YR(I1)        QQFYR
     C                   MOVE      CS(I1)        QQCASE
     C                   MOVE      IN(I1)        QQINV#
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   MOVEL     *BLANK        QQINV#
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C*  CLEAR SCREEN ARRAYS - SUBROUTINE (CLR)
     C*****************************************************************
     C     CLR           BEGSR
     C                   Z-ADD     *ZERO         GR
     C                   Z-ADD     *ZERO         YR
     C                   Z-ADD     *ZERO         CS
     C                   Z-ADD     *ZERO         IN
     C                   Z-ADD     *ZERO         TD
     C                   Z-ADD     *ZERO         PD
     C                   Z-ADD     *ZERO         IA
     C                   Z-ADD     *ZERO         OA
     C*** BEGIN ADD *** 12/31/92 **************************************
     C                   MOVE      *BLANKS       SY
     C*** END ADD *** 12/31/92 ****************************************
     C                   Z-ADD     1             I1
     C                   Z-ADD     ARFND         XHFND
     C                   Z-ADD     ARFYR         XHFYR
     C                   Z-ADD     ARCASE        XHCASE
     C                   Z-ADD     ARINV#        XHINV#
     C                   ENDSR
     C*****************************************************************
     C*  FILL SCREEN ARRAYS - SUBROUTINE (FILL)
     C*****************************************************************
     C     FILL          BEGSR
     C                   MOVE      ARFND         GR(I1)
     C                   MOVE      ARFYR         YR(I1)
     C                   MOVE      ARCASE        CS(I1)
     C                   MOVE      ARINV#        IN(I1)
     C*** BEGIN ADD *** 12/31/92 **************************************
     C                   MOVE      ARSYS         SY(I1)
     C*** END ADD *** 12/31/92 ****************************************
     C*
     C*  GET MOST RECENT TRANS/POST DATES
     C*
     C     ARTRTP        IFEQ      'I'                                          INVOICE TYPE
     C     ARSTAT        ANDNE     'D'                                          NOT DELETED
     C     ARTRCD        ANDEQ     *BLANKS                                      NOT MANUAL
     C*          ARTRDT    IFGE XHTRDT                     HOLD TRDT
     C*
     C                   Z-ADD     ARTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         TD(I1)
     C*
     C                   Z-ADD     ARPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PD(I1)
     C*
     C                   MOVE      ARTRDT        XHTRDT
     C*                    END                             ENDGT XHTRDT
     C                   END                                                    ENDEQ 'I'
     C*
     C*  CALCULATE INVOICE AMOUNT
     C*
     C     ARTRTP        IFEQ      'I'
     C                   ADD       ARAMT         IA(I1)                         INVOICE AMT
     C                   END
     C*
     C*  CALCULATE OPEN AMOUNT
     C*
     C     ARTRTP        IFEQ      'P'
     C                   SUB       ARAMT         OA(I1)                         OPEN AMT
     C                   ELSE
     C                   ADD       ARAMT         OA(I1)
     C                   END
     C*
     C                   ENDSR
     C*****************************************************************
     C*  LOAD FORWARD - SUBROUTINE (LOAD)
     C*****************************************************************
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
     C     ARFND         IFNE      XHFND
     C     ARFYR         ORNE      XHFYR
     C     ARCASE        ORNE      XHCASE
     C     ARINV#        ORNE      XHINV#
     C     IA(I1)        IFNE      *ZEROS
     C                   ADD       1             I1
     C                   ELSE
     C                   Z-ADD     0             OA(I1)
     C                   END                                                    DLTED INV?
     C     I1            CABGT     14            NEWT
     C                   MOVE      ARFND         XHFND             3 0
     C                   MOVE      ARFYR         XHFYR             3 0
     C                   MOVE      ARCASE        XHCASE            7 0
     C*** BEG CHG *** XX/XX/XX ***************************************
     C                   MOVE      ARINV#        XHINV#            9 0
     C**********           MOVE ARINV#    XHINV#  50
     C*** END CHG *** XX/XX/XX ***************************************
     C                   Z-ADD     *ZEROS        XHTRDT
     C                   END
     C*
     C                   EXSR      FILL
     C     ARKEY2        READE     PAR01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     NEWT          TAG
     C     I1            COMP      14                                   6161
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVE      GR(1)         KEY1(1)
     C                   MOVE      YR(1)         KEY2(1)
     C                   MOVE      CS(1)         KEY3(1)
     C                   MOVE      IN(1)         KEY4(1)
     C                   MOVE      GR(14)        KEY1(2)
     C                   MOVE      YR(14)        KEY2(2)
     C                   MOVE      CS(14)        KEY3(2)
     C                   MOVE      IN(14)        KEY4(2)
     C                   ENDSR
     C*****************************************************************
     C*  LOAD BACKWARD - SUBROUTINE (LOADP)
     C*****************************************************************
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     14            I1
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
     C     ARFND         IFNE      XHFND
     C     ARFYR         ORNE      XHFYR
     C     ARCASE        ORNE      XHCASE
     C     ARINV#        ORNE      XHINV#
     C     IA(I1)        IFNE      *ZEROS
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     0             OA(I1)
     C                   END                                                    DLTED INV?
     C     I1            CABLT     1             NEWT2
     C                   MOVE      ARFND         XHFND
     C                   MOVE      ARFYR         XHFYR
     C                   MOVE      ARCASE        XHCASE
     C                   MOVE      ARINV#        XHINV#
     C                   Z-ADD     *ZEROS        XHTRDT
     C                   END
     C*
     C                   EXSR      FILL
     C                   READP     PAR01                                  60
     C  N60ARCO#         COMP      UPCO#                              6060
     C  N60ARSUB#        COMP      UPSUB#                             6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     NEWT2         TAG
     C     I1            COMP      1                                  61  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   MOVE      GR(1)         KEY1(1)
     C                   MOVE      YR(1)         KEY2(1)
     C                   MOVE      CS(1)         KEY3(1)
     C                   MOVE      IN(1)         KEY4(1)
     C                   MOVE      GR(14)        KEY1(2)
     C                   MOVE      YR(14)        KEY2(2)
     C                   MOVE      CS(14)        KEY3(2)
     C                   MOVE      IN(14)        KEY4(2)
     C                   ENDSR
     C*****************************************************************
     C*  LOAD FROM BEGINNING OF FILE - SUBROUTINE (LOADB)
     C*****************************************************************
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *ZEROS        XXFND
     C                   MOVE      *ZEROS        XXFYR
     C                   MOVE      *ZEROS        XXCASE
     C                   MOVE      *ZEROS        XXINV#
     C     ARKEY         SETLL     PAR01
     C     ARKEY2        READE     PAR01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C*
     C     ARFND         IFNE      XHFND
     C     ARFYR         ORNE      XHFYR
     C     ARCASE        ORNE      XHCASE
     C     ARINV#        ORNE      XHINV#
     C     IA(I1)        IFNE      *ZEROS
     C                   ADD       1             I1
     C                   ELSE
     C                   Z-ADD     0             OA(I1)
     C                   END                                                    DLTED INV?
     C     I1            CABGT     14            NEWT1
     C                   MOVE      ARFND         XHFND
     C                   MOVE      ARFYR         XHFYR
     C                   MOVE      ARCASE        XHCASE
     C                   MOVE      ARINV#        XHINV#
     C                   Z-ADD     *ZEROS        XHTRDT
     C                   END
     C*
     C                   EXSR      FILL
     C     ARKEY2        READE     PAR01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     NEWT1         TAG
     C     I1            COMP      14                                   6161
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
**
ROLL UP
ROLL DOWN
