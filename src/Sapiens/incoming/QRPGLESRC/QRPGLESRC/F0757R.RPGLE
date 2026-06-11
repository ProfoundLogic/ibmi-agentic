     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H/TITLE F0757R - 10-Day Cover Letter Information
     H*****************************************************************
     H* Program Description: This program will allow the user to      *
     H*         select the appropriate values that will go on the     *
     H*         letter to be sent to the Provider.                    *
     H*                                                               *
     H* Called From: F0393R - Complete/Pend Program                   *
     H*              F0399R - WW EOMB Letters                         *
     H* Calls to   : None                                             *
     H*                                                               *
     H*****************************************************************
     H* Created on 12/06/2002 by c:62 for Project 114MEO              *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     F* 01/28/10  C:249  Project: SY215                               *
     F*           C:189  Retrieve brand Name
     F*                                                               *
     H*****************************************************************
     FF0757F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     FFAESL2    IF   E           K DISK
     FFALTP     IF A E             DISK
     FFAL2L1    IF   E           K DISK
     FFAL2L2    UF A E           K DISK
     F                                     RENAME(PL201:PL202)
     FFMC1L     UF   E           K DISK
     FFAEOP     IF   E             DISK
      /COPY SCOPYMT01D
     D*
      ***** BEG ADD ***** 01/28/10 ************************************
     D*
     D***PY SCOPYMT01I
     D*
      ***** END ADD ***** 01/28/10 ************************************
     D*
      ***** BEG DEL ***** 01/28/10 ************************************
     D***         DS
     D***                                     1  120DSDTTM
     D***                                     1   60DSTIME
     D***                                     7  120DSDATE
      ***** END DEL ***** 01/28/10 ************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
      ***** BEG DEL ***** 01/28/10 ************************************
     D***PSDS      ESDSSZQ1P
      ***** END DEL ***** 01/28/10 ************************************
     D*****************************************************************
     D* User Constants
     D*****************************************************************
     D OPTNM           C                   CONST('10-Day Cover Letter-
     D                                      Information')
      ***** BEG DEL ***** 01/28/10 ************************************
     D***           'Progressive Data So- C         PDSNAM
     D***           'lutions, Inc.'
      ***** END DEL ***** 01/28/10 ************************************
     D COMAPI          C                   CONST('FA007R')
      ***** BEG DEL ***** 01/28/10 ************************************
     D***           '-- Add --'           C         MODEA
     D***           '- Change -'          C         MODEC
     D***           '-Display-'           C         MODED
      ***** END DEL ***** 01/28/10 ************************************
     D DELETE          C                   CONST('* DELETED')

     C*****************************************************************
     C* Parmeter Lists
     C*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FAEOP
     C                   PARM                    @@RRN2            9 0          FALTP
     C                   PARM      *INKL         @@KL              1
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
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*****************************************************************
     C* Key Lists
     C*****************************************************************
     C*
     C     C1KY          KLIST
     C                   KFLD                    EOCO#
     C*
     C     ESKY1         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C                   KFLD                    KKIDT
     C                   KFLD                    KKITIM
     C*
     C     ESKY2         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C*
     C     L2KY1         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C                   KFLD                    SCLTR#
     C                   KFLD                    KKLSEQ
     C*
     C     L2KY2         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C                   KFLD                    SCLTR#
     C*
     C     L2KY3         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C                   KFLD                    SCLTR#
     C                   KFLD                    SCFRCD
     C                   KFLD                    SCLSEQ
     C*
     C     L2KY4         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C                   KFLD                    SCLTR#
     C                   KFLD                    KKFRCD
     C*****************************************************************
     C* Like Defines
     C*****************************************************************
     C     *LIKE         DEFINE    L2LSEQ        KKLSEQ
     C     *LIKE         DEFINE    L2LSEQ        XXLSEQ
     C     *LIKE         DEFINE    L2FRCD        KKFRCD
     C     *LIKE         DEFINE    L2FRCD        XXFRCD
     C     *LIKE         DEFINE    ESIDT         KKIDT
     C     *LIKE         DEFINE    ESITIM        KKITIM
     C*
     C*****************************************************************
     C* Program Main
     C*****************************************************************
     C                   EXSR      GETCOF
     C                   EXSR      GETPGT
     C                   EXSR      PGMSTR
     C     *IN90         DOUEQ     *OFF
     C                   SETOFF                                       90
     C                   EXSR      INZSR
     C     *IN91         DOUEQ     *OFF
     C                   SETOFF                                       91
     C                   EXSR      SCREXC
     C     *IN11         CASEQ     *ON           FKEYS
     C                   ENDCS                                                    END *IN11
     C*
     C     *IN91         IFEQ      *OFF
     C     *IN92         ANDEQ     *OFF
     C*
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C*
     C     SCPST1        IFNE      *BLANKS
     C                   MOVEL(P)  SCPST1        KKFRCD
     C                   SETON                                        90
     C                   ELSE
     C*
     C                   Z-ADD     RRN2          SVRRN2            5 0
     C*
     C                   READC     SCR01S                                 50
     C     *IN50         IFEQ      '0'
     C*
     C* IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD  , THEN
     C* SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
     C*
     C     SCSEL         IFEQ      '4'
     C                   EXSR      DELDSC
     C                   ELSE                                                   ELSE EQ '1'
     C*
     C* IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '4' TO   INDICATE
     C* A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C*
     C     SCSEL         IFNE      ' '
     C                   SETON                                        2091
     C                   Z-ADD     RRN2          #@$2
     C                   UPDATE    SCR01S
     C                   MOVE      *BLANK        SCSEL
     C                   ENDIF                                                  END NE ' '
     C                   ENDIF                                                  END EQ '4'
     C                   ENDIF                                                  END 0 *IN50
     C*
     C     *IN91         IFEQ      *OFF
     C     *IN09         IFEQ      *OFF
     C                   EXSR      ERRCK
     C     *IN91         IFEQ      *OFF
     C                   EXSR      WRTUPD
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C/EJECT
     C*****************************************************************====*
     C* DELDSC - Delete Line                                           C ***
     C*****************************************************************====*
     C     DELDSC        BEGSR
     C*
     C                   MOVE      RRN2          SRRN2             5 0
     C                   Z-ADD     *ZEROS        K                 5 0
     C                   SETOFF                                       75
     C     *IN75         DOUEQ     *ON
     C                   ADD       1             K
     C     K             CHAIN     SCR01S                             75
B01  C     *IN75         IFEQ      *OFF
B02  C     SCSEL         IFNE      *BLANKS
B02  C     SCSEL         ANDNE     '4'
     C                   SETON                                        2591
     C                   UPDATE    SCR01S
     C                   MOVE      *BLANK        SCSEL
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
S02  C                   ELSE
     C                   SETOFF                                       25
E02  C                   ENDIF                                                  END SCSEL
     C*
B03  C     *IN91         IFEQ      *OFF
B03  C     SCSEL         ANDEQ     '4'
     C     L2KY3         CHAIN     PL202                              99
B04  C     *IN99         IFEQ      *OFF
     C                   DELETE    PL202
     C                   CLEAR                   SCFRCD
     C                   CLEAR                   SCTOCD
     C                   CLEAR                   SCREAS
     C                   CLEAR                   SCLSEQ
     C                   MOVEL(P)  DELETE        SCREAS
     C                   SETON                                        0891
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    SCR01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
S04  C                   ELSE
     C                   CLEAR                   SCFRCD
     C                   CLEAR                   SCTOCD
     C                   CLEAR                   SCREAS
     C                   MOVEL(P)  DELETE        SCREAS
     C                   SETON                                        0891
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    SCR01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
E04  C                   ENDIF
S03  C                   ELSE
B05  C     *IN25         IFEQ      *OFF
     C                   UPDATE    SCR01S
E05  C                   ENDIF
E03  C                   ENDIF
E01  C                   ENDIF                                                  END *IN75
     C                   ENDDO                                                  END *IN75
     C*
     C                   ENDSR
     C/EJECT
     C*****************************************************************====*
     C* ERRCK - Screen Edits                                            C ***
     C*****************************************************************====*
     C     ERRCK         BEGSR
     C*
     C                   MOVEL     *OFF          ONE               1
     C                   MOVE      RRN2          SRRN2             5 0
     C                   Z-ADD     *ZEROS        K                 5 0
     C                   SETOFF                                       75
     C     *IN75         DOUEQ     *ON
     C                   ADD       1             K
     C     K             CHAIN     SCR01S                             75
     C     *IN75         IFEQ      *OFF
     C*
     C     SCFRCD        IFNE      *BLANKS
     C     SCTOCD        ORNE      *BLANKS
     C     SCREAS        ORNE      *BLANKS
     C     SCREAS        ANDNE     DELETE
     C                   MOVEL     *ON           ONE
     C*
     C* FROM CODE IS REQUIRED IF SOMETHING ENTERED ON LINE
     C*
     C     *IN91         IFEQ      *OFF
     C     SCFRCD        IFEQ      *BLANKS
     C                   SETON                                        2191
     C                   UPDATE    SCR01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   ELSE
     C                   SETOFF                                       21
     C                   ENDIF                                                  END SCFRCD
     C                   ENDIF
     C*
     C* TO CODE IS REQUIRED IF SOMETHING ENTERED ON LINE
     C*
     C     *IN91         IFEQ      *OFF
     C     SCTOCD        IFEQ      *BLANKS
     C                   SETON                                        2291
     C                   UPDATE    SCR01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   ELSE
     C                   SETOFF                                       22
     C                   ENDIF                                                  END SCTOCD
     C                   ENDIF
     C*
     C* TO CODE MUST BE DIFFERENT FROM FROM CODE.
     C*
     C     *IN91         IFEQ      *OFF
     C     SCTOCD        IFEQ      SCFRCD
     C                   SETON                                        2691
     C                   UPDATE    SCR01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   ELSE
     C                   SETOFF                                       22
     C                   ENDIF                                                  END SCTOCD
     C                   ENDIF
     C*
     C* REASON IS REQUIRED IF SOMETHING ENTERED ON LINE
     C*
     C     *IN91         IFEQ      *OFF
     C     SCREAS        IFEQ      *BLANKS
     C                   SETON                                        2391
     C                   UPDATE    SCR01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   ELSE
     C                   SETOFF                                       23
     C                   ENDIF                                                  END SCREAS
     C                   ENDIF
     C*
     C     *IN91         IFEQ      *OFF
     C                   UPDATE    SCR01S
     C                   ENDIF
     C                   ENDIF                                                  SCFRCD
     C                   ENDIF                                                  END *IN75
     C                   ENDDO                                                  END *IN75
     C*
     C     *IN91         IFEQ      *OFF
     C     ONE           IFEQ      *OFF
     C     1             CHAIN     SCR01S                             75
     C     *IN75         IFEQ      *OFF
     C                   SETON                                        2491
     C                   UPDATE    SCR01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     1             #@$2
     C                   ELSE
     C                   SETOFF                                       24
     C                   UPDATE    SCR01S
     C                   ENDIF                                                  *IN75 = *OFF
     C                   ENDIF                                                  K = 1
     C                   ENDIF                                                  *IN91 = *OFF
     C*
     C                   MOVE      SRRN2         RRN2
     C*
     C                   ENDSR
     C/EJECT
     C*****************************************************************
     C* FKEYS - Function Key Processing
     C*****************************************************************
     C     FKEYS         BEGSR
     C*
     C*   F5 = REFRESH
     C*
     C     *INKE         IFEQ      *ON
     C*
     C* Clear subfile
     C*
     C                   SETON                                        04
     C                   WRITE     SCR01C
     C                   SETOFF                                       04
     C*
     C                   Z-ADD     *ZEROS        RRN2
     C                   Z-ADD     *ZEROS        XXRRN2
     C*
     C     L2KY4         SETLL     PL202                              99
     C*
     C                   EXSR      LOAD
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   SETON                                        91
     C                   ENDIF
     C*
     C* F12 = CANCEL
     C*
     C     *INKL         IFEQ      *ON
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01S'
     C     Q2RECF        OREQ      'SCR01C'
     C                   SETON                                        92
     C                   ENDIF
     C                   ENDIF                                                  END *INKL
     C*
     C* *IN18 -  ROLLUP REQUEST
     C*
     C     *IN18         IFEQ      *ON                                          ROLLUP
     C     *IN32         IFEQ      *OFF
     C                   EXSR      LOAD
     C                   ENDIF
     C                   SETON                                        91
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C     *IN92         IFEQ      *OFF
     C                   SETON                                        91
     C                   ENDIF
     C*
     C                   ENDSR
     C/EJECT
     C*****************************************************************
     C*  GETCOF - Gets company and fund names if possible; otherwise
     C*           get PDS name for company name
     C*****************************************************************
     C     GETCOF        BEGSR
     C*
     C                   CALL      COMAPI        COMPL                  99
B01  C     *IN99         IFEQ      *ON
     C***  Begin Chg  ***  01/28/10  **************************************
     C************         MOVELPDSNAM    ##DATA    P
     C                   CALL      'RTVBRND'                            99
     C                   PARM      'I'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C*
     C                   MOVEL(P)  #BRND1        ##DATA
     C***  End   Chg  ***  01/28/10  **************************************
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        ##CONM
E01  C                   ENDIF
     C                   MOVEL     ##CONM        SCCONM
     C*
     C                   ENDSR
     C*****************************************************************
     C*  GETPGT - Get program title for display heading
     C*****************************************************************
     C     GETPGT        BEGSR
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C                   ENDSR
     C*****************************************************************====*
     C* INZSR - Initialization Program                                  T ***
     C*****************************************************************====*
     C     INZSR         BEGSR
     C*
     C     L2KY4         SETLL     PL202                              99
     C                   SETON                                        04
     C                   WRITE     SCR01C
     C                   SETOFF                                       04
     C                   Z-ADD     *ZEROS        RRN2
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C                   ENDSR
     C/EJECT
     C*****************************************************************====*
     C* LOAD - Load Subfile                                            D ***
     C*****************************************************************====*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN08
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN32
     C*
     C*   Position Cursor flag - used to check if the 1st subfile RRN
     C*     of the current page needs to be saved.
     C*
     C                   MOVE      'Y'           PCFL              1
     C*
     C* LOAD THE SUBFILE SCREEN WITH UPTO 6 LINES
     C                   DO        6             X                 5 0
     C*
     C     L2KY2         READE(N)  PL202                                  99
     C     *IN99         IFEQ      *OFF
     C*
     C                   ADD       1             RRN2              5 0
     C                   MOVEL(P)  L2FRCD        SCFRCD
     C                   MOVEL(P)  L2TOCD        SCTOCD
     C                   MOVEL(P)  L2REAS        SCREAS
     C                   Z-ADD     L2LSEQ        SCLSEQ
     C*
     C*   Save 1st subfile RRN of the current page
     C*
     C     PCFL          IFEQ      'Y'
     C                   Z-ADD     RRN2          XXRRN2            5 0
     C                   MOVE      'N'           PCFL
     C                   ENDIF
     C*
     C                   WRITE     SCR01S
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       SCSEL
     C                   MOVE      *BLANKS       SCFRCD
     C                   MOVE      *BLANKS       SCTOCD
     C                   MOVE      *BLANKS       SCREAS
     C                   Z-ADD     *ZEROS        SCLSEQ
     C                   ADD       1             RRN2
     C*
     C*   Save 1st subfile RRN of the current page
     C*
     C     PCFL          IFEQ      'Y'
     C                   Z-ADD     RRN2          XXRRN2
     C                   MOVE      'N'           PCFL
     C                   ENDIF
     C*
     C                   WRITE     SCR01S
     C*
     C                   ENDIF
     C                   ENDDO
     C*
     C* TO POSITION SCREEN AND CURSOR
     C*
     C                   Z-ADD     XXRRN2        #@$2
     C*
     C                   ENDSR
     C/EJECT
     C*****************************************************************====*
     C* PGMSTR - Program Start                                          C ***
     C*****************************************************************====*
     C     PGMSTR        BEGSR
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'A'
     C                   MOVEL(P)  MODEA         SCMODE
     C     @@MODE        WHENEQ    'C'
     C                   MOVEL(P)  MODEC         SCMODE
     C     @@MODE        WHENEQ    'D'
     C                   MOVEL(P)  MODED         SCMODE
     C                   ENDSL
     C*
     C     @@MODE        COMP      'D'                                    09
     C*
     C* GET RECORD IF IT EXISTS, ELSE ITS AN ADD
     C*
     C     @@RRN         CHAIN     PEO01                              99
     C*
     C                   Z-ADD     EOEOB#        SCEOB#
     C                   Z-ADD     EORCN         SCRCN
     C*
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C                   Z-ADD     EOTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
     C*
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANKS       #1L1             78
     C                   PARM      *BLANKS       #1L2             78
     C*
     C                   MOVEL     #1L1          SCHD1
     C                   MOVEL     #1L2          SCHD2
     C*
     C                   CALL      'FA707R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    @@RRN
     C                   PARM      *BLANKS       #1L1
     C*
     C                   MOVEL     #1L1          SCHD3
     C*
     C     @@RRN2        CHAIN(N)  PLT01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     LTLTR#        SCLTR#
     C                   ELSE
     C     C1KY          CHAIN     PC101                              99
     C     *IN99         IFEQ      *OFF
     C                   ADD       1             C1LTR#
     C                   Z-ADD     C1LTR#        SCLTR#
     C                   UPDATE    PC101
     C                   ENDIF
     C                   ENDIF
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C                   ENDSR
     C/EJECT
     C*****************************************************************
     C* SCREXC - Execute screens.
     C*****************************************************************
     C     SCREXC        BEGSR
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   SETON                                        01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   SETOFF                                       01
     C     SVRRN2        IFNE      *ZEROS
     C                   Z-ADD     SVRRN2        RRN2
     C                   Z-ADD     *ZEROS        SVRRN2
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C/EJECT
     C*****************************************************************====*
     C* WRTUPD - Writes or Updates FALTP and FAL2P                     C ***
     C*****************************************************************====*
     C     WRTUPD        BEGSR
     C*
     C                   Z-ADD     *ZEROS        K                 5 0
     C                   SETOFF                                       75
     C     *IN75         DOUEQ     *ON
     C                   ADD       1             K
     C     K             CHAIN     SCR01S                             75
     C     *IN75         IFEQ      *OFF
     C*
     C     SCFRCD        IFNE      *BLANKS
     C     SCTOCD        ORNE      *BLANKS
     C     SCREAS        ORNE      *BLANKS
     C     SCREAS        ANDNE     DELETE
     C     SCLSEQ        IFNE      *ZEROS
     C*
     C     L2KY3         CHAIN     PL202                              99
     C*
     C                   MOVEL(P)  SCFRCD        L2FRCD
     C                   MOVEL(P)  SCTOCD        L2TOCD
     C                   MOVEL(P)  SCREAS        L2REAS
     C*
     C                   MOVEL(P)  'A'           L2STAT
     C                   MOVEL(P)  Q1USER        L2USER
     C                   MOVEL(P)  Q1PGM         L2PGM
     C                   Z-ADD     DSTIME        L2TIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         L2DT
     C*
     C                   UPDATE    PL202
     C                   ELSE
     C*
     C                   Z-ADD     *HIVAL        KKLSEQ
     C     L2KY1         SETGT     PL201
     C     L2KY2         READPE    PL201                                  99
     C     *IN99         IFEQ      *OFF
     C     L2LSEQ        ADD       1             XXLSEQ
     C                   ELSE
     C                   Z-ADD     1             XXLSEQ
     C                   ENDIF
     C*
     C                   CLEAR                   PL202
     C*
     C                   Z-ADD     EOCO#         L2CO#
     C                   Z-ADD     EOSUB#        L2SUB#
     C                   Z-ADD     EOFND         L2FND
     C                   Z-ADD     EOFYR         L2FYR
     C                   Z-ADD     EOCASE        L2CASE
     C                   Z-ADD     EOEOB#        L2EOB#
     C                   Z-ADD     SCLTR#        L2LTR#
     C                   Z-ADD     XXLSEQ        L2LSEQ
     C                   Z-ADD     XXLSEQ        SCLSEQ
     C                   MOVEL(P)  SCFRCD        L2FRCD
     C                   MOVEL(P)  SCTOCD        L2TOCD
     C                   MOVEL(P)  SCREAS        L2REAS
     C*
     C                   MOVEL(P)  'A'           L2STAT
     C                   MOVEL(P)  Q1USER        L2USER
     C                   MOVEL(P)  Q1PGM         L2PGM
     C                   Z-ADD     DSTIME        L2TIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         L2DT
     C                   MOVEL(P)  Q1USER        L2IUSR
     C                   MOVEL(P)  Q1PGM         L2IPGM
     C                   Z-ADD     DSTIME        L2ITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         L2IDT
     C                   WRITE     PL202
     C                   ENDIF
     C                   ENDIF                                                  END SCFRCD
     C                   UPDATE    SCR01S
     C                   ENDIF                                                  END *IN75
     C                   ENDDO                                                  END *IN75
     C*
     C*  Write/Update FALTP.
     C*
     C     @@RRN2        IFEQ      *ZEROS
     C*
     C                   CLEAR                   PLT01
     C*
     C                   Z-ADD     EOCO#         LTCO#
     C                   Z-ADD     EOFND         LTFND
     C                   Z-ADD     EOFYR         LTFYR
     C                   Z-ADD     EOCASE        LTCASE
     C                   Z-ADD     SCLTR#        LTLTR#
     C                   MOVEL(P)  EOSS#         LTSS#
     C                   Z-ADD     EOACDT        LTACDT
     C                   MOVEL(P)  EOLSN         LTLNM
     C                   MOVEL(P)  EOFSN         LTFNM
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTTRDT
     C*
     C                   Z-ADD     EOFRDT        LTFSDT
     C                   Z-ADD     EOTODT        LTTSDT
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        ##DATE            8 0
     C                   PARM      10            ##DAYS            5 0
     C                   PARM      *BLANKS       ##ERR             1
     C*
     C                   Z-ADD     ##DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTDIDT
     C*
     C                   Z-ADD     LTTRDT        LTPRDT
     C                   Z-ADD     EOFEIN        LTFEIN
     C                   Z-ADD     EOMOD         LTMOD
     C                   MOVEL(P)  'I'           LTBTIM
     C                   MOVEL(P)  '1'           LTFLG1
     C                   MOVEL(P)  'T'           LTFLG2
     C                   MOVEL(P)  'P'           LTSTAT
     C                   Z-ADD     EOSUB#        LTSUB#
     C                   Z-ADD     EOEOB#        LTEOB#
     C                   Z-ADD     EORCN         LTRCN
     C*
     C                   Z-ADD     *HIVAL        KKIDT
     C                   Z-ADD     *HIVAL        KKITIM
     C     ESKY1         SETGT     PES01
     C     ESKY2         READPE    PES01                                  99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     ESITIM        LTSITM
     C                   Z-ADD     ESIDT         LTSIDT
     C                   ENDIF
     C*
     C                   MOVEL(P)  'Y'           LTLTFL
     C*
     C                   MOVE      Q1USER        LTUSER
     C                   MOVE      Q1PGM         LTPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTDT
     C*
     C                   Z-ADD     DSTIME        LTTIME
     C                   MOVE      Q1USER        LTIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTIDT
     C*
     C                   WRITE     PLT01
     C                   ENDIF
     C*
     C     @@MODE        IFEQ      'A'
     C     LTBTIM        IFEQ      'I'
     C*
     C                   MOVE      LTCO#         $$CO#
     C                   MOVE      LTSUB#        $$SUB#
     C                   MOVE      LTFND         $$FND
     C                   MOVE      LTFYR         $$FYR
     C                   MOVE      LTCASE        $$CASE
     C                   MOVE      LTLTR#        $$LTR#
     C*
     C                   CALL      'SA012R'
     C                   PARM      Q1USER        ##USER           10
     C                   PARM      *BLANKS       ##OUTQ           10
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C                   CALL      'F0758C'
     C                   PARM                    $$CO#             3
     C                   PARM                    $$SUB#            5
     C                   PARM                    $$FND             3
     C                   PARM                    $$FYR             3
     C                   PARM                    $$CASE            7
     C                   PARM                    $$LTR#            9
     C                   PARM      'I'           $$FLAG            1
     C                   PARM      ##OUTQ        $$OUTQ           10
     C                   PARM      'D'           $$MODE            1
     C*
     C                   ENDIF                                                  ENDIF 'I'
     C                   ENDIF                                                  ENDIF 'A'
     C*
     C                   ENDSR
     C/EJECT
